;;; index-extract.el --- Extract LaTeX passages flagged by \index keys  -*- lexical-binding: t; -*-

;; Author: Blaine Mooers <blaine-mooers@ou.edu>
;; Maintainer: Blaine Mooers <blaine-mooers@ou.edu>
;; Version: 1.3
;; Keywords: tex, convenience, outlines, wp
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/MooersLab/extract-index-el

;; Copyright (C) 2026 Blaine Mooers

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; index-extract pulls passages out of LaTeX files when those passages are
;; tagged with \index{...} keys.  It was built for a daily log or diary in
;; which there is one document file per day, the files are organized into
;; monthly subfolders, and each passage looks like this:
;;
;;     \subsubsection{Correspondence with my brother}
;;     \index{brother}
;;     \index{letter}
;;
;;     We traded a couple of emails about the reunion.
;;     I still owe him a real reply.
;;
;;     \subsubsection{Next topic}
;;     ...
;;
;; The main use is to collect everything you wrote about a topic across a
;; stretch of days, for example to gather the brother-related passages from
;; the past week into the weekly letter you write to him.  The same idea
;; serves a manuscript, a grant, a book, or a talk, by searching a different
;; key.
;;
;; COMMANDS
;; --------
;; M-x index-extract
;;     Pick the files to scan (add whole monthly subfolders and/or single
;;     files), then the \index key(s), and collect the passages into the
;;     *index-extract* buffer.
;;
;; M-x index-extract-last-days
;;     Instead of picking folders, scan every dated file from the last N days
;;     (default 7).  Files are found by recursively searching your diary root
;;     (`index-extract-root-directory') and keeping those whose name contains
;;     a date within range (see FILENAME DATES).  A week that spans two months
;;     is handled automatically.  With a prefix argument (C-u) the result is
;;     written to an
;;     org file instead of the buffer.
;;
;; M-x index-extract-range
;;     Like `index-extract-last-days' but you give an explicit start and end
;;     date.  With a prefix argument (C-u) the result goes to an org file.
;;
;; M-x index-extract-to-org
;;     Pick files and keys, then append the gathered passages to an org file
;;     as one org subtree: a top-level heading with one child heading per
;;     passage, each child carrying :SOURCE:, :LINE:, :KEYS:, and :DATE:
;;     properties and the passage text as its body.
;;
;; In the *index-extract* buffer:
;;   C-c C-s  save the buffer to a file
;;   C-c C-w  copy just the passage text (no label lines) to the kill ring
;;   C-c C-o  write these same results to an org file (subtree)
;;   C-c C-k  run again with the same files but new keys
;;   C-c C-f  run again with the same keys but a new file selection
;;
;; FILENAME DATES
;; --------------
;; The date commands read the date out of each file's name.  Two styles are
;; recognised out of the box: an ISO date such as 2026-09-26, and a
;; day-month-name-year date such as 26September2026 (the style this log uses).
;; Full month names and the common three-letter abbreviations both work, and
;; the month name is matched without regard to case.  See
;; `index-extract-date-regexp' and `index-extract-date-named-regexp'.
;;
;; SETUP
;; -----
;;   (require 'index-extract)
;;   ;; where the file prompts start, and the root the date commands search:
;;   (setq index-extract-default-directory "~/2026words/Content/"
;;         index-extract-root-directory    "~/2026words/Content/"
;;         index-extract-default-days      7)
;;
;; WHAT COUNTS AS A PASSAGE
;; ------------------------
;; A passage is the text that follows the matched \index line, down to the
;; next boundary: the next \index, the next sectioning command, the start of
;; an environment (\begin{...}, which keeps the trailing TODO itemize out of
;; the last passage), \end{document}, or a "% TODO" comment line.  See
;; `index-extract-boundary-regexp'.  Because the diary stacks several \index
;; keys under one heading and then the prose, `index-extract-skip-adjacent-index'
;; (on by default) skips the rest of that stacked run so that matching ANY of
;; the stacked keys returns the prose of the passage.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'time-date)

;; Optional Org integration; only called when Org is loaded.
(declare-function org-reveal "org" (&optional siblings))
(declare-function org-read-date "org"
                  (&optional with-time to-time from-string prompt
                             default-time default-input inactive))

(defgroup index-extract nil
  "Extract passages from LaTeX files that are flagged by \\index keys."
  :group 'tex
  :prefix "index-extract-")

(defcustom index-extract-default-directory nil
  "Directory where the file prompts start.
Nil means start in `default-directory'.  For a monthly diary you might set
this to, for example, \"~/2026words/Content/\"."
  :type '(choice (const :tag "Current directory" nil) directory))

(defcustom index-extract-root-directory nil
  "Root folder the date commands search recursively for dated files.
Nil falls back to `index-extract-default-directory', and failing that you are
prompted once.  Set it to the folder that holds your monthly subfolders, e.g.
\"~/2026words/Content/\", so `index-extract-last-days' never has to ask."
  :type '(choice (const :tag "Fall back / prompt" nil) directory))

(defcustom index-extract-default-days 7
  "Default number of days for `index-extract-last-days'."
  :type 'integer)

(defcustom index-extract-file-glob "*.tex"
  "Shell wildcard used when adding a whole directory of files."
  :type 'string)

(defcustom index-extract-date-regexp
  "\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)"
  "Regexp matching an ISO YYYY-MM-DD date in a file name.
Groups 1, 2 and 3 are the year, month and day."
  :type 'regexp)

(defcustom index-extract-date-named-regexp
  (concat "\\([0-9]\\{1,2\\}\\)"
          "\\(January\\|February\\|March\\|April\\|May\\|June\\|July"
          "\\|August\\|September\\|October\\|November\\|December"
          "\\|Jan\\|Feb\\|Mar\\|Apr\\|Jun\\|Jul\\|Aug\\|Sept\\|Sep"
          "\\|Oct\\|Nov\\|Dec\\)"
          "\\([0-9]\\{4\\}\\)")
  "Regexp matching a DayMonthNameYear date in a file name.
For example it matches the 26September2026 in \"26September2026.tex\".
Group 1 is the day, group 2 the month name, and group 3 the year.
Matching ignores case (see `index-extract--file-date')."
  :type 'regexp)

(defconst index-extract--month-names
  '(("january" . 1) ("february" . 2) ("march" . 3) ("april" . 4)
    ("may" . 5) ("june" . 6) ("july" . 7) ("august" . 8)
    ("september" . 9) ("october" . 10) ("november" . 11) ("december" . 12)
    ("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("sept" . 9) ("oct" . 10)
    ("nov" . 11) ("dec" . 12))
  "Alist mapping lower-case English month names and abbreviations to numbers.")

(defcustom index-extract-match-style 'exact
  "How a requested key is compared with the text inside \\index{...}.
`exact'      the key must equal the index text.
`substring'  the key must appear somewhere inside the index text.
`regexp'     the key is treated as an Emacs regexp."
  :type '(choice (const exact) (const substring) (const regexp)))

(defcustom index-extract-case-fold t
  "If non-nil, key matching ignores case."
  :type 'boolean)

(defcustom index-extract-include-heading t
  "If non-nil, label each passage with the nearest preceding section heading."
  :type 'boolean)

(defcustom index-extract-skip-adjacent-index t
  "If non-nil, skip \\index lines that immediately follow the matched one.
Your diary stacks several \\index keys directly under one heading and then
the prose.  With this enabled, matching any of the stacked keys returns the
prose of the passage.  With it disabled you get the strictly literal
behaviour of copying from the matched \\index down to the very next \\index."
  :type 'boolean)

(defcustom index-extract-heading-regexp
  (concat "^[ \t]*\\\\\\(?:sub\\)*section\\*?{\\([^}]*\\)}"
          "\\|^[ \t]*\\\\\\(?:chapter\\|paragraph\\|subparagraph\\)\\*?{\\([^}]*\\)}")
  "Regexp that matches a sectioning command.
Group 1 or group 2 holds the section title."
  :type 'regexp)

(defcustom index-extract-boundary-regexp
  (concat "^[ \t]*\\(?:"
          "\\\\index"                       ; the next \index
          "\\|\\\\\\(?:sub\\)*section"       ; \section \subsection \subsubsection
          "\\|\\\\chapter\\|\\\\paragraph\\|\\\\subparagraph"
          "\\|\\\\begin{"                    ; start of an environment (e.g. the TODO itemize)
          "\\|\\\\end{document}"
          "\\|% *TODO"                       ; a "% TODO" comment line
          "\\)")
  "A passage ends at the first following line matching this regexp.
The two boundaries you care about most, the next \\index and the next
sectioning command, are included first; the rest are safety boundaries so a
passage does not run into a following environment or the TODO list."
  :type 'regexp)

(defcustom index-extract-result-buffer "*index-extract*"
  "Name of the buffer that collects the extracted passages."
  :type 'string)

(defcustom index-extract-result-major-mode #'latex-mode
  "Major mode for the results buffer, or nil for no special mode.
The mode is turned on with its mode hooks suppressed, so a heavy
`latex-mode-hook', for example one that starts an LSP server, does not run in
this scratch buffer.  Set this to nil if you would rather the results buffer
stay in Fundamental mode."
  :type '(choice (const :tag "None" nil) function))

(defcustom index-extract-org-file nil
  "Default org file for the org-output commands.  Nil means always prompt."
  :type '(choice (const :tag "Always prompt" nil) file))

(defcustom index-extract-sort-files t
  "If non-nil, scan files in sorted (alphabetical) order.
Date-stamped filenames then come out in chronological order."
  :type 'boolean)

(defvar index-extract--last-files nil
  "File list used by the most recent extraction.")

(defvar index-extract--last-keys nil
  "Key list used by the most recent extraction.")

(defvar index-extract--last-results nil
  "Passage plists produced by the most recent extraction.")

(defconst index-extract--index-regexp
  "\\\\index\\(?:\\[[^]]*\\]\\)?{\\([^}]*\\)}"
  "Regexp matching \\index{...}; group 1 is the index text.")

(defun index-extract--keys-string (keys)
  "Return KEYS as a single comma-separated string."
  (mapconcat #'identity keys ", "))

;;;; Dates in file names

(defun index-extract--encode (year month day)
  "Return a Lisp time value for midnight on YEAR, MONTH, and DAY."
  (encode-time (list 0 0 0 day month year nil -1 nil)))

(defun index-extract--month-number (name)
  "Return the month number 1 to 12 for NAME, or nil if NAME is not a month."
  (cdr (assoc (downcase name) index-extract--month-names)))

(defun index-extract--file-date (file)
  "Return the date encoded in FILE's name as a Lisp time value, or nil.
Two filename styles are recognised: an ISO date such as 2026-09-26 (see
`index-extract-date-regexp') and a day-month-name-year date such as
26September2026 (see `index-extract-date-named-regexp').  The month name is
matched without regard to case."
  (let ((name (file-name-nondirectory file))
        (case-fold-search t))
    (cond
     ((string-match index-extract-date-regexp name)
      (index-extract--encode (string-to-number (match-string 1 name))
                             (string-to-number (match-string 2 name))
                             (string-to-number (match-string 3 name))))
     ((string-match index-extract-date-named-regexp name)
      (let ((month (index-extract--month-number (match-string 2 name))))
        (when month
          (index-extract--encode (string-to-number (match-string 3 name))
                                 month
                                 (string-to-number (match-string 1 name)))))))))

(defun index-extract--today-midnight ()
  "Return a Lisp time value for midnight today."
  (let ((n (decode-time)))
    (index-extract--encode (nth 5 n) (nth 4 n) (nth 3 n))))

(defun index-extract--resolve-root ()
  "Return the diary root folder for the date commands."
  (or index-extract-root-directory
      index-extract-default-directory
      (read-directory-name "Diary root folder (searched recursively): "
                           default-directory)))

;;;; File selection

(defun index-extract--files-in-directory (dir recursive)
  "Return the files in DIR matching `index-extract-file-glob'.
If RECURSIVE is non-nil, descend into subdirectories."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        (re (wildcard-to-regexp index-extract-file-glob)))
    (if recursive
        (directory-files-recursively dir re)
      (directory-files dir t re))))

(defun index-extract-read-files ()
  "Interactively build and return the list of .tex files to scan.
You can add whole directories (one per monthly subfolder) and single files,
so a week that spans two or more months is easy to cover."
  (let ((files '())
        (start (or index-extract-default-directory default-directory))
        (done nil))
    (while (not done)
      (let ((choice
             (car (read-multiple-choice
                   (format "Add to scan list (%d file(s) so far): " (length files))
                   '((?d "directory" "Add every matching file in a folder")
                     (?f "file"      "Add a single file")
                     (?l "list"      "Show the files chosen so far")
                     (?x "done"      "Finish and start scanning"))))))
        (pcase choice
          (?d (let* ((dir (read-directory-name "Folder to scan: " start))
                     (rec (y-or-n-p "Include its subfolders too? "))
                     (new (index-extract--files-in-directory dir rec)))
                (setq start dir)
                (if new
                    (progn (setq files (append files new))
                           (message "Added %d file(s) from %s" (length new) dir))
                  (message "No %s files in %s" index-extract-file-glob dir))))
          (?f (let ((f (read-file-name "File to add: " start nil t)))
                (when (and f (file-readable-p f))
                  (push (expand-file-name f) files)
                  (message "Added %s" f))))
          (?l (message "%s"
                       (if files
                           (mapconcat #'identity (reverse files) "\n")
                         "(none chosen yet)")))
          (?x (setq done t)))))
    (setq files (delete-dups (mapcar #'expand-file-name files)))
    (when index-extract-sort-files
      (setq files (sort files #'string<)))
    (unless files (user-error "No files selected"))
    files))

(defun index-extract-files-in-range (start end &optional root)
  "Return dated .tex files under ROOT whose filename date is in [START, END].
START and END are Lisp time values and the range is inclusive.  ROOT defaults
to the diary root (see `index-extract--resolve-root') and is searched
recursively, so files from any monthly subfolder are considered."
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (index-extract--resolve-root)))))
         (cands (directory-files-recursively
                 root (wildcard-to-regexp index-extract-file-glob)))
         (hits '()))
    (dolist (f cands)
      (let ((d (index-extract--file-date f)))
        (when (and d
                   (not (time-less-p d start))
                   (not (time-less-p end d)))
          (push f hits))))
    (setq hits (delete-dups hits))
    (when index-extract-sort-files (setq hits (sort hits #'string<)))
    hits))

(defun index-extract-files-last-days (days &optional root)
  "Return dated files under ROOT from the last DAYS days, ending today.
DAYS counts today and the DAYS-1 days before it."
  (let* ((end (index-extract--today-midnight))
         (start (time-subtract end (days-to-time (1- (max 1 days))))))
    (index-extract-files-in-range start end root)))

;;;; Keys

(defun index-extract--collect-keys (files)
  "Return a sorted list of all distinct \\index texts in FILES."
  (let ((keys '()))
    (dolist (f files)
      (when (file-readable-p f)
        (with-temp-buffer
          (insert-file-contents f)
          (goto-char (point-min))
          (while (re-search-forward index-extract--index-regexp nil t)
            (push (string-trim (match-string 1)) keys)))))
    (sort (delete-dups keys) #'string<)))

(defun index-extract-read-keys (files)
  "Prompt for one or more \\index keys, completing from the keys in FILES."
  (let* ((cands (index-extract--collect-keys files))
         (raw (completing-read-multiple
               "Index key(s) to extract (comma-separated): " cands))
         (keys (delete-dups (mapcar #'string-trim (remq nil raw)))))
    (unless keys (user-error "No keys given"))
    keys))

(defun index-extract--match-p (content keys)
  "Return non-nil if CONTENT (text inside \\index{}) matches any of KEYS."
  (let ((content (string-trim content)))
    (seq-some
     (lambda (key)
       (pcase index-extract-match-style
         ('exact (eq t (compare-strings key nil nil content nil nil
                                        index-extract-case-fold)))
         ('substring
          (let ((c (if index-extract-case-fold (downcase content) content))
                (k (if index-extract-case-fold (downcase key) key)))
            (string-match-p (regexp-quote k) c)))
         ('regexp
          (let ((case-fold-search index-extract-case-fold))
            (string-match-p key content)))))
     keys)))

;;;; Extraction

(defun index-extract--current-heading ()
  "Return the title of the nearest sectioning command before point, or nil."
  (save-excursion
    (when (re-search-backward index-extract-heading-regexp nil t)
      (string-trim (or (match-string 1) (match-string 2))))))

(defun index-extract--passage-after-index (index-line-end)
  "Return (START . BODY) for the passage after INDEX-LINE-END.
INDEX-LINE-END is the end position of a matched \\index line.  START is the
buffer position where the body begins; BODY is the trimmed passage text."
  (goto-char index-line-end)
  (forward-line 1)
  (when index-extract-skip-adjacent-index
    (while (and (not (eobp))
                (looking-at-p
                 (concat "^[ \t]*\\(?:$\\|" index-extract--index-regexp "\\)")))
      (forward-line 1)))
  (let ((start (point)))
    (if (re-search-forward index-extract-boundary-regexp nil t)
        (goto-char (match-beginning 0))
      (goto-char (point-max)))
    (cons start (string-trim (buffer-substring-no-properties start (point))))))

(defun index-extract--scan-file (file keys)
  "Return a list of passage plists for KEYS found in FILE.
Each plist has :file :line :heading :keys :body.  A passage tagged with more
than one of the requested keys is returned once, with all matched keys."
  (let ((results '())
        (seen (make-hash-table :test 'eql)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward index-extract--index-regexp nil t)
        (let ((content (string-trim (match-string 1)))
              (mbeg (match-beginning 0))
              (lend (line-end-position)))
          (when (index-extract--match-p content keys)
            (let* ((heading (and index-extract-include-heading
                                 (save-excursion
                                   (goto-char mbeg)
                                   (index-extract--current-heading))))
                   (lineno (line-number-at-pos mbeg))
                   (pb (save-excursion (index-extract--passage-after-index lend)))
                   (start (car pb))
                   (body (cdr pb))
                   (prev (gethash start seen)))
              (if prev
                  (plist-put prev :keys
                             (delete-dups (append (plist-get prev :keys)
                                                  (list content))))
                (let ((pl (list :file file :line lineno :heading heading
                                :keys (list content) :body body)))
                  (puthash start pl seen)
                  (push pl results))))))))
    (nreverse results)))

(defun index-extract--gather (files keys)
  "Scan FILES for KEYS and return the list of passage plists, in file order."
  (let ((ordered (if index-extract-sort-files
                     (sort (copy-sequence files) #'string<)
                   files))
        (all '()))
    (dolist (f ordered)
      (when (file-readable-p f)
        (setq all (append all (index-extract--scan-file f keys)))))
    all))

;;;; Results buffer (LaTeX comments)

(defun index-extract--format (results)
  "Insert the formatted RESULTS (a list of passage plists) at point."
  (dolist (pl results)
    (let ((file (plist-get pl :file))
          (line (plist-get pl :line))
          (heading (plist-get pl :heading))
          (keys (plist-get pl :keys))
          (body (plist-get pl :body)))
      (insert (format "%%%% ---- %s : line %d : key%s %s%s ----\n"
                      (file-name-nondirectory file)
                      line
                      (if (cdr keys) "s" "")
                      (mapconcat (lambda (k) (format "\"%s\"" k)) keys ", ")
                      (if heading (format " : [%s]" heading) "")))
      (insert (if (string-empty-p body) "%% (no body found)\n" (concat body "\n")))
      (insert "\n"))))

(defun index-extract--enable-result-mode ()
  "Turn on `index-extract-result-major-mode' in the current buffer.
The mode hooks are suppressed, so a heavy `latex-mode-hook', for example one
that starts an LSP server, does not run in this scratch results buffer.  If
the mode cannot be enabled, fall back to `fundamental-mode' and report the
problem rather than aborting the extraction."
  (when index-extract-result-major-mode
    (condition-case err
        ;; Bind `delayed-mode-hooks' so the hooks suppressed here are discarded
        ;; on exit and never flushed into a later buffer.
        (let ((delayed-mode-hooks nil))
          (delay-mode-hooks (funcall index-extract-result-major-mode)))
      (error
       (ignore-errors (fundamental-mode))
       (message "index-extract: could not enable %s (%s); using fundamental-mode"
                index-extract-result-major-mode
                (error-message-string err))))))

(defun index-extract--run (files keys)
  "Gather passages for KEYS from FILES into `index-extract-result-buffer'.
Return the list of passage plists."
  (setq index-extract--last-files files
        index-extract--last-keys keys)
  (let* ((all (index-extract--gather files keys))
         (nfiles (length (seq-filter #'file-readable-p files)))
         (buf (get-buffer-create index-extract-result-buffer)))
    (setq index-extract--last-results all)
    (with-current-buffer buf
      (erase-buffer)
      (index-extract--enable-result-mode)
      (index-extract-result-mode 1)
      (insert (format "%%%% index-extract: %d passage(s) for key(s) %s in %d file(s)\n"
                      (length all) (index-extract--keys-string keys) nfiles))
      (insert (format "%%%% generated %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      (index-extract--format all)
      (goto-char (point-min)))
    (display-buffer buf)
    (message "index-extract: %d passage(s) from %d file(s)" (length all) nfiles)
    all))

(defvar index-extract-result-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-s") #'index-extract-save-results)
    (define-key m (kbd "C-c C-w") #'index-extract-copy-bodies)
    (define-key m (kbd "C-c C-o") #'index-extract-results-to-org)
    (define-key m (kbd "C-c C-k") #'index-extract-again-keys)
    (define-key m (kbd "C-c C-f") #'index-extract-again-files)
    m)
  "Keymap for `index-extract-result-mode'.")

(define-minor-mode index-extract-result-mode
  "Minor mode for the `index-extract' results buffer."
  :lighter " IdxX")

(defun index-extract-copy-bodies ()
  "Copy just the passage bodies (no \"%%\" label lines) to the kill ring."
  (interactive)
  (let ((lines '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (looking-at-p "^%%")
          (push (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                lines))
        (forward-line 1)))
    (kill-new (string-trim (mapconcat #'identity (nreverse lines) "\n")))
    (message "Copied the passage text to the kill ring")))

(defun index-extract-save-results (file)
  "Write the results buffer to FILE."
  (interactive
   (list (read-file-name "Save extracted passages to: "
                         (or index-extract-default-directory default-directory))))
  (write-region (point-min) (point-max) file)
  (message "Saved to %s" file))

;;;; Org output

(defun index-extract--read-org-file ()
  "Read the target org file for the org-output commands."
  (read-file-name
   "Org file to append to: "
   (and index-extract-org-file (file-name-directory index-extract-org-file))
   index-extract-org-file nil
   (and index-extract-org-file (file-name-nondirectory index-extract-org-file))))

(defun index-extract--org-clean (s)
  "Make S safe for an org headline: single line, no leading stars."
  (setq s (replace-regexp-in-string "[\n\r]+" " " (or s "")))
  (setq s (replace-regexp-in-string "\\`\\*+" "" s))
  (string-trim s))

(defun index-extract--write-org (results keys files org-file top-heading)
  "Append RESULTS to ORG-FILE as one org subtree under TOP-HEADING.
KEYS and FILES are used only for the top heading's properties.  Return nil."
  (with-current-buffer (find-file-noselect org-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "\n")
    (let ((top-start (point))
          (stamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
      (insert (format "* %s\n" (index-extract--org-clean top-heading)))
      (insert ":PROPERTIES:\n")
      (insert (format ":KEYS: %s\n" (index-extract--keys-string keys)))
      (insert (format ":FILES: %d\n" (length (seq-filter #'file-readable-p files))))
      (insert (format ":PASSAGES: %d\n" (length results)))
      (insert (format ":GENERATED: %s\n" stamp))
      (insert ":END:\n")
      (dolist (pl results)
        (let* ((file (plist-get pl :file))
               (date (index-extract--file-date file))
               (datestr (and date (format-time-string "%Y-%m-%d" date)))
               (heading (or (plist-get pl :heading) (car (plist-get pl :keys))))
               (title (index-extract--org-clean
                       (if datestr (format "%s  (%s)" heading datestr) heading)))
               (body (plist-get pl :body)))
          (insert (format "** %s\n" title))
          (insert ":PROPERTIES:\n")
          (insert (format ":SOURCE: %s\n" (file-name-nondirectory file)))
          (insert (format ":LINE: %d\n" (plist-get pl :line)))
          (insert (format ":KEYS: %s\n" (index-extract--keys-string (plist-get pl :keys))))
          (when datestr (insert (format ":DATE: %s\n" datestr)))
          (insert ":END:\n")
          (unless (string-empty-p body) (insert body "\n"))))
      (save-buffer)
      (goto-char top-start)
      (when (and (fboundp 'org-mode) (not (derived-mode-p 'org-mode)))
        (org-mode))
      (when (derived-mode-p 'org-mode) (ignore-errors (org-reveal))))
    (pop-to-buffer (current-buffer)))
  nil)

;;;; Main entry points

;;;###autoload
(defun index-extract (files keys)
  "Extract passages flagged by \\index KEYS from FILES into a buffer.
Interactively, prompt first for the files to scan (you may add several
monthly subfolders and individual files) and then for the index keys.  The
passages are collected into `index-extract-result-buffer' in file order."
  (interactive
   (let* ((files (index-extract-read-files))
          (keys (index-extract-read-keys files)))
     (list files keys)))
  (index-extract--run files keys))

;;;###autoload
(defun index-extract-last-days (days keys &optional to-org)
  "Extract passages for KEYS from files dated within the last DAYS days.
The diary root (`index-extract-root-directory') is searched recursively and
files whose name holds a date in range are kept, so you need not pick folders
and a week spanning two months just works.  The ISO and day-month-name-year
filename styles are both recognised.  With a prefix
argument, write the result to an org file instead of the buffer."
  (interactive
   (let* ((days (read-number "Number of days back (including today): "
                             index-extract-default-days))
          (files (index-extract-files-last-days days)))
     (unless files (user-error "No dated files in the last %d day(s)" days))
     (list days (index-extract-read-keys files) current-prefix-arg)))
  (let ((files (index-extract-files-last-days days)))
    (unless files (user-error "No dated files in the last %d day(s)" days))
    (if to-org
        (index-extract-to-org files keys (index-extract--read-org-file)
                              (format "Last %d days: %s"
                                      days (index-extract--keys-string keys)))
      (index-extract--run files keys))))

;;;###autoload
(defun index-extract-range (start end keys &optional to-org)
  "Extract passages for KEYS from files whose date is in [START, END].
START and END are Lisp time values (read interactively).  The diary root is
searched recursively.  With a prefix argument, write to an org file."
  (interactive
   (let* ((start (index-extract--read-date "Start date"))
          (end (index-extract--read-date "End date"))
          (files (index-extract-files-in-range start end)))
     (unless files (user-error "No dated files in that range"))
     (list start end (index-extract-read-keys files) current-prefix-arg)))
  (let ((files (index-extract-files-in-range start end)))
    (unless files (user-error "No dated files in that range"))
    (if to-org
        (index-extract-to-org files keys (index-extract--read-org-file)
                              (format "%s to %s: %s"
                                      (format-time-string "%Y-%m-%d" start)
                                      (format-time-string "%Y-%m-%d" end)
                                      (index-extract--keys-string keys)))
      (index-extract--run files keys))))

;;;###autoload
(defun index-extract-to-org (files keys org-file &optional top-heading)
  "Extract passages for KEYS from FILES and append them to ORG-FILE.
The passages form one org subtree: a top-level heading (TOP-HEADING) with one
child heading per passage.  Each child carries :SOURCE:, :LINE:, :KEYS: and
:DATE: properties and the passage text as its body."
  (interactive
   (let* ((files (index-extract-read-files))
          (keys (index-extract-read-keys files))
          (org-file (index-extract--read-org-file)))
     (list files keys org-file nil)))
  (setq index-extract--last-files files
        index-extract--last-keys keys)
  (let ((all (index-extract--gather files keys)))
    (setq index-extract--last-results all)
    (index-extract--write-org
     all keys files org-file
     (or top-heading (format "Index extract: %s" (index-extract--keys-string keys))))
    (message "index-extract: wrote %d passage(s) to %s" (length all) org-file)
    all))

;;;###autoload
(defun index-extract-results-to-org (org-file)
  "Append the most recent extraction results to ORG-FILE as an org subtree."
  (interactive (list (index-extract--read-org-file)))
  (unless index-extract--last-results
    (user-error "No extraction results yet; run index-extract first"))
  (index-extract--write-org
   index-extract--last-results index-extract--last-keys index-extract--last-files
   org-file
   (format "Index extract: %s" (index-extract--keys-string index-extract--last-keys)))
  (message "index-extract: wrote %d passage(s) to %s"
           (length index-extract--last-results) org-file))

(defun index-extract--read-date (prompt)
  "Read a date with PROMPT and return it as a Lisp time value.
Uses `org-read-date' if available, otherwise reads a YYYY-MM-DD string."
  (if (fboundp 'org-read-date)
      (org-read-date nil t nil prompt)
    (let ((s (read-string (concat prompt " (YYYY-MM-DD): "))))
      (if (string-match index-extract-date-regexp s)
          (index-extract--encode (string-to-number (match-string 1 s))
                                 (string-to-number (match-string 2 s))
                                 (string-to-number (match-string 3 s)))
        (user-error "Could not parse date: %s" s)))))

;;;###autoload
(defun index-extract-again-keys ()
  "Run `index-extract' again with the same files but new keys."
  (interactive)
  (unless index-extract--last-files (user-error "No previous file list"))
  (index-extract--run index-extract--last-files
                      (index-extract-read-keys index-extract--last-files)))

;;;###autoload
(defun index-extract-again-files ()
  "Run `index-extract' again with the same keys but a new file selection."
  (interactive)
  (unless index-extract--last-keys (user-error "No previous key list"))
  (index-extract--run (index-extract-read-files) index-extract--last-keys))

(provide 'index-extract)

;;; index-extract.el ends here
