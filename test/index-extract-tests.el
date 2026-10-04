;;; index-extract-tests.el --- Tests for index-extract  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Blaine Mooers

;; This file is part of index-extract and is distributed under the terms of
;; the GNU General Public License version 3 or later.

;;; Commentary:

;; ERT tests for index-extract.  Run them in batch with:
;;
;;     make test
;;
;; or directly:
;;
;;     emacs -Q --batch -L . -l index-extract.el \
;;           -l test/index-extract-tests.el -f ert-run-tests-batch-and-exit
;;
;; The tests read two fixture files under test/fixtures/.  They never touch
;; the real clock: the date tests bind `index-extract--today-midnight' to a
;; fixed day so they stay deterministic.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'index-extract)

(defvar index-extract-tests--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory holding this test file.")

(defun index-extract-tests--fixtures ()
  "Return the absolute path of the fixtures directory."
  (expand-file-name "fixtures/" index-extract-tests--dir))

(defun index-extract-tests--file (rel)
  "Return the absolute path of fixture REL."
  (expand-file-name rel (index-extract-tests--fixtures)))

(defun index-extract-tests--sep ()
  "Return the 2026-09 fixture file."
  (index-extract-tests--file "2026-09/sample-2026-09-14.tex"))

(defun index-extract-tests--oct ()
  "Return the 2026-10 fixture file."
  (index-extract-tests--file "2026-10/sample-2026-10-02.tex"))

(defun index-extract-tests--bodies (plists)
  "Return the :body strings of PLISTS."
  (mapcar (lambda (pl) (plist-get pl :body)) plists))

;;;; Passage extraction

(ert-deftest index-extract-test-stacked-key-returns-prose ()
  "Matching any stacked key returns the passage prose, not the next index."
  (let ((index-extract-skip-adjacent-index t)
        (index-extract-match-style 'exact))
    ;; "letter" is the second of the stacked keys brother/letter.
    (let ((res (index-extract--scan-file (index-extract-tests--sep) '("letter"))))
      (should (= (length res) 1))
      (should (string= (plist-get (car res) :body)
                       (concat "We traded a couple of emails about the reunion.\n"
                               "I still owe him a real reply."))))))

(ert-deftest index-extract-test-key-matches-two-passages ()
  "The key brother tags two separate passages in the September fixture."
  (let ((index-extract-match-style 'exact))
    (let ((res (index-extract--scan-file (index-extract-tests--sep) '("brother"))))
      (should (= (length res) 2))
      (should (equal (index-extract-tests--bodies res)
                     (list
                      (concat "We traded a couple of emails about the reunion.\n"
                              "I still owe him a real reply.")
                      (concat "I thought about calling my brother while I walked.\n"
                              "The weather finally cooled off.")))))))

(ert-deftest index-extract-test-multi-key-dedup ()
  "A passage tagged by two requested keys is returned once, with both keys."
  (let ((index-extract-match-style 'exact))
    (let* ((res (index-extract--scan-file (index-extract-tests--sep)
                                          '("brother" "letter")))
           (first (car res)))
      ;; Two passages: the brother/letter one (merged) and the walk/brother one.
      (should (= (length res) 2))
      (should (equal (sort (copy-sequence (plist-get first :keys)) #'string<)
                     '("brother" "letter"))))))

(ert-deftest index-extract-test-boundary-excludes-todo ()
  "The last passage stops at the TODO comment, not the itemize block."
  (let ((index-extract-match-style 'exact))
    (let* ((res (index-extract--scan-file (index-extract-tests--sep) '("brother")))
           (last-body (plist-get (car (last res)) :body)))
      (should-not (string-match-p "TODO" last-body))
      (should-not (string-match-p "itemize" last-body))
      (should-not (string-match-p "\\\\item" last-body)))))

(ert-deftest index-extract-test-passage-stops-at-section ()
  "A passage stops at the next sectioning command."
  (let ((index-extract-match-style 'exact))
    (let ((res (index-extract--scan-file (index-extract-tests--sep) '("grant"))))
      (should (= (length res) 1))
      (should (string= (plist-get (car res) :body)
                       (concat "The R01 resubmission is due at the end of the month.\n"
                               "I have to rework the aims page."))))))

(ert-deftest index-extract-test-heading-captured ()
  "Each passage records the nearest preceding section heading."
  (let ((index-extract-match-style 'exact)
        (index-extract-include-heading t))
    (let ((res (index-extract--scan-file (index-extract-tests--sep) '("grant"))))
      (should (string= (plist-get (car res) :heading) "The grant deadline")))))

;;;; Match styles

(ert-deftest index-extract-test-exact-vs-substring ()
  "Exact never matches grant against grant application; substring does."
  (let ((file (index-extract-tests--oct)))
    (let ((index-extract-match-style 'exact))
      (should (= (length (index-extract--scan-file file '("grant"))) 0)))
    (let ((index-extract-match-style 'substring))
      (should (= (length (index-extract--scan-file file '("grant"))) 1)))))

(ert-deftest index-extract-test-case-fold ()
  "Case-insensitive matching is on by default."
  (let ((index-extract-match-style 'exact)
        (index-extract-case-fold t))
    (should (= (length (index-extract--scan-file (index-extract-tests--sep)
                                                 '("BROTHER")))
               2))))

;;;; Dates in file names

(ert-deftest index-extract-test-file-date-parsing ()
  "The date is read out of the file name."
  (should (equal (index-extract--file-date "sample-2026-09-14.tex")
                 (index-extract--encode 2026 9 14)))
  (should (null (index-extract--file-date "no-date-here.tex"))))

(ert-deftest index-extract-test-files-in-range ()
  "A date range filters the fixtures and spans two monthly subfolders."
  (let ((root (index-extract-tests--fixtures))
        (index-extract-sort-files t))
    ;; September only.
    (let ((sep (index-extract-files-in-range
                (index-extract--encode 2026 9 1)
                (index-extract--encode 2026 9 30)
                root)))
      (should (= (length sep) 1))
      (should (string= (file-name-nondirectory (car sep))
                       "sample-2026-09-14.tex")))
    ;; Both months, chronological because the names sort that way.
    (let ((both (index-extract-files-in-range
                 (index-extract--encode 2026 9 1)
                 (index-extract--encode 2026 10 31)
                 root)))
      (should (= (length both) 2))
      (should (equal (mapcar #'file-name-nondirectory both)
                     '("sample-2026-09-14.tex" "sample-2026-10-02.tex"))))))

(ert-deftest index-extract-test-named-date-parsing ()
  "Day-month-name-year filenames parse to the right date."
  (should (equal (index-extract--file-date "26September2026.tex")
                 (index-extract--encode 2026 9 26)))
  (should (equal (index-extract--file-date "1August2026.tex")
                 (index-extract--encode 2026 8 1)))
  ;; Abbreviations and mixed case both work.
  (should (equal (index-extract--file-date "3Oct2026.tex")
                 (index-extract--encode 2026 10 3)))
  (should (equal (index-extract--file-date "15march2026.tex")
                 (index-extract--encode 2026 3 15)))
  ;; The ISO style still works.
  (should (equal (index-extract--file-date "2026-09-26.tex")
                 (index-extract--encode 2026 9 26)))
  ;; A name with no date returns nil.
  (should (null (index-extract--file-date "notes.tex"))))

(ert-deftest index-extract-test-named-date-range-and-last-days ()
  "Range and last-N-days selectors work on day-month-name-year filenames."
  (let ((dir (make-temp-file "index-extract-named-" t))
        (index-extract-sort-files t))
    (unwind-protect
        (progn
          (dolist (n '("20September2026.tex" "26September2026.tex" "1October2026.tex"))
            (with-temp-file (expand-file-name n dir) (insert "")))
          ;; A range that includes 26 September and 1 October but not 20 September.
          (let ((hits (index-extract-files-in-range
                       (index-extract--encode 2026 9 25)
                       (index-extract--encode 2026 10 2)
                       dir)))
            (should (equal (mapcar #'file-name-nondirectory hits)
                           '("1October2026.tex" "26September2026.tex"))))
          ;; Last 7 days ending 2 October 2026 reaches back to 26 September.
          (cl-letf (((symbol-function 'index-extract--today-midnight)
                     (lambda () (index-extract--encode 2026 10 2))))
            (should (= (length (index-extract-files-last-days 7 dir)) 2))
            ;; Two days back reaches only 1 October.
            (should (= (length (index-extract-files-last-days 2 dir)) 1))))
      (delete-directory dir t))))

(ert-deftest index-extract-test-last-days-mocked-today ()
  "Last-N-days uses a fixed today and includes the right files."
  (let ((root (index-extract-tests--fixtures)))
    (cl-letf (((symbol-function 'index-extract--today-midnight)
               (lambda () (index-extract--encode 2026 10 2))))
      ;; Just today.
      (let ((one (index-extract-files-last-days 1 root)))
        (should (= (length one) 1))
        (should (string= (file-name-nondirectory (car one))
                         "sample-2026-10-02.tex")))
      ;; 18 days back reaches 2026-09-15, which is after 2026-09-14.
      (should (= (length (index-extract-files-last-days 18 root)) 1))
      ;; 19 days back reaches 2026-09-14, so both files qualify.
      (should (= (length (index-extract-files-last-days 19 root)) 2)))))

;;;; Results buffer robustness

(ert-deftest index-extract-test-result-mode-hook-error-does-not-abort ()
  "A failing mode hook does not abort extraction or run in the scratch buffer."
  (require 'tex-mode)
  (let ((index-extract-match-style 'exact)
        (ran nil))
    (let ((fn (lambda () (setq ran t) (error "boom from a mode hook"))))
      (add-hook 'latex-mode-hook fn)
      (add-hook 'text-mode-hook fn)
      (unwind-protect
          (progn
            ;; This must not signal even though the hooks error.
            (index-extract--run (list (index-extract-tests--sep)) '("brother"))
            (with-current-buffer index-extract-result-buffer
              (should (save-excursion (goto-char (point-min))
                                      (search-forward "reunion" nil t))))
            ;; The suppressed hooks never ran.
            (should-not ran))
        (remove-hook 'latex-mode-hook fn)
        (remove-hook 'text-mode-hook fn)))))

(ert-deftest index-extract-test-bad-result-mode-falls-back ()
  "A broken result major mode falls back to fundamental-mode, not an abort."
  (let ((index-extract-match-style 'exact)
        (index-extract-result-major-mode (lambda () (error "no such mode"))))
    (index-extract--run (list (index-extract-tests--sep)) '("brother"))
    (with-current-buffer index-extract-result-buffer
      (should (save-excursion (goto-char (point-min))
                              (search-forward "reunion" nil t))))))

;;;; Org output

(ert-deftest index-extract-test-org-output-parses ()
  "The org subtree written out is readable by Org's own API."
  (skip-unless (require 'org nil t))
  (let* ((index-extract-match-style 'exact)
         (tmp (make-temp-file "index-extract-org-" nil ".org")))
    (unwind-protect
        (progn
          (index-extract-to-org (list (index-extract-tests--sep)) '("brother")
                                tmp "Brother passages")
          (with-current-buffer (find-file-noselect tmp)
            (org-mode)
            (let ((top (org-map-entries (lambda () (nth 4 (org-heading-components)))
                                        "LEVEL=1"))
                  (dates (org-map-entries
                          (lambda () (org-entry-get (point) "DATE")) "LEVEL=2")))
              ;; One top heading, two child passages.
              (should (= (length top) 1))
              (should (= (length dates) 2))
              ;; The child DATE property came from the file name.
              (should (member "2026-09-14" dates)))
            (set-buffer-modified-p nil)
            (kill-buffer)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'index-extract-tests)

;;; index-extract-tests.el ends here
