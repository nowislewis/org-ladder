;;; org-ladder-sm.el --- org-sm review scores for org-ladder  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 3.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (org-ladder "3.0"))

;;; Commentary:
;; Score source for org-sm spaced-repetition reviews.
;;
;; Scans all LOGBOOK blocks in `org-ladder-sm-files'.  Each review line
;; contributes `org-ladder-sm-score-per-review' points.
;;
;; org-sm review lines start with "- <lowercase>" + inactive timestamp:
;;   - topic  a=1.5  [2026-03-11 Wed 18:47]
;;   - cloze  :easy  [2026-03-09 Mon 00:00]
;;   - dismissed    [2026-03-11 Wed 18:47]
;;
;; org built-in entries (CLOCK:, State, CLOSING NOTE...) are all uppercase,
;; so lowercase is a reliable discriminator.  No SRS_TYPE dependency means
;; dismissed cards are counted correctly.

;;; Code:

(require 'org)

(defgroup org-ladder-sm nil
  "org-sm review score source for org-ladder."
  :group 'org-ladder
  :prefix "org-ladder-sm-")

(defcustom org-ladder-sm-files nil
  "Org files to scan for review history.  Must be set explicitly."
  :type '(repeat file)
  :group 'org-ladder-sm)

(defcustom org-ladder-sm-score-per-review 2
  "Score awarded per review event."
  :type 'number
  :group 'org-ladder-sm)

;; org-sm review line: "- <lowercase-word> ... [YYYY-MM-DD ...]"
;; Group 1 captures the date.
(defconst org-ladder-sm--review-re
  (concat "^[ \t]*- [a-z]"
          ".*"
          "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
  "Regexp matching an org-sm LOGBOOK review line.  Group 1 = YYYY-MM-DD.")

(defun org-ladder-sm--scan-file (file tbl)
  "Scan all LOGBOOK blocks in FILE; accumulate day-key scores into TBL."
  (with-current-buffer (find-file-noselect file t)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "^:LOGBOOK:" nil t)
          (let* ((lb-start (point))
                 (lb-end   (save-excursion
                             (re-search-forward "^:END:" nil t))))
            (when lb-end
              (save-excursion
                (goto-char lb-start)
                (while (re-search-forward org-ladder-sm--review-re lb-end t)
                  (let ((key (org-ladder-time-parse-iso (match-string 1))))
                    (when key
                      (puthash key
                               (+ (gethash key tbl 0)
                                  org-ladder-sm-score-per-review)
                               tbl))))))))))))

(defun org-ladder-sm--collect ()
  "Return ((day-key . score) ...) from all org-sm review LOGBOOK entries."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (file org-ladder-sm-files)
      (when (file-exists-p file)
        (condition-case err
            (org-ladder-sm--scan-file file tbl)
          (error (message "org-ladder-sm: error in %s: %s"
                          file (error-message-string err))))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) tbl)
      result)))

(with-eval-after-load 'org-ladder
  (add-to-list 'org-ladder-score-sources #'org-ladder-sm--collect))

;;;###autoload
(defun org-ladder-sm-setup-review-hook ()
  "Invalidate org-ladder cache after each org-sm review (if hook exists)."
  (interactive)
  (if (boundp 'org-sm-after-review-functions)
      (add-hook 'org-sm-after-review-functions
                (lambda (&rest _)
                  (setq org-ladder--cache nil org-ladder--cache-time nil)))
    (message "org-ladder-sm: org-sm-after-review-functions not found")))

(provide 'org-ladder-sm)
;;; org-ladder-sm.el ends here
