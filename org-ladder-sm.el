;;; org-ladder-sm.el --- org-sm review scores for org-ladder  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 3.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (org-ladder "3.0"))

;;; Commentary:
;; Score source for org-sm (org-inc) spaced-repetition reviews.
;;
;; org-sm records each review as a line inside the heading's :LOGBOOK: drawer:
;;
;;   :LOGBOOK:
;;   - topic  a=1.5  [2026-03-11 Wed 18:47]
;;   - cloze  :easy  [2026-03-09 Mon 00:00]
;;   :END:
;;
;; This source scans all files in `org-ladder-sm-files' for headings that
;; carry a SRS_TYPE property, then counts every such LOGBOOK line as one
;; review, contributing `org-ladder-sm-score-per-review' points.
;;
;; Uses pure regexp scanning -- no org-element cache dependency.
;;
;; Usage:  (require 'org-ladder-sm)   ; self-registering

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

;;; ── Regexp ───────────────────────────────────────────────────────────────────

;; Matches a logbook review line, e.g.:
;;   - topic  a=1.5  [2026-03-11 Wed 18:47]
;;   - cloze  :easy  [2026-03-09 Mon 00:00]
(defconst org-ladder-sm--review-re
  (concat "^[ \t]*- \\(?:topic\\|cloze\\)"   ; review type
          ".*"                                ; grade/afactor
          "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)") ; date capture
  "Regexp matching an org-sm LOGBOOK review line.
Group 1 captures the YYYY-MM-DD date.")

;;; ── Scanner ──────────────────────────────────────────────────────────────────

(defun org-ladder-sm--scan-file (file tbl)
  "Scan FILE for org-sm review entries; accumulate day-key scores into TBL."
  (with-current-buffer (find-file-noselect file t)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        ;; Find every heading that has SRS_TYPE in its property drawer
        (while (re-search-forward "^:SRS_TYPE:" nil t)
          ;; Now look for a :LOGBOOK: block near this heading.
          ;; It may appear before or after the :PROPERTIES: drawer.
          ;; Search forward to the next heading boundary.
          (let* ((section-end (save-excursion
                                (or (re-search-forward "^\\*" nil t)
                                    (point-max))))
                 (lb-start (save-excursion
                              (re-search-forward "^:LOGBOOK:" section-end t)))
                 (lb-end   (when lb-start
                             (save-excursion
                               (goto-char lb-start)
                               (re-search-forward "^:END:" section-end t)))))
            (when (and lb-start lb-end)
              (save-excursion
                (goto-char lb-start)
                (while (re-search-forward org-ladder-sm--review-re lb-end t)
                  (let* ((date-str (match-string 1))
                         (key      (org-ladder-time-parse-iso date-str)))
                    (when key
                      (puthash key
                               (+ (gethash key tbl 0)
                                  org-ladder-sm-score-per-review)
                               tbl))))))))))))

;;; ── Source function ──────────────────────────────────────────────────────────

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

;;; ── Registration ─────────────────────────────────────────────────────────────

(add-to-list 'org-ladder-score-sources #'org-ladder-sm--collect)

;;; ── Optional cache invalidation ──────────────────────────────────────────────

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
