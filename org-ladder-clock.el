;;; org-ladder-clock.el --- org-clock/effort score source for org-ladder  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 3.0
;; Package-Requires: ((emacs "27.1") (org "9.0") (org-ladder "3.0"))

;;; Commentary:
;; Default score source: scans DONE tasks in `org-ladder-clock-files' and
;; awards points based on effort estimate and/or clocked time.
;;
;; Scoring rules (per task):
;;   clock <= effort  -> effort
;;   clock >  effort  -> clock
;;   clock only       -> max(clock, default-duration)
;;   effort only      -> max(effort, default-duration)
;;   neither          -> default-duration
;;
;; Habit tasks (:STYLE: habit) always use the State log regardless of
;; whether a CLOSED line is present.  Each "- CLOSING NOTE" entry is
;; one completion scored at max(effort, default-duration).
;;
;; Uses pure regexp scanning to avoid any dependency on org-element cache.
;; Loaded automatically by org-ladder.el.

;;; Code:

(require 'org)

(defgroup org-ladder-clock nil
  "org-clock/effort source for org-ladder."
  :group 'org-ladder
  :prefix "org-ladder-clock-")

(defcustom org-ladder-clock-files nil
  "Org files to scan for completed tasks.  Must be set explicitly."
  :type '(repeat file)
  :group 'org-ladder-clock)

(defcustom org-ladder-clock-default-duration 5
  "Score in minutes for tasks with neither effort nor clock data."
  :type 'integer
  :group 'org-ladder-clock)

;;; ── Pure-regexp helpers ──────────────────────────────────────────────────────
;; All helpers operate on the current buffer between BEG and END,
;; with no calls to org-element or org-map-entries.

(defun org-ladder-clock--next-heading-pos (beg)
  "Return position of the next heading after BEG, or point-max."
  (save-excursion
    (goto-char beg)
    (forward-line 1)
    (if (re-search-forward "^\\*" nil t)
        (line-beginning-position)
      (point-max))))

(defun org-ladder-clock--scan-file (file tbl)
  "Scan FILE for DONE tasks and accumulate scores into hash-table TBL.
Keys in TBL are day time keys; values are accumulated minute scores."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    ;; Walk every heading — habits may be in any TODO state
    (while (re-search-forward "^\\*+ " nil t)
          (let* ((heading-beg (line-beginning-position))
                 (heading-end (org-ladder-clock--next-heading-pos heading-beg))
                 closed-time effort-min clock-min
                 (habitp (save-excursion
                           (goto-char heading-beg)
                           (re-search-forward
                            ":STYLE:[ \t]+habit" heading-end t))))

            ;; 1. CLOSED timestamp (planning line, within ~5 lines of heading)
            (save-excursion
              (goto-char heading-beg)
              (forward-line 1)
              (let ((limit (min heading-end
                                (save-excursion (forward-line 5) (point)))))
                (when (re-search-forward
                       "CLOSED:[ \t]*\\[\\([^]]+\\)\\]" limit t)
                  (setq closed-time
                        (org-time-string-to-time (match-string 1))))))

            ;; 2. EFFORT property (used by both paths)
            (save-excursion
              (goto-char heading-beg)
              (when (re-search-forward
                     ":EFFORT:[ \t]*\\([0-9]+:[0-9]+\\)" heading-end t)
                (setq effort-min
                      (round (org-duration-to-minutes (match-string 1))))))

            (if habitp
                ;; ── Habit: score every State "DONE" log entry ────────────
                ;; Ignore CLOSED (may reflect only the last completion).
                (let ((score (max (or effort-min 0)
                                  org-ladder-clock-default-duration)))
                  (save-excursion
                    (goto-char heading-beg)
                    (while (re-search-forward
                            "^[ \t]*- CLOSING NOTE \\[\\([^\]]+\\)\\]"
                            heading-end t)
                      (let ((key (org-ladder-time-from-emacs
                                  (org-time-string-to-time (match-string 1)))))
                        (puthash key (+ (gethash key tbl 0) score) tbl)))))

              ;; ── Normal task: requires CLOSED timestamp ───────────────
              (when closed-time

                ;; 3. CLOCK lines with duration
                (save-excursion
                  (goto-char heading-beg)
                  (let ((total 0))
                    (while (re-search-forward
                            "CLOCK:.*=>[ \t]*\\([0-9]+:[0-9]+\\)" heading-end t)
                      (setq total
                            (+ total
                               (round (org-duration-to-minutes (match-string 1))))))
                    (when (> total 0)
                      (setq clock-min total))))

                ;; 4. Compute score and accumulate
                (let* ((default org-ladder-clock-default-duration)
                       (score
                        (cond
                         ((and effort-min clock-min (<= clock-min effort-min))
                          effort-min)
                         (clock-min  (max clock-min  default))
                         (effort-min (max effort-min default))
                         (t          default)))
                       (key (org-ladder-time-from-emacs closed-time)))
                  (puthash key (+ (gethash key tbl 0) score) tbl))))))))

;;; ── Source function ──────────────────────────────────────────────────────────

(defun org-ladder-clock--collect ()
  "Return ((day-key . score) ...) for all DONE tasks with a CLOSED timestamp."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (file org-ladder-clock-files)
      (when (file-exists-p file)
        (condition-case err
            (org-ladder-clock--scan-file file tbl)
          (error (message "org-ladder-clock: error in %s: %s"
                          file (error-message-string err))))))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) tbl)
      result)))

;;; ── Registration ─────────────────────────────────────────────────────────────

(with-eval-after-load 'org-ladder
  (add-to-list 'org-ladder-score-sources #'org-ladder-clock--collect))

(provide 'org-ladder-clock)
;;; org-ladder-clock.el ends here
