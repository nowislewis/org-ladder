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
  "Scan FILE for tasks and accumulate scores into hash-table TBL.
Keys in TBL are day time keys; values are accumulated minute scores.
Supports progressive clocking: logged CLOCK time is credited to the day
it occurred, even if the task is not yet completed."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
      (let* ((heading-beg (line-beginning-position))
             (heading-end (org-ladder-clock--next-heading-pos heading-beg))
             effort-min habitp)

        ;; 1. Check for Habit (limit search to avoid long body scanning)
        (save-excursion
          (goto-char heading-beg)
          (setq habitp (re-search-forward ":STYLE:[ \t]+habit" heading-end t)))

        ;; 2. Extract EFFORT (Support various org-duration formats like "1h", "10", "1:30")
        ;; Case insensitive to catch :Effort: or :EFFORT:
        (save-excursion
          (goto-char heading-beg)
          (let ((case-fold-search t))
            (when (re-search-forward "^[ \t]*:EFFORT:[ \t]*\\([^ \n\r\t]+\\)" heading-end t)
              (let ((effort-str (match-string 1)))
                (setq effort-min (ignore-errors
                                   (round (org-duration-to-minutes effort-str))))))))

        (let ((total-clock 0)
              closed-time)

          ;; A. ALWAYS Process CLOCK entries (Progressive Clocking for ALL tasks)
          (save-excursion
            (goto-char heading-beg)
            ;; Support both [...] and <...> timestamp formats for robust parsing
            (while (re-search-forward "^[ \t]*CLOCK:[ \t]*[[<]\\([^]>]+\\)[\]>].*=>[ \t]*\\([^ \n\r\t]+\\)" heading-end t)
              (let* ((clock-time-str (match-string 1))
                     (duration-str (match-string 2))
                     (clock-time (ignore-errors (org-time-string-to-time clock-time-str)))
                     (duration (ignore-errors (round (org-duration-to-minutes duration-str)))))
                (when (and clock-time duration)
                  (let ((clock-key (org-ladder-time-from-emacs clock-time)))
                    (setq total-clock (+ total-clock duration))
                    (puthash clock-key (+ (gethash clock-key tbl 0) duration) tbl))))))

          (if habitp
              ;; ── Habit Logic: Score per completion (Fallback if not clocked) ──
              ;; Habits usually trigger "- State DONE". If clocked, the user already got points above.
              ;; But if they just ticked it off without clocking, they should get the baseline effort score.
              ;; Since a habit can be completed many times, we give the baseline score for EVERY completion.
              ;; (Note: if they clocked AND ticked it off on the same day, they technically get both.
              ;;  This encourages clocking but rewards the raw completion action too).
              (let ((score (if effort-min 
                               (max effort-min org-ladder-clock-default-duration)
                             org-ladder-clock-default-duration)))
                (save-excursion
                  (goto-char heading-beg)
                  (while (re-search-forward "^[ \t]*- \\(?:State \"DONE\".*\\|CLOSING NOTE\\) *[[<]\\([^]>]+\\)[\]>]" heading-end t)
                    (let* ((time-str (match-string 1))
                           (time-obj (ignore-errors (org-time-string-to-time time-str))))
                      (when time-obj
                        (let ((key (org-ladder-time-from-emacs time-obj)))
                          (puthash key (+ (gethash key tbl 0) score) tbl)))))))

            ;; ── Normal Task Logic: Completion Bonus ──────────
            ;; B. Extract CLOSED time
            (save-excursion
              (goto-char heading-beg)
              (let ((limit (min heading-end (save-excursion (forward-line 10) (point)))))
                (when (re-search-forward "^[ \t]*CLOSED:[ \t]*[[<]\\([^]>]+\\)[\]>]" limit t)
                  (let ((time-str (match-string 1)))
                    (setq closed-time (ignore-errors (org-time-string-to-time time-str)))))))

            ;; C. Award Completion Bonus if the task is CLOSED
            (when closed-time
              (let* ((closed-key (org-ladder-time-from-emacs closed-time))
                     (target (max (or effort-min 0) org-ladder-clock-default-duration))
                     (bonus (if (> target total-clock) (- target total-clock) 0)))
                (when (> bonus 0)
                  (puthash closed-key (+ (gethash closed-key tbl 0) bonus) tbl))))))))))

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
