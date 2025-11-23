;;; org-ladder.el --- Gamification system for org-mode based on task completion time  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;;; Autoloads:

;;;###autoload
(defgroup org-ladder nil
  "Org Ladder gamification system."
  :group 'org
  :prefix "org-ladder-")

;;;###autoload
(defcustom org-ladder-default-duration 5
  "Default duration in minutes for tasks without effort or clock data."
  :type 'integer
  :group 'org-ladder)

;;;###autoload
(defcustom org-ladder-monthly-reset-day 1
  "Day of month when ladder resets (1-31)."
  :type 'integer
  :group 'org-ladder)

;;;###autoload
(defcustom org-ladder-retention-rate 0.1
  "Fraction of previous month's total score to retain (0.0-1.0)."
  :type 'float
  :group 'org-ladder)

;;;###autoload
(defcustom org-ladder-storage-file "~/.emacs.d/org-ladder-history.el"
  "File to store monthly reset history.
Only used for tracking monthly reset state, not for caching scores."
  :type 'file
  :group 'org-ladder)

;;;###autoload
(defcustom org-ladder-files nil
  "List of org files to scan for ladder statistics.
If nil, uses org-agenda-files."
  :type '(repeat file)
  :group 'org-ladder)

;;; Commentary:
;; This package provides a gamification system for org-mode tasks based on
;; completion time, with monthly ladder rankings and tier progression.
;;
;; Features:
;; - Real-time calculation from org-agenda files
;; - Monthly ladder reset with carry-over
;; - Tier progression with sub-tiers
;; - Integration with org-agenda
;; - Persistent storage of monthly history

;;; Code:

(defgroup org-ladder nil
  "Org Ladder gamification system."
  :group 'org
  :prefix "org-ladder-")

(defcustom org-ladder-default-duration 5
  "Default duration in minutes for tasks without effort or clock data."
  :type 'integer
  :group 'org-ladder)

(defcustom org-ladder-monthly-reset-day 1
  "Day of month when ladder resets (1-31)."
  :type 'integer
  :group 'org-ladder)

(defcustom org-ladder-retention-rate 0.1
  "Fraction of previous month's total score to retain (0.0-1.0)."
  :type 'float
  :group 'org-ladder)


(defcustom org-ladder-files nil
  "List of org files to scan for ladder statistics.
If nil, uses org-agenda-files."
  :type '(repeat file)
  :group 'org-ladder)

;; Tier configuration
(defconst org-ladder-tiers
  '((bronze   . (0   500  10))
    (silver   . (501 1200 10))
    (gold     . (1201 2100 10))
    (platinum . (2101 3200 10))
    (diamond  . (3201 4500 10))
    (legend   . (4501 nil nil)))
  "Tier configuration: (name . (min-score max-score sub-tier-count))
For legend tier, max-score and sub-tier-count are nil.")

;; State variables
(defvar org-ladder--current-score nil
  "Cached current month's total score.")

(defvar org-ladder--last-calculation-time nil
  "Timestamp of last score calculation.")

(defvar org-ladder--cache-data nil
  "Cached ladder data for all months.
Format: ((year month score tier) ...)
All data is kept in memory, no file storage needed.")

(defvar org-ladder--last-reset-month nil
  "Last month when reset was performed.
Format: (year month)")

;; Utility functions
(defun org-ladder--get-effort-minutes (task)
  "Get effort in minutes from TASK.
Return nil if no effort property found."
  (when-let* ((effort (org-element-property :EFFORT task)))
    (org-duration-to-minutes effort)))

(defun org-ladder--get-clock-minutes (task)
  "Get total clock time in minutes from TASK.
Return nil if no clock data found."
  (when-let* ((clock (org-element-property :CLOCK task)))
    (let ((total 0))
      (dolist (clock-item clock)
        (when (and (listp clock-item)
                   (eq (car clock-item) 'clock))
          (let ((duration (org-element-property :duration clock-item)))
            (when duration
              (setq total (+ total (org-duration-to-minutes duration)))))))
      (if (> total 0) total nil))))

(defun org-ladder--get-closed-time (task)
  "Get closed timestamp from TASK.
Return nil if no closed time found."
  (when-let* ((closed (org-element-property :closed task)))
    (org-timestamp-to-time closed)))

(defun org-ladder--get-month-year (time)
  "Get (year month) from TIME."
  (let ((decoded (decode-time time)))
    (list (nth 5 decoded) (nth 4 decoded))))

(defun org-ladder--hash-table-keys (hash-table)
  "Get all keys from HASH-TABLE."
  (let (keys)
    (maphash (lambda (k _v) (push k keys)) hash-table)
    keys))

(defun org-ladder--current-month-p (time)
  "Check if TIME is in current month."
  (let ((now (current-time)))
    (and (= (nth 4 (decode-time time)) (nth 4 (decode-time now)))
         (= (nth 5 (decode-time time)) (nth 5 (decode-time now))))))

(defun org-ladder--calculate-task-score (task)
  "Calculate score for a single TASK.
Rules:
- If task has effort and clock ≤ effort: use effort
- Otherwise: use clock or default duration (whichever is larger)
Return score in minutes."
  (let ((effort (org-ladder--get-effort-minutes task))
        (clock (org-ladder--get-clock-minutes task)))
    (cond
     ((and effort clock (<= clock effort))
      effort)
     (clock
      (max clock org-ladder-default-duration))
     (effort
      (max effort org-ladder-default-duration))
     (t
      org-ladder-default-duration))))

;; Main calculation function
(defun org-ladder-calculate-all-scores (&optional force-recalc)
  "Calculate scores for all months from org files.
If FORCE-RECALC is non-nil, recalculate even if cached.
Updates org-ladder--cache-data with all monthly scores.
Return current month's score."
  (when (or force-recalc
            (not org-ladder--cache-data)
            (not org-ladder--last-calculation-time)
            (> (time-to-seconds (time-since org-ladder--last-calculation-time))
               300)) ; 5 minutes cache
    (let ((monthly-scores (make-hash-table :test 'equal))
          (files (or org-ladder-files (and (boundp 'org-agenda-files) org-agenda-files))))
      (when files
        (dolist (file files)
          (when (file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (org-map-entries
               (lambda ()
                 (when-let* ((closed-time (org-ladder--get-closed-time (org-element-at-point)))
                             (done-p (member (org-get-todo-state) org-done-keywords)))
                   (let* ((month-year (org-ladder--get-month-year closed-time))
                          (score (org-ladder--calculate-task-score (org-element-at-point)))
                          (current-total (gethash month-year monthly-scores 0)))
                     (puthash month-year (+ current-total score) monthly-scores))))
               nil 'file)))))

      ;; Convert hash table to sorted list and update cache
      (setq org-ladder--cache-data
            (mapcar (lambda (month-year)
                      (let* ((score (gethash month-year monthly-scores 0))
                            (year (car month-year))
                            (month (cadr month-year))
                            (tier (car (org-ladder-get-tier-info score))))
                        (list year month score tier)))
                    (sort (org-ladder--hash-table-keys monthly-scores)
                          (lambda (a b)
                            (or (> (car a) (car b))
                                (and (= (car a) (car b)) (> (cadr a) (cadr b))))))))

      ;; Update current month score
      (let ((current-month (org-ladder--get-month-year (current-time))))
        (setq org-ladder--current-score (gethash current-month monthly-scores 0)
              org-ladder--last-calculation-time (current-time)))))
  org-ladder--current-score)

;;;###autoload
(defun org-ladder-force-refresh ()
  "Force refresh the current score cache.

This function clears the cached score and forces a recalculation from
all configured org files. Use this after completing tasks to see
immediate updates to your ladder status.

Example usage:
  (org-ladder-force-refresh)  ; Force refresh and show status
  (org-ladder-display-current-status)  ; Show updated status"
  (interactive)
  (setq org-ladder--cache-data nil
        org-ladder--current-score nil
        org-ladder--last-calculation-time nil)
  (let ((new-score (org-ladder-calculate-all-scores t)))
    (message "Org Ladder: Cache refreshed. Current score: %d minutes" new-score)
    new-score))

;; Tier calculation helper functions
(defun org-ladder--calculate-sub-tier-info (score tier-config)
  "Calculate sub-tier information for SCORE within TIER-CONFIG.

Return a list containing:
- tier-name: Symbol name of the tier
- sub-tier: Current sub-tier number (1-based)
- total-sub-tiers: Total number of sub-tiers in this tier
- score-to-next-tier: Score needed to reach next tier
- score-to-next-sub: Score needed to reach next sub-tier
- sub-tier-progress: Current progress within current sub-tier
- sub-tier-total: Total score needed for current sub-tier

TIER-CONFIG is a cons cell: (tier-name . (min-score max-score sub-tier-count))"
  (let* ((tier-name (car tier-config))
         (config (cdr tier-config))
         (min-score (nth 0 config))
         (max-score (nth 1 config))
         (sub-tier-count (nth 2 config))
         (tier-range (- max-score min-score))
         (sub-tier-range (/ (float tier-range) sub-tier-count))
         (progress (- score min-score))
         (current-sub-tier (1+ (floor (/ progress sub-tier-range))))
         (score-to-next-sub (if (< current-sub-tier sub-tier-count)
                                (ceiling (- (* current-sub-tier sub-tier-range) progress))
                              0))
         (sub-tier-progress (if (< current-sub-tier sub-tier-count)
                                (- progress (* (1- current-sub-tier) sub-tier-range))
                              sub-tier-range))
         (sub-tier-total sub-tier-range))

    ;; Ensure current sub-tier doesn't exceed maximum
    (when (> current-sub-tier sub-tier-count)
      (setq current-sub-tier sub-tier-count))

    (list tier-name current-sub-tier sub-tier-count
          (- max-score score)
          score-to-next-sub
          sub-tier-progress sub-tier-total)))

;; Tier calculation functions
(defun org-ladder-get-tier-info (score)
  "Get tier information for SCORE.

Return a list containing tier information as described in
`org-ladder--calculate-sub-tier-info`.

SCORE is the total minutes of completed tasks in current month.

Special handling for Legend tier (score >= 4501) which has no sub-tiers.
For scores below minimum tier range, defaults to Bronze tier."
  ;; Handle Legend tier (special case)
  (if (>= score 4501)
      (list 'legend nil nil nil nil nil nil)

    ;; Check regular tiers
    (catch 'found-tier
      (dolist (tier-config org-ladder-tiers)
        (let* ((tier-name (car tier-config))
               (config (cdr tier-config))
               (min-score (nth 0 config))
               (max-score (nth 1 config)))

          ;; Skip legend tier (already handled) and check score range
          (when (and (not (eq tier-name 'legend))
                     (>= score min-score)
                     (<= score max-score))
            (throw 'found-tier
              (org-ladder--calculate-sub-tier-info score tier-config)))))

      ;; Default to Bronze tier for scores below minimum
      (list 'bronze 1 10 500 50 0 50))))

(defun org-ladder-get-current-tier-info ()
  "Get current tier information.
Return (tier-name sub-tier total-sub-tiers score-to-next-tier score-to-next-sub
        sub-tier-progress sub-tier-total)."
  (let ((score (org-ladder-calculate-all-scores)))
    (org-ladder-get-tier-info score)))

;; Persistent storage (only for reset tracking)
(defun org-ladder-save-cache ()
  "Save reset tracking data to storage file."
  (with-temp-file org-ladder-storage-file
    (insert "(setq org-ladder--last-reset-month '")
    (prin1 org-ladder--last-reset-month (current-buffer))
    (insert ")\n")))

(defun org-ladder-load-cache ()
  "Load reset tracking data from storage file."
  (when (file-exists-p org-ladder-storage-file)
    (load-file org-ladder-storage-file)))

;; Monthly reset logic
(defun org-ladder-check-monthly-reset ()
  "Check if monthly reset is needed and perform it."
  (let* ((current-time (decode-time (current-time)))
         (current-year (nth 5 current-time))
         (current-month (nth 4 current-time))
         (current-day (nth 3 current-time)))

    ;; Load cache if not loaded
    (when (not org-ladder--cache-data)
      (org-ladder-load-cache))

    ;; Check if reset is needed
    (when (and (= current-day org-ladder-monthly-reset-day)
               (not (equal org-ladder--last-reset-month
                           (list current-year current-month))))
      (org-ladder-perform-monthly-reset))))

(defun org-ladder-perform-monthly-reset ()
  "Perform monthly ladder reset."
  (let* ((current-score (org-ladder-calculate-all-scores t))
         (current-time (decode-time (current-time)))
         (year (nth 5 current-time))
         (month (nth 4 current-time))
         (current-tier-info (org-ladder-get-current-tier-info))
         (carry-over (floor (* current-score org-ladder-retention-rate))))

    ;; Update reset tracking
    (setq org-ladder--last-reset-month (list year month))

    ;; Reset cache with carry-over
    (setq org-ladder--current-score carry-over
          org-ladder--last-calculation-time (current-time))

    ;; Save to persistent storage
    (org-ladder-save-cache)

    (message "Org Ladder: Monthly reset completed. Carry-over: %d minutes" carry-over)))

;; Display functions
(defun org-ladder-format-tier-display (&optional force-refresh)
  "Format current tier for display.

Returns a string showing current tier, sub-tier, and progress.
Focuses only on current sub-tier progress to avoid overwhelming users.

If FORCE-REFRESH is non-nil, force recalculation before formatting.

Examples:
- Bronze 1/10 (Current: 0/50 min)
- Silver 5/10 (Current: 25/50 min)
- 🌟 Legend (4501 minutes)"
  (let* ((tier-info (org-ladder-get-current-tier-info))
         (tier-name (nth 0 tier-info))
         (sub-tier (nth 1 tier-info))
         (total-sub (nth 2 tier-info))
         (score-to-next-sub (nth 4 tier-info))
         (sub-tier-progress (nth 5 tier-info))
         (sub-tier-total (nth 6 tier-info))
         (current-score (org-ladder-calculate-all-scores force-refresh)))

    (cond
     ((eq tier-name 'legend)
      (format " Legend (%d minutes)" current-score))
     (t
      (if (> score-to-next-sub 0)
          (format "󱊽 %s %d/%d (Current: %d/%d min)"
                  (upcase-initials (symbol-name tier-name))
                  sub-tier total-sub
                  sub-tier-progress sub-tier-total)
        (format "%s %d/%d (Complete)"
                (upcase-initials (symbol-name tier-name))
                sub-tier total-sub))))))

;;;###autoload
(defun org-ladder-display-current-status (&optional force-refresh)
  "Display current ladder status.

If FORCE-REFRESH is non-nil, force recalculation before displaying.
This is useful when you've just completed tasks and want to see
immediate updates."
  (interactive "P")
  (org-ladder-check-monthly-reset)
  (let ((tier-display (org-ladder-format-tier-display force-refresh))
        (current-score (org-ladder-calculate-all-scores force-refresh)))
    (message "Org Ladder: %s | Total: %d minutes" tier-display current-score)))

;;;###autoload
(defun org-ladder-show-detailed-status (&optional force-refresh)
  "Show detailed ladder status in a buffer.

Displays comprehensive information including:
- Current tier and sub-tier
- Total score and progress within current sub-tier
- Monthly history (last 6 months)

If FORCE-REFRESH is non-nil, force recalculation before displaying.

Opens in a dedicated buffer for detailed viewing."
  (interactive "P")
  (org-ladder-check-monthly-reset)
  (let* ((current-score (org-ladder-calculate-all-scores force-refresh))
         (tier-info (org-ladder-get-tier-info current-score))
         (tier-name (nth 0 tier-info))
         (sub-tier (nth 1 tier-info))
         (total-sub (nth 2 tier-info))
         (score-to-next-tier (nth 3 tier-info))
         (score-to-next-sub (nth 4 tier-info))
         (sub-tier-progress (nth 5 tier-info))
         (sub-tier-total (nth 6 tier-info)))

    (with-current-buffer (get-buffer-create "*Org Ladder Status*")
      (erase-buffer)
      (insert "=== Org Ladder Status ===\n\n")

      (cond
       ((eq tier-name 'legend)
        (insert (format " Current Tier: Legend\n")))
       (t
        (insert (format " Current Tier: %s %d/%d\n"
                        (capitalize (symbol-name tier-name)) sub-tier total-sub))
        (when (> score-to-next-sub 0)
          (insert (format "󰔚 Current Progress: %d/%d minutes\n"
                          sub-tier-progress sub-tier-total))
          (insert (format "󱊽 To Next Level: %d minutes\n" score-to-next-sub)))
        ))
      (insert (format "󰔓 Total Score: %d minutes\n\n" current-score))

      ;; Add monthly history
      (when org-ladder--cache-data
        (insert "=== Monthly History ===\n")
        (dolist (entry (seq-take org-ladder--cache-data 6)) ; Last 6 months
          (let ((year (nth 0 entry))
                (month (nth 1 entry))
                (score (nth 2 entry))
                (tier (nth 3 entry)))
            (insert (format "%d-%02d: %d minutes (%s)\n"
                            year month score (capitalize (symbol-name tier)))))))

      (if noninteractive
          (message "%s" (buffer-string))
        (display-buffer (current-buffer))))))

;; Integration with org-agenda
(defun org-ladder-agenda-integration ()
  "Integrate org-ladder with org-agenda.

Adds:
- Monthly reset check when agenda mode starts
- Custom agenda command 'L' with ladder status in header
- Ladder status display in agenda header"
  ;; Only integrate if org-agenda is available
  (when (featurep 'org-agenda)
    ;; Add to agenda mode hook
    (add-hook 'org-agenda-mode-hook #'org-ladder-check-monthly-reset)

    ;; Add custom agenda header
    (defun org-ladder-agenda-header ()
      "Add ladder status to agenda header."
      (let ((tier-display (org-ladder-format-tier-display))
            (current-score (org-ladder-calculate-all-scores)))
        (concat "Org Ladder: " tier-display " | Total: " (number-to-string current-score) " minutes\n")))

    ;; Initialize org-agenda-custom-commands if not defined
    (unless (boundp 'org-agenda-custom-commands)
      (setq org-agenda-custom-commands nil))

    (setq org-agenda-custom-commands
          (cons '("L" "Agenda with Ladder"
                  ((agenda "" ((org-agenda-overriding-header
                                (org-ladder-agenda-header))))
                   (alltodo "")))
                org-agenda-custom-commands))))

;; Initialize
(defun org-ladder-initialize ()
  "Initialize org-ladder system.

Performs the following setup:
- Loads reset tracking from persistent storage
- Integrates with org-agenda
- Checks if monthly reset is needed
- Displays initialization message"
  (org-ladder-load-cache)
  (org-ladder-agenda-integration)
  (org-ladder-check-monthly-reset)
  (message "Org Ladder initialized"))

;; History review integration
;;;###autoload
(defun org-ladder-load-history-review ()
  "Load history review functionality.

This function loads the history review module and makes its
functions available. Call this if you want to use history review
features without requiring the module at startup."
  (interactive)
  (require 'org-ladder-history-review)
  (message "Org Ladder History Review loaded"))

;; Provide the package
(provide 'org-ladder)

;;; org-ladder.el ends here
