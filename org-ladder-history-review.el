;;; org-ladder-history-review.el --- History review functionality for org-ladder  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;;; Autoloads:

;;; Commentary:
;; This package provides history review functionality for org-ladder,
;; allowing users to view detailed status information for past months.

;;; Code:

(require 'org-ladder)

;;;###autoload
(defgroup org-ladder-history-review nil
  "Org Ladder history review functionality."
  :group 'org-ladder
  :prefix "org-ladder-history-review-")

;;;###autoload
(defcustom org-ladder-history-review-months-count 12
  "Number of months to show in history review.
Set to nil to show all available months."
  :type '(choice (integer :tag "Number of months")
                 (const :tag "All months" nil))
  :group 'org-ladder-history-review)

(defun org-ladder-history-review--format-monthly-details (year month score tier)
  "Format detailed information for a specific month.

YEAR, MONTH, SCORE, and TIER are the historical data for the month.
Returns a formatted string with detailed status information."
  (let* ((tier-info (org-ladder-get-tier-info score))
         (tier-name (nth 0 tier-info))
         (sub-tier (nth 1 tier-info))
         (total-sub (nth 2 tier-info))
         (score-to-next-tier (nth 3 tier-info))
         (score-to-next-sub (nth 4 tier-info))
         (sub-tier-progress (nth 5 tier-info))
         (sub-tier-total (nth 6 tier-info)))

    (concat
     (format "=== %d-%02d ===\n" year month)
     (format "Total Score: %d minutes\n" score)
     (format "Tier: %s\n" (capitalize (if (symbolp tier) (symbol-name tier) tier)))

     (cond
      ((eq tier-name 'legend)
       (format "Status: Legend (Achieved)\n"))
      (t
       (format "Sub-tier: %d/%d\n" sub-tier total-sub)
       (when (> score-to-next-sub 0)
         (format "Progress: %d/%d minutes\n" sub-tier-progress sub-tier-total)
         (format "To Next Level: %d minutes\n" score-to-next-sub))))

     (format "To Next Tier: %d minutes\n" (or score-to-next-tier 0))
     "\n")))

(defun org-ladder-history-review--get-history-months (count)
  "Get history months for display.

COUNT specifies how many months to return. If nil, return all months.
Returns a list of monthly entries in chronological order."
  ;; Ensure cache is loaded and up-to-date
  (org-ladder-calculate-all-scores)

  (if count
      (seq-take org-ladder--cache-data (min count (length org-ladder--cache-data)))
    org-ladder--cache-data))

;;;###autoload
(defun org-ladder-show-history-review (&optional months-count)
  "Show detailed history review of past months.

MONTHS-COUNT specifies how many months to display. If nil, uses
`org-ladder-history-review-months-count`.

Displays detailed status information for each month, similar to
`org-ladder-show-detailed-status` but for historical data.

Example usage:
  (org-ladder-show-history-review)      ; Show default number of months
  (org-ladder-show-history-review 6)    ; Show last 6 months
  (org-ladder-show-history-review nil)  ; Show all months"
  (interactive "P")

  (let* ((display-count (or months-count org-ladder-history-review-months-count))
         (history-months (org-ladder-history-review--get-history-months display-count)))

    (if (null history-months)
        (message "No history data available")

      (with-current-buffer (get-buffer-create "*Org Ladder History Review*")
        (erase-buffer)
        (insert "=== Org Ladder History Review ===\n\n")

        (if display-count
            (insert (format "Showing last %d months:\n\n" display-count))
          (insert "Showing all available months:\n\n"))

        ;; Display detailed information for each month
        (dolist (entry history-months)
          (let ((year (nth 0 entry))
                (month (nth 1 entry))
                (score (nth 2 entry))
                (tier (nth 3 entry)))
            (insert (org-ladder-history-review--format-monthly-details
                     year month score tier))))

        ;; Add summary statistics
        (when (> (length history-months) 1)
          (insert "=== Summary Statistics ===\n")
          (let* ((scores (mapcar (lambda (entry) (nth 2 entry)) history-months))
                 (avg-score (/ (apply '+ scores) (length scores)))
                 (max-score (apply 'max scores))
                 (min-score (apply 'min scores)))
            (insert (format "Average Score: %.1f minutes\n" avg-score))
            (insert (format "Highest Score: %d minutes\n" max-score))
            (insert (format "Lowest Score: %d minutes\n" min-score))
            (insert (format "Total Months: %d\n" (length history-months)))))

        (if noninteractive
            (message "%s" (buffer-string))
          (display-buffer (current-buffer)))))))

;;;###autoload
(defun org-ladder-show-history-review-all ()
  "Show detailed history review for all available months."
  (interactive)
  (org-ladder-show-history-review nil))

;;;###autoload
(defun org-ladder-show-history-review-6 ()
  "Show detailed history review for the last 6 months."
  (interactive)
  (org-ladder-show-history-review 6))

;;;###autoload
(defun org-ladder-show-history-review-12 ()
  "Show detailed history review for the last 12 months."
  (interactive)
  (org-ladder-show-history-review 12))

;; Provide the package
(provide 'org-ladder-history-review)

;;; org-ladder-history-review.el ends here