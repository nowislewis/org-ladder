;;; org-ladder.el --- Gamification ladder for org-mode  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 3.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;;; Commentary:
;; Core framework: time utilities, score aggregation, tier calculation,
;; monthly reset, and display.  No org-file scanning happens here.
;;
;; Scores come from sources registered in `org-ladder-score-sources'.
;; Each source is a zero-argument function returning ((TIME-KEY . SCORE) ...).
;;
;; Bundled sources:
;;   org-ladder-clock.el  — DONE tasks via effort/clock  (loaded by default)
;;   org-ladder-sm.el     — org-sm review history        (opt-in)
;;
;; TIME-KEY constructors:
;;   (org-ladder-time-key-day   YEAR MONTH DAY)
;;   (org-ladder-time-key-month YEAR MONTH)
;;   (org-ladder-time-key-week  YEAR WEEK)
;;
;; All keys are aggregated to month level before tier calculation.

;;; Code:

;;; ── Customisation ────────────────────────────────────────────────────────────

(defgroup org-ladder nil
  "Org Ladder gamification system."
  :group 'org
  :prefix "org-ladder-")

(defcustom org-ladder-score-sources nil
  "List of zero-argument functions, each returning ((TIME-KEY . SCORE) ...).
TIME-KEY is produced by `org-ladder-time-key-day', `-month', or `-week'.
All scores are aggregated to month granularity before tier calculation."
  :type '(repeat function)
  :group 'org-ladder)

(defcustom org-ladder-retention-rate 0.1
  "Fraction of last month's score carried over on monthly reset (0.0–1.0)."
  :type 'float
  :group 'org-ladder)

(defcustom org-ladder-monthly-reset-day 1
  "Day of month (1–31) on which the monthly reset occurs."
  :type 'integer
  :group 'org-ladder)

(defcustom org-ladder-storage-file
  (expand-file-name "org-ladder-history.el" user-emacs-directory)
  "File used to persist the last-reset month across Emacs sessions."
  :type 'file
  :group 'org-ladder)

;;; ── Tier configuration ───────────────────────────────────────────────────────

(defconst org-ladder-tiers
  '((bronze   .  (0    500  10))
    (silver   .  (501  1200 10))
    (gold     .  (1201 2100 10))
    (platinum .  (2101 3200 10))
    (diamond  .  (3201 4500 10))
    (legend   .  (4501 nil  nil)))
  "Alist of (NAME . (MIN MAX SUB-COUNT)).  Legend tier has no upper bound.")

;;; ── State ────────────────────────────────────────────────────────────────────

(defvar org-ladder--cache nil
  "Alist of ((year month) . score) for every month seen; nil when stale.")

(defvar org-ladder--cache-time nil
  "Emacs time of last cache computation.")

(defvar org-ladder--last-reset-month nil
  "(year month) of the most recent monthly reset.")

;;; ── Time key API ─────────────────────────────────────────────────────────────
;;
;; Three-element lists encode granularity via sentinel values:
;;   Day   (year month day)   month > 0, day > 0
;;   Month (year month 0)     month > 0, day = 0
;;   Week  (year 0 week)      month = 0  (day field holds ISO week number)

(defun org-ladder-time-key-day (year month day)
  "Return a day-granularity time key (YEAR MONTH DAY)."
  (list year month day))

(defun org-ladder-time-key-month (year month)
  "Return a month-granularity time key (YEAR MONTH 0)."
  (list year month 0))

(defun org-ladder-time-key-week (year week)
  "Return an ISO-week time key (YEAR 0 WEEK)."
  (list year 0 week))

(defun org-ladder-time-key-granularity (key)
  "Return `day', `month', or `week' for time KEY."
  (cond ((= (nth 1 key) 0) 'week)
        ((= (nth 2 key) 0) 'month)
        (t                 'day)))

(defun org-ladder-time-key-to-month (key)
  "Return (YEAR MONTH) for KEY.  Week keys resolve to their ISO Monday's month."
  (pcase (org-ladder-time-key-granularity key)
    ((or 'day 'month)
     (list (nth 0 key) (nth 1 key)))
    ('week
     (let* ((year  (nth 0 key))
            (week  (nth 2 key))
            (jan4  (encode-time 0 0 0 4 1 year))
            (j-dow (nth 6 (decode-time jan4)))
            (w1-mon (time-subtract
                     jan4 (seconds-to-time
                           (* (mod (1- (if (= j-dow 0) 7 j-dow)) 7) 86400))))
            (monday (time-add w1-mon
                              (seconds-to-time (* (1- week) 7 86400))))
            (d (decode-time monday)))
       (list (nth 5 d) (nth 4 d))))))

(defun org-ladder-time-parse-iso (string)
  "Return a day key for the first YYYY-MM-DD found in STRING, or nil."
  (when (string-match
         "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" string)
    (list (string-to-number (match-string 1 string))
          (string-to-number (match-string 2 string))
          (string-to-number (match-string 3 string)))))

(defun org-ladder-time-encode (year month day)
  "Encode YEAR MONTH DAY to an Emacs time value."
  (encode-time 0 0 0 day month year))

(defun org-ladder-time-from-emacs (time)
  "Convert Emacs TIME to a day key (YEAR MONTH DAY)."
  (let ((d (decode-time time)))
    (list (nth 5 d) (nth 4 d) (nth 3 d))))

(defun org-ladder-time-today ()
  "Return today as a day key."
  (org-ladder-time-from-emacs (current-time)))

(defun org-ladder-time-this-week ()
  "Return the Monday of the current ISO week as a day key."
  (org-ladder-time-truncate (org-ladder-time-today) 'week))

(defun org-ladder-time-truncate (key granularity)
  "Truncate KEY to GRANULARITY (`day', `week', `month', or `year')."
  (pcase granularity
    ('day   key)
    ('month (apply #'org-ladder-time-key-month (org-ladder-time-key-to-month key)))
    ('year  (org-ladder-time-key-month (nth 0 key) 1))
    ('week
     (let* ((year (nth 0 key)) (month (nth 1 key)) (day (nth 2 key))
            (t0   (org-ladder-time-encode year month day))
            (dow  (nth 6 (decode-time t0)))
            (mon  (time-subtract t0 (seconds-to-time (* (mod (1- dow) 7) 86400))))
            (md   (decode-time mon))
            (my   (nth 5 md))
            (jan4 (encode-time 0 0 0 4 1 my))
            (j-dow (nth 6 (decode-time jan4)))
            (w1-mon (time-subtract
                     jan4 (seconds-to-time
                           (* (mod (1- (if (= j-dow 0) 7 j-dow)) 7) 86400))))
            (week (1+ (floor (/ (float-time (time-subtract mon w1-mon))
                                (* 7 86400))))))
       (org-ladder-time-key-week my week)))))

(defun org-ladder-time-filter-range (entries start end)
  "Keep only ENTRIES whose time key falls within day keys START..END."
  (let ((s (apply #'org-ladder-time-encode start))
        (e (apply #'org-ladder-time-encode end)))
    (seq-filter
     (lambda (entry)
       (let ((rep (pcase (org-ladder-time-key-granularity (car entry))
                    ('day   (apply #'org-ladder-time-encode (car entry)))
                    ('month (org-ladder-time-encode
                             (nth 0 (car entry)) (nth 1 (car entry)) 1))
                    ('week  (apply #'org-ladder-time-encode
                                   (org-ladder-time-truncate
                                    (apply #'org-ladder-time-key-day
                                           (append (org-ladder-time-key-to-month
                                                    (car entry))
                                                   '(1)))
                                    'week))))))
         (and (not (time-less-p rep s)) (not (time-less-p e rep)))))
     entries)))

(defun org-ladder-time-group-scores (entries granularity)
  "Aggregate ENTRIES by GRANULARITY; return a chronologically sorted alist."
  (let ((buckets (make-hash-table :test 'equal)))
    (dolist (entry entries)
      ;; Normalise to a day key before truncating (needed for 'week)
      (let* ((key     (car entry))
             (day-key (pcase (org-ladder-time-key-granularity key)
                        ('day key)
                        (_    (apply #'org-ladder-time-key-day
                                     (append (org-ladder-time-key-to-month key)
                                             '(1))))))
             (bucket (org-ladder-time-truncate day-key granularity)))
        (puthash bucket (+ (gethash bucket buckets 0) (cdr entry)) buckets)))
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) buckets)
      (sort result (lambda (a b)
                     (or (< (nth 0 (car a)) (nth 0 (car b)))
                         (and (= (nth 0 (car a)) (nth 0 (car b)))
                              (or (< (nth 1 (car a)) (nth 1 (car b)))
                                  (and (= (nth 1 (car a)) (nth 1 (car b)))
                                       (< (nth 2 (car a)) (nth 2 (car b))))))))))))

;;; ── Score aggregation ────────────────────────────────────────────────────────

(defun org-ladder--month-key (time)
  "Return (year month) from Emacs TIME."
  (let ((d (decode-time time)))
    (list (nth 5 d) (nth 4 d))))

(defun org-ladder--normalize-entries (raw-entries)
  "Normalize RAW-ENTRIES to ((year month) . score) alist.
Accepts both legacy (y m) keys and time keys of any granularity."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (entry raw-entries)
      (let* ((k  (car entry))
             (ym (if (= (length k) 2) k
                   (org-ladder-time-key-to-month k))))
        (puthash ym (+ (gethash ym tbl 0) (cdr entry)) tbl)))
    (let (rows)
      (maphash (lambda (ym s) (push (cons ym s) rows)) tbl)
      rows)))

(defun org-ladder--sort-monthly (rows)
  "Sort monthly score ROWS descending by (year month)."
  (sort rows (lambda (a b)
               (or (> (caar a) (caar b))
                   (and (= (caar a) (caar b))
                        (> (cadar a) (cadar b)))))))

(defun org-ladder-calculate-scores-by-source ()
  "Return scores broken down by source.
Result: alist of (SOURCE-FN . MONTHLY-ROWS) where MONTHLY-ROWS is
a sorted alist of ((year month) . score)."
  (mapcar (lambda (src)
            (cons src
                  (condition-case err
                      (org-ladder--sort-monthly
                       (org-ladder--normalize-entries (funcall src)))
                    (error
                     (message "org-ladder: source %s: %s" src
                              (error-message-string err))
                     nil))))
          org-ladder-score-sources))

(defun org-ladder-calculate-scores (&optional force)
  "Return alist ((year month) . score) for every month with data.
Cached for 5 minutes; pass FORCE to bypass."
  (when (or force
            (null org-ladder--cache)
            (null org-ladder--cache-time)
            (> (float-time (time-since org-ladder--cache-time)) 300))
    (let ((tbl (make-hash-table :test 'equal)))
      (dolist (src org-ladder-score-sources)
        (condition-case err
            (dolist (entry (funcall src))
              (let* ((k  (car entry))
                     (ym (if (= (length k) 2) k
                           (org-ladder-time-key-to-month k))))
                (puthash ym (+ (gethash ym tbl 0) (cdr entry)) tbl)))
          (error (message "org-ladder: source %s: %s" src
                          (error-message-string err)))))
      (let (rows)
        (maphash (lambda (ym s) (push (cons ym s) rows)) tbl)
        (setq org-ladder--cache (org-ladder--sort-monthly rows)
              org-ladder--cache-time (current-time))))
    org-ladder--cache)
  org-ladder--cache)

(defun org-ladder-current-score ()
  "Return the current month's total score (0 if none)."
  (or (cdr (assoc (org-ladder--month-key (current-time))
                  (org-ladder-calculate-scores)))
      0))

;;;###autoload
(defun org-ladder-force-refresh ()
  "Invalidate cache and recompute scores."
  (interactive)
  (setq org-ladder--cache nil org-ladder--cache-time nil)
  (message "Org Ladder: score %d" (org-ladder-current-score)))

;;; ── Tier calculation ─────────────────────────────────────────────────────────

(defun org-ladder-get-tier-info (score)
  "Return (NAME SUB NSUBS TO-TIER TO-SUB PROGRESS SUB-SIZE) for SCORE."
  (if (>= score 4501)
      (list 'legend nil nil nil nil nil nil)
    (catch 'found
      (dolist (tc org-ladder-tiers)
        (let* ((min   (nth 0 (cdr tc)))
               (max   (nth 1 (cdr tc)))
               (nsubs (nth 2 (cdr tc))))
          (when (and max (>= score min) (<= score max))
            (let* ((sub-sz   (/ (float (- max min)) nsubs))
                   (progress (- score min))
                   (sub      (min nsubs (1+ (floor (/ progress sub-sz))))))
              (throw 'found
                (list (car tc) sub nsubs
                      (- max score)
                      (if (< sub nsubs)
                          (ceiling (- (* sub sub-sz) progress)) 0)
                      (- progress (* (1- sub) sub-sz))
                      sub-sz))))))
      (list 'bronze 1 10 500 50 0 50))))

;;; ── Monthly reset ────────────────────────────────────────────────────────────

(defun org-ladder--save-reset ()
  "Persist `org-ladder--last-reset-month' to `org-ladder-storage-file'."
  (with-temp-file org-ladder-storage-file
    (insert "(setq org-ladder--last-reset-month '")
    (prin1 org-ladder--last-reset-month (current-buffer))
    (insert ")\n")))

(defun org-ladder--load-reset ()
  "Load persisted reset state if the storage file exists."
  (when (file-exists-p org-ladder-storage-file)
    (load-file org-ladder-storage-file)))

(defun org-ladder-check-monthly-reset ()
  "Trigger a monthly reset when today matches `org-ladder-monthly-reset-day'."
  (org-ladder--load-reset)
  (let* ((d     (decode-time (current-time)))
         (year  (nth 5 d)) (month (nth 4 d)) (day (nth 3 d))
         (ym    (list year month)))
    (when (and (= day org-ladder-monthly-reset-day)
               (not (equal org-ladder--last-reset-month ym)))
      (let ((carry (floor (* (org-ladder-current-score) org-ladder-retention-rate))))
        (setq org-ladder--last-reset-month ym
              org-ladder--cache            nil
              org-ladder--cache-time       nil)
        (org-ladder--save-reset)
        (message "Org Ladder: monthly reset — carry-over %d" carry)))))

;;; ── Display ──────────────────────────────────────────────────────────────────

(defun org-ladder--tier-string (score)
  "Return a compact display string for SCORE."
  (pcase-let ((`(,name ,sub ,nsubs ,_tt ,_ts ,prog ,tot)
               (org-ladder-get-tier-info score)))
    (if (eq name 'legend)
        (format "Legend (%d)" score)
      (format "%s %d/%d (%d/%d min)"
              (capitalize (symbol-name name))
              sub nsubs (floor prog) (floor tot)))))

;;;###autoload
(defun org-ladder-status ()
  "Show current tier and score in the minibuffer."
  (interactive)
  (org-ladder-check-monthly-reset)
  (message "Org Ladder: %s" (org-ladder--tier-string (org-ladder-current-score))))

;;;###autoload
(defun org-ladder-show-details ()
  "Show tier, score, source breakdown, and monthly history in a buffer."
  (interactive)
  (org-ladder-check-monthly-reset)
  (let* ((score      (org-ladder-current-score))
         (info       (org-ladder-get-tier-info score))
         (cur-ym     (org-ladder--month-key (current-time)))
         (by-source  (org-ladder-calculate-scores-by-source)))
    (pcase-let ((`(,name ,sub ,nsubs ,to-t ,to-s ,prog ,tot) info))
      (with-current-buffer (get-buffer-create "*Org Ladder*")
        (erase-buffer)

        ;; ── Tier & total ──────────────────────────────────────────────────
        (if (eq name 'legend)
            (insert (format "Tier: Legend  Score: %d\n\n" score))
          (insert (format "Tier:  %s %d/%d\n" (capitalize (symbol-name name)) sub nsubs))
          (insert (format "Score: %d  Progress: %d/%d  next-sub: %d  next-tier: %d\n\n"
                          score (floor prog) (floor tot) to-s to-t)))

        ;; ── Source breakdown for current month ───────────────────────────
        (insert "Sources (this month)\n")
        (dolist (src-entry by-source)
          (let* ((src       (car src-entry))
                 (monthly   (cdr src-entry))
                 (src-score (or (cdr (assoc cur-ym monthly)) 0))
                 (src-name  (symbol-name src))
                 ;; Strip internal prefix for readability: foo--collect -> foo
                 (label     (replace-regexp-in-string "--.*$" "" src-name)))
            (insert (format "  %-30s %5d\n" label src-score))))
        (insert "\n")

        ;; ── Monthly history ───────────────────────────────────────────────
        (insert "Monthly history\n")
        (dolist (entry (seq-take (org-ladder-calculate-scores) 12))
          (let* ((ym    (car entry))
                 (sc    (cdr entry))
                 ;; Per-source scores for this month, as "src:N" annotations
                 (breakdown
                  (mapconcat
                   (lambda (src-entry)
                     (let* ((src      (car src-entry))
                            (monthly  (cdr src-entry))
                            (s        (or (cdr (assoc ym monthly)) 0))
                            (label    (replace-regexp-in-string
                                       "--.*$" "" (symbol-name src))))
                       (format "%s:%d" label s)))
                   by-source "  ")))
            (insert (format "  %d-%02d  %5d  %-10s  %s\n"
                            (car ym) (cadr ym) sc
                            (capitalize (symbol-name
                                         (car (org-ladder-get-tier-info sc))))
                            breakdown))))
        (display-buffer (current-buffer))))))

;;; ── Load default source ──────────────────────────────────────────────────────

(require 'org-ladder-clock)

(provide 'org-ladder)
;;; org-ladder.el ends here
