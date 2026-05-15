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

(require 'cl-lib)
(require 'seq)
(require 'calendar)

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
  '((bronze   .  (0    499  10))
    (silver   .  (500  1199 10))
    (gold     .  (1200 2099 10))
    (platinum .  (2100 3199 10))
    (diamond  .  (3200 4499 10))
    (legend   .  (4500 nil  nil)))
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

(defun org-ladder--sort-monthly (rows)
  "Sort monthly score ROWS descending by (year month)."
  (sort rows (lambda (a b)
               (or (> (caar a) (caar b))
                   (and (= (caar a) (caar b))
                        (> (cadar a) (cadar b)))))))

(defun org-ladder--collect-raw ()
  "Return a flat list of all raw (TIME-KEY . SCORE) entries from every source.
Errors in individual sources are logged and skipped."
  (let (result)
    (dolist (src org-ladder-score-sources)
      (condition-case err
          (setq result (nconc result (funcall src)))
        (error (message "org-ladder: source %s: %s" src
                        (error-message-string err)))))
    result))

(defun org-ladder--normalize-entries (raw-entries)
  "Aggregate RAW-ENTRIES into a monthly alist ((year month) . score).
Accepts time keys of any granularity."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (entry raw-entries)
      (let ((ym (org-ladder-time-key-to-month (car entry))))
        (puthash ym (+ (gethash ym tbl 0) (cdr entry)) tbl)))
    (let (rows)
      (maphash (lambda (ym s) (push (cons ym s) rows)) tbl)
      rows)))

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

(defun org-ladder--collect-daily ()
  "Return a hash-table of day-key -> score across all sources."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (e (org-ladder--collect-raw))
      (when (eq (org-ladder-time-key-granularity (car e)) 'day)
        (puthash (car e) (+ (gethash (car e) tbl 0) (cdr e)) tbl)))
    tbl))

(defun org-ladder-calculate-scores (&optional force)
  "Return alist ((year month) . score) for every month with data.
Cached for 5 minutes; pass FORCE to bypass."
  (when (or force
            (null org-ladder--cache)
            (null org-ladder--cache-time)
            (> (float-time (time-since org-ladder--cache-time)) 300))
    (setq org-ladder--cache
          (org-ladder--sort-monthly
           (org-ladder--normalize-entries (org-ladder--collect-raw)))
          org-ladder--cache-time (current-time)))
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
  (if (>= score 4500)
      (list 'legend nil nil nil nil nil nil)
    (catch 'found
      (dolist (tc org-ladder-tiers)
        (let* ((min   (nth 0 (cdr tc)))
               (max   (nth 1 (cdr tc)))
               (nsubs (nth 2 (cdr tc))))
          (when (and max (>= score min) (<= score max))
            (let* ((sub-sz   (/ (1+ (- max min)) nsubs))
                   (progress (- score min))
                   (sub      (min nsubs (1+ (/ progress sub-sz)))))
              (throw 'found
                (list (car tc) sub nsubs
                      (- max score)
                      (if (< sub nsubs) (- (* sub sub-sz) progress) (- max score))
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

;;; ── Display helpers ──────────────────────────────────────────────────────────

(defun org-ladder--progress-bar (current total width)
  "Return a progress bar string of WIDTH chars for CURRENT/TOTAL."
  (let ((filled (max 0 (min width (round (* width (/ (float current) total)))))))
    (concat "[" (make-string filled ?█) (make-string (- width filled) ?░) "]")))

(defun org-ladder--rank-name (score)
  "Return a short rank string like \"Gold 6\" or \"Legend\" for SCORE."
  (pcase-let ((`(,name ,sub) (org-ladder-get-tier-info score)))
    (if (eq name 'legend) "Legend"
      (format "%s %d" (capitalize (symbol-name name)) sub))))

(defun org-ladder--tier-string (score)
  "Return a compact display string for SCORE."
  (pcase-let ((`(,name ,sub ,nsubs ,_to-t ,_to-s ,prog ,sub-sz)
               (org-ladder-get-tier-info score)))
    (if (eq name 'legend)
        (format "Legend (%d)" score)
      (let* ((tier-entry (assoc name org-ladder-tiers))
             (tier-min   (nth 0 (cdr tier-entry)))
             (tier-max   (nth 1 (cdr tier-entry))))
        (format "\uf091 %s %d/%d  sub %s  tier %s  %d/%d"
                (capitalize (symbol-name name)) sub nsubs
                (org-ladder--progress-bar prog sub-sz 10)
                (org-ladder--progress-bar (- score tier-min) (- tier-max tier-min) 10)
                score tier-max)))))

(defun org-ladder--bar-chart (values labels title &optional height)
  "Insert a vertical bar chart for VALUES with LABELS and TITLE.
HEIGHT is the number of rows (default 8)."
  (let* ((height (or height 8))
         (max-v  (apply #'max (cons 1 values)))
         (width  3)
         (pad    "  "))
    (insert (format "** %s  (max: %d)\n" title max-v))
    (dotimes (row height)
      (let ((threshold (/ (* (- height row) max-v) (float height))))
        (insert (format "%s%4d " pad (round threshold)))
        (dolist (v values)
          (insert (if (> v threshold) (make-string width ?█) (make-string width ? )))
          (insert " "))
        (insert "\n")))
    (insert (format "%s     %s\n" pad
                    (mapconcat (lambda (_) (make-string width ?─)) values "─")))
    (insert (format "%s     %s\n" pad
                    (mapconcat (lambda (v) (format (format "%%-%ds " width) v)) values "")))
    (insert (format "%s     %s\n\n" pad
                    (mapconcat (lambda (l) (format (format "%%-%ds " width) l)) labels "")))))

;;; ── Commands ─────────────────────────────────────────────────────────────────

;;;###autoload
(defun org-ladder-status ()
  "Show current tier and score in the minibuffer."
  (interactive)
  (org-ladder-check-monthly-reset)
  (message "Org Ladder: %s" (org-ladder--tier-string (org-ladder-current-score))))

;;;###autoload
(defun org-ladder--src-label (src)
  "Return a short display name for source function SRC."
  (replace-regexp-in-string "--.*$" "" (symbol-name src)))

(defun org-ladder-show-details (&optional months)
  "Show a compact Org Ladder dashboard.
This version is intentionally plain-text (not `org-mode') so custom faces
for the GitHub-style heatmap are preserved reliably."
  (interactive "P")
  (org-ladder-check-monthly-reset)
  (let* ((score      (org-ladder-current-score))
         (info       (org-ladder-get-tier-info score))
         (cur-ym     (org-ladder--month-key (current-time)))
         (cur-year   (car cur-ym))
         (cur-month  (cadr cur-ym))
         (by-source  (org-ladder-calculate-scores-by-source))
         (all-scores (org-ladder-calculate-scores))
         (history    (cond ((null months)  (seq-take all-scores 12))
                           ((listp months) all-scores)
                           (t              (seq-take all-scores months))))
         (daily-tbl  (org-ladder--collect-daily)))
    (pcase-let ((`(,name ,sub ,nsubs ,_to-t ,_to-s ,_prog ,_tot) info))
      (with-current-buffer (get-buffer-create "*Org Ladder*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (fundamental-mode)

          ;; ── Artifact progress ──────────────────────────────────────────
          (insert (propertize "🏺 Artifact\n" 'face '(:weight bold :height 1.2)))
          (if (and (fboundp 'org-ladder-art--get-encoded)
                   (fboundp 'org-ladder-art--decrypt)
                   (fboundp 'org-ladder-art--render-buffer))
              (let* ((ym-str (format "%04d-%02d" cur-year cur-month))
                     (encoded (org-ladder-art--get-encoded ym-str)))
                (if encoded
                    (org-ladder-art--render-buffer
                     ym-str score (org-ladder-art--decrypt encoded) "Current Season")
                  (insert (format "  no artifact file for %s\n\n" ym-str))))
            (insert "  artifact module not loaded. Run M-x load-library RET org-ladder-art RET\n\n"))

          ;; ── GitHub style 14-day heatmap ────────────────────────────────
          (insert (propertize "🔥 Activity\n" 'face '(:weight bold :height 1.2)))
          (let* ((today-time (current-time))
                 (streak 0)
                 (best-day 0))
            ;; streak: if today is empty, yesterday can still be the active streak
            (let ((offset (if (> (gethash (org-ladder-time-from-emacs today-time) daily-tbl 0) 0) 0 1)))
              (catch 'done
                (dotimes (i 30)
                  (let* ((t-day (time-subtract today-time (seconds-to-time (* (+ i offset) 86400))))
                         (sc (gethash (org-ladder-time-from-emacs t-day) daily-tbl 0)))
                    (if (> sc 0)
                        (setq streak (1+ streak))
                      (throw 'done t))))))
            ;; best day in current month
            (dotimes (i (nth 3 (decode-time today-time)))
              (setq best-day (max best-day
                                  (gethash (list cur-year cur-month (1+ i)) daily-tbl 0))))
            ;; 14-day green squares, two rows of seven days
            (insert "  ")
            (cl-loop for i from 13 downto 0 do
                     (let* ((t-day (time-subtract today-time (seconds-to-time (* i 86400))))
                            (sc (gethash (org-ladder-time-from-emacs t-day) daily-tbl 0))
                            (color (cond ((= sc 0)   "#30363D")
                                         ((< sc 30)  "#0E4429")
                                         ((< sc 90)  "#006D32")
                                         ((< sc 180) "#26A641")
                                         (t          "#39D353"))))
                       (insert (propertize "■ " 'face `(:foreground ,color))))
                     (when (= i 7) (insert "\n  ")))
            (insert (format "\n  streak: %d days    best day: %d min\n\n" streak best-day)))

          ;; ── Tier / projection ──────────────────────────────────────────
          (insert (propertize "📈 Rank\n" 'face '(:weight bold :height 1.2)))
          (let* ((day-now     (nth 3 (decode-time (current-time))))
                 (days-total  (calendar-last-day-of-month cur-month cur-year))
                 (avg-daily   (if (> day-now 0) (/ (float score) day-now) 0))
                 (eom-score   (round (* avg-daily days-total)))
                 (eom-rank    (org-ladder--rank-name eom-score))
                 (today-score (gethash (org-ladder-time-today) daily-tbl 0))
                 (today-thresholds '((0 . "💤 idle / 待机中")
                                     (15 . "✨ spark / 火花点燃")
                                     (30 . "🔥 warm-up / 系统预热")
                                     (60 . "🚀 active / 引擎启动")
                                     (90 . "🎯 locked-in / 锁定目标")
                                     (120 . "🧠 focused / 进入航道")
                                     (180 . "🌊 deep work / 深潜作业")
                                     (240 . "⚡ overdrive / 推进过载")
                                     (360 . "🦸 hero mode / 英雄模式")
                                     (480 . "🏆 legendary / 传说降临")
                                     (999999 . "🌌 mythic ascension / 神话升格")))
                 (today-state (catch 'state
                                (let ((prev (cdar today-thresholds)))
                                  (dolist (entry (cdr today-thresholds) prev)
                                    (when (< today-score (car entry))
                                      (throw 'state prev))
                                    (setq prev (cdr entry))))))
                 (today-next (catch 'next
                               (dolist (entry today-thresholds 999999)
                                 (when (> (car entry) today-score)
                                   (throw 'next (car entry))))))
                 (today-bar (if (>= today-next 999999)
                                (org-ladder--progress-bar 1 1 10)
                              (org-ladder--progress-bar today-score today-next 10))))
            (insert (format "  TODAY   %s\n" today-state))
            (insert (format "          %s %d/%s min\n\n"
                            today-bar today-score
                            (if (>= today-next 999999) "∞" (number-to-string today-next))))
            (insert (format "  SEASON  %s %s · %d min\n"
                            (capitalize (symbol-name name))
                            (if (eq name 'legend) "" (format "%d/%d" sub nsubs))
                            score))
            (insert (format "  TRACK   %.0f/day → %s\n\n" avg-daily eom-rank)))

          ;; ── History ────────────────────────────────────────────────────
          (insert (propertize "📅 History\n" 'face '(:weight bold :height 1.2)))
          (insert "  Month    Score   Rank\n")
          (insert "  ─────────────────────────\n")
          (dolist (entry history)
            (let ((ym (car entry)) (sc (cdr entry)))
              (insert (format "  %d-%02d  %5d   %s\n"
                              (car ym) (cadr ym) sc (org-ladder--rank-name sc)))))

          (goto-char (point-min))
          (setq buffer-read-only t))
        (display-buffer (current-buffer))))))

;;; ── Load default source ──────────────────────────────────────────────────────

(require 'org-ladder-clock)

(provide 'org-ladder)
;;; org-ladder.el ends here
