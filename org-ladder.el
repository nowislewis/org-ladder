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

;;; ── Tier configuration ───────────────────────────────────────────────────────

(defconst org-ladder-tiers
  '((bronze   .  (0    499  10  " Bronze"))
    (silver   .  (500  1199 10  " Silver"))
    (gold     .  (1200 2099 10  " Gold"))
    (platinum .  (2100 3199 10  " Platinum"))
    (diamond  .  (3200 4499 10  "󰫞 Diamond"))
    (legend   .  (4500 nil  nil " Legend")))
  "Alist of (NAME . (MIN MAX SUB-COUNT DISPLAY-NAME)).
DISPLAY-NAME intentionally includes the icon as part of the rank name.
Legend tier has no upper bound.")

;;; ── State ────────────────────────────────────────────────────────────────────

(defvar org-ladder--cache nil
  "Alist of ((year month) . score) for every month seen; nil when stale.")

(defvar org-ladder--daily-cache nil
  "Hash-table of day-key -> score cached alongside `org-ladder--cache'.")

(defvar org-ladder--cache-time nil
  "Emacs time of last cache computation.")

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

(defun org-ladder-hash-to-alist (tbl)
  "Convert hash-table TBL to an alist of (KEY . VALUE)."
  (let (result)
    (maphash (lambda (k v) (push (cons k v) result)) tbl)
    result))

(defun org-ladder-time-key-to-month (key)
  "Return (YEAR MONTH) for KEY.  Week keys resolve to their ISO Monday's month."
  (pcase (org-ladder-time-key-granularity key)
    ((or 'day 'month)
     (list (nth 0 key) (nth 1 key)))
    ('week
     (let* ((year  (nth 0 key))
            (week  (nth 2 key))
            (jan4  (encode-time 0 0 0 4 1 year))
            (j-dow (decoded-time-weekday (decode-time jan4)))
            (w1-mon (time-subtract
                     jan4 (seconds-to-time
                           (* (mod (1- (if (= j-dow 0) 7 j-dow)) 7) 86400))))
            (monday (time-add w1-mon
                              (seconds-to-time (* (1- week) 7 86400))))
            (d (decode-time monday)))
       (list (decoded-time-year d) (decoded-time-month d))))))

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
    (list (decoded-time-year d) (decoded-time-month d) (decoded-time-day d))))

(defun org-ladder-time-today ()
  "Return today as a day key."
  (org-ladder-time-from-emacs (current-time)))

;;; ── Score aggregation ────────────────────────────────────────────────────────

(defun org-ladder--month-key (time)
  "Return (year month) from Emacs TIME."
  (let ((d (decode-time time)))
    (list (decoded-time-year d) (decoded-time-month d))))

(defun org-ladder--sort-monthly (rows)
  "Sort monthly score ROWS descending by (year month)."
  (sort rows (lambda (a b) (value< (car b) (car a)))))

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
  "Aggregate RAW-ENTRIES into a monthly alist ((year month) . score)."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (entry raw-entries)
      (let ((ym (org-ladder-time-key-to-month (car entry))))
        (puthash ym (+ (gethash ym tbl 0) (cdr entry)) tbl)))
    (org-ladder-hash-to-alist tbl)))

(defun org-ladder-calculate-scores (&optional force)
  "Return alist ((year month) . score) for every month with data.
Also populates `org-ladder--daily-cache' in the same single pass.
Cached for 5 minutes; pass FORCE to bypass."
  (when (or force
            (null org-ladder--cache)
            (null org-ladder--daily-cache)
            (null org-ladder--cache-time)
            (> (float-time (time-since org-ladder--cache-time)) 300))
    (let ((raw   (org-ladder--collect-raw))
          (daily (make-hash-table :test 'equal)))
      (dolist (e raw)
        (when (eq (org-ladder-time-key-granularity (car e)) 'day)
          (puthash (car e) (+ (gethash (car e) daily 0) (cdr e)) daily)))
      (setq org-ladder--daily-cache daily
            org-ladder--cache (org-ladder--sort-monthly
                               (org-ladder--normalize-entries raw))
            org-ladder--cache-time (current-time))))
  org-ladder--cache)

(defun org-ladder--collect-daily ()
  "Return cached hash-table of day-key -> score across all sources."
  (org-ladder-calculate-scores)
  org-ladder--daily-cache)

(defun org-ladder-current-score ()
  "Return the current month's total score (0 if none)."
  (or (cdr (assoc (org-ladder--month-key (current-time))
                  (org-ladder-calculate-scores)))
      0))

;;;###autoload
(defun org-ladder-force-refresh ()
  "Invalidate cache and recompute scores."
  (interactive)
  (setq org-ladder--cache nil org-ladder--daily-cache nil org-ladder--cache-time nil)
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

(defun org-ladder--insert-badge-shelf (all-scores)
  "Insert permanent badge shelf from ALL-SCORES."
  (insert (propertize "🏅 赛季徽章陈列 (永久保留)\n" 'face '(:weight bold :height 1.2)))
  (let ((counts (make-hash-table :test 'eq))
        (best   nil))
    (dolist (entry all-scores)
      (let* ((sc   (cdr entry))
             (tier (car (org-ladder-get-tier-info sc))))
        (puthash tier (1+ (gethash tier counts 0)) counts)
        (when (or (null best) (> sc (cdr best)))
          (setq best entry))))
    (let (parts)
      (dolist (tc (reverse org-ladder-tiers))
        (let ((n (gethash (car tc) counts 0)))
          (when (> n 0)
            (push (format "%-12s ×%d" (nth 3 (cdr tc)) n) parts))))
      (insert (if parts
                  (format "  %s\n" (string-join (nreverse parts) "\n  "))
                "  (暂无赛季记录)\n")))
    (when best
      (insert (format "  最高单月  %d-%02d · %d min (%s)\n"
                      (caar best) (cadar best) (cdr best)
                      (org-ladder--rank-name (cdr best)))))
    (insert "\n")))

;;; ── Display helpers ──────────────────────────────────────────────────────────

(defun org-ladder--progress-bar (current total width)
  "Return a progress bar string of WIDTH chars for CURRENT/TOTAL."
  (let ((filled (max 0 (min width (round (* width (/ (float current) total)))))))
    (concat "[" (make-string filled ?█) (make-string (- width filled) ?░) "]")))

(defun org-ladder--tier-display-name (name)
  "Return display name for tier NAME, including its icon."
  (or (nth 3 (cdr (assoc name org-ladder-tiers)))
      (capitalize (symbol-name name))))

(defun org-ladder--rank-name (score)
  "Return a short rank string like \" Gold 6\" or \" Legend\" for SCORE."
  (pcase-let ((`(,name ,sub) (org-ladder-get-tier-info score)))
    (if (eq name 'legend) (org-ladder--tier-display-name name)
      (format "%s %d" (org-ladder--tier-display-name name) sub))))

;;; ── Commands ─────────────────────────────────────────────────────────────────

;;;###autoload
(defcustom org-ladder-status-quotes
  '("Keep the streak alive."
    "Small wins compound."
    "Ship one brick today."
    "Momentum beats motivation."
    "Lock in and move."
    "One clean rep."
    "Make it count."
    "Progress, not perfection."
    "You are already in motion."
    "Stay in the pocket."
    "Another rep, another level."
    "Tiny push. Big arc.")
  "Short motivational lines randomly shown in the Campfire Hub."
  :type '(repeat string)
  :group 'org-ladder)

(declare-function org-sm-review-start-light "org-sm")
(declare-function org-sm-review-start "org-sm")
(declare-function org-sm-tree "org-sm")

(defvar org-ladder-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "s" #'org-sm-review-start-light)
    (define-key map "S" #'org-sm-review-start)
    (define-key map "l" #'org-sm-tree)
    (define-key map "g" #'org-ladder-show-details)
    map)
  "Keymap for `org-ladder-mode' Campfire Hub.")

(define-derived-mode org-ladder-mode special-mode "Org-Ladder"
  "Campfire Hub mode for `*Org Ladder*'.
\\[org-sm-review-start-light] starts a 6-card random expedition,
\\[org-sm-review-start] reviews all due cards, \\[org-sm-tree] opens the
card tree, and \\[forward-button] cycles through stage/monster buttons."
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm) (org-ladder-show-details))))

;;;###autoload
(defun org-ladder-show-details (&optional months)
  "Show the Org Ladder Campfire Hub dashboard.
With prefix argument MONTHS, also expand top deepest Clozes and Monsters."
  (interactive "P")
  (let* ((all-scores (org-ladder-calculate-scores))
         (score      (org-ladder-current-score))
         (info       (org-ladder-get-tier-info score))
         (cur-ym     (org-ladder--month-key (current-time)))
         (cur-year   (car cur-ym))
         (cur-month  (cadr cur-ym))
         (top-n      (cond ((null months) 0)
                           ((listp months) 10)
                           (t (prefix-numeric-value months))))
         (history    (cond ((null months)  (seq-take all-scores 12))
                           ((listp months) all-scores)
                           (t              (seq-take all-scores months))))
         (daily-tbl  (org-ladder--collect-daily)))
    (pcase-let ((`(,name ,sub ,nsubs ,_to-t ,_to-s ,_prog ,_tot) info))
      (with-current-buffer (get-buffer-create "*Org Ladder*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (derived-mode-p 'org-ladder-mode)
            (org-ladder-mode))

          (let ((quote (nth (random (length org-ladder-status-quotes))
                            org-ladder-status-quotes)))
            (insert (propertize (format "🔥 %s\n" quote)
                                'face '(:weight bold :height 1.3 :foreground "#A6E22E")))
            (insert "  "
                    (buttonize "[🎲 随机远征 6 张 (s)]"
                               (lambda (_) (require 'org-sm) (org-sm-review-start-light)))
                    "   "
                    (buttonize "[⚔️ 全量出征 (S)]"
                               (lambda (_) (require 'org-sm) (org-sm-review-start)))
                    "   "
                    (buttonize "[🗺️ 选怪树图 (l)]"
                               (lambda (_) (require 'org-sm) (call-interactively #'org-sm-tree)))
                    "\n")
            (insert (propertize "────────────────────────────────────────────────────────\n\n"
                                'face '(:foreground "#444444"))))

          ;; ── Permanent stock (first screen, never resets) ───────────────
          (require 'org-ladder-stock nil t)
          (when (fboundp 'org-ladder-stock-insert-section)
            (org-ladder-stock-insert-section nil top-n))

          ;; ── Artifact progress (unlocked by cumulative score) ───────────
          (require 'org-ladder-art nil t)
          (insert (propertize "🏺 Artifact (累计解锁)\n" 'face '(:weight bold :height 1.2)))
          (if (fboundp 'org-ladder-art-insert-current)
              (org-ladder-art-insert-current
               (gethash (org-ladder-time-today) daily-tbl 0))
            (insert "  artifact module not loaded.\n\n"))

          ;; ── Permanent badge shelf ──────────────────────────────────────
          (org-ladder--insert-badge-shelf all-scores)

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

          ;; ── Tier / projection (decorative season layer) ────────────────
          (insert (propertize "📈 Season (本月装饰)\n" 'face '(:weight bold :height 1.2)))
          (let* ((day-now     (nth 3 (decode-time (current-time))))
                 (days-total  (calendar-last-day-of-month cur-month cur-year))
                 (avg-daily   (if (> day-now 0) (/ (float score) day-now) 0))
                 (eom-score   (round (* avg-daily days-total)))
                 (eom-rank    (org-ladder--rank-name eom-score))
                 (today-score (gethash (org-ladder-time-today) daily-tbl 0))
                 (today-thresholds '((0 . "💤 待机中")
                                     (15 . "✨ 火花点燃")
                                     (30 . "🔥 系统预热")
                                     (60 . "🚀 引擎启动")
                                     (90 . "🎯 锁定目标")
                                     (120 . "🧠 进入航道")
                                     (180 . "🌊 深潜作业")
                                     (240 . "⚡ 推进过载")
                                     (360 . "🦸 英雄模式")
                                     (480 . "🏆 传说降临")
                                     (600 . "🌌 神话升格")))
                 (today-state (catch 'state
                                (let ((prev (cdar today-thresholds)))
                                  (dolist (entry (cdr today-thresholds) prev)
                                    (when (< today-score (car entry))
                                      (throw 'state prev))
                                    (setq prev (cdr entry))))))
                 (today-next-entry (catch 'next
                                     (dolist (entry today-thresholds nil)
                                       (when (> (car entry) today-score)
                                         (throw 'next entry)))))
                 (today-next (or (car-safe today-next-entry) 999999))
                 (today-next-state (cdr-safe today-next-entry))
                 (today-remaining (and today-next-state (- today-next today-score)))
                 (today-bar (if today-next-state
                                (org-ladder--progress-bar today-score today-next 10)
                              (org-ladder--progress-bar 1 1 10))))
            (insert (format "  TODAY   %d min · %s\n" today-score today-state))
            (insert (if today-next-state
                        (format "  NEXT    %s %d min to %s\n\n"
                                today-bar today-remaining today-next-state)
                      (format "  NEXT    %s max stage reached\n\n" today-bar)))
            (insert (format "  SEASON  %s %s · %d min\n"
                            (org-ladder--tier-display-name name)
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
(require 'org-ladder-stock)

(provide 'org-ladder)
;;; org-ladder.el ends here
