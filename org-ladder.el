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
TIME-KEY is produced by `org-ladder-time-key-day', `-month', or `-week'."
  :type '(repeat function)
  :group 'org-ladder)

;;; ── State ────────────────────────────────────────────────────────────────────

(defvar org-ladder--cache nil
  "Alist of ((year month) . score) for every month seen; nil when stale.")

(defvar org-ladder--daily-cache nil
  "Hash-table of day-key -> score cached alongside `org-ladder--cache'.")

(defvar org-ladder--cache-time nil
  "Emacs time of last cache computation.")

;;; ── Time key API ─────────────────────────────────────────────────────────────
(defun org-ladder-hash-to-alist (tbl)
  "Convert hash-table TBL to an alist of (KEY . VALUE)."
  (let (result)
    (maphash (lambda (k v) (push (cons k v) result)) tbl)
    result))

(defun org-ladder-time-parse-iso (string)
  "Return a day key for the first YYYY-MM-DD found in STRING, or nil."
  (when (string-match
         "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" string)
    (list (string-to-number (match-string 1 string))
          (string-to-number (match-string 2 string))
          (string-to-number (match-string 3 string)))))

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
  "Aggregate day-key RAW-ENTRIES into a monthly alist ((year month) . score)."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (entry raw-entries)
      (let ((ym (list (caar entry) (cadar entry))))
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
        (puthash (car e) (+ (gethash (car e) daily 0) (cdr e)) daily))
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

(defun org-ladder--progress-bar (current total width)
  "Return a progress bar string of WIDTH chars for CURRENT/TOTAL."
  (let ((filled (max 0 (min width (round (* width (/ (float current) total)))))))
    (concat "[" (make-string filled ?█) (make-string (- width filled) ?░) "]")))

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
(declare-function org-ladder-sm--format-prev "org-ladder-sm")
(defvar org-ladder-sm-last-log)

(defvar org-ladder-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "s" #'org-sm-review-start-light)
    (define-key map "S" #'org-sm-review-start)
    (define-key map "l" #'org-sm-tree)
    map)
  "Keymap for `org-ladder-mode' Campfire Hub.")

(define-derived-mode org-ladder-mode special-mode "Org-Ladder"
  "Campfire Hub mode for `*Org Ladder*'.
\\[org-sm-review-start-light] starts a 6-card random expedition,
\\[org-sm-review-start] reviews all due cards, \\[org-sm-tree] opens the
card tree, and \\[forward-button] cycles through stage/monster buttons."
  (setq-local header-line-format
              (substitute-command-keys
               " \\[org-sm-review-start-light] 随机远征  \\[org-sm-review-start] 全量出征  \\[org-sm-tree] 选怪树图  \\[forward-button] 跳按钮  \\[revert-buffer] 刷新  \\[quit-window] 退出")
              revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (setq org-ladder--cache nil
                      org-ladder--daily-cache nil
                      org-ladder--cache-time nil)
                (org-ladder-show-details))))

;;;###autoload
(defun org-ladder-show-details (&optional months)
  "Show the Org Ladder Campfire Hub dashboard.
With prefix argument MONTHS, also expand top deepest Clozes and Monsters."
  (interactive "P")
  (let* ((cur-ym    (org-ladder--month-key (current-time)))
         (cur-year  (car cur-ym))
         (cur-month (cadr cur-ym))
         (top-n     (cond ((null months) 0)
                          ((listp months) 10)
                          (t (prefix-numeric-value months))))
         (daily-tbl (org-ladder--collect-daily)))
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
                  (buttonize (substitute-command-keys
                              "[🎲 随机远征 6 张 (\\[org-sm-review-start-light])]")
                             (lambda (_) (require 'org-sm) (org-sm-review-start-light)))
                  "   "
                  (buttonize (substitute-command-keys
                              "[⚔️ 全量出征 (\\[org-sm-review-start])]")
                             (lambda (_) (require 'org-sm) (org-sm-review-start)))
                  "   "
                  (buttonize (substitute-command-keys
                              "[🗺️ 选怪树图 (\\[org-sm-tree])]")
                             (lambda (_) (require 'org-sm) (call-interactively #'org-sm-tree)))
                  "\n")
          (insert (propertize "────────────────────────────────────────────────────────\n\n"
                              'face '(:foreground "#444444"))))

        ;; ── Last session review log ────────────────────────────────────
        (when-let* ((log (bound-and-true-p org-ladder-sm-last-log)))
          (insert (propertize (format "🔥 上次战报 (%d 张)\n" (length log))
                              'face '(:weight bold :height 1.2)))
          (dolist (line log)
            (let ((s (if (fboundp 'org-ladder-sm--format-prev)
                         (org-ladder-sm--format-prev line)
                       line)))
              (insert "  "
                      (replace-regexp-in-string
                       " · \\([^·]+\\)\\'"
                       (concat (propertize "  " 'display '(space :align-to 28)) "\\1")
                       s t)
                      "\n")))
          (insert "\n"))

        ;; ── Permanent stock & chronicle ────────────────────────────────
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

        ;; ── GitHub style 14-day heatmap ────────────────────────────────
        (insert (propertize "🔥 Activity\n" 'face '(:weight bold :height 1.2)))
        (let* ((today-time (current-time))
               (streak 0)
               (best-day 0))
          (let ((offset (if (> (gethash (org-ladder-time-from-emacs today-time) daily-tbl 0) 0) 0 1)))
            (catch 'done
              (dotimes (i 30)
                (let* ((t-day (time-subtract today-time (seconds-to-time (* (+ i offset) 86400))))
                       (sc (gethash (org-ladder-time-from-emacs t-day) daily-tbl 0)))
                  (if (> sc 0)
                      (setq streak (1+ streak))
                    (throw 'done t))))))
          (dotimes (i (nth 3 (decode-time today-time)))
            (setq best-day (max best-day
                                (gethash (list cur-year cur-month (1+ i)) daily-tbl 0))))
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
          (insert (format "\n  streak: %d days    best day: %d min\n" streak best-day)))

        (goto-char (point-min))
        (setq buffer-read-only t))
      (display-buffer (current-buffer)))))

;;; ── Load default source ──────────────────────────────────────────────────────

(require 'org-ladder-clock)
(require 'org-ladder-stock)

(provide 'org-ladder)
;;; org-ladder.el ends here
