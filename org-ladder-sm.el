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
(require 'cl-lib)
(require 'org-ladder-stock)

(defvar org-ladder--cache)
(defvar org-ladder--cache-time)
(defvar org-sm-review-start-hook)
(defvar org-sm-after-review-functions)
(defvar org-sm-after-card-change-functions)
(defvar org-sm-prompt-function)
(defvar org-sm-feedback-function)
(defvar org-sm-finish-function)
(declare-function org-sm-default-feedback "org-sm")
(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-poshandler-window-bottom-right-corner "posframe")

(defgroup org-ladder-sm nil
  "org-sm review score source for org-ladder."
  :group 'org-ladder
  :prefix "org-ladder-sm-")

(defcustom org-ladder-sm-files nil
  "Source of org files to scan for review history.
Either a list of files, or a function of no arguments returning such a
list.  Prefer a function (e.g. `org-sm-files') so the file set is resolved
live on each scan instead of captured as a stale snapshot."
  :type '(choice (repeat file) function)
  :group 'org-ladder-sm)

(defun org-ladder-sm--files ()
  "Resolve `org-ladder-sm-files' to a concrete list of files."
  (org-ladder-stock--resolve-files org-ladder-sm-files))

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
  "Scan all LOGBOOK blocks in FILE; accumulate day-key scores into TBL.
Uses a temporary buffer with pure-regexp scanning to avoid the heavy cost
of fully initializing `org-mode' (font-lock, startup hooks, etc.) for every
file — `find-file-noselect' on hundreds of files can take many seconds."
  (with-temp-buffer
    (insert-file-contents file)
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
                           tbl))))))))))

(defun org-ladder-sm--collect ()
  "Return ((day-key . score) ...) from all org-sm review LOGBOOK entries."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (file (org-ladder-sm--files))
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

;;; ── Live review integration (Forest stages, Boss banner/kill, Bonfire) ──────

(defvar org-ladder-sm--session-stats nil
  "Plist (:reviewed N :dismissed N :boss-slain N :promoted (STAGE-DISPLAY ...))
accumulated during an `org-sm' review session.")

(defun org-ladder-sm--reset-session ()
  "Reset session counters at the start of an `org-sm' review."
  (setq org-ladder-sm--session-stats
        (list :reviewed 0 :dismissed 0 :boss-slain 0 :promoted nil)))

(defun org-ladder-sm--stage-label (days)
  "Return Forest stage display string for DAYS, or nil."
  (when days
    (org-ladder-stock-stage-display (org-ladder-stock-stage days))))

(defun org-ladder-sm--promoted-stage (event)
  "Return (OLD-LABEL . NEW-LABEL) when EVENT promotes a Cloze card, else nil."
  (let* ((card (plist-get event :card))
         (old  (plist-get event :old-days))
         (new  (plist-get event :new-days)))
    (when (and (eq (plist-get event :action) 'cloze)
               (eq (plist-get card :type) 'cloze)
               new)
      (let ((old-idx (org-ladder-stock-stage-index old))
            (new-idx (org-ladder-stock-stage-index new)))
        (when (and new-idx (> new-idx (or old-idx 0)))
          (cons (or (org-ladder-sm--stage-label old) "🌱 幼苗")
                (org-ladder-sm--stage-label new)))))))

(defun org-ladder-sm--boss-card-p (card)
  "Return non-nil if CARD plist is a Boss topic."
  (and (eq (plist-get card :type) 'topic)
       (>= (or (plist-get card :reviews) 0)
           org-ladder-stock-boss-threshold)))

(defun org-ladder-sm-banner (card)
  "Return stage / Boss banner string for CARD plist."
  (let ((type (plist-get card :type))
        (days (plist-get card :interval-days))
        (rev  (or (plist-get card :reviews) 0))
        (ct   (or (plist-get card :child-topics) 0))
        (cc   (or (plist-get card :child-clozes) 0)))
    (cond
     ((org-ladder-sm--boss-card-p card)
      (if (> (+ ct cc) 0)
          (format "⚔️ Boss · %d次 · 削血 %dT/%dC" rev ct cc)
        (format "⚔️ Boss · %d次 · 0子卡" rev)))
     ((eq type 'cloze)
      (let ((stage (or (org-ladder-sm--stage-label (or days 0)) "🌱 幼苗")))
        (if (and days (> days 0))
            (format "%s (%dd)" stage days)
          (format "%s (新卡)" stage))))
     ((eq type 'topic)
      (format "📖 阅读池 (%d次%s)" rev
              (if (and days (> days 0)) (format " · %dd" days) ""))))))

(defconst org-ladder-sm--hud-buffer " *org-ladder-sm-hud*"
  "Posframe buffer name for the bottom-right review HUD.")

(defvar org-ladder-sm--last-hud nil
  "Last formatted HUD text rendered by `org-ladder-sm--show-hud'.")

(defun org-ladder-sm--show-hud (text &optional timeout)
  "Show TEXT in a bottom-right posframe when graphical, else return TEXT."
  (setq org-ladder-sm--last-hud text)
  (if (and (display-graphic-p)
           (require 'posframe nil t)
           (posframe-workable-p))
      (let ((padded (concat " " (replace-regexp-in-string "\n" " \n " text) " ")))
        (posframe-show org-ladder-sm--hud-buffer
                       :string padded
                       :poshandler #'posframe-poshandler-window-bottom-right-corner
                       :x-pixel-offset -12
                       :y-pixel-offset -12
                       :left-fringe 8
                       :right-fringe 8
                       :internal-border-width 1
                       :internal-border-color (or (face-foreground 'shadow nil t) "#555555")
                       :background-color (face-background 'default nil t)
                       :foreground-color (face-foreground 'default nil t)
                       :timeout timeout)
        nil)
    text))

(defun org-ladder-sm-format-hud (state)
  "Format the bottom-right HUD text for review STATE plist."
  (let* ((card   (plist-get state :card))
         (pos    (or (plist-get state :position) "SM"))
         (ban    (org-ladder-sm-banner card))
         (cstate (when (eq (plist-get card :type) 'cloze)
                   (if (eq (plist-get state :cloze-state) 'hidden)
                       "hidden" "revealed")))
         (prev   (plist-get state :prev))
         (line1  (string-join (delq nil (list pos ban cstate)) " · ")))
    (if prev
        (format "%s\n✓ %s" line1 prev)
      line1)))

(defun org-ladder-sm-prompt (state)
  "Present review STATE in the bottom-right HUD (or echo area in terminal)."
  (when-let* ((fallback (org-ladder-sm--show-hud (org-ladder-sm-format-hud state) nil)))
    (message "%s" (replace-regexp-in-string "\n" "  |  " fallback))))

(defun org-ladder-sm--on-card-change (_event)
  "Invalidate derived score caches after any card mutation."
  (setq org-ladder--cache nil org-ladder--cache-time nil))

(defun org-ladder-sm--on-review-event (event)
  "Accumulate only completed actions belonging to the active review session."
  (when (and (plist-get event :session)
             (memq (plist-get event :action) '(topic cloze dismiss)))
    (unless org-ladder-sm--session-stats (org-ladder-sm--reset-session))
  (let ((action (plist-get event :action))
        (card   (plist-get event :card)))
    (if (eq action 'dismiss)
        (if (org-ladder-sm--boss-card-p card)
            (cl-incf (plist-get org-ladder-sm--session-stats :boss-slain))
          (cl-incf (plist-get org-ladder-sm--session-stats :dismissed)))
      (cl-incf (plist-get org-ladder-sm--session-stats :reviewed))
      (when-let* ((prom (org-ladder-sm--promoted-stage event)))
        (push (cdr prom)
              (plist-get org-ladder-sm--session-stats :promoted)))))))

(defun org-ladder-sm-feedback (event)
  "Format echo-area feedback for `org-sm' review EVENT."
  (let ((action (plist-get event :action))
        (card   (plist-get event :card))
        (old    (plist-get event :old-days))
        (new    (plist-get event :new-days)))
    (cond
     ((eq action 'dismiss)
      (if (org-ladder-sm--boss-card-p card)
          (format "🏆 击败 Boss (%d次 · %d子卡)"
                  (or (plist-get card :reviews) 0)
                  (+ (or (plist-get card :child-topics) 0)
                     (or (plist-get card :child-clozes) 0)))
        "已筛选"))
     ((org-ladder-sm--promoted-stage event)
      (let ((prom (org-ladder-sm--promoted-stage event)))
        (format "✨ %dd → %dd  %s → %s"
                (or old 0) new (car prom) (cdr prom))))
     ((eq action 'cloze)
      (if (and new (> new 0))
          (format "%dd → %dd · %s"
                  (or old 0) new (org-ladder-sm--stage-label new))
        (format "→ %s · 🌱 幼苗"
                (or (plist-get event :interval-label) "学习中"))))
     ((fboundp 'org-sm-default-feedback)
      (org-sm-default-feedback event))
     (t (format "%s" action)))))

(defun org-ladder-sm-bonfire-text (&optional prev)
  "Return bonfire summary text at the end or stop of an `org-sm' session."
  (let* ((rev  (or (plist-get org-ladder-sm--session-stats :reviewed) 0))
         (dis  (or (plist-get org-ladder-sm--session-stats :dismissed) 0))
         (slain (or (plist-get org-ladder-sm--session-stats :boss-slain) 0))
         (prom (plist-get org-ladder-sm--session-stats :promoted))
         (tot  (+ rev dis slain)))
    (if (= tot 0)
        "🔥 篝火小憩 · 随时停下，进度已保存"
      (let (parts)
        (when (> rev 0)   (push (format "复习 %d" rev) parts))
        (when (> dis 0)   (push (format "已筛选 %d" dis) parts))
        (when (> slain 0) (push (format "击败 Boss %d" slain) parts))
        (concat
         (format "🔥 篝火小结 · 本次处理 %d 张（%s）"
                 tot (string-join (nreverse parts) " · "))
         (if prom
             (format " · 升阶 %s" (string-join (reverse prom) " "))
           "")
         (if prev (format "  [last: %s]" prev) ""))))))

(defun org-ladder-sm-bonfire (&optional prev _aborted)
  "Present bonfire summary in the bottom-right HUD (auto-hiding after 4s)."
  (org-ladder-sm--show-hud (org-ladder-sm-bonfire-text prev) 4))

;;;###autoload
(defun org-ladder-sm-setup-review-hook ()
  "Connect `org-ladder' HUD, feedback, and bonfire hooks to `org-sm'."
  (interactive)
  (add-hook 'org-sm-review-start-hook #'org-ladder-sm--reset-session)
  (add-hook 'org-sm-after-review-functions #'org-ladder-sm--on-review-event)
  (add-hook 'org-sm-after-card-change-functions #'org-ladder-sm--on-card-change)
  (setq org-sm-prompt-function #'org-ladder-sm-prompt
        org-sm-feedback-function #'org-ladder-sm-feedback
        org-sm-finish-function #'org-ladder-sm-bonfire))

;;;###autoload
(with-eval-after-load 'org-sm
  (org-ladder-sm-setup-review-hook))

(provide 'org-ladder-sm)
;;; org-ladder-sm.el ends here
