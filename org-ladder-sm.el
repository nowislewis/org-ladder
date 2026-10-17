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
(require 'org-ladder)
(require 'org-ladder-stock)
(require 'org-ladder-notify)

(defvar org-sm-review-start-hook)
(defvar org-sm-after-review-functions)
(defvar org-sm-after-card-change-functions)
(defvar org-sm-prompt-function)
(defvar org-sm-feedback-function)
(defvar org-sm-finish-function)
(defvar org-sm--current-buffer)
(defvar org-sm-capture-parent-marker)
(declare-function org-sm-default-feedback "org-sm")
(declare-function org-sm--current-review-card-p "org-sm")
(declare-function org-sm--show-prompt "org-sm")

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
    (org-ladder-hash-to-alist tbl)))

(with-eval-after-load 'org-ladder
  (add-to-list 'org-ladder-score-sources #'org-ladder-sm--collect))

;;; ── Live review integration (Forest stages, Boss banner/kill, Bonfire) ──────

(defvar org-ladder-sm--session-stats nil
  "Plist (:reviewed N :dismissed N :boss-slain N :promoted (STAGE-GAIN ...))
accumulated during an `org-sm' review session.")

(defvar org-ladder-sm--stock-snapshot nil
  "Mutable stock summary plist captured once per session for O(1) ledger deltas.")

(defun org-ladder-sm--reset-session ()
  "Reset session counters and lazy stock snapshot at the start of a review."
  (setq org-ladder-sm--session-stats
        (list :reviewed 0 :dismissed 0 :boss-slain 0 :promoted nil)
        org-ladder-sm--stock-snapshot nil))

(defun org-ladder-sm--ensure-snapshot ()
  "Return the session's mutable stock summary plist, initializing it once."
  (unless org-ladder-sm--stock-snapshot
    (let ((sum (ignore-errors (org-ladder-stock-summary))))
      (setq org-ladder-sm--stock-snapshot
            (if (and sum (> (or (plist-get sum :total) 0) 0))
                (copy-tree sum)
              (list :stages (mapcar (lambda (s) (cons (car s) 0))
                                    org-ladder-stock-stages)
                    :scheduled 0 :unscheduled 0 :dismissed 0
                    :boss-slain 0 :boss-active 0 :boss-wounded 0
                    :topic-pool 0 :total 0)))))
  org-ladder-sm--stock-snapshot)

(defun org-ladder-sm--stage-incf (stage delta)
  "Add DELTA to STAGE in the session stock snapshot and return the new count."
  (let* ((snap (org-ladder-sm--ensure-snapshot))
         (cell (assq stage (plist-get snap :stages))))
    (when cell
      (setcdr cell (max 0 (+ (cdr cell) delta))))))

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
  "Return left-anchored stage / Monster identity string for CARD plist."
  (let ((type (plist-get card :type))
        (days (plist-get card :interval-days))
        (rev  (or (plist-get card :reviews) 0))
        (ct   (or (plist-get card :child-topics) 0))
        (cc   (or (plist-get card :child-clozes) 0)))
    (cond
     ((eq type 'topic)
      (let ((label (org-ladder-stock-monster-display
                    (org-ladder-stock-monster rev)))
            (sub   (cond ((and (> ct 0) (> cc 0)) (format " · %dT/%dC" ct cc))
                         ((> cc 0) (format " · %dC" cc))
                         ((> ct 0) (format " · %dT" ct))
                         ((and days (> days 0)) (format " · %dd" days))
                         (t ""))))
        (format "%s · %d次%s" label rev sub)))
     ((eq type 'cloze)
      (let ((stage (or (org-ladder-sm--stage-label (or days 0)) "🌱 幼苗")))
        (if (and days (> days 0))
            (format "%s · %dd" stage days)
          (format "%s · 新卡" stage)))))))

(defun org-ladder-sm--hud-hide-p (_info)
  "Auto-hide persistent HUD when leaving the review card or workbench."
  (not (or (eq (current-buffer) (bound-and-true-p org-sm--current-buffer))
           (bound-and-true-p org-sm-capture-parent-marker))))

(defun org-ladder-sm-prompt (state)
  "Present review STATE in the bottom-right HUD with stage on the left."
  (let* ((card    (plist-get state :card))
         (raw-pos (or (plist-get state :position) "SM"))
         (pos     (if (string-match "\\([0-9]+/[0-9]+\\)" raw-pos)
                      (format "[%s]" (match-string 1 raw-pos))
                    (format "[%s]" raw-pos)))
         (ban     (org-ladder-sm-banner card))
         (cstate  (when (eq (plist-get card :type) 'cloze)
                    (if (eq (plist-get state :cloze-state) 'hidden)
                        "hidden" "revealed")))
         (prev    (plist-get state :prev))
         (title   (format "%s   %s"
                          (string-join (delq nil (list ban cstate)) " · ")
                          pos))
         (body    (and prev (format "✓ %s" prev))))
    (when-let* ((fallback (org-ladder-notify body :title title
                                             :hidehandler #'org-ladder-sm--hud-hide-p)))
      (message "%s" (replace-regexp-in-string "\n" "  |  " fallback)))))

(defun org-ladder-sm--on-card-change (event)
  "Invalidate derived score caches and refresh active HUD on child extraction."
  (setq org-ladder--cache nil org-ladder--daily-cache nil org-ladder--cache-time nil)
  (when (and (eq (plist-get event :action) 'extract)
             (fboundp 'org-sm--current-review-card-p)
             (org-sm--current-review-card-p)
             (fboundp 'org-sm--show-prompt))
    (org-sm--show-prompt "🗡️ 产出子卡 +1")))

(defun org-ladder-sm--on-review-event (event)
  "Accumulate session stats and O(1) stock ledger deltas for EVENT."
  (when (and (plist-get event :session)
             (memq (plist-get event :action) '(topic cloze dismiss)))
    (unless org-ladder-sm--session-stats (org-ladder-sm--reset-session))
    (let ((action (plist-get event :action))
          (card   (plist-get event :card))
          (old    (plist-get event :old-days))
          (new    (plist-get event :new-days))
          (snap   (org-ladder-sm--ensure-snapshot)))
      (if (eq action 'dismiss)
          (if (org-ladder-sm--boss-card-p card)
              (let ((slain (cl-incf (plist-get snap :boss-slain)))
                    (left  (setf (plist-get snap :boss-active)
                                 (max 0 (1- (or (plist-get snap :boss-active) 0))))))
                (cl-incf (plist-get org-ladder-sm--session-stats :boss-slain))
                (plist-put event :ledger-slain slain)
                (plist-put event :ledger-left left))
            (let ((dis (cl-incf (plist-get snap :dismissed))))
              (cl-incf (plist-get org-ladder-sm--session-stats :dismissed))
              (plist-put event :ledger-dismissed dis)))
        (cl-incf (plist-get org-ladder-sm--session-stats :reviewed))
        (when (and (eq action 'cloze) (eq (plist-get card :type) 'cloze)
                   new (> new 0))
          (let ((old-st (and old (> old 0) (org-ladder-stock-stage old)))
                (new-st (org-ladder-stock-stage new)))
            (when (not (eq old-st new-st))
              (when old-st (org-ladder-sm--stage-incf old-st -1))
              (let* ((cnt (org-ladder-sm--stage-incf new-st 1))
                     (lbl (org-ladder-stock-stage-display new-st))
                     (gain (format "%s +1 (共 %d)" lbl cnt)))
                (plist-put event :ledger-gain gain)
                (push gain (plist-get org-ladder-sm--session-stats :promoted))))))))))

(defun org-ladder-sm-feedback (event)
  "Format restrained feedback for EVENT, reporting ledger totals only on transitions."
  (let ((action (plist-get event :action))
        (card   (plist-get event :card))
        (old    (plist-get event :old-days))
        (new    (plist-get event :new-days))
        (gain   (plist-get event :ledger-gain)))
    (cond
     ((eq action 'dismiss)
      (if (org-ladder-sm--boss-card-p card)
          (if-let* ((slain (plist-get event :ledger-slain)))
              (format "🏆 Boss 讨伐 +1 (累计 %d · 剩 %d)"
                      slain (or (plist-get event :ledger-left) 0))
            "🏆 Boss 讨伐 +1")
        (if-let* ((dis (plist-get event :ledger-dismissed)))
            (format "已归档 +1 (共 %d)" dis)
          "已归档")))
     (gain
      (if (and old (> old 0))
          (format "%dd → %dd  %s" old new gain)
        (format "→ %dd  %s" new gain)))
     ((org-ladder-sm--promoted-stage event)
      (let ((prom (org-ladder-sm--promoted-stage event)))
        (format "%dd → %dd  %s +1" (or old 0) new (cdr prom))))
     ((eq action 'cloze)
      (if (and new (> new 0))
          (format "%dd → %dd" (or old 0) new)
        (format "→ %s" (or (plist-get event :interval-label) "学习中"))))
     ((eq action 'topic)
      (if (and old (> old 0))
          (format "%dd → %dd" old new)
        (format "→ %dd" new)))
     ((eq action 'postpone)
      (format "推迟 %dd" (plist-get event :postpone-days)))
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
        (when (> dis 0)   (push (format "已归档 %d" dis) parts))
        (when (> slain 0) (push (format "击败 Boss %d" slain) parts))
        (concat
         (format "🔥 篝火小结 · 本次处理 %d 张（%s）"
                 tot (string-join (nreverse parts) " · "))
         (if prom
             (format " · 升阶 %s" (string-join (reverse prom) " "))
           "")
         (if prev (format "  [last: %s]" prev) ""))))))

(defun org-ladder-sm-bonfire (&optional prev _aborted)
  "Present bonfire summary via `org-ladder-notify' and refresh Campfire Hub."
  (when (and (get-buffer "*Org Ladder*")
             (fboundp 'org-ladder-show-details))
    (save-window-excursion
      (org-ladder-show-details)))
  (org-ladder-notify (and prev (format "✓ %s" prev))
                     :title (org-ladder-sm-bonfire-text)
                     :timeout 4))

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
