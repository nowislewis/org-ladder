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

;;; ── Live review integration (Forest stages, Boss banner/kill, Campfire) ───

(defvar org-ladder-sm-last-log nil
  "List of feedback strings from the most recent `org-sm' review session.")

(defvar org-ladder-sm--log-fresh nil
  "Non-nil before the first session action rotates `org-ladder-sm-last-log'.")

(with-eval-after-load 'savehist
  (defvar savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'org-ladder-sm-last-log))

(defvar org-ladder-sm--stock-snapshot nil
  "Mutable stock summary plist captured once per session for O(1) ledger deltas.")

(defun org-ladder-sm--reset-session ()
  "Reset lazy stock snapshot and mark `org-ladder-sm-last-log' for rotation."
  (setq org-ladder-sm--stock-snapshot nil
        org-ladder-sm--log-fresh t))

(defun org-ladder-sm--ensure-snapshot ()
  "Return the session's mutable stock summary plist, initializing it once."
  (unless org-ladder-sm--stock-snapshot
    (let ((sum (ignore-errors (org-ladder-stock-summary))))
      (setq org-ladder-sm--stock-snapshot
            (if (and sum (> (or (plist-get sum :total) 0) 0))
                (copy-tree sum)
              (list :stages (mapcar (lambda (s) (cons (car s) 0))
                                    org-ladder-stock-stages)
                    :monsters (mapcar (lambda (m) (list (car m) 0 0))
                                      org-ladder-stock-monsters)
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

(defun org-ladder-sm--stage-label (&optional days)
  "Return Forest stage display string for DAYS (defaulting to seedling)."
  (org-ladder-stock-stage-display (org-ladder-stock-stage (or days 0))))

(defun org-ladder-sm--monster-label (&optional reviews)
  "Return Monster tier display string for REVIEWS."
  (org-ladder-stock-monster-display (org-ladder-stock-monster reviews)))

(defun org-ladder-sm--boss-card-p (card)
  "Return non-nil if CARD plist is a Boss topic."
  (and (eq (plist-get card :type) 'topic)
       (>= (or (plist-get card :reviews) 0)
           org-ladder-stock-boss-threshold)))

(defun org-ladder-sm-banner (card)
  "Return left-anchored stage / Monster identity string for CARD plist."
  (let ((days (plist-get card :interval-days))
        (rev  (or (plist-get card :reviews) 0))
        (ct   (or (plist-get card :child-topics) 0))
        (cc   (or (plist-get card :child-clozes) 0)))
    (pcase (plist-get card :type)
      ('topic
       (format "%s · %d次%s" (org-ladder-sm--monster-label rev) rev
               (cond ((and (> ct 0) (> cc 0)) (format " · %dT/%dC" ct cc))
                     ((> cc 0)                (format " · %dC" cc))
                     ((> ct 0)                (format " · %dT" ct))
                     ((and days (> days 0))   (format " · %dd" days))
                     (t ""))))
      ('cloze
       (format "%s · %s" (org-ladder-sm--stage-label days)
               (if (and days (> days 0)) (format "%dd" days) "新卡"))))))

(defun org-ladder-sm--tree-nodes (here up base)
  "Collect (ME-P DEPTH TITLE) nodes up to 3 levels deep around HERE."
  (delq nil
        (org-map-entries
         (lambda ()
           (let ((d (- (org-outline-level) base))
                 (me (= (point) here)))
             (when (and (< d 3)
                        (or me (and up (= d 0))
                            (and (member (org-entry-get nil "SRS_TYPE") '("topic" "cloze"))
                                 (not (member "dismissed" (org-get-tags nil t))))))
               (list me d (truncate-string-to-width
                           (org-get-heading t t t t) 32 nil nil "…")))))
         nil (if up 'tree nil))))

(defun org-ladder-sm--context-tree (&optional active-tag)
  "Return a compact <=3-level outline tree around point, or nil.
When ACTIVE-TAG is non-nil, append it after `◀' on the current card line."
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (save-excursion
       (when (or (org-at-heading-p) (ignore-errors (org-back-to-heading t) t))
         (let* ((here (point))
                (up   (org-up-heading-safe))
                (raw  (org-ladder-sm--tree-nodes here up (if up (org-outline-level) 1)))
                (mark (if active-tag (format "  ◀ %s" active-tag) " ◀"))
                (rows (cl-loop for ((me d txt) . rest) on raw
                               for br = (cond ((and up (= d 0)) "▸")
                                              ((< (or (cadar rest) -1) d) "└─")
                                              (t "├─"))
                               collect (cons me (propertize
                                                 (format "%s%s %s%s" (make-string (* 2 d) ?\s)
                                                         br txt (if me mark ""))
                                                 'face (if me 'bold 'shadow))))))
           (when (> (length rows) 1)
             (let* ((idx (or (cl-position-if #'car rows) 0))
                    (st  (max (if up 1 0) (- idx 2)))
                    (sub (seq-subseq rows st (min (length rows) (+ st 4)))))
               (mapconcat #'cdr (if up (cons (car rows) sub) sub) "\n")))))))))

(defun org-ladder-sm--format-prev (prev)
  "Format PREV feedback string with status prefix and face."
  (when prev
    (if (string-prefix-p "↻ " prev)
        (propertize prev 'face 'warning)
      (propertize (format "✓ %s" prev) 'face 'success))))

(defun org-ladder-sm-prompt (state)
  "Present review STATE in the bottom-right HUD with feedback and context tree."
  (let* ((card    (plist-get state :card))
         (raw-pos (or (plist-get state :position) "SM"))
         (pos     (if (string-match "\\([0-9]+/[0-9]+\\)" raw-pos)
                      (format "[%s]" (match-string 1 raw-pos))
                    (format "[%s]" raw-pos)))
         (hdr     (format "%s %s" (org-ladder-sm-banner card) pos))
         (prev-ln (org-ladder-sm--format-prev
                   (or (plist-get state :prev)
                       (and (not org-ladder-sm--log-fresh)
                            (car (last org-ladder-sm-last-log))))))
         (title   (or prev-ln hdr))
         (body    (if prev-ln
                      (or (org-ladder-sm--context-tree hdr) hdr)
                    (org-ladder-sm--context-tree)))
         (keys    (and (fboundp 'org-sm-review-keys-hint)
                       (org-sm-review-keys-hint))))
    (if (org-ladder-notify body :title title)
        (message "%s" (string-join (delq nil (list prev-ln hdr keys)) "  |  "))
      (when keys (message "%s" keys)))))

(defun org-ladder-sm--on-card-change (event)
  "Invalidate derived score caches and refresh active HUD on card edits."
  (setq org-ladder--cache nil org-ladder--daily-cache nil org-ladder--cache-time nil)
  (when (and (fboundp 'org-sm--current-review-card-p)
             (org-sm--current-review-card-p)
             (fboundp 'org-sm--show-prompt))
    (pcase (plist-get event :action)
      ('extract (org-sm--show-prompt "🗡️ 产出子卡 +1"))
      ('edit    (org-sm--show-prompt "✨ 已精炼正文")))))

(defun org-ladder-sm--on-review-event (event)
  "Accumulate O(1) stock ledger deltas for EVENT."
  (when (and (plist-get event :session)
             (memq (plist-get event :action) '(topic cloze dismiss)))
    (let ((action (plist-get event :action))
          (card   (plist-get event :card))
          (old    (plist-get event :old-days))
          (new    (plist-get event :new-days))
          (snap   (org-ladder-sm--ensure-snapshot)))
      (if (eq action 'dismiss)
          (if (eq (plist-get card :type) 'topic)
              (let* ((rev    (or (plist-get card :reviews) 0))
                     (boss-p (org-ladder-sm--boss-card-p card))
                     (tier   (org-ladder-stock-monster rev))
                     (mcell  (assq tier (plist-get snap :monsters)))
                     (slain  (if boss-p
                                 (cl-incf (plist-get snap :boss-slain))
                               (cl-incf (plist-get snap :dismissed))))
                     (left   (if boss-p
                                 (setf (plist-get snap :boss-active)
                                       (max 0 (1- (or (plist-get snap :boss-active) 0))))
                               (setf (plist-get snap :topic-pool)
                                     (max 0 (1- (or (plist-get snap :topic-pool) 0)))))))
                (when mcell
                  (setq left  (setf (nth 1 mcell) (max 0 (1- (nth 1 mcell))))
                        slain (cl-incf (nth 2 mcell))))
                (plist-put event :ledger-slain slain)
                (plist-put event :ledger-left left))
            (plist-put event :ledger-dismissed
                       (cl-incf (plist-get snap :dismissed))))
        (when (and (eq action 'cloze) (eq (plist-get card :type) 'cloze)
                   new (> new 0))
          (let ((old-st (and old (> old 0) (org-ladder-stock-stage old)))
                (new-st (org-ladder-stock-stage new)))
            (when (not (eq old-st new-st))
              (when old-st (org-ladder-sm--stage-incf old-st -1))
              (let* ((cnt (org-ladder-sm--stage-incf new-st 1))
                     (lbl (org-ladder-stock-stage-display new-st)))
                (plist-put event :ledger-gain
                           (format "%s +1 (共 %d)" lbl cnt))))))))))

(defun org-ladder-sm--ivl-arrow (old new &optional label mins)
  "Format interval transition from OLD days (or MINS) to NEW days (or LABEL)."
  (let ((lhs (cond ((and old (> old 0)) (format "%dd" old))
                   ((and mins (> mins 0)) (format "%dm" mins))
                   (t "0d")))
        (rhs (if (and new (> new 0)) (format "%dd" new) (or label "0d"))))
    (format "%s → %s" lhs rhs)))

(defun org-ladder-sm-feedback (event)
  "Format unified monster and forest stage feedback for EVENT."
  (let* ((action (plist-get event :action))
         (card   (plist-get event :card))
         (type   (plist-get card :type))
         (rev    (or (plist-get card :reviews) 0))
         (old    (plist-get event :old-days))
         (new    (plist-get event :new-days))
         (arrow  (org-ladder-sm--ivl-arrow old new
                                           (plist-get event :interval-label)
                                           (plist-get card :interval-mins)))
         (text
          (pcase action
            ('dismiss
             (if (eq type 'topic)
                 (let ((mon (org-ladder-sm--monster-label rev)))
                   (if-let* ((slain (plist-get event :ledger-slain)))
                       (format "🏆 击败 %s · 累计 %d · 剩 %d"
                               mon slain (or (plist-get event :ledger-left) 0))
                     (format "🏆 击败 %s · %d次" mon rev)))
               (format "%s · 已归档%s" (org-ladder-sm--stage-label old)
                       (if-let* ((dis (plist-get event :ledger-dismissed)))
                           (format " (共 %d)" dis) ""))))
            ('topic
             (let ((m0 (org-ladder-sm--monster-label rev))
                   (m1 (org-ladder-sm--monster-label (1+ rev))))
               (if (equal m0 m1)
                   (format "%s · %d次 · %s" m1 (1+ rev) arrow)
                 (format "%s → %s · %s" m0 m1 arrow))))
            ('cloze
             (format "%s%s · %s"
                     (if (eq (plist-get event :rating) :again) "↻ " "")
                     (or (plist-get event :ledger-gain) (org-ladder-sm--stage-label new))
                     arrow))
            ('postpone
             (format "推迟 %dd" (plist-get event :postpone-days)))
            (_ (if (fboundp 'org-sm-default-feedback)
                   (org-sm-default-feedback event)
                 (format "%s" action))))))
    (when (plist-get event :session)
      (when org-ladder-sm--log-fresh
        (setq org-ladder-sm-last-log nil org-ladder-sm--log-fresh nil))
      (setq org-ladder-sm-last-log (append org-ladder-sm-last-log (list text))))
    text))

(defun org-ladder-sm-bonfire (&optional prev _aborted)
  "Close review HUD posframe, display `*Org Ladder*' buffer, and return PREV."
  (org-ladder-notify-close)
  (when (fboundp 'org-ladder-show-details)
    (org-ladder-show-details))
  (org-ladder-sm--format-prev prev))

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
