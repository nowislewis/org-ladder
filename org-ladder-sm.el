;;; org-ladder-sm.el --- org-sm review scores for org-ladder  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 3.2
;; Package-Requires: ((emacs "27.1") (org "9.0") (org-ladder "3.0"))

;;; Commentary:
;; Score source for org-sm spaced-repetition reviews.
;;
;; Scans all LOGBOOK blocks in `org-ladder-sm-files'.  Each valid review line
;; (`- topic', `- cloze', `- dismissed', excluding `postpone' and `explain')
;; contributes `org-ladder-sm-score-per-review' points (4 points).
;; Each scanned card also contributes `org-ladder-sm-score-per-review' points
;; toward the cumulative climb total.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'org-ladder)
(require 'org-ladder-stock)
(require 'org-ladder-notify)
(require 'org-ladder-art)

(defvar org-sm-review-start-hook)
(defvar org-sm-after-card-change-functions)
(defvar org-sm-prompt-function)
(defvar org-sm-feedback-function)
(defvar org-sm-finish-function)
(defvar org-sm-web-hud-function)
(defvar org-sm-web-badge-function)
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

(defcustom org-ladder-sm-score-per-review 4
  "Score awarded per review event and per card."
  :type 'number
  :group 'org-ladder-sm)

(defconst org-ladder-sm--review-re
  (concat "^[ \t]*- \\(topic\\|cloze\\|dismissed\\)\\b\\(.*\\)"
          "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
  "Regexp matching an org-sm LOGBOOK review line.
Group 1 = kind, Group 2 = middle text, Group 3 = YYYY-MM-DD.")

(defun org-ladder-sm--scan-file (file tbl)
  "Scan all LOGBOOK blocks in FILE; accumulate day-key scores into TBL."
  (org-ladder-call-with-file
   file
   (lambda ()
     (let ((case-fold-search nil))
       (while (re-search-forward "^:LOGBOOK:" nil t)
         (let* ((lb-start (point))
                (lb-end   (save-excursion
                            (re-search-forward "^:END:" nil t))))
           (when lb-end
             (save-excursion
               (goto-char lb-start)
               (while (re-search-forward org-ladder-sm--review-re lb-end t)
                 (let ((rest (match-string 2))
                       (key  (org-ladder-time-parse-iso (match-string 3))))
                   (when (and key
                              (not (string-match-p
                                    "\\`[ \t]+\\(?:postpone\\|explain\\)\\b"
                                    rest)))
                     (puthash key
                              (+ (gethash key tbl 0)
                                 org-ladder-sm-score-per-review)
                              tbl))))))))))))

(defun org-ladder-sm--count-cards-in-file (file)
  "Count `:SRS_TYPE: topic|cloze' entries in FILE."
  (let ((count 0))
    (org-ladder-call-with-file
     file
     (lambda ()
       (let ((case-fold-search t))
         (while (re-search-forward "^[ \t]*:SRS_TYPE:[ \t]+\\(topic\\|cloze\\)\\b" nil t)
           (setq count (1+ count))))))
    count))

(defun org-ladder-sm--card-bonus-score ()
  "Return cumulative base score from all cards (`4 * card-count')."
  (let ((total-cards 0))
    (dolist (file (org-ladder-sm--files))
      (when (file-exists-p file)
        (condition-case err
            (setq total-cards (+ total-cards (org-ladder-sm--count-cards-in-file file)))
          (error (message "org-ladder-sm: error counting cards in %s: %s"
                          file (error-message-string err))))))
    (* total-cards org-ladder-sm-score-per-review)))

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

;;;###autoload
(with-eval-after-load 'savehist
  (defvar savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'org-ladder-sm-last-log))

(defun org-ladder-sm--reset-session ()
  "Mark `org-ladder-sm-last-log' for rotation on the next review action."
  (setq org-ladder-sm--log-fresh t))

(defun org-ladder-sm--stage-label (&optional days)
  "Return Forest stage icon for DAYS (defaulting to seedling)."
  (org-ladder-stock-stage-icon (org-ladder-stock-stage (or days 0))))

(defun org-ladder-sm--monster-label (&optional reviews)
  "Return Monster tier icon for REVIEWS."
  (org-ladder-stock-monster-icon (org-ladder-stock-monster reviews)))

(defun org-ladder-sm-banner (card)
  "Return minimal stage / Monster identity string for CARD plist."
  (pcase (plist-get card :type)
    ('topic (org-ladder-stock-monster-display
             (org-ladder-stock-monster (plist-get card :reviews))))
    ('cloze (org-ladder-stock-stage-display
             (org-ladder-stock-stage (or (plist-get card :interval-days) 0))))))

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

(defun org-ladder-sm--climb-line ()
  "Return a quiet peripheral climb line for the review HUD, or nil."
  (ignore-errors
    (let ((st (org-ladder-art-climb-state)))
      (format "🧗 %dF %s"
              (plist-get st :floor)
              (org-ladder-art-step-bar (plist-get st :step-in-floor)
                                       (plist-get st :steps-per-floor))))))

(defun org-ladder-sm-prompt (state)
  "Present current card STATE in the bottom-right HUD with context tree."
  (let* ((card    (plist-get state :card))
         (raw-pos (or (plist-get state :position) "SM"))
         (pos     (if (string-match "\\([0-9]+/[0-9]+\\)" raw-pos)
                      (format "[%s]" (match-string 1 raw-pos))
                    (format "[%s]" raw-pos)))
         (hdr     (format "%s %s" (org-ladder-sm-banner card) pos))
         (tree    (org-ladder-sm--context-tree))
         (climb   (org-ladder-sm--climb-line))
         (body    (string-join (delq nil (list tree (and climb (propertize climb 'face 'shadow))))
                               "\n"))
         (keys    (and (fboundp 'org-sm-review-keys-hint)
                       (org-sm-review-keys-hint))))
    (if (org-ladder-notify body :title hdr)
        (message "%s" (string-join (delq nil (list hdr keys)) "  |  "))
      (when keys (message "%s" keys)))))

(defun org-ladder-sm--on-card-change (event)
  "Invalidate derived score caches and refresh active HUD on card edits."
  (setq org-ladder--cache nil org-ladder--daily-cache nil org-ladder--cache-time nil)
  (when (and (fboundp 'org-sm--current-review-card-p)
             (org-sm--current-review-card-p))
    (when (eq (plist-get event :action) 'extract)
      (when org-ladder-sm--log-fresh
        (setq org-ladder-sm-last-log nil org-ladder-sm--log-fresh nil))
      (setq org-ladder-sm-last-log
            (append org-ladder-sm-last-log '("✨"))))
    (when (fboundp 'org-sm--show-prompt)
      (pcase (plist-get event :action)
        ('extract (org-sm--show-prompt "✨"))
        ('edit    (org-sm--show-prompt "✎"))))))

(defun org-ladder-sm-feedback (event)
  "Format pure icon feedback for EVENT (no interval or count noise)."
  (let* ((action (plist-get event :action))
         (card   (plist-get event :card))
         (type   (plist-get card :type))
         (rev    (or (plist-get card :reviews) 0))
         (old    (plist-get event :old-days))
         (new    (plist-get event :new-days))
         (text
          (pcase action
            ('dismiss
             (if (eq type 'topic)
                 (format "🏆 %s" (org-ladder-sm--monster-label rev))
               (format "📦 %s" (org-ladder-sm--stage-label old))))
            ('topic
             (let ((m0 (org-ladder-sm--monster-label rev))
                   (m1 (org-ladder-sm--monster-label (1+ rev))))
               (if (equal m0 m1) m1 (format "%s → %s" m0 m1))))
            ('cloze
             (let ((s0 (and old (> old 0) (org-ladder-sm--stage-label old)))
                   (s1 (org-ladder-sm--stage-label new)))
               (format "%s%s"
                       (if (eq (plist-get event :rating) :again) "↻ " "")
                       (if (and s0 (> (or new 0) 0) (not (equal s0 s1)))
                           (format "%s → %s" s0 s1)
                         s1))))
            ('postpone "⏳")
            (_ (if (fboundp 'org-sm-default-feedback)
                   (org-sm-default-feedback event)
                 (format "%s" action))))))
    (when (plist-get event :session)
      (when org-ladder-sm--log-fresh
        (setq org-ladder-sm-last-log nil org-ladder-sm--log-fresh nil))
      (setq org-ladder-sm-last-log (append org-ladder-sm-last-log (list text))))
    text))

(defun org-ladder-sm-clean-log-entry (s)
  "Normalize legacy log string S to the current icon set."
  (let* ((s1 (replace-regexp-in-string
              "\\(?:[ \t]*·.*\\|[ \t]+\\(?:幼苗\\|成株\\|常青\\|古树\\|远古\\|菜鸡\\|小猫\\|老虎\\|小怪\\|精英\\|首领\\|魔王\\|击败\\|产出子卡.*\\)\\)"
              "" s))
         (s2 (string-replace "👾" "🐣"
              (string-replace "🛡️" "🐱"
               (string-replace "⚔️" "🐯"
                (string-replace "🗡️" "✨" s1))))))
    (string-trim s2)))

(defun org-ladder-sm-bonfire (&optional prev _aborted)
  "Close review HUD posframe, display `*Org Ladder*' buffer, and return PREV."
  (org-ladder-notify-close)
  (when (fboundp 'org-ladder-show-details)
    (org-ladder-show-details))
  (org-ladder-sm--format-prev prev))

(defun org-ladder-sm-web-hud (&optional include-forest)
  "Return fine-grained climb and relic HUD plist for `org-sm-web'.
When INCLUDE-FOREST is non-nil, also include the Forest and climb chronicle."
  (let ((climb (org-ladder-art-climb-state nil include-forest)))
    (if include-forest
        (let* ((sum (org-ladder-stock-summary))
               (stages (mapcar (lambda (pair)
                                 (list :name (symbol-name (car pair))
                                       :label (org-ladder-stock-stage-icon (car pair))
                                       :count (cdr pair)))
                               (plist-get sum :stages)))
               (chronicle (mapcar (lambda (r)
                                    (list :period (plist-get r :period)
                                          :floors (plist-get r :floors)
                                          :from (plist-get r :from)
                                          :to (plist-get r :to)
                                          :tiles (plist-get r :tiles)
                                          :relics (vconcat (plist-get r :relics))))
                                  (org-ladder-art-chronicle 8))))
          (append climb (list :trees (plist-get sum :trees)
                              :stages (vconcat stages)
                              :chronicle (vconcat chronicle))))
      climb)))

;;;###autoload
(defun org-ladder-sm-setup-review-hook ()
  "Connect `org-ladder' HUD, feedback, and bonfire hooks to `org-sm'."
  (interactive)
  (add-hook 'org-sm-review-start-hook #'org-ladder-sm--reset-session)
  (add-hook 'org-sm-after-card-change-functions #'org-ladder-sm--on-card-change)
  (setq org-sm-prompt-function #'org-ladder-sm-prompt
        org-sm-feedback-function #'org-ladder-sm-feedback
        org-sm-finish-function #'org-ladder-sm-bonfire
        org-sm-web-hud-function #'org-ladder-sm-web-hud
        org-sm-web-badge-function #'org-ladder-sm-banner))

;;;###autoload
(with-eval-after-load 'org-sm
  (org-ladder-sm-setup-review-hook))

(provide 'org-ladder-sm)
;;; org-ladder-sm.el ends here
