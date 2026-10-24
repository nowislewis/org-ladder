;;; org-ladder-stock.el --- Permanent knowledge stock for org-ladder  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 3.1
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-ladder "3.0") (org-sm "0"))

;;; Commentary:
;; The permanent layer of org-ladder: how long each org-sm card has survived.
;;
;; Cloze intervals (SCHEDULED - SRS_LAST) form the permanent Forest histogram.
;; Topic repetitions and descendant cards form a separate Guardian Boss pipeline.
;; Stage thresholds: 7 / 30 / 90 / 180 days.
;;
;; This layer is decoupled from the seasonal score on purpose.  It never reads
;; `org-ladder-retention-rate', never consults `org-ladder--cache', and has no
;; reset path: the monthly reset can only affect the decorative season layer,
;; never the stock computed here.  Dismissed cards are reported separately as
;; "filtered" (screened) items -- a positive processing count, not a gap.
;;
;; Card storage is read through the lightweight `org-sm-card' interface.
;; This file owns classification and presentation, not Org property parsing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org-sm-card)

(defgroup org-ladder-stock nil
  "Permanent knowledge stock for org-ladder."
  :group 'org-ladder
  :prefix "org-ladder-stock-")

(defcustom org-ladder-stock-files nil
  "Source of org files to scan for permanent stock.
Either a list of files, or a function of no arguments returning such a list.
Prefer a function (e.g. `org-sm-files') so the file set is resolved live."
  :type '(choice (repeat file) function)
  :group 'org-ladder-stock)

(defcustom org-ladder-stock-boss-threshold 5
  "Minimum review count for a topic card to become a Boss."
  :type 'integer
  :group 'org-ladder-stock)

;;; ── Stage thresholds ─────────────────────────────────────────────────────────

(defconst org-ladder-stock-stages
  '((seedling . (0   7   "🌱 幼苗"))
    (plant    . (7   30  "🌿 成株"))
    (evergreen . (30  90  "🌲 常青"))
    (ancient  . (90  180 "🗿 古树"))
    (elder    . (180 nil "⛰️ 远古")))
  "Alist of (NAME MIN MAX DISPLAY).  MIN inclusive, MAX exclusive; nil = unbounded.")

(defun org-ladder-stock-stage (days)
  "Return the stage name for an interval of DAYS."
  (catch 'found
    (dolist (s org-ladder-stock-stages 'elder)
      (let ((min (nth 0 (cdr s)))
            (max (nth 1 (cdr s))))
        (when (and (>= days min) (or (null max) (< days max)))
          (throw 'found (car s)))))))

(defun org-ladder-stock-stage-index (days)
  "Return 0..4 stage index for DAYS, or nil when DAYS is nil."
  (when days
    (cl-position (org-ladder-stock-stage days)
                 org-ladder-stock-stages
                 :key #'car)))

(defun org-ladder-stock-stage-display (name)
  "Return the display string for stage NAME."
  (or (nth 2 (cdr (assoc name org-ladder-stock-stages)))
      (symbol-name name)))

(defconst org-ladder-stock-monsters
  '((mob   . (0 2   "🐣 菜鸡"))
    (elite . (2 5   "🐱 小猫"))
    (boss  . (5 8   "🐯 老虎"))
    (demon . (8 nil "👹 魔王")))
  "Alist of (NAME MIN-REV MAX-REV DISPLAY) for Topic monster tiers.")

(defun org-ladder-stock-monster (reviews)
  "Return the monster tier symbol (`mob', `elite', `boss', `demon') for REVIEWS."
  (let ((r (or reviews 0)))
    (catch 'found
      (dolist (m org-ladder-stock-monsters 'demon)
        (let ((min (nth 0 (cdr m)))
              (max (nth 1 (cdr m))))
          (when (and (>= r min) (or (null max) (< r max)))
            (throw 'found (car m))))))))

(defun org-ladder-stock-monster-display (name)
  "Return the display string for monster tier NAME."
  (or (nth 2 (cdr (assoc name org-ladder-stock-monsters)))
      (symbol-name name)))

(defun org-ladder-stock-stage-icon (name)
  "Return the icon-only symbol for stage NAME."
  (car (split-string (org-ladder-stock-stage-display name))))

(defun org-ladder-stock-monster-icon (name)
  "Return the icon-only symbol for monster tier NAME."
  (car (split-string (org-ladder-stock-monster-display name))))

(defun org-ladder-stock-boss-state (type reviews dismissed &optional child-topics child-clozes)
  "Return Boss state (`active', `wounded', `slain', or nil) for a card."
  (when (and (member type '("topic" topic))
             (>= (or reviews 0) org-ladder-stock-boss-threshold))
    (cond
     (dismissed 'slain)
     ((> (+ (or child-topics 0) (or child-clozes 0)) 0) 'wounded)
     (t 'active))))

;;; ── Scanning ─────────────────────────────────────────────────────────────────

(defun org-ladder-stock--resolve-files (source)
  "Resolve a file list or zero-argument function SOURCE."
  (when (and (eq source 'org-sm-files) (not (fboundp source)))
    (require 'org-sm-card))
  (let ((files (cond ((null source) nil)
                     ((and (listp source) (not (functionp source))) source)
                     ((or (symbolp source) (functionp source))
                      (unless (functionp source)
                        (user-error "Undefined file-source function: %S" source))
                      (funcall source))
                     (t (user-error "Invalid file source: %S" source)))))
    (unless (and (listp files) (cl-every #'stringp files))
      (user-error "File source must return a list of paths: %S" source))
    files))

(defun org-ladder-stock--files ()
  "Resolve `org-ladder-stock-files' to a concrete list of files.
Falls back to `org-ladder-sm--files' when unset so one file list serves both."
  (cond (org-ladder-stock-files
         (org-ladder-stock--resolve-files org-ladder-stock-files))
        ((fboundp 'org-ladder-sm--files)
         (org-ladder-sm--files))))

(defun org-ladder-stock--scan-file (file)
  "Enrich canonical cards from FILE with Forest and Monster classifications."
  (mapcar
   (lambda (card)
     (let ((type (plist-get card :type))
           (days (plist-get card :interval-days))
           (rev  (plist-get card :reviews)))
       (append card
               (list :stage (and (eq type 'cloze)
                                 (org-ladder-stock-stage (or days 0)))
                     :monster (and (eq type 'topic) (org-ladder-stock-monster rev))
                     :boss-state (org-ladder-stock-boss-state
                                  type rev
                                  (plist-get card :dismissed)
                                  (plist-get card :child-topics)
                                  (plist-get card :child-clozes))))))
   (org-sm-cards-in-file file)))

(declare-function org-id-goto "org-id")
(declare-function org-sm-tree "org-sm")

(defun org-ladder-stock--title-button (card)
  "Return CARD's title, clickable via `org-id-goto' when `:id' is present."
  (let ((title (or (plist-get card :title) ""))
        (id    (plist-get card :id)))
    (if id
        (buttonize title (lambda (cid) (require 'org-id) (org-id-goto cid)) id)
      title)))

(defun org-ladder-stock--tree-button (label tag)
  "Return LABEL as a button that opens `org-sm-tree' filtered to TAG."
  (buttonize label (lambda (kw) (require 'org-sm) (org-sm-tree nil kw)) tag))

;;;###autoload
(defun org-ladder-stock-cards ()
  "Return a list of plists describing every org-sm card found.
Scans `org-ladder-stock-files' fresh on each call; no caching, since the
result is cheap and must never lag behind a review."
  (let (cards)
    (dolist (file (org-ladder-stock--files))
      (when (file-exists-p file)
        (condition-case err
            (setq cards (nconc (org-ladder-stock--scan-file file) cards))
          (error (message "org-ladder-stock: error in %s: %s"
                          file (error-message-string err))))))
    cards))

;;;###autoload
(defun org-ladder-stock-summary (&optional cards)
  "Return a summary plist of the permanent Cloze forest and Topic Boss pipeline.
CARDS defaults to `org-ladder-stock-cards'.  Keys:
  :trees         number of live Cloze cards
  :stages        alist of (STAGE-NAME . COUNT), scheduled Cloze cards only
  :monsters      list of (MONSTER-NAME LIVE-COUNT SLAIN-COUNT)
  :scheduled     number of live scheduled Cloze cards
  :unscheduled   live Cloze cards with no interval yet
  :boss-wounded  count of live Boss topics that already have child cards
  :bosses        live Boss card plists sorted by review count descending
  :total         every card seen
  :deepest       the live Cloze card with the largest interval, or nil"
  (let* ((cards (or cards (org-ladder-stock-cards)))
         (clozes (cl-remove-if-not
                  (lambda (c)
                    (and (eq (plist-get c :type) 'cloze)
                         (not (plist-get c :dismissed))))
                  cards))
         (sched (cl-remove-if-not (lambda (c) (plist-get c :interval-days)) clozes))
         (bosses (cl-remove-if-not
                  (lambda (c)
                    (memq (plist-get c :boss-state) '(active wounded)))
                  cards))
         (wounded (cl-count-if
                   (lambda (c) (eq (plist-get c :boss-state) 'wounded))
                   bosses))
         (counts (make-hash-table :test 'eq))
         (m-live (make-hash-table :test 'eq))
         (m-slain (make-hash-table :test 'eq)))
    (dolist (c clozes)
      (let ((st (or (plist-get c :stage) 'seedling)))
        (puthash st (1+ (gethash st counts 0)) counts)))
    (dolist (c cards)
      (when (eq (plist-get c :type) 'topic)
        (let ((m   (or (plist-get c :monster)
                       (org-ladder-stock-monster (plist-get c :reviews))))
              (tbl (if (plist-get c :dismissed) m-slain m-live)))
          (puthash m (1+ (gethash m tbl 0)) tbl))))
    (list :trees (length clozes)
          :stages (mapcar (lambda (s)
                            (cons (car s) (gethash (car s) counts 0)))
                          org-ladder-stock-stages)
          :monsters (mapcar (lambda (m)
                              (list (car m)
                                    (gethash (car m) m-live 0)
                                    (gethash (car m) m-slain 0)))
                            org-ladder-stock-monsters)
          :scheduled (length sched)
          :unscheduled (- (length clozes) (length sched))
          :boss-wounded wounded
          :bosses (sort (copy-sequence bosses)
                        (lambda (a b)
                          (> (or (plist-get a :reviews) 0)
                             (or (plist-get b :reviews) 0))))
          :total (length cards)
          :deepest (car (sort (copy-sequence sched)
                              (lambda (a b) (> (or (plist-get a :interval-days) 0)
                                               (or (plist-get b :interval-days) 0))))))))

(defun org-ladder-stock--align-to (col)
  "Insert spaces up to column COL with pixel-exact `:align-to' display property."
  (let ((pad (max 1 (- col (current-column)))))
    (insert (propertize (make-string pad ?\s)
                        'display `(space :align-to ,col)))))

(defun org-ladder-stock--insert-overview (sum)
  "Insert the clickable 5-stage Cloze Forest overview for SUM."
  (let ((live-topics (apply #'+ (mapcar #'cadr (plist-get sum :monsters)))))
    (insert (propertize (format "📚 知识森林 · 共 %d 棵树"
                                (or (plist-get sum :trees) 0))
                        'face '(:weight bold :height 1.2))
            (if (> live-topics 0)
                (propertize (format "  (另有 %d 张阅读卡待拆解)\n" live-topics)
                            'face 'shadow)
              "\n")
            (propertize "  🌱幼苗 · 🌿成株 · 🌲常青 · 🗿古树 · ⛰️远古\n"
                        'face 'shadow)))
  (let ((max-c (max 1 (apply #'max (cons 1 (mapcar #'cdr (plist-get sum :stages)))))))
    (dolist (pair (plist-get sum :stages))
      (let* ((name  (car pair))
             (count (cdr pair))
             (len   (if (= count 0) 0 (max 1 (round (* 24 (/ (float count) max-c))))))
             (bar   (make-string len ?█))
             (btn   (org-ladder-stock--tree-button
                     (org-ladder-stock-stage-icon name)
                     (concat "@" (symbol-name name)))))
        (insert "  " btn)
        (org-ladder-stock--align-to 7)
        (insert (format "%4d" count))
        (org-ladder-stock--align-to 13)
        (insert (propertize bar 'face '(:foreground "#66D9EF")) "\n"))))
  (insert "\n"))

(defun org-ladder-stock-insert-section (&optional cards _top-n)
  "Insert the permanent Cloze Forest stock block into current buffer."
  (let* ((cards (or cards (org-ladder-stock-cards)))
         (sum (org-ladder-stock-summary cards)))
    (if (= (plist-get sum :total) 0)
        (insert "📚 知识森林\n  (no org-sm cards found)\n\n")
      (org-ladder-stock--insert-overview sum))))

(defun org-ladder-stock--period-lines (cards pred)
  "Return Cloze gain and Monster kill/spawn lines for CARDS matching PRED."
  (let ((st (make-hash-table :test 'eq))
        (sl (make-hash-table :test 'eq))
        (new-topics 0)
        lines)
    (dolist (c cards)
      (when-let* ((d (plist-get c :date)))
        (when (funcall pred d)
          (cond ((and (eq (plist-get c :type) 'cloze)
                      (not (plist-get c :dismissed))
                      (plist-get c :stage))
                 (let ((s (plist-get c :stage)))
                   (puthash s (1+ (gethash s st 0)) st)))
                ((and (eq (plist-get c :type) 'topic)
                      (plist-get c :dismissed))
                 (let ((m (or (plist-get c :monster)
                              (org-ladder-stock-monster (plist-get c :reviews)))))
                   (puthash m (1+ (gethash m sl 0)) sl)))
                ((and (eq (plist-get c :type) 'topic)
                      (zerop (or (plist-get c :reviews) 0)))
                 (setq new-topics (1+ new-topics)))))))
    (dolist (m org-ladder-stock-monsters)
      (let ((n (gethash (car m) sl 0)))
        (when (> n 0)
          (push (format "%s  斩 %d" (nth 2 (cdr m)) n) lines))))
    (when (> new-topics 0)
      (push (format "🐣 新魔物  +%d" new-topics) lines))
    (dolist (s org-ladder-stock-stages)
      (let ((n (gethash (car s) st 0)))
        (when (> n 0)
          (push (format "%s  +%d" (nth 2 (cdr s)) n) lines))))
    lines))

(defun org-ladder-stock--insert-period (title lines)
  "Insert chronicle section TITLE followed by indented LINES."
  (when lines
    (insert (propertize (format "  %s\n" title) 'face 'bold))
    (dolist (l lines)
      (insert (format "    %s\n" l)))))

(defun org-ladder-stock-insert-chronicle (cards)
  "Insert Today, This Week, and Monthly Forest/Monster growth chronicle for CARDS."
  (let* ((now      (current-time))
         (today    (format-time-string "%F" now))
         (week-ago (format-time-string "%F" (time-subtract now (days-to-time 6))))
         (yms      (seq-take
                    (sort (seq-uniq
                           (delq nil (mapcar (lambda (c)
                                               (when-let* ((d (plist-get c :date)))
                                                 (substring d 0 7)))
                                             cards)))
                          #'string>)
                    12)))
    (insert (propertize "📅 成长与讨伐编年史\n" 'face '(:weight bold :height 1.2)))
    (org-ladder-stock--insert-period
     "今日" (org-ladder-stock--period-lines cards (lambda (d) (equal d today))))
    (org-ladder-stock--insert-period
     "近7天" (org-ladder-stock--period-lines
              cards (lambda (d) (not (string< d week-ago)))))
    (dolist (ym yms)
      (org-ladder-stock--insert-period
       ym (org-ladder-stock--period-lines
           cards (lambda (d) (string-prefix-p ym d)))))
    (insert "\n")))

(provide 'org-ladder-stock)
;;; org-ladder-stock.el ends here
