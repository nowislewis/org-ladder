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
  '((mob   . (0 2   "👾 小怪"))
    (elite . (2 5   "🛡️ 精英"))
    (boss  . (5 8   "⚔️ 首领"))
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
               (list :stage (and (eq type 'cloze) days (org-ladder-stock-stage days))
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
    (dolist (c sched)
      (let ((st (plist-get c :stage)))
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
  "Insert the clickable Forest and Monster overview blocks for SUM."
  (insert (propertize (format "📚 知识森林 · 共 %d 棵树 (已排期 %d · 待萌发 %d)\n"
                              (or (plist-get sum :trees) 0)
                              (plist-get sum :scheduled)
                              (plist-get sum :unscheduled))
                      'face '(:weight bold :height 1.2)))
  (let ((max-c (max 1 (apply #'max (cons 1 (mapcar #'cdr (plist-get sum :stages)))))))
    (dolist (pair (plist-get sum :stages))
      (let* ((name  (car pair))
             (count (cdr pair))
             (len   (if (= count 0) 0 (max 1 (round (* 24 (/ (float count) max-c))))))
             (bar   (make-string len ?█))
             (btn   (org-ladder-stock--tree-button
                     (org-ladder-stock-stage-display name)
                     (concat "@" (symbol-name name)))))
        (insert "  " btn)
        (org-ladder-stock--align-to 12)
        (insert (format "%4d" count))
        (org-ladder-stock--align-to 18)
        (insert (propertize bar 'face '(:foreground "#66D9EF")) "\n"))))
  (when-let* ((deep (plist-get sum :deepest)))
    (insert "  最远")
    (org-ladder-stock--align-to 12)
    (insert (format "%3dd" (plist-get deep :interval-days)))
    (org-ladder-stock--align-to 18)
    (insert (org-ladder-stock--title-button deep) "\n"))
  (let* ((monsters (plist-get sum :monsters))
         (live-tot (apply #'+ (mapcar #'cadr monsters)))
         (slain-tot (apply #'+ (mapcar #'caddr monsters)))
         (max-m (max 1 (apply #'max (cons 1 (mapcar #'cadr monsters))))))
    (insert "\n"
            (propertize (format "⚔️ 守林魔物 · 存活 %d · 已讨伐 %d（已削血 %d）\n"
                                live-tot slain-tot (plist-get sum :boss-wounded))
                        'face '(:weight bold :height 1.2)))
    (dolist (row monsters)
      (pcase-let* ((`(,name ,live ,slain) row)
                   (len (if (= live 0) 0 (max 1 (round (* 20 (/ (float live) max-m))))))
                   (bar (make-string len ?█))
                   (btn (org-ladder-stock--tree-button
                         (org-ladder-stock-monster-display name)
                         (concat "@" (symbol-name name)))))
        (insert "  " btn)
        (org-ladder-stock--align-to 12)
        (insert (format "%4d" live))
        (org-ladder-stock--align-to 18)
        (insert (propertize bar 'face '(:foreground "#F92672")))
        (org-ladder-stock--align-to 40)
        (insert (format "(已斩 %d)\n" slain)))))
  (insert "\n"))

(defun org-ladder-stock-insert-section (&optional cards top-n)
  "Insert the permanent Cloze stock and Monster pipeline block into current buffer.
When TOP-N is positive, also list the TOP-N deepest Clozes and oldest Monsters."
  (let* ((cards (or cards (org-ladder-stock-cards)))
         (sum (org-ladder-stock-summary cards))
         (n   (or top-n 0)))
    (if (= (plist-get sum :total) 0)
        (insert "📚 知识森林\n  (no org-sm cards found)\n\n")
      (org-ladder-stock--insert-overview sum)
      (when (> n 0)
        (insert (propertize (format "🛡️ 最远的 %d 棵树\n" n)
                            'face '(:weight bold :height 1.15)))
        (let ((sched (cl-remove-if-not
                      (lambda (c)
                        (and (eq (plist-get c :type) 'cloze)
                             (not (plist-get c :dismissed))
                             (plist-get c :interval-days)))
                      cards)))
          (dolist (c (seq-take (sort sched (lambda (a b)
                                             (> (plist-get a :interval-days)
                                                (plist-get b :interval-days))))
                               n))
            (insert (format "  %4dd  %s\n"
                            (plist-get c :interval-days)
                            (org-ladder-stock--title-button c)))))
        (when-let* ((bosses (seq-take (plist-get sum :bosses) n)))
          (insert (propertize (format "\n👹 盘踞最久的 %d 只魔物\n" (length bosses))
                              'face '(:weight bold :height 1.15)))
          (dolist (b bosses)
            (let ((ct (or (plist-get b :child-topics) 0))
                  (cc (or (plist-get b :child-clozes) 0)))
              (insert (format "  照面 %2d 次  %-12s %s\n"
                              (plist-get b :reviews)
                              (if (> (+ ct cc) 0)
                                  (format "[削血 %dT/%dC]" ct cc)
                                "[0子卡]")
                              (org-ladder-stock--title-button b))))))
        (insert "\n"))
      (org-ladder-stock-insert-chronicle cards))))

(defun org-ladder-stock--period-lines (cards pred)
  "Return per-level Cloze gain and Monster kill lines for CARDS matching PRED."
  (let ((st (make-hash-table :test 'eq))
        (sl (make-hash-table :test 'eq))
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
                   (puthash m (1+ (gethash m sl 0)) sl)))))))
    (dolist (m org-ladder-stock-monsters)
      (let ((n (gethash (car m) sl 0)))
        (when (> n 0)
          (push (format "%s  斩 %d" (nth 2 (cdr m)) n) lines))))
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
