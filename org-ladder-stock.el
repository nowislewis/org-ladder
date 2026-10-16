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

(defun org-ladder-stock-boss-state (type reviews dismissed &optional child-topics child-clozes)
  "Return Boss state (`active', `wounded', `slain', or nil) for a card."
  (when (and (member type '("topic" topic))
             (>= (or reviews 0) org-ladder-stock-boss-threshold))
    (cond
     (dismissed 'slain)
     ((> (+ (or child-topics 0) (or child-clozes 0)) 0) 'wounded)
     (t 'active))))

;;; ── Scanning ─────────────────────────────────────────────────────────────────

(defun org-ladder-stock--files ()
  "Resolve `org-ladder-stock-files' to a concrete list of files.
Falls back to `org-ladder-sm--files' when unset so one file list serves both."
  (cond (org-ladder-stock-files
         (if (functionp org-ladder-stock-files)
             (funcall org-ladder-stock-files)
           org-ladder-stock-files))
        ((fboundp 'org-ladder-sm--files)
         (org-ladder-sm--files))))

(defun org-ladder-stock--scan-file (file)
  "Enrich canonical cards from FILE with Forest and Boss classifications."
  (mapcar
   (lambda (card)
     (let ((type (plist-get card :type))
           (days (plist-get card :interval-days)))
       (append card
               (list :stage (and (eq type 'cloze) days (org-ladder-stock-stage days))
                     :boss-state (org-ladder-stock-boss-state
                                  type (plist-get card :reviews)
                                  (plist-get card :dismissed)
                                  (plist-get card :child-topics)
                                  (plist-get card :child-clozes))))))
   (org-sm-cards-in-file file)))

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
  :stages        alist of (STAGE-NAME . COUNT), scheduled Cloze cards only
  :scheduled     number of live scheduled Cloze cards
  :unscheduled   live Cloze cards with no interval yet
  :dismissed     count of screened non-Boss cards (positive processing count)
  :boss-slain    count of dismissed Boss topics (reviews >= threshold)
  :boss-active   count of live Boss topics (including wounded)
  :boss-wounded  count of live Boss topics that already have child cards
  :topic-pool    count of live non-Boss topics in the reading pool
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
         (live-bosses (cl-remove-if-not
                       (lambda (c)
                         (memq (plist-get c :boss-state) '(active wounded)))
                       cards))
         (wounded-bosses (cl-remove-if-not
                          (lambda (c) (eq (plist-get c :boss-state) 'wounded))
                          live-bosses))
         (slain-bosses (cl-remove-if-not
                        (lambda (c) (eq (plist-get c :boss-state) 'slain))
                        cards))
         (topic-pool (cl-remove-if-not
                      (lambda (c)
                        (and (eq (plist-get c :type) 'topic)
                             (not (plist-get c :dismissed))
                             (null (plist-get c :boss-state))))
                      cards))
         (screened (cl-remove-if-not
                    (lambda (c)
                      (and (plist-get c :dismissed)
                           (not (eq (plist-get c :boss-state) 'slain))))
                    cards))
         (counts (make-hash-table :test 'eq)))
    (dolist (c sched)
      (let ((st (plist-get c :stage)))
        (puthash st (1+ (gethash st counts 0)) counts)))
    (list :stages (mapcar (lambda (s)
                            (cons (car s) (gethash (car s) counts 0)))
                          org-ladder-stock-stages)
          :scheduled (length sched)
          :unscheduled (- (length clozes) (length sched))
          :dismissed (length screened)
          :boss-slain (length slain-bosses)
          :boss-active (length live-bosses)
          :boss-wounded (length wounded-bosses)
          :topic-pool (length topic-pool)
          :bosses (sort (copy-sequence live-bosses)
                        (lambda (a b)
                          (> (or (plist-get a :reviews) 0)
                             (or (plist-get b :reviews) 0))))
          :total (length cards)
          :deepest (car (sort (copy-sequence sched)
                              (lambda (a b) (> (or (plist-get a :interval-days) 0)
                                               (or (plist-get b :interval-days) 0))))))))

(defun org-ladder-stock-insert-section (&optional cards)
  "Insert the permanent Cloze stock and Boss pipeline block into current buffer."
  (let* ((cards (or cards (org-ladder-stock-cards)))
         (sum (org-ladder-stock-summary cards)))
    (insert (propertize "📚 知识存量 (Cloze 森林 · 永不重置)\n"
                        'face '(:weight bold :height 1.2)))
    (if (= (plist-get sum :total) 0)
        (insert "  (no org-sm cards found)\n\n")
      (let ((max-c (max 1 (apply #'max (mapcar #'cdr (plist-get sum :stages))))))
        (dolist (pair (plist-get sum :stages))
          (let* ((name (car pair)) (count (cdr pair))
                 (len  (if (= count 0) 0 (max 1 (round (* 24 (/ (float count) max-c))))))
                 (bar  (make-string len ?█)))
            (insert (format "  %-10s %4d  %s\n"
                            (or (org-ladder-stock-stage-display name) "")
                            count
                            (propertize bar 'face '(:foreground "#66D9EF")))))))
      (insert (format "\n  Cloze 已排期 %d · 未排期 %d · 已筛选 %d\n"
                      (plist-get sum :scheduled)
                      (plist-get sum :unscheduled)
                      (plist-get sum :dismissed)))
      (insert (format "  ⚔️ 守林 Boss  已击败 %d · 攻略中 %d（已削血 %d） · 阅读池 %d\n"
                      (plist-get sum :boss-slain)
                      (plist-get sum :boss-active)
                      (plist-get sum :boss-wounded)
                      (plist-get sum :topic-pool)))
      (when-let* ((deep (plist-get sum :deepest)))
        (insert (format "  最远   %dd  %s\n"
                        (plist-get deep :interval-days) (plist-get deep :title))))
      (insert "\n"))))

;;;###autoload
(defun org-ladder-stock-show (&optional n)
  "Display the permanent stock ledger.
With prefix argument N, also list the N deepest Cloze cards and N top Bosses."
  (interactive "P")
  (let* ((cards (org-ladder-stock-cards))
         (sum (org-ladder-stock-summary cards))
         (n (if n (prefix-numeric-value n) 0))
         (buf (get-buffer-create "*Org Ladder Stock*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (insert (propertize "📚 知识存量 (Cloze 森林)\n"
                            'face '(:weight bold :height 1.3 :foreground "#A6E22E")))
        (insert (propertize "间隔 = SCHEDULED - SRS_LAST，仅统计 Cloze，永不重置\n"
                            'face '(:foreground "#75715E")))
        (insert (propertize "────────────────────────────────────────\n\n"
                            'face '(:foreground "#444444")))
        (dolist (pair (plist-get sum :stages))
          (let* ((name (car pair)) (count (cdr pair))
                 (bar  (make-string (min count 40) ?█)))
            (insert (format "  %-10s %4d  %s\n"
                            (or (org-ladder-stock-stage-display name) "")
                            count
                            (propertize bar 'face '(:foreground "#66D9EF"))))))
        (insert "\n")
        (insert (format "  Cloze 已排期  %d\n" (plist-get sum :scheduled)))
        (insert (format "  Cloze 未排期  %d\n" (plist-get sum :unscheduled)))
        (insert (format "  已筛选        %d  （常规筛选，不是缺口）\n"
                        (plist-get sum :dismissed)))
        (insert (format "  ⚔️ 守林 Boss  已击败 %d · 攻略中 %d（已削血 %d） · 阅读池 %d\n"
                        (plist-get sum :boss-slain)
                        (plist-get sum :boss-active)
                        (plist-get sum :boss-wounded)
                        (plist-get sum :topic-pool)))
        (when-let* ((deep (plist-get sum :deepest)))
          (insert (format "\n  最深的一棵    %dd  %s\n"
                          (plist-get deep :interval-days) (plist-get deep :title))))
        (when (> n 0)
          (insert (propertize (format "\n🛡️ 最远的 %d 棵 Cloze\n" n)
                              'face '(:weight bold :height 1.2)))
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
                              (plist-get c :title)))))
          (when-let* ((bosses (seq-take (plist-get sum :bosses) n)))
            (insert (propertize (format "\n⚔️ 盘踞最久的 %d 只守林 Boss\n"
                                        (length bosses))
                                'face '(:weight bold :height 1.2)))
            (dolist (b bosses)
              (let ((ct (or (plist-get b :child-topics) 0))
                    (cc (or (plist-get b :child-clozes) 0)))
                (insert (format "  照面 %2d 次  %-12s %s\n"
                                (plist-get b :reviews)
                                (if (> (+ ct cc) 0)
                                    (format "[削血 %dT/%dC]" ct cc)
                                  "[0 cloze]")
                                (plist-get b :title)))))))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (display-buffer buf)))

(provide 'org-ladder-stock)
;;; org-ladder-stock.el ends here
