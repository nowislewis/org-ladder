;;; org-ladder-stock.el --- Permanent knowledge stock for org-ladder  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 3.1
;; Package-Requires: ((emacs "27.1") (org "9.0") (org-ladder "3.0"))

;;; Commentary:
;; The permanent layer of org-ladder: how long each org-sm card has survived.
;;
;; Every card has exactly ONE number: its current interval in days, computed as
;; SCHEDULED - SRS_LAST.  Topic and cloze cards use the same arithmetic, so one
;; metric covers both.  Stage thresholds: 7 / 30 / 90 / 180 days.
;;
;; This layer is decoupled from the seasonal score on purpose.  It never reads
;; `org-ladder-retention-rate', never consults `org-ladder--cache', and has no
;; reset path: the monthly reset can only affect the decorative season layer,
;; never the stock computed here.  Dismissed cards are reported separately as
;; "filtered" (screened) items -- a positive processing count, not a gap.
;;
;; Scanning is pure regexp over the org files (no `org-mode' initialization),
;; following the same approach as `org-ladder-sm--scan-file'.

;;; Code:

(require 'cl-lib)
(declare-function org-ladder-time-parse-iso "org-ladder")

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

(defun org-ladder-stock-stage-display (name)
  "Return the display string for stage NAME."
  (or (nth 2 (cdr (assoc name org-ladder-stock-stages)))
      (symbol-name name)))

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

(defun org-ladder-stock--card-interval (sched last)
  "Return interval days between SCHEDULED and SRS_LAST date strings, or nil."
  (when (and sched last)
    (let* ((a (org-ladder-time-parse-iso sched))
           (b (org-ladder-time-parse-iso last)))
      (when (and a b)
        (let ((ta (apply #'encode-time 0 0 0 (reverse a)))
              (tb (apply #'encode-time 0 0 0 (reverse b))))
          (floor (/ (float-time (time-subtract ta tb)) 86400.0)))))))

(defun org-ladder-stock--scan-file (file)
  "Scan FILE and return one plist per card.
Each plist: (:title T :type TYPE :days N :reviews N :stage STAGE
:dismissed BOOL).  Reading stops at the next heading, so a parent
topic never absorbs its children's reviews."
  (let (cards)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (let* ((head-beg (line-beginning-position))
               (head-end (line-end-position))
               (head (buffer-substring-no-properties head-beg head-end))
               (next (save-excursion
                       (goto-char head-end)
                       (if (re-search-forward "^\\*+ " nil t)
                           (line-beginning-position)
                         (point-max))))
               (chunk (buffer-substring-no-properties head-beg next)))
          (when (string-match ":SRS_TYPE:[ \t]*\\(\\w+\\)" chunk)
            (let* ((type (match-string 1 chunk))
                   (dismissed (and (string-match-p ":dismissed:" head) t))
                   (sched (and (string-match "SCHEDULED:[ \t]*<\\([0-9-]+\\)" chunk)
                               (match-string 1 chunk)))
                   (last (and (string-match ":SRS_LAST:[ \t]*\\([0-9-]+\\)" chunk)
                              (match-string 1 chunk)))
                   (days (org-ladder-stock--card-interval sched last))
                   (reviews (let ((n 0) (pos 0))
                              (while (string-match "^[ \t]*- \\(?:topic\\|cloze\\)\\b" chunk pos)
                                (setq n (1+ n) pos (match-end 0)))
                              n)))
              (push (list :title (org-ladder-stock--clean-title head)
                          :type type
                          :days days
                          :reviews reviews
                          :stage (and days (org-ladder-stock-stage days))
                          :dismissed dismissed)
                    cards))))))
    cards))

(defun org-ladder-stock--clean-title (head)
  "Return HEAD stripped of its stars, SRS prefixes and trailing tags."
  (let ((s (replace-regexp-in-string "^\\*+[ \t]*" "" head)))
    (setq s (replace-regexp-in-string "^\\[T\\][ \t]*\\|^\\[C\\][ \t]*" "" s))
    (replace-regexp-in-string "[ \t]+:[A-Za-z0-9_:@]+:[ \t]*$" "" s)))

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
  "Return a summary plist of the permanent stock.
CARDS defaults to `org-ladder-stock-cards'.  Keys:
  :stages      alist of (STAGE-NAME . COUNT), scheduled cards only
  :scheduled   number of live (non-dismissed) cards with a computed interval
  :unscheduled live cards with no interval yet
  :dismissed   count of screened-out cards (a positive processing count)
  :total       every card seen
  :deepest     the card with the largest interval, or nil"
  (let* ((cards (or cards (org-ladder-stock-cards)))
         (live (cl-remove-if (lambda (c) (plist-get c :dismissed)) cards))
         (sched (cl-remove-if-not (lambda (c) (plist-get c :days)) live))
         (counts (make-hash-table :test 'eq)))
    (dolist (c sched)
      (let ((st (plist-get c :stage)))
        (puthash st (1+ (gethash st counts 0)) counts)))
    (list :stages (mapcar (lambda (s)
                            (cons (car s) (gethash (car s) counts 0)))
                          org-ladder-stock-stages)
          :scheduled (length sched)
          :unscheduled (- (length live) (length sched))
          :dismissed (length (cl-remove-if-not (lambda (c) (plist-get c :dismissed)) cards))
          :total (length cards)
          :deepest (car (sort (copy-sequence sched)
                              (lambda (a b) (> (or (plist-get a :days) 0)
                                               (or (plist-get b :days) 0))))))))

(defun org-ladder-stock-insert-section (&optional cards)
  "Insert the permanent stock summary block into the current buffer."
  (let* ((cards (or cards (org-ladder-stock-cards)))
         (sum (org-ladder-stock-summary cards)))
    (insert (propertize "📚 知识存量 (Permanent Stock · 永不重置)\n"
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
      (insert (format "\n  已排期 %d · 未排期 %d · 已筛选 %d\n"
                      (plist-get sum :scheduled)
                      (plist-get sum :unscheduled)
                      (plist-get sum :dismissed)))
      (when-let* ((deep (plist-get sum :deepest)))
        (insert (format "  最远   %dd  %s\n"
                        (plist-get deep :days) (plist-get deep :title))))
      (insert "\n"))))

;;;###autoload
(defun org-ladder-stock-show (&optional n)
  "Display the permanent stock ledger.
With prefix argument N, also list the N deepest cards (default 20 when called
with any prefix)."
  (interactive "P")
  (let* ((cards (org-ladder-stock-cards))
         (sum (org-ladder-stock-summary cards))
         (n (if n (prefix-numeric-value n) 0))
         (buf (get-buffer-create "*Org Ladder Stock*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (fundamental-mode)
        (insert (propertize "📚 知识存量 (Permanent Stock)\n"
                            'face '(:weight bold :height 1.3 :foreground "#A6E22E")))
        (insert (propertize "间隔 = SCHEDULED - SRS_LAST，只增不减，永不重置\n"
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
        (insert (format "  已排期卡片    %d\n" (plist-get sum :scheduled)))
        (insert (format "  未排期        %d\n" (plist-get sum :unscheduled)))
        (insert (format "  已筛选        %d  （你的判断，不是缺口）\n"
                        (plist-get sum :dismissed)))
        (when-let* ((deep (plist-get sum :deepest)))
          (insert (format "\n  最深的一张    %dd  %s\n"
                          (plist-get deep :days) (plist-get deep :title))))
        (when (> n 0)
          (insert (propertize (format "\n🛡️ 最远的 %d 张\n" n)
                              'face '(:weight bold :height 1.2)))
          (let ((sched (cl-remove-if-not (lambda (c) (plist-get c :days)) cards)))
            (dolist (c (seq-take (sort sched (lambda (a b)
                                               (> (plist-get a :days)
                                                  (plist-get b :days))))
                                 n))
              (insert (format "  %4dd  %-6s %s\n"
                              (plist-get c :days)
                              (plist-get c :type)
                              (plist-get c :title))))))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (display-buffer buf)))

(provide 'org-ladder-stock)
;;; org-ladder-stock.el ends here
