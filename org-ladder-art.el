;;; org-ladder-art.el --- Vector relic gallery for org-ladder -*- lexical-binding: t; -*-

(require 'org-ladder)
(require 'cl-lib)
(require 'seq)

(declare-function svg-create "svg")
(declare-function svg-rectangle "svg")
(declare-function svg-polygon "svg")
(declare-function svg-circle "svg")
(declare-function svg-line "svg")
(declare-function svg-image "svg")

(defcustom org-ladder-art-directory
  (expand-file-name "arts" (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory storing plain Lisp vector relic `.art' files (named YYYY-MM.art)."
  :type 'directory
  :group 'org-ladder)

(defun org-ladder-art--read (ym-str)
  "Read plain Lisp relic plist for YM-STR from `org-ladder-art-directory'."
  (let ((file (expand-file-name (format "%s.art" ym-str) org-ladder-art-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun org-ladder-art--available-keys ()
  "Return chronologically sorted list of YM-STR keys for all `.art' files."
  (when (file-directory-p org-ladder-art-directory)
    (sort (mapcar #'file-name-base
                  (directory-files org-ladder-art-directory nil "\\.art\\'"))
          #'string<)))

(defun org-ladder-art--unlock-dates (step)
  "Return vector of unlock date strings (\"YYYY-MM-DD\") per STEP-point threshold."
  (let ((days (sort (org-ladder-hash-to-alist (org-ladder--collect-daily))
                    (lambda (a b) (value< (car a) (car b)))))
        (acc 0)
        (next step)
        dates)
    (dolist (d days)
      (setq acc (+ acc (cdr d)))
      (while (>= acc next)
        (pcase-let ((`(,y ,m ,day) (car d)))
          (push (format "%04d-%02d-%02d" y m day) dates))
        (setq next (+ next step))))
    (vconcat (nreverse dates))))

(defun org-ladder-art--allocations ()
  "Allocate cumulative total score sequentially across available `.art' files.
Each artifact requires 4500 points; points spill into the next artifact once
the previous one reaches 4500.  Return list of (KEY ALLOC ITEM UNLOCK-DATE)."
  (let ((remaining (apply #'+ (mapcar #'cdr (org-ladder-calculate-scores))))
        (step 4500)
        (dates (org-ladder-art--unlock-dates 4500))
        (idx 0)
        result)
    (dolist (ym-str (org-ladder-art--available-keys))
      (when-let* ((item (org-ladder-art--read ym-str)))
        (let ((alloc (min step (max 0 remaining)))
              (date  (and (< idx (length dates)) (aref dates idx))))
          (setq remaining (max 0 (- remaining alloc))
                idx (1+ idx))
          (push (list ym-str alloc item date) result))))
    (nreverse result)))

(defun org-ladder-art--active-entry ()
  "Return the (YM-STR ALLOC ITEM) currently being excavated, or the last one."
  (let ((allocs (org-ladder-art--allocations)))
    (or (cl-find-if (lambda (e) (< (nth 1 e) 4500)) allocs)
        (car (last allocs)))))

(defun org-ladder-art--shuffle-indices (indices seed)
  "Return deterministically shuffled INDICES using integer SEED."
  (let ((vec (vconcat indices))
        (s seed))
    (dotimes (i (length vec))
      (setq s (logand (+ (* s 1103515245) 12345) #x7FFFFFFF))
      (let* ((j (mod s (length vec)))
             (tmp (aref vec i)))
        (aset vec i (aref vec j))
        (aset vec j tmp)))
    (append vec nil)))

(defun org-ladder-art--render-svg (ym-str item ratio)
  "Render vector shapes in ITEM with 6x6 sector fog at unlock RATIO (0.0..1.0)."
  (require 'svg)
  (let* ((w 264) (h 240)
         (cols 6) (rows 6)
         (cw (/ w cols)) (ch (/ h rows))
         (total (* cols rows))
         (open-n (min total (round (* total ratio))))
         (order (org-ladder-art--shuffle-indices
                 (number-sequence 0 (1- total))
                 (abs (sxhash ym-str))))
         (open-set (seq-take order open-n))
         (svg (svg-create w h)))
    (svg-rectangle svg 0 0 w h :fill "#0D1117" :rx 8 :ry 8)
    (dolist (sh (plist-get item :shapes))
      (pcase (car sh)
        ('poly   (apply #'svg-polygon svg (cadr sh) (cddr sh)))
        ('circle (apply #'svg-circle svg (nth 1 sh) (nth 2 sh) (nth 3 sh) (nthcdr 4 sh)))
        ('line   (apply #'svg-line svg (nth 1 sh) (nth 2 sh) (nth 3 sh) (nth 4 sh) (nthcdr 5 sh)))
        ('rect   (apply #'svg-rectangle svg (nth 1 sh) (nth 2 sh) (nth 3 sh) (nth 4 sh) (nthcdr 5 sh)))))
    (when (< open-n total)
      (dotimes (idx total)
        (unless (memq idx open-set)
          (let* ((sx (* (mod idx cols) cw))
                 (sy (* (/ idx cols) ch)))
            (svg-rectangle svg (+ sx 1) (+ sy 1) (- cw 2) (- ch 2)
                           :fill "#262C36" :stroke "#3B4252" :stroke-width 1.5 :rx 3 :ry 3)
            (svg-line svg (+ sx 8) (+ sy 8) (+ sx cw -8) (+ sy ch -8)
                      :stroke "#3B4252" :stroke-width 1.5)
            (svg-line svg (+ sx cw -8) (+ sy 8) (+ sx 8) (+ sy ch -8)
                      :stroke "#3B4252" :stroke-width 1.5)))))
    (svg-image svg :ascent 'center)))

(defun org-ladder-art--render-tty (ym-str ratio)
  "Render a compact 6x6 sector grid in non-graphical terminals."
  (let* ((total 36)
         (open-n (min total (round (* total ratio))))
         (open-set (seq-take (org-ladder-art--shuffle-indices
                              (number-sequence 0 (1- total))
                              (abs (sxhash ym-str)))
                             open-n)))
    (dotimes (r 6)
      (insert "    ")
      (dotimes (c 6)
        (insert (if (memq (+ (* r 6) c) open-set)
                    (propertize "██ " 'face '(:foreground "#A6E22E"))
                  (propertize "▒▒ " 'face '(:foreground "#444444")))))
      (insert "\n"))))

(defun org-ladder-art--render-buffer (ym-str score item &optional today-score unlock-date)
  "Render artifact ITEM for YM-STR according to SCORE and optional UNLOCK-DATE."
  (let* ((max-score 4500.0)
         (ratio (min 1.0 (/ (max 0.0 (float score)) max-score)))
         (donep (>= score 4500))
         (today-gain (and today-score (> today-score 0)
                          (* 100 (/ (float today-score) max-score))))
         (status (if donep
                     (format "RELIC RESTORED · %s%s"
                             ym-str
                             (if unlock-date (format " · %s 解锁" unlock-date) ""))
                   (format "SIGNAL LOCK · %s  %s %.1f%%%s"
                           ym-str
                           (org-ladder--progress-bar (round (* ratio 36)) 36 10)
                           (* 100 ratio)
                           (if today-gain (format " (+%.1f%% today)" today-gain) "")))))
    (insert (propertize (format "  %s\n" status)
                        'face (if donep '(:foreground "#E6DB74" :weight bold)
                                '(:foreground "#75715E"))))
    (if (and (display-graphic-p)
             (image-type-available-p 'svg)
             (require 'svg nil t))
        (progn
          (insert "  ")
          (insert-image (org-ladder-art--render-svg ym-str item ratio))
          (insert "\n"))
      (org-ladder-art--render-tty ym-str ratio))
    (if donep
        (insert (propertize (format "  【%s】 %s\n\n"
                                    (plist-get item :title)
                                    (plist-get item :lore))
                            'face '(:foreground "#E6DB74")))
      (insert (propertize "  ??? UNKNOWN RELIC — 勘探进行中，继续积累以揭开迷雾。\n\n"
                          'face '(:foreground "#75715E"))))))

(defun org-ladder-art-insert-current (&optional today-score)
  "Insert the currently-excavating artifact (by cumulative score) at point."
  (let* ((allocs   (org-ladder-art--allocations))
         (restored (cl-count-if (lambda (e) (>= (nth 1 e) 4500)) allocs))
         (entry    (org-ladder-art--active-entry)))
    (if entry
        (pcase-let ((`(,ym-str ,alloc ,item ,date) entry))
          (insert (format "  已完整发掘 %d/%d 件  " restored (length allocs))
                  (buttonize "[🏛️ 查看遗迹陈列室]"
                             (lambda (_) (org-ladder-art-gallery)))
                  "\n")
          (org-ladder-art--render-buffer ym-str alloc item today-score date))
      (insert (format "  no artifact files in %s\n\n" org-ladder-art-directory)))))

;;;###autoload
(defun org-ladder-art-gallery ()
  "Display the cumulative artifact gallery."
  (interactive)
  (let ((allocs (org-ladder-art--allocations))
        (total  (apply #'+ (mapcar #'cdr (org-ladder-calculate-scores))))
        (buf    (get-buffer-create "*Org Ladder Gallery*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "🏛️ Org Ladder 遗迹陈列室\n"
                            'face '(:weight bold :height 1.3 :foreground "#66D9EF")))
        (insert (format "按累计总分依次发掘（每件 4500，当前累计 %d），永不锁死残缺。\n\n" total))
        (dolist (entry allocs)
          (pcase-let ((`(,ym-str ,alloc ,item ,date) entry))
            (when (or (> alloc 0) (eq entry (car allocs)))
              (org-ladder-art--render-buffer ym-str alloc item nil date))))
        (goto-char (point-min)))
      (special-mode)
      (pop-to-buffer buf))))

(provide 'org-ladder-art)
;;; org-ladder-art.el ends here
