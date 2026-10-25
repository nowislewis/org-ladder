;;; org-ladder-art.el --- Quantized vine climb and vector relic gallery -*- lexical-binding: t; -*-

(require 'org-ladder)
(require 'cl-lib)
(require 'seq)

(declare-function svg-create "svg")
(declare-function svg-rectangle "svg")
(declare-function svg-polygon "svg")
(declare-function svg-circle "svg")
(declare-function svg-line "svg")
(declare-function svg-image "svg")
(declare-function svg-print "svg")
(declare-function org-ladder-sm--card-bonus-score "org-ladder-sm")

(defcustom org-ladder-art-directory
  (expand-file-name "arts" (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory storing plain Lisp vector relic `.art' files."
  :type 'directory
  :group 'org-ladder)

(defconst org-ladder-art-points-per-step 24
  "Ladder points per floor (`score // 24' = 6 card reviews).")

(defconst org-ladder-art-steps-per-floor 5
  "Floors per special fog-clearing floor (every 5 floors = 120 pts = +1 fog tile).")

(defconst org-ladder-art-floors-per-relic 36
  "Fog tiles per vector relic (36 special floors = 180 floors = 4,320 pts).")

(defconst org-ladder-art-relic-step
  (* org-ladder-art-points-per-step
     org-ladder-art-steps-per-floor
     org-ladder-art-floors-per-relic)
  "Points required to fully unveil one 36-tile vector relic (4,320 points).")

(defconst org-ladder-art-vine-window '(2 . 5)
  "Floors drawn (BELOW . ABOVE) the current floor in the vine ladder.")

(defun org-ladder-art--vine (floor total-relics)
  "Return vine ladder rows around FLOOR, highest floor first.
Every 5th floor is a special floor that unveils 1 fog tile; every 180th
floor completes a relic."
  (let* ((span org-ladder-art-steps-per-floor)
         (tiles org-ladder-art-floors-per-relic)
         (per (* span tiles))
         rows)
    (cl-loop for f from (max 1 (- floor (car org-ladder-art-vine-window)))
             to (+ floor (cdr org-ladder-art-vine-window))
             for specialp = (zerop (mod f span))
             for relic = (1+ (/ (1- f) per))
             for tile = (and specialp (1+ (mod (1- (/ f span)) tiles)))
             do (push (list :floor f
                            :status (cond ((< f floor) "cleared")
                                          ((= f floor) "current")
                                          (t "locked"))
                            :special (and specialp t)
                            :relic (and (<= relic total-relics) relic)
                            :tile tile
                            :relic-done (and specialp (= tile tiles)))
                      rows))
    rows))

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

(defun org-ladder-art--total-score ()
  "Return cumulative total score including daily sources and card stock bonus."
  (+ (apply #'+ (mapcar #'cdr (org-ladder-calculate-scores)))
     (if (fboundp 'org-ladder-sm--card-bonus-score)
         (org-ladder-sm--card-bonus-score)
       0)))

(defun org-ladder-art--unlock-dates (step)
  "Return vector of unlock date strings (\"YYYY-MM-DD\") per STEP-point threshold."
  (let ((days (sort (org-ladder-hash-to-alist (org-ladder--collect-daily))
                    (lambda (a b) (value< (car a) (car b)))))
        (acc (if (fboundp 'org-ladder-sm--card-bonus-score)
                 (org-ladder-sm--card-bonus-score)
               0))
        (next step)
        dates)
    (dolist (d days)
      (setq acc (+ acc (cdr d)))
      (while (>= acc next)
        (pcase-let ((`(,y ,m ,day) (car d)))
          (push (format "%04d-%02d-%02d" y m day) dates))
        (setq next (+ next step))))
    (vconcat (nreverse dates))))

(defun org-ladder-art--allocations (&optional total-score)
  "Allocate cumulative TOTAL-SCORE sequentially across available `.art' files.
Each artifact requires `org-ladder-art-relic-step' points (4,320).
Return list of (KEY ALLOC ITEM UNLOCK-DATE INDEX)."
  (let ((remaining (or total-score (org-ladder-art--total-score)))
        (step org-ladder-art-relic-step)
        (dates (org-ladder-art--unlock-dates org-ladder-art-relic-step))
        (idx 0)
        result)
    (dolist (ym-str (org-ladder-art--available-keys))
      (when-let* ((item (org-ladder-art--read ym-str)))
        (let ((alloc (min step (max 0 remaining)))
              (date  (and (< idx (length dates)) (aref dates idx))))
          (setq remaining (max 0 (- remaining alloc))
                idx (1+ idx))
          (push (list ym-str alloc item date idx) result))))
    (nreverse result)))

(defun org-ladder-art--active-entry (&optional allocs)
  "Return the (YM-STR ALLOC ITEM DATE INDEX) currently being excavated."
  (let ((allocs (or allocs (org-ladder-art--allocations))))
    (or (cl-find-if (lambda (e) (< (nth 1 e) org-ladder-art-relic-step)) allocs)
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

(defun org-ladder-art--build-svg (seed item open-n)
  "Build an SVG object for ITEM with OPEN-N (0..36) unveiled fog sectors."
  (require 'svg)
  (let* ((w 264) (h 240)
         (cols 6) (rows 6)
         (cw (/ w cols)) (ch (/ h rows))
         (total (* cols rows))
         (open-n (max 0 (min total open-n)))
         (order (org-ladder-art--shuffle-indices
                 (number-sequence 0 (1- total))
                 (* (max 1 seed) 97)))
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
    svg))

(defun org-ladder-art--render-svg (seed item open-n)
  "Render vector shapes in ITEM as an Emacs image with OPEN-N fog tiles unveiled."
  (svg-image (org-ladder-art--build-svg seed item open-n) :ascent 'center))

(defun org-ladder-art--svg-string (seed item open-n)
  "Return the SVG XML string for ITEM with OPEN-N fog tiles unveiled."
  (require 'svg)
  (with-temp-buffer
    (svg-print (org-ladder-art--build-svg seed item open-n))
    (buffer-string)))

(defun org-ladder-art--render-tty (seed open-n)
  "Render a compact 6x6 sector grid in non-graphical terminals."
  (let* ((total 36)
         (open-set (seq-take (org-ladder-art--shuffle-indices
                              (number-sequence 0 (1- total))
                              (* (max 1 seed) 97))
                             (max 0 (min total open-n)))))
    (dotimes (r 6)
      (insert "    ")
      (dotimes (c 6)
        (insert (if (memq (+ (* r 6) c) open-set)
                    (propertize "██ " 'face '(:foreground "#A6E22E"))
                  (propertize "▒▒ " 'face '(:foreground "#444444")))))
      (insert "\n"))))

(defun org-ladder-art-climb-state (&optional total-score with-svg)
  "Compute fine-grained vine climb (`score // 24' = floors) and relic state.
1 floor = 24 pts (6 cards, 4 pts per pip); every 5th floor = special floor
\(+1 fog tile = 120 pts); 1 relic = 180 floors (36 special floors = 4,320 pts)."
  (let* ((score (max 0 (or total-score (org-ladder-art--total-score))))
         (today-score (if total-score 0
                        (gethash (org-ladder-time-today)
                                 (org-ladder--collect-daily) 0)))
         (cleared-floors (/ score org-ladder-art-points-per-step))
         (today-floors (- cleared-floors
                          (/ (max 0 (- score today-score))
                             org-ladder-art-points-per-step)))
         (floor (1+ cleared-floors))
         (step-rem (mod score org-ladder-art-points-per-step))
         (step-in-floor (/ step-rem 4))
         (steps-per-floor 6)
         (steps-left (- steps-per-floor step-in-floor))
         (floors-per-relic (* org-ladder-art-steps-per-floor
                              org-ladder-art-floors-per-relic))
         (special-floor (* org-ladder-art-steps-per-floor
                           (1+ (/ cleared-floors org-ladder-art-steps-per-floor))))
         (allocs (org-ladder-art--allocations score))
         (total-relics (length allocs))
         (unlocked-count (cl-count-if
                          (lambda (e) (>= (nth 1 e) org-ladder-art-relic-step))
                          allocs))
         (entry (org-ladder-art--active-entry allocs)))
    (pcase-let* ((`(,key ,alloc ,item ,date ,idx) (or entry '(nil 0 nil nil 1)))
                 (donep (>= alloc org-ladder-art-relic-step))
                 (open-tiles (if donep 36
                               (/ alloc (* org-ladder-art-points-per-step
                                           org-ladder-art-steps-per-floor)))))
      (list :score score
            :total-steps cleared-floors
            :cleared-floors cleared-floors
            :today-floors today-floors
            :floor floor
            :step-in-floor step-in-floor
            :step-rem step-rem
            :points-per-step org-ladder-art-points-per-step
            :steps-per-floor steps-per-floor
            :steps-left steps-left
            :special-floor special-floor
            :open-tiles open-tiles
            :unlocked-count unlocked-count
            :total-relics total-relics
            :relic-key key
            :relic-floor (* floors-per-relic
                            (1+ (/ cleared-floors floors-per-relic)))
            :vine (vconcat (org-ladder-art--vine floor total-relics))
            :relic-unlocked-p (and donep t)
            :unlock-date date
            ;; Never expose title or lore before all 36 tiles are unveiled!
            :title (and donep item (plist-get item :title))
            :lore (and donep item (plist-get item :lore))
            :svg (and with-svg item
                      (org-ladder-art--svg-string (or idx 1) item open-tiles))))))

(defun org-ladder-art--render-buffer (ym-str score item &optional _today-score unlock-date idx)
  "Render artifact ITEM for YM-STR according to SCORE and optional UNLOCK-DATE."
  (let* ((donep  (>= score org-ladder-art-relic-step))
         (open-n (if donep 36
                   (/ score (* org-ladder-art-points-per-step
                               org-ladder-art-steps-per-floor))))
         (status (if donep
                     (format "%s · 已解锁%s"
                             ym-str (if unlock-date (format " (%s)" unlock-date) ""))
                   (format "%s · 破雾 %d/36 格 (%.0f%%)"
                           ym-str open-n (* 100 (/ (float open-n) 36.0))))))
    (insert (propertize (format "  %s\n" status)
                        'face (if donep '(:foreground "#FFB86C" :weight bold)
                                'shadow)))
    (if (and (display-graphic-p)
             (image-type-available-p 'svg)
             (require 'svg nil t))
        (progn
          (insert "  ")
          (insert-image (org-ladder-art--render-svg (or idx 1) item open-n))
          (insert "\n"))
      (org-ladder-art--render-tty (or idx 1) open-n))
    (if donep
        (insert (propertize (format "  【%s】 %s\n\n"
                                    (plist-get item :title)
                                    (plist-get item :lore))
                            'face '(:foreground "#FFB86C")))
      (insert "\n"))))

(defun org-ladder-art--vine-note (row)
  "Return the optional right-hand note for vine ROW."
  (cond ((plist-get row :relic-done)
         (format "   遗物 #%d" (plist-get row :relic)))
        ((plist-get row :special)
         (format "   破雾 %d/36" (plist-get row :tile)))
        (t "")))

(defun org-ladder-art-step-bar (step-in-floor &optional steps-per-floor)
  "Render a 6-pip floor bar (`1 card = 1 pip') as spaced `■'/`□' pips."
  (let* ((total  (max 1 (or steps-per-floor 6)))
         (filled (max 0 (min total (or step-in-floor 0))))
         (done   (propertize "■" 'face '(:foreground "#A6E22E")))
         (todo   (propertize "□" 'face 'shadow)))
    (string-join (cl-loop for i below total
                          collect (if (< i filled) done todo))
                 " ")))

(defun org-ladder-art--insert-vine (st)
  "Insert the vine ladder of climb state ST, highest floor first.
Symbols show floor type (`·' / `🏺' / `🏆'); color highlights only active deltas."
  (let* ((floor  (plist-get st :floor))
         (step   (plist-get st :step-in-floor))
         (per    (plist-get st :steps-per-floor))
         (todayf (or (plist-get st :today-floors) 0))
         (vine   (append (plist-get st :vine) nil))
         (top    (or (plist-get (car vine) :floor) floor))
         (rf     (plist-get st :relic-floor))
         (morep  (< (plist-get st :unlocked-count) (plist-get st :total-relics)))
         (left   (and rf (1+ (- rf floor))))
         (gface  '(:foreground "#FFB86C" :weight bold))
         (sface  '(:foreground "#A6E22E" :weight bold)))
    (when (and morep rf (> rf top))
      (insert "  🏆"
              (propertize " " 'display '(space :align-to 6))
              (propertize (format "%4dF   遗物 #%d · "
                                  rf (1+ (plist-get st :unlocked-count)))
                          'face 'bold)
              (propertize (format "还差 %d 层\n" left) 'face gface))
      (when (> (1- rf) top)
        (insert "  "
                (propertize "⋮" 'face 'shadow)
                (propertize " " 'display '(space :align-to 6))
                (propertize "   ...\n" 'face 'shadow))))
    (dolist (row vine)
      (let* ((status (plist-get row :status))
             (specialp (plist-get row :special))
             (relicp (plist-get row :relic-done))
             (icon (cond ((equal status "current") "🧗")
                         (relicp "🏆")
                         (specialp "🏺")
                         (t "·")))
             (face (pcase status
                     ("cleared" 'shadow)
                     ("current" 'bold)
                     (_ (if relicp 'bold 'default)))))
        (insert "  "
                (propertize icon 'face face)
                (propertize " " 'display '(space :align-to 6))
                (propertize (format "%4dF%s"
                                    (plist-get row :floor)
                                    (org-ladder-art--vine-note row))
                            'face face)
                (cond ((equal status "current")
                       (concat "   " (org-ladder-art-step-bar step per)
                               (if (> todayf 0)
                                   (concat (propertize "  今日 " 'face 'shadow)
                                           (propertize (format "+%d" todayf) 'face sface)
                                           (propertize " 层" 'face 'shadow))
                                 "")))
                      ((and relicp morep (> left 0))
                       (concat (propertize " · " 'face 'bold)
                               (propertize (format "还差 %d 层" left) 'face gface)))
                      (t ""))
                "\n")))))

(defun org-ladder-art-insert-current (&optional today-score)
  "Insert the fine-grained vine ladder and active artifact at point."
  (let* ((st     (org-ladder-art-climb-state))
         (floor  (plist-get st :floor))
         (allocs (org-ladder-art--allocations (plist-get st :score)))
         (entry  (org-ladder-art--active-entry allocs)))
    (insert (propertize (format "🧗 通天藤蔓 · 第 %d 层\n" floor)
                        'face '(:weight bold :height 1.2)))
    (org-ladder-art--insert-vine st)
    ;; Accumulation: what has been earned so far, linking to the full gallery.
    (insert (propertize (format "  🏅 已登 %d 层 · 已收集 %d/%d 件  "
                                (plist-get st :cleared-floors)
                                (plist-get st :unlocked-count)
                                (length allocs))
                        'face 'shadow)
            (buttonize "[🏛️ 遗迹陈列室]" (lambda (_) (org-ladder-art-gallery)))
            "\n\n")
    (if entry
        (pcase-let ((`(,ym-str ,alloc ,item ,date ,idx) entry))
          (org-ladder-art--render-buffer ym-str alloc item today-score date idx))
      (insert (format "  no artifact files in %s\n\n" org-ladder-art-directory)))))

;;;###autoload
(defun org-ladder-art-gallery ()
  "Display the cumulative artifact gallery."
  (interactive)
  (let* ((total  (org-ladder-art--total-score))
         (allocs (org-ladder-art--allocations total))
         (buf    (get-buffer-create "*Org Ladder Gallery*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "🏛️ Org Ladder 遗迹陈列室\n"
                            'face '(:weight bold :height 1.3 :foreground "#66D9EF")))
        (insert (format "按累计层数依次发掘（每件 180 层 / 36 特殊层 / %d 分，当前累计 %d 分），永不锁死残缺。\n\n"
                        org-ladder-art-relic-step total))
        (dolist (entry allocs)
          (pcase-let ((`(,ym-str ,alloc ,item ,date ,idx) entry))
            (when (or (> alloc 0) (eq entry (car allocs)))
              (org-ladder-art--render-buffer ym-str alloc item nil date idx))))
        (goto-char (point-min)))
      (special-mode)
      (pop-to-buffer buf))))

(defun org-ladder-art--daily-timeline ()
  "Return chronologically sorted list of (ISO-DATE BEFORE-SCORE AFTER-SCORE)."
  (let ((days (sort (org-ladder-hash-to-alist (org-ladder--collect-daily))
                    (lambda (a b) (value< (car a) (car b)))))
        (acc (if (fboundp 'org-ladder-sm--card-bonus-score)
                 (org-ladder-sm--card-bonus-score)
               0))
        out)
    (dolist (d days (nreverse out))
      (pcase-let ((`(,y ,m ,day) (car d))
                  (before acc))
        (setq acc (+ acc (cdr d)))
        (push (list (format "%04d-%02d-%02d" y m day) before acc) out)))))

(defun org-ladder-art--period-climb (timeline pred allocs)
  "Summarize floor, fog, and relic gains across TIMELINE entries matching PRED."
  (let ((matches (cl-remove-if-not (lambda (e) (funcall pred (car e))) timeline)))
    (when matches
      (let* ((s0 (nth 1 (car matches)))
             (s1 (nth 2 (car (last matches))))
             (f0 (/ s0 org-ladder-art-points-per-step))
             (f1 (/ s1 org-ladder-art-points-per-step))
             (df (- f1 f0))
             (sp (* org-ladder-art-points-per-step org-ladder-art-steps-per-floor))
             (dt (- (/ s1 sp) (/ s0 sp)))
             (r0 (/ s0 org-ladder-art-relic-step))
             (r1 (/ s1 org-ladder-art-relic-step))
             (titles (cl-loop for i from r0 below (min r1 (length allocs))
                              for item = (nth 2 (nth i allocs))
                              when item collect (format "【%s】" (plist-get item :title)))))
        (when (> df 0)
          (list :floors df :from (1+ f0) :to (1+ f1)
                :tiles dt :relics titles))))))

(defun org-ladder-art-chronicle (&optional max-months)
  "Return climb chronicle rows for Today, Last 7 Days, and recent months."
  (let* ((timeline (org-ladder-art--daily-timeline))
         (allocs   (org-ladder-art--allocations))
         (now      (current-time))
         (today    (format-time-string "%F" now))
         (week-ago (format-time-string "%F" (time-subtract now (days-to-time 6))))
         (yms      (seq-take
                    (sort (seq-uniq (mapcar (lambda (e) (substring (car e) 0 7))
                                            timeline))
                          #'string>)
                    (or max-months 12)))
         rows)
    (dolist (spec (append (list (cons "今日" (lambda (d) (equal d today)))
                                (cons "近7天" (lambda (d) (not (string< d week-ago)))))
                          (mapcar (lambda (ym)
                                    (cons ym (lambda (d) (string-prefix-p ym d))))
                                  yms)))
      (when-let* ((c (org-ladder-art--period-climb timeline (cdr spec) allocs)))
        (push (append (list :period (car spec)) c) rows)))
    (nreverse rows)))

(defun org-ladder-art-insert-chronicle (&optional max-months)
  "Insert the floor and relic climb chronicle at point."
  (when-let* ((rows (org-ladder-art-chronicle max-months)))
    (insert (propertize "📅 攀登编年史\n" 'face '(:weight bold :height 1.2)))
    (dolist (r rows)
      (let ((relics (plist-get r :relics))
            (tiles  (plist-get r :tiles)))
        (insert (format "  %-8s " (plist-get r :period))
                (propertize (format "+%3d 层" (plist-get r :floors))
                            'face '(:foreground "#A6E22E" :weight bold))
                (propertize (format "  (%dF → %dF)" (plist-get r :from) (plist-get r :to))
                            'face 'shadow)
                (cond (relics
                       (propertize (format "  🏆 收集%s" (string-join relics " "))
                                   'face '(:foreground "#FFB86C" :weight bold)))
                      ((> tiles 0)
                       (propertize (format "  🏺 破雾 %d" tiles)
                                   'face 'shadow))
                      (t ""))
                "\n")))
    (insert "\n")))

(provide 'org-ladder-art)
;;; org-ladder-art.el ends here
