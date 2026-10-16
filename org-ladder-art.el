;;; org-ladder-art.el --- 月度加密遗迹图鉴系统 -*- lexical-binding: t -*-

(require 'org-ladder)
(require 'cl-lib)

;; ---------------------------------------------------------
;; 1. 目录配置 (数据与代码隔离)
;; ---------------------------------------------------------

(defcustom org-ladder-art-directory
  (expand-file-name "org-ladder-arts" user-emacs-directory)
  "存放每月加密图鉴文件的目录。文件命名格式应为 YYYY-MM.art"
  :type 'directory
  :group 'org-ladder)

(defun org-ladder-art--get-encoded (ym-str)
  "尝试从配置目录读取对应月份的 .art 加密文件"
  (let ((file (expand-file-name (format "%s.art" ym-str) org-ladder-art-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (string-trim (buffer-string))))))

(defun org-ladder-art--available-keys ()
  "Return chronologically sorted list of YM-STR keys for all .art files."
  (when (file-directory-p org-ladder-art-directory)
    (sort (mapcar #'file-name-base
                  (directory-files org-ladder-art-directory nil "\\.art\\'"))
          #'string<)))

(defun org-ladder-art--allocations ()
  "Allocate cumulative total score sequentially across available .art files.
Each artifact requires 4500 points; points spill into the next artifact once
the previous one reaches 4500, so no artifact is ever stranded incomplete.
Return list of (YM-STR ALLOCATED-SCORE ENCODED)."
  (let ((remaining (apply #'+ (mapcar #'cdr (org-ladder-calculate-scores))))
        (step 4500)
        result)
    (dolist (ym-str (org-ladder-art--available-keys))
      (when-let* ((encoded (org-ladder-art--get-encoded ym-str)))
        (let ((alloc (min step (max 0 remaining))))
          (setq remaining (max 0 (- remaining alloc)))
          (push (list ym-str alloc encoded) result))))
    (nreverse result)))

(defun org-ladder-art--active-entry ()
  "Return the (YM-STR ALLOC ENCODED) currently being excavated, or the last one."
  (let ((allocs (org-ladder-art--allocations)))
    (or (cl-find-if (lambda (e) (< (nth 1 e) 4500)) allocs)
        (car (last allocs)))))

;; ---------------------------------------------------------
;; 2. 开发者工具 (打包并直接生成文件)
;; ---------------------------------------------------------

(defun org-ladder-art-pack-to-file (ym-str title lore art &optional palette)
  "【维护工具】将图鉴加密并保存到对应月份的文件中。
PALETTE 可选 plist (:color1 HEX :color2 HEX :color3 HEX :accent HEX)。"
  (let* ((plist-data (append (list :version 2 :title title :lore lore :art art)
                             palette))
         (str-data (prin1-to-string plist-data))
         (b64 (base64-encode-string (encode-coding-string str-data 'utf-8) t))
         (dir org-ladder-art-directory)
         (file (expand-file-name (format "%s.art" ym-str) dir)))
    (unless (file-exists-p dir) (make-directory dir t))
    (with-temp-file file
      (insert b64))
    (message "✅ 成功生成加密遗迹档案: %s" file)))

(defun org-ladder-art--decrypt (b64-string)
  "解密 Base64 字符串为 plist"
  (car (read-from-string (decode-coding-string (base64-decode-string b64-string) 'utf-8))))

;; ---------------------------------------------------------
;; 3. 核心渲染与伪随机算法
;; ---------------------------------------------------------

(defface org-ladder-art-fog-face
  '((t :foreground "#2E3440" :inherit fixed-pitch))
  "未发掘区域 (暗雾)"
  :group 'org-ladder)

(defface org-ladder-art-reveal-face
  '((t :foreground "#A6E22E" :weight bold :inherit fixed-pitch))
  "已发掘区域 (默认亮绿)"
  :group 'org-ladder)

(defun org-ladder-art--pixel-face (ch item)
  "Return the face for revealed pixel character CH in ITEM."
  (let ((c1 (or (plist-get item :color1) "#A6E22E"))
        (c2 (or (plist-get item :color2) "#66D9EF"))
        (c3 (or (plist-get item :color3) "#75715E"))
        (ac (or (plist-get item :accent) "#E6DB74")))
    (pcase ch
      ((or ?█ ?▀ ?▄) `(:foreground ,c1 :weight bold :inherit fixed-pitch))
      (?▓            `(:foreground ,c2 :inherit fixed-pitch))
      ((or ?▒ ?░)    `(:foreground ,c3 :inherit fixed-pitch))
      ((or ?◆ ?★ ?● ?✦ ?▲ ?▼) `(:foreground ,ac :weight bold :inherit fixed-pitch))
      (_             `(:foreground ,c1 :weight bold :inherit fixed-pitch)))))

(defun org-ladder-art--pad (art)
  "将 ASCII 补齐为完美矩形防止轮廓剧透"
  (let* ((lines (split-string art "\n"))
         (max-w (apply #'max (mapcar #'length lines))))
    (mapconcat (lambda (l) (concat l (make-string (- max-w (length l)) ?\s)))
               lines "\n")))

(defun org-ladder-art--rule (left label width right)
  "Return a border line using LEFT, LABEL, WIDTH and RIGHT.
WIDTH is the inner art width.  LABEL is kept short and padded with dashes."
  (let* ((inner (+ width 2))
         (text (format "─ %s " label))
         (fill (max 1 (- inner (length text)))))
    (concat left text (make-string fill ?─) right "\n")))

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

(defun org-ladder-art--render-buffer (ym-str score item _title &optional today-score)
  "Render artifact ITEM for YM-STR according to SCORE.
When TODAY-SCORE is positive, show a small positive signal gain."
  (let* ((max-score 4500)
         (art-padded (org-ladder-art--pad (plist-get item :art)))
         (lines (split-string art-padded "\n"))
         (art-width (length (car lines)))
         (len (length art-padded))
         (total-px (- len (cl-count ?\n art-padded)))
         (unlock-count (min total-px (round (* total-px (/ (float (min score max-score)) max-score)))))
         (progress (/ (float unlock-count) total-px))
         (donep (>= score max-score))
         (top-label (if donep
                        (format "RELIC RESTORED · %s" ym-str)
                      (format "SIGNAL LOCK · %s" ym-str)))
         (today-gain (and today-score
                          (> today-score 0)
                          (* 100 (/ (float today-score) max-score))))
         (bottom-label (if donep
                           (format "archived: %s" ym-str)
                         (format "signal: %s %.1f%%%s"
                                 (org-ladder--progress-bar unlock-count total-px 10)
                                 (* 100 progress)
                                 (if today-gain
                                     (format "  +%.1f%% today" today-gain)
                                   ""))))
         ;; Frame width must fit both the art and labels.  If labels are wider
         ;; than the image, keep the art centered instead of stretching the
         ;; reveal grid.
         (frame-width (max art-width
                           (+ 2 (length top-label))
                           (+ 2 (length bottom-label))))
         (left-pad (/ (- frame-width art-width) 2))
         (right-pad (- frame-width art-width left-pad))
         (border-face (if donep
                          '(:foreground "#E6DB74")
                        '(:foreground "#75715E")))
         (indices nil))
    ;; collect drawable positions
    (dotimes (i len)
      (unless (char-equal (aref art-padded i) ?\n)
        (push i indices)))
    ;; deterministic reveal order: month + image content
    (setq indices (org-ladder-art--shuffle-indices
                   indices (abs (sxhash (concat ym-str art-padded)))))
    (let ((unlocked-table (make-hash-table :test 'eql)))
      (dolist (i (seq-take indices unlock-count))
        (puthash i t unlocked-table))
      ;; top border
      (insert (propertize
               (org-ladder-art--rule "╭" top-label frame-width "╮")
               'face border-face))
      ;; art body
      (let ((global-index 0))
        (dolist (line lines)
          (insert (propertize "│ " 'face border-face))
          (insert (make-string left-pad ?\s))
          (dotimes (x art-width)
            (let ((c (aref line x)))
              (insert (if (gethash global-index unlocked-table)
                          (propertize (string c) 'face (org-ladder-art--pixel-face c item))
                        (propertize "░" 'face 'org-ladder-art-fog-face)))
              (setq global-index (1+ global-index))))
          (insert (make-string right-pad ?\s))
          ;; account for the newline in ART-PADDED
          (setq global-index (1+ global-index))
          (insert (propertize " │\n" 'face border-face))))
      ;; bottom border
      (insert (propertize
               (org-ladder-art--rule "╰" bottom-label frame-width "╯")
               'face border-face))
      ;; title/lore outside frame to avoid long text breaking borders
      (if donep
          (insert (propertize (format "\n【%s】\n%s\n\n"
                                      (plist-get item :title)
                                      (plist-get item :lore))
                              'face '(:foreground "#E6DB74")))
        (insert (propertize "\n??? UNKNOWN RELIC — 数据残缺，需进一步发掘。\n\n"
                            'face '(:foreground "#75715E")))))))

;; ---------------------------------------------------------
;; 4. 用户侧入口命令
;; ---------------------------------------------------------

(defun org-ladder-art-insert-current (&optional today-score)
  "Insert the currently-excavating artifact (by cumulative score) at point."
  (let* ((allocs   (org-ladder-art--allocations))
         (restored (cl-count-if (lambda (e) (>= (nth 1 e) 4500)) allocs))
         (entry    (org-ladder-art--active-entry)))
    (if entry
        (pcase-let ((`(,ym-str ,alloc ,encoded) entry))
          (insert (format "  已完整发掘 %d/%d 件  " restored (length allocs))
                  (buttonize "[🏛️ 查看遗迹陈列室]"
                             (lambda (_) (org-ladder-art-gallery)))
                  "\n")
          (org-ladder-art--render-buffer
           ym-str alloc (org-ladder-art--decrypt encoded) "累计发掘" today-score))
      (insert (format "  no artifact files in %s\n\n" org-ladder-art-directory)))))

;;;###autoload
(defun org-ladder-art-today ()
  "查看当前正在按累计存量发掘的遗迹"
  (interactive)
  (let* ((daily-tbl (org-ladder--collect-daily))
         (today-score (gethash (org-ladder-time-today) daily-tbl 0)))
    (with-current-buffer (get-buffer-create "*Org Ladder Artifact*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (org-ladder-art--active-entry)
            (org-ladder-art-insert-current today-score)
          (insert (propertize (format "【 司令部警告 】\n\n未探测到任何遗迹档案，请放入目录:\n%s\n"
                                      org-ladder-art-directory)
                              'face '(:foreground "#F92672"))))
        (goto-char (point-min)))
      (read-only-mode 1)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun org-ladder-art-gallery ()
  "查阅按累计存量解锁的遗迹陈列室（永不锁死残缺）"
  (interactive)
  (let ((allocs (org-ladder-art--allocations))
        (total  (apply #'+ (mapcar #'cdr (org-ladder-calculate-scores))))
        (buf    (get-buffer-create "*Org Ladder Gallery*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "🏛️ Org Ladder 神经漫游遗迹陈列室 🏛️\n"
                            'face '(:weight bold :height 1.5 :foreground "#66D9EF")))
        (insert (format "按累计总分依次发掘（每件 4500，当前累计 %d），永不锁死残缺。\n\n" total))
        (if (null allocs)
            (insert "暂无遗迹档案。\n")
          (dolist (entry allocs)
            (pcase-let ((`(,ym-str ,alloc ,encoded) entry))
              (when (or (> alloc 0) (eq entry (car allocs)))
                (org-ladder-art--render-buffer
                 ym-str alloc (org-ladder-art--decrypt encoded) "累计档案")))))
        (goto-char (point-min)))
      (read-only-mode 1)
      (pop-to-buffer buf))))

(provide 'org-ladder-art)
;;; org-ladder-art.el ends here
