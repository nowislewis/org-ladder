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

;; ---------------------------------------------------------
;; 2. 开发者工具 (打包并直接生成文件)
;; ---------------------------------------------------------

(defun org-ladder-art-pack-to-file (ym-str title lore art)
  "【维护工具】将图鉴加密并保存到对应月份的文件中。
例如: (org-ladder-art-pack-to-file \"2026-05\" \"标题\" \"故事\" \"字符画\")"
  (let* ((plist-data (list :version 1 :title title :lore lore :art art))
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
  '((t :foreground "#444444" :inherit fixed-pitch))
  "未发掘区域 (暗灰)")

(defface org-ladder-art-reveal-face
  '((t :foreground "#A6E22E" :weight bold :inherit fixed-pitch))
  "已发掘区域 (亮绿)")

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
                          (propertize (string c) 'face 'org-ladder-art-reveal-face)
                        (propertize "▓" 'face 'org-ladder-art-fog-face)))
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

;;;###autoload
(defun org-ladder-art-today ()
  "查看当前赛季 (本月) 的遗迹发掘情况"
  (interactive)
  (let* ((cur-ym (org-ladder--month-key (current-time)))
         (ym-str (format "%04d-%02d" (car cur-ym) (cadr cur-ym)))
         (score (org-ladder-current-score))
         (encoded (org-ladder-art--get-encoded ym-str)))
    (with-current-buffer (get-buffer-create "*Org Ladder Artifact*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if encoded
            (let* ((daily-tbl (org-ladder--collect-daily))
                   (today-score (gethash (org-ladder-time-today) daily-tbl 0)))
              (org-ladder-art--render-buffer
               ym-str score (org-ladder-art--decrypt encoded) "当前赛季正在发掘" today-score))
          (insert (propertize (format "【 司令部警告 】\n\n坐标 %s 区域未探测到任何遗迹信号。\n请将有效的加密图鉴档案放入目录: \n%s\n" 
                                      ym-str org-ladder-art-directory) 
                              'face '(:foreground "#F92672"))))
        (goto-char (point-min)))
      (read-only-mode 1)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun org-ladder-art-gallery ()
  "查阅过往所有月份的历史遗迹馆"
  (interactive)
  (let ((scores (org-ladder-calculate-scores))
        (buf (get-buffer-create "*Org Ladder Gallery*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "🏛️ Org Ladder 神经漫游遗迹陈列室 🏛️\n" 'face '(:weight bold :height 1.5 :foreground "#66D9EF")))
        (insert "分数未达到 4500 (传说) 的月份，遗迹将永远定格在残缺状态。\n\n")
        (unless scores (insert "暂无历史记录。\n"))
        (dolist (record scores)
          (let* ((ym-str (format "%04d-%02d" (nth 0 (car record)) (nth 1 (car record))))
                 (score (cdr record))
                 (encoded (org-ladder-art--get-encoded ym-str)))
            (when encoded
              (org-ladder-art--render-buffer ym-str score (org-ladder-art--decrypt encoded) "历史档案"))))
        (goto-char (point-min)))
      (read-only-mode 1)
      (pop-to-buffer buf))))

(provide 'org-ladder-art)
;;; org-ladder-art.el ends here
