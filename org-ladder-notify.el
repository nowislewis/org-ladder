;;; org-ladder-notify.el --- Bottom-right posframe notifications  -*- lexical-binding: t; -*-

;; Author: Org Ladder System
;; Version: 3.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Minimal, reusable bottom-right notification / HUD popup built on `posframe',
;; with a `notify-send' / `notifications-notify'-style entry point:
;;
;;   (org-ladder-notify "Body text" :title "Header" :timeout 3)
;;   (org-ladder-notify nil :title "Persistent HUD")
;;   (org-ladder-notify-close)
;;
;; Falls back cleanly when `posframe' or a graphical display is unavailable.

;;; Code:

(require 'cl-lib)

(declare-function posframe-workable-p "posframe")
(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-poshandler-window-bottom-right-corner "posframe")

(defgroup org-ladder-notify nil
  "Bottom-right posframe notifications."
  :group 'org-ladder
  :prefix "org-ladder-notify-")

(defface org-ladder-notify-title-face
  '((t :weight bold :height 1.3))
  "Face for the notification title / current-item line."
  :group 'org-ladder-notify)

(defface org-ladder-notify-body-face
  '((t :inherit default))
  "Face for secondary body / previous-item lines."
  :group 'org-ladder-notify)

(defconst org-ladder-notify-buffer " *org-ladder-notify*"
  "Default posframe buffer name.")

(defvar org-ladder-notify-last-text nil
  "Last formatted notification text (without padding), for inspection/tests.")

(defun org-ladder-notify-format (&optional body title)
  "Format TITLE and BODY into a propertized notification string."
  (let ((t-str (and title (not (string-empty-p title))
                    (propertize title 'face 'org-ladder-notify-title-face)))
        (b-str (and body (not (string-empty-p body))
                    (propertize body 'face 'org-ladder-notify-body-face))))
    (string-join (delq nil (list t-str b-str)) "\n")))

;;;###autoload
(cl-defun org-ladder-notify (body &key title timeout (buffer org-ladder-notify-buffer))
  "Show a bottom-right notification with optional TITLE, BODY, and TIMEOUT.
When TIMEOUT is nil, the popup remains visible until replaced or closed via
`org-ladder-notify-close'.  Returns nil when shown in a graphical posframe,
or the plain formatted string when falling back to non-graphical mode."
  (let ((text (org-ladder-notify-format body title)))
    (setq org-ladder-notify-last-text (substring-no-properties text))
    (if (and (display-graphic-p)
             (require 'posframe nil t)
             (posframe-workable-p))
        (let ((padded (concat " " (replace-regexp-in-string "\n" " \n " text) " ")))
          (posframe-show buffer
                         :string padded
                         :poshandler #'posframe-poshandler-window-bottom-right-corner
                         :x-pixel-offset -12
                         :y-pixel-offset -12
                         :left-fringe 8
                         :right-fringe 8
                         :internal-border-width 1
                         :internal-border-color (or (face-foreground 'shadow nil t) "#555555")
                         :background-color (face-background 'default nil t)
                         :foreground-color (face-foreground 'default nil t)
                         :timeout timeout)
          nil)
      org-ladder-notify-last-text)))

;;;###autoload
(defun org-ladder-notify-close (&optional buffer)
  "Hide the bottom-right notification posframe BUFFER."
  (interactive)
  (when (and (featurep 'posframe) (fboundp 'posframe-hide))
    (posframe-hide (or buffer org-ladder-notify-buffer))))

(provide 'org-ladder-notify)
;;; org-ladder-notify.el ends here
