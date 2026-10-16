;;; test-org-ladder-stock.el --- Tests for org-ladder-stock and org-ladder-sm  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-ladder)
(require 'org-ladder-stock)
(require 'org-ladder-sm)

(ert-deftest org-ladder-stock-forest-and-boss-separation ()
  "Forest stages count only Cloze cards; high-review Topics become Bosses."
  (let ((temp-file (make-temp-file "org-ladder-stock-test-" nil ".org")))
    (with-temp-file temp-file
      (insert "* [T] Undefeated Boss\n"
              "SCHEDULED: <2026-12-01 Tue>\n"
              ":PROPERTIES:\n:SRS_TYPE: topic\n:SRS_LAST: 2026-10-01T00:00:00Z\n:END:\n"
              ":LOGBOOK:\n"
              "- topic  a=1.5  [2026-09-01 Tue 10:00]\n"
              "- topic  a=1.5  [2026-09-05 Sat 10:00]\n"
              "- topic  a=1.5  [2026-09-10 Thu 10:00]\n"
              "- topic  a=1.5  [2026-09-15 Tue 10:00]\n"
              "- topic  a=1.5  [2026-09-20 Sun 10:00]\n"
              ":END:\n"
              "* [T] Wounded Boss\n"
              "SCHEDULED: <2026-11-01 Sun>\n"
              ":PROPERTIES:\n:SRS_TYPE: topic\n:SRS_LAST: 2026-10-01T00:00:00Z\n:END:\n"
              ":LOGBOOK:\n"
              "- topic  a=1.5  [2026-09-01 Tue 10:00]\n"
              "- topic  a=1.5  [2026-09-05 Sat 10:00]\n"
              "- topic  a=1.5  [2026-09-10 Thu 10:00]\n"
              "- topic  a=1.5  [2026-09-15 Tue 10:00]\n"
              "- topic  a=1.5  [2026-09-20 Sun 10:00]\n"
              "- topic  a=1.5  [2026-09-25 Fri 10:00]\n"
              ":END:\n"
              "** [C] Extracted Cloze\n"
              "SCHEDULED: <2026-11-15 Sun>\n"
              ":PROPERTIES:\n:SRS_TYPE: cloze\n:SRS_LAST: 2026-10-01T00:00:00Z\n:END:\n"
              "* [T] Slain Boss :dismissed:\n"
              ":PROPERTIES:\n:SRS_TYPE: topic\n:END:\n"
              ":LOGBOOK:\n"
              "- topic  a=1.5  [2026-09-01 Tue 10:00]\n"
              "- topic  a=1.5  [2026-09-05 Sat 10:00]\n"
              "- topic  a=1.5  [2026-09-10 Thu 10:00]\n"
              "- topic  a=1.5  [2026-09-15 Tue 10:00]\n"
              "- topic  a=1.5  [2026-09-20 Sun 10:00]\n"
              "- dismissed  [2026-10-01 Thu 10:00]\n"
              ":END:\n"
              "* [T] Young Topic\n"
              "SCHEDULED: <2026-10-05 Mon>\n"
              ":PROPERTIES:\n:SRS_TYPE: topic\n:SRS_LAST: 2026-10-01T00:00:00Z\n:END:\n"))
    (unwind-protect
        (let* ((cards (org-ladder-stock--scan-file temp-file))
               (sum   (org-ladder-stock-summary cards)))
          ;; Only the 1 Cloze (45 days -> evergreen) enters the Forest stages:
          (should (= (plist-get sum :scheduled) 1))
          (should (= (cdr (assq 'evergreen (plist-get sum :stages))) 1))
          (should (= (cdr (assq 'seedling (plist-get sum :stages))) 0))
          ;; Boss pipeline stats:
          (should (= (plist-get sum :boss-active) 2))
          (should (= (plist-get sum :boss-wounded) 1))
          (should (= (plist-get sum :boss-slain) 1))
          (should (= (plist-get sum :topic-pool) 1)))
      (delete-file temp-file))))

(ert-deftest org-ladder-sm-boss-banner-kill-and-bonfire ()
  "Verify Boss banner, Boss kill feedback, Cloze stage promotion, and bonfire."
  (org-ladder-sm--reset-session)
  (setq org-ladder-sm--stock-snapshot
        (list :stages (list (cons 'seedling 8) (cons 'plant 16)
                            (cons 'evergreen 52) (cons 'ancient 37)
                            (cons 'elder 6))
              :scheduled 119 :unscheduled 31 :dismissed 126
              :boss-slain 67 :boss-active 48 :boss-wounded 0
              :topic-pool 202 :total 500))
  (let ((boss-unextracted '(:type topic :title "T1" :reviews 11
                            :child-topics 0 :child-clozes 0))
        (boss-wounded     '(:type topic :title "T2" :reviews 8
                            :child-topics 2 :child-clozes 1))
        (cloze-card       '(:type cloze :title "C1" :reviews 3
                            :child-topics 0 :child-clozes 0)))
    (let ((cloze-evergreen '(:type cloze :title "C2" :reviews 3 :interval-days 42
                             :child-topics 0 :child-clozes 0)))
      (should (equal (org-ladder-sm-banner boss-unextracted)
                     "⚔️ Boss · 11次"))
      (should (equal (org-ladder-sm-banner boss-wounded)
                     "⚔️ Boss · 8次 · 2T/1C"))
      (should (equal (org-ladder-sm-banner cloze-card)
                     "🌱 幼苗 · 新卡"))
      (should (equal (org-ladder-sm-banner cloze-evergreen)
                     "🌲 常青 · 42d")))
    (let ((ev-promote (list :action 'cloze :card cloze-card
                            :rating :good :old-days 25 :new-days 42 :session t))
          (ev-same    (list :action 'cloze :card cloze-card
                            :rating :good :old-days 42 :new-days 65 :session t))
          (ev-kill    (list :action 'dismiss :card boss-wounded
                            :old-days 30 :session t)))
      (org-ladder-sm--on-review-event ev-promote)
      (org-ladder-sm--on-review-event ev-kill)
      (should (equal (org-ladder-sm-feedback ev-promote)
                     "25d → 42d  🌲 常青 +1 (共 53)"))
      (should (equal (org-ladder-sm-feedback ev-same)
                     "42d → 65d"))
      (should (equal (org-ladder-sm-feedback ev-kill)
                     "🏆 Boss 讨伐 +1 (累计 68 · 剩 47)"))
      (should (equal (org-ladder-sm-bonfire-text)
                     "🔥 篝火小结 · 本次处理 2 张（复习 1 · 击败 Boss 1） · 升阶 🌲 常青 +1 (共 53)")))))

(ert-deftest org-ladder-sm-session-isolation-and-cache-invalidation ()
  (let ((org-ladder-sm--session-stats nil)
        (org-ladder--cache '(stale))
        (org-ladder--cache-time t))
    (org-ladder-sm--reset-session)
    (org-ladder-sm--on-review-event '(:action topic))
    (org-ladder-sm--on-review-event '(:action postpone :session t))
    (should (= (plist-get org-ladder-sm--session-stats :reviewed) 0))
    (org-ladder-sm--on-card-change '(:action extract))
    (should-not org-ladder--cache)
    (should-not org-ladder--cache-time)))

(ert-deftest org-ladder-sm-review-flow-integration ()
  "Exercise review presentation, grading and stopping on disposable cards."
  (require 'org-sm)
  (let ((buffer (generate-new-buffer " *org-sm-flow*"))
        (org-sm--queue nil)
        (org-sm--current-buffer nil)
        (org-sm--current-card-marker nil)
        (org-ladder-sm--session-stats nil)
        (org-sm-prompt-function #'org-ladder-sm-prompt)
        (org-sm-feedback-function #'org-ladder-sm-feedback)
        (org-sm-finish-function #'org-ladder-sm-bonfire)
        (org-sm-review-start-hook '(org-ladder-sm--reset-session))
        (org-sm-after-review-functions '(org-ladder-sm--on-review-event))
        (org-sm-after-card-change-functions '(org-ladder-sm--on-card-change)))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buffer
            (org-mode)
            (insert "* Boss\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n:LOGBOOK:\n")
            (dotimes (_ 5) (insert "- topic a=1.5 [2026-10-01 Thu]\n"))
            (insert ":END:\nsub\n** Memory\n:PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n{{answer}}\n")
            (goto-char (point-min))
            (let ((boss (point-marker))
                  (cloze (save-excursion (outline-next-heading) (point-marker))))
              ;; Batch Emacs has no redisplay geometry; all other presentation
              ;; and queue operations use their real implementations.
              (cl-letf (((symbol-function 'recenter) #'ignore))
                (org-sm--review-markers (list boss cloze))
                (should (org-sm--current-review-card-p))
                (should-not (org-ladder-sm--hud-hide-p nil))
                (should-not (local-variable-p 'header-line-format))
                (should (equal org-ladder-notify-last-text
                               "⚔️ Boss · 5次 · 1C   [1/2]"))
                ;; Extracting a child card while reviewing updates the Boss HUD immediately:
                (org-sm-extract 'topic "sub" 0 3)
                (should (equal org-ladder-notify-last-text
                               "⚔️ Boss · 5次 · 1T/1C   [1/2]\n✓ 🗡️ 产出子卡 +1"))
                (org-sm-item-dismiss)
                (should (eq org-sm--cloze-state 'hidden))
                (should (equal org-ladder-notify-last-text
                               "🌱 幼苗 · 新卡 · hidden   [2/2]\n✓ 🏆 Boss 讨伐 +1 (累计 1 · 剩 0)"))
                (should (buffer-narrowed-p))
                (org-sm-review-confirm)
                (should (eq org-sm--cloze-state 'revealed))
                (should (equal org-ladder-notify-last-text
                               "🌱 幼苗 · 新卡 · revealed   [2/2]"))
                (cl-letf (((symbol-function 'read-multiple-choice)
                           (lambda (&rest _) '(?r "good"))))
                  (org-sm-review-confirm))
                (should-not (buffer-narrowed-p))
                (should-not org-sm--current-card-marker)
                (should-not org-sm--cloze-state)
                (should (= (plist-get org-ladder-sm--session-stats :boss-slain) 1))
                (should (= (plist-get org-ladder-sm--session-stats :reviewed) 1))
                (goto-char boss)
                (org-sm-item-undismiss)
                (org-sm--review-markers (list (copy-marker boss)))
                (org-sm-review-abort)
                (should-not org-sm--queue)
                (should-not (buffer-narrowed-p)))
              (set-marker boss nil)
              (set-marker cloze nil))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest org-ladder-stock-resolve-files-contract ()
  "Resolve function symbols, lambdas, and lists; reject unbound symbols."
  (let ((org-sm-directory nil)
        (org-ladder-stock-files #'org-sm-files))
    (should (null (org-ladder-stock-cards))))
  (let ((org-ladder-stock-files (lambda () '("a.org" "b.org"))))
    (should (equal (org-ladder-stock--files) '("a.org" "b.org"))))
  (let ((org-ladder-stock-files 'org-ladder-nonexistent-files-fn))
    (should-error (org-ladder-stock--files) :type 'user-error)))

(ert-deftest org-sm-tree-filter-shorthands ()
  "Verify @boss, @cloze, @topic filters in `org-sm--tree-filter'."
  (with-temp-buffer
    (org-mode)
    (insert "* Boss\n:PROPERTIES:\n:SRS_TYPE: topic\n:END:\n:LOGBOOK:\n")
    (dotimes (_ 5) (insert "- topic a=1.5 [2026-10-01 Thu]\n"))
    (insert ":END:\n* Cloze\n:PROPERTIES:\n:SRS_TYPE: cloze\n:END:\n")
    (goto-char (point-min))
    (should (funcall (org-sm--tree-filter nil "@boss")))
    (should (funcall (org-sm--tree-filter nil "@topic")))
    (should-not (funcall (org-sm--tree-filter nil "@cloze")))
    (outline-next-heading)
    (should (funcall (org-sm--tree-filter nil "@cloze")))
    (should-not (funcall (org-sm--tree-filter nil "@boss")))))

(provide 'test-org-ladder-stock)
;;; test-org-ladder-stock.el ends here
