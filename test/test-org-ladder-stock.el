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
          (should (= (length (plist-get sum :bosses)) 2))
          (should (= (plist-get sum :boss-wounded) 1))
          (should (equal (plist-get sum :monsters)
                         '((mob 1 0) (elite 0 0) (boss 2 1) (demon 0 0))))
          (should (equal (org-ladder-stock--period-lines
                          cards (lambda (d) (string-prefix-p "2026-10" d)))
                         '("🌲 常青  +1" "🐣 新魔物  +1" "🐯 老虎  斩 1"))))
      (delete-file temp-file))))

(ert-deftest org-ladder-sm-boss-banner-kill-and-bonfire ()
  "Verify Boss banner, Boss kill feedback, Cloze stage promotion, and bonfire."
  (org-ladder-sm--reset-session)
  (let ((boss-unextracted '(:type topic :title "T1" :reviews 11
                            :child-topics 0 :child-clozes 0))
        (boss-wounded     '(:type topic :title "T2" :reviews 8
                            :child-topics 2 :child-clozes 1))
        (cloze-card       '(:type cloze :title "C1" :reviews 3
                            :child-topics 0 :child-clozes 0)))
    (let ((cloze-evergreen '(:type cloze :title "C2" :reviews 3 :interval-days 42
                             :child-topics 0 :child-clozes 0)))
      (should (equal (org-ladder-sm-banner boss-unextracted)
                     "👹 魔王"))
      (should (equal (org-ladder-sm-banner boss-wounded)
                     "👹 魔王"))
      (should (equal (org-ladder-sm-banner cloze-card)
                     "🌱 幼苗"))
      (should (equal (org-ladder-sm-banner cloze-evergreen)
                     "🌲 常青")))
    (let ((ev-promote (list :action 'cloze :card cloze-card
                            :rating :good :old-days 25 :new-days 42 :session t))
          (ev-same    (list :action 'cloze :card cloze-card
                            :rating :good :old-days 42 :new-days 65 :session t))
          (ev-again   (list :action 'cloze :card cloze-card
                            :rating :again :old-days 42 :new-days 0
                            :interval-label "10m" :session t))
          (ev-topic   (list :action 'topic :card '(:type topic :title "阅读架构" :reviews 4)
                            :old-days 6 :new-days 9 :session t))
          (ev-mob     (list :action 'dismiss :card '(:type topic :title "笔记法" :reviews 3)
                            :old-days 12 :session t))
          (ev-kill    (list :action 'dismiss :card boss-wounded
                            :old-days 30 :session t))
          (ev-archive (list :action 'dismiss :card cloze-card
                            :old-days 10 :session t)))
      (should (equal (org-ladder-sm-feedback ev-promote)
                     "🌿 → 🌲"))
      (should (equal (org-ladder-sm-feedback ev-same)
                     "🌲"))
      (should (equal (org-ladder-sm-feedback ev-again)
                     "↻ 🌱"))
      (should (equal (org-ladder-sm-feedback ev-topic)
                     "🐱 → 🐯"))
      (should (equal (org-ladder-sm-feedback ev-mob)
                     "🏆 🐱"))
      (should (equal (org-ladder-sm-feedback ev-kill)
                     "🏆 👹"))
      (should (equal (org-ladder-sm-feedback ev-archive)
                     "📦 🌿")))))

(ert-deftest org-ladder-sm-session-isolation-and-cache-invalidation ()
  (let ((org-ladder--cache '(stale))
        (org-ladder--cache-time t))
    (org-ladder-sm--reset-session)
    (org-ladder-sm--on-card-change '(:action extract))
    (should-not org-ladder--cache)
    (should-not org-ladder--cache-time)))

(ert-deftest org-ladder-sm-review-flow-integration ()
  "Exercise review presentation, grading and stopping on disposable cards."
  (require 'org-sm)
  (let ((buffer (generate-new-buffer " *org-sm-flow*"))
        (org-ladder-stock-files nil)
        (org-ladder-sm-files nil)
        (org-sm--queue nil)
        (org-sm--current-buffer nil)
        (org-sm--current-card-marker nil)
        (org-sm-prompt-function #'org-ladder-sm-prompt)
        (org-sm-feedback-function #'org-ladder-sm-feedback)
        (org-sm-finish-function #'org-ladder-sm-bonfire)
        (org-sm-review-start-hook '(org-ladder-sm--reset-session))
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
                (should-not (local-variable-p 'header-line-format))
                (should (equal org-ladder-notify-last-text
                               "🐯 老虎 [1/2]\n├─ Boss ◀\n  └─ Memory\n🧗 1F [░░░░░░]"))
                ;; Extracting a child card while reviewing updates the Boss HUD immediately:
                (org-sm-extract 'topic "sub" 0 3)
                (should (equal org-ladder-notify-last-text
                               "🐯 老虎 [1/2]\n├─ Boss ◀\n  ├─ Memory\n  └─ [T] sub\n🧗 1F [░░░░░░]"))
                (org-sm-item-dismiss)
                (should (eq org-sm--cloze-state 'hidden))
                (should (equal org-ladder-notify-last-text
                               "🌱 幼苗 [2/2]\n▸ Boss\n  ├─ Memory ◀\n  └─ [T] sub\n🧗 1F [░░░░░░]"))
                (should (buffer-narrowed-p))
                (org-sm-review-confirm)
                (should (eq org-sm--cloze-state 'revealed))
                (should (equal org-ladder-notify-last-text
                               "🌱 幼苗 [2/2]\n▸ Boss\n  ├─ Memory ◀\n  └─ [T] sub\n🧗 1F [░░░░░░]"))
                (cl-letf (((symbol-function 'read-multiple-choice)
                           (lambda (&rest _) '(?r "good"))))
                  (org-sm-review-confirm))
                (should (equal org-ladder-sm-last-log
                               '("✨"
                                 "🏆 🐯"
                                 "🌱")))
                (with-current-buffer "*Org Ladder*"
                  (should (string-match-p "🔥 上次战报 (3 张)" (buffer-string))))
                (should-not (buffer-narrowed-p))
                (should-not org-sm--current-card-marker)
                (should-not org-sm--cloze-state)
                (goto-char boss)
                (org-sm-item-undismiss)
                (org-sm--review-markers (list (copy-marker boss)))
                (org-sm-review-abort)
                (should-not org-sm--queue)
                (should-not (buffer-narrowed-p)))
              (set-marker boss nil)
              (set-marker cloze nil))))
      (when (get-buffer "*Org Ladder*") (kill-buffer "*Org Ladder*"))
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
    (should-not (funcall (org-sm--tree-filter nil "@mob")))
    (should-not (funcall (org-sm--tree-filter nil "@cloze")))
    (outline-next-heading)
    (should (funcall (org-sm--tree-filter nil "@cloze")))
    (should (funcall (org-sm--tree-filter nil "@seedling")))
    (should-not (funcall (org-sm--tree-filter nil "@boss")))))

(ert-deftest org-ladder-today-utc-offset-and-unsaved-buffer ()
  "Local-date conversion of UTC SRS_LAST and unsaved buffer scanning."
  (let ((temp-file (make-temp-file "org-ladder-today-test-" nil ".org"))
        (tz (getenv "TZ"))
        buf)
    (unwind-protect
        (progn
          (setenv "TZ" "UTC-8")
          (setq buf (find-file-noselect temp-file))
          (with-current-buffer buf
            (org-mode)
            ;; 2026-10-18T16:29:02Z is 2026-10-19 00:29:02 in UTC+8:
            (insert "* [C] Midnight Cloze\n"
                    "SCHEDULED: <2027-03-26 Fri 00:29>\n"
                    ":PROPERTIES:\n:SRS_TYPE: cloze\n:SRS_LAST: 2026-10-18T16:29:02Z\n:END:\n"
                    ":LOGBOOK:\n- cloze  :good  5mo  [2026-10-19 Mon 00:29]\n:END:\n"))
          ;; Intentionally do NOT save `buf' to disk:
          (let* ((cards (org-ladder-stock--scan-file temp-file))
                 (tbl   (make-hash-table :test 'equal)))
            (should (equal (plist-get (car cards) :date) "2026-10-19"))
            (should (equal (org-ladder-stock--period-lines
                            cards (lambda (d) (equal d "2026-10-19")))
                           '("🗿 古树  +1")))
            (org-ladder-sm--scan-file temp-file tbl)
            (should (= (gethash '(2026 10 19) tbl 0) org-ladder-sm-score-per-review))))
      (setenv "TZ" tz)
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (delete-file temp-file))))

(ert-deftest org-ladder-art-quantized-climb-math ()
  "24 pts = 1 floor (6 pips); every 5th floor = 1 fog tile; 180 floors = 1 relic."
  (let ((st (org-ladder-art-climb-state 20)))
    (should (= (plist-get st :cleared-floors) 0))
    (should (= (plist-get st :floor) 1))
    (should (= (plist-get st :step-in-floor) 5)))
  (let ((st (org-ladder-art-climb-state (+ (* 24 5 27) (* 24 4) 16))))
    (should (= (plist-get st :floor) 140))
    (should (= (plist-get st :step-in-floor) 4))
    (should (= (plist-get st :steps-left) 2))
    (should (= (plist-get st :special-floor) 140))
    (should (= (plist-get st :open-tiles) 27))
    (should-not (plist-get st :title)))
  (let ((st (org-ladder-art-climb-state 4320)))
    (should (= (plist-get st :cleared-floors) 180))
    (should (= (plist-get st :unlocked-count) 1))
    (should (= (plist-get st :open-tiles) 0))
    (should-not (plist-get st :svg))))

(provide 'test-org-ladder-stock)
;;; test-org-ladder-stock.el ends here
