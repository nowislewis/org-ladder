;;; test-org-ladder-clock.el --- Tests for org-ladder-clock.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-ladder-clock)

(defun org-ladder-clock--run-test (org-content)
  "Helper: Create a temp file with ORG-CONTENT, run scan, and return the hash table."
  (let* ((temp-file (make-temp-file "org-ladder-clock-test-"))
         (tbl (make-hash-table :test 'equal)))
    (with-temp-file temp-file
      (insert org-content))
    (unwind-protect
        (progn
          (org-ladder-clock--scan-file temp-file tbl)
          tbl)
      (delete-file temp-file))))

;; 1. Progressive Clocking (No CLOSED)
(ert-deftest org-ladder-clock-progressive-only ()
  "Test progressive clocking on a task without a CLOSED timestamp."
  (let* ((content "* TODO Read Book\nCLOCK: [2026-05-18 Mon 10:00]--[2026-05-18 Mon 11:00] =>  1:00\nCLOCK: [2026-05-19 Tue 10:00]--[2026-05-19 Tue 10:30] =>  0:30\n")
         (tbl (org-ladder-clock--run-test content)))
    (should (= (gethash (org-ladder-time-from-emacs (org-time-string-to-time "2026-05-18")) tbl 0) 60))
    (should (= (gethash (org-ladder-time-from-emacs (org-time-string-to-time "2026-05-19")) tbl 0) 30))))

;; 2. Completion Bonus (No Clock)
(ert-deftest org-ladder-clock-completion-bonus-no-clock ()
  "Test completion bonus for a task with EFFORT but no CLOCK."
  (let* ((content "* DONE Quick Task\n  CLOSED: [2026-05-20 Wed 10:00]\n  :PROPERTIES:\n  :EFFORT: 2h\n  :END:\n")
         (tbl (org-ladder-clock--run-test content)))
    (should (= (gethash (org-ladder-time-from-emacs (org-time-string-to-time "2026-05-20")) tbl 0) 120))))

;; 3. Completion Bonus (Partial Clock)
(ert-deftest org-ladder-clock-completion-bonus-partial ()
  "Test completion bonus when clocked time is less than effort."
  (let* ((content "* DONE Project Task\n  CLOSED: [2026-05-21 Thu 12:00]\n  :PROPERTIES:\n  :EFFORT: 3h\n  :END:\n  CLOCK: [2026-05-21 Thu 10:00]--[2026-05-21 Thu 11:00] =>  1:00\n")
         (tbl (org-ladder-clock--run-test content)))
    ;; 60 from CLOCK + 120 Bonus on the same day = 180 (3h)
    (should (= (gethash (org-ladder-time-from-emacs (org-time-string-to-time "2026-05-21")) tbl 0) 180))))

;; 4. Completion Bonus (Over Clock)
(ert-deftest org-ladder-clock-completion-bonus-over ()
  "Test no bonus is given if clocked time exceeds effort."
  (let* ((content "* DONE Hard Task\n  CLOSED: [2026-05-22 Fri 12:00]\n  :PROPERTIES:\n  :EFFORT: 1h\n  :END:\n  CLOCK: [2026-05-22 Fri 09:00]--[2026-05-22 Fri 11:00] =>  2:00\n")
         (tbl (org-ladder-clock--run-test content)))
    ;; Only the 120 minutes from CLOCK, 0 bonus
    (should (= (gethash (org-ladder-time-from-emacs (org-time-string-to-time "2026-05-22")) tbl 0) 120))))

;; 5. Habit (No Clock, Baseline Score)
(ert-deftest org-ladder-clock-habit-baseline ()
  "Test habit scoring with DONE state."
  (let* ((content "* TODO Exercise\n  :PROPERTIES:\n  :STYLE: habit\n  :EFFORT: 30\n  :END:\n  - State \"DONE\"       from \"TODO\"       [2026-05-23 Sat 10:00]\n")
         (tbl (org-ladder-clock--run-test content)))
    (should (= (gethash (org-ladder-time-from-emacs (org-time-string-to-time "2026-05-23")) tbl 0) 30))))

;; 6. Habit (With Clock, Dual Score)
(ert-deftest org-ladder-clock-habit-with-clock ()
  "Test habit scoring with both CLOCK and DONE state."
  (let* ((content "* TODO Deep Work Habit\n  :PROPERTIES:\n  :STYLE: habit\n  :EFFORT: 1h\n  :END:\n  CLOCK: [2026-05-24 Sun 10:00]--[2026-05-24 Sun 11:30] =>  1:30\n  - State \"DONE\"       from \"TODO\"       [2026-05-24 Sun 11:30]\n")
         (tbl (org-ladder-clock--run-test content)))
    ;; 90 from CLOCK + 60 from DONE state = 150
    (should (= (gethash (org-ladder-time-from-emacs (org-time-string-to-time "2026-05-24")) tbl 0) 150))))

;; 7. Malformed Input Resilience
(ert-deftest org-ladder-clock-malformed-input ()
  "Test resilience against malformed CLOCK/EFFORT entries."
  (let* ((content "* DONE Bad Task\n  CLOSED: [2026-05-25 Mon 10:00]\n  :PROPERTIES:\n  :EFFORT: bad-effort\n  :END:\n  CLOCK: [2026-05-25 Mon 10:00]--[2026-05-25 Mon 11:00] =>  bad-duration\n")
         (tbl (org-ladder-clock--run-test content)))
    ;; Malformed Effort & Clock are ignored.
    ;; Task has CLOSED without valid clock, so it falls back to default-duration (5).
    (should (= (gethash (org-ladder-time-from-emacs (org-time-string-to-time "2026-05-25")) tbl 0) 5))))

(provide 'test-org-ladder-clock)
;;; test-org-ladder-clock.el ends here