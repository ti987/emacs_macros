;;; test-vhdl-sm.el --- Batch tests for vhdl-sm.el
;;; Usage: cd <repo-root> && emacs --batch -l test-vhdl-sm.el

(require 'cl-lib)

;; Load the module under test
(load-file (expand-file-name "vhdl-sm.el"
                             (file-name-directory load-file-name)))

(defvar test-sm-vhd-file
  (expand-file-name "test_vhdl/sm/sm_test.vhd"
                    (file-name-directory load-file-name)))

(defvar test-pass-count 0)
(defvar test-fail-count 0)

(defmacro check (desc form)
  `(if ,form
       (progn
         (setq test-pass-count (1+ test-pass-count))
         (message "  PASS: %s" ,desc))
     (progn
       (setq test-fail-count (1+ test-fail-count))
       (message "  FAIL: %s" ,desc))))

;;; -------------------------------------------------------------------------
;;; Load the test VHDL file
;;; -------------------------------------------------------------------------
(message "\n=== vhdl-sm.el unit tests ===\n")

(let* ((buf  (find-file-noselect test-sm-vhd-file))
       (sms  (with-current-buffer buf
               (vhdl-sm--scan-buffer vhdl-sm-state-regexp))))

  ;; --- Scan results ---
  (message "-- State machine scan --")

  (check "Finds at least 2 state machines (cur_state and next_state)"
         (>= (length sms) 2))

  (check "Detects cur_state as a state signal"
         (cl-some (lambda (sm) (string-equal (plist-get sm :sm-signal) "cur_state"))
                  sms))

  (check "Detects next_state as a state signal"
         (cl-some (lambda (sm) (string-equal (plist-get sm :sm-signal) "next_state"))
                  sms))

  (check "Does NOT detect data_in as a state machine (non-matching signal)"
         (not (cl-some (lambda (sm) (string-equal (plist-get sm :sm-signal) "data_in"))
                       sms)))

  ;; --- Process-label detection ---
  (message "\n-- Process label detection --")

  (check "nsl_proc label captured for cur_state machine"
         (cl-some (lambda (sm)
                    (and (string-equal (plist-get sm :sm-signal) "cur_state")
                         (string-equal (plist-get sm :process-label) "nsl_proc")))
                  sms))

  (check "Unlabeled process has nil process-label"
         (cl-some (lambda (sm)
                    (and (string-equal (plist-get sm :sm-signal) "next_state")
                         (null (plist-get sm :process-label))))
                  sms))

  ;; --- State enumeration ---
  (message "\n-- State enumeration --")

  (let ((cur-sm (cl-find "cur_state" sms
                         :key (lambda (sm) (plist-get sm :sm-signal))
                         :test #'string-equal)))

    (check "cur_state machine has at least 3 states"
           (and cur-sm (>= (length (plist-get cur-sm :states)) 3)))

    (check "cur_state machine includes ST_IDLE"
           (and cur-sm
                (cl-some (lambda (s) (string-equal (car s) "ST_IDLE"))
                         (plist-get cur-sm :states))))

    (check "cur_state machine includes ST_RUNNING"
           (and cur-sm
                (cl-some (lambda (s) (string-equal (car s) "ST_RUNNING"))
                         (plist-get cur-sm :states))))

    (check "cur_state machine includes ST_DONE"
           (and cur-sm
                (cl-some (lambda (s) (string-equal (car s) "ST_DONE"))
                         (plist-get cur-sm :states))))

    (check "cur_state machine does NOT include 'others' as a state"
           (and cur-sm
                (not (cl-some (lambda (s) (string-equal (downcase (car s)) "others"))
                              (plist-get cur-sm :states))))))

  ;; --- Line numbers ---
  (message "\n-- Line numbers --")

  (check "sm-line is a positive integer for each machine"
         (cl-every (lambda (sm) (and (integerp (plist-get sm :sm-line))
                                     (> (plist-get sm :sm-line) 0)))
                   sms))

  (check "process-line is a positive integer for each machine"
         (cl-every (lambda (sm) (and (integerp (plist-get sm :process-line))
                                     (> (plist-get sm :process-line) 0)))
                   sms))

  ;; --- Transition collection ---
  (message "\n-- Transition collection --")

  (let ((cur-sm (cl-find "cur_state" sms
                         :key (lambda (sm) (plist-get sm :sm-signal))
                         :test #'string-equal)))

    (check ":transitions key is present in scan result"
           (and cur-sm (not (eq (plist-get cur-sm :transitions) 'missing))))

    (check "cur_state machine has at least one transition"
           (and cur-sm (> (length (plist-get cur-sm :transitions)) 0)))

    (check "ST_IDLE -> ST_RUNNING transition captured"
           (and cur-sm
                (cl-some (lambda (tr)
                           (and (string-equal (car  tr) "ST_IDLE")
                                (string-equal (cadr tr) "ST_RUNNING")))
                         (plist-get cur-sm :transitions))))

    (check "ST_RUNNING -> ST_DONE transition captured"
           (and cur-sm
                (cl-some (lambda (tr)
                           (and (string-equal (car  tr) "ST_RUNNING")
                                (string-equal (cadr tr) "ST_DONE")))
                         (plist-get cur-sm :transitions))))

    (check "ST_DONE -> ST_IDLE transition captured"
           (and cur-sm
                (cl-some (lambda (tr)
                           (and (string-equal (car  tr) "ST_DONE")
                                (string-equal (cadr tr) "ST_IDLE")))
                         (plist-get cur-sm :transitions))))

    (check "ST_ERROR -> ST_IDLE transition captured"
           (and cur-sm
                (cl-some (lambda (tr)
                           (and (string-equal (car  tr) "ST_ERROR")
                                (string-equal (cadr tr) "ST_IDLE")))
                         (plist-get cur-sm :transitions))))

    (check "No duplicate transitions (each (FROM TO) pair is unique)"
           (and cur-sm
                (let ((trs (plist-get cur-sm :transitions)))
                  (= (length trs)
                     (length (cl-remove-duplicates trs :test #'equal))))))

    (check "data_in case produces no transitions in its own scan (not a state machine)"
           ;; data_proc is never added to sms, so no transitions either
           (not (cl-some (lambda (sm)
                           (string-equal (plist-get sm :sm-signal) "data_in"))
                         sms))))

  ;; --- Format output ---
  (message "\n-- Output format --")

  (let ((output (vhdl-sm--format-output sms "sm_test.vhd")))

    (check "Output starts with '-- %DSL_START extract-state-machines'"
           (string-match-p "^-- %DSL_START extract-state-machines" output))

    (check "Output ends with '-- %DSL_END'"
           (string-match-p "-- %DSL_END\\'" output))

    (check "Output contains cur_state"
           (string-match-p "cur_state" output))

    (check "Output contains next_state"
           (string-match-p "next_state" output))

    (check "Output contains nsl_proc process label"
           (string-match-p "nsl_proc" output))

    (check "Output contains ST_IDLE state name"
           (string-match-p "ST_IDLE" output))

    (check "Output contains ST_RUNNING state name"
           (string-match-p "ST_RUNNING" output))

    (check "Output does NOT contain data_in"
           (not (string-match-p "data_in" output)))

    (check "Output contains 'case line:' annotation"
           (string-match-p "case line:" output))

    ;; --- New: node declaration format ---
    (check "Output contains node declaration for ST_IDLE"
           (string-match-p "node ST_IDLE " output))

    (check "Output contains node declaration for ST_RUNNING"
           (string-match-p "node ST_RUNNING " output))

    (check "Output contains node declaration for ST_DONE"
           (string-match-p "node ST_DONE " output))

    (check "node label for ST_IDLE includes state comment (Idle: wait for start)"
           (string-match-p "node ST_IDLE \"ST_IDLE\\\\nIdle: wait for start\"" output))

    (check "node label for ST_RUNNING includes state comment (Running: process data)"
           (string-match-p "node ST_RUNNING \"ST_RUNNING\\\\nRunning: process data\"" output))

    (check "node label for ST_ERROR has no extra \\n (no comment)"
           (string-match-p "node ST_ERROR \"ST_ERROR\"" output))

    ;; --- New: transition arrow format ---
    (check "Output contains transition arrow ST_IDLE -> ST_RUNNING"
           (string-match-p "ST_IDLE -> ST_RUNNING" output))

    (check "Output contains transition arrow ST_RUNNING -> ST_DONE"
           (string-match-p "ST_RUNNING -> ST_DONE" output))

    (check "Output contains transition arrow ST_DONE -> ST_IDLE"
           (string-match-p "ST_DONE -> ST_IDLE" output))

    (check "Output contains transition arrow ST_ERROR -> ST_IDLE"
           (string-match-p "ST_ERROR -> ST_IDLE" output))

    (check "Output contains self-loop ST_IDLE -> ST_IDLE (from else branch)"
           (string-match-p "ST_IDLE -> ST_IDLE" output))

    ;; Node declarations appear before the blank separator / transition section
    (check "Node declaration appears before corresponding transition arrow"
           (< (string-match "node ST_IDLE " output)
              (string-match "ST_IDLE -> ST_RUNNING" output))))

  ;; --- Head-comment insertion ---
  (message "\n-- Head comment insertion --")

  (let* ((test-buf  (generate-new-buffer "*sm-test-insert*"))
         (sample-vhd "-- sample.vhd\n-- A sample file.\n\nlibrary ieee;\n"))
    (with-current-buffer test-buf
      (insert sample-vhd)
      (let ((output (vhdl-sm--format-output sms "sample.vhd")))
        (vhdl-sm--write-head-comment output)))

    (let ((content (with-current-buffer test-buf (buffer-string))))
      (check "DSL_START present after insertion"
             (string-match-p "-- %DSL_START extract-state-machines" content))
      (check "DSL_END present after insertion"
             (string-match-p "-- %DSL_END" content))
      (check "Original head comment preserved before DSL block"
             (string-match-p "-- sample.vhd" content))
      (check "Non-comment VHDL content remains after DSL block"
             (string-match-p "library ieee;" content))
      (check "DSL_START appears after last head-comment line"
             (< (string-match "-- A sample file" content)
                (string-match "%DSL_START" content))))

    ;; Idempotency: running again replaces the block, not appends it
    (with-current-buffer test-buf
      (let ((output (vhdl-sm--format-output sms "sample.vhd")))
        (vhdl-sm--write-head-comment output)))

    (let ((content (with-current-buffer test-buf (buffer-string))))
      (check "Exactly one DSL_START after second run (idempotent)"
             (let ((count 0) (pos 0))
               (while (string-match "-- %DSL_START" content pos)
                 (setq count (1+ count))
                 (setq pos (match-end 0)))
               (= count 1))))

    (kill-buffer test-buf))

  ;; --- Custom regexp ---
  (message "\n-- Custom regexp --")

  (let ((custom-sms (with-current-buffer buf
                      (vhdl-sm--scan-buffer "cur_state"))))
    (check "Custom regexp 'cur_state' finds exactly cur_state machine"
           (and (= (length custom-sms) 1)
                (string-equal (plist-get (car custom-sms) :sm-signal)
                              "cur_state"))))

  (let ((no-sms (with-current-buffer buf
                  (vhdl-sm--scan-buffer "NOMATCH_REGEXP"))))
    (check "Non-matching regexp returns empty list"
           (null no-sms)))

  (message "\n=== Results: %d passed, %d failed ===\n"
           test-pass-count test-fail-count)
  (when (> test-fail-count 0)
    (kill-emacs 1))
  (kill-emacs 0))
