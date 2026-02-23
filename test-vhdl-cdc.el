;;; test-vhdl-cdc.el --- Batch tests for vhdl-cdc.el
;;; Usage: emacs --batch -l test-vhdl-cdc.el

(require 'cl-lib)

;; Load the module under test
(load-file (expand-file-name "vhdl-cdc.el"
                             (file-name-directory load-file-name)))

(defvar test-vhdl-file
  (expand-file-name "test_vhdl/cdc/cdc_test.vhd"
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
;;; Configure user variables for the test
;;; -------------------------------------------------------------------------
(setq vhdl-cdc-clock '("extra_clk"))     ; A.3 (not actually in the test file,
                                          ; but exercises the lookup)

(setq vhdl-cdc-clk-domain
      '(("sync_ff" "d" "clk")))          ; B.3 rule used in cdc_test.vhd

(setq vhdl-cdc-ignore
      '((:name "gray_count" :rationale "gray-coded counter, safe to cross")))

;;; -------------------------------------------------------------------------
;;; Load the test VHDL file into a buffer
;;; -------------------------------------------------------------------------
(message "\n=== vhdl-cdc.el unit tests ===\n")

(let* ((buf    (find-file-noselect test-vhdl-file))
       (decls  (with-current-buffer buf (vhdl-cdc--scan-declarations buf)))
       (clocks (vhdl-cdc--find-clocks decls))
       (clock-names (mapcar (lambda (c) (plist-get c :name)) clocks))
       (dom-b1 (with-current-buffer buf
                 (vhdl-cdc--domains-from-processes buf clock-names)))
       (dom-b2 (vhdl-cdc--domains-from-comments decls clock-names))
       (dom-b3 (with-current-buffer buf
                 (vhdl-cdc--domains-from-instances buf clock-names)))
       (domains (vhdl-cdc--merge-domains dom-b1 dom-b2 dom-b3)))

  ;; --- Clock detection ---
  (message "-- Clock detection --")

  (check "A.1: sys_clk detected as clock"
         (cl-some (lambda (c) (and (string-equal (plist-get c :name) "sys_clk")
                                   (string-equal (plist-get c :method) "A.1")))
                  clocks))

  (check "A.1: fast_clk detected as clock"
         (cl-some (lambda (c) (and (string-equal (plist-get c :name) "fast_clk")
                                   (string-equal (plist-get c :method) "A.1")))
                  clocks))

  (check "A.2: ref_clock detected as clock"
         (cl-some (lambda (c) (and (string-equal (plist-get c :name) "ref_clock")
                                   (string-equal (plist-get c :method) "A.2")))
                  clocks))

  (check "A.1: rst_n is NOT detected as a clock"
         (not (cl-some (lambda (c) (string-equal (plist-get c :name) "rst_n"))
                       clocks)))

  ;; --- B.1 process-based domain assignment ---
  (message "\n-- B.1 process domain assignment --")

  (check "B.1: counter in sys_clk domain"
         (let ((entries (gethash "counter" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 1 e) "B.1")))
                    entries)))

  (check "B.1: fast_counter in fast_clk domain"
         (let ((entries (gethash "fast_counter" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "fast_clk")
                                     (string-equal (nth 1 e) "B.1")))
                    entries)))

  (check "B.1: crossing_data in sys_clk domain"
         (let ((entries (gethash "crossing_data" dom-b1)))
           (cl-some (lambda (e) (string-equal (car e) "sys_clk")) entries)))

  (check "B.1: crossing_data also in fast_clk domain"
         (let ((entries (gethash "crossing_data" dom-b1)))
           (cl-some (lambda (e) (string-equal (car e) "fast_clk")) entries)))

  ;; --- B.2 comment-based domain assignment ---
  (message "\n-- B.2 comment-based domain assignment --")

  (check "B.2: annotated_sig in sys_clk domain via clk_dom: comment"
         (let ((entries (gethash "annotated_sig" dom-b2)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 1 e) "B.2")))
                    entries)))

  ;; --- B.3 instance port rule ---
  (message "\n-- B.3 instance port domain assignment --")

  (check "B.3: synced_input in sys_clk domain via sync_ff instance"
         (let ((entries (gethash "synced_input" dom-b3)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 1 e) "B.3")))
                    entries)))

  ;; --- CDC violation detection ---
  (message "\n-- CDC violation detection --")

  (check "CDC: crossing_data has multiple domains -> violation"
         (let ((entries (gethash "crossing_data" domains)))
           (> (length entries) 1)))

  (check "CDC: gray_count has multiple domains -> violation (but ignored)"
         (let ((entries (gethash "gray_count" domains)))
           (> (length entries) 1)))

  (check "CDC: gray_count is in vhdl-cdc-ignore"
         (vhdl-cdc--ignored-p "gray_count"))

  (check "CDC: crossing_data is NOT in vhdl-cdc-ignore"
         (not (vhdl-cdc--ignored-p "crossing_data")))

  ;; --- Full output test ---
  (message "\n-- Full output generation --")
  (let ((output (vhdl-cdc--format-output "cdc_test.vhd" clocks domains decls)))
    (check "Output contains 'Domain Clocks' section"
           (string-match-p "Domain Clocks" output))
    (check "Output contains sys_clk clock"
           (string-match-p "sys_clk" output))
    (check "Output contains *** for crossing_data"
           (string-match-p "\\*\\*\\*.*crossing_data" output))
    (check "Output shows ignored signal without ***"
           (and (string-match-p "gray_count" output)
                (string-match-p "ignored:" output)))
    (check "Output contains 'Unknown Clock Domain' section"
           (string-match-p "Unknown Clock Domain" output))
    ;; rst_n is declared as a port but never assigned a clock domain
    (check "Unknown section contains rst_n"
           (string-match-p "rst_n" output))
    ;; Unknown section must appear before CDC section
    (check "Unknown section precedes CDC section"
           (< (string-match "Unknown Clock Domain" output)
              (string-match "CDC Signals" output))))

  ;; --- Regression: vhdl-cdc-clk-domain nil must not crash ---
  (message "\n-- Regression: nil vhdl-cdc-clk-domain returns hash-table --")
  (let ((vhdl-cdc-clk-domain nil))
    (check "domains-from-instances returns hash-table when vhdl-cdc-clk-domain is nil"
           (hash-table-p
            (with-current-buffer buf
              (vhdl-cdc--domains-from-instances buf clock-names))))
    ;; The full analyze pipeline must also work with nil vhdl-cdc-clk-domain
    (check "merge-domains does not error with nil vhdl-cdc-clk-domain"
           (let* ((d3  (with-current-buffer buf
                         (vhdl-cdc--domains-from-instances buf clock-names)))
                  (merged (vhdl-cdc--merge-domains dom-b1 dom-b2 d3)))
             (hash-table-p merged))))

  (message "\n=== Results: %d passed, %d failed ===\n"
           test-pass-count test-fail-count)
  (when (> test-fail-count 0)
    (kill-emacs 1))
  (kill-emacs 0))
