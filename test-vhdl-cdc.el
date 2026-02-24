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
      '(("sync_ff" ("d" . "clk"))))    ; B.3 rule used in cdc_test.vhd

(setq vhdl-cdc-ignore
      '((:name "gray_count" :rationale "gray-coded counter, safe to cross")))

;;; -------------------------------------------------------------------------
;;; Load the test VHDL file into a buffer
;;; -------------------------------------------------------------------------
(message "\n=== vhdl-cdc.el unit tests ===\n")

(let* ((buf        (find-file-noselect test-vhdl-file))
       (decls      (with-current-buffer buf (vhdl-cdc--scan-declarations buf)))
       (clocks     (vhdl-cdc--find-clocks decls))
       (clock-names (mapcar (lambda (c) (plist-get c :name)) clocks))
       (decl-names  (mapcar (lambda (d) (plist-get d :name)) decls))
       (dom-b1     (with-current-buffer buf
                     (vhdl-cdc--domains-from-processes buf clock-names decl-names)))
       (dom-b2     (vhdl-cdc--domains-from-comments decls clock-names))
       (dom-b3     (with-current-buffer buf
                     (vhdl-cdc--domains-from-instances buf clock-names)))
       (domains    (vhdl-cdc--merge-domains dom-b1 dom-b2 dom-b3)))

  ;; --- Clock detection ---
  (message "-- Clock detection --")

  (check "buffer-entity-name: finds 'cdc_test' from entity declaration"
         (string-equal (vhdl-cdc--buffer-entity-name buf) "cdc_test"))

  (check "A.1: sys_clk detected as clock"
         (cl-some (lambda (c) (and (string-equal (plist-get c :name) "sys_clk")
                                   (string-equal (plist-get c :method) "A.1")))
                  clocks))

  (check "A.1: fast_clk detected as clock"
         (cl-some (lambda (c) (and (string-equal (plist-get c :name) "fast_clk")
                                   (string-equal (plist-get c :method) "A.1")))
                  clocks))

  (check "A.2: ref_clock detected via cdc_clock keyword"
         (cl-some (lambda (c) (and (string-equal (plist-get c :name) "ref_clock")
                                   (string-equal (plist-get c :method) "A.2")))
                  clocks))

  (check "A.2: aux_clock detected via cdc_clock keyword"
         (cl-some (lambda (c) (and (string-equal (plist-get c :name) "aux_clock")
                                   (string-equal (plist-get c :method) "A.2")))
                  clocks))

  (check "A.1: rst_n is NOT detected as a clock"
         (not (cl-some (lambda (c) (string-equal (plist-get c :name) "rst_n"))
                       clocks)))

  ;; --- B.1 process-based domain assignment (assigned) ---
  (message "\n-- B.1 process domain assignment (assigned) --")

  (check "B.1: counter in sys_clk domain (assigned)"
         (let ((entries (gethash "counter" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 1 e) "B.1")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  (check "B.1: fast_counter in fast_clk domain (assigned)"
         (let ((entries (gethash "fast_counter" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "fast_clk")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  (check "B.1: crossing_data in sys_clk domain (assigned)"
         (let ((entries (gethash "crossing_data" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  (check "B.1: crossing_data also in fast_clk domain (assigned)"
         (let ((entries (gethash "crossing_data" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "fast_clk")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  ;; --- B.1 reference detection ---
  (message "\n-- B.1 reference detection --")

  (check "B.1: shared_ref_data assigned in sys_clk (assigned)"
         (let ((entries (gethash "shared_ref_data" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  (check "B.1: shared_ref_data referenced in fast_clk (referenced)"
         (let ((entries (gethash "shared_ref_data" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "fast_clk")
                                     (string-equal (nth 3 e) "referenced")))
                    entries)))

  (check "B.1: ref_only_sig referenced in sys_clk (referenced, not assigned)"
         (let ((entries (gethash "ref_only_sig" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 3 e) "referenced")))
                    entries)))

  (check "B.1: ref_only_sig referenced in fast_clk (referenced, not assigned)"
         (let ((entries (gethash "ref_only_sig" dom-b1)))
           (cl-some (lambda (e) (and (string-equal (car e) "fast_clk")
                                     (string-equal (nth 3 e) "referenced")))
                    entries)))

  (check "B.1: ref_only_sig has NO assigned entry in any domain"
         (let ((entries (gethash "ref_only_sig" dom-b1)))
           (not (cl-some (lambda (e) (string-equal (nth 3 e) "assigned"))
                         entries))))

  ;; --- B.2 comment-based domain assignment ---
  (message "\n-- B.2 comment-based domain assignment --")

  (check "B.2: annotated_sig via inline cdc_domain: keyword"
         (let ((entries (gethash "annotated_sig" dom-b2)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 1 e) "B.2")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  (check "B.2: block_sig1 via block comment cdc_domain: annotation"
         (let ((entries (gethash "block_sig1" dom-b2)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  (check "B.2: block_sig2 via same block comment annotation"
         (let ((entries (gethash "block_sig2" dom-b2)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  ;; --- B.3 instance port rule ---
  (message "\n-- B.3 instance port domain assignment --")

  (check "B.3: synced_input in sys_clk domain (exact entity name)"
         (let ((entries (gethash "synced_input" dom-b3)))
           (cl-some (lambda (e) (and (string-equal (car e) "sys_clk")
                                     (string-equal (nth 1 e) "B.3")
                                     (string-equal (nth 3 e) "assigned")))
                    entries)))

  ;; Test B.3 with regexp entity name
  (check "B.3: regexp entity name sync.* also matches sync_ff"
         (let* ((vhdl-cdc-clk-domain '(("sync.*" ("d" . "clk"))))
                (d3-re (with-current-buffer buf
                         (vhdl-cdc--domains-from-instances buf clock-names)))
                (entries (gethash "synced_input" d3-re)))
           (cl-some (lambda (e) (string-equal (car e) "sys_clk")) entries)))

  ;; --- cdc_ignore: declaration-level and statement-level ---
  (message "\n-- cdc_ignore annotation --")

  (check "cdc_ignore decl: decl_ignored_sig has :cdc-ignore t in declarations"
         (let ((d (cl-find "decl_ignored_sig" decls
                           :key (lambda (d) (plist-get d :name))
                           :test #'string-equal)))
           (and d (plist-get d :cdc-ignore))))

  (check "cdc_ignore decl: line_ignored_sig does NOT have :cdc-ignore t"
         (let ((d (cl-find "line_ignored_sig" decls
                           :key (lambda (d) (plist-get d :name))
                           :test #'string-equal)))
           (and d (not (plist-get d :cdc-ignore)))))

  (check "cdc_ignore stmt: line_ignored_sig is NOT in fast_clk domain (ref suppressed)"
         (let ((entries (gethash "line_ignored_sig" dom-b1)))
           (not (cl-some (lambda (e) (string-equal (car e) "fast_clk")) entries))))

  (check "cdc_ignore stmt: line_ignored_sig IS in sys_clk domain (assignment tracked)"
         (let ((entries (gethash "line_ignored_sig" dom-b1)))
           (cl-some (lambda (e) (string-equal (car e) "sys_clk")) entries)))

  (check "cdc_ignore stmt: decl_ignored_sig IS in fast_clk domain (ref NOT suppressed)"
         (let ((entries (gethash "decl_ignored_sig" dom-b1)))
           (cl-some (lambda (e) (string-equal (car e) "fast_clk")) entries)))

  ;; --- CDC violation detection ---
  (message "\n-- CDC violation detection --")

  (check "CDC: crossing_data has multiple assigned domains -> violation"
         (let ((entries (gethash "crossing_data" domains)))
           (and (> (length entries) 1)
                (cl-every (lambda (e) (string-equal (nth 3 e) "assigned"))
                          entries))))

  (check "CDC: shared_ref_data is CDC (assigned sys_clk, referenced fast_clk)"
         (let ((entries (gethash "shared_ref_data" domains)))
           (and (> (length entries) 1)
                (cl-some (lambda (e) (string-equal (nth 3 e) "assigned")) entries))))

  (check "CDC: ref_only_sig IS CDC (referenced in multiple domains, same as cross-domain read)"
         (let ((entries (gethash "ref_only_sig" domains)))
           (> (length entries) 1)))

  (check "CDC: gray_count has multiple domains -> violation (but ignored)"
         (let ((entries (gethash "gray_count" domains)))
           (> (length entries) 1)))

  (check "CDC: gray_count is in vhdl-cdc-ignore"
         (vhdl-cdc--ignored-p "gray_count"))

  (check "CDC: crossing_data is NOT in vhdl-cdc-ignore"
         (not (vhdl-cdc--ignored-p "crossing_data")))

  ;; --- Full output test (uses combined vhdl-cdc-ignore + decl-ignores) ---
  (message "\n-- Full output generation --")
  ;; Simulate what vhdl-cdc-analyze does: augment vhdl-cdc-ignore with decl annotations
  (let* ((decl-ignores
          (cl-loop for d in decls
                   when (plist-get d :cdc-ignore)
                   collect (list :name (plist-get d :name)
                                 :rationale "cdc_ignore annotation")))
         (vhdl-cdc-ignore (append vhdl-cdc-ignore decl-ignores))
         ;; entity-name is scanned from the buffer (not from file name)
         (entity-name (vhdl-cdc--buffer-entity-name buf))
         (output (vhdl-cdc--format-output entity-name "cdc_test.vhd" clocks domains decls)))
    (check "Output contains 'Domain Clocks' section"
           (string-match-p "Domain Clocks" output))
    (check "Output contains sys_clk clock"
           (string-match-p "sys_clk" output))
    (check "Output contains aux_clock clock (cdc_clock keyword)"
           (string-match-p "aux_clock" output))
    (check "Output contains *** for crossing_data"
           (string-match-p "\\*\\*\\*.*crossing_data" output))
    (check "Output contains *** for shared_ref_data (ref+assign CDC)"
           (string-match-p "\\*\\*\\*.*shared_ref_data" output))
    (check "Output contains *** for ref_only_sig (referenced in multiple domains = CDC)"
           (string-match-p "\\*\\*\\*.*ref_only_sig" output))
    (check "Domain listing shows 'assigned' column"
           (string-match-p "assigned" output))
    (check "Domain listing shows 'referenced' column"
           (string-match-p "referenced" output))
    (check "Output contains 'Ignored CDC Signals' section"
           (string-match-p "Ignored CDC Signals" output))
    (check "Ignored section contains decl_ignored_sig (cdc_ignore on declaration)"
           (string-match-p "decl_ignored_sig" output))
    (check "decl_ignored_sig NOT preceded by *** (declaration-ignored)"
           (not (string-match-p "\\*\\*\\*.*decl_ignored_sig" output)))
    (check "line_ignored_sig NOT in CDC section (per-line suppression prevents fast_clk entry)"
           (not (string-match-p "\\*\\*\\*.*line_ignored_sig" output)))
    (check "Output contains gray_count" ; still ignored via vhdl-cdc-ignore variable
           (string-match-p "gray_count" output))
    (check "Ignored section precedes CDC section"
           (< (string-match "Ignored CDC Signals" output)
              (string-match "CDC Signals (Multiple" output)))
    (check "gray_count NOT preceded by *** in output"
           (not (string-match-p "\\*\\*\\*.*gray_count" output)))
    (check "Output contains 'Unknown Clock Domain' section"
           (string-match-p "Unknown Clock Domain" output))
    (check "Unknown section precedes Ignored section"
           (< (string-match "Unknown Clock Domain" output)
              (string-match "Ignored CDC Signals" output)))
    (check "Output contains 'Port Domain Mappings' section"
           (string-match-p "Port Domain Mappings" output))
    (check "Snippet contains vhdl-cdc-add-port-domains call"
           (string-match-p "vhdl-cdc-add-port-domains" output))
    (check "Snippet names the entity cdc_test"
           (string-match-p "\"cdc_test\"" output))
    (check "Snippet maps data_in to sys_clk (B.1 assigned)"
           (string-match-p "\"data_in\".*\\..*\"sys_clk\"" output))
    (check "Snippet does not include sys_clk itself (it is a clock port, not data)"
           ;; sys_clk should not appear as a DATA-PORT on the left of a cons pair
           ;; in the snippet (it is the CLK-PORT side for data_in/rst_n)
           (not (string-match-p "(\"sys_clk\"" output))))

  ;; --- vhdl-cdc-add-port-domains accumulation ---
  (message "\n-- vhdl-cdc-add-port-domains --")

  (let ((vhdl-cdc-clk-domain nil))
    (vhdl-cdc-add-port-domains "mod_a" '(("d" . "clk")))
    (check "add-port-domains: first call creates entry"
           (assoc "mod_a" vhdl-cdc-clk-domain))
    (check "add-port-domains: entry has correct port mapping"
           (equal (cdr (assoc "mod_a" vhdl-cdc-clk-domain))
                  '(("d" . "clk"))))
    (vhdl-cdc-add-port-domains "mod_b" '(("wr" . "wr_clk") ("rd" . "rd_clk")))
    (check "add-port-domains: second call adds second entry"
           (and (assoc "mod_a" vhdl-cdc-clk-domain)
                (assoc "mod_b" vhdl-cdc-clk-domain)))
    (vhdl-cdc-add-port-domains "mod_a" '(("q" . "clk")))
    (check "add-port-domains: repeat call replaces existing entry"
           (equal (cdr (assoc "mod_a" vhdl-cdc-clk-domain))
                  '(("q" . "clk")))))

  ;; --- Regression: vhdl-cdc-clk-domain nil must not crash ---
  (message "\n-- Regression: nil vhdl-cdc-clk-domain returns hash-table --")
  (let ((vhdl-cdc-clk-domain nil))
    (check "domains-from-instances returns hash-table when vhdl-cdc-clk-domain is nil"
           (hash-table-p
            (with-current-buffer buf
              (vhdl-cdc--domains-from-instances buf clock-names))))
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
