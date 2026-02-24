;;
;; vhdl-cdc.el - VHDL Clock Domain Crossing (CDC) Analysis
;; T. Isogai 2024
;;
;; Analyze a VHDL file (buffer) to group signals and ports by clock domain.
;;
;; Clock domain detection rules:
;;   A.1 - signal/port name ends with "clk" (case-insensitive)
;;   A.2 - signal/port inline comment (or preceding block comment) contains
;;         "dom_clk:" or "domain_clock:"
;;   A.3 - signal/port name is listed in `vhdl-cdc-clock'
;;
;; Signal clock-domain assignment rules:
;;   B.1 - signal assigned or referenced in a process whose sensitivity list
;;         contains a domain clock.  Each entry carries a USAGE tag:
;;         "assigned" (LHS of <=) or "referenced" (read in expression).
;;         Only "assigned"-in-one / any-entry-in-another creates a CDC issue.
;;   B.2 - signal declaration (inline or preceding block comment) has
;;         "clk_dom:CLOCKNAME" or "clock_domain:CLOCKNAME"
;;   B.3 - signal connected to a port of an instance whose entity name
;;         matches a regexp defined in `vhdl-cdc-clk-domain'
;;
;; Block comment annotation (rules A.2, B.2):
;;   A block is a pure-comment line followed by consecutive signal/port
;;   declaration lines ended by a blank line.  A keyword in the leading
;;   comment line applies to every signal/port in the block.
;;
;; Usage:
;;   M-x vhdl-cdc-analyze   (run in a VHDL buffer)
;;
;; Configuration variables:
;;   vhdl-cdc-clock      - extra clock signal names
;;   vhdl-cdc-clk-domain - instance-port domain rules (entity name is a regexp)
;;   vhdl-cdc-ignore     - signals excluded from CDC violation reporting

(require 'cl-lib)

;;; ---------------------------------------------------------------------------
;;; User-configurable variables
;;; ---------------------------------------------------------------------------

(defvar vhdl-cdc-clock nil
  "List of additional clock signal names (rule A.3).
Each element is a string.
Example:
  (setq vhdl-cdc-clock \\='(\"sys_clk\" \"ref_clk\"))")

(defvar vhdl-cdc-clk-domain nil
  "Rules for determining signal clock domain via instance port connections (rule B.3).
Each element is a list (ENTITY-REGEXP PORT CLOCK-PORT).
ENTITY-REGEXP is a regexp (case-insensitive) matched against the component/
entity name of each instantiation.  Use `regexp-quote' for a literal name.
When a signal is connected to PORT of an instance whose entity name matches
ENTITY-REGEXP, the signal's clock domain is the clock connected to
CLOCK-PORT of the same instance.
Example:
  (setq vhdl-cdc-clk-domain
        \\='((\"sync_ff\"      \"d\"       \"clk\")   ; exact (no metacharacters)
          (\"cdc_fifo.*\"    \"wr_data\"  \"wr_clk\")  ; any entity starting with cdc_fifo
          (\"\\\\`sync_.*\\\\'\" \"d\"       \"clk\")))  ; anchored regexp))")

(defvar vhdl-cdc-ignore nil
  "Signals excluded from CDC violation reporting.
Each element is a plist with :name (string) and :rationale (string).
Example:
  (setq vhdl-cdc-ignore
        \\='((:name \"reset_sync\"  :rationale \"synchronized externally\")
          (:name \"gray_counter\" :rationale \"gray-coded, safe to cross\")))")

;;; ---------------------------------------------------------------------------
;;; Internal helpers
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--inline-comment (line)
  "Return the text following \"--\" in LINE, or nil."
  (when (string-match "--[ \t]*\\(.*\\)$" line)
    (match-string 1 line)))

(defun vhdl-cdc--clock-method (name comment)
  "Return the clock-detection method string for NAME/COMMENT, or nil.
Returns \"A.1\", \"A.2\", or \"A.3\" for the first matching rule.
Rule A.2 matches comments containing \"dom_clk:\" or \"domain_clock:\"."
  (cond
   ((string-match-p "clk\\'" (downcase name))                            "A.1")
   ((and comment
         (string-match-p "\\(?:dom_clk\\|domain_clock\\):" comment))    "A.2")
   ((member name vhdl-cdc-clock)                                         "A.3")
   (t nil)))

(defun vhdl-cdc--ignored-p (name)
  "Return non-nil when NAME is listed in `vhdl-cdc-ignore'."
  (cl-some (lambda (e) (string-equal (plist-get e :name) name))
           vhdl-cdc-ignore))

(defun vhdl-cdc--ignore-rationale (name)
  "Return the :rationale string for NAME in `vhdl-cdc-ignore', or nil."
  (let ((entry (cl-find name vhdl-cdc-ignore
                        :key (lambda (e) (plist-get e :name))
                        :test #'string-equal)))
    (when entry (plist-get entry :rationale))))

(defun vhdl-cdc--parse-id-list (str)
  "Return a list of VHDL identifiers found in STR."
  (let ((ids  '())
        (pos  0))
    (while (string-match "\\b\\([a-zA-Z][a-zA-Z0-9_]*\\)\\b" str pos)
      (push (match-string 1 str) ids)
      (setq pos (match-end 0)))
    (nreverse ids)))

(defun vhdl-cdc--extract-clk-dom (comment)
  "Return the clock name from a \"clk_dom:\" or \"clock_domain:\" annotation.
COMMENT is a string (inline or block comment text).  Returns nil when
no annotation is present."
  (when (and comment
             (string-match
              "\\(?:clk_dom\\|clock_domain\\):\\([a-zA-Z][a-zA-Z0-9_]*\\)"
              comment))
    (match-string 1 comment)))

(defun vhdl-cdc--record-usage (domains sig clk method line usage)
  "Record SIG belonging to CLK domain in DOMAINS hash table.
Each entry has the form (CLK METHOD LINE USAGE).
USAGE is \"assigned\" or \"referenced\".
\"assigned\" takes priority over \"referenced\" for the same (sig clk) pair:
if an existing entry for the same CLK has USAGE \"referenced\" and the new
entry has \"assigned\", the old entry is replaced."
  (let* ((current  (gethash sig domains))
         (existing (cl-find clk current :key #'car :test #'string-equal)))
    (cond
     ((null existing)
      (puthash sig (cons (list clk method line usage) current) domains))
     ((and (string-equal (nth 3 existing) "referenced")
           (string-equal usage "assigned"))
      ;; Upgrade referenced → assigned
      (puthash sig
               (cons (list clk method line usage)
                     (cl-remove existing current :test #'eq))
               domains)))))

;;; ---------------------------------------------------------------------------
;;; Declaration scanner (signals and ports)
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--scan-declarations (buf)
  "Scan BUF for signal and port declarations.
Returns a list of plists, each with:
  :name          - identifier string
  :kind          - \\='signal or \\='port
  :line          - line number (integer)
  :comment       - inline comment string or nil
  :block-comment - leading block-comment text applying to this decl, or nil

Block comment: a pure-comment line (or the last of consecutive comment lines)
immediately preceding a run of declarations, reset by a blank line."
  (let ((decls         '())
        (block-comment nil)   ; comment from the line preceding the current block
        (in-decl-block nil))  ; t while inside a consecutive run of declarations
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((text    (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                 (linenum (line-number-at-pos))
                 (comment (vhdl-cdc--inline-comment text))
                 name kind)
            (cond
             ;; Blank line – reset block context
             ((string-match-p "^[ \t]*$" text)
              (setq block-comment nil
                    in-decl-block nil))
             ;; Pure comment line (no declaration on it)
             ((string-match-p "^[ \t]*--" text)
              ;; Only update block-comment while not yet inside a decl run
              (unless in-decl-block
                (setq block-comment comment)))
             (t
              ;; Try to match a declaration
              ;; signal declaration:  signal NAME :
              (when (string-match
                     "^[ \t]*signal[ \t]+\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t]*:"
                     text)
                (setq name (match-string 1 text)
                      kind 'signal))
              ;; port declaration:  NAME : in/out/inout/buffer TYPE
              ;; (exclude port-map connections that contain "=>")
              (when (and (not name)
                         (not (string-match "=>" text))
                         (string-match
                          "^[ \t]*\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t]*:[ \t]*\
\\(?:in\\|out\\|inout\\|buffer\\)[ \t]"
                          (concat (downcase text) " ")))
                ;; re-run without downcase to get correct casing
                (when (string-match
                       "^[ \t]*\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t]*:" text)
                  (setq name (match-string 1 text)
                        kind 'port)))
              (if name
                  (progn
                    (setq in-decl-block t)
                    (push (list :name name :kind kind :line linenum
                                :comment comment
                                :block-comment block-comment)
                          decls))
                ;; Non-blank, non-comment, non-declaration line
                (setq block-comment nil
                      in-decl-block nil)))))
          (forward-line 1))))
    (nreverse decls)))

;;; ---------------------------------------------------------------------------
;;; Phase A  --  identify domain clocks
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--find-clocks (decls)
  "Given DECLS (from `vhdl-cdc--scan-declarations'), return clock entries.
Each entry is a plist: :name :line :method.
Rule A.2 checks both the inline comment and the preceding block comment."
  (let ((clocks '()))
    (dolist (d decls)
      (let* ((name    (plist-get d :name))
             (comment (plist-get d :comment))
             (blk     (plist-get d :block-comment))
             ;; Inline comment has priority; fall back to block comment
             (method  (or (vhdl-cdc--clock-method name comment)
                          (vhdl-cdc--clock-method name blk))))
        (when method
          (push (list :name name :line (plist-get d :line) :method method)
                clocks))))
    (nreverse clocks)))

;;; ---------------------------------------------------------------------------
;;; Phase B.1  --  process sensitivity-list analysis
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--find-process-blocks (buf)
  "Return list of process block plists in BUF.
Each plist has:
  :sens-list  - list of identifiers in sensitivity list
  :body-start - buffer position after sensitivity list closing paren
  :body-end   - buffer position of \"end process\" keyword"
  (let ((blocks '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "\\bprocess[ \t\n]*(\\([^)]*\\))[ \t\n]*\\(is\\b\\)?"
                nil t)
          (let* ((sens-str    (match-string 1))
                 (body-start  (match-end 0))
                 (sens-list   (vhdl-cdc--parse-id-list sens-str))
                 body-end)
            (save-excursion
              (when (re-search-forward "\\bend[ \t\n]+process\\b" nil t)
                (setq body-end (match-beginning 0))))
            (when body-end
              (push (list :sens-list  sens-list
                          :body-start body-start
                          :body-end   body-end)
                    blocks))))))
    (nreverse blocks)))

(defun vhdl-cdc--assignments-in-region (buf start end)
  "Return list of (NAME . LINE) for signal assignments in BUF from START to END."
  (let ((result '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char start)
        (while (and (< (point) end)
                    (re-search-forward
                     "^[ \t]*\\([a-zA-Z][a-zA-Z0-9_]*\\)\
\\(?:[ \t]*([ \t]*[^)]*[ \t]*)\\)?[ \t]*<="
                     end t))
          (push (cons (match-string-no-properties 1) (line-number-at-pos))
                result))))
    (nreverse result)))

(defun vhdl-cdc--references-in-region (buf start end decl-names exclude-names)
  "Return list of (NAME . LINE) for signal/port reads in BUF from START to END.
Only names present in DECL-NAMES (and absent from EXCLUDE-NAMES) are returned.
Names that appear as the LHS of a signal assignment on the same line are not
counted as references from that line (they are handled by
`vhdl-cdc--assignments-in-region').
Results are deduplicated by (name . line)."
  (let ((assign-re "^[ \t]*\\([a-zA-Z][a-zA-Z0-9_]*\\)\
\\(?:[ \t]*([ \t]*[^)]*[ \t]*)\\)?[ \t]*<=")
        (id-re     "\\b\\([a-zA-Z][a-zA-Z0-9_]*\\)\\b")
        (result    '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let* ((text    (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                 (linenum (line-number-at-pos))
                 ;; Strip inline comment
                 (code    (if (string-match "--" text)
                              (substring text 0 (match-beginning 0))
                            text))
                 ;; LHS identifier when this line is an assignment
                 (lhs     (when (string-match assign-re code)
                            (match-string 1 code)))
                 ;; Scan: RHS of assignment, or whole line if not an assignment
                 (scan    (if (and lhs (string-match "<=" code))
                              (substring code (match-end 0))
                            code))
                 (pos     0))
            (while (string-match id-re scan pos)
              (let ((id (match-string 1 scan)))
                (when (and (member id decl-names)
                           (not (member id exclude-names)))
                  (push (cons id linenum) result)))
              (setq pos (match-end 0))))
          (forward-line 1))))
    (cl-remove-duplicates
     (nreverse result)
     :test (lambda (a b) (and (string-equal (car a) (car b))
                              (= (cdr a) (cdr b)))))))

(defun vhdl-cdc--domains-from-processes (buf clock-names decl-names)
  "Rule B.1: return domain hash-table from process analysis in BUF.
CLOCK-NAMES is a list of known clock name strings.
DECL-NAMES is a list of all declared signal/port name strings used for
reference scanning.  Clock names are excluded from reference results.
Each entry has the form (CLK \"B.1\" LINE USAGE) where USAGE is
\"assigned\" (LHS of <=) or \"referenced\" (read in an expression).
\"assigned\" takes priority over \"referenced\" for the same (sig clk) pair."
  (let ((domains (make-hash-table :test 'equal)))
    (dolist (block (vhdl-cdc--find-process-blocks buf))
      (let* ((sens      (plist-get block :sens-list))
             (clks-here (cl-intersection sens clock-names :test #'string-equal))
             (bstart    (plist-get block :body-start))
             (bend      (plist-get block :body-end)))
        (when clks-here
          (let ((assigns (vhdl-cdc--assignments-in-region buf bstart bend))
                (refs    (vhdl-cdc--references-in-region  buf bstart bend
                                                           decl-names
                                                           clock-names)))
            ;; Record assignments first (they take priority)
            (dolist (assign assigns)
              (let ((sig  (car assign))
                    (line (cdr assign)))
                (dolist (clk clks-here)
                  (vhdl-cdc--record-usage domains sig clk "B.1" line "assigned"))))
            ;; Record references (only added if no "assigned" entry for same clk)
            (dolist (ref refs)
              (let ((sig  (car ref))
                    (line (cdr ref)))
                (dolist (clk clks-here)
                  (vhdl-cdc--record-usage domains sig clk "B.1" line "referenced"))))))))
    domains))

;;; ---------------------------------------------------------------------------
;;; Phase B.2  --  declaration comment  clk_dom:/clock_domain:CLOCK
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--domains-from-comments (decls clock-names)
  "Rule B.2: return domain alist from declaration comments.
Checks both the inline comment and the preceding block comment for
\"clk_dom:CLOCK\" or \"clock_domain:CLOCK\" annotations.
DECLS is from `vhdl-cdc--scan-declarations'.
CLOCK-NAMES is the list of known clock name strings."
  (let ((domains (make-hash-table :test 'equal)))
    (dolist (d decls)
      (let* ((comment (plist-get d :comment))
             (blk     (plist-get d :block-comment))
             ;; Inline comment has priority; fall back to block comment
             (clk     (or (vhdl-cdc--extract-clk-dom comment)
                          (vhdl-cdc--extract-clk-dom blk))))
        (when (and clk (member clk clock-names))
          (let* ((name    (plist-get d :name))
                 (line    (plist-get d :line))
                 (current (gethash name domains)))
            (unless (cl-find clk current :key #'car :test #'string-equal)
              (puthash name
                       (cons (list clk "B.2" line "assigned") current)
                       domains))))))
    domains))

;;; ---------------------------------------------------------------------------
;;; Phase B.3  --  instance port connection rules
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--parse-portmap-connections (portmap-str)
  "Return alist ((PORT . SIGNAL) ...) from a port-map body string PORTMAP-STR."
  (let ((conns '())
        (pos   0))
    (while (string-match
            "\\b\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t\n]*=>[ \t\n]*\
\\([a-zA-Z][a-zA-Z0-9_]*\\)"
            portmap-str pos)
      (push (cons (downcase (match-string 1 portmap-str))
                  (match-string 2 portmap-str))
            conns)
      (setq pos (match-end 0)))
    (nreverse conns)))

(defun vhdl-cdc--domains-from-instances (buf clock-names)
  "Rule B.3: return domain alist from instance port connections in BUF.
CLOCK-NAMES is the list of known clock name strings."
  (let ((domains (make-hash-table :test 'equal)))
    (when vhdl-cdc-clk-domain
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          ;; Match both  "LABEL : ENTITY port map ("
          ;; and         "LABEL : entity work.ENTITY port map ("
          (while (re-search-forward
                  ":\\(?:[ \t\n]+entity[ \t\n]+\\(?:[a-zA-Z][a-zA-Z0-9_.]*\\.\\)?\\)?\
[ \t\n]*\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t\n]+port[ \t\n]+map[ \t\n]*("
                  nil t)
            (let* ((entity-name  (match-string-no-properties 1))
                   (paren-open   (1- (point)))  ; position of the "("
                   (portmap-str  nil)
                   (portmap-line (line-number-at-pos)))
              ;; Extract contents between the outer parentheses
              (save-excursion
                (goto-char paren-open)
                (condition-case nil
                    (progn
                      (forward-sexp 1)
                      (setq portmap-str
                            (buffer-substring-no-properties
                             (1+ paren-open) (1- (point)))))
                  (error nil)))
              (when portmap-str
                (let ((conns (vhdl-cdc--parse-portmap-connections portmap-str)))
                  (dolist (rule vhdl-cdc-clk-domain)
                    (let* ((rule-entity   (nth 0 rule))
                           (rule-port     (downcase (nth 1 rule)))
                           (rule-clk-port (downcase (nth 2 rule))))
                      (when (string-match-p (downcase rule-entity)
                                              (downcase entity-name))
                        (let ((data-sig (cdr (assoc rule-port conns)))
                              (clk-sig  (cdr (assoc rule-clk-port conns))))
                          (when (and data-sig clk-sig
                                     (member clk-sig clock-names))
                            (vhdl-cdc--record-usage domains data-sig clk-sig
                                                    "B.3" portmap-line
                                                    "assigned")))))))))))))
    domains))

;;; ---------------------------------------------------------------------------
;;; Merge domain hash tables
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--merge-domains (&rest tables)
  "Merge multiple domain hash-tables into one.
Each table maps SIG-NAME -> ((CLK METHOD LINE USAGE) ...).
Duplicate (SIG . CLK) pairs are deduplicated; \"assigned\" takes priority
over \"referenced\" for the same pair (via `vhdl-cdc--record-usage')."
  (let ((merged (make-hash-table :test 'equal)))
    (dolist (tbl tables)
      (maphash
       (lambda (sig entries)
         (dolist (entry entries)
           (vhdl-cdc--record-usage merged sig
                                   (nth 0 entry) (nth 1 entry) (nth 2 entry)
                                   (or (nth 3 entry) "assigned"))))
       tbl))
    merged))

;;; ---------------------------------------------------------------------------
;;; Output formatting
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--format-output (source-name clocks domains decls)
  "Return a formatted string for the CDC analysis report.
SOURCE-NAME is the file/buffer name.
CLOCKS is a list of plists (:name :line :method).
DOMAINS maps signal-name -> ((CLK METHOD LINE USAGE) ...).
DECLS is the full list of signal/port declaration plists."
  (with-temp-buffer
    (insert (format "VHDL Clock Domain Analysis\n"))
    (insert (format "==========================\n"))
    (insert (format "Source: %s\n\n" source-name))

    ;; Section 1 -- Domain Clocks
    (insert "Domain Clocks\n")
    (insert "-------------\n")
    (if clocks
        (dolist (c clocks)
          (insert (format "  %-30s  line %4d  [%s]\n"
                          (plist-get c :name)
                          (plist-get c :line)
                          (plist-get c :method))))
      (insert "  (none found)\n"))
    (insert "\n")

    ;; Section 2 -- Signals grouped by clock domain
    (let ((clock-names (mapcar (lambda (c) (plist-get c :name)) clocks)))
      (dolist (clk-name clock-names)
        (insert (format "Clock Domain: %s\n" clk-name))
        (insert (make-string (+ 14 (length clk-name)) ?-))
        (insert "\n")
        (let ((sigs '()))
          (maphash
           (lambda (sig entries)
             (dolist (e entries)
               (when (string-equal (car e) clk-name)
                 (push (list sig (nth 1 e) (nth 2 e)
                             (or (nth 3 e) "assigned"))
                       sigs))))
           domains)
          (if sigs
              (dolist (s (sort sigs (lambda (a b) (string< (car a) (car b)))))
                (insert (format "  %-30s  line %4d  [%s]  %s\n"
                                (nth 0 s) (nth 2 s) (nth 1 s) (nth 3 s))))
            (insert "  (no signals found)\n")))
        (insert "\n"))

      ;; Section 3 -- Unknown clock domain
      ;; Signals/ports declared but with no clock domain determined,
      ;; and not themselves a domain clock.
      (insert "Unknown Clock Domain\n")
      (insert "--------------------\n")
      (let ((unknown-sigs '()))
        (dolist (d decls)
          (let ((name (plist-get d :name)))
            (when (and (not (member name clock-names))
                       (not (gethash name domains)))
              (push (list name (plist-get d :line)) unknown-sigs))))
        (if unknown-sigs
            (dolist (s (sort unknown-sigs (lambda (a b) (string< (car a) (car b)))))
              (insert (format "  %-30s  line %4d\n" (nth 0 s) (nth 1 s))))
          (insert "  (none)\n")))
      (insert "\n")

      ;; Helper: does a signal have at least one "assigned" entry?
      (cl-flet ((has-assigned-p (entries)
                  (cl-some (lambda (e)
                             (string-equal (or (nth 3 e) "assigned") "assigned"))
                           entries))
                ;; CDC = multiple distinct clock domains AND at least one assigned
                (is-cdc-p (entries)
                  (and (> (length entries) 1)
                       (cl-some (lambda (e)
                                  (string-equal (or (nth 3 e) "assigned") "assigned"))
                                entries))))

        ;; Section 4 -- Ignored CDC signals (multi-domain but in vhdl-cdc-ignore)
        (insert "Ignored CDC Signals\n")
        (insert "-------------------\n")
        (let ((ignored-sigs '()))
          (maphash
           (lambda (sig entries)
             (when (and (is-cdc-p entries)
                        (vhdl-cdc--ignored-p sig))
               (push (cons sig entries) ignored-sigs)))
           domains)
          (if ignored-sigs
              (dolist (item (sort ignored-sigs
                                  (lambda (a b) (string< (car a) (car b)))))
                (let* ((sig-name (car item))
                       (entries  (cdr item)))
                  (insert (format "  %s\n" sig-name))
                  (let ((rationale (vhdl-cdc--ignore-rationale sig-name)))
                    (when rationale
                      (insert (format "    rationale: %s\n" rationale))))
                  (dolist (e (sort (copy-sequence entries)
                                   (lambda (a b) (string< (car a) (car b)))))
                    (insert (format "    domain %-20s  line %4d  [%s]  %s\n"
                                    (nth 0 e) (nth 2 e) (nth 1 e)
                                    (or (nth 3 e) "assigned"))))))
            (insert "  (none)\n")))
        (insert "\n")

        ;; Section 5 -- CDC violations (non-ignored multi-domain with >=1 assigned)
        (insert "CDC Signals (Multiple Clock Domains)\n")
        (insert "-------------------------------------\n")
        (let ((cdc-sigs '()))
          (maphash
           (lambda (sig entries)
             (when (and (is-cdc-p entries)
                        (not (vhdl-cdc--ignored-p sig)))
               (push (cons sig entries) cdc-sigs)))
           domains)
          (if cdc-sigs
              (dolist (item (sort cdc-sigs
                                  (lambda (a b) (string< (car a) (car b)))))
                (let* ((sig-name (car item))
                       (entries  (cdr item)))
                  (insert (format "*** %s\n" sig-name))
                  (dolist (e (sort (copy-sequence entries)
                                   (lambda (a b) (string< (car a) (car b)))))
                    (insert (format "       domain %-20s  line %4d  [%s]  %s\n"
                                    (nth 0 e) (nth 2 e) (nth 1 e)
                                    (or (nth 3 e) "assigned"))))))
            (insert "  (none found)\n")))))
    (buffer-string)))

;;; ---------------------------------------------------------------------------
;;; Main entry point
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun vhdl-cdc-analyze ()
  "Analyze the current VHDL buffer for clock domain crossings.

Identifies domain clocks using:
  A.1 - signal/port name ends with \\\"clk\\\"
  A.2 - inline comment (or preceding block comment) contains
        \\\"dom_clk:\\\" or \\\"domain_clock:\\\"
  A.3 - name is in variable `vhdl-cdc-clock'

Assigns signals to clock domains using:
  B.1 - signal assigned or referenced in a process whose sensitivity list
        has a domain clock.  Each entry is tagged \"assigned\" or
        \"referenced\".  A signal referenced in one domain and assigned in
        another is a CDC violation.
  B.2 - declaration comment (inline or preceding block comment) has
        \\\"clk_dom:CLOCKNAME\\\" or \\\"clock_domain:CLOCKNAME\\\"
  B.3 - signal connected to a port whose entity name matches a regexp
        specified in `vhdl-cdc-clk-domain'

Block comment annotation: a keyword in the leading comment line of a
declaration block (comment line(s) then declarations then blank line)
applies the keyword to every signal/port in that block.

Output is displayed in the *VHDL CDC Analysis* buffer.
The domain group listings show an \"assigned\" or \"referenced\" column.
Signals in `vhdl-cdc-ignore' that cross domains are listed in a
separate \\\"Ignored CDC Signals\\\" section before the CDC violations.

Configuration:
  `vhdl-cdc-clock'      - extra clock signal names (rule A.3)
  `vhdl-cdc-clk-domain' - instance-port clock-domain rules (rule B.3)
  `vhdl-cdc-ignore'     - signals to exclude from CDC violation reporting"
  (interactive)
  (let* ((src-buf     (current-buffer))
         (source-name (or (buffer-file-name) (buffer-name)))
         ;; Phase A: declarations & clocks
         (decls       (vhdl-cdc--scan-declarations src-buf))
         (clocks      (vhdl-cdc--find-clocks decls))
         (clock-names (mapcar (lambda (c) (plist-get c :name)) clocks))
         (decl-names  (mapcar (lambda (d) (plist-get d :name)) decls))
         ;; Phase B: signal domain assignment
         (dom-b1      (vhdl-cdc--domains-from-processes src-buf clock-names
                                                        decl-names))
         (dom-b2      (vhdl-cdc--domains-from-comments  decls clock-names))
         (dom-b3      (vhdl-cdc--domains-from-instances src-buf clock-names))
         ;; Merge all domain tables
         (domains     (vhdl-cdc--merge-domains dom-b1 dom-b2 dom-b3))
         ;; Format and display
         (output      (vhdl-cdc--format-output source-name clocks domains decls))
         (out-buf     (get-buffer-create "*VHDL CDC Analysis*")))
    (with-current-buffer out-buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (display-buffer out-buf)
    (message "CDC analysis complete. See *VHDL CDC Analysis* buffer.")))

(provide 'vhdl-cdc)
