;;
;; vhdl-cdc.el - VHDL Clock Domain Crossing (CDC) Analysis
;; T. Isogai 2024
;;
;; Analyze a VHDL file (buffer) to group signals and ports by clock domain.
;;
;; Clock domain detection rules:
;;   A.1 - signal/port name ends with "clk" (case-insensitive)
;;   A.2 - signal/port inline comment contains "dom_clk:"
;;   A.3 - signal/port name is listed in `vhdl-cdc-clock'
;;
;; Signal clock-domain assignment rules:
;;   B.1 - signal assigned in a process whose sensitivity list contains a
;;         domain clock
;;   B.2 - signal declaration has "clk_dom:CLOCKNAME" in its inline comment
;;   B.3 - signal connected to a pre-specified entity/port pair defined in
;;         `vhdl-cdc-clk-domain'
;;
;; Usage:
;;   M-x vhdl-cdc-analyze   (run in a VHDL buffer)
;;
;; Configuration variables:
;;   vhdl-cdc-clock      - extra clock signal names
;;   vhdl-cdc-clk-domain - instance-port domain rules
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
Each element is a list (ENTITY PORT CLOCK-PORT).
When a signal is connected to PORT of an instance of ENTITY, the signal's
clock domain is the clock connected to CLOCK-PORT of the same instance.
Example:
  (setq vhdl-cdc-clk-domain
        \\='((\"sync_ff\"  \"d\"       \"clk\")
          (\"cdc_fifo\"  \"wr_data\"  \"wr_clk\")))")

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
Returns \"A.1\", \"A.2\", or \"A.3\" for the first matching rule."
  (cond
   ((string-match-p "clk\\'" (downcase name))          "A.1")
   ((and comment (string-match-p "dom_clk:" comment))  "A.2")
   ((member name vhdl-cdc-clock)                       "A.3")
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

;;; ---------------------------------------------------------------------------
;;; Declaration scanner (signals and ports)
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--scan-declarations (buf)
  "Scan BUF for signal and port declarations.
Returns a list of plists, each with:
  :name    - identifier string
  :kind    - \\='signal or \\='port
  :line    - line number (integer)
  :comment - inline comment string or nil"
  (let ((decls '()))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((text    (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                 (linenum (line-number-at-pos))
                 (comment (vhdl-cdc--inline-comment text))
                 name kind)
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
            (when name
              (push (list :name name :kind kind :line linenum :comment comment)
                    decls)))
          (forward-line 1))))
    (nreverse decls)))

;;; ---------------------------------------------------------------------------
;;; Phase A  --  identify domain clocks
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--find-clocks (decls)
  "Given DECLS (from `vhdl-cdc--scan-declarations'), return clock entries.
Each entry is a plist: :name :line :method."
  (let ((clocks '()))
    (dolist (d decls)
      (let* ((name    (plist-get d :name))
             (comment (plist-get d :comment))
             (method  (vhdl-cdc--clock-method name comment)))
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

(defun vhdl-cdc--domains-from-processes (buf clock-names)
  "Rule B.1: return domain alist from process analysis in BUF.
CLOCK-NAMES is a list of known clock name strings.
Returns an alist: ((SIG-NAME . ((CLK-NAME METHOD LINE) ...)) ...)"
  (let ((domains (make-hash-table :test 'equal)))
    (dolist (block (vhdl-cdc--find-process-blocks buf))
      (let* ((sens      (plist-get block :sens-list))
             (clks-here (cl-intersection sens clock-names :test #'string-equal))
             (bstart    (plist-get block :body-start))
             (bend      (plist-get block :body-end)))
        (when clks-here
          (dolist (assign (vhdl-cdc--assignments-in-region buf bstart bend))
            (let ((sig  (car assign))
                  (line (cdr assign)))
              (dolist (clk clks-here)
                (let ((current (gethash sig domains)))
                  (unless (cl-find clk current :key #'car :test #'string-equal)
                    (puthash sig
                             (cons (list clk "B.1" line) current)
                             domains)))))))))
    domains))

;;; ---------------------------------------------------------------------------
;;; Phase B.2  --  declaration comment  clk_dom:CLOCK
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--domains-from-comments (decls clock-names)
  "Rule B.2: return domain alist from declaration comments.
DECLS is from `vhdl-cdc--scan-declarations'.
CLOCK-NAMES is the list of known clock name strings."
  (let ((domains (make-hash-table :test 'equal)))
    (dolist (d decls)
      (let ((comment (plist-get d :comment)))
        (when (and comment
                   (string-match "clk_dom:\\([a-zA-Z][a-zA-Z0-9_]*\\)" comment))
          (let* ((clk  (match-string 1 comment))
                 (name (plist-get d :name))
                 (line (plist-get d :line)))
            (when (member clk clock-names)
              (let ((current (gethash name domains)))
                (unless (cl-find clk current :key #'car :test #'string-equal)
                  (puthash name
                           (cons (list clk "B.2" line) current)
                           domains))))))))
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
                      (when (string-equal (downcase entity-name)
                                          (downcase rule-entity))
                        (let ((data-sig (cdr (assoc rule-port conns)))
                              (clk-sig  (cdr (assoc rule-clk-port conns))))
                          (when (and data-sig clk-sig
                                     (member clk-sig clock-names))
                            (let ((current (gethash data-sig domains)))
                              (unless (cl-find clk-sig current
                                               :key #'car :test #'string-equal)
                                (puthash data-sig
                                         (cons (list clk-sig "B.3" portmap-line)
                                               current)
                                         domains)))))))))))))))
    domains))

;;; ---------------------------------------------------------------------------
;;; Merge domain hash tables
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--merge-domains (&rest tables)
  "Merge multiple domain hash-tables into one.
Each table maps SIG-NAME -> ((CLK METHOD LINE) ...).
Duplicate (SIG . CLK) pairs are deduplicated."
  (let ((merged (make-hash-table :test 'equal)))
    (dolist (tbl tables)
      (maphash
       (lambda (sig entries)
         (dolist (entry entries)
           (let* ((clk     (car entry))
                  (current (gethash sig merged)))
             (unless (cl-find clk current :key #'car :test #'string-equal)
               (puthash sig (cons entry current) merged)))))
       tbl))
    merged))

;;; ---------------------------------------------------------------------------
;;; Output formatting
;;; ---------------------------------------------------------------------------

(defun vhdl-cdc--format-output (source-name clocks domains)
  "Return a formatted string for the CDC analysis report.
SOURCE-NAME is the file/buffer name.
CLOCKS is a list of plists (:name :line :method).
DOMAINS maps signal-name -> ((CLK METHOD LINE) ...)."
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
                 (push (list sig (nth 1 e) (nth 2 e)) sigs))))
           domains)
          (if sigs
              (dolist (s (sort sigs (lambda (a b) (string< (car a) (car b)))))
                (insert (format "  %-30s  line %4d  [%s]\n"
                                (nth 0 s) (nth 2 s) (nth 1 s))))
            (insert "  (no signals found)\n")))
        (insert "\n"))

      ;; Section 3 -- CDC violations
      (insert "CDC Signals (Multiple Clock Domains)\n")
      (insert "-------------------------------------\n")
      (let ((cdc-sigs '()))
        (maphash
         (lambda (sig entries)
           (when (> (length entries) 1)
             (push (cons sig entries) cdc-sigs)))
         domains)
        (if cdc-sigs
            (dolist (item (sort cdc-sigs
                                (lambda (a b) (string< (car a) (car b)))))
              (let* ((sig-name (car item))
                     (entries  (cdr item))
                     (ignored  (vhdl-cdc--ignored-p sig-name))
                     (prefix   (if ignored "    " "*** ")))
                (insert (format "%s%s\n" prefix sig-name))
                (dolist (e (sort (copy-sequence entries)
                                 (lambda (a b) (string< (car a) (car b)))))
                  (insert (format "       domain %-20s  line %4d  [%s]\n"
                                  (nth 0 e) (nth 2 e) (nth 1 e))))
                (when ignored
                  (insert (format "       (ignored: %s)\n"
                                  (or (vhdl-cdc--ignore-rationale sig-name) ""))))))
          (insert "  (none found)\n"))))
    (buffer-string)))

;;; ---------------------------------------------------------------------------
;;; Main entry point
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun vhdl-cdc-analyze ()
  "Analyze the current VHDL buffer for clock domain crossings.

Identifies domain clocks using:
  A.1 - signal/port name ends with \\\"clk\\\"
  A.2 - inline comment contains \\\"dom_clk:\\\"
  A.3 - name is in variable `vhdl-cdc-clock'

Assigns signals to clock domains using:
  B.1 - signal assigned in a process whose sensitivity list has a domain clock
  B.2 - declaration comment has \\\"clk_dom:CLOCKNAME\\\"
  B.3 - signal connected to a port specified in `vhdl-cdc-clk-domain'

Output is displayed in the *VHDL CDC Analysis* buffer.
Signals belonging to multiple clock domains are listed with \\\"***\\\",
unless they appear in `vhdl-cdc-ignore'.

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
         ;; Phase B: signal domain assignment
         (dom-b1      (vhdl-cdc--domains-from-processes src-buf clock-names))
         (dom-b2      (vhdl-cdc--domains-from-comments  decls clock-names))
         (dom-b3      (vhdl-cdc--domains-from-instances src-buf clock-names))
         ;; Merge all domain tables
         (domains     (vhdl-cdc--merge-domains dom-b1 dom-b2 dom-b3))
         ;; Format and display
         (output      (vhdl-cdc--format-output source-name clocks domains))
         (out-buf     (get-buffer-create "*VHDL CDC Analysis*")))
    (with-current-buffer out-buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (display-buffer out-buf)
    (message "CDC analysis complete. See *VHDL CDC Analysis* buffer.")))

(provide 'vhdl-cdc)
