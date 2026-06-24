;;
;; verilog-to-vhdl-inst.el
;; Parse a Verilog module declaration and generate a VHDL component instance.
;; Supports ANSI style and split (non-ANSI) style declarations.
;; T. Isogai
;;
;; Usage:
;;   M-x vhdl-instance-from-verilog-region  -- parse region, insert instance at point
;;   M-x vhdl-instance-from-verilog-buffer  -- parse whole buffer, insert instance at point
;;   M-x vhdl-instance-from-verilog-file    -- pick a .v file, insert instance at point
;;
;; ANSI example:
;;   module fifo #(parameter DEPTH=16, DATA_W=8) (
;;     input              clk, rst_n,
;;     input  [DATA_W-1:0] wr_data,
;;     output [DATA_W-1:0] rd_data,
;;     output              empty, full);
;;
;; Split (non-ANSI) example:
;;   module fifo (clk, rst_n, wr_data, rd_data, empty, full);
;;     parameter DEPTH = 16;
;;     parameter DATA_W = 8;
;;     input              clk, rst_n;
;;     input  [DATA_W-1:0] wr_data;
;;     output [DATA_W-1:0] rd_data;
;;     output             empty, full;


;;; ----------------------------------------------------------------
;;; Internal helpers
;;; ----------------------------------------------------------------

(defun vtv--strip-comments (text)
  "Remove // line comments and /* */ block comments from Verilog TEXT."
  (with-temp-buffer
    (insert text)
    ;; block comments
    (goto-char (point-min))
    (while (re-search-forward "/\\*" nil t)
      (let ((s (match-beginning 0)))
        (if (re-search-forward "\\*/" nil t)
            (delete-region s (point))
          (delete-region s (point-max)))))
    ;; line comments
    (goto-char (point-min))
    (while (re-search-forward "//[^\n]*" nil t)
      (replace-match ""))
    (buffer-string)))


(defun vtv--extract-balanced (text open-ch close-ch)
  "Return (inner-text . rest) where INNER-TEXT is inside the first
balanced pair of OPEN-CH/CLOSE-CH in TEXT, and REST is everything
after the closing character.  Returns nil if no opening char found."
  (let ((i 0) (len (length text)) start depth)
    ;; skip leading whitespace then find open-ch
    (while (and (< i len) (not (eq (aref text i) open-ch)))
      (setq i (1+ i)))
    (unless (< i len) (cl-return-from vtv--extract-balanced nil))
    (setq i (1+ i))                    ; skip open-ch
    (setq start i depth 1)
    (while (and (< i len) (> depth 0))
      (let ((ch (aref text i)))
        (cond ((eq ch open-ch)  (setq depth (1+ depth)))
              ((eq ch close-ch) (setq depth (1- depth)))))
      (setq i (1+ i)))
    (cons (substring text start (1- i))   ; inner (before last close-ch)
          (substring text i))))            ; rest after close-ch


(defun vtv--split-commas (text)
  "Split TEXT at top-level commas (not inside () or []).
Returns list of trimmed, non-empty strings."
  (let (result current (depth 0))
    (dolist (ch (string-to-list text))
      (cond
       ((memq ch '(?\( ?\[)) (setq depth (1+ depth))
        (setq current (concat current (string ch))))
       ((memq ch '(?\) ?\])) (setq depth (1- depth))
        (setq current (concat current (string ch))))
       ((and (= ch ?,) (= depth 0))
        (let ((s (string-trim (or current ""))))
          (when (> (length s) 0) (push s result)))
        (setq current ""))
       (t (setq current (concat current (string ch))))))
    (let ((s (string-trim (or current ""))))
      (when (> (length s) 0) (push s result)))
    (nreverse result)))


(defun vtv--remove-type-keywords (text)
  "Strip Verilog net/reg/signing keywords from TEXT, return trimmed result."
  (replace-regexp-in-string
   "\\<\\(wire\\|reg\\|logic\\|tri\\|tri0\\|tri1\\|trireg\\|wand\\|wor\\
\\|supply0\\|supply1\\|signed\\|unsigned\\)\\>"
   "" text))


(defun vtv--parse-vector (text)
  "Parse first [MSB:LSB] in TEXT.  Returns (msb-str . lsb-str) or nil."
  (when (string-match "\\[\\s-*\\([^]:]+?\\)\\s-*:\\s-*\\([^]]+?\\)\\s-*\\]" text)
    (cons (string-trim (match-string 1 text))
          (string-trim (match-string 2 text)))))


;;; ----------------------------------------------------------------
;;; ANSI port list parser
;;; ----------------------------------------------------------------

(defun vtv--parse-ansi-ports (port-text)
  "Parse ANSI-style port list PORT-TEXT (content inside the module's outer parens).
Direction and vector size are inherited across comma-separated names that share
the same declaration (e.g. 'output [7:0] a, b').  A new explicit direction
resets the inherited vector.
Returns list of (direction name msb lsb)."
  (let (ports cur-dir cur-vec)
    (dolist (entry (vtv--split-commas port-text))
      (setq entry (string-trim entry))
      (when (> (length entry) 0)
        (let (dir vec clean has-dir msb lsb)
          ;; Detect explicit direction keyword
          (setq has-dir (string-match "\\<\\(input\\|output\\|inout\\)\\>" entry))
          (if has-dir
              (progn
                (setq dir (match-string 1 entry))
                (setq clean (concat (substring entry 0 (match-beginning 0))
                                    (substring entry (match-end 0)))))
            (setq dir cur-dir)
            (setq clean entry))
          ;; Strip net/reg/signing keywords
          (setq clean (vtv--remove-type-keywords clean))
          ;; Extract vector [msb:lsb]
          (if (string-match "\\(\\[[^]]+\\]\\)" clean)
              (progn
                (setq vec (vtv--parse-vector (match-string 1 clean)))
                (setq clean (replace-regexp-in-string "\\[[^]]+\\]" "" clean)))
            ;; No vector in this entry: inherit only when direction is also inherited
            (unless has-dir
              (setq vec cur-vec)))
          ;; Update running state when direction is explicit (resets inherited vec too)
          (when has-dir
            (setq cur-dir dir)
            (setq cur-vec vec))
          ;; Collect port names from remaining text
          (setq msb (car vec) lsb (cdr vec))
          (dolist (n (split-string (string-trim clean) "[,\t\n ]+" t))
            (when (string-match "^[a-zA-Z_][a-zA-Z0-9_$]*$" n)
              (push (list dir n msb lsb) ports))))))
    (nreverse ports)))


;;; ----------------------------------------------------------------
;;; Non-ANSI (split) port list parser
;;; ----------------------------------------------------------------

(defun vtv--parse-nonansi-ports (name-text body-text)
  "Parse non-ANSI ports.
NAME-TEXT is the port-name list inside the module declaration parens.
BODY-TEXT is the module body up to endmodule (contains direction declarations).
Returns list of (direction name msb lsb) in the order of NAME-TEXT."
  (let ((order (delq nil
                     (mapcar (lambda (s) (let ((t1 (string-trim s)))
                                           (when (string-match "^[a-zA-Z_][a-zA-Z0-9_$]*$" t1) t1)))
                             (split-string name-text "[,\t\n ]+" t))))
        decl-map)
    ;; Build map: port-name -> (direction msb lsb)
    (with-temp-buffer
      (insert body-text)
      (goto-char (point-min))
      (while (re-search-forward
              "\\<\\(input\\|output\\|inout\\)\\>\\([^;]+\\);"
              nil t)
        (let* ((dir   (match-string-no-properties 1))
               (rest  (match-string-no-properties 2))
               (vec   nil) msb lsb clean)
          (setq clean (vtv--remove-type-keywords rest))
          (when (string-match "\\(\\[[^]]+\\]\\)" clean)
            (setq vec (vtv--parse-vector (match-string 1 clean)))
            (setq clean (replace-regexp-in-string "\\[[^]]+\\]" "" clean)))
          (setq msb (car vec) lsb (cdr vec))
          (dolist (n (split-string (string-trim clean) "[,\t\n ]+" t))
            (when (string-match "^[a-zA-Z_][a-zA-Z0-9_$]*$" n)
              (push (cons n (list dir msb lsb)) decl-map))))))
    ;; Produce ordered port list
    (mapcar (lambda (pname)
              (let ((d (cdr (assoc pname decl-map))))
                (if d
                    (list (nth 0 d) pname (nth 1 d) (nth 2 d))
                  ;; Declaration not found - default to scalar input
                  (list "input" pname nil nil))))
            order)))


;;; ----------------------------------------------------------------
;;; Parameter parsers
;;; ----------------------------------------------------------------

(defun vtv--parse-params-from-text (text)
  "Parse 'parameter NAME = VALUE' items from TEXT.
TEXT may be a #() parameter block or a module body.
Returns list of (name default-value-string)."
  (let (params)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Remove 'parameter' / 'localparam' keywords then scan name=value
      (while (re-search-forward
              "\\<\\(?:parameter\\|localparam\\)\\>\\s-*\\(?:\\[[^]]*\\]\\s-*\\)?\\([a-zA-Z_][a-zA-Z0-9_$]*\\)\\s-*=\\s-*\\([^,;)\n]+\\)"
              nil t)
        (push (list (match-string-no-properties 1)
                    (string-trim (match-string-no-properties 2)))
              params)))
    (nreverse params)))


;;; ----------------------------------------------------------------
;;; Top-level module parser
;;; ----------------------------------------------------------------

(defun verilog-parse-module (text)
  "Parse a Verilog module declaration from TEXT.
Handles both ANSI style (ports declared inline) and non-ANSI/split style
(ports listed by name, declarations in the module body).

Returns (module-name params ports) where:
  module-name  - string
  params       - list of (name default-value-string)
  ports        - list of (direction name msb lsb)
                   direction is \"input\", \"output\", or \"inout\"
                   msb / lsb are expression strings or nil (scalar)"
  (require 'cl-lib)
  (let* ((clean (vtv--strip-comments text))
         name after params ports ansi-p)

    ;; Locate 'module <name>'
    (unless (string-match "\\<module\\>\\s-+\\([a-zA-Z_][a-zA-Z0-9_$]*\\)" clean)
      (error "verilog-parse-module: no module declaration found"))
    (setq name  (match-string 1 clean))
    (setq after (substring clean (match-end 0)))

    ;; Optional parameter block  #( ... )
    (when (string-match "\\`\\s-*#" after)
      (setq after (substring after (match-end 0)))
      (let ((r (vtv--extract-balanced after ?\( ?\))))
        (unless r (error "verilog-parse-module: unbalanced #() in %s" name))
        (setq params (vtv--parse-params-from-text (car r)))
        (setq after  (cdr r))))

    ;; Port list  ( ... )
    (let ((r (vtv--extract-balanced after ?\( ?\))))
      (unless r (error "verilog-parse-module: no port list in %s" name))
      (let ((port-text (car r)))
        (setq after (cdr r))
        ;; Determine style: ANSI if direction keyword appears inside parens
        (setq ansi-p (string-match "\\<\\(input\\|output\\|inout\\)\\>" port-text))
        (if ansi-p
            (setq ports (vtv--parse-ansi-ports port-text))
          ;; Non-ANSI: body text extends up to 'endmodule' (or EOF)
          (let ((body (if (string-match "\\<endmodule\\>" after)
                          (substring after 0 (match-beginning 0))
                        after)))
            ;; Strip leading ';'
            (when (string-match "\\`\\s-*;" body)
              (setq body (substring body (match-end 0))))
            ;; Parameters may be in body for non-ANSI
            (unless params
              (setq params (vtv--parse-params-from-text body)))
            (setq ports (vtv--parse-nonansi-ports port-text body))))))

    (list name params ports)))


;;; ----------------------------------------------------------------
;;; VHDL type / direction converters
;;; ----------------------------------------------------------------

(defun vtv--vhdl-type (msb lsb)
  "Return VHDL type string for a port with MSB/LSB range strings.
Returns \"std_logic\" for scalar, \"std_logic_vector(MSB downto LSB)\" otherwise."
  (if (and msb lsb)
      (format "std_logic_vector(%s downto %s)" msb lsb)
    "std_logic"))

(defun vtv--vhdl-dir (dir)
  "Return VHDL direction keyword padded to 5 chars for DIR."
  (cond ((string= dir "input")  "in   ")
        ((string= dir "output") "out  ")
        ((string= dir "inout")  "inout")
        (t (or dir "in   "))))


;;; ----------------------------------------------------------------
;;; Instance generator
;;; ----------------------------------------------------------------

(defun vhdl-insert-instance-from-verilog (text &optional instance-name)
  "Parse Verilog module TEXT and insert a VHDL component instance at point.
INSTANCE-NAME defaults to u_<module-name> when nil.
Indentation is taken from the current column."
  (let* ((parsed   (verilog-parse-module text))
         (mod-name (nth 0 parsed))
         (params   (nth 1 parsed))
         (ports    (nth 2 parsed))
         (base-col (current-column))
         (pad0     (make-string base-col ?\s))
         (pad2     (make-string (+ base-col 2) ?\s))
         (pad4     (make-string (+ base-col 4) ?\s)))

    (setq instance-name
          (or (and instance-name (> (length instance-name) 0) instance-name)
              (concat "u_" (downcase mod-name))))

    ;; Label : component_name
    (insert instance-name " : " mod-name "\n")

    ;; generic map
    (when params
      (insert pad2 "generic map (\n")
      (let ((n (length params)) (i 0))
        (dolist (p params)
          (setq i (1+ i))
          (insert pad4 (nth 0 p) " => " (nth 1 p))
          (insert (if (= i n) "\n" ",\n"))))
      (insert pad2 ")\n"))

    ;; port map
    (when ports
      (insert pad2 "port map (\n")
      (let ((n (length ports)) (i 0))
        (dolist (port ports)
          (setq i (1+ i))
          (insert pad4 (nth 1 port) " => " (nth 1 port))
          (insert (if (= i n) "\n" ",\n")))))
    (insert pad2 ");\n")))


;;; ----------------------------------------------------------------
;;; Interactive entry points
;;; ----------------------------------------------------------------

(defun vhdl-instance-from-verilog-region (start end &optional instance-name)
  "Parse the region (START to END) as a Verilog module and insert a VHDL
component instance at point.  With a prefix arg, prompt for the instance label."
  (interactive
   (list (region-beginning) (region-end)
         (when current-prefix-arg (read-string "Instance name: "))))
  (vhdl-insert-instance-from-verilog
   (buffer-substring-no-properties start end)
   instance-name))


(defun vhdl-instance-from-verilog-buffer (&optional instance-name)
  "Parse the current buffer as a Verilog module and insert a VHDL component
instance at point.  Useful when the buffer is the .v source file.
With a prefix arg, prompt for the instance label."
  (interactive
   (list (when current-prefix-arg (read-string "Instance name: "))))
  (vhdl-insert-instance-from-verilog
   (buffer-substring-no-properties (point-min) (point-max))
   instance-name))


(defun vhdl-instance-from-verilog-file (file &optional instance-name)
  "Read a Verilog module from FILE and insert a VHDL component instance at point.
With a prefix arg, prompt for the instance label."
  (interactive
   (list (read-file-name "Verilog file: " nil nil t nil
                         (lambda (f) (or (file-directory-p f)
                                        (string-match "\\.s?v$" f))))
         (when current-prefix-arg (read-string "Instance name: "))))
  (let ((text (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
    (vhdl-insert-instance-from-verilog text instance-name)))


;;; ----------------------------------------------------------------
;;; Optional: also expose the parser so callers can set verilog-module
;;; and reuse the existing vhdl-port-paste-verilog-instance from
;;; vhdl-helper.el.
;;;
;;; (setq verilog-module (verilog-parse-module-for-vhdl-helper <text>))
;;; ----------------------------------------------------------------

(defun verilog-parse-module-for-vhdl-helper (text)
  "Parse Verilog module TEXT and return a list compatible with the
verilog-module variable used by vhdl-helper.el:
  (name nil params-list ports-list)
where params-list entries are  (nil name nil nil type default)
and   ports-list  entries are  (direction name msb lsb)."
  (let* ((parsed   (verilog-parse-module text))
         (mod-name (nth 0 parsed))
         (params   (nth 1 parsed))
         (ports    (nth 2 parsed))
         compat-params compat-ports)
    (dolist (p params)
      (push (list nil (nth 0 p) nil nil "integer" (nth 1 p)) compat-params))
    (dolist (p ports)
      (push p compat-ports))
    (list mod-name nil (nreverse compat-params) (nreverse compat-ports))))


(provide 'verilog-to-vhdl-inst)
