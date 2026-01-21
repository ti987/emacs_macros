;;
;; VHDL to Verilog converter
;; Convert VHDL entity declaration to Verilog module declaration
;;

(require 'regexp-const)
(require 'vhdl-re)

(defun vhdl-entity-to-verilog-module ()
  "Convert VHDL entity declaration to Verilog (not SystemVerilog) module declaration.
The function works on the current region or buffer, searching for a VHDL entity
declaration and converting it to Verilog module syntax.

Conversion rules:
- entity <name> is ... end <name>; => module <name> (...); endmodule
- in => input
- out => output  
- inout => inout
- STD_LOGIC => wire (no bit range)
- STD_LOGIC_VECTOR (N downto M) => [N:M]
- STD_LOGIC_VECTOR (M to N) => [M:N]
- Removes semicolons from port list (except last)
- Converts semicolons to commas between ports
"
  (interactive)
  (save-excursion
    (let ((entity-name "")
          (port-list '())
          (start-pos (point-min))
          (end-pos (point-max)))
      
      ;; Use region if active
      (when (use-region-p)
        (setq start-pos (region-beginning))
        (setq end-pos (region-end)))
      
      (goto-char start-pos)
      
      ;; Find entity declaration
      (if (re-search-forward "^\\s-*entity\\s-+\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-+is" end-pos t)
          (progn
            (setq entity-name (match-string 1))
            
            ;; Find port declaration start
            (if (re-search-forward "^\\s-*port\\s-*(" end-pos t)
                (progn
                  (let ((port-start (point))
                        (port-end nil)
                        (paren-depth 1))
                    
                    ;; Find matching closing parenthesis
                    (while (and (> paren-depth 0) (< (point) end-pos))
                      (cond
                       ((looking-at "(")
                        (setq paren-depth (1+ paren-depth))
                        (forward-char 1))
                       ((looking-at ")")
                        (setq paren-depth (1- paren-depth))
                        (if (= paren-depth 0)
                            (setq port-end (point)))
                        (forward-char 1))
                       (t (forward-char 1))))
                    
                    ;; Parse ports
                    (when port-end
                      (goto-char port-start)
                      (while (< (point) port-end)
                        (when (looking-at "^\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*:\\s-*\\(in\\|out\\|inout\\|buffer\\)\\s-+\\([^;]+\\);")
                          (let* ((signal-name (match-string 1))
                                 (direction (match-string 2))
                                 (signal-type (match-string 3))
                                 (verilog-dir (cond
                                               ((string= direction "in") "input")
                                               ((string= direction "out") "output")
                                               ((string= direction "inout") "inout")
                                               ((string= direction "buffer") "output")))
                                 (verilog-type (vhdl-convert-type-to-verilog signal-type)))
                            (push (list signal-name verilog-dir verilog-type) port-list)))
                        (forward-line 1)))
                    
                    ;; Reverse the list to maintain original order
                    (setq port-list (nreverse port-list))
                    
                    ;; Generate Verilog module
                    (goto-char start-pos)
                    (let ((verilog-output (vhdl-generate-verilog-module entity-name port-list)))
                      ;; Insert at point or replace selection
                      (if (use-region-p)
                          (delete-region (region-beginning) (region-end)))
                      (insert verilog-output))))
              (message "Could not find port declaration in entity")))
        (message "Could not find entity declaration")))))

(defun vhdl-convert-type-to-verilog (type-str)
  "Convert VHDL type string to Verilog bit range format.
TYPE-STR is a VHDL type like 'STD_LOGIC' or 'STD_LOGIC_VECTOR ( 47 downto 0 )'.
Returns a string with Verilog bit range like '[47:0]' or empty string for single bit.
Note: The bit range is enclosed in parentheses as per Verilog syntax."
  (let ((trimmed-type (replace-regexp-in-string "^\\s-+" "" type-str))
        (bit-range ""))
    (setq trimmed-type (replace-regexp-in-string "\\s-+$" "" trimmed-type))
    
    (cond
     ;; STD_LOGIC_VECTOR with downto
     ((string-match "STD_LOGIC_VECTOR\\s-*(\\s-*\\([0-9]+\\)\\s-+downto\\s-+\\([0-9]+\\)\\s-*)" trimmed-type)
      (let ((high (match-string 1 trimmed-type))
            (low (match-string 2 trimmed-type)))
        (setq bit-range (format "[%s:%s]" high low))))
     
     ;; STD_LOGIC_VECTOR with to
     ((string-match "STD_LOGIC_VECTOR\\s-*(\\s-*\\([0-9]+\\)\\s-+to\\s-+\\([0-9]+\\)\\s-*)" trimmed-type)
      (let ((low (match-string 1 trimmed-type))
            (high (match-string 2 trimmed-type)))
        (setq bit-range (format "[%s:%s]" low high))))
     
     ;; STD_LOGIC (single bit) - no range needed
     ((string-match "STD_LOGIC" trimmed-type)
      (setq bit-range ""))
     
     ;; Default case - no range
     (t
      (setq bit-range "")))
    
    bit-range))

(defun vhdl-generate-verilog-module (entity-name port-list)
  "Generate Verilog module declaration from entity name and port list.
ENTITY-NAME is the module name.
PORT-LIST is a list of (signal-name direction type-range) tuples."
  (let ((output "")
        (port-count (length port-list))
        (current-port 0))
    
    ;; Module header
    (setq output (concat "module " entity-name " (\n"))
    
    ;; Add ports
    (dolist (port port-list)
      (setq current-port (1+ current-port))
      (let* ((signal-name (nth 0 port))
             (direction (nth 1 port))
             (type-range (nth 2 port))
             (comma (if (< current-port port-count) "," "")))
        
        ;; Format: "  direction [range] signal_name,"
        (if (string= type-range "")
            (setq output (concat output (format "  %s %s%s\n" direction signal-name comma)))
          (setq output (concat output (format "  %s %s %s%s\n" direction type-range signal-name comma))))))
    
    ;; Module footer
    (setq output (concat output ");\n"))
    (setq output (concat output "endmodule\n"))
    
    output))

(defun vhdl-instantiation-to-verilog ()
  "Convert VHDL component/entity instantiation to Verilog module instantiation.
The function works on the current region or buffer, searching for a VHDL
instantiation and converting it to Verilog syntax.

Conversion rules:
VHDL:
  inst_name : entity work.module_name
    generic map (
      PARAM1 => value1,
      PARAM2 => value2
    )
    port map (
      port1 => signal1,
      port2 => signal2
    );

Verilog:
  module_name #(
    .PARAM1(value1),
    .PARAM2(value2)
  ) inst_name (
    .port1(signal1),
    .port2(signal2)
  );

Also handles component instantiation:
  inst_name : component_name
    port map (...);
"
  (interactive)
  (save-excursion
    (let ((inst-name "")
          (module-name "")
          (generic-list '())
          (port-list '())
          (start-pos (point-min))
          (end-pos (point-max)))
      
      ;; Use region if active
      (when (use-region-p)
        (setq start-pos (region-beginning))
        (setq end-pos (region-end)))
      
      (goto-char start-pos)
      
      ;; Find instantiation: inst_name : [entity work.]module_name or inst_name : component module_name
      (if (re-search-forward "^\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*:\\s-*\\(?:entity\\s-+\\(?:work\\.\\)?\\|component\\s-+\\)?\\([a-zA-Z][a-zA-Z0-9_]*\\)" end-pos t)
          (progn
            (setq inst-name (match-string 1))
            (setq module-name (match-string 2))
            
            ;; Look for generic map (optional)
            (when (re-search-forward "^\\s-*generic\\s-+map\\s-*(" end-pos t)
              (setq generic-list (vhdl-parse-map-clause end-pos)))
            
            ;; Look for port map
            (when (re-search-forward "^\\s-*port\\s-+map\\s-*(" end-pos t)
              (setq port-list (vhdl-parse-map-clause end-pos)))
            
            ;; Generate Verilog instantiation
            (let ((verilog-output (vhdl-generate-verilog-instantiation 
                                   inst-name module-name generic-list port-list)))
              ;; Insert at point or replace selection
              (goto-char start-pos)
              (if (use-region-p)
                  (delete-region (region-beginning) (region-end)))
              (insert verilog-output)))
        (message "Could not find VHDL instantiation")))))

(defun vhdl-parse-map-clause (end-pos)
  "Parse a VHDL generic map or port map clause.
Returns a list of (name => value) associations.
END-POS is the end position to search within."
  (let ((map-list '())
        (paren-depth 1)
        (clause-start (point))
        (clause-end nil))
    
    ;; Find matching closing parenthesis
    (while (and (> paren-depth 0) (< (point) end-pos))
      (cond
       ((looking-at "(")
        (setq paren-depth (1+ paren-depth))
        (forward-char 1))
       ((looking-at ")")
        (setq paren-depth (1- paren-depth))
        (if (= paren-depth 0)
            (setq clause-end (point)))
        (forward-char 1))
       (t (forward-char 1))))
    
    ;; Parse the clause content
    (when clause-end
      (goto-char clause-start)
      (let ((clause-str (buffer-substring-no-properties clause-start clause-end)))
        ;; Split by commas (but not commas inside parentheses)
        (let ((entries (vhdl-split-map-entries clause-str)))
          (dolist (entry entries)
            (when (string-match "^\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*=>\\s-*\\(.+\\)\\s-*$" entry)
              (let ((name (match-string 1 entry))
                    (value (match-string 2 entry)))
                ;; Trim whitespace from value
                (setq value (replace-regexp-in-string "^\\s-+" "" value))
                (setq value (replace-regexp-in-string "\\s-+$" "" value))
                (push (cons name value) map-list)))))))
    
    (nreverse map-list)))

(defun vhdl-split-map-entries (str)
  "Split a map clause string by commas, respecting parentheses nesting.
STR is the string to split."
  (let ((entries '())
        (current-entry "")
        (paren-depth 0)
        (i 0))
    (while (< i (length str))
      (let ((char (substring str i (1+ i))))
        (cond
         ((string= char "(")
          (setq paren-depth (1+ paren-depth))
          (setq current-entry (concat current-entry char)))
         ((string= char ")")
          (setq paren-depth (1- paren-depth))
          (setq current-entry (concat current-entry char)))
         ((and (string= char ",") (= paren-depth 0))
          ;; Found a top-level comma, save current entry
          (let ((trimmed (replace-regexp-in-string "^\\s-+" "" current-entry)))
            (setq trimmed (replace-regexp-in-string "\\s-+$" "" trimmed))
            (when (> (length trimmed) 0)
              (push trimmed entries)))
          (setq current-entry ""))
         (t
          (setq current-entry (concat current-entry char)))))
      (setq i (1+ i)))
    
    ;; Add the last entry
    (let ((trimmed (replace-regexp-in-string "^\\s-+" "" current-entry)))
      (setq trimmed (replace-regexp-in-string "\\s-+$" "" trimmed))
      (when (> (length trimmed) 0)
        (push trimmed entries)))
    
    (nreverse entries)))

(defun vhdl-generate-verilog-instantiation (inst-name module-name generic-list port-list)
  "Generate Verilog instantiation from parsed VHDL instantiation components.
INST-NAME is the instance name.
MODULE-NAME is the module/component name.
GENERIC-LIST is a list of (param . value) pairs.
PORT-LIST is a list of (port . signal) pairs."
  (let ((output "")
        (indent "  "))
    
    ;; Module name with optional parameters
    (if generic-list
        (progn
          (setq output (concat output module-name " #(\n"))
          (let ((param-count (length generic-list))
                (current-param 0))
            (dolist (param generic-list)
              (setq current-param (1+ current-param))
              (let* ((param-name (car param))
                     (param-value (cdr param))
                     (comma (if (< current-param param-count) "," "")))
                (setq output (concat output (format "%s.%s(%s)%s\n" 
                                                    indent param-name param-value comma)))))
          (setq output (concat output ") " inst-name " (\n")))
      ;; No parameters
      (setq output (concat output module-name " " inst-name " (\n")))
    
    ;; Port connections
    (when port-list
      (let ((port-count (length port-list))
            (current-port 0))
        (dolist (port port-list)
          (setq current-port (1+ current-port))
          (let* ((port-name (car port))
                 (signal-name (cdr port))
                 (comma (if (< current-port port-count) "," "")))
            (setq output (concat output (format "%s.%s(%s)%s\n" 
                                                indent port-name signal-name comma)))))))
    
    ;; Close instantiation
    (setq output (concat output ");\n"))
    
    output))

(provide 'vhdl-to-verilog)
