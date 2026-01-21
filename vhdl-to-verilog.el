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

(provide 'vhdl-to-verilog)
