;;
;; vhdl programming helper
;; T. Isogai 2005-2023
;;

;;(defun vhdl-add-default-assignments ()
;;(defun vhdl-add-state-declarations ()
;;(defun vhdl-add-state-declarations-above ()
;;(defun vhdl-add-state-enum_encoding (type len)
;;(defun vhdl-backward-block ()
;;(defun vhdl-capture-comment ()
;;(defun vhdl-disable-identify-block ()  
;;(defun vhdl-enable-identify-block ()  
;;(defun vhdl-fill-comment ()
;;(defun vhdl-flatten-ports ()
;;(defun vhdl-flatten-signals ()
;;(defun vhdl-forward-block (&optional count)
;;(defun vhdl-goto-error (event)
;;(defun vhdl-goto-error-key
;;(defun vhdl-identify-block ()
;;(defun vhdl-import-verilog-ports ()
;;(defun vhdl-insert-bit-sizes ()
;;(defun vhdl-line-replace (regexp to-str)
;;(defun vhdl-port-paste-verilog-component ()
;;(defun vhdl-port-paste-verilog-generic-map (&optional secondary no-constants)
;;(defun vhdl-port-paste-verilog-instance (&optional name no-indent)
;;(defun vhdl-port-paste-verilog-parameter (&optional no-init)
;;(defun vhdl-port-paste-verilog-port ()
;;(defun vhdl-port-paste-verilog-port-map ()
;;(defun vhdl-replace-line-number ()
;;(defun vhdl-scroll-down-command (arg1)
;;(defun vhdl-scroll-up-command (arg1)
;;(defun vhdl-search-forward-re (re &optional ep err)
;;(defun vhdl-sort-port-signal-assignment-block ()
;; 
;;(defun vhdl-update-mode-menu ()
;;(defun test1 ()

;; 02-17-2023 
;;   Added unsigned, signed, ufixed, sfixed, etc. as vector for bit size calc
;;   Added integer, positive, and natural as 32 bit vector for bit size calc. Range
;;   is not considered for these.
;;   added real for 32 bit vector for bit size calc for tempoary

;(require 'assoc-array)


(defvar vhdl-rst-low-active nil)  ; low active reset for process opening substitution
(defvar vhdl-fill-comment-column 80)

(defun insert-string (str)
  "insert <str>  at point (xemacs->emacs conversion)"
  (insert str) )


;; add default assignments in sequencer
(defun vhdl-add-default-assignments ()
  "Add default reset assignments in sequencer.
Search signal reset assignments in the sequencer and add default assignments;
   0 for naturals and integers,
   (others=>'0') for vectors,
   '0' for all others
in the reset block. 
Invoke within the reset block"
  (interactive) 
  ; find if statement
  (let ((ap)  ; assignment point
        (sig-list '()) ; list of signals to be assigned
        (sig-decl) ; list of signal declarations
        (sig) ; elem of sig-list
        (ix) ; elem index
        (st-if-reset (concat vhdl-re-lead-sp0
                          "\\<if\\>"
                          re-wspace0
                          ".*"
                          re-open
                            "reset"
                          re-OR
                            "rst"
                          re-OR
                            "sys_rst"
                          re-OR
                            "por"
                          re-close
;                          re-open-opt
;                            "_?[nf]"
;                          re-close-opt
;                          re-any "*?"
 ;                         "\\<then\\>"
                          )  )
        (st-else (concat vhdl-re-lead-sp0
                            "\\<else\\>" ))
        (st-elsif (concat vhdl-re-lead-sp0
                            "\\<elsif\\>" ))
        )
    (beginning-of-line)
    (while (not (looking-at st-if-reset))
      (forward-line -1) )
    (forward-line)
    (while (looking-at vhdl-re-ln-comment)
      (forward-line)  )
    (beginning-of-line)
    (setq ap (point))       ; end of if-then
    (forward-line)
; this is for synchronous reset scheme
;    (while (not (looking-at st-else))
;      (forward-line) )
    (while (not (looking-at st-elsif))
      (forward-line) )
    ; make signal list from assignment statements
    (while (not (looking-at vhdl-re-st-endprocess))
      (if (looking-at vhdl-re-st-assign)
          ;;register signal 
                                        ;(aarray-add (list (match-string 1) . (nil))  'sig-list)
          (progn
            (setq sig (match-string-no-properties 1))
            (if (not (assoc sig sig-list))
                (setq sig-list (append sig-list (list (cons sig t) )
                                 ))))
        )
      (forward-line) )
    ;(setq sig-list (aarray-sort sig-list 'string<))
;    (sort sig-list (lambda (a b)
 ;                    (string< (car a)(car  b)))
  ;        )

    ; search sig def for data type
    (beginning-of-buffer)
    (while (< (point) ap)
      (forward-line)
      (if (looking-at vhdl-re-decl-port)
          (if (not (string-match (match-string-no-properties 2) "in"))
          (setq  sig-decl (append sig-decl (list  (cons (match-string-no-properties 1)   ; id
                                                        (match-string-no-properties 3) )))) ; type
          )
              
        (if (looking-at vhdl-re-decl-signal)
            ;;(aarray-add (list  (match-string 1)   ; id
            ;;                 (list (concat (match-string 2)  ; type
            ;;                             (match-string 3)) )) 
            ;;      'sig-decl) )  )
            (setq  sig-decl (append sig-decl (list  (cons (match-string-no-properties 1)   ; id
                                                          (concat (match-string-no-properties 2)  ; type
                                                                  (match-string-no-properties 3)) )) 
                                    )))
        ))
      
    (goto-char ap)

    (setq ix 0)
    ; insert assignment statement a
    (while (setq sig (car (nth ix sig-list)))
      (vhdl-indent-line)
      (insert-string sig)
      (insert-string " <= ")
      (setq type (cdr (assoc sig sig-decl)))
      ; see if it needs vector assignment
      (if (and sig-decl
               type)
          (if (or (string-match "vector" type)
                   (string-match "suv" type)
                   (string-match "slv" type)
                   (string-match "signed" type) )
              (insert-string "(others => '0');\n")
            (if (or (string-match "natural" type)
                    (string-match "integer" type))
                (insert-string "0;\n")
              (if (string-match "state_type" type)
                  (insert-string "ST_IDLE;\n")
                (insert-string " '0';\n") ;; all other types
                )))
        ;; else
        (insert-string (concat "'0'; -- not declared.  signal  " sig " : std_ulogic;\n") ))
        
      (setq ix (+ 1 ix))
      )
    (forward-line -1)
    (vhdl-align-assignment)
    (vhdl-indent-line)
))

    
(defun vhdl-flatten-signals ()
  "flatten signal definitions"
  (interactive)
  ; find break line
  (while (progn
           (beginning-of-line)
           (not (looking-at "\\s-*\\(--.*\\)?$")) 
           )
    (forward-line -1) )
  (forward-line 1)  ; should be at the first statememnt in a block
  (vhdl-indent-line) ; start with right alignment
  ; find receiver from current line. goto bol. copy. comment out.
 ;;  (beginning-of-line)
  (while (looking-at "\\s-*\\(signal\\|variable\\|constant\\)\\>")
    (re-search-forward  "\\(\\<\\(signal\\|variable\\|constant\\)\\>\\|:\\)")
    (let (
          (decl (match-string 2))
          (bp (point)) ; begin point
          (tp)        ; type point
          (stype) ; signal type 
          (stype-len) ; stype length
          )
      (if (equal ":" (match-string 1))
          (progn 
            (re-search-backward  "\\(signal\\|variable\\|constant\\)\\>")
            (re-search-forward  
             "\\(\\<\\(signal\\|variable\\|constant\\)\\>\\|:\\)")
            (setq decl (match-string 2))
            (setq bp (point))
            ))
      
      
      (re-search-forward "[a-zA-Z]")
      (forward-char -1)
      (kill-region bp (point))
      (insert-string " ")  ; remove crlf
      (re-search-forward ": *")
      (setq tp (point))
      (re-search-forward ";")  ; including newline
      (forward-char -1)
      (copy-to-register 'stype tp (point))    
      (setq stype-len (- (point) tp))
      
      (goto-char bp)
 
      (while (not (looking-at ":"))
        ; point at next id
        (re-search-forward "\\s-*[,:]\\s-*") ; jump to ,or:
        (search-backward (match-string 0))
        (if (= (current-column) 0)
            (delete-char -1))  ; delete crlf
        (if (looking-at "\\s-*,\\s-*")
            (progn
              (replace-match " : ")
              (insert-register 'stype)
              (forward-char stype-len)
              (insert-string ";")
              (if (looking-at "\\s-*--") 
                  (progn
                    (insert-string " ")
                    (forward-line 1)) ; keep comment
                (if (not (looking-at "$"))
                    (insert-string "\n")) ; add crlf
                ) 
              ; done with one
              (re-search-forward "[a-zA-Z]") ; next id
              (forward-char -1)
              (insert-string decl)  
              (insert-string " ")  
              )
          ; else :
          (search-forward ":")
          (forward-char -1)
          )
        )

;      (while (not (looking-at ":"))
;       (re-search-forward "[,:]")
;       (forward-char -1)
;       (if (looking-at ":[ \\t]+")
;           ;; remove extra spaces
;           (progn
;             (delete-char (- (match-end 0) (point)) )
;             (insert-string ": ")
;             (forward-char -2) ;; put cursor back on :
;             )
;         
;         ;; add type
;         (progn
;           (insert-string ": ")  
;           (insert-register 'stype)
;           (search-forward ",")
;           (backward-delete-char) ; delete ,
;           (insert-string ";\n") 
;                                       ; done with one
;           (while (looking-at "\\(\\s-\\|$\\)+")
;             (delete-char))
;           (re-search-forward "[a-zA-Z]") ; next id
;           (forward-char -1)
;           
;           (insert-string decl)  
;           (insert-string " ")  
;           (vhdl-indent-line)
;           )
;         )
;       )
      )
    (forward-line 1)
    )
    (forward-line -1) ;; back to the block
  (vhdl-align-flattened-signal-declarations)
)

(defun vhdl-flatten-ports ()
  "flatten port definitions"
  (interactive)
  ; find port or generic
  (while (progn
           (beginning-of-line)
           (not (looking-at "\\s-*\\<\\(port\\|generic\\)\\>")) 
           )
    (forward-line -1) )
  (forward-line 1)  ; should be at the first statememnt in a block
    
  ; find receiver from current line. goto bol. copy. comment out.
 ;;  (beginning-of-line)
  (while (not (looking-at "\\(\\s-\\|\n\\)*)"))
    (let (
          (bp (point)) ; begin point
          (tp)        ; type point
          (stype) ; signal type 
          (stype-len) ; stype length
          )
      
      
;      (re-search-forward "[a-zA-Z]")
;      (forward-char -1)
;      (kill-region bp (point))

      (insert-string "  ")  ; remove crlf
      (re-search-forward ":\\s-*")
      (setq tp (point))
      (re-search-forward ";")           ; eol or end of ports
      (if (not (looking-at "\\(\\s-\\|\n\\)*end\\W"))
          (forward-char -1) ; eol
        (search-backward ")")  ; eop
        (re-search-backward "[0-9a-zA-Z)_]") 
        (forward-char 1))

      (copy-to-register 'stype tp (point))    
      (setq stype-len (- (point) tp))
      
      (goto-char bp)
      (vhdl-indent-line) ; start with right alignment

      (while (not (looking-at ":"))
        ; point at next id
        (re-search-forward "\\s-*[,:]\\s-*") ; jump to ,or:
        (search-backward (match-string 0))
        (if (= (current-column) 0)
            (delete-char -1))  ; delete crlf
        (if (looking-at "\\s-*,\\s-*")
            (progn
              (replace-match " : ")
              (insert-register 'stype)
              (forward-char stype-len)
              (insert-string ";")
              (if (looking-at "\\s-*--") 
                  (progn
                    (insert-string " ")
                    (forward-line 1)) ; keep comment
                (if (not (looking-at "$"))
                    (insert-string "\n")) ; add crlf
                ) 
              ; done with one
              (re-search-forward "[a-zA-Z]") ; next id
              (forward-char -1)
              (insert-string " ")  
              )
          ; else :
          (search-forward ":")
          (forward-char -1)
          )
        )
      
      )
    (forward-line 1)
    (beginning-of-line)
    )
  (forward-line -1) ;; back to the block

  (vhdl-colon-align t)
)





(defun vhdl-goto-error (event)
  "Jump to error location in the source file"
  (interactive "@e")
  (mouse-set-point event)
  (beginning-of-line)
  (if (search-forward-regexp 
       "^\\*\\* \\(Error:\\|Warning: \\[[0-9]+\\]\\) *\\([^()]+\\)(\\([0-9]+\\)): *\\(.*\\)"
       nil t)
      (let ((file (match-string 2))
            (line (string-to-number (match-string 3)))
            (emsg (match-string 4))
            )
        (other-window 1)
        (find-file file)
        (goto-line line)
        (message emsg)
        )
    ))

;(vhdl-activate-customizations)

(defun vhdl-goto-error-key ()
  (define-key compilation-mode-map [(button2)] 'vhdl-goto-error )
)


(setq vhdl-type-list '()) ; list of types
(setq vhdl-constant-list '()) ; list of constants
(setq vhdl-record-list '()) ; list of records

(defun vhdl-insert-bit-sizes ()
  "calculate insert bit size in signals in record definitions"
  (interactive)
  (let ((val)
        (id)
        (esize)
        (rsize)
        (element-list '()) ; record elements
        )
  (save-excursion

    (setq vhdl-type-list '())
    (setq vhdl-constant-list '()) ; list of constants
    (setq vhdl-record-list '()) ; list of records
    
      (beginning-of-buffer)
    (while (not (equal (point) (buffer-end 1)))
      (beginning-of-line)
      (if (looking-at vhdl-re-decl-constant)
          (progn
            (setq id (match-string-no-properties 2))
            (setq val (match-string-no-properties 4))
            (add-to-list 'vhdl-constant-list ( cons id  (list (cons 'type  "constant" ) (cons 'size  (vhdl-evaluate-value val) ))))

            )
        (if (looking-at vhdl-re-decl-type-record)   ; record declaration
            (progn
              (setq rsize 0)
              (setq id (match-string-no-properties 2))
              
              (setq element-list '())
              (forward-line 1)
              (while (not (looking-at vhdl-re-st-endrecord))
                (if (looking-at vhdl-re-decl-record-element)  ; record element
                    (let ( (sig)
                           (val)
                           (tsize)
                           (esize)
                           (ep)
                           (type) )
                      (setq sig (match-string-no-properties 1))
                      (setq val (match-string-no-properties 2)) ; right side of :
                      (setq tsize (vhdl-get-type-size val)) ; another type?
                      (setq esize (car tsize))
                      (setq type (cdr tsize))
                      (add-bit-size-cmt rsize)
                      (add-to-ordered-list 'element-list  (cons sig (list (cons 'size  esize ) (cons 'base rsize) (cons 'type type)))  )
                      (if esize
                          (setq rsize (+ rsize esize)) )
                      ))
                
                (forward-line 1)
                
                )
              (add-bit-size-cmt rsize)
              (setq element-list (reverse element-list))
              (add-to-ordered-list 'vhdl-record-list  (cons id (list (cons 'type 'record)(cons 'size rsize) (cons 'element element-list))))
              )
          
          (if (looking-at vhdl-re-decl-type)
              (progn
                (setq id (match-string-no-properties 2))
                (setq val (match-string-no-properties 3))
                (setq tsize (vhdl-get-type-size val ))
                (setq esize (car tsize))
                (setq type (cdr tsize))
                (if (numberp esize)
                    ()
                  (if esize
                      (setq esize (string-to-number esize))
                    (setq esize 0) ))
                (add-to-list 'vhdl-type-list ( cons id  (list (cons 'type  type ) (cons 'size  esize) ))) 
                (end-of-line)
                (add-bit-size-cmt esize)
                )
          ))) ; end of if-looking-at
             
      (forward-line 1)
      )
    (setq vhdl-record-list (reverse vhdl-record-list))
    )
  )
  )

(defun add-bit-size-cmt (base)
  "add bit size base as a comment at EOL. If necessary, remove old value."
  (remove-bit-size-cmt)
  (end-of-line)
;  (if (> size 0) 
      (insert-string (concat "  --[" (number-to-string base) "]"))              
;    (message (concat "unknown size at line " (number-to-string (line-number-at-pos))))
;    (insert-string (concat "  --[XXX]"))              
;      )
      )


(defun remove-bit-size-cmt () 
  "remove bit size comment at EOL"
  (end-of-line)
  (let ((ep)
        (sp))
    (setq ep (point))
    (beginning-of-line)
    (setq sp (point))
    (replace-regexp "  --\\[\\([0-9]+\\|XXX\\)\\]" "" nil sp ep)
    ))



(defun vhdl-evaluate-value (str)
  "evaluate numberical value or <str>"
  (interactive)
  (let ((val)
        (const)
        )
    (if (numberp str)
        (setq val str)
      (if (setq const (assoc-string str vhdl-constant-list t)) ; another type that is declared already
          (setq val (cdr (assoc 'size (cdr const)) ))
        (setq val (string-to-number str))
        ))
    val
  ))


(defun vhdl-get-slice-size (val)
  "get bit size of slice <val>"
  (interactive)
  (let (( llmt )
        ( rlmt )
        (lrange)
        (rrange)
        (size)
        (re-vector 
         (concat "\\<"
                 re-open  ;; 1
                   "std_u?logic_vector"
                   re-OR
                   "suv"
                   re-OR
                   "slv"
                   re-OR
                   "array"
                   re-OR
                   "unsigned"
                   re-OR
                   "signed"
                   re-OR
                   "sfixed"
                   re-OR
                   "ufixed"
                   re-OR
                   "u_sfixed"
                   re-OR
                   "u_ufixed"
                 re-close
                 re-space0
                 "("
                 re-space0
                 re-open  ;; 2
                   ".+?"
                 re-close
                 re-space0
                 re-open  ;; 3
                   "downto"
                   re-OR
                   "to"
                 re-close
                 re-space1
                 re-open  ;;4
                   ".+?"
                 re-close
                 re-space0
                 ")"))
;;        (re-vector (concat "\\<\\(std_ulogic_vector *(\\|suv *(\\|slv *(\\|array *(\\) *\\(.+?\\) *\\(downto\\|to\\) +\\(.+?\\) *)"))
        )
    (if (string-match re-vector val)
        (progn
          (setq lrange  (match-string-no-properties 2 val))
          (setq rrange  (match-string-no-properties 4 val))
          (setq llmt (vhdl-evaluate-value lrange))
          (setq rlmt (vhdl-evaluate-value rrange))
          (if (> llmt rlmt)
              (setq size (+ 1 (- llmt rlmt)))
            (setq size (+ 1 (- rlmt llmt))) )
          )
      )
    ))

(defun vhdl-get-type-size (val)
  "get bit size and type of slice <val> from <vhdl-type-list>"
  (interactive)
  (let (
        (type-val)
        (rec-val)
        (esize nil)
        (type)
        (re-array 
         (concat 
          re-word-beg
          re-open    ;;1
            "array" re-space0 "(" re-space0
            ".+"    ;; array id
            re-space1
            re-open 
              vhdl-re-down-or-up  ;;2
            re-close
            re-space1
            ".*" 
            ")"
          re-close 
          re-space0 "of" re-space1 
          re-open   ;; 3
            ".+?"
          re-close
          re-tail-space0
          )) 
;;        (re-array "\\<\\(array *( *.* \\(downto\\|to\\) +.*)\\) *of +\\(.+?\\) *$" )
        )
    (if (string-match re-array val ) ; array 
          (let ((slice (match-string 1 val))
                (base  (match-string 3 val))
                (asize)
                (bsize)
                
                )
            (setq asize (vhdl-get-slice-size slice))
            (if (string-match "\\<\\(integer\\|real\\|natural\\|positive\\)\\>" base)
                (setq bsize 32)
              (setq bsize (car (vhdl-get-type-size base))) )
            (setq esize (* asize bsize))
            (setq type 'array)
          )
      (if (setq rec-val (assoc-string val vhdl-record-list t)) ; another type record that is declared already
          (progn
            (setq esize (vhdl-evaluate-value (cdr (assoc 'size (cdr rec-val)) )))
            (setq type 'record)
            )
        (if (setq type-val (assoc-string val vhdl-type-list t)) ; another type that is declared already
            (progn
              (setq esize (vhdl-evaluate-value (cdr (assoc 'size (cdr type-val)) )))
              (setq type (cdr (assoc 'type (cdr type-val))))
              )
                                        ;else
          (if (string-match "\\<\\(suv\\|slv\\)_\\([0-9]+\\)" val) ; suv_32 etc. arrays defined by myself
              (setq esize (string-to-number (match-string-no-properties 2 val)))
                                        ;else
            (if (setq esize (vhdl-get-slice-size val)) ; suv(9 downto 0) etc.
                (setq type 'vector )
                                        ;else
              (if (string-match-p  "\\<\\(std_ulogic\\|std_logic\\)\\>" val) ; single bit
                  (progn
                    (setq esize 1)
                    (setq type 'logic )
                    )
            ))))) )
    (cons esize type)
    ))

; line replace
(defun vhdl-line-replace (regexp to-str)
 "replace regexp with to-str from current position to end-of-line"
 (let ((cp (point))
       (ep)
       )
   (end-of-line)
   (setq ep (point))
   (goto-char cp)
   (if (search-forward-regexp regexp ep t)
       (replace-match to-str))
   ))

(defun vhdl-import-verilog-ports ()
 "import verilog ports (input/output declaration) and reformat to vhdl"
 (interactive)
  ; find breakable
  (while (progn
           (beginning-of-line)
           (looking-at "\\s-*\\(\\(input\\|output\\)\\>\\|//\\|$\\)")
           )
    (forward-line -1) )
  (forward-line 1)  ; should be at the first statememnt in a block

  (let ((vec "\\[\\([0-9]+\\):\\([0-9]+\\)\\]")
        (spc "\\s-*")
        (kw)
        )
    (while (and (not (equal (point) (buffer-end 1)))
                (looking-at "\\s-*\\(\\(in\\|out\\)put\\>\\|//\\|$\\)"))
      (setq kw (match-string 2))
      (if (or (equal kw "in") (equal kw "out"))
        (if (looking-at (concat spc kw "put" spc vec))
            (progn
              (vhdl-line-replace 
               (concat kw "put" spc vec spc "\\(.*\\);")
               (concat "\\3 : " kw " std_logic_vector(\\1 downto \\2);"))
              )
          (vhdl-line-replace 
           (concat kw "put" spc "\\(.*\\);")
           (concat "\\1 : " kw " std_logic;"))))
      (beginning-of-line)
      (vhdl-line-replace "//" "--")
      (forward-line)
      )
    )
)

(defun vhdl-disable-identify-block ()  
  "disable vhdl-identify-block function"
  (interactive)
  (setq vhdl-identify-block-enable nil)
)


(defun vhdl-enable-identify-block ()  
  "enable vhdl-identify-block function"
  (interactive)
  (setq vhdl-identify-block-enable t)
)

;; identify vhdl block point blongs
(defun vhdl-identify-block ()
 "identify vhdl block point blongs"
 (interactive)
    (while nil 
  (save-excursion
   (set this-command 'vhdl-identify-block)

   (if (eq last-command 'vhdl-identify-block)
       ;; if successively called, find next (outer) block opener
       (progn
         (goto-char vhdl-identify-block-point)
         (if (eq vhdl-identify-block-level 1)
             ;; already at the outmost block opener
             ();;(string-match "\\(.\\)" "=") ; dummy matching
           (if (eq vhdl-identify-block-level 2)
               (search-backward-regexp (concat "^\\s-*" vhdl-block-level-1-re))
             (search-backward-regexp vhdl-block-re)
             ))
         )
     ;; new sequence
     (end-of-line)
     (search-backward-regexp vhdl-block-re) )

     (let ((str1 (match-string 1))
           (str2  (match-string 2))
           (end-list nil)
           (level 0)
           )
       ; if 'end' is first, build list
       ; if the list is empty and found opener, stop

       
       (while (string-match  "^end\\s-+loop" str1)
         ; skip loop 
         (setq level 1)
         (while (> level 0)
           (search-backward-regexp vhdl-block-re)
           (setq str1 (match-string 1))
           (cond ((string-match  "^end\\s-+loop" str1)
                  (setq level (+ level 1)))
                 ((string-match  vhdl-loop-re str1)
                  (setq level (- level 1)))
                 ))
         (search-backward-regexp vhdl-block-re))

       (cond ((string-match  "^end\\b" str1)
              (search-backward-regexp vhdl-block-level-1-re)
              (setq str1 (match-string 1)))
             ((string-match  vhdl-block-port-map-end-re str1)
              (search-backward-regexp vhdl-block-level-1-re)
              (setq str1 (match-string 1)))
             )
       
       
       
       (setq modeline-misc-info str1)
       (cond ((string-match vhdl-block-level-1-re str1)
              (setq vhdl-identify-block-level  1))
             ((string-match vhdl-block-level-2-re str1)
              (setq vhdl-identify-block-level  2))
             ((string-match vhdl-instance-re str1)
              (setq vhdl-identify-block-level  2))
             (t
              (setq vhdl-identify-block-level  9)))
       
       (message "--%s" str1)
       
       )
 
   (setq vhdl-identify-block-point (point))
   ))
) 


(defvar vhdl-identify-block-point 
  "point to perform vhdl-identify-block function successively"
  1 )


(defconst vhdl-block-level-1-re
  "\\(\\(architecture\\|entity\\|package\\s-+body\\|package\\)\\s-*[a-zA-Z0-9_]+\\|configuration\\)")

(defconst vhdl-block-level-2-re
   "\\(\\(component\\|procedure\\|function\\)\\s-*[a-zA-Z0-9_]+\\|[a-zA-Z0-9_]+\\s-*:\\s-*\\(process\\|block\\)\\)" )

(defconst vhdl-block-level-2-end-re
   "\\(end\\)\\s-+\\(component\\|procedure\\|function\\|process\\|block\\)")

(defconst vhdl-block-port-map-end-re
   "\\(port\\s-+map[^;]+;\\)")


(defconst vhdl-label-re
  "[a-zA-Z0-9_]+\\s-*:\\s-*[a-zA-Z0-9_]+")
(defconst vhdl-map-re 
  "[ \t\n]+\\(generic\\|port\\)\\s-+map" )


(defconst vhdl-instance-re
  (concat vhdl-label-re vhdl-map-re))

(defconst vhdl-loop-re
  "[a-zA-Z0-9_]+\\s-*:\\s-*\\(while\\|for\\|loop\\)\\b")


(defconst vhdl-block-re
  (concat
   "^\\s-*"
   "\\("
   vhdl-block-level-1-re
   "\\|"
   vhdl-block-level-2-re
   "\\|"
   vhdl-block-level-2-end-re
   "\\|"
   vhdl-block-port-map-end-re
   "\\|"
   vhdl-label-re
   vhdl-map-re
   "\\|"
   vhdl-loop-re
   "\\)"
))


(defvar modeline-misc-info "miscellaneous information on mode line" nil)
  
  (setq modeline-format
        (list 
         "" 
         'mode-line-canna-mode  
         'modeline-modified
         "%b--"
         ":"
         'default-directory
         "  "
         'global-mode-string
         "  %[("
         'mode-name
         'modeline-process
         'minor-mode-alist
         "%n"
         ")%]--"
         '(line-number-mode "L%l--")
         'modeline-misc-info
         "-%-"))
  

(defun vhdl-scroll-up-command (arg1)
 "same as scroll-up-command except it updates modeline"
   (interactive "p")
   (scroll-up-command nil)
   (if vhdl-identify-block-enable
     (vhdl-identify-block)  )
)

(defun vhdl-scroll-down-command (arg1)
 "same as scroll-down-command except it updates modeline"
   (interactive "p")
   (scroll-down-command nil)
   (if vhdl-identify-block-enable
       (vhdl-identify-block)  )
)

(defun vhdl-fill-comment ( &optional width )
  "Fill the vhdl comment paragraph where cursor is.
Change contiguous blank lines to comments by preceeding with --"
   (interactive )
   (let ( (cmt "^--")
          (blank-cmt "^--\\s-*$")
          (subtitle-cmt "^--\\s-\\S-+\\s-*:")
          (cmt-sp "^--\\s-*")
          (bp)
          (ep)
          )

     (if (eq nil width)
         (setq width vhdl-fill-comment-column)
       )
     (setq fill-column width)
     
     ;;search beginning of comment paragraph
     (beginning-of-line)
     (while (and (looking-at cmt) (not (looking-at blank-cmt)) (not (looking-at subtitle-cmt)) (= (forward-line -1) 0))
       (match-data)
       )

     
     (if (not (and (looking-at cmt) (not (looking-at blank-cmt)) (not (looking-at subtitle-cmt)) ))
         (forward-line 1))  ;; if not at bof or 1st line not proper comment, go next line

     (setq bp (point))
     
     ;;search beginning of comment paragraph
     (while (and (looking-at cmt-sp) (not (looking-at blank-cmt)) (not (looking-at subtitle-cmt)) )
       (replace-match "")
       (forward-line 1)
       )
     (setq ep (point))
     (fill-region-as-paragraph bp ep)
     ;;(fill-paragraph nil)
     (forward-char -1)
     (setq ep (point))
     (push-mark)
     (goto-char bp)
     ; (activate-region)
     (replace-regexp "^" "-- " nil bp ep)
))



(defun vhdl-search-forward-re (re &optional ep err)
  "Search forward with regular expression.
Skips comments during search."
  ;; dummy for now
  (search-forward-regexp re ep err)
  )
  
(defun vhdl-add-state-enum_encoding (type len)
  "Add enum_encoding attribute lines for a state machine to enumerate states
<type> - declared type id
<len>  - number of states"

  (let ((ix 0) ; vector index
        (zeros) ; string filled with 0s
        (vec)  ; each vector value
        (str "") ; entire type string
        )

    (while (> len ix)
      ;; create a vector
      (setq zeros (make-string len ?0))
      (setq vec (substring zeros 0 (- -1 ix)))
      (setq vec (concat vec "1"))
      (if (> ix 0)
          (setq vec (concat vec (substring zeros 0 ix ) ) ) )
      (setq str (concat str " " vec ))
      (setq ix (+ 1 ix))
      )
    (setq str (substring str 1 nil))
    (vhdl-indent-line)
    (insert-string "attribute enum_encoding : string;\n")
    (vhdl-indent-line)
    (insert-string (concat "attribute enum_encoding of " type " : type is \"" str "\";\n"))
    )
  )




(defun vhdl-add-state-declarations ()
  "Scan for states and add declaration.
Search for 'when XYZ =>' and capture state ID and comment in the following line, then
generate state type declaration."
  (interactive)
  (let ((sig) ;; state signal name
        (case-list) ;; list of state info
        (cl-list (list)) ;; list of list of state info
        (case1)   ;; list of 2 below items 
        (state)  ;; state id
        (cmt)    ;; comment on state
        (ix)     ;; list index
        (last-ix);; last item index of case-list
        (nest 0) ;; case statement nesting level
        (eof (point-max)) ;; end of file pointer
        )

    ;; gather state info
    (save-excursion
      (while
          (vhdl-search-forward-re vhdl-re-st-case eof t)
        (setq nest 1)
        (setq sig (match-string 1))
        (while (and (zerop (forward-line)) ;; moved
                    (or (not (looking-at vhdl-re-st-endcase))
                        (> nest 1)
                        )   )     
      
          (if (looking-at vhdl-re-st-case)
              (setq nest (+ nest 1 ) )   ;; inner case-statement is not a state machine
            (if (looking-at vhdl-re-st-endcase)
              (setq nest (- nest 1) )
                ))
            
              
          (if (and (looking-at vhdl-re-st-when)
                   (= nest 1) )
              (progn
                (setq state (match-string 1))
                (forward-line)
                (if (not (equal state "others"))
                    (progn
                      (if (looking-at vhdl-re-ln-comment)
                          (progn
                            (setq cmt (match-string 1))
                            (forward-line) )
                        (setq cmt nil) )
                      (setq case-list (append case-list (list (list state cmt ))))
                  ) )
                ) )  
          ) 
        (setq cl-list (append (append cl-list (list sig)) (list case-list)))
        (setq case-list nil)
        ))

    ;; print state types
    (while (setq sig (car cl-list))
      (setq cl-list (cdr cl-list))
      (setq case-list (car cl-list))
      (setq cl-list (cdr cl-list))
      (beginning-of-line)
      (vhdl-indent-line) ; start with right alignment
      (insert-string (concat "type " sig "_type is\n"))
      (vhdl-indent-line)
      (insert-string "(")
      (setq ix 0)
      (setq last-ix (- (length case-list) 1))
      (while (setq case1 (nth ix case-list))
        (vhdl-indent-line)
        
        (insert-string (car case1))        ;; state id
        (if (> last-ix ix) 
            (insert-string ",") )
        (if (nth 1 case1)
            (progn
              (insert-string (nth 1 case1))   ;; comment
              (vhdl-align-inline-comment-group)
              ))
        ;;(vhdl-electric-return)
        (insert-string "\n")
        (setq ix (+ 1 ix))
        )
      (vhdl-indent-line)
      
      (insert-string ");\n\n")
      ;;(vhdl-electric-return)

      ;; add enum_encoding attribute
      ;;(vhdl-add-state-enum_encoding (concat sig "_type")  (+ 1 last-ix))
      ) ; end while

    ) )
        

(defun vhdl-sort-port-signal-assignment-block ()
  "Sort current port, signal, or assignment block.
Block is bounded by blank line or comment only line."
  (interactive)

  (let ((top)
        (bottom)
        )

    ;; skip blank or comment line
    (beginning-of-line)
    (while (looking-at vhdl-re-blank-comment-line)
      (forward-line) )

    ;; find the top of block
    (while (or
            (looking-at vhdl-re-st-assign)
            (looking-at vhdl-re-decl-signal)
            (looking-at vhdl-re-decl-variable)
            (looking-at vhdl-re-decl-port)
            )
      (forward-line -1) )
    (forward-line)
    (setq top (point))

    ;; find the bottom of block
    (while (or
            (looking-at vhdl-re-st-assign)
            (looking-at vhdl-re-decl-signal)
            (looking-at vhdl-re-decl-variable)
            (looking-at vhdl-re-decl-port)
            )
      (forward-line) )
    (setq bottom (point))

    (sort-lines nil top bottom)
    ))

(defun vhdl-forward-block (&optional count)
  "Move cursor to next block beginning or ending. 
Block is bounded by blank line"
  (interactive)

  (if (eq nil count)
      (setq count 1))
  (beginning-of-line)
  (if (< count 0)
      ;; backward
      (progn
        (forward-line count)
        (while (and (<(point)(point-max))
                    (looking-at vhdl-re-blank-line)
                    )
          (forward-line count) )
        (while (and (<(point)(point-max))
                    (not (looking-at vhdl-re-blank-line))
                    )
          (forward-line count) )
        (forward-line)
        )
    
      ;; forward
    (if  (not (looking-at vhdl-re-blank-line))
        (progn
          ;; find non blank line
          (while (and (<(point)(point-max))
                      (looking-at vhdl-re-blank-line)
                      )
            (forward-line count) )
          (while (and (<(point)(point-max))
                      (not (looking-at vhdl-re-blank-line))
                      )
              (forward-line count) )
          )
      )
    
    (while (and (<(point)(point-max))
                (looking-at vhdl-re-blank-line)
                )
      (forward-line count) )
    )
  )


(defun vhdl-backward-block ()
  "Move cursor to previous block beginning or ending. 
Block is bounded by blank line"
  (interactive)
  (vhdl-forward-block -1) )

;;
;; verilog interface
;;

(defun vhdl-port-paste-verilog-component ()
  "Paste as a component declaration."
  (interactive)

  (let ((margin (current-indentation))
        (name (nth 0 verilog-module)))

    (vhdl-insert-keyword "component ")
    (insert name)
    (when (not (vhdl-standard-p '87))
        (vhdl-insert-keyword " is"))
    ;; paste generic and port clause
    (when (nth 1 vhdl-port-list)
      (insert "\n")
;      (when (and (memq vhdl-insert-empty-lines '(unit all)) (eq kind 'entity))
;       (insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
;      (vhdl-port-paste-generic (eq kind 'component)))
      (vhdl-port-paste-verilog-parameter t))
    (when (nth 2 vhdl-port-list)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset)))
;    (vhdl-port-paste-port)
    (vhdl-port-paste-verilog-port)
    (insert "\n")
    (indent-to margin)
    (vhdl-insert-keyword "end")
    (vhdl-insert-keyword " component")
    (unless (vhdl-standard-p '87) (insert " " name)))
    (insert ";\n")
    )
                                      
(defun vhdl-port-paste-verilog-parameter (&optional no-init)
  "Paste a verilog parameter clause."
  (let ((margin (current-indentation))
        list-margin start names generic
        generics-list
        type
        )
    (setq generics-list (third verilog-module))

    ;; paste generic clause
    (when generics-list
      (setq start (point))
      (vhdl-insert-keyword "generic (")
      (unless vhdl-argument-list-indent
        (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
      (setq list-margin (current-column))
      (while generics-list
        (setq generic (car generics-list))
        ;; paste group comment and spacing
        ;(when (memq vhdl-include-group-comments '(decl always))
         ; (vhdl-port-paste-group-comment (nth 4 generic) list-margin))
        ;; paste names
        (insert (second generic))
        ;; paste type
        (setq type (fifth generic))
        (if (not type)
            (setq type "integer") )
        (insert " : " type)
        ;; paste initialization
        (when (and (not no-init) (sixth generic))
          (insert " := " (sixth generic)))
        (unless (cdr generics-list) (insert ")"))
        (insert ";")
        ;; paste comment
        ;(when (and vhdl-include-port-comments (nth 3 generic))
        ;  (vhdl-comment-insert-inline (nth 3 generic) t))
        (setq generics-list (cdr generics-list))
        (when generics-list (insert "\n") (indent-to list-margin)))
      ;; align generic clause
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1 t))
      )))


(defun vhdl-port-paste-verilog-port ()
  "Paste a verilog port clause."
  (let ((margin (current-indentation))
        list-margin start names port
        (ports-list)
        )

    (setq ports-list (fourth verilog-module))

    ;; paste port clause
    (when ports-list
      (setq start (point))
      (vhdl-insert-keyword "port (")
      (unless vhdl-argument-list-indent
        (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
      (setq list-margin (current-column))
      (while ports-list
        (setq port (car ports-list))
        ;; paste names
        (insert (second port))
        ;; paste direction
        (insert " : ")
        (when (equal (first port) "input")
          (insert "in    "))
        (when (equal (first port) "output")
          (insert "out   "))
        (when (equal (first port) "inout")
          (insert "inout "))
        ;; paste type
        (if (equal (third port) "")
            (insert "std_logic")
          (insert "std_logic_vector(") 
          (if (< (string-to-int (third port)) (string-to-int (fourth port)))
              (insert (third port) " to " (fourth port) ")")
            (insert (third port) " downto " (fourth port) ")")
            ) )
        (unless (cdr ports-list) (insert ")"))
        (insert ";")
        (setq ports-list (cdr ports-list))
        (when ports-list (insert "\n") (indent-to list-margin)))
      ;; align port clause
      (when vhdl-auto-align (vhdl-align-region-groups start (point) 1))
      )))


(defun vhdl-port-paste-verilog-instance (&optional name no-indent)
  "Paste verilog module as an instantiation."
  (interactive)
    (let ((port-list verilog-module))

      ;; flatten local copy of port list (must be flat for port mapping)

      (unless no-indent (indent-according-to-mode))
      (let ((margin (current-indentation)))
        ;; paste instantiation
        (if name
            (insert name ": ")
          (if (equal (cdr vhdl-instance-name) "")
              (vhdl-template-field "instance name" ": ")
            (insert (vhdl-replace-string vhdl-instance-name
                                         (nth 0 port-list)) ": ")))
        (message "Pasting port as instantiation...")
        (if (vhdl-standard-p '87)
            (insert (nth 0 port-list))
          (vhdl-insert-keyword "ENTITY ")
          (insert (vhdl-work-library) "." (nth 0 port-list)))
        (when (nth 2 port-list)
          (insert "\n") (indent-to (+ margin vhdl-basic-offset))
          (vhdl-port-paste-verilog-generic-map t t))
        (when (nth 3 port-list)
          (insert "\n") (indent-to (+ margin vhdl-basic-offset))
          (vhdl-port-paste-verilog-port-map))
        (message "Pasting port as instantiation...done"))
      ;(setq vhdl-port-list orig-vhdl-port-list)
      ))

(defun vhdl-port-paste-verilog-generic-map (&optional secondary no-constants)
  "Paste as a generic map."
  (interactive)
  (unless secondary (indent-according-to-mode))
  (let ((margin (current-indentation))
        list-margin start generic
        (generics-list (nth 2 verilog-module)))
    (when generics-list
      (setq start (point))
      (vhdl-insert-keyword "GENERIC MAP (")
      (if (not vhdl-association-list-with-formals)
          ;; paste list of actual generics
          (while generics-list
            (insert (if no-constants
                      (car (nth 1 (car generics-list)))
                    (or (nth 2 (car generics-list)) " ")))
            (setq generics-list (cdr generics-list))
            (insert (if generics-list ", " ")")))
        (unless vhdl-argument-list-indent
          (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
        (setq list-margin (current-column))
        (while generics-list
          (setq generic (car generics-list))
          ;; paste formal and actual generic
          (insert (nth 1 generic) " => "
                  (if no-constants
                      (nth 1 generic)
                    (or (nth 3 generic) "")))
          (setq generics-list (cdr generics-list))
          (insert (if generics-list "," ")"))
          (when generics-list (insert "\n") (indent-to list-margin)))
        ;; align generic map
        (when vhdl-auto-align
          (vhdl-align-region-groups start (point) 1 t))))))

(defun vhdl-port-paste-verilog-port-map ()
  "Paste as a port map."
  (let ((margin (current-indentation))
        list-margin start port
        (ports-list (nth 3 verilog-module)))
    (when ports-list
      (setq start (point))
      (vhdl-insert-keyword "PORT MAP (")
      (if (not vhdl-association-list-with-formals)
          ;; paste list of actual ports
          (while ports-list
            (insert (vhdl-replace-string vhdl-actual-port-name
                                         (nth 1 (car ports-list))))
            (setq ports-list (cdr ports-list))
            (insert (if ports-list ", " ");")))
        (unless vhdl-argument-list-indent
          (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
        (setq list-margin (current-column))
        (while ports-list
          (setq port (car ports-list))
          ;; paste formal and actual port
          (insert (nth 1 port) " => ")
          (insert (vhdl-replace-string vhdl-actual-port-name
                                       (nth 1 port)))
          (setq ports-list (cdr ports-list))
          (insert (if ports-list "," ");"))
          (when ports-list (insert "\n") (indent-to list-margin)))
        ;; align port clause
        (when vhdl-auto-align
          (vhdl-align-region-groups start (point) 1))))
    (insert "\n")
    ))



;;
;; mod menu


(defvar vhdl-helper-menu-list 
     '("VHelper"
       ["copy entity" (vhdl-port-copy)]
       ["paste verilog module as a component" (vhdl-port-paste-verilog-component)]
       ["paste verilog module as an instance" (vhdl-port-paste-verilog-instance)]
       ["flatten-signals" (vhdl-flatten-signals)]
       ["flatten-ports" (vhdl-flatten-ports)]
       ["import-verilog-ports" (vhdl-import-verilog-ports)]
       ["disable-identify-block" (vhdl-disable-identify-block)]
       ["enable-identify-block" (vhdl-enable-identify-block)]
       ["identify-block" (vhdl-identify-block)]
       ["fill-comment" (vhdl-fill-comment)]
       ["add-state-declarations" (vhdl-add-state-declarations)]
       ["add-default-assignments" (vhdl-add-default-assignments)]
       ["port-paste-verilog-component" (vhdl-port-paste-verilog-component)]
       ["port-paste-verilog-instance" (vhdl-port-paste-verilog-instance)]
       ["replace-line-number" (vhdl-replace-line-number)]
       ["update-mode-menu" (vhdl-update-mode-menu)]
       ["trace-port-up" (vhdl-trace-port-up)]
       ["trace-port" (vhdl-trace-port)]
       ["forward-block" (vhdl-forward-block)]
       ["backwrd-block" (vhdl-forward-block)]
       ["sort block" (vhdl-sort-port-signal-assignment-block)]
       ) 
  "VHDL Helper menu.")

(defun vhdl-update-helper-menu ()
  "Update VHDL helper menu."
  (interactive)
  (easy-menu-remove vhdl-helper-menu-list) ; for XEmacs
  (easy-menu-add vhdl-helper-menu-list)   ; for XEmacs
  (easy-menu-define vhdl-helper-menu vhdl-mode-map
                    "Interface ports Menu" vhdl-helper-menu-list)
  )


; replace __LINE__ with actual line number 0_01_0#...#
; replace 0_01_0#(decimal number)# with actual line number 0_01_0#...#
; or replace the last number where a line contains __LINENUM__ in comment with
; the actual (new) line number
; line number is shown as 0_01_0#123#
(defun vhdl-replace-line-number ()
  "replace __LINE__ or 0_01_0#(decimal number)# with a present line number in 0_01_0 format"
  (interactive)
  (save-excursion
  (let ((rep))

    ;; replace the last number in the line
    (beginning-of-buffer)
    (while 
        (search-forward-regexp "\\(0_01_0#[0-9]+#\\|[0-9]+\\)\\([^0-9]*--.*\\)__LINENUM__" (point-max) t)
      (replace-match (concat
                      "0_01_0#"
                      (number-to-string (line-number-at-pos))
                      "#"
                      (match-string 2) 
                      ))  )
      
    ;; replace 0_01_0# number in the line
    (beginning-of-buffer)
    (while 
        (search-forward-regexp "0_01_0#\\([0-9]+\\)#" (point-max) t)
      (replace-match (concat
                      "0_01_0#"
                      (number-to-string (line-number-at-pos))
                      "#"
                      (match-string 2) 
                      )) )
    
    ;; replace __LINE__ with a line number
    (beginning-of-buffer)
    (while 
        (vhdl-re-search-forward "__LINE__\\(.*\\)" (point-max) t)
      (replace-match (concat
                      "0_01_0#"
                      (number-to-string (line-number-at-pos))
                      "#"
                      (match-string 1) 
                      )
                     ))
  ))
)

;; replace __LINE__ with actual line number
;; or replace a number where a line contains __LINENUM__ in comment with
;; the actual (new) line number
;(defun vhdl-replace-line-number ()
;  "replace _LINE_ with actual line number"
;  (interactive)
;  (save-excursion
;  (let ((rep))
;    (beginning-of-buffer)
;    (while 
;        ;; replace the last number in the line
;        (search-forward-regexp "\\([0-9]+\\)\\([^0-9\n]*?--.*__LINENUM__\\)" (point-max) t)
;      (replace-match (concat
;                      (number-to-string (line-number-at-pos))
;                      (match-string 2) 
;                      )) )
;      
;    (beginning-of-buffer)
;    (while 
;        (vhdl-re-search-forward "__LINE__\\(.*\\)" (point-max) t)
;      (replace-match (concat
;                      (number-to-string (line-number-at-pos))
;                      (match-string 1) 
;                      " -- __LINENUM__"
;                      )
;                     ))
;  ))
;)

;; add default assignments in async reset sequencer 
(defun vhdl-add-default-assignments-2 ()
  "Add default reset assignments in sequencer.
Search signal reset assignments in the sequencer and add default assignments;
   0 for naturals and integers,
   (others=>'0') for vectors,
   '0' for all others
in the reset block. 
Invoke within the reset block"
  (interactive)


  ;; search back <process>
  ;; search <if> - async reset block
  ;; search <elsif> -- main block. keep current point.
  ;; 
  ; find if statement
  (let ((ap)  ; assignment point
        (sig-list '()) ; list of signals to be assigned
        (sig-decl) ; list of signal declarations
        (sig) ; elem of sig-list
        (ix) ; elem index
        (st-if-reset (concat vhdl-re-lead-sp0
                          "\\<if\\>"
                          re-wspace0
                          ".*"
                          re-open
                            "reset"
                          re-OR
                            "rst"
                          re-OR
                            "por"
                          re-close
;                          re-open-opt
;                            "_?[nf]"
;                          re-close-opt
;                          re-any "*?"
 ;                         "\\<then\\>"
                          )  )
        (st-else (concat vhdl-re-lead-sp0
                            "\\<else\\>" ))
        )
    (beginning-of-line)
    (while (not (looking-at st-if-reset))
      (forward-line -1) )
    (forward-line)
    (while (looking-at vhdl-re-ln-comment)
      (forward-line)  )
    (beginning-of-line)
    (setq ap (point))       ; end of if-then
    (forward-line)
    (while (not (looking-at st-else))
      (forward-line) )
    ; make signal list from assignment statements
    (while (not (looking-at vhdl-re-st-endprocess))
      (if (looking-at vhdl-re-st-assign)
          ;;register signal 
                                        ;(aarray-add (list (match-string 1) . (nil))  'sig-list)
          (progn
            (setq sig (match-string-no-properties 1))
            (if (not (assoc sig sig-list))
                (setq sig-list (append sig-list (list (cons sig t) )
                                 ))))
        )
      (forward-line) )
    ;(setq sig-list (aarray-sort sig-list 'string<))
;    (sort sig-list (lambda (a b)
 ;                    (string< (car a)(car  b)))
  ;        )

    ; search sig def for data type
    (beginning-of-buffer)
    (while (< (point) ap)
      (forward-line)
      (if (looking-at vhdl-re-decl-signal)
          ;;(aarray-add (list  (match-string 1)   ; id
            ;;                 (list (concat (match-string 2)  ; type
              ;;                             (match-string 3)) )) 
                ;;      'sig-decl) )  )
          (setq  sig-decl (append sig-decl (list  (cons (match-string-no-properties 1)   ; id
                                                        (concat (match-string-no-properties 2)  ; type
                                                                (match-string-no-properties 3)) )) 
                                  ))))
    (goto-char ap)

    (setq ix 0)
    ; insert assignment statement a
    (while (setq sig (car (nth ix sig-list)))
      (vhdl-indent-line)
      (insert-string sig)
      (insert-string " <= ")
      (setq type (cdr (assoc sig sig-decl)))
      ; see if it needs vector assignment
      (if (and sig-decl
               type)
          (if (or (string-match "vector" type)
                   (string-match "suv" type)
                   (string-match "slv" type)
                   (string-match "signed" type) )
              (insert-string "(others => '0');\n")
            (if (or (string-match "natural" type)
                    (string-match "integer" type))
                (insert-string "0;\n")
              (if (string-match "state_type" type)
                  (insert-string "ST_IDLE;\n")
                (insert-string " '0';\n") ;; all other types
                )))
        ;; else
        (insert-string "'0'; -- not declared\n") )
        
      (setq ix (+ 1 ix))
      )
    (forward-line -1)
    (vhdl-align-assignment)
    (vhdl-indent-line)
))

            
(defun test1 ()
 "identify vhdl block point blongs"
 (interactive)
    (modify-syntax-entry ?\n "> " )
    (char-syntax ?\n )
    (char-syntax ?\r )
   emacs-lisp-mode-syntax-table
 (vhdl-identify-block) 
;;     (search-backward-regexp  vhdl-block-re))

            (define-key global-map [(control f10)] 'test1)


(vhdl-disable-identify-block)

(forward-line 1)

(setq alist (list (list 3 4)))
(append alist (list (list 1 2)))


)
















(defun vhdl-capture-comment (s-cmt)
  "Capture comment that the pointer is current on and return in s-cmt"
  (interactive)
  (save-excursion
    (let ((cmt1 "--"))
    
      ;; go to the beginning of comment block
      (while (progn (beginning-of-line)
                    (looking-at vhdl-re-ln-comment))
        (forward-line -1))
      (forward-line)
      
      ;; gather comment lines
      (while (progn (beginning-of-line)
                    (looking-at vhdl-re-ln-comment))
        (setq cmt1 (concat cmt1 (match-string-no-properties 2) " "))
        (forward-line)
        )

      (set s-cmt cmt1)
      )))
    
  
      
    
  
(defun vhdl-add-state-declarations-above ()
  "Scan for states and add declaration.
Search for 'when XYZ =>' and capture state ID and comment in the following line, then
generate state type declaration. Comments are above 'when' clauses."
  (interactive)
  (let ((sig) ;; state signal name
        (case-list) ;; list of state info
        (case)   ;; list of 2 below items 
        (state)  ;; state id
        (cmt)    ;; comment on state
        (ix)     ;; list index
        (last-ix);; last item index of case-list
        (nest 0) ;; case statement nesting level
        )
    (save-excursion
      (vhdl-search-forward-re vhdl-re-st-case)
      (setq nest 1)
      (setq sig (match-string 1))
      (while (and (zerop (forward-line)) ;; moved
                  (or (not (looking-at vhdl-re-st-endcase))
                      (> nest 1)
                      )   )     
      
        (if (looking-at vhdl-re-st-case)
            (setq nest (+ nest 1 ) )   ;; inner case-statement is not a state machine
          (if (looking-at vhdl-re-st-endcase)
            (setq nest (- nest 1) )
              ))
          
            
        (if (and (looking-at vhdl-re-st-when)
                 (= nest 1) )
            (progn
              (setq state (match-string 1))
              (if (not (equal state "others"))
                  (progn
                    (forward-line -1)
                    (if (looking-at vhdl-re-ln-comment)
                        (progn
                          (vhdl-capture-comment 'cmt)
                          (forward-line 2) )
                      (setq cmt nil) )
                    (setq case-list (append case-list (list (list state cmt ))))
                ) )
              ) )  
        ) )
    (beginning-of-line)
    (vhdl-indent-line) ; start with right alignment
    (insert-string (concat "type " sig "_type is\n"))
    (vhdl-indent-line)
    (insert-string "(")
    (setq ix 0)
    (setq last-ix (- (length case-list) 1))
    (while (setq case (nth ix case-list))
      (vhdl-indent-line)

      (insert-string (car case))        ;; state id
      (if (> last-ix ix) 
          (insert-string ",") )
      (if (nth 1 case)
          (progn
            (insert-string (nth 1 case))   ;; comment
            (vhdl-align-inline-comment-group)
            ))
      ;;(vhdl-electric-return)
      (insert-string "\n")
      (setq ix (+ 1 ix))
      )
    (vhdl-indent-line)
    
    (insert-string ");\n")
    ;;(vhdl-electric-return)
    ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tmp
(defun print-list ()
  (setq xbuff (generate-new-buffer "*my output*"))

  (with-output-to-temp-buffer xbuff
    
    ;; this is inserted in current buffer
    (princ-list vhdl-record-list)
    
    )

  (switch-to-buffer xbuff )
  )

(defun tmp2 ()
  (print-list)
  )

(defun tmp1 ()
  (if t (progn
          (setq str "  535 abc <= 123;")
          (string-match "\\([a-z][a-z0-9_]*\\) *<= *\\([0-9]*\\);" str)
          (setq m1 (match-string-no-properties  1 str))
          (setq m2 (match-string-no-properties  2 str))
          (setq m0 (match-string-no-properties  0 str))
          (setq md (match-data))
          ))  ; abc <= 1234;
  (message m1)
(re-search-forward "\\([a-z]+\\) *<= *\\([-a-z0-9]+\\)" ) ; abc <= 123;
(setq sig-decl '(("abc" . "std_ulogic")
                 ("wr" . "boolean")
                 ("wr" . "std_ulogic")
                 ("data" . "std_ulogic_vector(7 downto 0)")))
(cdr (assoc "wr2" sig-decl))
(string-match "abc" nil)
(setq sig "wr")
(car nil)
)



;(toggle-debug-on-error)


