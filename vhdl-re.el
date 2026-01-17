;;
;; collection of regexp for vhdl
;; Toshi Isogai 2010-2018
;;


;;
;; vhdl primitives
;;

;; ID
(defconst vhdl-re-id    "\\(\\<[a-zA-Z][a-zA-Z0-9_.]*\\>\\)" "identifier regexp")
;; leading space (just ' ' \t)
(defconst vhdl-re-lead-sp0 "^[ \t]*" "leading spaces or tabs")
;; operator - assignment
(defconst vhdl-re-op-assign "[<:]="  "assignment operator regexp")

(defconst vhdl-re-blank-comment-line "^\\s-*\\(--\\)"
  "blank or comment line. comment is backreferenced.")
(defconst vhdl-re-blank-line "^\\s-*$"
  "blank line")

(defconst vhdl-re-down-or-up "downto\\|to")
;; slice - ID with partial (or full) slice of bits
(defconst vhdl-re-slice    "\\(\\<[a-zA-Z][a-zA-Z0-9_.]*\\> *(.* \\(downto\\|to\\).*)\\)" "identifier regexp")

;;
;; statements/openings/conditions
;;

;; case
(defconst vhdl-re-st-case 
  (concat vhdl-re-lead-sp0
          "\\<case\\>" 
          re-wspace1
          re-open
            vhdl-re-id  ;; 1
          re-OR
            vhdl-re-slice ;;2
          re-close-grp
          re-wspace1
          "\\<is\\>" )
  "case statement opening regexp" )

;; end case
(defconst vhdl-re-st-endcase 
  (concat vhdl-re-lead-sp0
          "\\<end\\>" 
          re-wspace1
          "\\<case\\>" )
  "end case regexp" )

;; end process
(defconst vhdl-re-st-endprocess 
  (concat vhdl-re-lead-sp0
          "\\<end\\>" 
          re-wspace1
          "\\<process\\>" )
  "end process regexp" )

;; end 
(defconst vhdl-re-st-end
  (concat vhdl-re-lead-sp0
          "\\<end\\>" 
          )
  "end regexp" )

;; begin 
(defconst vhdl-re-st-begin
  (concat vhdl-re-lead-sp0
          "\\<begin\\>" 
          )
  "begin regexp" )

;; when
(defconst vhdl-re-st-when 
  (concat vhdl-re-lead-sp0
          "\\<when\\>" 
          re-wspace1
          vhdl-re-id  ;; 1
          re-wspace0
          "=>" )
  "when condition regexp" )

;; comment line
(defconst vhdl-re-ln-comment
  (concat vhdl-re-lead-sp0
          re-open  ;; 1
          "---*\\s-*"
          re-open  ;; 2 trimmed comment 
          ".*?"
          re-close
          "\\s-*$"
          re-close
          )
  "comment line regexp" )

;; assignment statement
(defconst vhdl-re-st-assign 
  (concat
   vhdl-re-lead-sp0
   vhdl-re-id       ;; 1
   re-wspace0
   re-open-opt-grp
   "(.*)"   
   re-close-opt
   re-wspace0
   vhdl-re-op-assign
   re-wspace0
   re-rest
   ";"
   ) )

;; signal declaration (flattened)
(defconst vhdl-re-decl-signal
  (concat
   vhdl-re-lead-sp0
   "\\<signal\\>"
   re-wspace1
   vhdl-re-id       ;; 1
   re-wspace0
   ":"
   re-wspace0
   re-open-opt-grp
     re-open          ;; 1
       "[^;:]+?"       ;; type with comment
     re-close
     re-open-opt-grp
       ":="
       re-wspace0
       "[^:;]+"
     re-close-opt
     "--.*"
   re-OR
     re-open          ;; 1
       "[^;:]+"         ;; type without comment
     re-close
     re-open-opt-grp
       ":="
       re-wspace0
       re-open
       "[^;]+"       ;; 1 initial value
       re-close
     re-close-opt
   re-close-grp
   re-wspace0
   ";"
   ) 
  "signal declaration (flattened) regexp"
  )

;; signal declaration (flattened)
(defconst vhdl-re-decl-variable
  (concat
   vhdl-re-lead-sp0
   "\\<variable\\>"
   re-wspace1
   vhdl-re-id       ;; 1
   re-wspace0
   ":"
   re-wspace0
   re-open-opt-grp
     re-open          ;; 1
       "[^;:]+?"       ;; type with comment
     re-close
     re-open-opt-grp
       ":="
       re-wspace0
       "[^:;]+"
     re-close-opt
     "--.*"
   re-OR
     re-open          ;; 1
       "[^;:]+"         ;; type without comment
     re-close
     re-open-opt-grp
       ":="
       re-wspace0
       re-open
       "[^;]+"       ;; 1 initial value
       re-close
     re-close-opt
   re-close-grp
   re-wspace0
   ";"
   ) 
  "signal declaration (flattened) regexp"
  )

;; port declaration (flattened)
(defconst vhdl-re-decl-port
  (concat
   vhdl-re-lead-sp0
   vhdl-re-id       ;; 1 id
   re-wspace0
   ":"
   re-wspace0
   re-open          ;; 2 mode
     "in"
   re-OR
     "out"
   re-OR
     "inout"
   re-OR
     "buffer"   
   re-close
   re-wspace1
   re-open re-non-semi re-close-short  ;; 3 type
   re-wspace0
   ";"
   ) 
  "port declaration (flattened) regexp"
  )

(defconst vhdl-re-decl-constant
  (concat
   vhdl-re-lead-sp0
   re-open          ;; 1 to identify it is a constant
   "\\<constant\\>"
   re-close
   re-wspace1
   vhdl-re-id       ;; 1
   re-wspace0
   ":"
   re-wspace0
   re-open          ;; 1
     "[^;:]+"         ;; type without comment
   re-close

   ":="
   re-wspace0
   re-open
     "[^;]+"       ;; 1 initial value
   re-close
   re-wspace0
   ";"
   ) 
  "constnt declaration (flattened) regexp"
  )


(defconst vhdl-re-decl-type-record
  (concat
   vhdl-re-lead-sp0
   re-open          ;; 1 to identify it is a type
     "\\<type\\>"
   re-close
   re-wspace1
   vhdl-re-id       ;; 1
   re-wspace0
   "is"
   re-wspace0
   re-open          ;; 1
     "record"         
   re-close
   ) 
  "type record declaration regexp"
  )
(defconst vhdl-re-st-endrecord
  (concat vhdl-re-lead-sp0
          "\\<end\\>" 
          re-wspace1
          "\\<record\\>" )
  "end record regexp" )

(defconst vhdl-re-decl-type
  (concat
   vhdl-re-lead-sp0
   re-open          ;; 1 to identify it is a type
     "\\<type\\>"
     re-OR
     "\\<subtype\\>"
   re-close
   re-wspace1
   vhdl-re-id       ;; 1
   re-wspace0
   "is"
   re-wspace0
   re-open          ;; 1
     "[^;:]+"         ;; type without comment
   re-close
   ) 
  "type record declaration regexp"
  )

(defconst vhdl-re-decl-record-element
  (concat
   vhdl-re-lead-sp0
   vhdl-re-id       ;; 1
   re-wspace0
   ":"
   re-wspace0
   re-open          ;; 1
     "[^;:]+"         ;; type without comment
   re-close
   ) 
  "element in type record declaration regexp"
  )


