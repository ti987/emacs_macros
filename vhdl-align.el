;;
;; vhdl align stuff
;; Toshi Isogai 2010 - 2018
;;
;(defun vhdl-align-flattened-signal-declarations ()
;(defun vhdl-colon-align (&optional blank-cont)
;(defun vhdl-tok-align-arg (tok col)
;(defun vhdl-param-align ()
;(defun vhdl-align-assignment ()

(defvar vhdl-align-type-ind-column 28) ; colon in sig, var, const
(defvar vhdl-align-type-ind-column-alt 30) 
(defvar vhdl-align-port-colon-column 22) ; colon in other declarations
(defvar vhdl-align-port-colon-column-alt 30) 
(defvar vhdl-align-assoc-column 26) 
(defvar vhdl-align-assoc-column-alt 34) 
(defvar vhdl-align-comment-column 48)
(defvar vhdl-align-comment-column-alt 67)
(defvar vhdl-align-assign-column-offset 28) ;; column offeset for "<=" or ":="
(defvar vhdl-align-assign-column-offset-alt 46) ;; column offeset for "<=" or ":="
(defvar vhdl-align-signal-sort t) ;; sort signals after aligning if non-nil

(defcustom vhdl-inline-comment-column vhdl-align-comment-column
  "*Column to indent and align inline comments to.
Overrides local option `comment-column'.

NOTE: Activate the new setting in a VHDL buffer by using the menu entry
      \"Activate Customization\"."
  :type 'integer
  :group 'vhdl-comment)

(setq comment-column vhdl-inline-comment-column)



(defun vhdl-align-flattened-signal-declarations ()
  "align signal declarations"
  (interactive)
  ;; find blank line
  (while (progn
           (beginning-of-line)
           (not (looking-at "\\s-*\\(--.*\\)?$")) 
           )
    (forward-line -1) )
  (forward-line)

  (let( (bp (point))
        )
    
    (while (looking-at "^\\s-*\\(signal\\|variable\\|constant\\)\\>")
      ;;leading 2 spc 
      (beginning-of-line)
      
      (if (looking-at  "^\\s-*")
          (progn
            (delete-char (- (match-end 0) (point)))
            (insert-string "  "))    )
      
      ;; spc after signal
      (re-search-forward "\\(signal\\|variable\\|constant\\)")
      
      (if (looking-at  "\\s-*")
          (replace-match " "))
      ;; if id extends beyond col 28, remove spc
      (search-forward-regexp "[a-zA-Z0-9_]+")
      (if (>= (current-column) column)
          (if (looking-at  "\\s-*")
              (progn
                (delete-char (- (match-end 0) (point)) )
                (insert-string " "))    )
        ;;else set : at col 28
      (if (looking-at  "\\s-*")
          (progn
            (delete-char (- (match-end 0) (point)) )
            (insert-char ?\  (- column (current-column)) )
            ))
      )
      (forward-line)
      (beginning-of-line)
      ) ;; end of while
    (if vhdl-align-signal-sort
        (sort-lines nil bp (point)) )
    )
)


(defun vhdl-colon-align-alt ()
  (interactive)
  ( vhdl-colon-align nil
                     vhdl-align-comment-column-alt
                     vhdl-align-type-ind-column-alt
                     vhdl-align-port-colon-column-alt
                     )
  )
  

(defun vhdl-colon-align (&optional blank-cont opt-comment-column opt-type-column opt-port-column)
  ;; align colon in declaration
  ;; if blank-cont is true, continue search block through blank lines
  ;; if opt-comment-column is defined, align comment at that number
  (interactive)
  (let (
        (comment-column vhdl-align-comment-column)
        (type-column vhdl-align-type-ind-column)
        (port-column vhdl-align-port-colon-column)
        )
    (if opt-comment-column
        (setq comment-column opt-comment-column) )
    
    (if opt-type-column (setq type-column opt-type-column) )
    (if opt-port-column (setq port-column opt-port-column) )

    
    ;; seach block beginning
    (while (progn
             (beginning-of-line)
             (or (looking-at "\\(\\s-*--\\|.*:\\)") ; comment line or : is in
                 (and blank-cont (looking-at "\\s-*$"))))
      (forward-line -1) )
    (forward-line 1)  ; should be at the first statememnt in a block
    
    
    (while (progn
             (beginning-of-line)
             (or (looking-at "\\(\\s-*--\\|.*:\\)") ; comment line or : is in
                 (and blank-cont (looking-at "\\s-*$"))))
      (beginning-of-line)
      (if (not (looking-at "\\s-*--"))
                                        ; skip comment only line
          (progn
            (if (looking-at "^\\(\\s-*\\(signal\\|variable\\|constant\\)\\)\\s-*")
                                        ; give a little more room for name
                (progn
                  (replace-match (match-string 1))
                  (insert-char ?\  1)
                  (vhdl-tok-align-arg ":" type-column) )
              (vhdl-tok-align-arg ":" port-column) 
              (if (looking-at "\\s-*\\(in \\|out\\)\\s-*")
                  (replace-match (concat (match-string 1) " "))  ; align in and out
                )
              ;;            (if (looking-at "\\s-*")
              ;;               (replace-match (concat (match-string 0) ""))  ; align in and out
              ;;              )
              )
            (vhdl-tok-align-arg "--" comment-column)
            )
        )
      (forward-line 1) 
      )
    )
  )

  


(defun vhdl-tok-align-arg (tok col)
  "align 'tok' position at column 'col'.
search token (tok) in the current line. if found move the rest of the line to
column postion (col)."

  (vhdl-indent-line)
  (let (
        (tlen)
        (ccn) ; current column number
        (ep) ; eol position
        )
    (end-of-line)
    (setq ep (point))
    (beginning-of-line)
    (if (vhdl-search-forward-re tok ep t) ; if seach to eol and found
        (progn
          (setq tlen (length (match-string 0)))
          (backward-char tlen )
          (setq ccn (current-column))
          
          (if (> ccn col)
                                        ; remove spaces
              (progn 
                (delete-horizontal-space)
                (setq ccn (current-column))
                )
            )

          (if (< ccn col)
              ; add spaces
              (progn
                (insert-char ?\  (- col ccn))
                )
;           (if (> ccn col)
;                                       ; remove spaces
;               (progn 
;                 (forward-char -1)
;                                       ; if name is too long stop deleting
;                 (while (and (>= (current-column) col) (looking-at "\\s-"))
;             (delete-char)
;             (forward-char -1)
;             )
;                 (forward-char 1) ; cursor on  token
;                 )
;             )
            )
          (forward-char tlen) ; cursor on char after token
          (if (not (looking-at "\\s-*$"))
              (progn
                (insert-char ?\   1) ; insert space to start with. erase the rest
                (delete-spaces)))
          )
      )
    )
  )

(defun vhdl-param-align (&optional opt-assoc-column opt-comment-column)
  "align name associated parameters in instance, procedure, and function.
If optional <opt-assoc-column> and <opt-comment-column> are given, align association and
comment respectively. Otherwise align at <vhdl-align-assoc-column> and 
<vhdl-align-comment-column>."
  ;;"Also align assignments."
  (interactive)
  (let (
        (comment-column vhdl-align-comment-column)
        (assoc-column vhdl-align-assoc-column)
        )
    (if opt-comment-column (setq comment-column opt-comment-column) )
    (if opt-assoc-column (setq assoc-column opt-assoc-column) )

    
    ;; seach block beginning
    (while (progn
             (beginning-of-line)
             (and
              (not (looking-at "\\s-*\\([a-zA-Z0-9_]+\\s-*:\\|port\\s-+map\\)"))
              (looking-at "\\(\\s-*--\\|.*=>\\|.*<=\\)") ; comment line or => is in
              )
             )
      (forward-line -1) )
    (forward-line 1)  ; should be at the first statememnt in a block
    
    (while (progn
             (beginning-of-line)
             (looking-at "\\(\\s-*--\\|.*=>\\|.*<=\\)") ; comment line or : is in
             )
      
      (if (looking-at "\\s-*[a-zA-Z0-9_.()]+\\s-*=>")
          ;; skip comment only line
          (progn
            (vhdl-tok-align-arg "=>" assoc-column) 
            (vhdl-tok-align-arg "--" comment-column)
            )
        (if (looking-at "\\s-*[a-zA-Z0-9_.()]+\\s-*<=")
            (progn
              (vhdl-tok-align-arg "<=" assoc-column) 
              (vhdl-tok-align-arg "--" comment-column)
              )
          )
        )
      (forward-line 1)
      )
    )
  )

(defun vhdl-param-align-alt ()
  "align name associated parameters in instance, procedure, and function.
align at association at <vhdl-align-assoc-column-alt> and comment at
<vhdl-align-comment-column-alt>."
  (interactive)
  (vhdl-param-align vhdl-align-assoc-column-alt vhdl-align-comment-column-alt)
  )

(defun vhdl-align-assignment (&optional offset)
  "align assignment statement.
If optional <offset> is given, align at <offset>.
Otherwise align at <vhdl-align-assign-column-offset>."
  (interactive)
  (let ((col)
        (column-offset)
        )
    
    (if offset (setq column-offset offset)
      (setq column-offset vhdl-align-assign-column-offset))
    
    (beginning-of-line)
    ;; search up until not an assignment
    (while (and (looking-at vhdl-re-st-assign)
                (zerop (forward-line -1) ))  )

    (while (and (zerop(forward-line 1) )
                (looking-at vhdl-re-st-assign) )
      (vhdl-indent-line)
      (search-forward-regexp re-space0)
      (setq col (+ (current-column) column-offset))
      (vhdl-tok-align-arg vhdl-re-op-assign col)
      )
    ) )

(defun vhdl-align-assignment-alt ()
    "align assignment statement with <vhdl-align-assign-column-offset>"
  (interactive)
  (vhdl-align-assignment vhdl-align-assign-column-offset-alt)
  )

;;
;; menu stuff
;;
(defvar vhdl-align-menu-list 
  '("Align"
    ["align-flattened-signal-declarations" (vhdl-align-flattened-signal-declarations)]
    ["colon-align" (vhdl-colon-align)]
    ["param-align" (vhdl-param-align)]
    ["align-assignment" (vhdl-align-assignment)]
    ) )

(defun vhdl-update-align-menu ()
  "Update VHDL align menu."
  (interactive)
  (easy-menu-remove vhdl-align-menu-list) ; for XEmacs
  (easy-menu-add vhdl-align-menu-list)   ; for XEmacs
  (easy-menu-define vhdl-align-menu vhdl-mode-map
                    "Align Menu" vhdl-align-menu-list)
  )


(defun test1 ()
 "identify vhdl block point blongs"
 (interactive)
;    (modify-syntax-entry ?\n " " )

    (vhdl-align-assignment)

(setq vhdl-align-assign-column-offset 32) ;; column offeset for "<=" or ":="
(setq vhdl-align-assign-column-offset-alt 40) ;; column offeset for "<=" or ":="
)

            (define-key global-map [(control f10)] 'test1)
