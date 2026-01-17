;;
;; vhdl/verilog file tree mode
;; written by toshi isogai
;; file names are button to open the file
;;

(defconst vtree-indent 4 "spaces for each indentation level")
(defvar vtree-outline-regexp "sdfadfas")
(defvar vtree-imenu-generic-expression "sadfasdf")

(defconst re-space0 "\\s-*")
(defconst re-hspace0 "[ \t]*")
(defconst re-wspace0 "\\(?:\\s-\\|\n\\)*")
(defconst re-space1 "\\s-+")
(defconst re-wspace1 "\\(?:\\s-\\|\n\\)+")
(defconst re-lead-space0 "^\\s-*")
(defconst re-OR "\\|")
(defconst re-open  "\\(")
(defconst re-open-grp  "\\(?:")
(defconst re-close "\\)")
(defconst re-open-opt  "\\(")  ;; same as re-open
(defconst re-close-opt "\\)?")
(defconst re-close-opt0 "\\)*")
(defconst re-close-opt1 "\\)+")
(defconst re-id "\\<\\([a-zA-Z][a-zA-Z0-9_]*\\)\\>")

(defconst re-filename (concat
                       re-open ;; 1
                       re-id   ;; 1
                       re-open-opt
                       "\\."
                       re-OR
                       "\\w"
                       re-close-opt0
                       re-close
                       ) 
  "file name regexp")


(defconst re-any (concat re-open-grp "." re-OR "\n" re-close )) 



(defun vtree-scan ()
  "scan vtree file and create buttons"
  (interactive)
  (let (
        (re-line (concat
                  re-open    ;;1
                  re-space0
                  re-id      ;;1
                  re-space0
                  "::"
                  re-space0
                  re-close  
                  re-open    ;; 1
                  re-filename ;; 2
                  re-space0
                  "$"
                  re-close
                  ) ) 
        (bpos) ;; button pos
        (filename)
        (adv)
        )
    (beginning-of-buffer)
    (while (< (point) (point-max))
      (if (looking-at "abc")
          (progn
           (setq bpos (length (match-string 1)))
           (setq adv (length (match-string 3)))
           (setq filename (match-string 4))
           (forward-char bpos)
            (make-annotation
             [button
                     :face dired-face-marked
                     :descriptor "Open"
                     :callback-ex
                     (lambda (image-instance event)
                       ;(find-file filename) 
                       (message (integer-to-string (point)))
                       )]
             (point)
             'text
             )
            
;            (forward-char adv) 
        (forward-line 1)
            )
        (forward-line 1)
))))


(provide 'vtree)
          
;;
;;
;;(defvar vtree-mode-syntax-table
;;  (let ((st (make-syntax-table)))
;;    (modify-syntax-entry ?# "." st)
;;    (modify-syntax-entry ?\n ">" st)
;;    (modify-syntax-entry ?_ "w" st)
;;    (modify-syntax-entry ?\- ". 12" (syntax-table))
;;    st)
;;  "Syntax table for `vtree-mode'.")
;;
;;;;;###autoload
;;(define-derived-mode vtree-mode fundamental-mode "Vtree"
;;  "A major mode for editing Vtree files."
;;  ;  :syntax-table vtree-mode-syntax-table
;;  (set (make-local-variable 'comment-start) "--")
;;  (set (make-local-variable 'comment-start-skip) "--+\\s-*")
;;  (set (make-local-variable 'indent-line-function) 'vtree-indent-line)
;;  (setq font-lock-defaults '(vtree-keywords))
;;  (setq mode-name "vtree")
;;  ;(set (make-local-variable 'imenu-generic-expression)
;;  ;    vtree-imenu-generic-expression)
;;  ;(set (make-local-variable 'outline-regexp) vtree-outline-regexp)
;;
;;  )
;;
;;;;; Indentation
;;
;;(defun vtree-indent-line ()
;;  "Indent current line of Vtree code."
;;  (interactive)
;;  (let ((savep (> (current-column) (current-indentation)))
;;        (indent (condition-case nil (max (vtree-calculate-indentation) 0)
;;           (error 0))))
;;    (if savep
;;        (save-excursion (indent-line-to indent))
;;      (indent-line-to indent))))
;;
;;
;;(defun vtree-calculate-indentation ()
;;  "Return the column to which the current line should be indented."
;;  (interactive)
;;  (let ((cc)
;;        (newc)
;;        (un-ind)
;;        (level) ; paren level
;;        )
;;    (beginning-of-line)
;;    (if (looking-at (concat re-space0 "----"))
;;        ;; don't move ---- comment line
;;        (progn
;;          (search-forward-regexp "\\s-*")
;;          (current-column)
;;          )
;;      
;;      (if (looking-at (concat re-hspace0 vtree-re-block-end))
;;          (setq un-ind t)) ; un-indent later
;;      (save-excursion
;;        (while (and (not newc) (>= (forward-line -1) 0))
;;          (search-forward-regexp-in-line "\\s-*") ; first non-sp
;;          (setq cc (current-column))
;;          (cond ((looking-at vtree-re-block-start)
;;                 (setq newc (+ cc vtree-indent))   ; in a block
;;                 )
;;                ((looking-at (concat re-open
;;                                     ".*)"
;;                                     re-OR
;;                                     ".*("
;;                                     re-close))       ; contains ( or )
;;                 ;; count difference between ( and )
;;                 (setq level 0)
;;                 (beginning-of-line)
;;                 (while (search-forward-regexp-in-line "(")
;;                   (setq level (+ level 1))
;;                   )
;;                 (beginning-of-line)
;;                 (while (search-forward-regexp-in-line ")")
;;                   (setq level (- level 1))  
;;                   )
;;                 (setq newc (+ cc (* level vtree-indent)))   ; in a paren
;;                 )
;;                ((looking-at "\\s-*\\(----\\|$\\)") 
;;                 ;;ignore blankline and ---- comment line
;;                 )
;;                (t
;;                 (setq newc cc)
;;                 )
;;                )
;;          ))
;;      (end-of-line)
;;      (if un-ind
;;          (- newc vtree-indent)
;;        newc )
;;      )
;;) )
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;;;
;;;;
;;;;(defvar vtree-mode-map
;;;;  (let ((map (make-sparse-keymap)))
;;;;    (define-key map [foo] 'vtree-do-foo)
;;;;    map)
;;;;  "Keymap for `vtree-mode'.")
;;;;
;;;;(defvar vtree-mode-syntax-table
;;;;  (let ((st (make-syntax-table)))
;;;;    (modify-syntax-entry ?# "<" st)
;;;;    (modify-syntax-entry ?\n ">" st)
;;;;    st)
;;;;  "Syntax table for `vtree-mode'.")
;;;;
;;;;(defvar vtree-font-lock-keywords
;;;;  '(("function \\(\\sw+\\)" (1 font-lock-function-name-face)))
;;;;  "Keyword highlighting specification for `vtree-mode'.")
;;;;
;;;;(defvar vtree-imenu-generic-expression )
;;;;  ...)
;;;;
;;;;(defvar vtree-outline-regexp )
;;;;  ...)
;;;;
;;;;;;;###autoload
;;;;(define-derived-mode vtree-mode fundamental-mode "Vtree"
;;;;  "A major mode for editing Vtree files."
;;;;  :syntax-table vtree-mode-syntax-table
;;;;  (set (make-local-variable 'comment-start) "# ")
;;;;  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
;;;;  (set (make-local-variable 'font-lock-defaults)
;;;;       '(vtree-font-lock-keywords))
;;;;  (set (make-local-variable 'indent-line-function) 'vtree-indent-line)
;;;;  (set (make-local-variable 'imenu-generic-expression)
;;;;       vtree-imenu-generic-expression)
;;;;  (set (make-local-variable 'outline-regexp) vtree-outline-regexp)
;;;;  ...)
;;;;
;;;;;;; Indentation
;;;;
;;;;(defun vtree-indent-line ()
;;;;  "Indent current line of Vtree code."
;;;;  (interactive)
;;;;  (let ((savep (> (current-column) (current-indentation)))
;;;;        (indent (condition-case nil (max (vtree-calculate-indentation) 0)
;;;;           (error 0))))
;;;;    (if savep
;;;;        (save-excursion (indent-line-to indent))
;;;;      (indent-line-to indent))))
;;;;
;;;;
;;;;
;;;;(provide 'vtree)
;;;;