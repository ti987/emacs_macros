;;
;; lightning keys
;;   hit the same 3 times or more will replace them to user defined strings
;; 
;;  toshi@isogai.us - 2016
;;
;;  Example usage in .emacs
;;    (load-file "~/emacs/lightning-keys.el")
;;    (setq scad-key-list (list
;;             '("d"
;;         (list
;;          '("dd")
;;          '("difference() {")
;;          )  )
;;        ... rest of lightning key list ....
;;     )
;;
;;   (add-hook 'scad-mode-hook 'scad-mode-init)
;;   
;;   (defun scad-mode-init ()
;;     (lightning-keys-setup 'scad-mode 'scad-mode-map
;;                           scad-lightning-key-list )
;;     (setq scad-indent-style "stroustrup")
;;     (turn-on-font-lock)
;;     )
;;
;;  Replacement list
;;    '("case  is" 5 t)
;;    arguments
;;    1 replacement string
;;    2 the number of position from the original point to advance
;;    3 regex use (set it t when multiple lines are in replacement string)
;;    4 count consecutive spaces as 1 for arg 2
;;

(defvar lightning-ix) ; index of lightning insert string list
(defvar lightning-pos 0 "Pointer position before 'electric' insertion is done")
(defvar lightning-buf-mode nil "Major mode that this buffer uses. set when lightning-key-setup is called.")
(defvar lightning-buf-key-map nil "Key map that this buffer uses. set when lightning-key-setup is called.")
(defcustom lightning-replace-any-comb nil "If non nil, any key combinations can be replaced. Be careful!")

(defun lightning-keys-setup (mode key-map key-list)
  "setup lightning keys <key-list> in <key-map>"
  (interactive)
  (setq lightning-buf-mode mode)
  (setq lightning-buf-key-map key-map)
  
  (let ((klist) (elist) (key) (trans-list) (sexp))
    (setq klist key-list)
    
    (while (setq elist (car klist))
      (setq key (car elist))
      (setq trans-list (nth 1 elist))
      (setq sexp (concat "(define-key "
                         (symbol-name key-map) " \"" key "\"" 
                         " '(lambda () (interactive) (lightning-key "
                         "\"" key "\" " (prin1-to-string trans-list ) ") ) )" ) )
      (eval-string sexp)
      (setq klist (cdr klist))
      ))   
)

(defun lightning-key (key trans-list) 
  "translates key according to trans-list"
  (interactive "p")
  (lightning-replace-key key
                         trans-list
                         (1- (length trans-list))
                         1)
)

    

(defun lightning-replace-key (key elist esize count) 
  "replace typed or replaced string if it matches an element with the next element in the list"
  (let ((eix 0)
        (replaced nil)
        )
    (setq this-command key) ; keep the current command
    (if (or lightning-replace-any-comb (eq last-command key))
        (while (and (not replaced) (<= eix esize))
          (if (lightning-delete-if-inserted (nth 0 (nth eix elist))
                                          (nth 2 (nth eix elist)) ) ;; regexp option
              (progn
                (setq replaced t)
                ;(goto-char lightning-pos)
                (if (= eix esize)
                    (setq eix 0)   ; loop back
                  (setq eix (1+ eix)) )
                (lightning-insert (nth 0 (nth eix elist))
                                  (nth 1 (nth eix elist))  ;; insert point if exists
                                  (nth 3 (nth eix elist)) ) ;; literal mode. count spaces (other than initial ones)
                )  
            (setq eix (1+ eix))  
            ) ) )
    (if (not replaced)
        (progn
          (self-insert-command count)
          (setq lightning-pos (point)) ; reset
          ) )
    ))



(defun lightning-delete-if-inserted (str &optional exp-sp)
  "If text matches to 'str', delete 'str' and return t otherwise return nil.
If 'exp-sp' is t, move pointer back 'lightning-pos', replace '\n' in 'str' 
with '\n\\s-*' then delete 'str' if matches."
  (if exp-sp
      (progn
        (let ((str2 (replace-in-string str "\n *" "\n\\s-*"))
              (bpos (point))
              )
          (setq str2 (concat "\\s-*" str2))
          ;; protect [ and ] 
          (setq str2 (replace-regexp-in-string
                      "\\(\\[\\|\\]\\)"  "\\\\\\1" str2  t nil))

          (goto-char lightning-pos)
          (if (looking-at str2)
              (progn 
                (replace-match "")
                t)
            (goto-char bpos)
            nil
            )
          ))
      
    (if (search-backward str (- (point) (length str)) t )
        (progn 
          (delete-char (length str))
          t))  
    )
)

(defconst re-open  "\\(")
(defconst re-close "\\)")
(defconst re-OR "\\|")
(defconst re-lightning-line (concat re-open ".+" re-OR "\n" re-close))

(defun lightning-insert (str &optional pos cnt-sp) 
  "Insert 'str' using electric to align.
If 'str' contains '\n', then use lightning-terminate-line.
If pos is a number move pointer formard 'pos' from original position,
otherwise leave pointer at the end of 'str'.
If cnt-sp is t, count each space characters as 1.
"
  (interactive)
  (setq lightning-pos (point)) ; global
  (let (
        (ix 0)
        (ix2)
        (substr)
        (bpos (point))
        )
    
      ;; insert string line by line
      (while
          (string-match re-lightning-line str ix)
        (setq ix2 (match-end 0))
        (setq substr (substring str ix ix2))
        (setq ix ix2)
        (if (equal substr "\n")
            (lightning-terminate-line)
          (insert substr)
          (lightning-tab)
          )
        )
      ;; if pos is a number move pointer formard 'pos' from original position
      (if pos
          (progn
            (goto-char bpos)
            ; skip initial spaces
            (if (looking-at "\\s-")
                (search-forward-regexp "\\s-+") )

            (if  cnt-sp
                (forward-char pos) ;; count each space as 1
              
              ;; count all spaces as 1
              (while (> pos 0) 
                (if (looking-at "\\(\\s-\\|$\\)")
                    (search-forward-regexp "\\(\\s-\\|\\)+")
                  (forward-char 1)  )
                (setq pos (- pos 1))
                ))
              ))
        
        

      (lightning-tab)
    )
  )

(defun lightning-tab ()
  (funcall (key-binding (kbd "TAB")))
  )
(defun lightning-terminate-line ()
  (funcall (key-binding (kbd "RET")))
  )
  

