;;
;; vhdl trace stuff
;; Toshi Isogai 2005-2010
;;
;(defun vhdl-trace-port-up ()
;(defun vhdl-trace-port (&optional n m)


(defvar vhdl-trace-module nil)
(defvar vhdl-trace-formal nil)
(defvar vhdl-trace-file nil)
(defvar vhdl-buf-stack nil)  ;; buffer stack for jump into sub-modules

(defvar keyword "^\\s-*\\(signal\\|variable\\|procedure\\|function\\)\\>")

(defun vhdl-trace-port-up ()
  "back up to vhdl file where trace port was performed"
  (interactive)
  (let ((cur-dir (file-name-directory buffer-file-name)))
    (other-window 1)
    (cd cur-dir)
    (find-file (pop vhdl-buf-stack)))
  )

(defun vhdl-trace-port (&optional n m)
  "jump to formal parameter in the module file"
  (interactive)
    (modify-syntax-entry ?_ "w" (syntax-table))
    (let ((curpnt (point))
          (loopcnt)
          )
      (end-of-line)
      (beginning-of-line)
      (vhdl-re-search-forward "\\(=>\\|:\\|$\\)")
      (if (equal (match-string 1) "=>")
          ;; parameters in asscociation
          (progn 
            (beginning-of-line)
            ;; get formal parameter
            (search-forward-regexp "\\<\\([a-zA-Z0-9_]+\\)\\s-*=>")
            (setq vhdl-trace-formal (match-string-no-properties 1))
            ;; find module or function
            (setq loopcnt 1000)
            (while 
                (and (not (vhdl-trace-find-mod-func curpnt)) 
                     (loopcnt > 0))
              (setq loopcnt (- loopcnt 1))
              )
            )   
        ;; else - not parameters
        (if (equal (match-string 1) ":")
            ;; possible port name
            (if (looking-at "\\s-*process\\>")
                (goto-char curpnt);; skip process line
              ;; else - port name
              (progn
                (beginning-of-line)
                (if (looking-at "\\s-*attribute\\>")
                    ;; attribute - ignore 
                    (goto-char curpnt);; skip process line
                  (progn
                    (vhdl-re-search-forward "[a-zA-Z0-9_]+")
                    (setq vhdl-trace-formal (concat "\\<" (match-string-no-properties 0) "\\>"))
                    )))))
        (vhdl-trace-find-formal)
        )
      )
  ;(vhdl-trace-find-formal)
  )

(defun vhdl-trace-find-formal ()
  "\
find formal parameter in architecture
"
  ;; search formal which doesn't start with declaration keyword nor isn't a formal parameter
  (let ((formal vhdl-trace-formal))
    (beginning-of-line)
    (while (progn
             (let ((ep))
               (vhdl-re-search-forward formal)
                 (save-excursion
                   (end-of-line)
                   (setq ep (point)) )
                 ;; continue if it is declaration
                 ;; exit if not
                 (or
                  (vhdl-re-search-forward 
                   (concat vhdl-trace-formal "\\s-*=>" )  ;formal parameter, skip it
                   ;; move pointer beyond formal
                   ep t )  ;; upto eol, no error
                  (vhdl-re-search-forward 
                   (concat keyword "\\s-+" vhdl-trace-formal)  ; declaration, skip it
                   ;; move pointer beyond formal
                   ep t )  ;; upto eol, no error
                     )
                 )))
    (vhdl-re-search-forward formal) ;; pointer has moved to bop
    )
  )

(defun vhdl-trace-find-mod-func (curpnt)
  "\
find module name or func name trying the name as it is 
first if not found downcase the name.
It searches in the order of buffers, current directory, child directries, 
parent directory, and sibling directories. No recursive directory 
search is performed.
return true if formal is found in new module or function.
"
  ;; push buffer on stack for backtracking
  (push (buffer-file-name) vhdl-buf-stack)

  (let ((cur-dir (file-name-directory buffer-file-name)))
  
  ;; we don't need to consider functions, do we?
  ;;(search-backward-regexp "\\(:\\|(\\)")
  ;(search-backward-regexp "\\(:\\)")   
  (vhdl-re-search-backward "\\(:\\)")   
  (cond ((equal (match-string 1) ":")
         ;; module line
         (vhdl-re-search-forward "[a-zA-Z0-9_]+")
         
         ;; keep module name
         (if (or (equal (match-string 0) "entity")
                 (equal (match-string 0) "ENTITY"))
             ;; in-form-of "entity work.sync_mod"
             (if (looking-at "\\s-*[a-zA-Z0-9_]+\.\\s-*\\([a-zA-Z0-9_]+\\)")
                 (setq vhdl-trace-module (match-string-no-properties 1))
               (vhdl-re-search-forward "[a-zA-Z0-9_]+")
                 (setq vhdl-trace-module (match-string-no-properties 0))
                 )
           (setq vhdl-trace-module (match-string-no-properties 0)) ;; just module name
           )

         (cd cur-dir)

         ;; assume file is vhdl-trace-module + .vhd
         (setq vhdl-trace-file (concat vhdl-trace-module ".vhd"))

         ;; find in directory, buffer, else give up
         (if (file-readable-p vhdl-trace-file)
             ; file exist?
             (progn
               (goto-char curpnt)
               (other-window 1)
               (cd cur-dir)
               (find-file vhdl-trace-file) )
           ;; else
           (if (get-buffer vhdl-trace-file)
               ;; already file in buffer ring
             (progn
               (goto-char curpnt)
               (other-window 1)
               (switch-to-buffer (get-buffer vhdl-trace-file)))
             ;; else downcase file name
             (setq vhdl-trace-module (downcase vhdl-trace-module))
             (setq vhdl-trace-file (concat vhdl-trace-module ".vhd"))
             (if (get-buffer vhdl-trace-file)
                 ;; already file in buffer ring
                 (progn
                   (goto-char curpnt)
                   (switch-to-buffer (get-buffer vhdl-trace-file)))
               (if (file-exists-p vhdl-trace-file)
                   (progn
                     (other-window 1)
                     (cd cur-dir)
                     (find-file vhdl-trace-file)
                     )
                 ;; else try other directories
                 (let ((vhdl-trace-file-2)) 
                   
                   ; search child directory
                   ; only the first file in alphabetical search. 
                   (setq vhdl-trace-file-2 
                           (car (file-expand-wildcards
                                 (concat "./*/" vhdl-trace-file))))
                   (if vhdl-trace-file-2
                       (progn
                         (other-window 1)
                         (cd cur-dir)
                         (find-file vhdl-trace-file-2)
                         )
                     ;; search parent directory
                     (setq vhdl-trace-file-2 (concat "../" vhdl-trace-file))
                     (if (file-exists-p vhdl-trace-file-2)
                         (progn
                           (other-window 1)
                           (cd cur-dir)
                           (find-file vhdl-trace-file-2)
                                        ;(message-box 
                                        ; (concat vhdl-trace-file-2 " found in the parent directory")) 
                           )
                       ;; else try sibling directories
                       (setq vhdl-trace-file-2 
                             (car (file-expand-wildcards
                                   (concat "../*/" vhdl-trace-file))))
                       (if vhdl-trace-file-2
                           (progn
                             (buffer-file-name)
                             (other-window 1)
                             (cd cur-dir)
                             (find-file vhdl-trace-file-2)
                             )
                         
                         ;; else - give up
                         (error "Cannot locate file %s" vhdl-trace-file)
                         )) ))
             ))))

         (beginning-of-buffer)
         (vhdl-re-search-forward 
          (concat "^\\s-*entity\\s-+"  vhdl-trace-module))
         (vhdl-re-search-forward 
          (concat "\\<" vhdl-trace-formal "\\>")
          nil t)
         )
        ;; not module. see if function
        ((equal (match-string 1) "(")
         (let ((func))
           (re-search-backward "\\<")
           (re-search-forward "\\([a-zA-Z0-9_]+\\)\\s-*(")
           (setq func (match-string-no-properties 1))
           (cond ((equal func "map")
                  ;; port or generic map - return nil
                  (search-backward "map") ;; so it won't stack at same spot
                  nil
                  )
                 ;; see if array - (xx) or (xx downto yy) (xx to yy)
                 ((looking-at "\\s-*\\(\\w+\\|\\w\\s-+\\(down\\)?to\\s-+\\w+\\)")
                   ;; move pointer so it won't stack at same spot
                  (search-backward "(") 
                  nil)
                 (t
                  (search-backward-regexp (concat keyword "\\s-+" func "\\>"))
                  (vhdl-trace-find-formal)
                  )
                 )
           )
         )
        )
  )
  )


