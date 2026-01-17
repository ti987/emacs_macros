;;
;; vhdl stuff
;; Toshi Isogai 2010
;;
(require 'regexp-const)

(defvar electric-pos 0 "Pointer position before 'electric' insertion is done")
(defvar this-command-pos nil "Pointer position after 'electric' insertion, but 
before moving to insert position")


(defun delete-if-at-end-of-string (str &optional exp-sp)
  "If pointer is at the end of 'str', delete 'str' and return t otherwise return nil.
If 'exp-sp' is t, move pointer back 'electric-pos', replace '\n' in 'str' with '\n\\s-*'
then delete 'str'."
  (if exp-sp
      (progn
        (let ((str2 (replace-in-string str "\n" "\n\\s-*"))
              (bpos (point))
              )
          (setq str2 (concat "\\s-*" str2))
          (goto-char electric-pos)
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

(defun electric-terminate-line () 
  "just call vhdl-electric-return"
  (vhdl-electric-return) )


(defun electric-insert (str &optional pos) 
  "Insert 'str' using electric to align.
If 'str' contains '\n', then use electric-terminate-line.
If pos is a string move pointer forward to the end of pos from the original position,
otherwise leave pointer at the end of 'str'.
"
  (interactive)
  (setq electric-pos (point)) ; global
  (let ((re-line (concat re-open ".+" re-OR "\n" re-close))
        (ix 0)
        (ix2)
        (substr)
        (bpos (point))
        )
    
      (while
          (string-match re-line str ix)
        (setq ix2 (match-end 0))
        (setq substr (substring str ix ix2))
        (setq ix ix2)
        (if (equal substr "\n")
            (electric-terminate-line)
          (insert substr)
          ;;(vhdl-electric-tab)
          )
        (vhdl-indent-line)
        )
      ;; remember the end of insert for the next replacement
      (setq this-command-pos (point))  
      ;; if pos is a string move pointer to the end of pos from original position
      (if pos
          (progn
            (goto-char bpos)
            (setq pos (replace-in-string pos "\n" "\n\\s-*"))

            (vhdl-search-forward-re pos)  
            (let ((cp (point))
                  (pdiff))

              (vhdl-indent-line)
              ;;(vhdl-electric-tab)
              (setq pdiff (- (point) cp))
              (setq this-command-pos (+ this-command-pos pdiff)) )

            ) )
    )
  )

(defun tmp () ""
  (goto-char this-command-pos) )

(defun electric-replace (command elist esize count) 
  "replace typed or replaced string if it matches an element with the next element in the list"
  (let ((eix 0)
        (replaced nil)
        )
    (setq this-command command) ; keep the current command
    (if (equal last-command command)
        (progn
          ;; move to the end of last replacement
          (if this-command-pos
              (progn
                (goto-char this-command-pos)
                (setq this-command-pos nil) ) )
          
          (while (and (not replaced) (<= eix esize))
            (if (delete-if-at-end-of-string (nth 0 (nth eix elist))
                                            (nth 2 (nth eix elist)) ) ;; regexp option
                (progn
                  (setq replaced t)
                                        ;(goto-char electric-pos)
                  (if (= eix esize)
                      (setq eix 0)   ; loop back
                    (setq eix (1+ eix)) )
                  (electric-insert (nth 0 (nth eix elist))
                                         (nth 1 (nth eix elist)) ) ;; insert point if exists
                  )  
              (setq eix (1+ eix))  
            ) ) ) 

      ; if the last command is not the same letter
      (setq this-command-pos nil)
      )
    (if (not replaced)
        (progn
          (self-insert-command count)
          (setq electric-pos (point)) ; reset
          ) )
    ))

;; overridden
(if nil
    (defun delete-if-at-end-of-string (str)
      "if pointer is at the end of 'str', delete 'str' and return t"
      "otherwise return nil"
      (if (search-backward str (- (point) (length str)) t )
          (progn 
            (delete-char (length str))
            t)))
)

(defun vhdl-electric-backquote (count) "` -->  _,  `, ~"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (if (= (char-before) ?\`)
          (progn (delete-char -1) (insert-char ?\~ 1))
        (if (= (char-before) ?\_)
            (progn (delete-char -1) (insert-char ?\` 1))
          (insert-char ?\_ 1)
          ))))

(defun vhdl-electric-under (count) "_->_, -, --"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
    (progn
      (setq this-command 'vhdl-electric-under)
      (if (eq last-command 'vhdl-electric-under)
          (cond ( (= (char-before) ?\_)
                  (progn (delete-char -1) (insert-char ?\- 1)))
                ( (= (char-before) ?\-)
                  (progn (delete-char -1) (insert-char ?\- 2)))
                (t (self-insert-command count))
                )
        (self-insert-command count))
      )
    (self-insert-command count)))

(defvar vhdl-electric-table 
  '( ("a" . ( 
             ("aa")
             ("architecture")
             ("after")
             ) )
     ("b" . ( 
             ("bb")
             ("begin")
             ("block\nbegin\n\nend block;" "begin\n" t)
             ) )

     ("c" . ( 
             ("cc")
             ("constant")
             ("case")
             ("end case;")
             ) )
     ("d" . (
             ("dd")
             ("downto 0)")
             ("downto")
             ))
     ("e" . (
             ("ee")
             ("end if;")
             ("else")
             ("elsif")
             ("end process;")
             ("end procedure;")
             ("end loop;")
             ("end case;")
             ))
     ("f" .(
            ("ffff")
            ("function")
            ("falling_edge(")
            ("falling_edge(clk) begin\n")
            ("file")
            ))

     ("p" . (
             ("pp")
             ("process (clk)\n  begin\n    if rising_edge(clk) then\n      if rst_f='0' then\n")
             ("process (clk)\n  begin\n    if rising_edge(clk) then\n      if rst='1' then\n")
             ("procedure")
             ("process")
             ))

     ("s" . (
             ("ss")
             ("signal")
             ("std_ulogic;")
             ("suv(7 downto 0);")
             ("std_ulogic_vector(")
             ("std_logic;")
             ("std_logic_vector(")
             ("subtype")
             ("severity")
             ))
    ) )

(defun vhdl-electric-common (key count) 
  "consecutive keys (3 or more) translated to vhdl keywords"
  (interactive "p")
  (let ((elist (cdr (assoc key vhdl-electric-table)))
        )
    (setq this-command (concat "vhdl-electric-" key))

    (electric-replace this-command
                      elist
                      (1- (length elist))
                      count)
    ))

(defun vhdl-electric-a (count) 
  "consecutive a keys (3 or more) translated to architecture, after, assert, access"
  (interactive "p")
  (vhdl-electric-common "a" count)
)
(defun vhdl-electric-b (count) 
  "consecutive b keys (3 or more) translated to keyword"
  (interactive "p")
  (vhdl-electric-common "b" count)
)
(defun vhdl-electric-c (count) 
  "consecutive c keys (3 or more) translated to keyword"
  (interactive "p")
  (vhdl-electric-common "c" count)
)
(defun vhdl-electric-d (count) 
  "consecutive d keys (3 or more) translated to keyword"
  (interactive "p")
  (vhdl-electric-common "d" count)
)

(defun vhdl-electric-e (count) 
  "consecutive e keys (3 or more) translated to keyword"
  (interactive "p")
  (vhdl-electric-common "e" count)
)

(defun vhdl-electric-f (count) 
  "consecutive f keys (5 or more) translated to keyword"
  (interactive "p")
  (vhdl-electric-common "f" count)
)

(defun vhdl-electric-p (count) 
  "consecutive p keys (3 or more) translated to keyword"
  (interactive "p")
  (vhdl-electric-common "p" count)
)

(defun vhdl-electric-s (count) 
  "consecutive s keys (3 or more) translated to keyword"
  (interactive "p")
  (vhdl-electric-common "s" count)
)




(defun vhdl-electric-g (count) "g->g, gg->gg, ggg->generate, generic" 
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-g)
        (if (eq last-command 'vhdl-electric-g)
            (cond ((delete-if-at-end-of-string "gg")
                   (insert "generate"))
                  ((delete-if-at-end-of-string "generate")
                   (insert "generic"))
                  ((delete-if-at-end-of-string "generic")
                   (insert ""))
                 
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-h (count) "h, hh, process..rising..then"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (let 
          ((sub))

        (if vhdl-rst-low-active
            (setq sub "process (clk)\n  begin\n    if rising_edge(clk) then\n      if rst_f='0' then\n")
          (setq sub "process (clk)\n  begin\n    if rising_edge(clk) then\n      if rst='1' then\n"))
        (setq this-command 'vhdl-electric-h)
        (if (eq last-command 'vhdl-electric-h)
            (cond ((delete-if-at-end-of-string "hh")
                   (insert sub))
                  ((delete-if-at-end-of-string sub)
                   (insert ""))
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-i (count) "i, ii, inout, integer" 
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-h)
        (if (eq last-command 'vhdl-electric-h)
            (cond 
                  ((delete-if-at-end-of-string "ii")
                   (insert "inout"))
                  ((delete-if-at-end-of-string "inout")
                   (insert "integer"))
                  ((delete-if-at-end-of-string "integer")
                   (insert ""))

                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-n (count) "n->n, nn->nn, nnn->null " 
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-n)
        (if (eq last-command 'vhdl-electric-n)
            (cond ((delete-if-at-end-of-string "nn")
                   (insert "null"))
                  ((delete-if-at-end-of-string "null")
                   (insert ""))
                 
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))


(defun vhdl-electric-o (count) "o->o, oo->oo, ooo-> others, open " 
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-o)
        (if (eq last-command 'vhdl-electric-o)
            (cond ((delete-if-at-end-of-string "oo")
                   (insert "others"))
                  ((delete-if-at-end-of-string "others")
                   (insert "others => '0'"))
                  ((delete-if-at-end-of-string "others => '0'")
                   (insert "others => '1'"))
                  ((delete-if-at-end-of-string "others => '1'")
                   (insert "open"))
                  ((delete-if-at-end-of-string "open")
                   (insert ""))
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))


(defun vhdl-electric-q (count) 
  "q->q, p, process, procedure, port, package"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-n)
        (if (eq last-command 'vhdl-electric-n)
            (cond ((delete-if-at-end-of-string "q")
                   (insert "p"))
                  ((delete-if-at-end-of-string "p")
                   (insert "process"))
                  ((delete-if-at-end-of-string "process")
                   (insert "procedure"))
                  ((delete-if-at-end-of-string "procedure")
                   (insert "port"))
                  ((delete-if-at-end-of-string "port")
                   (insert "package"))
                  ((delete-if-at-end-of-string "package")
                   (insert ""))
                 
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-r (count) 
  "r->r, rr->rr, rrr->, report, return, range, record, rising_edge(" 
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-r)
        (if (eq last-command 'vhdl-electric-r)
            (cond ((delete-if-at-end-of-string "rr")
                   (insert "rising_edge("))
                  ((delete-if-at-end-of-string "rising_edge(")
                   (insert "report"))
                  ((delete-if-at-end-of-string "report")
                   (insert "return"))
                  ((delete-if-at-end-of-string "return")
                   (insert "range"))
                  ((delete-if-at-end-of-string "range")
                   (insert "record"))
                  ((delete-if-at-end-of-string "record")
                   (insert ""))
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))



(defun vhdl-electric-t (count) "t->t, tt->tt, ttt->then, type, to_integer, transport" 
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-t)
        (if (eq last-command 'vhdl-electric-t)
            (cond ((delete-if-at-end-of-string "tt")
                   (insert "then\n"))
                  ((delete-if-at-end-of-string "then\n")
                   (insert "type"))
                  ((delete-if-at-end-of-string "type")
                   (insert "to_integer"))
                  ((delete-if-at-end-of-string "to_integer")
                   (insert "transport"))
                  ((delete-if-at-end-of-string "transport")
                   (insert ""))
                
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-u (count) "u->u, uu->unsigned, until"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-u)
        (if (eq last-command 'vhdl-electric-u)
            (cond  ((delete-if-at-end-of-string "u")
                   (insert "unsigned"))
                  ((delete-if-at-end-of-string "unsigned")
                   (insert "until"))
                  ((delete-if-at-end-of-string "until")
                   (insert ""))
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-v (count) "v->v, vv->variable"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-v)
        (if (eq last-command 'vhdl-electric-v)
            (cond  ((delete-if-at-end-of-string "vv")
                   (insert "variable"))
                  ((delete-if-at-end-of-string "variable")
                   (insert ""))
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-w (count) 
  "w, ww, wait, when, while" 
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-w)
        (if (eq last-command 'vhdl-electric-w)
            (cond 
                  ((delete-if-at-end-of-string "ww")
                   (insert "wait"))
                  ((delete-if-at-end-of-string "wait")
                   (insert "when"))
                  ((delete-if-at-end-of-string "when")
                   (insert "while"))
                  ((delete-if-at-end-of-string "while")
                   (insert ""))
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-x (count) "x->x, xx->(, (x->), xor"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-x)
        (if (eq last-command 'vhdl-electric-x)
            (cond((eq (char-before) ?x)
                   (progn (delete-char -1)(insert-char ?\( 1)))
                  ((eq (char-before) ?\()
                   (progn (delete-char -1)(insert-char ?\) 1)))
                  ((delete-if-at-end-of-string ")")
                   (insert "xor"))
                  ((delete-if-at-end-of-string "xor")
                   (insert ""))
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-y (count) "y->y, yy->', " 
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1) )
      (progn
        (setq this-command 'vhdl-electric-y)
        (if (eq last-command 'vhdl-electric-y)
            (cond ((delete-if-at-end-of-string "y")
                   (insert "="))
                  ((delete-if-at-end-of-string "=")
                   (insert "'"))
                  ((delete-if-at-end-of-string "'")
                   (insert-char ?\"))
                  ((delete-if-at-end-of-string "\"")
                   (insert ""))
                 
                  (t (self-insert-command count))
                  )
          (self-insert-command count))
        )
    (self-insert-command count)))

(defun vhdl-electric-z (count) "convert 3 or more z's to keywords"
  (interactive "p")
  (setq this-command 'vhdl-electric-z)
  (let ((elist (list 
                '("zz")
                '("ZERO")
                '("ZERO16")
                ))
        )

    (electric-replace this-command
                      elist
                      (1- (length elist))
                      count)
    ))

