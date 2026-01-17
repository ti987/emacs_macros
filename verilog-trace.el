;;
;; verilog netlist tracing aid
;; tisogai 2008
;;
;; capture the cell that the cursor is at. copy it to trace file
;; trace (backwards) the signal that the cursor is at.

(defun verilog-trace-backward ()
  "capture the cell and back-trace the signal"
  (interactive)
  (verilog-trace nil))

(defun verilog-trace-forward ()
  "capture the cell and forward-trace the signal (1st occurance from the top of file)"
  (interactive)
  (verilog-trace t))


(defun verilog-trace (trace-forward)
  "capture the cell and trace the signal"
  (interactive)
  
  ;; create trace file buffer from current buffer name
  (let ((current-file (buffer-name)) 
	(trace-file)
	(module) 
	(instance)
	(signal) ; signal in question
	(bp)   ; instance beginning point
	(ep)         ; instance end point
	(cp (point)) ; current point
	(re-alpha-num "\\([a-zA-Z_0-9]+\\)") ; alpha numeric underscore seq
	(re-sig "\\([][./_()| a-zA-Z0-9\\\\]+\\)") ; parethesized signal name
	(re-sp0 "\\s-*") ; 0 or more sp ex
	(re-sp1 "\\s-+") ; 1 or more sp ex
	(re-output "\\.\\([YQO]\\|OB\\|regout\\|combout\\|cout\\|cascout\\|dataout\\|DO[AB]\\|DOP[AB]\\)")
	;; port-signal line. formal arg - actual arg
	(re-port-sig)
	;; module-instanace line
	(re-mod-inst)
	)

    (setq re-mod-inst  (concat re-sp0 re-alpha-num re-sp1 re-sig re-sp0 "("))
    (setq re-port-sig  (concat re-sp0 "\\." re-alpha-num re-sp0 "(" re-sp0 re-sig ")"))

    (setq trace-file (replace-in-string current-file "\\..*" ".vtrc"))
    (find-file trace-file) ; find or create one
    
    (switch-to-buffer (get-buffer current-file))
   
    ;; get signal
    (beginning-of-line)
    (if (looking-at re-port-sig)
	()
      (error "not a signal line"))
    (setq signal (match-string 2))

    ;; find instance
    (while (progn
	     (beginning-of-line)
	     (not (looking-at re-mod-inst))
	     )
      (forward-line -1) )
    (setq bp (point))
    (setq module (match-string 1))
    (setq instance (match-string 2))

    ;; search  ); 
    (while (progn
	     (beginning-of-line)
	     (not (looking-at ".*);\\s-*$"))
	     )
      (forward-line 1) )
    (forward-line 1) 
    (beginning-of-line)
    
    (setq ep (point))
    (copy-region-as-kill bp ep)
    ;; copy instance
    (switch-to-buffer (get-buffer trace-file))
    (end-of-buffer)
    (yank)
    (insert-string (concat 
		    "        -- "
		    (if trace-forward "forward" "backward")
		    " search " signal "\n"
		    "        add wave {" signal "}\n"
		    "        (progn (switch-to-buffer (get-buffer \"" 
		    current-file
		    "\")) (goto-char " 
		    (int-to-string cp) "))\n\n" ))  ;; jump instruction

    (switch-to-buffer (get-buffer current-file))

    ;; search
    (beginning-of-buffer)
    ;; protect special char
    (setq signal (replace-in-string signal "\\\\" "\\\\\\\\"))
    (setq signal (replace-in-string signal "[[]" "\\\\["))
    (setq signal (replace-in-string signal "[]]" "\\\\]"))

    (let ((re-out-assign (concat re-sp0 re-output "(" signal re-sp0 ")"))
	  )
      (if trace-forward
	  (let ((re-assign (concat "\\." re-alpha-num "(" signal re-sp0 ")"))
		)
	    (while (progn
		     (re-search-forward re-assign)
		     (beginning-of-line)
		     (looking-at re-out-assign))
	      (forward-line 1)))

      ;backward trace
      (if (re-search-forward re-out-assign nil t)
	  ()
	(message (concat "Not found==" signal "=="))
	(goto-char cp))
      ))
))

    

(defvar anno-face (make-face 'anno-face)
  "")
(set-face-background anno-face "gray20")
(set-face-foreground anno-face "pink")

(defvar anno2-face (make-face 'anno2-face)
  "")
(set-face-background anno2-face "yello")
(set-face-foreground anno2-face "black")


(defvar default-face (make-face 'default-face) "")
(set-face-background anno-face "gray80")
(set-face-foreground anno-face "red")

(defun test-anno2 ()
  " "
 (interactive)
  (setq g2 (make-glyph (format "%7d" (point))))
;  (setq g (make-glyph "adfsdf"))
 (setq an 
 (make-annotation (make-glyph '([xpm :file "/usr/lib/xemacs-21.4.20/etc/xemacs-beta.xpm"]
                                    [string :data "fallback-text"]))
                      (point)
                      'outside-margin
                      (current-buffer)))
;  (set-glyph-face g anno-face) 
  (set-glyph-face g2 anno-face) 
;  (set-annotation-face an anno-face)
  (set-annotation-down-glyph an g2)
;  (set-annotation-glyph an (make-glyph "hi"))
  (reveal-annotation an)       
)

(defun test-anno ()
  " "
 (interactive)
  (setq g2 (make-glyph (format "%7d" (point))))
  (setq g (make-glyph "hello"))
 (setq an 
 (make-annotation (make-glyph "hello")
                      (point)
                      'outside-margin
                      (current-buffer)))
  (set-glyph-face g anno-face) 
  (set-glyph-face g2 anno2-face) 
;  (set-annotation-face an anno-face)
  (set-annotation-down-glyph an g2)
;  (set-annotation-glyph an (make-glyph "hi"))
  (reveal-annotation an)       
)


 





