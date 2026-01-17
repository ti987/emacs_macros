;;
;; converting from .xemac/init.el 
;;

(defun dot-emacs1 ()
 ".emacs in function"

 (setq xemacs nil)

(setq homedir (concat (getenv "HOME") "/"))
(setq minibuffer-max-depth nil)


(setq load-path (cons (concat homedir "emacs/") load-path))
(setq load-path (cons (concat homedir "emacs/vhdl-ext") load-path))
;(setq treesit-extra-load-path (concat homedir "emacs/tree-sitter"))

;; (setenv "PATH" (concat (getenv "PATH") ";" "/mnt/c/cygwin64/bin;"))

 ;;
 ;; key mapping
;;

;; ctrl-x 8 ret 0060 for backtick
 
 (global-set-key "\C-z" 'nop)   ;; don't minimize the window
 (global-set-key "\M-\S-u" 'upcase-last-word)

 (global-set-key [kp-delete] 'delete-char)

; (define-key key-translation-map [kp-subtract] (kbd "¯"))
 (global-set-key [kp-subtract] 'press-kp-sub)
 (global-set-key [kp-divide] 'press-kp-div)
 (global-set-key [delete] 'delete-char)
 (global-set-key [(control delete)] 'delete-spaces)
 (global-set-key [backspace] 'delete-backward-char)

 (if (equal (get-device-terminal nil) 'tty)
     (progn
       ;; tty mode
       (global-set-key "\C-h" 'delete-backward-char)
       (global-set-key [f1] 'help-command) )
 )
 ;;(global-set-key "\C-u" 'backward-kill-line)
 (define-key esc-map "%" 'query-replace-regexp)
 (global-set-key [(alt percent)] 'query-replace-regexp)
 (global-set-key [(control f5)] 'query-replace-regexp)

 (global-set-key "\C-r" 'isearch-backward-regexp)
 (global-set-key "\C-s" 'isearch-forward-regexp)

 (global-set-key [insert] nil)

 (define-key esc-map "s" 'isearch-repeat-forward)
 (define-key esc-map "g" 'goto-line)
 (define-key esc-map "P" 'eval-print-last-sexp)
 (define-key ctl-x-map "rc" 'copy-rectangle-to-register1)
 (define-key ctl-x-map "ri" '((insert-rectangle  1)))
 ;(define-key ctl-x-map "cc" 'save-buffers-kill-emacs)
 ; to avoid exit emacs from gnuclient window
 (define-key ctl-x-map "ck" 'delete-frame)
 ;(define-key ctl-x-map "ck" 'save-buffers-kill-emacs)
 (define-key ctl-x-map "\C-c" nil) ;; don't exit so easily
 (global-set-key "\C-^" 'join-line )
 (global-set-key [(control end)] 'end-of-buffer )
 (global-set-key [(control home)] 'beginning-of-buffer )
 (global-set-key [end] 'end-of-line )
 (global-set-key [home] 'beginning-of-line )
 (global-set-key [(kp-add)] 'copy-line-as-kill)

 (global-set-key [(f6)] 'copy-line-to-other-buffer)
 (global-set-key [(control kp-enter)] 'reload-emacs-config)
 (define-key help-map "\C-f" 'function-apropos )
 (define-key minibuffer-local-map [up] 'previous-history-element )
 (define-key minibuffer-local-map [down] 'next-history-element )
 (global-set-key [(control f1)] 'scroll-up-1 )
 (global-set-key [(control f2)] 'scroll-down-1 )
 (global-set-key [(control f6)] 'face-at-point-command )
 (global-set-key [(f7)] 'yank-clipboard-selection )
 (global-set-key [(shift f7)] 'toggle-truncate-lines )
 (global-set-key [(control f7)] 'highlight-line )
 (global-set-key [(f8)] 'other-window )
 (global-set-key [(control tab)] 'other-window )
 (global-set-key [(control ?=)] 'other-window )
 (global-set-key [(f9)] 'execute-kbd-macro-i )
 (global-set-key [(f11)] 'redo )
 (global-set-key [(f12)] 'undo )
 (global-set-key [(f19)] 'press-kp-sub) ; insert macron 
 (global-set-key [(kp-div)] 'press-kp-div) ; insert macron 
 (global-set-key "\M-." 'find-tag-2 )
 (global-set-key [(alt space)] 'newline )
 (global-set-key [(control iso-left-tab)] 'other-frame )
 (global-set-key [(alt u)] 'upcase-last-word )

 (global-set-key [(control ?x) ?r ?a] 'append-to-register)
 (global-set-key [(control ?x) ?r ?p] 'prepend-to-register)
 (global-set-key [(super ?_) ?b] 'toggle-backup-inhibited)

 (global-set-key [(shift f12)] 'press-kp-sub )
 (global-set-key [(kp-sub)] 'press-kp-sub )
 (global-set-key [(shift return)] 'forward-line-eol )

 (global-set-key [(shift f17)] 'insert-backtick )

 ;; ctrl-shift d to shorten search string when C-w is used
 (define-key isearch-mode-map [(control shift ?d)] 'isearch-del-char)
  (define-key isearch-mode-map [backspace] 'isearch-del-char)

 ;; for mac, with karabiner remapping nfer to option-r.
 ;; this remap option-r to meta key
 (setq mac-right-option-modifier 'meta)
 (setq mac-left-option-modifier 'alt)

 ;; (global-set-key [(control \') a] 'a-umlaut)

 (tool-bar-mode -1)   ; remove tool-bar

 ;(set tool-bar-lines-needed 1)
 ;(setq default-toolbar-position 'left)
 ;;xemacs(set-specifier default-toolbar-visible-p nil)

 (require 'redo)   
 (setq-default tab-width 8 )
 (line-number-mode 1)
 ;; high light matching parenthesis
 (setq paren 'paren)


 ;; auto save
 (setq auto-save-interval 0)

 ;;
 ;; major modes
 ;;
 ;(setq default-major-mode 'text-mode)

 ;;(autoload 'octave-mode "octave")
 (setq auto-mode-alist (append 
    (list (cons "\\.m$" 'octave-mode) ) 
    auto-mode-alist
    )   )

 (autoload 'arduino-mode "arduino-mode")
 (setq auto-mode-alist (append 
    (list (cons "\\.ino$" 'arduino-mode) ) 
    auto-mode-alist
    )   )

 (autoload 'dsfcp-mode "dsfcp-mode")
 (setq auto-mode-alist
    (append '(("\\.fcp.out$"  . dsfcp-mode)
              ("\\.cmd.in$" . dsfcp-mode)
              ("\\.prs.out$" . dsfcp-mode)
             ) auto-mode-alist))

 (autoload 'hm--html-mode "hm--html-mode")
 (setq auto-mode-alist 
    (append '(("\\.html$"  . hm--html-mode)
              ("\\.htm$" . hm--html-mode)
             ) auto-mode-alist))
              
 (autoload 'ksh-mode "ksh-mode")
 (setq auto-mode-alist (append 
    (list (cons "\\.zshrc$" 'ksh-mode) ) 
    auto-mode-alist
    ) )

 (setq auto-mode-alist (append 
    (list (cons "\\.sh$" 'ksh-mode) ) 
    auto-mode-alist
    )   )

 (autoload 'cperl-mode "cperl-mode")
 (setq auto-mode-alist (append
    (list (cons "\\.pl$" 'cperl-mode) ) 
    auto-mode-alist
    ) )

 (setq auto-mode-alist (append 
    (list (cons "\\.pm$" 'cperl-mode) ) 
    auto-mode-alist
    ) )

 (setq auto-mode-alist
       (append  '(("\\.tdg$" . cperl-mode)
                  ("\\.tg2$" . cperl-mode)
                  ) auto-mode-alist
                    ) )

 (setq auto-mode-alist (append 
    (list (cons "\\.scad$" 'scad-mode) ) 
    auto-mode-alist
    ) )

(load-file "~/emacs/vhdl-gpt-v1.el")
  (autoload 'vhdl-mode "vhdl-mode")
;(require 'vhdl-gpt)
 (setq auto-mode-alist (cons '("\\.vhd$" .  vhdl-mode) auto-mode-alist ))

 (autoload 'verilog-mode "verilog-mode")
 (setq auto-mode-alist 
       (append '(("\\.sv$"  . verilog-mode)
                 ("\\.svh$" . verilog-mode)
                 ("\\.v$" . verilog-mode)
                 ) auto-mode-alist))

 (autoload 'xrdb-mode "xrdb-mode")
 (setq auto-mode-alist (cons '("\\.Xdefaults$" .  xrdb-mode) auto-mode-alist ))

 (autoload 'winmgr-mode "winmgr-mode")
 (setq auto-mode-alist (cons '("\\.fvwm2rc$" .  winmgr-mode) auto-mode-alist ))

 (autoload 'ruby-mode "ruby-mode")
 (setq auto-mode-alist (cons '("\\.rb$" .  ruby-mode) auto-mode-alist ))

 (autoload 'emu-mode "emu-mode")
 (setq auto-mode-alist 
       (append '(
                 ("\\.emu$" .  emu-mode)
                 ("/cmd$" .  emu-mode)
                 )auto-mode-alist ))


 (autoload 'ada-mode "ada-mode")
 (setq auto-mode-alist (cons '("\\.ada$" .  ada-mode) auto-mode-alist ))

 (autoload 'lisp-mode "lisp-mode")
 ;(load "text-mode.el")
 (autoload 'emacs-lisp-mode "lisp-mode")
 (setq auto-mode-alist 
    (append '(("\\.emacs$"  . emacs-lisp-mode)
              ("\.vm$" .  emacs-lisp-mode) ) auto-mode-alist ))
 ;;(load "hyper-apropos.el")

 ;;(display-column-mode)
 (setq auto-mode-alist (append 
                        (list (cons "\\.scr$" 'c-mode) ) 
                        auto-mode-alist
                        )
       )

 (setq auto-mode-alist
    (append '(("\\.C$"  . c++-mode)
              ("\\.cc$" . c++-mode)
                          ("\\.cpp$" . c++-mode)
              ("\\.c$"  . c-mode)
              ("\\.h$"  . c-mode)
;;              ("\\.m$"  . objc-mode)
             ) auto-mode-alist))



 (put 'eval-expression 'disabled nil)


)


(defun dot-emacs2 ()
 ".emacs in function"

 ;;
 ;; moving
 ;;
 (defun line-to-top ()
    (interactive)
    (set-window-start (selected-window) (point-at-bol)))

 (defun scroll-down-1 ()
   (interactive)
   (scroll-down 1)
 )

 (defun scroll-up-1 ()
   (interactive)
   (scroll-up 1)
 )

 (defun upcase-last-word ()
   (interactive)
   (backward-word)
   (upcase-word 1)
   )

 (defun forward-line-eol ()
   (interactive)
   (forward-line)
   (end-of-line) )
 ;; 
 ;; mouse
 ;;
 (setq-default mouse-yank-at-point t)

 ;; 
 ;; region
 ;;
 (setq transient-mark-mode t)

 ;;
 ;; hooks
 ;;
 (defun font-lock-pre-idle-hook () ()
 )
 (add-hook 'font-lock-mode-hook 'font-lock-mode-hook-function)
 (defun font-lock-mode-hook-function ()
   (setq font-lock-maximum-size 100000))


 (remove-hook 'dsfcp-mode-hook 'dsfcp-mode-hook-function)
 (add-hook 'dsfcp-mode-hook 'dsfcp-mode-hook-function)
 (defun dsfcp-mode-hook-function () 
   (font-lock-mode) )

 (add-hook 'winmgr-mode-hook 
           #'(lambda ()
                   (font-lock-mode)))

 (remove-hook 'ruby-mode-hook 'ruby-mode-hook-function)
 (add-hook 'ruby-mode-hook 'ruby-mode-hook-function)
 (defun ruby-mode-hook-function () 
   (modify-syntax-entry ?_ "w" (syntax-table))
   (font-lock-mode) )

 (remove-hook 'emu-mode-hook 'emu-mode-hook-function)
 (add-hook 'emu-mode-hook 'emu-mode-hook-function)
 (defun emu-mode-hook-function () 
   (modify-syntax-entry ?_ "w" (syntax-table))
   (font-lock-mode) )


 ;(add-hook 'font-lock-mode-hook  'turn-on-fast-lock)
 (add-hook 'c-mode-common-hook   'font-lock-mode)
 (add-hook 'emacs-lisp-mode-hook 'font-lock-mode)
 (remove-hook 'shell-mode-hook 'shell-mode-hook-function)
 (add-hook 'shell-mode-hook 'shell-mode-hook-function)
 (defun shell-mode-hook-function ()
           (font-lock-mode) 
           (make-face-unitalic 'shell-output-face)
           (make-face-larger 'shell-output-face)
         ;  (define-key shell-mode-map "\M-." 'shell-insert-meta-period) 
           )
 ;(defun shell-insert-meta-period () 
 ;  "insert escape ."
 ;  (interactive)
 ;  (self-insert-internal ?\e)
 ;  (self-insert-internal ?.) )

 (remove-hook 'vhdl-mode-hook  'vhdl-mode-hook-function)
 (add-hook 'vhdl-mode-hook 'vhdl-mode-hook-function)

 (defun vhdl-mode-hook-function ()
   (setq  vhdl-basic-offset 3 )
   (setq  tab-width 3 )

   (setq vhdl-compiler "modelsim6")
   (define-key vhdl-mode-map [(control up)] 'vhdl-backward-sexp)
   (define-key vhdl-mode-map [(control down)] 'vhdl-forward-sexp)
 ;;     (define-key vhdl-mode-map [(f8)] 'test-func3)
 ;;     (define-key vhdl-mode-map [(f9)] 'test-func)
 ;;     (define-key vhdl-mode-map [(f10)]   'test-func2)
 ;;            (define-key vhdl-mode-map [(f11)] 'test-func)
   (setq vhdl-compiler "modelsim6")
   (setq vhdl-company-name "Ball Aerospace")
   (setq vhdl-reset-active-high nil)
   (setq vhdl-reset-kind 'sync)
   (setq vhdl-underscore-is-part-of-word t)
 ;;        (vhdl-activate-customizations)
   (define-key vhdl-mode-map [delete] 'delete-char)
   (define-key vhdl-mode-map "\r"           'newline)
   (define-key vhdl-mode-map [enter] 'newline)
   (define-key vhdl-mode-map [return] 'newline)
   (define-key vhdl-mode-map [tab] 'vhdl-indent-line)
 ;;            (define-key vhdl-mode-map [space] 
 ;;              'canna-henkan-region-or-self-insert)
   (define-key vhdl-mode-map [space] 'self-insert-command)
   (define-key vhdl-mode-map [(shift tab)] 'expand-abbrev)
   (define-key vhdl-mode-map [(button2)] 'mouse-yank )
   ;;(define-key vhdl-mode-map [(button2)] 'nop )

   (modify-syntax-entry ?_ "w" (syntax-table))

   (setq vhdl-file-header (concat homedir "emacs/vhdlheader"))
   (turn-on-font-lock)
   (defun font-lock-pre-idle-hook () ())
   
 ;;            (modify-syntax-entry ?\n "\-" (syntax-table))
              ;;(remove-hook 'pre-idle-hook 'font-lock-pre-idle-hook )

 ;; binding keys
   (define-key vhdl-mode-map [(control f6)] 'vhdl-align-flattened-signal-declarations)
   (define-key vhdl-mode-map [(shift control f6)] 'vhdl-insert-bit-sizes)
   (define-key vhdl-mode-map [(control f7)] 'vhdl-trace-port)
   (define-key vhdl-mode-map [(meta f7)] 'vhdl-trace-port-up)
   (define-key vhdl-mode-map [(control f8)] 'vhdl-colon-align-alt)
   (define-key vhdl-mode-map [(meta control f8)] 'vhdl-colon-align-alt)
   (define-key vhdl-mode-map [(shift control f8)] 'vhdl-sort-port-signal-assignment-block)
   (define-key vhdl-mode-map [(control down)] 'vhdl-forward-block)
   (define-key vhdl-mode-map [(control up)] 'vhdl-backward-block)
   (define-key vhdl-mode-map [(control f9)] 'vhdl-param-align-alt)
   (define-key vhdl-mode-map [(meta control f9)] 'vhdl-param-align-alt)
   (define-key vhdl-mode-map [(control f10)] 'vhdl-align-assignment)
   (define-key vhdl-mode-map [(shift control f10)] 'vhdl-align-assignment-alt)
   (define-key vhdl-mode-map [(f10)] 'vhdl-identify-block)
   (define-key vhdl-mode-map "\C-c\C-d"     'vhdl-compile)
   (define-key vhdl-mode-map "\C-c\M-\C-d"  'vhdl-make)
   
   (define-key vhdl-mode-map "\C-r" 'isearch-backward-regexp)
   (define-key vhdl-mode-map "\C-s" 'isearch-forward-regexp)
   (define-key isearch-mode-map  [(f10)] 'vhdl-identify-block)
 ;;             (define-key vhdl-mode-map [(next)] 'vhdl-scroll-up-command)
 ;;             (define-key vhdl-mode-map [(prior)] 'vhdl-scroll-down-command) 
   (define-key vhdl-mode-map [(meta f12)] 'test2)

;;   (define-key vhdl-mode-map "\C-cj"     'vhdl-gpt-jump-to-declaration)
   (define-key vhdl-mode-map "\C-cj"     'vhdl-gpt-signal-decl-jump)
   
   (lightning-keys-setup 'vhdl-mode 'vhdl-mode-map vhdl-lightning-key-list )
   (setq vhdl-stutter-mode t) ; [ -> ( etc.
   (setq font-lock-auto-fontify nil)
   
   (vhdl-update-helper-menu)
   
   (defun lightning-tab ()
     (vhdl-indent-line)
     )
   (defun lightning-terminate-line ()
     (funcall (key-binding (kbd "RET")))
     )

   (setq vhdl-signal-sys-clk "sys_clk")
   (setq vhdl-signal-sys-rst "sys_rst_n")
   (setq vhdl-signal-sys-rst-active "0")

   (setq vhdl-end-comment-column 199)
   )



 
 (define-key emacs-lisp-mode-map [(f9)] 'test-func)
 (define-key emacs-lisp-mode-map [(f10)]   'test-func2)
 (define-key emacs-lisp-mode-map [(hyper backspace)]   'test-func3)
 (define-key emacs-lisp-mode-map [(alt backspace)]   'test-func3)
 (defun font-lock-pre-idle-hook () 
   (remove-hook 'pre-idle-hook 'font-lock-pre-idle-hook )
 )


)

(defun vhdl-signal-custom ()
  (interactive)
   (setq vhdl-signal-sys-clk "s_axis_aclk")
   (setq vhdl-signal-sys-rst "s_axis_aresetn")
   (setq vhdl-signal-sys-rst-active "0")
)

(defun vhdl-signal-axis ()
  (interactive)
   (setq vhdl-signal-sys-clk "axis_clk")
   (setq vhdl-signal-sys-rst "axis_rst_n")
   (setq vhdl-signal-sys-rst-active "0")
)

(defun vhdl-signal-axi ()
  (interactive)
   (setq vhdl-signal-sys-clk "axi_clk")
   (setq vhdl-signal-sys-rst "axi_rst_n")
   (setq vhdl-signal-sys-rst-active "0")
)

(defun vhdl-signal-clk ()
  (interactive)
   (setq vhdl-signal-sys-clk "clk")
   (setq vhdl-signal-sys-rst "rst_n")
   (setq vhdl-signal-sys-rst-active "0")
)

(defun dot-emacs3 ()
 ".emacs in function"

 ;;; ccompile mode

 (remove-hook 'mode-compile-after-compile-hook  'compile-mode-hook-function)
 (add-hook 'mode-compile-after-compile-hook  'compile-mode-hook-function)

 (defun compile-mode-hook-function ()
      (define-key compilation-mode-map [(button2)] 'vhdl-goto-error )
 )

 ;  variable  a, b : time;
 (defun skip-syntax-backward-2  (syn)
   "skip-syntax-backward replacement"
   (interactive)
   (let ((ch1 (char-after))
         (cnt 0  ))
     (while (and 
             (> (point) 1)
             (or 
              (progn
                (backward-char)
                (setq ch1 (char-after))
                (string-match 
                 (concat "\\" (char-to-string (char-syntax ch1)))
                 syn)
                )   
              (forward-char)
              )
             )
              
       (incf cnt)
     cnt
   )))

 (defun test-func3 ()
   ""
  (interactive)
  (vhdl-font-lock-match-item 28))

 (defun test-func ()
   "test-function"
   (interactive)
   (skip-syntax-backward "w_"))

 (defun test-func2 ()
   "test-function"
   (interactive)
   (skip-syntax-backward " "))


 (remove-hook 'verilog-mode-hook 'verilog-mode-hook-function)
 (add-hook 'verilog-mode-hook 'verilog-mode-hook-function)
 (defun verilog-mode-hook-function ()  
   (turn-on-font-lock) 
   (verilog-update-helper-menu)
   (define-key verilog-mode-map [delete] 'delete-char)        
   
   (define-key verilog-mode-map [(control f9)]  'verilog-align-instance)
   (define-key verilog-mode-map [(control f8)]  'verilog-align-variable-declarations)
;   (define-key verilog-mode-map [(control f9)]  'verilog-align-port-declarations)
   (define-key verilog-mode-map [(control f11)] 'verilog-declare-states)
   (define-key verilog-mode-map [(control f7)]  'verilog-trace-port)
   (define-key verilog-mode-map [(meta f7)]     'verilog-trace-port-up)
   (define-key verilog-mode-map [(control f12)] 'verilog-add-reset-assignment)
   (define-key verilog-mode-map [(alt a)]       'verilog-align-seq-or-conc-assignments)
   (define-key verilog-mode-map [(alt v)]       'verilog-align-variable-declarations)
   (define-key verilog-mode-map [(alt p)]       'verilog-align-port-declarations)
   
   (define-key verilog-mode-map [(meta f12)] 'test1)
   (define-key verilog-mode-map "\C-c\C-p" nil)
   (define-key verilog-mode-map "\C-c\C-p\C-w" 'verilog-get-module)
   (define-key verilog-mode-map "\C-c\C-p\C-i" 'verilog-instantiate-module)
   (define-key verilog-mode-map "\C-c\C-pi" 'verilog-instantiate-module)
   
   (lightning-keys-setup 'verilog-mode 'verilog-mode-map
                         verilog-lightning-key-list )
 ;  (verilog-setup-electric-keys)
   (setq verilog-auto-newline nil)
   )


 (add-hook 'tcl-mode-hook 
           #'(lambda()
              (turn-on-font-lock) 
              (define-key tcl-mode-map [delete] 'delete-char)    
              ))

 (add-hook 'java-mode-hook 'java-mode-hook-function)
 (defun java-mode-hook-function ()
   (define-key java-mode-map [delete] 'delete-char)
   (setq js-indent-level 3)
   )

 (defun perl-mode-hook-function ()
   (turn-on-font-lock) 
   (define-key perl-mode-map [delete] 'delete-char)
   )
 (defun cperl-mode-hook-function ()
   (turn-on-font-lock) 
   (define-key cperl-mode-map [delete] 'delete-char)
   )


 (remove-hook 'perl-mode-hook 'perl-mode-hook-function)
 (remove-hook 'cperl-mode-hook 'cperl-mode-hook-function)
 (add-hook 'perl-mode-hook 'perl-mode-hook-function)
 (add-hook 'cperl-mode-hook 'cperl-mode-hook-function)

 (add-hook 'c-mode-hook #'(lambda ()
                           (turn-on-font-lock)
                           (define-key c-mode-map [delete] 'delete-char)
                           (setq c-basic-offset 4)
                           )
           )

 (add-hook 'w3-file-done-hook '(lambda ()
                           (setq truncate-lines nil )) )


 (add-hook 'w3-file-prepare-hook '(lambda ()
                           (setq truncate-lines nil )   
                           (w3-do-incremental-display t)
                           (w3-delay-image-loads (quote t))
                           (url-be-asynchronous t)         ) )


 (setq                           truncate-partial-width-windows nil)   

 (remove-hook 'find-file-hooks 'file-check-hook)
 (setq find-file-use-truenames t)
 (add-hook 'find-file-hooks 'file-check-hook)

 (add-hook 'ksh-mode-hook '(lambda ()
                             (define-key ksh-mode-map [delete] 'delete-char)
                             (turn-on-font-lock)
                             )     )

 (add-hook 'diary-display-hooks 'appt-make-list)
 ;(display-time)


 (defun jump-to-column ()
   "move point to column"
   (interactive)
   (let ((input (string-to-int (read-from-minibuffer "column:" () () nil))))
     (move-to-column input))
 )

 ; decide mode by looking at file contents
 (defun file-check-hook ()
    "This runs when file is opened.
 Assign major modes when appropriate. ksh-mode on zsh files. xrdb-mode.
 Create local-variable backup."
     (save-excursion
       (make-local-variable 'backup-inhibited)
       (setq backup-inhibited nil)

 ;    replaced with setting 'find-file-use-truenames
 ;      (set-visited-file-name
 ;       (compute-buffer-file-truename))

       (save-restriction
        (widen)
        (goto-char (point-min))
        ;(setnu)
        ;(line-numbers-mode)
        ; use true file name

 ;      (let ((buffer (compute-buffer-file-truename))
 ;           )
 ;       (setq buffer (replace-in-string buffer "/mnt/auto" ""))
 ;       (rename-buffer buffer))
                             (update-mode-line-format)

        (cond ((looking-at "\\(#![ \t]*/.*/zsh\\|#!/bin/sh\\(.*\n\\)*exec zsh\\)")
               (ksh-mode))
              ((looking-at "!\\(\\s-*\$XConsortium:\\)?\\s-*\\w+.ad" )
               (xrdb-mode))
              )   )))



 (defun mapis () 
   (interactive)
   (defvar mapis nil)
   (setq mapis (current-keymaps)))
 ;;
 ;; local key bind
 ;;

 ;(define-key help-map "a" 'hyper-apropos)
 (define-key lisp-mode-map [delete] 'delete-char)
 (define-key emacs-lisp-mode-map [delete] 'delete-char)
 (define-key emacs-lisp-mode-map "\C-\M-x" 'edebug-defun)
 (define-key help-map "\C-s" 'super-apropos)
 (setq apropos-do-all t)

 ;; redo describe-function  
 ;; it handles reg exp
 (fset 'function-apropos 'apropos-function)

 (defun execute-kbd-macro-i ()
   "Execute keyboard macro"
   (interactive)
   (execute-kbd-macro last-kbd-macro) )

;; (defun apropos-function (apropos-regexp &optional do-all)
;;   "Shows commands (interactively callable functions) that match REGEXP.
;; "
;;   (interactive (list (read-string (concat "Apropos function "
;;                                           (if current-prefix-arg
;;                                               "or variable ")
;;                                           "(regexp): "))
;;                      current-prefix-arg))
;;   (if (not (boundp 'apropos-do-all))
;;           (autoload 'apropos-print "apropos")
;;           )
;;   (let ((message
;;          (let ((standard-output (get-buffer-create "*Help*")))
;;            (print-help-return-message 'identity))))
;;     (or do-all (setq do-all nil))
;;     (setq apropos-accumulator
;;           (apropos-internal apropos-regexp
;;                             (if do-all
;;                                 (lambda (symbol) (or (commandp symbol)
;;                                                      (user-variable-p symbol)
;;                                                      (fboundp symbol)
;;                                                      ))
;;                               'fboundp)))
;;     (if (apropos-print
;;          t
;;          (lambda (p)
;;            (let (doc symbol)
;;              (while p
;;                (setcar p (list
;;                           (setq symbol (car p))
;;                           (if (fboundp symbol)
;;                               (if (setq doc (documentation symbol t))
;;                                   (substring doc 0 (string-match "\n" doc))
;;                                 "(not documented)"))
;;                           (and do-all
;;                                (user-variable-p symbol)
;;                                (if (setq doc (documentation-property
;;                                               symbol 'variable-documentation t))
;;                                    (substring doc 0
;;                                               (string-match "\n" doc))))))
;;                (setq p (cdr p)))))
;;          nil)
;;         (and message (message message)))))
;;
 (defun copy-line-as-kill ()
   "TI:Save the line and goto the next line"
   (interactive)
         (progn
           (beginning-of-line)
           (let ((pos (point)))
                 (next-line 1)
                 (beginning-of-line)
                 (kill-ring-save pos (point))
                 )
           )

   )

 (defun copy-rectangle-to-register1 () 
   "TI:copy rectangle to register 1"
   (save-excursion 
         (exchange-point-and-mark)
         )
   )

 (defun copy-line-to-other-buffer ()
   "TI:copy current line to the other buffer. Then, Move to next line.
 Must have 2 windows already open"
   (interactive)
   (save-excursion 
     (let ((ln (int-to-string (line-number))))
       (copy-line-as-kill)
       (other-window 1)
       (beginning-of-line)
       (insert-string (concat  ln ": "))
       (yank)
       (other-window -1)
       )
   )
   (next-line 1)
 )

 (defun reload-emacs-config () 
   "TI:Reload .emacs file"
   (interactive)
   (load-file (concat homedir ".emacs"))
 )


 ;;
 ;; news server
 ;;;
 ;(setq gnus-select-method '(nntp "news.stortek.com"))
 ;(setq gnus-select-method '(nntp "news.colorado.edu"))

 ;;
 ;; printer
 ;;
 (setq lpr-command "lpr")
 (setq lpr-switches '("-Pengcopier") )
 ; 60 lines per page. Offset 4 characters.
 (setq lpr-page-header-switches '("-f" "-l" "60" "-o" "4")) 

 (setq ps-paper-type 'letter)
 (setq ps-spool-duplex t)
 (setq ps-line-number t)
 (setq ps-line-number-start 5)
 (setq ps-line-number-step 5)
 ;(setq ps-line-number-font "Courier")
 ;(setq ps-line-number-font-size 7)
 (setq ps-font-size 9)
 (setq ps-line-height (if (fboundp 'float) 10.29 10))
 (setq ps-bold-faces '(font-lock-function-name-face
                         font-lock-keyword-face  )  )
 (setq ps-print-color-p nil)
 (setq ps-multibyte-buffer 'bdf-font-except-latin)

 ;;(setq ps-lpr-switches '("-Jjct,duplex_long -dmattpr"))


 (defun ti-print-buffer ()
   "TI:print buffer using postscript"
   (interactive)
   (color2)
   (ps-print-buffer)
   (color1)
 )
   

 ;;
 ;; vc
 ;;
 (defcustom vc-header-alist
   '((SCCS "\%W\%") (RCS "\$Id\$") (CVS "\$Id\$"))
   "*Header keywords to be inserted by `vc-insert-headers'.
 Must be a list of two-element lists, the first element of each must
 be `RCS', `CVS', or `SCCS'.  The second element is the string to
 be inserted for this particular backend."
   :type '(repeat (list :format "%v"
                        (choice :tag "System"
                                (const SCCS)
                                (const RCS)
                                (const CVS))
                        (string :tag "Header")))
   :group 'vc)



 ;(load-file "/usr/lib/xemacs/xemacs-packages/lisp/vc/vc.elc")
 ;;(load "/usr/share/emacs/site-lisp/subversion/vc-svn.elc")
 ;(setq vc-path '("RCS"))

 ; cvs
 ;(autoload 'cvs-update "pcl-cvs" nil t)


 ;;
 ;; mail
 ;;
 ;; more in .vm file
 (setq toolbar-mail-reader 'vm)

 ;;
 ;; calc
 ;;
 (autoload 'calc-dispatch "calc" "Emacs Calculator" t nil)
 (global-set-key "\e#" 'calc-dispatch)

 ;;
 ;; diary
 ;;
 ;(require 'appt)
 ;(autoload 'appt-initialize "appt")
 ;(setq view-diary-entries-initially t)


 ;;
 ;; tag
 ;;
 (defun find-tag-2 (tagname &optional next)
   "TI:find tag in other window and move cursor back"
   (interactive (if current-prefix-arg
                    '(nil t)
                  (require 'etags)
                  (list (find-tag-tag "Find tag other window: "))))
    
   (if next
       (find-tag nil t)
     (find-tag tagname t)   ) 
   ;(mark-word 1)
   (backward-other-window 1)   )


 ;; desktop
 ;;
 ;(load "desktop")

 ;;
 ;; alternative forward-word
 ;;
 (defun my-forward-word (count)
   (interactive "p")
   (forward-word count)
   (if (> count 0)
       (skip-syntax-forward "^w")
       (skip-syntax-backward "^w")))

 (defun my-backward-word (count)
   (interactive "p")
   (my-forward-word (- count)))

 ;(global-set-key [(meta f)] 'my-forward-word)
 ;(global-set-key [(meta b)] 'my-backward-word)



 ;; backup
 (load-file (concat homedir "emacs/backup.el"))
 ;;(autoload 'save-file-with-backup "backup.el") 
 (define-key ctl-x-map "\C-s" 'save-file-with-backup)

 ;;
 ;; shell stuff
 ;;
 (defvar my-shell 0)
 (defun  my-shell ()
    (interactive)
    (setq my-shell (1+ my-shell))
    (if (eq window-system 'x)
           (select-frame(new-frame)))
    (shell)
    (rename-buffer (format "*shell%d*" my-shell)))

 ;;
 ;; Info stuff
 ;;
 ;(setq Info-default-directory-list
 ;      (reverse
 ;       (cons "/usr/info/" 
 ;            (reverse Info-default-directory-list))))

 ;;
 ;; auto uncompress
 ;;
 ;(require 'crypt)
 (set-variable 'crypt-auto-write-buffer t)

 ;; gnu server
;;(gnuserv-start)
 ;; emacsclient server
; (setq server-socket-dir "~/.emacs.d/server")
;; (server-force-stop)
;(server-start)

;(load-file (concat homedir "emacs/frame-fns.el"))
;(load-file (concat homedir "emacs/frame-cmds.el"))
;; start emacsclient file in a new frame
  (add-hook 'server-switch-hook
              (lambda nil
                (rename-frame (selected-frame) (concat "emacsclient - " (buffer-name)))
                ))
  
 ;; font lock
 (make-face 'font-lock-default-face)
 (set-face-foreground 'font-lock-default-face "#000" )
 (make-face 'font-lock-assignment-operator-face)
 (set-face-foreground 'font-lock-assignment-operator-face "#f82" )
 (make-face 'font-lock-relational-operator-face)
 (set-face-foreground 'font-lock-relational-operator-face "yellow3" )
 (make-face 'font-lock-when-operator-face)
 (set-face-foreground 'font-lock-when-operator-face "slateblue" )
 (make-face 'font-lock-keyword-face)
 (set-face-foreground 'font-lock-keyword-face "#9d4" )
 ;;
 ;; custom variable (option saved)
 ;;



)

(defun dot-emacs31 ()



 ;
 ;; background
 ;

 ;;
 ;; modeline format
 ;;

 (defface user-header-libera-ste-face
   '((((class color) (min-colors 88) (background light))
      :background "seagreen4" :foreground "black")
     (((class color) (min-colors 88) (background dark))
      :background "seagreen3" :foreground "black")
     (((class color) (min-colors 16) (background light))
      :background "seagreen2" :foreground "black")
     (((class color) (min-colors 16) (background dark))
      :background "seagreen1" :foreground "black")
     (((class color) (min-colors 8))
      :background "seagreen4" :foreground "black")
     (t :inverse-video t))
   "libera-ste highlight face."
   :group 'user-faces)


 (defface user-header-lwc-face
   '((((class color) (min-colors 88) (background light))
      :background "orange3" :foreground "black")
     (((class color) (min-colors 88) (background dark))
      :background "orange3" :foreground "black")
     (((class color) (min-colors 16) (background light))
      :background "orange3" :foreground "black")
     (((class color) (min-colors 16) (background dark))
      :background "orange3" :foreground "black")
     (((class color) (min-colors 8))
      :background "orange3" :foreground "black")
     (t :inverse-video t))
   "lwc highlight face."
   :group 'custom-faces)

 ;; name of file at the top of buffer window
 (setq header-line-format 
       (list
        '(:eval (propertize "%f " 
                            'face 
                            (if (string-match "/libera_ste/" (buffer-file-name))
                                'user-header-libera-ste-face
                              (if (string-match "/lwc/" (buffer-file-name))      
                                  'user-header-lwc-face
                                'font-lock-type-face))
                            'help-echo (buffer-file-name)   ) )
        ))
       
 (setq default-header-line-format header-line-format )

;    (global-linum-mode t)
  (global-display-line-numbers-mode 1) ;; Enable line numbers globally


)

(defun dot-emacs32 ()

 (defun update-mode-line-format ()
   (interactive)
    (setq mode-line-format default-mode-line-format)
   (setq header-line-format default-header-line-format)
   )

 ;(setq mode-line-format 
 ;      '("-"
 ;       mode-line-mule-info
 ;       mode-line-modified
 ;       mode-line-frame-identification
 ;       mode-line-buffer-identification
 ;       
 ;       "   "
 ;       mode-line-position
 ;       (vc-mode vc-mode)
 ;       "   "
 ;       
 ;       mode-line-modes
 ;       (which-func-mode ("" which-func-format "--"))
 ;       (global-mode-string ("--" global-mode-string))
 ;       "-%-") )
 ;(setq frame-title-format
 ;      '(buffer-file-name "%b - %f" ; File buffer
 ;        (dired-directory dired-directory ; Dired buffer
 ;         (revert-buffer-function "%b" ; Buffer Menu
 ;          ("%b - Dir: " default-directory))))) ; Plain buffer
 ; use setq-default to set it for /all/ modes
 (setq default-mode-line-format
   (list
     ;; the buffer name; the file name as a tool tip
     '(:eval (propertize "%b " 
                         'face 
                            (if (string-match "/libera_ste" (buffer-file-name))
                                'user-header-libera-ste-face
                              (if (string-match "/lwc" (buffer-file-name))      
                                  'user-header-lwc-face
                                'font-lock-default-face))

                         'help-echo (buffer-file-name)))

     ;; line and column
     "[" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-default-face) ","
       (propertize "%02c" 'face 'font-lock-default-face) 
     "] "

     ;; relative position, size of file
     "["
     (propertize "%p" 'face 'font-lock-default-face) ;; % above top
     "/"
     (propertize "%I" 'face 'font-lock-default-face) ;; size
     "] "

     ;; the current major mode for the buffer.
     "["

     '(:eval (propertize "%m" 'face 'font-lock-default-face
               'help-echo buffer-file-coding-system))
     "] "

     "[" ;; insert vs overwrite mode, input-method in a tooltip
     '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
               'face 'font-lock-default-face
               'help-echo (concat "Buffer is in "
                            (if overwrite-mode "overwrite" "insert") " mode")))

     ;; was this buffer modified since the last save?
     '(:eval (when (buffer-modified-p)
               (concat ","  (propertize "Mod"
                              'face 'user-header-lwc-face ;'font-lock-comment-face
                              'help-echo "Buffer has been modified"))))

     ;; is this buffer read-only?
     '(:eval (when buffer-read-only
               (concat ","  (propertize "RO"
                              'face 'font-lock-default-face
                              'help-echo "Buffer is read-only"))))  
     "] "

     ; add the time, with the date and the emacs uptime in the tooltip
     '(:eval (propertize (format-time-string "%H:%M")
               'help-echo
               (concat (format-time-string "%c; ")
                       (emacs-uptime "Uptime:%hh"))))
     
     ; suversion info
     " [Svn:"
     ;'(:eval (propertize 
     ;         (concat 
     ;          "r"
     ;          (vc-svn-workfile-version (buffer-file-name) )
     ;          " "
     ;          (symbol-name (vc-svn-state (buffer-file-name)) )
     ;          )
     ;         'face 'font-lock-type-face
     ;         'help-echo
     ;         "svn revision"))
     "]"

     ;; backup inhibited variable
     " [Bkup:"
     '(:eval (propertize (if backup-inhibited "N" "Y")
                         'face 'user-header-libera-ste-face ;'font-lock-string-face
                         ))
     "]"
     
     ;" --"
     ;; i don't want to see minor-modes; but if you want, uncomment this:
     ; minor-mode-alist  ;; list of minor modes
     ;"%-" ;; fill with '-'
     )) 
 (setq mode-line-format default-mode-line-format)

 (setq a 1)
)

(defun dot-emacs4 ()
 ".emacs in function"

 ;;  (set-face-background 'modeline "gray25")
 ;;
 ;; counter
 ;;
    (global-set-key "\C-cc" 'counter)
    (autoload 'counter "counter" nil t)

 ;; X will be the numbering register.
 ;; you can start at 1 by replacing the 0
 ;; below.
 ;(set-register 'X 0)

 ;; This increments by 1.  To increase that,
 ;; change the 1 that follows increment-register.
 ;(setq last-kbd-macro (read-kbd-macro
 ;"M-: (insert-register SPC 'X) RET M-: (increment-register SPC 1 SPC 'X) RET M-% 2*<M-p> RET 2*<M-p> RET ."))

 ;; highlight

 (defun highlight-line ()
   "TI:highlight current line. Nothing more"
   (interactive)
   (beginning-of-line)
   (let ( (bol  (point))
          (eol)  
          (ext) )
     (end-of-line)
     (setq eol (point)) 
     (beginning-of-line)
     (setq ext (make-extent bol eol))
     (make-face 'extent-face)
     (set-face-background 'extent-face "blue")
     (set-extent-face ext 'extent-face)
     ))




 ;;Select menu item Edit -> Text Properties -> List Properties
 ;; to find out face name of the text under cursor?
  (require 'facemenu)


 (setq undo-high-threshold 300000)
 (setq undo-threshold 200000)
 (setq font-lock-always-fontify-immediately t)
 ;(setq font-lock-always-fontify-immediately nil)




 ;; add-on

 ;; completion list adjustment
 (delete ".log" completion-ignored-extensions )
 (setq completion-ignored-extensions
 (append completion-ignored-extensions '(".NG") '(".anlzd")))


 ;;tmp
 (defun test1 (num n2)
   "testing..."
   (interactive "nNumber1: \nnN2:")
   (message "numbers are %d %d" num n2))

 (defun test2 ()
   "testing..."
   (interactive )
   (setq vhdl-lightning-keyword-rst "sys_rst_n")
   (setq vhdl-lightning-keyword-rst-active )
   (message (buffer-local-variables))
)
 (defun test3 ()
   "testing..."
(setq aaa (get-buffer ".emacs"))
vhdl-lightning-keyword-rst
(setq resize-mini-windows 'grow-only)
(setq max-mini-window-height 10)
(message "abcdefghi")
(member 'vhdl-lightning-keyword-rst (buffer-local-variables))
(setq message-log-max nil)
(message-box "hello")
(setq vhdl-lightning-keyword-rst-active 1)
(buffer-local-value 'vhdl-lightning-keyword-rst-active (current-buffer))
(buffer-local-value 'homedir (current-buffer))
(type-of homedir)
(type-of vhdl-lightning-keyword-rst-active)
(buffer-local-variables (get-buffer "spw_rx2.vhd"))
(symbol-plist 'vhdl-lightning-keyword-rst-active)
(symbol-plist 'homedir)
(symbol-plist 'my-var)
(defvar my-var "my value" "my documentation")
)
;; get the property list for the variable
(setq prop-list (symbol-plist 'my-var))




 (defun leval (exp)
   "TI:s-exp evaluation"
   (interactive "Xlisp expression:")
   (message "value is %s" exp ))
   

 ;; set minibuffer larger
 ;xemacs
;(let ((win (get-buffer-window (window-buffer))))
;  (enlarge-window 3) nil (select-window (minibuffer-window)))
;  (select-window win)

 (load-file (concat homedir "emacs/regexp-const.el"))
 (load-file (concat homedir "emacs/verilog-helper.el"))

 (load-file (concat homedir "emacs/vhdl-re.el"))
 (load-file (concat homedir "emacs/vhdl-helper.el"))
;; (load-file (concat homedir "emacs/vhdl-electric.el"))
 (load-file (concat homedir "emacs/vhdl-trace.el"))
 (load-file (concat homedir "emacs/vhdl-align.el"))


 ;(setq ps-postscript-code-directory "/usr/lib/xemacs/xemacs-packages/etc/ps-print")



 ;;
 ;; japanese
 ;;

 ;(require 'un-define)
 ;(coding-system-put 'utf-8 'category 'utf-8)
 ;(set-coding-category-system 'utf-8 'utf-8)
 ;(set-language-info
 ; "Japanese" 
 ; 'coding-priority (cons 'utf-8
 ;(get-language-info "Japanese" 'coding-priority)))
 ;(set-language-environment "Japanese")



 ;;(set-language-environment 'japanese)
 ;;(load "mime-setup")
 ;;  
 ;;  ; .fvwm must be in euc-jp
 ;;  (set-default-coding-systems             'iso-2022-jp)
 ;;  ;(set-default-coding-systems             'euc-jp)
 ;;  (set-default-buffer-file-coding-system  'iso-2022-jp)
 ;;  ;(set-default-buffer-file-coding-system  'euc-jp)
 ;;  (set-buffer-file-coding-system-for-read 'iso-2022-jp)
 ;;  ;(set-buffer-file-coding-system-for-read 'euc-jp)
 ;;  (set-pathname-coding-system             'iso-2022-jp)
 ;;  ;(set-pathname-coding-system             'euc-jp)
 ;;  (set-keyboard-coding-system             'iso-2022-jp)
 ;;  ;(set-keyboard-coding-system             'euc-jp)
 ;;  (set-buffer-file-coding-system          'iso-2022-jp)
 ;;  ;(set-buffer-file-coding-system          'euc-jp)
 ;;  
 ;;(set-face-font 'default 
 ;;;;               '("-*-fixed-medium-r-normal-14-*-iso8859-1"
 ;;               '(
 ;;
 ;;
 ;;               "-*-fixed-medium-r-normal--12-*-iso8859-1"
 ;;                 "-*-fixed-medium-r-normal--12-*-jisx0201.1976-*"
 ;;                 "-*-fixed-medium-r-normal--12-*-jisx0208.1983-*"
 ;;             
 ;;             "-*-fixed-medium-r-normal--20-*-iso8859-1"
 ;;             "-*-fixed-medium-r-normal--20-*-jisx0201.1976-*"
 ;;             "-*-fixed-medium-r-normal--20-*-jisx0208.1983-*"
 ;;
 ;;             "-*-fixed-medium-r-normal--16-*-iso8859-1"
 ;;             "-*-fixed-medium-r-normal--16-*-jisx0201.1976-*"
 ;;             "-*-fixed-medium-r-normal--16-*-jisx0208.1983-*"
 ;;                 ))
 ;;
       
 (global-set-key [(f1)] 'kill-region )
 (global-set-key [(f2)] 'kill-ring-save )
 (global-set-key [(f3)] 'yank )
 (global-set-key [(meta f3)] 'yank-pop )
 (global-set-key [(f4)] 'kill-line )
 (global-set-key [(f5)] 'copy-line-as-kill)

 (global-set-key [(alt ?x)] 'backward-char)
 (global-set-key [(alt ?v)] 'forward-char)
 (global-set-key [(alt ?f)] 'previous-line)
 (global-set-key [(alt ?c)] 'next-line)
 (global-set-key [(alt ?d)] 'delete-char)
 (global-set-key [(alt ?b)] 'delete-backward-char)
 (global-set-key [(meta ?n)] 'switch-to-other-buffer)


 (set-variable 'font-lock-maximum-size 1000000)

 (set-variable 'auto-save-timer 1000)
 (auto-save-mode t)

 ;; subversion interface
 ;(load "psvn")



 ;; for terminal mode
       (define-key esc-map "[A" 'previous-line)
       (define-key esc-map "[B" 'next-line)
       (define-key esc-map "[D" 'backward-char-command)
       (define-key esc-map "[C" 'forward-char-command)



 (require 'compile)

 ;; general functions
 (defun delete-spaces ()
   (interactive)
   (while (looking-at "\\(\\s-\\|$\\)+")
       (delete-char 1)
       )
 )
 (defun delete-backward-spaces ()
   "backspace as long as the character to be removed is a white space"
   (interactive)
   (while (progn 
            (forward-char -1)
            (looking-at "\\s-")
            )
     (delete-char 1)
     )
   (forward-char)
 )

 (defun search-forward-regexp-in-line (re)
   "search-forward-regexp from the point to the end of the current line"
   (interactive)
   
   (let ((eol)
         )
     (save-excursion
       (end-of-line)
       (setq eol (point))
       )
     (search-forward-regexp re eol t)
     ) 
   )
   
 (defun custom-func ()
 (custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default-toolbar-position (quote left))
  '(ecb-options-version "2.31")
  '(ediff-coding-system-for-write (quote raw-text))
  '(font-lock-maximum-size 15000)
  '(js-indent-level 3)
  '(paren-mode (quote paren) nil (paren))
  '(query-user-mail-address nil)
  '(tool-bar-mode nil)
  '(toolbar-news-reader (quote gnus))
 )



 (defun show-filename-pattern-of-specific-mode-of-auto-mode-alist (modename)
   "auto-mode-alist"
   ;;"auto-mode-alistä¸­ã®ç‰¹å®šã®ã¢ã¼ã‰ã®ãã¡ã¤ã«åã‘ã¿ã¼ã³ã’ä¸€æ™è¡¨ç¤ºã™ã"
   ;; ã·ã³ãœã«åã’åã„åãããæ™ã¯ã€"S"
   (interactive "SMode Name(ex. cperl-mode): ")
   (save-current-buffer
     (with-output-to-temp-buffer "**File Name Pattern of Specific Mode**"
       (set-buffer "**File Name Pattern of Specific Mode**")
       (insert
        (mapconcat (function
                    (lambda (cell)
                      (if (eq modename (cdr cell))
                          (concat (car cell) "\n")
                        nil)))
                   auto-mode-alist "")))))

 (defun re-font-lock ()
   "re-apply font-lock mode"
   (interactive)
   (font-lock-mode)
   (font-lock-mode)
 )      



 ;; function to evaluate string
 (defun eval-string (str) (eval (read str)))


 ;; functions to make it compatible with xemacs
 (defun replace-in-string (str regexp text) 
   "replace regexp in str with text, using replace-regexp-in-string
 reordered arguments."
   (replace-regexp-in-string regexp text str t t)
 )

 
)

(defun dot-emacs5 ()
 ".emacs in function"

 ;; coding system
 (set-default-coding-systems             'utf-8-unix)
 ;;(set-default-buffer-file-coding-system  'utf-8-unix)
 ;;   (set-buffer-file-coding-system          'utf-8-unix)

 ;;
 ;; initial size
 ;;
 (add-to-list 'default-frame-alist '(left . 0))
 (add-to-list 'default-frame-alist '(top . 0))
 (add-to-list 'default-frame-alist '(height . 50))
 (add-to-list 'default-frame-alist '(width . 124))

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 400)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

 ;; 
 ;; lightning-verilog  key defs
 ;;

 ;; collection of electrc keys defined here
 ;;   '("g"                  ;; key
 ;;     (list                ;; list of translation strings
 ;;      '("gg")
 ;;      '("generate\nif ()" 16 t )   ;; arg 2 is insertion point
 ;;                                   ;; arg 3 t to resume, key translation
 ;;      '("endgenerate")
 ;;      ) )
 ;;
 ;; alternative format
 ;;   '("y"
 ;;     '(
 ;;       ("yy")
 ;;       ("abcdef")
 ;;       ("z sldfj ")
 ;;       ))

 (setq verilog-lightning-key-list
   (list
    '("a"
      (list
       '("aa")
       '("always @(posedge clk) begin\nif (~rst_f) begin\n" nil t)
       '("always_comb begin\n\nend" 18 t)
       '("assign")
       '("always @(posedge clk) begin")
       )  )

    '("b"
      (list 
       '("bb")
       '("begin\n\nend" 6 t)
       '("begin")
       ) ) 

    '("c"
      (list 
       '("cc")
       '("case ()\n\nendcase" 8 t)
       '("endcase")
       ) )

    '("d"
      (list 
       '("dd")
       '("default: begin\n\nend" 8 t)
       ) ) 

    '("e"
      (list 
       '("ee")
       '("end")
       '("end else begin")
       '("end else if")
       '("endfunction")
       '("endmodule")
       ) ) 

    '("f"
      (list 
       '("ffff")
       '("for (")
       '("function automatic")
       '("for (ix=0; ix<7; ix=ix+1) begin")
       ) ) 

    '("g"
      (list 
       '("gg")
       '("generate\nif ()" 13 t )
       '("endgenerate")
       ) )

    '("i"
      (list 
       '("ii")
       '("inout")
       '("integer")
       '("initial")
       '("interface")
       '("input  wire")
       ))

    '("l"
      (list 
       '("ll")
       '("localparam")
       '("logic [31:0]")
       '("logic")
       ))

    '("m"
      (list 
       '("mm")
       '("module ")
       '("endmodule")
       '("modport")
       ) ) 

    '("o"
      (list 
       '("oo")
       '("output logic")
       ) ) 

    '("p"
      (list 
       '("pp")
       '("parameter")
       ) ) 

    '("v"
      (list 
       '("vv")
       '("virtual")
       ) ) 

    '("w"
      (list 
       '("ww")
       '("wire")
       ) ) 

    '("["
      (list 
       '("[")
       '("[7:0]")
       '("[15:0]")
       '("[31:0]")
       '("[63:0]")
       ) ) 

    '(";"
      (list 
       '(";")
       '(":")
       ) ) 

    '(","
      (list 
       '(",") 
       ;; '(" ,")   have not impletmented or-function
       '("<= ")
       '("< ")
       '("<< ")
       ) ) 

    ;; alternative format test
    '("y"
      '(
        ("yy")
        ("begin\nend\n" 6 t)
        ("abcdef ")
        ))

    ;; no translation
    '("x"   '() )

  
 ))

 ;;
 ;; lightning key
 ;;
 (load (concat homedir "emacs/lightning-keys.el"))

 
 (setq vhdl-lightning-key-list
   (list
    '("a" 
      (list
       '("aa")
              '("process \n  begin\n" 0 t)
       '("architecture")
       '("architecture  of  is\nbegin\nend architecture ;" 12 t)
       '("after")
       ) )
    '("b" 
      (list 
       '("bb")
       '("boolean")
       '("process\nbegin\n\nend process;" 0 t)
       '("block\nbegin\n\nend block;" 0 t)
       '("begin")
       ) )

    '("c" 
      (list
       '("cc")
       '("constant")
       '("case  is\nwhen ST_IDLE =>\nwhen others =>\n-- seu\nend case;" 5 t t)
       '("case")
       '("end case;")
       ) )
      '("d"  
        (list
         '("dd")
         '("downto 0)")
         '("downto")
         '("dummy_1   : std_ulogic;")
         ))
      '("e" 
        (list
         '("ee")
         '("end if;")
         '("else")
         '("elsif")
         '("end process;")
         '("end function;")
         '("end loop;")
         '("end case;")
         '("end procedure;")
         ))
      '("f" (list
             '("ffff")
             '("FIFO ")
             '("function")
             '("end function;")
             '("for ix in  loop\nend loop;" 10 t)
             '("falling_edge(")
             '("falling_edge(clk) begin\n")
             '("file")
             ))
      '("g" (list
             '("gg")
             '("generic")
             ))
      '("i" (list
             '("ii")
             '("integer")
             '("integer range")
             '("if then\nend if;\n" 3 t)
             ))
      '("l" (list
             '("ll")
             '("loop")
             '("end loop;")
             ))
      '("n" (list
             '("nn")
             '("natural")
             '("natural range 0 to ")
             ))
      '("o" (list
             '("oo")
             '("(others =>'0')")
             '("(others => (others =>'0'))")
             '("others =>\n" 0 t)
             ))
      '("P" (list
              '("PP")
              '("procedure (")
              '("end procedure;")
             ))
      '("p" (list
              '("pp")
              (progn (setq vhdl-process-async-start 
                           (concat "process (" vhdl-signal-sys-clk ", " vhdl-signal-sys-rst ")\n  begin\n    if " 
                                   vhdl-signal-sys-rst "='" vhdl-signal-sys-rst-active
                                   "' then \n    -- under reset\n\n   "))
                     (list (concat vhdl-process-async-start "    elsif rising_edge(" vhdl-signal-sys-clk 
                                   ") then\n    -- in operation\n    end if;\n  end process;") 0 t))
              (list (concat vhdl-process-async-start "state <= ST_IDLE;\n    elsif rising_edge(" vhdl-signal-sys-clk ") then\n    -- in operation\n    case state is\n       when ST_IDLE =>\n      when others =>\n     end case;\n    end if;\n  end process;") 0 t)
              (progn (setq vhdl-process-sync-start
                           (concat "process (" vhdl-signal-sys-clk ")\n  begin\n    if rising_edge(" vhdl-signal-sys-clk 
                            ") then\n    " ))
                     (list (concat vhdl-process-sync-start "end if;\n  end process;") 18 t))
              (list (concat vhdl-process-sync-start "if " vhdl-signal-sys-rst "='" vhdl-signal-sys-rst-active "' then\n") 12 t)
              (list vhdl-process-sync-start 12 t)
              '("procedure")
              '("process")
              ))
      '("r" (list
             '("rr")
             '("RMAP ")
             '("RFPE ")
             ))
      '("s" (list
              '("ss")
              '("signal")
              '("std_ulogic;")
              '("std_ulogic_vector(")
              '("std_logic;")
              '("std_logic_vector(")
              '("subtype")
              '("severity")
              ))
      '("t" (list
             '("tt")
             '("transmit")
             ))
      '("u" (list
             '("uu")
             '("unsigned")
             '("UART ")
             '("suv_8")
             '("suv_16")
             '("suv_32")
             ))
      '("v" (list
             '("vv")
             '("variable")
             ))
      '("y"
        '(
          ("yy")
          ("begin\nend\n" 6 t)
          ("abcdefg ")
          ("one\ntwo\nthree\n ")
          )        )
      
      '("z" (list
             '("zz")
             '("ZERO_8")
             '("ZERO_16")
             '("ZERO_32")
             '("ZERO_64")
             ))
      '("-"
        (list
         '("--")
         '("--------------------------------------------------")
         ))
      '("'"
        (list
         '("''")
         '("\"")
         '("'0'")
         '("'1'")
         ))
      '(","
        (list 
         '(",") 
         '("<= ") 
         '("<= '0';") 
         '("<= '1';") 
         '("<= (others => '0');") 
         ))
      '("."
        (list 
         '(".") 
         '(" => ") 
         '(" => '0'") 
         '(" => (others => '0');") 
         ))
      '("="
        (list
         '("==")
         '(" = '1' then\n")
         ))
      )
   )


 (defun reload-lightning-vhdl ()
   "reload lightning keys in vhdl mode"
   (interactive)
   (lightning-keys-setup 'vhdl-mode 'vhdl-mode-map vhdl-lightning-key-list )
   )

 (setq scad-lightning-key-list
       (list
        
        '("c"
          (list
           '("cc")
           '("cube([], center=true);" 6 t)
           '("cylinder(r=, center=true);" 11 t)
           '("circle();" 7 t)
           )  )
        
        '("d"
          (list
           '("dd")
           '("difference() {")
           )  )
        
        '("i"
          (list
           '("ii")
           '("intersection() {")
           '("include<>;" 8 t)
           )  )
        
        '("m"
          (list
           '("mm")
           '("module() {" 7 t)
           )  )
        
        '("r"
          (list
           '("rr")
           '("rotate([]) " 8 t)
           )  )
        
        '("s"
          (list
           '("ss")
           '("sphere();" 7 t)
           '("square([]);" 8 t)
           )  )
        
        '("t"
          (list
           '("tt")
           '("translate([]) " 11 t)
           )  )

        '("u"
          (list
           '("uu")
           '("union() {")
           )  )
        
        ))


 (remove-hook 'scad-mode-hook 'scad-mode-init)
 (add-hook 'scad-mode-hook 'scad-mode-init)

 (defun scad-mode-init ()
              
   (lightning-keys-setup 'scad-mode 'scad-mode-map
                         scad-lightning-key-list )
   (setq scad-indent-style "stroustrup")
   (turn-on-font-lock)
   ) 



)

(defun dot-emacs6 ()
 ".emacs  dark color theme "
 
 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default ((t (:foreground "white" :background "black" :size "11 pt" :family "DejaVu Sans Mono"))))
  '(attr-select ((t (:foreground "DarkOliveGreen1" :background "cadet blue"))) t)
  '(attr-yomi ((t (:foreground "gray85" :background "blue"))) t)
  '(bold ((t (:bold t))))
  '(bold-italic ((t (:bold t :italic t))))
  '(comint-input-face ((((class color) (background light)) (:foreground "skyblue"))))
  '(cperl-array-face ((((class color) (background light)) (:foreground "#fdb" :bold t))))
  '(cperl-hash-face ((((class color) (background light)) (:foreground "orange2" :bold t :italic t))))
  '(custom-button-face ((t (:bold t))))
  '(custom-comment-tag-face ((((class color) (background light)) (:foreground "#aaaaff"))))
  '(custom-group-tag-face ((((class color) (background light)) (:foreground "cyan" :underline t))))
  '(custom-invalid-face ((((class color)) (:foreground "yellow" :background "orchid"))))
  '(custom-state-face ((((class color) (background light)) (:foreground "green3"))))
  '(custom-variable-tag-face ((((class color) (background light)) (:underline t :foreground "skyblue2"))))
  '(dired-face-boring ((((class color)) (:foreground "grey80" :background "#113355"))))
  '(dired-face-directory ((t (:background "gray55"))))
  '(dired-face-socket ((((class color)) (:foreground "purple"))))
  '(font-lock-comment-face ((t (:foreground "salmon"))))
  '(font-lock-doc-string-face ((t (:foreground "seagreen2"))))
  '(font-lock-function-name-face ((t (:foreground "skyblue3"))))
  '(font-lock-keyword-face ((t (:foreground "#ee9"))))
  '(font-lock-other-type-face ((t (:foreground "#aaf"))) t)
  '(font-lock-preprocessor-face ((t (:foreground "skyblue"))))
  '(font-lock-reference-face ((t (:foreground "orange"))))
  '(font-lock-string-face ((t (:foreground "green3"))))
  '(font-lock-type-face ((t (:foreground "#cd4"))))
  '(font-lock-variable-name-face ((t (:foreground "#8ff"))))
  '(font-lock-when-operator-face ((t (:foreground "#ff8"))) t)
  '(gnus-emphasis-bold ((t (:bold t))))
  '(gnus-group-mail-1-empty-face ((((class color) (background light)) (:foreground "DeepPink"))))
  '(gnus-group-mail-1-face ((((class color) (background light)) (:bold t :foreground "DeepPink"))))
  '(gnus-group-mail-3-empty-face ((((class color) (background light)) (:foreground "magenta"))))
  '(gnus-group-mail-3-face ((((class color) (background light)) (:bold t :foreground "magenta"))))
  '(gnus-group-mail-low-empty-face ((((class color) (background light)) (:foreground "DeepPink"))))
  '(gnus-group-mail-low-face ((((class color) (background light)) (:bold t :foreground "DeepPink"))))
  '(gnus-group-news-low-empty-face ((((class color) (background light)) (:foreground "green3"))))
  '(gnus-group-news-low-face ((((class color) (background light)) (:bold t :foreground "green3"))))
  '(gnus-header-content-face ((((class color) (background light)) (:italic t :foreground "indianred1"))))
  '(gnus-header-name-face ((((class color) (background light)) (:foreground "maroon1"))))
  '(gnus-header-newsgroups-face ((((class color) (background light)) (:bold t :italic t :foreground "cyan3"))))
  '(gnus-header-subject-face ((((class color) (background light)) (:bold t :foreground "maroon1"))))
  '(gnus-summary-high-read-face ((((class color) (background light)) (:bold t :foreground "green3"))))
  '(gnus-summary-low-ancient-face ((((class color) (background light)) (:italic t :foreground "skyblue"))))
  '(gnus-summary-low-read-face ((((class color) (background light)) (:italic t :foreground "green3"))))
  '(gnus-summary-low-ticked-face ((((class color) (background light)) (:italic t :foreground "firebrick1"))))
  '(gnus-summary-normal-read-face ((((class color) (background light)) (:foreground "green3"))))
  '(gnus-summary-normal-ticked-face ((((class color) (background light)) (:foreground "firebrick1"))))
  '(highlight ((t (:foreground "black" :background "darkseagreen2"))))
  '(hyper-apropos-documentation ((((class color) (background light)) (:foreground "seagreen3"))))
  '(hyper-apropos-heading ((t (:foreground "khaki2"))))
  '(hyper-apropos-hyperlink ((((class color) (background light)) (:foreground "skyblue"))))
  '(hyper-apropos-major-heading ((t (:foreground "green3"))))
  '(hyper-apropos-section-heading ((t (:foreground "cyan3"))))
  '(hyper-apropos-warning ((t (:foreground "red"))))
  '(isearch ((t (:foreground "black" :background "paleturquoise"))))
  '(isearch-secondary ((t (:foreground "red2"))) t)
  '(italic ((t (:family " misc fixed medium i normal  0 0 75 75 c 0 iso8859 1" :italic t))))
  '(list-mode-item-selected ((t (:background "gray88"))) t)
  '(message-cited-text ((t nil)))
  '(message-header-contents ((t nil)))
  '(message-highlighd-header-contents ((t (:bold t))))
  '(mode-line-buffer-id ((t (:foreground "blue" :family "courier"))))
  '(modeline ((t (:foreground "gray44" :background "#113355"))) t)
  '(modeline-mousable ((t (:foreground "violetred"))) t)
  '(modeline-mousable-minor-mode ((t (:foreground "green3"))) t)
  '(paren-match ((t (:foreground "gray22" :background "darkseagreen2"))) t)
  '(region ((t (:background "#eeeeaa"))) t)
  '(shell-output-2-face ((t (:foreground "slategray2"))) t)
  '(shell-output-face ((((class color) (background light)) (:foreground "gray90" :italic nil))) t)
  '(svn-status-directory-face ((((class color) (background light)) (:foreground "#18f"))))
  '(text-cursor ((t (:foreground "black" :background "Red3"))) t)
  '(toolbar ((t (:foreground "blue"))) t)
  '(vhdl-font-lock-enumvalue-face ((((class color) (background light)) (:foreground "pink2"))))
  '(widget-documentation ((((class color) (background light)) (:foreground "green3"))))
  '(widget-field ((((class grayscale color) (background light)) (:foreground "black" :background "gray55"))))
  '(widget-inactive ((((class grayscale color) (background light)) (:foreground "gray55"))))
  '(zmacs-region ((t (:foreground "black" :background "gray55"))) t)) 

)


(defun dot-emacs7 ()
 ".emacs faces"

 (custom-set-faces
  '(region ((t (:foreground "black" :background "turquoise"))))
  )
 
 (defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

 (defun get-face ()
   (interactive)
   (let ((face (get-char-property (point) 'face) )
         )
     (message "Face: %s" face) )
   )

 )


(defun dot-emacs8 ()
 ".emacs setup"

 (custom-set-variables
  ;; uncomment to always end a file with a newline
  ;'(require-final-newline t)
  ;; uncomment to disable loading of "default.el" at startup
  ;'(inhibit-default-init t)
  ;; default to unified diffs
  '(diff-switches "-u"))

 ;;; uncomment for CJK utf-8 support for non-Asian users
 ;; (require 'un-define)
 (put 'downcase-region 'disabled nil)

 (defun insert-backtick () 
   "f17 - backtick-atmark "
   (interactive)
   (insert-string "`"))

 (defun press-kp-sub ()
   "kp subtract for macron"
   (interactive)
   (insert-string "¯")
   )

 (defun press-kp-div ()
   "insert lamda character"
   (interactive)
   (insert-string "Λ")   ;  Lambda U+039B
   )

;;;  (eval-after-load "isearch" '(require 'isearch+))



 (defun tok-align-command ( tok col)
  "align vertically. It takes 2 arguments, a regexp 'token' and a column number.
It aligns line starting from 'token' at the position column."
  (interactive "sregexp:\nncolumn number:")
  (message "tok: %s, col: %d" tok  col)
  (save-excursion

    (let ( (cmd ?=) )
      (beginning-of-line)
      (while (and (not (char-equal cmd ?q))
                  (search-forward-regexp tok) )
        (if (char-equal cmd ?!)
            (tok-align-arg tok col) ;; all y to the end
          (setq cmd (read-char "align y/n/q?"))
          (if (or (char-equal cmd ?y) (char-equal cmd ?!))
              (tok-align-arg tok col)
            )
          )
        )
      )  ) )


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
 
 )  ; end of dot-emacs8

(defun dot-emacs9 ()
 ".emacs setup of unicode characters"

 (defun draw-box-horizontal ()
   (interactive)
   (insert-string "─") ; ───
   )

 (defun draw-box-corner-left-bottom ()
   (interactive)
   (insert-string "└")  ; └───┐
   )

 (defun draw-box-vertical ()
   (interactive)
   (insert-string "│")  ; ───┐
   )                    ;    │

 (defun draw-box-corner-right-top ()
   (interactive)
   (insert-string "┐")  ; ──┐
   )                    ; 

 (defun draw-box-corner-left-top ()
   (interactive)
   (insert-string "┌")  ; ┌
   )                    ;  

 (defun draw-box-corner-right-bottom ()
   (interactive)
   (insert-string "┘")  ; ┌─
   )                    ; │

 (defun draw-box-3way-left ()
   (interactive)
   (insert-string "┤")  ; ┌─┤
   )                    

 (defun draw-box-3way-top ()
   (interactive)
   (insert-string "┴")  ; ┌─┴
   )                    

  (defun draw-box-3way-right ()
   (interactive)
   (insert-string "├")  ; ├──
   )                    

 (defun draw-box-3way-left ()
   (interactive)
   (insert-string "┬")  ; ─┬
   )                    

 (defun draw-box-cross ()
   (interactive)
   (insert-string "┼")  ; ┌─┼
   )                    
 
 )
;; "   " U+00a0 non breakable space
;; end of dot-emacs9

(dot-emacs9)

(defun dot-emacs10 ()
  "defines more functions"

  (defun tok-align-arg ( tok col)
    "align 'tok' position at column 'col'.
search token (tok) in the current line. if found move the rest of the line to
column postion (col)."
    (let (
          (tlen)
          (ccn) ; current column number
          (ep) ; eol position
          )
      (end-of-line)
      (setq ep (point))
      (beginning-of-line)
      (if (search-forward-regexp tok ep t) ; if seach to eol and found
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
              )
            (forward-char tlen) ; cursor on char after token
            
            ) ) ) )
  
                                        ;   (require 'undo-tree)
                                        ;   (global-undo-tree-mode)
  
  (defun eval-string (exp)
    "evaluate <exp>"
    (eval-expression exp)
    )
  
  (defun face-at-point-command ()
    (interactive)
    (message "%s" (face-at-point))
    )

  (defun gcode-remove-temp-settings ()
    "remove the initial temperature settings in g-code"
    (interactive)
    (beginning-of-buffer)
    (let 
      ;; seach and replace temperature code until it hits M82 code
      (beginning-of-buffer)
      (while (and (search-forward-regexp
               "^m\\(140\\|105\\|190\\|104\\|109\\|82\\).*" )
                  (not (equal (match-string 1) "82")))
        (replace-match "; \\&")
        )
      ) )


) ; end of dot-emacs10

(defun dot-emacs11 ()
  ".emacs section 11"
  (setq treesit-face-mapping
      '((comment              . font-lock-comment-face)
        (string               . font-lock-string-face)
        (escape-sequence      . font-lock-regexp-grouping-backslash)
        (keyword              . font-lock-keyword-face)
        (operator             . font-lock-builtin-face)
        (function             . font-lock-function-name-face)
        (function.call        . font-lock-function-name-face)
        (function.method      . font-lock-function-name-face)
        (function.builtin     . font-lock-builtin-face)
        (function.macro       . font-lock-preprocessor-face)
        (type                 . font-lock-type-face)
        (type.builtin         . font-lock-builtin-face)
        (variable             . font-lock-variable-name-face)
        (variable.parameter   . font-lock-variable-name-face)
        (variable.special     . font-lock-constant-face)
        (constant             . font-lock-constant-face)
        (number               . font-lock-constant-face)
        (boolean              . font-lock-constant-face)
        (property             . font-lock-variable-name-face)
        (punctuation.delimiter . font-lock-delimiter-face)
        (punctuation.bracket  . font-lock-delimiter-face)
        (tag                  . font-lock-keyword-face)
        (attribute            . font-lock-variable-name-face)
        (label                . font-lock-constant-face)
        (namespace            . font-lock-constant-face)
        (module               . font-lock-type-face)
        (field                . font-lock-variable-name-face)
        (constructor          . font-lock-type-face)
        (annotation           . font-lock-preprocessor-face)))
  (setq treesit-font-lock-level 3)
)



  ;; choose chromium as default browser
  (setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")
  )


(dot-emacs10)



 ;; smaller chunks for easier debugging

 (dot-emacs1)
 (dot-emacs2)
 (dot-emacs3)
 (dot-emacs31)
 (dot-emacs32)

 (dot-emacs4)
 (dot-emacs5)
; (dot-emacs6)
 (dot-emacs7)
 (dot-emacs8)
 (dot-emacs9)
; (require 'edit-server)
; (edit-server-start)
 (dot-emacs11)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("c0171bb428327b3a8d6040c8e2838651261f106843e3cc68f535c45d4ea94eda" default))
 '(default-toolbar-position 'left)
 '(diff-switches "-u")
 '(ecb-options-version "2.31")
 '(ediff-coding-system-for-write 'raw-text)
 '(ediff-split-window-function 'split-window-horizontally)
 '(font-lock-maximum-size 500000 t)
 '(js-indent-level 4)
 '(package-selected-packages
   '(copilot-chat copilot org-ai modus-themes tree-sitter treesit-auto flycheck vhdl-ext vhdl-ts-mode "uniline" "uniline" "uniline" "uniline" "uniline" es-lib svg-clock js2-mode lsp-mode elisp-demos))
 '(paren-mode 'paren nil (paren))
 '(query-user-mail-address nil)
 '(tool-bar-mode nil)
 '(toolbar-news-reader 'gnus)
 '(vhdl-end-comment-column 199))
;(custom-set-faces
; ;; custom-set-faces was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "outline" :family "Courier New"))))
; '(font-lock-keyword-face ((t (:foreground "#8c3"))))
; '(font-lock-variable-name-face ((t (:foreground "medium orchid"))))
; '(region ((t (:foreground "black" :background "turquoise")))))



(add-to-list 'custom-theme-load-path (concat homedir "emacs/themes/"))
;(load-theme `tron t)



;;
;; folding mode
;;
;(if (require 'folding nil 'noerror)
;    (folding-mode-add-find-file-hook)
;  (message "Library `folding' not found"))

;(folding-add-to-marks-list 'ruby-mode "#{{{" "#}}}" nil t)
;(folding-add-to-marks-list 'vhdl-mode ".*process" "end process" nil t)


;;
;; tree-sitter parser
;;
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (vhdl "https://github.com/alemuller/tree-sitter-vhdl" "main" "src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;(use-package flycheck
;  :ensure t
;  :init (global-flycheck-mode))
(require 'flycheck)

;; vhdl-ext
(use-package vhdl-ext
  :hook ((vhdl-mode . vhdl-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET vhdl-ext':
  ;; Comment out/remove the ones you do not need
  (setq vhdl-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          lsp
          lsp-bridge
          lspce
          flycheck
          beautify
          navigation
          template
          compilation
          imenu
          which-func
          hideshow
          time-stamp
          ports))
  :config
  (vhdl-ext-mode-setup)) 





;;
;; more setup in  ~/emacs/custom.el
;;
(load-file (concat homedir "/emacs/custom.el"))



;;; testing (list) and '()
(setq test1 '(a b 1))
(setq test2 (list 'a 'b 1))
(equal test1 test2)
(setq test3 '("ab" "cd" 1))
(setq test4 (list "ab" "cd" 1))
(equal test3 test4)
(setq test5 '( (list "ab" "cd" 1) (2 a b) 4))
(setq test6 (list '( "ab" "cd" 1) 2 4))
(setq test7 '( (list '("ab" "cd" 1) 2 4)))
(setq test8 '( ( ("ab" "cd" 1) 2 4)))
      (nth 0 (nth 0 test8))
(setq test9 '( '("a" "b") '("a2")))
(setq test10 '( (quote ("a" "b")) (quote ("a2"))))
(setq test11 '( ("a" "b") ("a2")))

(defun test2 ()
    (interactive)
  (frame-height)
(setq frame-title-format "emacs - %f %i")
)
(defun test3 ()
  (interactive)
  (rename-frame (selected-frame) (concat "emacsclient - " (buffer-name)))
  (rename-frame (selected-frame) (concat "emacs - " (buffer-name)))

)


 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "white" :background "black" :height 120 :family "Monospace"))))
 '(attr-select ((t (:foreground "DarkOliveGreen1" :background "cadet blue"))) t)
 '(attr-yomi ((t (:foreground "gray85" :background "blue"))) t)
 '(bold ((t (:bold t))))
 '(bold-italic ((t (:bold t :italic t))))
 '(comint-input-face ((((class color) (background light)) (:foreground "skyblue"))))
 '(cperl-array-face ((((class color) (background light)) (:foreground "#fdb" :bold t))))
 '(cperl-hash-face ((((class color) (background light)) (:foreground "orange2" :bold t :italic t))))
 '(custom-button-face ((t (:bold t))))
 '(custom-comment-tag-face ((((class color) (background light)) (:foreground "#aaaaff"))))
 '(custom-group-tag-face ((((class color) (background light)) (:foreground "cyan" :underline t))))
 '(custom-invalid-face ((((class color)) (:foreground "yellow" :background "orchid"))))
 '(custom-state-face ((((class color) (background light)) (:foreground "green3"))))
 '(custom-variable-tag-face ((((class color) (background light)) (:underline t :foreground "skyblue2"))))
 '(dired-face-boring ((((class color)) (:foreground "grey80" :background "#113355"))))
 '(dired-face-directory ((t (:background "gray55"))))
 '(dired-face-socket ((((class color)) (:foreground "purple"))))
 '(font-lock-comment-face ((t (:foreground "salmon"))))
 '(font-lock-doc-string-face ((t (:foreground "seagreen2"))))
 '(font-lock-function-name-face ((t (:foreground "skyblue3"))))
 '(font-lock-keyword-face ((t (:foreground "#ee9"))))
 '(font-lock-other-type-face ((t (:foreground "#aaf"))) t)
 '(font-lock-preprocessor-face ((t (:foreground "skyblue"))))
 '(font-lock-reference-face ((t (:foreground "orange"))))
 '(font-lock-string-face ((t (:foreground "green3"))))
 '(font-lock-type-face ((t (:foreground "#cd4"))))
 '(font-lock-variable-name-face ((t (:foreground "#8ff"))))
 '(font-lock-when-operator-face ((t (:foreground "#ff8"))) t)
 '(gnus-emphasis-bold ((t (:bold t))))
 '(gnus-group-mail-1-empty-face ((((class color) (background light)) (:foreground "DeepPink"))))
 '(gnus-group-mail-1-face ((((class color) (background light)) (:bold t :foreground "DeepPink"))))
 '(gnus-group-mail-3-empty-face ((((class color) (background light)) (:foreground "magenta"))))
 '(gnus-group-mail-3-face ((((class color) (background light)) (:bold t :foreground "magenta"))))
 '(gnus-group-mail-low-empty-face ((((class color) (background light)) (:foreground "DeepPink"))))
 '(gnus-group-mail-low-face ((((class color) (background light)) (:bold t :foreground "DeepPink"))))
 '(gnus-group-news-low-empty-face ((((class color) (background light)) (:foreground "green3"))))
 '(gnus-group-news-low-face ((((class color) (background light)) (:bold t :foreground "green3"))))
 '(gnus-header-content-face ((((class color) (background light)) (:italic t :foreground "indianred1"))))
 '(gnus-header-name-face ((((class color) (background light)) (:foreground "maroon1"))))
 '(gnus-header-newsgroups-face ((((class color) (background light)) (:bold t :italic t :foreground "cyan3"))))
 '(gnus-header-subject-face ((((class color) (background light)) (:bold t :foreground "maroon1"))))
 '(gnus-summary-high-read-face ((((class color) (background light)) (:bold t :foreground "green3"))))
 '(gnus-summary-low-ancient-face ((((class color) (background light)) (:italic t :foreground "skyblue"))))
 '(gnus-summary-low-read-face ((((class color) (background light)) (:italic t :foreground "green3"))))
 '(gnus-summary-low-ticked-face ((((class color) (background light)) (:italic t :foreground "firebrick1"))))
 '(gnus-summary-normal-read-face ((((class color) (background light)) (:foreground "green3"))))
 '(gnus-summary-normal-ticked-face ((((class color) (background light)) (:foreground "firebrick1"))))
 '(highlight ((t (:foreground "black" :background "darkseagreen2"))))
 '(hyper-apropos-documentation ((((class color) (background light)) (:foreground "seagreen3"))))
 '(hyper-apropos-heading ((t (:foreground "khaki2"))))
 '(hyper-apropos-hyperlink ((((class color) (background light)) (:foreground "skyblue"))))
 '(hyper-apropos-major-heading ((t (:foreground "green3"))))
 '(hyper-apropos-section-heading ((t (:foreground "cyan3"))))
 '(hyper-apropos-warning ((t (:foreground "red"))))
 '(isearch ((t (:foreground "black" :background "paleturquoise"))))
 '(isearch-secondary ((t (:foreground "red2"))) t)
 '(italic ((t (:family " misc fixed medium i normal  0 0 75 75 c 0 iso8859 1" :italic t))))
 '(list-mode-item-selected ((t (:background "gray88"))) t)
 '(message-cited-text ((t nil)) t)
 '(message-header-contents ((t nil)))
 '(message-highlighted-header-contents ((t (:bold t))))
 '(modeline ((t (:foreground "gray55" :background "#113355"))) t)
 '(modeline-buffer-id ((t (:foreground "skyblue3" :family "courier"))) t)
 '(modeline-mousable ((t (:foreground "violetred"))) t)
 '(modeline-mousable-minor-mode ((t (:foreground "green3"))) t)
 '(paren-match ((t (:foreground "gray22" :background "darkseagreen2"))) t)
 '(region ((t (:foreground "black" :background "turquoise"))))
 '(shell-output-2-face ((t (:foreground "slategray2"))) t)
 '(shell-output-face ((((class color) (background light)) (:foreground "gray90" :italic nil))) t)
 '(svn-status-directory-face ((((class color) (background light)) (:foreground "#18f"))))
 '(text-cursor ((t (:foreground "black" :background "Red3"))) t)
 '(toolbar ((t (:foreground "blue"))) t)
 '(vhdl-font-lock-enumvalue-face ((((class color) (background light)) (:foreground "pink2"))))
 '(widget-documentation-face ((((class color) (background light)) (:foreground "green3"))))
 '(widget-field-face ((((class grayscale color) (background light)) (:foreground "black" :background "gray55"))))
 '(widget-inactive-face ((((class grayscale color) (background light)) (:foreground "gray55"))))
 '(zmacs-region ((t (:foreground "black" :background "gray55"))) t))

(defun split-window-three-horizontally ()
  "Split the current window into three equal horizontal sections."
  (interactive)
  (when (= 1 (length (window-list)))
    (let ((height (/ (window-height) 3)))  ;; Calculate 1/3 of the current window height
      (split-window-below height)          ;; Split into two windows
      (other-window 1)
      (split-window-below height)))        ;; Split again into three
  (balance-windows))                        ;; Balance all windows for equal size




