;; backup using svn
;; everything saved with xemacs is backed up in 
;; $HOME/trunk directory
;; USAGE
;; create svn repository $HOME/backup/repos and checkout $HOME/backup/trunk
;; add these 2 lines in .xemacs
;; (autoload 'save-file-with-backup "backup.el") 
;; (define-key ctl-x-map "\C-s" 'save-file-with-backup)

;; modified for emacs

;; LANG should be "c"
;; utf doesn't work. svn returns utf code that can't be decipher

;;
;; interactive commands
;;
;;(defun save-file-with-backup ()
;;(defun toggle-backup-inhibited ()
;;(defun update-svn-rev ()
;;(defun get-backup-name (buf s-home s-trunk s-buffer s-backup s-backup-dir)
;;(defun svn-view-rev ()
;;(defun save-buffer-after-untabify ()
;;(defun save-all-buffers-after-untabify ()
;;

;; #::save-date 2022-11-07 10:19:42::#
;; 2022-11-07 1.2.1    Added save-date time stamp feature


;;
;; parameters
;;
;(setq svn-cmd "LD_LIBRARY_PATH=$HOME/usr/lib; svn")
(setq svn-cmd "svn")
(setq grep-cmd "grep")

(setq backup-inhibited t)
(setq backup-revision-write t) ; write backup revision to variable : #:svn-backup-rev   :#

(setq untabify-inhibited nil) ; default is to replace tab to spaces before saving

(setq user-home homedir)
(setq backup-trunk "backup/trunk") 
(setq version "1.2.1")
;;
;;

(defvar backup-mask 
  "\\(/acc[0-9]?\\|\\.vo\\|\\.vho\\|\\.sdf\\|\\.ps\\|\\.pdf\\|\\.edf\\|\\.edn\\|\\.tmp\\)$" 
"if the buffer's full path name matches backup-filter regexp, then
subversion backup will be not created. 
Set this to nil if all files are to be backed up"
)

(defvar non-untabify-list
"^\\(Makefile\\|deps\\|dep_list\\|configure\\)"
"if the buffer's full path name matches this regexp, then
untabify will not be done."
)


;;
;; not working in windows environment
;;
(defun save-file-with-backup (commit-comment)
  "save backup file in $HOME/backup/trunk directory with full path name 
commit the new backup file to subversion repository backup/repos
save the original file. It asks for a comment for svn commit operation."

  (interactive "sLog comment:")
  (if (buffer-modified-p)
      
      (let( (home)
            (trunk)
            (file-name)
            (win-file-name) ;; contains drive name c: 
            (backup-name)
            (backup-dir)
            (status "")
            (new)
            (svn-cmd-str "") 
            (svn-status "")
            (buffer-name-v (buffer-name))
            )

        (if (string= commit-comment "")
            (setq commit-comment "auto"))

        ;; update svn back up revision number
        (update-svn-rev)

        ;; update save-date code
        (update-save-date)
        
        ;; unless Makefile,expand tabs to spaces
        (if (and (not untabify-inhibited) (not (string-match non-untabify-list (buffer-name)))   )
            (save-excursion
              (mark-whole-buffer)
              (untabify (point-min) (point-max))
              (deactivate-mark)
              ))

        (if (string-match "\\.vhdl?$" (buffer-name)   )
            (vhdl-replace-line-number) )

        (if (or (string-match "\\.v$" (buffer-name)   )
                (string-match "\\.vh$" (buffer-name)   ))
            (verilog-replace-line-number) )

       
        (get-backup-name nil 'home 'trunk 'file-name 'backup-name 'backup-dir 'win-file-name)
        ;(get-backup-name nil 'trunk 'file-name 'backup-name 'backup-dir)

        ;; exclude backup-inhibited file, backup themselves, or masked
        (if (or backup-inhibited 
                (and backup-mask (string-match backup-mask file-name))
                (string-match "/backup/" file-name)
                )
            ;; masked. just save
            (progn
              (save-buffer)
              (message (concat "saving " file-name))
              )

          ;;  save file in backup directory and real directory
          (progn
            ; try to save both real file and backup file even one fails
            ;(ignore-errors
            ;      (delete-file file-name))
            ;(write-file file-name)
            (save-buffer)
          
            (condition-case err
                (progn
                  
                  ;; create directory if necessary
                  ;(eshell-command-to-value  somehow it doesn't creat the directory...
                  (eshell-command
                   (concat "mkdir -p " backup-dir))
                  ;(write-file backup-name)
                  (eshell-command (concat "cp -p " win-file-name " " backup-dir))
   
                  ;; create the directory recursively in svn repo by if it is not already in
                  (setq status (shell-command
                                (concat svn-cmd " ls " backup-dir ">c:/Users/tisogai/tmp-log; "
                                        "if [ $? -ne 0 ] ; then " svn-cmd " add --parents " backup-dir "; fi")))
                 
                ;; update backup directory to avoid weird svn error
                (shell-command-to-string (concat  svn-cmd " update " trunk))

                 
                  ;; get new or modified item
                  ;; stderr redir for directory start with . (why different??)
                  (setq status 
                        (shell-command-to-string 
                         (concat ;; "cd " trunk "; "
                                 svn-cmd " status " trunk " 2>c:/Users/tisogai/tmp-log | head -n 1 ")))
                                 ;svn-cmd " status | head -n 1 "))) ; somehow this fails
                  
                  ;; isolate item
                  (setq new 
                        (replace-regexp-in-string 
                         " " "\\\\ "
                         (replace-regexp-in-string  
                          "........\\([-0-9A-Za-z_./].*\\)\n" "\\1" status)
                         ))
                  
                  (setq new (replace-regexp-in-string "\\\\" "/" new))
                        
                  ;; add, commit, and update
                  (if(not (string-equal  new ""))
                      (progn 
                        (setq svn-cmd-str 
                              (concat
                               svn-cmd " cleanup " trunk))
                        (setq svn-status 
                              (shell-command-to-string svn-cmd-str))
                

                        (setq svn-cmd-str 
                              (concat
                               svn-cmd " add -q " 
                               new ))
                        (setq svn-status (concat svn-status
                              (shell-command-to-string svn-cmd-str)))
                               
                        (setq svn-cmd-str 
                              (concat
                               svn-cmd " commit " trunk " -m '"
                               commit-comment "'"
                               ))
                        (setq svn-status (concat svn-status
                              (shell-command-to-string svn-cmd-str)))
                               
                        (setq svn-cmd-str 
                              (concat
                               svn-cmd " update " trunk
                               ))
                        (setq svn-status (concat svn-status
                              (shell-command-to-string svn-cmd-str)))
                       
                        (message svn-status) ) )
                  ) 
              (error
               (progn
                 ;(write-file buffer)
                 (message (concat svn-cmd " backup failed: " err "++" status "---" svn-cmd-str "===\n" svn-status)) 
                 )
               )
              )
            )
          )
        )
    )
  )



(defun toggle-backup-inhibited ()
   "toggle backup-inhibited variable"
  (interactive)
  (setq backup-inhibited (not  backup-inhibited ))
  (message (concat "backup-inhibited is " (if backup-inhibited "t" "nil")))
)

(defun toggle-untabify-inhibited ()
   "toggle untabify-inhibited variable"
  (interactive)
  (setq untabify-inhibited (not  untabify-inhibited ))
  (message (concat "untabify-inhibited is " (if untabify-inhibited "t" "nil")))
)

(defun update-svn-rev ()
  "get svn revion of backup directory and replace
#::svn-backup-revision:2839:2022-11-07::# with revision number"
  (interactive)
  (if backup-revision-write
      (save-excursion
        (let ((rev)
              (cmdstr))
          (setq cmdstr
                (concat "sh -c \"LANG=; "
                        svn-cmd " info " user-home backup-trunk
                        "| perl -ne 'print $1 if /Revision: ([0-9]+)/;' \""))
          (setq rev 
                (+ 1
                (string-to-number
                 ;(eshell-command-to-value 
                 (shell-command-to-string cmdstr ))))
          (beginning-of-buffer)
          (replace-regexp "#::svn-backup-revision:?[0-9]*:?[-0-9]*::#" 
                          (concat "#::svn-backup-revision:" 
                                  (number-to-string rev)
                                  ":" 
                                  (format-time-string "%Y-%m-%d" )
                                  "::#"))
          )
        ))) 

(defun update-save-date ()
  "get current date-time and replace
  '#:: save-date::#' (no space between : and save-date)
  with date code, such as (no space after :)
  '#:: save-date 2022-11-06 13:30 ::#' "

  (interactive)
  (save-excursion
    (let (
          (datecode (shell-command-to-string  "date +'%Y-%m-%d %H:%M:%S'"))
          )
      (setq datecode (replace-regexp-in-string "[\n\s\t]*$" "" datecode))
      
      (beginning-of-buffer)
      (replace-regexp (concat "#::" "save-date.*::#" )
                      (concat "#::" "save-date " datecode "::#")
      ))))

  
(defun get-backup-name (buf s-home  s-trunk s-file-name s-backup s-backup-dir s-win-file-name)
  "get backup file name 
     arg1    - buffer name. nil for current buffer
   returns
     arg3    - trunk of repository
     arg4    - full path of buf
     arg5    - backup file full path 
     arg6    - backup directory with _ instead of space"
  (interactive)
  
  (set s-home homedir) 
  (set s-trunk (replace-regexp-in-string "c:/Users" "c:/users" (concat home backup-trunk)) )
  (let ((buffer1)  (backup1))
    
    (setq buffer1 (expand-file-name buffer-file-truename))
    (setq buffer1 (replace-regexp-in-string " "   "\\\\ " buffer1 ) )
    (setq buffer1 (replace-regexp-in-string "\\(/mnt\\)?/auto(ldap)?" "" buffer1 ) )
    (set s-win-file-name buffer1 ) 
    (setq buffer1 (replace-regexp-in-string "\\(.\\):/"   "/\\1/" buffer1 ) )  ;; other drives in Windows
    (set s-file-name buffer1 ) 
    (setq backup1 (concat trunk file-name))
    (set s-backup backup1)
    (set s-backup-dir (file-name-directory  backup1 ))
    )
  )



(defun svn-view-rev ()
  "view a file of a revision"
  (interactive)
  (save-excursion
    (let ( (trunk) (buffer) (backup) (log))
      (setq log
            (shell-command-to-string 
             (concat "LANG=; "
                     svn-cmd " log " backup)))
      (message "%s" log)
      
      )))

 
(defun save-buffer-after-untabify ()
  "Save current buffer after untabify. 
Makefile will be saved as it is.
The buffer will save even if it is not modified since the last save."
  (interactive)
  (if (and (not untabify-inhibited) (not (string-match "^\\(Makefile\\|deps\\|configure\\)" (buffer-name))) )
      (save-excursion
        ;(mark-whole-buffer)
        (untabify (point-min) (point-max))
        ;(zmacs-deactivate-region)
        (save-buffer)

        )
    (save-buffer)
    (message "This file is excluded from save-untabified-buffer function. Saved as it is.")  
    )
)
 
  


(defun save-all-buffers-after-untabify ()
  "save all buffers that are files (not *scratch, *Warnings etc.)"
  (interactive)
  (let ((list (buffer-list))
        (buf)
        (name)
        )
    (while (car list)
      (setq buf (car list))
      (setq list (cdr list))
      (setq name (expand-file-name buffer-file-truename))
      (when name
        (switch-to-buffer buf)
        (save-buffer-after-untabify) )
        
      )

    ))

;; if the first arg is -save-and-quit 
;; then call save-all-buffers-after-untabify and quit
(add-hook 'pre-idle-hook 'check-args-hook)

(defun check-args-hook ()
  "see if the first arg is -save-and-quit 
then call save-all-buffers-after-untabify and quit"

  (if (equal (car (cdr command-line-args)) "-untabify-and-quit")
      (progn
        (save-all-buffers-after-untabify) 
        (save-buffers-kill-emacs) 
        ) 
    ;else
    (remove-hook 'pre-idle-hook 'check-args-hook)
    )  )


;; testing 
(defun test1 ()
  (setq backup-dir "adfsdf/asdlfja")
  (shell-command-to-string 
   (concat "mkdir -p " backup-dir))

  (shell-command-to-string 
   "cd /local/tisogai/home/backup/trunk; svn status  | head -n 1")
)

