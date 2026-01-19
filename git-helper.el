;;
;; git-helper.el
;; Helper commands for git operations with ediff
;; T. Isogai 2026
;;

;;
;; Interactive commands:
;;
;; (defun git-ediff-buffer-with-head ()
;; (defun git-ediff-head-with-previous ()
;;

;;
;; git-ediff-buffer-with-head
;;
;; Run ediff to compare the current buffer with the HEAD version of the file in git.
;; This is useful to see what local changes have been made since the last commit.
;;

(defun git-ediff-buffer-with-head ()
  "Compare current buffer with HEAD version using ediff.
Shows differences between the local buffer and the committed version in git."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (git-revision "HEAD")
         (temp-file (make-temp-file "git-head-"))
         (git-cmd (format "git show %s:%s" 
                         git-revision 
                         (file-relative-name file-name 
                                            (vc-git-root file-name)))))
    
    (if (not file-name)
        (error "Current buffer is not visiting a file"))
    
    (if (not (vc-git-root file-name))
        (error "Current file is not in a git repository"))
    
    ;; Get HEAD version and save to temp file
    (with-temp-file temp-file
      (shell-command git-cmd (current-buffer)))
    
    ;; Run ediff with current buffer and HEAD version
    (let* ((temp-buffer (find-file-noselect temp-file))
           (cleanup-fn nil))
      (setq cleanup-fn
            `(lambda ()
               (when (file-exists-p ,temp-file)
                 (delete-file ,temp-file))
               (when (buffer-live-p ,temp-buffer)
                 (with-current-buffer ,temp-buffer
                   (set-buffer-modified-p nil))
                 (kill-buffer ,temp-buffer))
               (remove-hook 'ediff-quit-hook ',cleanup-fn)))
      (add-hook 'ediff-quit-hook cleanup-fn nil t)
      (ediff-buffers (current-buffer) temp-buffer))))


;;
;; git-ediff-head-with-previous
;;
;; Run ediff to compare the HEAD version with the previous version (HEAD~1) of 
;; the current buffer's file. This is useful to see what changes were made in 
;; the last commit.
;;

(defun git-ediff-head-with-previous ()
  "Compare HEAD version with previous version (HEAD~1) using ediff.
Shows differences between the last commit and the commit before it."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (head-revision "HEAD")
         (prev-revision "HEAD~1")
         (temp-file-head (make-temp-file "git-head-"))
         (temp-file-prev (make-temp-file "git-prev-"))
         (file-rel-name (file-relative-name file-name 
                                            (vc-git-root file-name)))
         (git-cmd-head (format "git show %s:%s" head-revision file-rel-name))
         (git-cmd-prev (format "git show %s:%s" prev-revision file-rel-name)))
    
    (if (not file-name)
        (error "Current buffer is not visiting a file"))
    
    (if (not (vc-git-root file-name))
        (error "Current file is not in a git repository"))
    
    ;; Get HEAD version
    (with-temp-file temp-file-head
      (shell-command git-cmd-head (current-buffer)))
    
    ;; Get previous version (HEAD~1)
    (with-temp-file temp-file-prev
      (shell-command git-cmd-prev (current-buffer)))
    
    ;; Run ediff with HEAD and previous version
    (let* ((temp-buffer-prev (find-file-noselect temp-file-prev))
           (temp-buffer-head (find-file-noselect temp-file-head))
           (cleanup-fn nil))
      (setq cleanup-fn
            `(lambda ()
               (when (file-exists-p ,temp-file-head)
                 (delete-file ,temp-file-head))
               (when (file-exists-p ,temp-file-prev)
                 (delete-file ,temp-file-prev))
               (when (buffer-live-p ,temp-buffer-head)
                 (with-current-buffer ,temp-buffer-head
                   (set-buffer-modified-p nil))
                 (kill-buffer ,temp-buffer-head))
               (when (buffer-live-p ,temp-buffer-prev)
                 (with-current-buffer ,temp-buffer-prev
                   (set-buffer-modified-p nil))
                 (kill-buffer ,temp-buffer-prev))
               (remove-hook 'ediff-quit-hook ',cleanup-fn)))
      (add-hook 'ediff-quit-hook cleanup-fn nil t)
      (ediff-buffers temp-buffer-prev temp-buffer-head))))

(provide 'git-helper)
