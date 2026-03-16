;;
;; shell_command_helper.el
;; Helper command to run a shell command and insert commented output at point
;; T. Isogai 2026
;;

;;
;; Interactive commands:
;;
;; (defun shell-command-insert-commented-output ()
;;

;;
;; shell-command-insert-commented-output
;;
;; Prompt for a shell command, run it, and insert the captured stdout at the
;; current cursor position.  Each output line is prefixed with the comment
;; syntax of the current buffer's major mode (e.g. "-- " for vhdl-mode,
;; ";; " for emacs-lisp-mode, "// " for c-mode, "# " for python-mode).
;; A header comment line showing the command that was run is inserted first.
;; Falls back to "# " when comment-start is nil (e.g. fundamental-mode).
;;

(defun shell-command-insert-commented-output (command)
  "Run COMMAND in a shell and insert its output, commented, at point.
Each line of the output is prefixed with the current buffer's comment
syntax (from `comment-start`).  A header line recording the command is
inserted before the output lines.  Falls back to \"# \" when
`comment-start` is nil."
  (interactive (list (read-shell-command "Shell command: ")))
  (when (string-empty-p command)
    (error "No shell command provided"))
  ;; Determine the comment prefix for the current major mode
  (let* ((cs (or comment-start "# "))
         ;; Ensure a trailing space after the comment marker
         (prefix (if (string-suffix-p " " cs)
                     cs
                   (concat cs " ")))
         (output (shell-command-to-string command))
         (lines (split-string output "\n")))
    ;; Remove a single trailing empty string caused by a final newline
    (when (and lines (string-equal (car (last lines)) ""))
      (setq lines (butlast lines)))
    ;; Insert header comment followed by each commented output line
    (insert (concat prefix "Output of: " command "\n"))
    (dolist (line lines)
      (insert (concat prefix line "\n")))))

(provide 'shell-command-helper)
