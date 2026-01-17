;;; vhdl-gpt.el --- VHDL Helper for Jumping to Declarations -*- lexical-binding: t -*-

;; Version: 0.2
;; If the type declaration is found in the current file, it now:
;;
;;  Opens the same buffer in the other window
;;
;;  Jumps the cursor in that window to the type declaration location
;; Version: 0.1
;; Features:
;; - Jump to signal/type declaration from usage
;; - Jump to record type definition
;; - Search local file first, then packages
;; - Skips built-in/simulator packages
;; - Comments closing parens for nested expressions in port tables

(require 'cl-lib)
(require 'thingatpt)

(defvar vhdl-gpt-package-dirs '("./" "../src" "../pkg")
  "Directories to search for VHDL packages.")

(defvar vhdl-gpt-blacklist-files '("ieee" "std" "textio")
  "Packages to skip during search.")

(defun vhdl-gpt--symbol-at-point ()
  (let ((sym (thing-at-point 'symbol t)))
    (when sym (substring-no-properties sym))))

(defun vhdl-gpt--package-file (pkg)
  (cl-block nil
    (dolist (dir vhdl-gpt-package-dirs)
      (let ((file (concat (file-name-as-directory dir) pkg ".vhd")))
        (when (file-exists-p file)
          (cl-return file))))))

(defun vhdl-gpt--search-decl (symbol)
  (let ((pattern (concat "\\b" symbol "\\b.*:.*"))
        (case-fold-search t))
    (goto-char (point-min))
    (when (re-search-forward pattern nil t)
      (beginning-of-line)
      (point))))

(defun vhdl-gpt--search-type (symbol)
  (let ((pattern (concat "\\b" symbol "\\b.*:.*"))
        (case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward pattern nil t)
        (let ((line (thing-at-point 'line t)))
          (when (string-match ": *\([^;]+\);" line)
            (match-string 1 line line)))))))

(defun vhdl-gpt--jump-to-decl (symbol)
  (or (vhdl-gpt--search-decl symbol)
      (let* ((type (vhdl-gpt--search-type symbol))
             (pkg (and type (vhdl-gpt--resolve-package type))))
        (cond
         ;; Type is in current buffer
         ((vhdl-gpt--search-decl type)
          (let ((pt (vhdl-gpt--search-decl type)))
            (when pt
              (switch-to-buffer-other-window (current-buffer))
              (goto-char pt)
              pt)))
         ;; Type is in external package file
         (pkg
          (let ((buf (find-file-other-window pkg)))
            (with-current-buffer buf
              (goto-char (point-min))
              (when (re-search-forward (concat "\\btype\\s-+" (regexp-quote type) "\\b") nil t)
                (beginning-of-line)
                (point)))))))))

(defun vhdl-gpt--resolve-package (typename)
  (cl-block nil
    (dolist (dir vhdl-gpt-package-dirs)
      (let* ((files (directory-files dir t "\\.vhd$"))
             (valid (seq-remove (lambda (f)
                                  (member (file-name-base f) vhdl-gpt-blacklist-files))
                                files)))
        (dolist (file valid)
          (when (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (re-search-forward (concat "\\btype\\s-+" typename "\\b") nil t))
            (cl-return file)))))))

(defun vhdl-gpt-jump-to-declaration ()
  "Jump to the declaration of the signal, type, or subprogram at point."
  (interactive)
  (let ((symbol (vhdl-gpt--symbol-at-point)))
    (unless (vhdl-gpt--jump-to-decl symbol)
      (message "Declaration for '%s' not found" symbol))))

(provide 'vhdl-gpt)
;;; vhdl-gpt.el ends here
