(defface my-hex-vec-face
  '((t :foreground "cyan3" :weight bold))
  "Face for grouped hex/bit chunks.")



(defun my-highlight-groups-from-right (beg end face chunk-size)
  "Apply FACE to odd-indexed chunks of CHUNK-SIZE chars from BEG to END.
Chunks are indexed from the right starting at 0.
Odd-indexed chunks (1, 3, 5, ...) get FACE; even-indexed chunks are unchanged."
  (let ((pos end)
        (group-idx 0))
    (while (> pos beg)
      (let ((chunk-start (max beg (- pos chunk-size))))
        (when (= 1 (mod group-idx 2))
          (put-text-property chunk-start pos 'font-lock-face face))
        (setq pos chunk-start)
        (setq group-idx (1+ group-idx))))))

(defun my-highlight-hex-or-bits (limit)
  "Highlight parts of x\"...\" strings as hex or bit vectors.

Digits are grouped from the right in chunks of up to 4.
Groups are numbered from the right starting at 0.
Even-index groups (0, 2, 4, ...) are left unchanged.
Odd-index groups (1, 3, 5, ...) get `my-hex-vec-face`."
  (when (re-search-forward "x\"\\([^\n\"]*\\)\"" limit t)
    (let ((str-beg (match-beginning 1))
          (str-end (match-end 1)))
      (let ((s (buffer-substring-no-properties str-beg str-end)))
        (cond
         ;; Hex case: all characters are hex
         ((string-match-p "\\`[0-9A-Fa-f]+\\'" s)
          (my-highlight-groups-from-right str-beg str-end 'my-hex-vec-face 4))
         ;; Bit-vector case: all characters 0 or 1
         ((string-match-p "\\`[01]+\\'" s)
          (my-highlight-groups-from-right str-beg str-end 'my-hex-vec-face 4)))))
    t))

(defun my-hex-or-bits-setup ()
  (font-lock-add-keywords
   nil
   '((my-highlight-hex-or-bits 0 nil prepend))
   'append))

;; Use your real language-mode hook here:
;;(add-hook 'emacs-lisp-mode-hook #'my-hex-or-bits-setup)
(remove-hook 'vhdl-mode-hook #'my-hex-or-bits-setup)
(add-hook 'vhdl-mode-hook #'my-hex-or-bits-setup)


