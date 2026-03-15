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
  "Highlight VHDL vector literals [bBoOxX]\"...\" with alternating group faces.
Digits are grouped from the right in chunks of 4.
Odd-indexed groups (1, 3, 5, ...) get `my-hex-vec-face'.
Only highlights well-formed literals (correct digit set for the given prefix)."
  (when (re-search-forward "\\([bBoOxX]\\)\"\\([^\n\"]*\\)\"" limit t)
    ;; Save buffer positions from match-data before any string operations
    ;; that might disturb the match state.
    (let* ((prefix  (downcase (match-string-no-properties 1)))
           (str-beg (match-beginning 2))
           (str-end (match-end 2))
           (content (buffer-substring-no-properties str-beg str-end))
           (valid   (cond
                     ((string= prefix "b") (string-match-p "\\`[01]+\\'"         content))
                     ((string= prefix "o") (string-match-p "\\`[0-7]+\\'"        content))
                     ((string= prefix "x") (string-match-p "\\`[0-9A-Fa-f]+\\'"  content)))))
      (when valid
        (my-highlight-groups-from-right str-beg str-end 'my-hex-vec-face 4)))
    t))

;; Register for all vhdl-mode buffers.
;; Using 'vhdl-mode (not nil) stores the keyword in font-lock-keywords-alist,
;; which font-lock-set-defaults includes for every vhdl-mode buffer regardless
;; of when this file is loaded.  Remove first so reloading the file stays
;; idempotent; font-lock-remove-keywords is a no-op when the keyword is absent.
(font-lock-remove-keywords 'vhdl-mode '((my-highlight-hex-or-bits)))
(font-lock-add-keywords    'vhdl-mode '((my-highlight-hex-or-bits)) 'append)


