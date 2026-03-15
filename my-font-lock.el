;;; vec4-face.el --- Custom face to make hexadigit/bit vector more readable

;; 1. Define the custom face
(defface vec4-face
  '((t (:foreground "yellowgreen"
        :weight bold
        :underline nil)))
  "Face for every other 4 hexadigits"
  :group 'font-lock-faces)

;; 2. Define the font-lock keyword rule
;;    4 and 4 hexadigits before d-qoute 
(defconst vec4-font-lock-keywords
  '(
    ("\\([0-9a-fA-F]\\{1,4\\}\\)\\([0-9a-fA-F]\\{28\\}\\)\"" (1 'vec4-face t))
    ("\\([0-9a-fA-F]\\{1,4\\}\\)\\([0-9a-fA-F]\\{20\\}\\)\"" (1 'vec4-face t))
    ("\\([0-9a-fA-F]\\{1,4\\}\\)\\([0-9a-fA-F]\\{12\\}\\)\"" (1 'vec4-face t))
    ("\\([0-9a-fA-F]\\{1,4\\}\\)\\([0-9a-fA-F]\\{4\\}\\)\"" (1 'vec4-face t))
    ("\"\\([0-9a-fA-F]*[^0-9a-fA-F\"]+[0-9a-fA-F]*\\)\"" (1 'font-lock-string-face t))
    )
  "Font-lock keywords to highlight every other 4 hexadigits")

;; 3. Function to enable the custom highlighting in the current buffer
(defun vec4-highlight-enable ()
  "Enable custom font-lock highlighting for identifiers starting with M_AXI."
  (interactive)
  (font-lock-add-keywords nil vec4-font-lock-keywords)
  (font-lock-flush)
  (font-lock-ensure))

;; 4. Function to disable the custom highlighting in the current buffer
(defun vec4-highlight-disable ()
  "Disable custom font-lock highlighting for identifiers starting with M_AXI."
  (interactive)
  (font-lock-remove-keywords nil vec4-font-lock-keywords)
  (font-lock-flush)
  (font-lock-ensure))

;; 4. Function to disable the custom highlighting in the current buffer
(defun vec4-highlight-update ()
  "Disable custom font-lock highlighting every other 4 hexadigits"
  (interactive)
  (font-lock-remove-keywords nil vec4-font-lock-keywords)
  (font-lock-flush)
  (font-lock-ensure)
  (font-lock-add-keywords nil vec4-font-lock-keywords)
  (font-lock-flush)
  (font-lock-ensure)
  )

;; 5. (Optional) Hook it into a specific mode automatically, e.g. verilog-mode
;; (add-hook 'verilog-mode-hook #'vec4-highlight-enable)
;; (add-hook 'c-mode-hook       #'vec4-highlight-enable)
;; (add-hook 'prog-mode-hook    #'vec4-highlight-enable)  ; all programming modes
(remove-hook 'vhdl-mode-hook #'vec4-highlight-enable)
(add-hook 'vhdl-mode-hook #'vec4-highlight-enable)
