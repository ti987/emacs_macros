;;; vhdl-gpt.el --- VHDL Helper for Jumping to Declarations -*- lexical-binding: t -*-

;; Version: 0.3
;; Features:
;; - Jump to signal/type declaration from usage
;; - Jump to record type definition
;; - Search local file first, then packages
;; - Skips built-in/simulator packages
;; - Comments closing parens for nested expressions in port tables
;; - Find overloaded function/procedure declaration by argument count and type

(require 'cl-lib)
(require 'seq)
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

;;; Overloaded subprogram declaration finding

(defun vhdl-find-decl--find-close-paren-from (pos)
  "Scan from POS (just inside an open paren, depth=1) and return position of matching ')'.
Returns nil if no matching paren is found before end of buffer."
  (save-excursion
    (goto-char pos)
    (let ((depth 1))
      (while (and (< (point) (point-max)) (> depth 0))
        (let ((ch (char-after)))
          (cond
           ((= ch ?\() (setq depth (1+ depth)) (forward-char))
           ((= ch ?\))
            (setq depth (1- depth))
            (unless (= depth 0) (forward-char)))
           (t (forward-char)))))
      (when (= depth 0) (point)))))

(defun vhdl-find-decl--split-args (str)
  "Split comma-separated argument string STR, respecting nested parens.
Returns list of trimmed non-empty argument strings."
  (let ((result '())
        (buf "")
        (depth 0))
    (dotimes (i (length str))
      (let ((ch (aref str i)))
        (cond
         ((= ch ?\() (setq depth (1+ depth)) (setq buf (concat buf (string ch))))
         ((= ch ?\)) (setq depth (1- depth)) (setq buf (concat buf (string ch))))
         ((and (= ch ?,) (= depth 0))
          (let ((s (string-trim buf)))
            (when (> (length s) 0) (push s result)))
          (setq buf ""))
         (t (setq buf (concat buf (string ch)))))))
    (let ((s (string-trim buf)))
      (when (> (length s) 0) (push s result)))
    (nreverse result)))

(defun vhdl-find-decl--call-enclosing ()
  "Return (NAME . ARG-LIST) for the function/procedure call enclosing point.
Scans backward to find an opening paren with an identifier before it.
Returns nil if point is not inside a call."
  (save-excursion
    (let ((depth 0)
          open-pos)
      (while (and (> (point) (point-min)) (null open-pos))
        (backward-char)
        (let ((ch (char-after)))
          (cond
           ((= ch ?\)) (setq depth (1+ depth)))
           ((= ch ?\()
            (if (> depth 0)
                (setq depth (1- depth))
              (setq open-pos (point)))))))
      (when open-pos
        (goto-char open-pos)
        (skip-chars-backward " \t\n")
        (let ((end (point)))
          (skip-chars-backward "a-zA-Z0-9_.")
          (let ((start (point)))
            (when (< start end)
              (let* ((name (buffer-substring-no-properties start end))
                     (args-start (1+ open-pos))
                     (close-pos (vhdl-find-decl--find-close-paren-from args-start))
                     (args-str (when close-pos
                                 (buffer-substring-no-properties args-start close-pos)))
                     (args (if args-str (vhdl-find-decl--split-args args-str) '())))
                (cons name args)))))))))

(defun vhdl-find-decl--call-at-point ()
  "Return (NAME . ARG-LIST) for a function/procedure call starting at point.
Looks for an identifier at point followed by '('.
Returns nil if not found."
  (save-excursion
    (skip-chars-backward "a-zA-Z0-9_.")
    (when (looking-at "\\([a-zA-Z][a-zA-Z0-9_.]*\\)[ \t\n]*(")
      (let* ((name (match-string-no-properties 1))
             (args-start (match-end 0))
             (close-pos (vhdl-find-decl--find-close-paren-from args-start))
             (args-str (when close-pos
                         (buffer-substring-no-properties args-start close-pos)))
             (args (if args-str (vhdl-find-decl--split-args args-str) '())))
        (cons name args)))))

(defun vhdl-find-decl--base-type (type-str)
  "Extract the base type name from TYPE-STR, dropping range qualifiers."
  (let ((s (string-trim (or type-str ""))))
    (cond
     ((string-match "^\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*(" s) (match-string 1 s))
     ((string-match "^\\([a-zA-Z][a-zA-Z0-9_]*\\)" s) (match-string 1 s))
     (t s))))

(defun vhdl-find-decl--types-compat-p (actual formal)
  "Return t if ACTUAL type is compatible with FORMAL type (case-insensitive).
Handles common VHDL subtype families."
  (when (and actual formal)
    (let ((a (downcase (vhdl-find-decl--base-type actual)))
          (f (downcase (vhdl-find-decl--base-type formal))))
      (or (string= a f)
          (and (member a '("std_logic" "std_ulogic"))
               (member f '("std_logic" "std_ulogic")))
          (and (member a '("integer" "natural" "positive"))
               (member f '("integer" "natural" "positive")))
          (and (member a '("std_logic_vector" "std_ulogic_vector"))
               (member f '("std_logic_vector" "std_ulogic_vector")))))))

(defun vhdl-find-decl--arg-type (arg)
  "Determine the VHDL type of actual argument expression ARG.
Returns a type string or nil if the type cannot be determined."
  ;; Strip named association syntax: 'formal => actual'
  (let ((expr (if (string-match "\\s-*=>\\s-*\\(.+\\)$" arg)
                  (string-trim (match-string 1 arg))
                (string-trim arg))))
    (cond
     ;; Simple identifier: look up its declaration in current buffer
     ((string-match "^[a-zA-Z][a-zA-Z0-9_.]*$" expr)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t)
              found)
          (while (and (not found)
                      (re-search-forward
                       (concat "\\b" (regexp-quote expr) "\\b"
                               "\\s-*:\\s-*"
                               "\\(?:\\(?:in\\|out\\|inout\\|buffer\\)\\s-+\\)?"
                               "\\([a-zA-Z][a-zA-Z0-9_]*\\(?:\\s-*(\\(?:[^)]*\\))\\)?\\)")
                       nil t))
            (setq found (string-trim (match-string-no-properties 1))))
          found)))
     ;; Integer literal
     ((string-match "^[0-9]+$" expr) "integer")
     ;; Bit-string literal: "0101..."
     ((string-match "^\"[01 ]*\"$" expr) "std_logic_vector")
     ;; Char literal '0' or '1'
     ((string-match "^'[01]'$" expr) "std_logic")
     ;; Boolean literal
     ((string-match "^\\(true\\|false\\)$" expr) "boolean")
     ;; Complex expression: type unknown
     (t nil))))

(defun vhdl-find-decl--parse-params (params-str)
  "Parse a subprogram formal parameter list PARAMS-STR.
Returns a list of (NAME MODE BASE-TYPE HAS-DEFAULT) for each parameter,
expanding multi-name groups like 'a, b : integer' into separate entries.
HAS-DEFAULT is t when the parameter carries a ':=' default value."
  (let ((result '()))
    (dolist (group (split-string params-str ";" t))
      (setq group (string-trim group))
      ;; Each group: 'name1, name2 : [mode] type [:= default]'
      (when (string-match "\\([^:]+\\):\\([^\n]*\\)" group)
        (let* ((names-part (string-trim (match-string 1 group)))
               (type-part  (string-trim (match-string 2 group)))
               (mode "in")
               has-default type-only)
          ;; Extract optional mode keyword
          (when (string-match
                 "^\\(in\\|out\\|inout\\|buffer\\)\\s-+\\(.*\\)" type-part)
            (setq mode     (downcase (match-string 1 type-part)))
            (setq type-part (string-trim (match-string 2 type-part))))
          ;; Detect and strip ':= default' — [^:]* stops at the first ':'
          (if (string-match "^\\([^:]*\\):=.*" type-part)
              (setq has-default t
                    type-only   (string-trim (match-string 1 type-part)))
            (setq has-default nil
                  type-only   type-part))
          (let* ((base  (vhdl-find-decl--base-type type-only))
                 (names (mapcar #'string-trim
                                (split-string names-part "," t))))
            (dolist (n names)
              (push (list (downcase n) mode base has-default) result))))))
    (nreverse result)))

(defun vhdl-find-decl--collect-in-buffer (name buf)
  "Return all function/procedure declarations of NAME found in buffer BUF.
Each entry is a list (POINT PARAMS KIND FILE) where:
  POINT  - buffer position of the declaration keyword
  PARAMS - list of (NAME MODE BASE-TYPE HAS-DEFAULT) per formal parameter
  KIND   - symbol 'function or 'procedure
  FILE   - absolute path of the file, or nil for unsaved buffers."
  (let ((result '())
        (file (buffer-file-name buf))
        (case-fold-search t))
    (with-current-buffer buf
      (save-excursion
        (dolist (kind-str '("function" "procedure"))
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "\\<" kind-str "\\>"
                          "[ \t\n]+" (regexp-quote name) "\\b")
                  nil t)
            (let ((decl-pt (match-beginning 0)))
              (skip-chars-forward " \t\n")
              (when (= (char-after) ?\()
                (let* ((args-start (1+ (point)))
                       (close-pt   (vhdl-find-decl--find-close-paren-from args-start))
                       (params-str (when close-pt
                                     (buffer-substring-no-properties
                                      args-start close-pt)))
                       (params     (vhdl-find-decl--parse-params
                                    (or params-str ""))))
                  (push (list decl-pt params (intern kind-str) file) result))))))))
    (nreverse result)))

(defun vhdl-find-decl--use-packages ()
  "Return package names declared via 'use work.PKG.all' in the current buffer."
  (let ((result '())
        (case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (concat "\\<use\\>[ \t]+"
                      "\\<work\\>[ \t]*\\.[ \t]*"
                      "\\([a-zA-Z][a-zA-Z0-9_]*\\)"
                      "[ \t]*\\.[ \t]*\\<all\\>")
              nil t)
        (push (match-string-no-properties 1) result)))
    (nreverse result)))

(defun vhdl-find-decl--count-match-p (params nargs)
  "Return t if PARAMS can accept NARGS arguments.
Parameters with ':=' defaults may be omitted, so the required count is
the number of parameters that have no default."
  (let ((required (seq-count (lambda (p) (not (nth 3 p))) params))
        (total    (length params)))
    (and (>= nargs required) (<= nargs total))))

(defun vhdl-find-decl--positional-type-match-p (params arg-types)
  "Return t if ARG-TYPES are positionally compatible with PARAMS.
params element layout: (NAME MODE BASE-TYPE HAS-DEFAULT).
Unknown actual types (nil) are treated as compatible with any formal type."
  (cl-every
   (lambda (i)
     (let ((atype (nth i arg-types))
           (ftype (nth 2 (nth i params))))
       (or (null atype)
           (vhdl-find-decl--types-compat-p atype ftype))))
   (number-sequence 0 (1- (length arg-types)))))

(defun vhdl-find-decl--named-type-match-p (params args arg-types)
  "Return t if named-association ARGS are type-compatible with PARAMS.
Each arg of the form 'formal => actual' is looked up by formal name in
PARAMS and its actual type (from ARG-TYPES) is compared to the formal's
type.  Unresolved formals and unknown actual types are treated as compatible.
Args that lack '=>' (positional in a mixed call) are skipped."
  (cl-every
   (lambda (i)
     (let ((arg   (nth i args))
           (atype (nth i arg-types)))
       (if (string-match
            "^\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)\\s-*=>" arg)
           (let* ((fname (downcase (match-string 1 arg)))
                  (param (cl-find fname params
                                  :key  (lambda (p) (car p))
                                  :test #'string=)))
             (or (null param)
                 (null atype)
                 (vhdl-find-decl--types-compat-p atype (nth 2 param))))
         t)))                          ; positional in mixed call: skip
   (number-sequence 0 (1- (length args)))))

(defun vhdl-find-declaration-overloaded ()
  "Find the function/procedure declaration that matches the call at point.
Handles overloading by matching argument count and, where determinable,
argument types.  Searches the current buffer first, then imported packages.

Supports:
  - Positional association: foo(a, b)
  - Named association (any order): foo(y => b, x => a)
  - Mixed: foo(a, y => b)
  - Omitted arguments with default values: foo(a) matching foo(a; b := 0)

When exactly one match is found it jumps there in the other window.
When multiple matches remain (e.g. unknown argument types) it offers
a completing-read selection."
  (interactive)
  (let* ((call (or (vhdl-find-decl--call-enclosing)
                   (vhdl-find-decl--call-at-point))))
    (unless call
      (user-error "No function or procedure call found at point"))
    (let* ((name      (car call))
           (args      (cdr call))
           (nargs     (length args))
           (arg-types (mapcar #'vhdl-find-decl--arg-type args))
           (all-decls (vhdl-find-decl--collect-in-buffer name (current-buffer))))
      ;; Add declarations from imported package files
      (dolist (pkg (vhdl-find-decl--use-packages))
        (let ((pkg-file (vhdl-gpt--package-file pkg)))
          (when pkg-file
            (setq all-decls
                  (append all-decls
                          (vhdl-find-decl--collect-in-buffer
                           name (find-file-noselect pkg-file)))))))
      (when (null all-decls)
        (user-error "No declaration found for '%s'" name))
      ;; Determine association style: named when any arg contains '=>'
      (let* ((named-p  (cl-some (lambda (a) (string-match "=>" a)) args))
             ;; Filter 1: count — required params <= nargs <= total params
             (by-count (seq-filter
                        (lambda (d)
                          (vhdl-find-decl--count-match-p (nth 1 d) nargs))
                        all-decls))
             ;; Filter 2: type compatibility — dispatch on association style.
             ;;           Fall back to count-only when type filter yields nothing.
             (candidates
              (or (seq-filter
                   (lambda (d)
                     (let ((params (nth 1 d)))
                       (if named-p
                           (vhdl-find-decl--named-type-match-p
                            params args arg-types)
                         (vhdl-find-decl--positional-type-match-p
                          params arg-types))))
                   by-count)
                  by-count)))
        (if (null candidates)
            (user-error
             "No declaration for '%s' with %d arg(s) found (%d total declaration(s))"
             name nargs (length all-decls))
          (let* ((decl
                  (if (= (length candidates) 1)
                      (car candidates)
                    ;; Multiple candidates: let user choose
                    (let* ((choices
                            (mapcar
                             (lambda (d)
                               (let* ((params (nth 1 d))
                                      (kind   (nth 2 d))
                                      (file   (nth 3 d))
                                      ;; params: (NAME MODE BASE-TYPE HAS-DEFAULT)
                                      (sig    (mapconcat
                                               (lambda (p)
                                                 (format "%s %s %s%s"
                                                         (nth 1 p) (car p) (nth 2 p)
                                                         (if (nth 3 p) ":=…" "")))
                                               params "; ")))
                                 (format "%s %s(%s)  [%s]"
                                         kind name sig
                                         (if file
                                             (file-name-nondirectory file)
                                           "current buffer"))))
                             candidates))
                           (chosen (completing-read
                                    (format "Multiple '%s' declarations: " name)
                                    choices nil t)))
                      (nth (cl-position chosen choices :test #'string=)
                           candidates))))
                 (pt   (nth 0 decl))
                 (file (nth 3 decl)))
            (if (and file (not (string= file (or (buffer-file-name) ""))))
                (find-file-other-window file)
              (switch-to-buffer-other-window (current-buffer)))
            (goto-char pt)
            (message "Found '%s' declaration with %d arg(s)" name nargs)))))))

(provide 'vhdl-gpt)
;;; vhdl-gpt.el ends here
