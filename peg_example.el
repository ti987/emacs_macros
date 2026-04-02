;;; peg_example.el --- peg.el parsing examples for VHDL  -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple examples using peg.el 1.0.2 to parse VHDL syntax.
;; Each example is self-contained with a runnable demo function.
;;
;; Usage:
;;   M-x load-file RET peg_example.el RET
;;   M-x peg-ex1-demo   -- parse a plain integer
;;   M-x peg-ex2-demo   -- parse a VHDL integer constant declaration
;;   M-x peg-ex3-demo   -- scan constants_pkg.vhd for all integer constants
;;   M-x peg-ex4-demo   -- parse VHDL port declarations
;;
;; Results appear in the *Messages* buffer (C-h e to view).
;;
;; peg.el stack note:
;;   (substring RULE) captures matched text and PUSHes it onto a LIFO stack.
;;   peg-run returns the stack as-is, so the LAST capture is FIRST in the list.
;;   The examples below use (action ...) to pop and reorder captures into a
;;   predictable (name value ...) structure before peg-run returns.

(require 'peg)

;; Suppress byte-compiler warning for the dynamically-bound peg--stack
;; that we access inside (action ...) forms.
(defvar peg--stack)


;;;; ---------------------------------------------------------------
;;;; Example 1 — Parse a plain integer at point
;;;; ---------------------------------------------------------------
;;
;; This is the simplest possible peg.el program: one rule that matches
;; one or more digits and captures the text.
;;
;; PEG rule:
;;   integer <- [0-9]+
;;
;; [0-9] is peg.el's vector-literal character-class syntax (like regex [0-9]).
;; (substring RULE) captures what RULE matched and pushes it onto the stack.
;; (+ RULE) means "one or more".

(defun peg-ex1-parse-integer-at-point ()
  "Parse an integer (one or more digits) at point.
Returns the matched string, or nil if no integer starts at point."
  (with-peg-rules ((integer (substring (+ [0-9]))))
    (car (peg-run (peg integer)))))

(defun peg-ex1-demo ()
  "Demo for Example 1."
  (interactive)
  (dolist (text '("42 is the answer" "1024" "abc"))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((val (peg-ex1-parse-integer-at-point)))
        (if val
            (message "Ex1: %S  →  integer = %S" text val)
          (message "Ex1: %S  →  no integer at point" text))))))


;;;; ---------------------------------------------------------------
;;;; Example 2 — Parse a VHDL integer constant declaration
;;;; ---------------------------------------------------------------
;;
;; Target syntax:
;;   constant NAME : integer := VALUE ;
;;
;; PEG grammar:
;;   const-decl <- "constant" ws+ name ws* ":" ws* "integer" ws* ":=" ws* value ws* ";"
;;   name       <- id-char+
;;   value      <- [0-9]+
;;   id-char    <- [a-zA-Z0-9_]
;;   ws         <- " " / "\t"
;;
;; The (action ...) form pops the two captured substrings off the stack and
;; pushes a single (NAME VALUE) list so peg-run returns ((NAME VALUE)).

(defun peg-ex2-parse-vhdl-int-constant ()
  "Parse a VHDL integer constant declaration at point.
Returns (NAME VALUE-STRING) on success, nil on failure."
  (with-peg-rules
      ((const-decl
        "constant" (+ ws)
        (substring (+ id-char))               ; push: NAME
        (* ws) ":" (* ws) "integer" (* ws) ":=" (* ws)
        (substring (+ [0-9]))                 ; push: VALUE
        (* ws) ";"
        (action                               ; pop both, push combined list
         (let* ((val  (pop peg--stack))
                (name (pop peg--stack)))
           (push (list name val) peg--stack))))
       (ws      (set " \t"))
       (id-char [a-z A-Z 0-9 ?_]))
    (car (peg-run (peg const-decl)))))

(defun peg-ex2-demo ()
  "Demo for Example 2."
  (interactive)
  (let ((lines '("constant DATA_WIDTH  : integer := 32;"
                 "constant MAX_COUNT   : integer := 1024;"
                 ;; This one should NOT match (not an integer type):
                 "constant RESET_VALUE : std_logic_vector(7 downto 0) := x\"00\";")))
    (dolist (line lines)
      (with-temp-buffer
        (insert line)
        (goto-char (point-min))
        (let ((result (peg-ex2-parse-vhdl-int-constant)))
          (if result
              (message "Ex2: %-20s = %s" (car result) (cadr result))
            (message "Ex2: (skipped, not integer constant)  %s" line)))))))


;;;; ---------------------------------------------------------------
;;;; Example 3 — Collect ALL integer constants from a VHDL file
;;;; ---------------------------------------------------------------
;;
;; Strategy: use re-search-forward to jump to each "constant" keyword,
;; then apply the PEG parser from Example 2.  This combines the speed
;; of a regex pre-filter with the precision of a PEG parser.
;;
;; The demo opens:  test_vhdl/pkg/constants_pkg.vhd
;; Expected output:
;;   DATA_WIDTH = 32
;;   ADDR_WIDTH = 16
;;   MAX_COUNT  = 1024

(defun peg-ex3-collect-int-constants ()
  "Collect all integer constant declarations in the current buffer.
Returns an alist of (NAME . INTEGER-VALUE) pairs."
  (save-excursion
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward "\\bconstant\\b" nil t)
        ;; Jump back to the start of the "constant" keyword and try the PEG rule.
        (goto-char (match-beginning 0))
        (let ((found (peg-ex2-parse-vhdl-int-constant)))
          (when found
            (push (cons (car found) (string-to-number (cadr found)))
                  results)))
        ;; Advance past "constant" so the next re-search-forward moves forward.
        (forward-word 1))
      (nreverse results))))

(defun peg-ex3-demo ()
  "Demo for Example 3: scan constants_pkg.vhd for integer constants."
  (interactive)
  (let* ((this-dir (file-name-directory
                    (or load-file-name buffer-file-name default-directory)))
         (vhd-file (expand-file-name "test_vhdl/pkg/constants_pkg.vhd" this-dir)))
    (if (not (file-exists-p vhd-file))
        (message "Ex3: cannot find %s" vhd-file)
      (with-temp-buffer
        (insert-file-contents vhd-file)
        (let ((consts (peg-ex3-collect-int-constants)))
          (if consts
              (message "Ex3: found %d integer constant(s) in %s:\n%s"
                       (length consts)
                       (file-name-nondirectory vhd-file)
                       (mapconcat (lambda (c)
                                    (format "  %-20s = %d" (car c) (cdr c)))
                                  consts "\n"))
            (message "Ex3: no integer constants found in %s" vhd-file)))))))


;;;; ---------------------------------------------------------------
;;;; Example 4 — Parse a VHDL port declaration
;;;; ---------------------------------------------------------------
;;
;; Target syntax (one port line, no trailing semicolon required):
;;   NAME : DIRECTION TYPE
;;
;; PEG grammar:
;;   port-decl  <- name ws* ":" ws* dir ws+ type-str
;;   name       <- id-char+
;;   dir        <- "inout" / "out" / "in"   (longest match first!)
;;   type-str   <- (!(";"|"\n") any)*        (everything to eol or semicolon)
;;   id-char    <- [a-zA-Z0-9_]
;;   ws         <- " " / "\t"
;;
;; Note: (not (set ";\n")) is a zero-width negative lookahead;
;;       (any) then consumes the safe character.

(defun peg-ex4-parse-port-decl ()
  "Parse a VHDL port declaration at point.
Returns (NAME DIRECTION TYPE) as strings, or nil on failure."
  (with-peg-rules
      ((port-decl
        (substring (+ id-char))               ; push: NAME
        (* ws) ":" (* ws)
        (substring (or "inout" "out" "in"))   ; push: DIRECTION (longest first)
        (+ ws)
        (substring (* (not (set ";\n")) (any))) ; push: TYPE (rest of line)
        (action
         (let* ((typ  (string-trim (pop peg--stack)))
                (dir  (pop peg--stack))
                (name (pop peg--stack)))
           (push (list name dir typ) peg--stack))))
       (id-char [a-z A-Z 0-9 ?_])
       (ws      (set " \t")))
    (car (peg-run (peg port-decl)))))

(defun peg-ex4-demo ()
  "Demo for Example 4: parse VHDL port declaration lines."
  (interactive)
  ;; These lines come from test_vhdl/rtl/test_design.vhd
  (let ((ports '("clk : in std_logic"
                 "data_in : in std_logic_vector(DATA_WIDTH-1 downto 0)"
                 "data_out : out std_logic_vector(DATA_WIDTH-1 downto 0)"
                 "addr : inout std_logic_vector(7 downto 0)")))
    (dolist (p ports)
      (with-temp-buffer
        (insert p)
        (goto-char (point-min))
        (let ((r (peg-ex4-parse-port-decl)))
          (if r
              (message "Ex4: name=%-12s  dir=%-6s  type=%s"
                       (nth 0 r) (nth 1 r) (nth 2 r))
            (message "Ex4: parse failed for: %s" p)))))))


(provide 'peg_example)
;;; peg_example.el ends here
