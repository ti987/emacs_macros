;;; test-my-font-lock.el --- Batch tests for my-font-lock.el
;;; Usage: emacs --batch -l test-my-font-lock.el
;;;   Exits 0 on success, 1 if any test fails.

(require 'font-lock)

(load-file (expand-file-name "my-font-lock.el"
                             (file-name-directory load-file-name)))

(defvar test-pass-count 0)
(defvar test-fail-count 0)

(defmacro check (desc form)
  `(if ,form
       (progn
         (setq test-pass-count (1+ test-pass-count))
         (message "  PASS: %s" ,desc))
     (progn
       (setq test-fail-count (1+ test-fail-count))
       (message "  FAIL: %s" ,desc))))

;;; -------------------------------------------------------------------------
;;; Helpers
;;; -------------------------------------------------------------------------

(defun face-set-p (text)
  "Return non-nil if any character in TEXT receives `my-hex-vec-face' after
calling `my-highlight-hex-or-bits'.  TEXT is inserted into a fresh temp buffer."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (my-highlight-hex-or-bits (point-max))
    (let ((found nil))
      (let ((pos (point-min)))
        (while (and (not found) (< pos (point-max)))
          (when (eq (get-text-property pos 'font-lock-face) 'my-hex-vec-face)
            (setq found t))
          (setq pos (1+ pos))))
      found)))

(defun face-at-p (text buf-pos)
  "Return non-nil if the character at BUF-POS (1-based Emacs buffer position)
in TEXT receives `my-hex-vec-face' after calling `my-highlight-hex-or-bits'.
Buffer positions: 1 = first char of TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (my-highlight-hex-or-bits (point-max))
    (eq (get-text-property buf-pos 'font-lock-face) 'my-hex-vec-face)))

;;; -------------------------------------------------------------------------
;;; Tests for the user-supplied vectors
;;; -------------------------------------------------------------------------
;;; Buffer-position map for each vector (all positions 1-based, ranges exclusive):
;;;
;;;   x"11101110111011101110111011101110"
;;;     pos 1='x'  2='"'  3..34=32 hex digits  35='"'
;;;     group 0 (rightmost 4, "1110"): [31,35)  HIGHLIGHTED
;;;     group 1                      : [27,31)
;;;     group 2                      : [23,27)  HIGHLIGHTED
;;;     group 3                      : [19,23)
;;;     group 4                      : [15,19)  HIGHLIGHTED
;;;     group 5                      : [11,15)
;;;     group 6                      :  [7,11)  HIGHLIGHTED
;;;     group 7                      :  [3, 7)
;;;
;;;   x"abcd1234"
;;;     pos 1='x'  2='"'  3..10="abcd1234"  11='"'
;;;     group 0 ("1234"):  [7,11)  HIGHLIGHTED
;;;     group 1 ("abcd"):  [3, 7)
;;;
;;;   x"aabb10101a1110000"
;;;     pos 1='x'  2='"'  3..19="aabb10101a1110000"  20='"'
;;;     group 0 ("0000"):  [16,20)  HIGHLIGHTED
;;;     group 1 ("a111"):  [12,16)
;;;     group 2 ("0101"):   [8,12)  HIGHLIGHTED
;;;     group 3 ("abb1"):   [4, 8)
;;;     group 4 ("a"):      [3, 4)  HIGHLIGHTED

(message "\n=== my-font-lock.el unit tests ===\n")
(message "-- User-supplied test vectors --")

;; 1. x"11101110111011101110111011101110" (32 hex digits, 8 groups)
(let ((text "x\"11101110111011101110111011101110\""))
  (check "32-digit hex: some chars highlighted"
         (face-set-p text))
  (check "32-digit hex: exactly 4 highlighted runs (groups 0,2,4,6)"
         (let ((runs 0)
               (in-run nil))
           (with-temp-buffer
             (insert text)
             (goto-char (point-min))
             (my-highlight-hex-or-bits (point-max))
             (dotimes (i (length text))
               (let ((pos (1+ i)))
                 (if (eq (get-text-property pos 'font-lock-face) 'my-hex-vec-face)
                     (unless in-run (setq runs (1+ runs) in-run t))
                   (setq in-run nil)))))
           (= 4 runs)))
  (check "32-digit hex: group 0 (rightmost \"1110\", pos 31) highlighted"
         (face-at-p text 31))
  (check "32-digit hex: group 7 (leftmost \"1110\", pos 3) NOT highlighted"
         (not (face-at-p text 3))))

;; 2. x"abcd1234" (8 hex digits, 2 groups)
(let ((text "x\"abcd1234\""))
  (check "x\"abcd1234\": group 0 (\"1234\", pos 7) highlighted"
         (face-at-p text 7))
  (check "x\"abcd1234\": group 1 (\"abcd\", pos 3) NOT highlighted"
         (not (face-at-p text 3))))

;; 3. "101011110000" — no prefix, must not match
(check "bare string \"101011110000\" (no prefix): nothing highlighted"
       (not (face-set-p "\"101011110000\"")))

;; 4. x"abcd1234" duplicate — same as #2
(check "second x\"abcd1234\": group 0 highlighted"
       (face-at-p "x\"abcd1234\"" 7))

;; 5. foo x"wxyz1234" — 'w','y','z' not valid hex digits
(check "x\"wxyz1234\" (invalid hex content): nothing highlighted"
       (not (face-set-p "foo x\"wxyz1234\"")))

;; 6. x"aabb10101a1110000" (17 hex digits, 5 groups)
(let ((text "x\"aabb10101a1110000\""))
  (check "x\"aabb10101a1110000\": group 0 (\"0000\", pos 16) highlighted"
         (face-at-p text 16))
  (check "x\"aabb10101a1110000\": group 1 (\"a111\", pos 12) NOT highlighted"
         (not (face-at-p text 12)))
  (check "x\"aabb10101a1110000\": group 2 (\"0101\", pos 8) highlighted"
         (face-at-p text 8))
  (check "x\"aabb10101a1110000\": group 3 (\"abb1\", pos 4) NOT highlighted"
         (not (face-at-p text 4)))
  (check "x\"aabb10101a1110000\": group 4 (\"a\", pos 3) highlighted"
         (face-at-p text 3)))

;;; -------------------------------------------------------------------------
(message "\n-- Edge cases --")

;; Short literal (1 chunk = group 0): the original parity bug caused this to
;; go un-highlighted.  With the fix group 0 is always highlighted.
(check "x\"FF\" (2 chars, single chunk): highlighted"
       (face-set-p "x\"FF\""))

(check "x\"DEAD\" (4 chars, single chunk): highlighted"
       (face-set-p "x\"DEAD\""))

(check "b\"1010\" (4 bits, single chunk): highlighted"
       (face-set-p "b\"1010\""))

;; 8-bit binary: 2 groups, group 0 (rightmost "1010", pos 7) highlighted
(check "b\"10101010\": group 0 (pos 7) highlighted"
       (face-at-p "b\"10101010\"" 7))

;; Invalid content for prefix
(check "b\"0123\" (non-binary digits): nothing highlighted"
       (not (face-set-p "b\"0123\"")))

;; Empty content
(check "x\"\" (empty content): nothing highlighted"
       (not (face-set-p "x\"\"")))

;; Octal prefix
(check "o\"7654\" (4 octal digits, single chunk): highlighted"
       (face-set-p "o\"7654\""))

(check "O\"7654\" (uppercase O prefix): highlighted"
       (face-set-p "O\"7654\""))

;;; -------------------------------------------------------------------------
(message "\n=== Results: %d passed, %d failed ===\n"
         test-pass-count test-fail-count)
(when (> test-fail-count 0)
  (kill-emacs 1))
(kill-emacs 0)
