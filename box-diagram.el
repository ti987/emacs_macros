;;
;; box-diagram.el - Draw Unicode block diagrams from text input
;;
;; Syntax:
;;
;;   -- Definition section (before the first blank line) --
;;
;;   ID := box("label")
;;       Single-border box.
;;
;;   ID := double-box("label", ID1, ID2, ...)
;;       Double-border container.  ID1..IDn are the node IDs rendered INSIDE
;;       the border.  All other nodes (text nodes) appear OUTSIDE.
;;
;;   ID := text("label")
;;       Plain text label; no box drawn.
;;
;;   -- Connection section (after the first blank line) --
;;
;;   Each non-blank line is an independent chain:
;;
;;     ID1 -> ID2 -> ID3
;;
;;   Semicolons separate multiple chains on one line:
;;
;;     ID1 -> ID2 ; ID3 -> ID4
;;
;;   Unicode right-arrow (\u2192) may be used instead of ->.
;;
;;   The very first non-blank line of the connection section may be a bare
;;   identifier naming a double-box container.  The graph-based renderer is
;;   then used: each box appears exactly once, made taller when it has
;;   multiple input or output connections; text nodes appear outside the
;;   module border.
;;
;;     F
;;     I1 -> A -> B -> C -> O1
;;     I2 -> D -> B
;;     C  -> E -> O2
;;
;; Usage:
;;   M-x box-diagram-render        - render the current buffer
;;   M-x box-diagram-render-region - render the selected region

(require 'cl-lib)
(require 'subr-x)

;;; ---- Parsers ---------------------------------------------------------------

(defun box-diagram--parse-defs (text)
  "Return alist (ID . PLIST) from definition lines in TEXT.
Plist keys: :type (box|double-box|text), :label, :children (double-box)."
  (let (defs)
    (dolist (line (split-string text "\n"))
      (cond
       ((string-match
         "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*:=[ \t]*box(\"\\([^\"]+\\)\")" line)
        (push (cons (match-string 1 line)
                    (list :type 'box :label (match-string 2 line))) defs))
       ((string-match
         "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*:=[ \t]*text(\"\\([^\"]+\\)\")" line)
        (push (cons (match-string 1 line)
                    (list :type 'text :label (match-string 2 line))) defs))
       ((string-match
         (concat "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*:=[ \t]*"
                 "double-box(\"\\([^\"]+\\)\"\\(.*\\))")
         line)
        (let* ((id    (match-string 1 line))
               (label (match-string 2 line))
               (rest  (match-string 3 line))
               (kids  (when (string-match ",\\(.*\\)" rest)
                        (delq nil
                              (mapcar (lambda (s)
                                        (let ((s (string-trim s)))
                                          (when (string-match "^[A-Za-z0-9_]+$" s) s)))
                                      (split-string (match-string 1 rest) ","))))))
          (push (cons id (list :type 'double-box :label label :children kids)) defs)))))
    (nreverse defs)))

(defun box-diagram--parse-chains (text)
  "Return list of chains from TEXT.  Each chain is a list of ID strings.
Lines with -> (or Unicode \u2192) form chains.  Semicolons split multiple
chains on one line."
  (let (chains)
    (dolist (line (split-string text "\n"))
      (let ((trimmed (string-trim line)))
        (when (and (> (length trimmed) 0)
                   (string-match-p "->\\|\u2192" trimmed)
                   (not (string-match-p ":=" trimmed)))
          (dolist (seg (split-string trimmed ";"))
            (let* ((seg   (string-trim seg))
                   (nodes (and (string-match-p "->\\|\u2192" seg)
                               (mapcar #'string-trim
                                       (split-string seg
                                                     "[ \t]*\\(->\\|\u2192\\)[ \t]*" t)))))
              (when (and nodes (cdr nodes))
                (push nodes chains)))))))
    (nreverse chains)))

;;; ---- Graph-based layout (used when a double-box container is declared) ----

(defun box-diagram--label (id defs)
  "Return the display label for ID."
  (let ((def (cdr (assoc id defs))))
    (if (and def (plist-get def :label)) (plist-get def :label) id)))

(defun box-diagram--bw (label)
  "Outer character width of a box: | + space + label + space + | = len+4."
  (+ (length label) 4))

(defun box-diagram--assign-rows (chains)
  "Return a hash id->row-number.
Each chain seeds its new nodes on the same row determined by the maximum
row of any already-placed node in the chain plus 1, or the next free row."
  (let ((rows (make-hash-table :test 'equal))
        (next 0))
    (dolist (chain chains)
      (let ((max-placed -1) (has-placed nil))
        (dolist (id chain)
          (when (gethash id rows)
            (setq has-placed t)
            (setq max-placed (max max-placed (gethash id rows)))))
        (let ((base (if has-placed (1+ max-placed)
                      (prog1 next (setq next (1+ next))))))
          (dolist (id chain)
            (unless (gethash id rows) (puthash id base rows))))))
    rows))

(defun box-diagram--assign-cols (chains inner-set)
  "Return a hash id->column for nodes in INNER-SET.
Column = longest path from any source inside node (Bellman-Ford)."
  (let ((cols  (make-hash-table :test 'equal))
        (inner '()))
    (dolist (chain chains)
      (dolist (id chain)
        (when (and (gethash id inner-set) (not (member id inner)))
          (push id inner))))
    (dolist (id inner) (puthash id 0 cols))
    (dotimes (_ (length inner))
      (dolist (chain chains)
        (let ((rest chain))
          (while (cdr rest)
            (let ((f (car rest)) (t_ (cadr rest)))
              (when (and (gethash f inner-set) (gethash t_ inner-set))
                (when (> (1+ (gethash f cols 0)) (gethash t_ cols 0))
                  (puthash t_ (1+ (gethash f cols 0)) cols))))
            (setq rest (cdr rest))))))
    cols))

(defun box-diagram--collect-ports (chains rows inner-set)
  "Return (IN-PORTS . OUT-PORTS) where each is a hash id->list of (peer . port-row).
Port-row for edge (f->t) = max(row[f], row[t])."
  (let ((in-p  (make-hash-table :test 'equal))
        (out-p (make-hash-table :test 'equal)))
    (dolist (chain chains)
      (let ((rest chain))
        (while (cdr rest)
          (let* ((f   (car rest))
                 (t_  (cadr rest))
                 (pr  (max (gethash f rows 0) (gethash t_ rows 0))))
            (let ((cur (gethash f out-p '())))
              (unless (assoc t_ cur)
                (puthash f (append cur (list (cons t_ pr))) out-p)))
            (let ((cur (gethash t_ in-p '())))
              (unless (assoc f cur)
                (puthash t_ (append cur (list (cons f pr))) in-p))))
          (setq rest (cdr rest)))))
    ;; Sort port lists by port-row
    (dolist (id (hash-table-keys in-p))
      (puthash id (sort (copy-sequence (gethash id in-p))
                        (lambda (a b) (< (cdr a) (cdr b)))) in-p))
    (dolist (id (hash-table-keys out-p))
      (puthash id (sort (copy-sequence (gethash id out-p))
                        (lambda (a b) (< (cdr a) (cdr b)))) out-p))
    (cons in-p out-p)))

;;; ---- Canvas ----------------------------------------------------------------

(defun box-diagram--canvas (h w)
  "Vector of H strings, each W spaces."
  (let ((v (make-vector h nil)))
    (dotimes (r h) (aset v r (make-string w ?\s)))
    v))

(defun box-diagram--put (canvas row col str)
  "Paint STR at (ROW COL) on CANVAS, extending the row string if needed."
  (when (and (>= row 0) (< row (length canvas)) str (> (length str) 0))
    (let* ((s   (aref canvas row))
           (end (+ col (length str))))
      (when (> end (length s))
        (aset canvas row (concat s (make-string (- end (length s)) ?\s)))
        (setq s (aref canvas row)))
      (let ((chars (string-to-list s)))
        (let ((i 0))
          (dolist (ch (string-to-list str))
            (setcar (nthcdr (+ col i) chars) ch)
            (setq i (1+ i))))
        (aset canvas row (apply #'string chars))))))

(defun box-diagram--canvas-to-lines (canvas)
  "Canvas -> list of right-trimmed strings."
  (let (r)
    (dotimes (i (length canvas))
      (push (replace-regexp-in-string "[ \t]+$" "" (aref canvas i)) r))
    (nreverse r)))

;;; ---- Graph renderer --------------------------------------------------------

(defun box-diagram--render-graph (chains defs outer-def)
  "Render diagram from CHAINS/DEFS using graph-based layout.
OUTER-DEF is the double-box plist (:label :children).
Returns a list of strings.

Border style  : ===Title=== top, | sides, === bottom.
Arrow crossing: label -------|--> box    (dashes approach |, --> enters)
Exit crossing : box +--------|-->  label  (dashes to |, --> exits)
VL scheme     : 3 visual lines per grid row:
                  ivl-top(r)     = 3r     top border
                  ivl-content(r) = 3r+1   connection/label row
                  ivl-bot(r)     = 3r+2   bottom border"
  (let* ((inner-ids (plist-get outer-def :children))
         (title     (plist-get outer-def :label))
         (inner-set (let ((h (make-hash-table :test 'equal)))
                      (dolist (id inner-ids) (puthash id t h)) h))
         (rows (box-diagram--assign-rows chains))
         (cols (box-diagram--assign-cols chains inner-set))
         (ports (box-diagram--collect-ports chains rows inner-set))
         (in-ports  (car ports))
         (out-ports (cdr ports))
         ;; Inner nodes in first-appearance order
         (inner-order
          (let (lst)
            (dolist (chain chains)
              (dolist (id chain)
                (when (and (gethash id inner-set) (not (member id lst)))
                  (push id lst))))
            (nreverse lst)))
         (num-cols
          (if inner-order
              (1+ (apply #'max (mapcar (lambda (id) (gethash id cols 0)) inner-order)))
            1))
         (col-widths
          (let ((v (make-vector num-cols 4)))
            (dolist (id inner-order)
              (let ((c (gethash id cols 0))
                    (w (box-diagram--bw (box-diagram--label id defs))))
                (when (> w (aref v c)) (aset v c w))))
            v))
         (num-rows
          (let ((seen (make-hash-table :test 'eql)))
            (maphash (lambda (_ v) (puthash v t seen)) rows)
            (max 1 (hash-table-count seen))))
         ;; Outside-left/right maps: port-row -> outside-node-id
         (oleft  (make-hash-table :test 'eql))
         (oright (make-hash-table :test 'eql)))

    ;; Populate oleft / oright
    (dolist (chain chains)
      (let ((rest chain))
        (while (cdr rest)
          (let* ((f  (car rest)) (t_ (cadr rest))
                 (pr (max (gethash f rows 0) (gethash t_ rows 0))))
            (when (and (not (gethash f inner-set)) (gethash t_ inner-set))
              (puthash pr f oleft))
            (when (and (gethash f inner-set) (not (gethash t_ inner-set)))
              (puthash pr t_ oright)))
          (setq rest (cdr rest)))))

    ;; ---- Geometry -----------------------------------------------------------
    ;; Outside-left pattern: "S0_AXI -------|-->" + "| box"
    ;;   label + " -------" (8 chars, no arrowhead before |)
    ;;   | wall stays; --> is drawn inside the module after |

    (let* ((dashes-out (concat " " (make-string 7 ?\u2500)))  ; " " + 7 x BOX LIGHT HORIZ
           (left-lbls
            (let (ls) (maphash (lambda (_ id)
                                 (push (box-diagram--label id defs) ls))
                               oleft) ls))
           (max-llbl (if left-lbls (apply #'max (mapcar #'length left-lbls)) 0))
           (left-w   (+ max-llbl (length dashes-out)))
           ;; entry-w=3: arrow "-->" between | and first column
           ;; gap=3:     arrow "-->" between adjacent columns
           ;; exit-pad=5: spare space after last column before right |
           (gap      3)
           (entry-w  3)
           (exit-pad 5)
           (col-x
            (let ((v (make-vector num-cols 0)) (x entry-w))
              (dotimes (c num-cols)
                (aset v c x)
                (setq x (+ x (aref col-widths c) gap)))
              v))
           (inside-w
            (max (+ entry-w
                    (apply #'+ (mapcar #'identity (cl-coerce col-widths 'list)))
                    (* gap (max 0 (1- num-cols)))
                    exit-pad)
                 (length title)))
           ;; 3 VLs per row; canvas = top+title+sep + n-in-vls + bottom = n-in-vls+4
           (n-in-vls (* 3 num-rows))
           (n-vls    (+ n-in-vls 4))
           (canvas-w (+ left-w 1 inside-w 1 40))
           (cv       (box-diagram--canvas n-vls canvas-w))
           (mod-lx   left-w)
           (mod-rx   (+ mod-lx 1 inside-w))
           ;; Arrow string used between boxes and at entries
           (arrow    (concat (make-string 2 ?\u2500) "\u25ba")))

      (cl-flet
          ((abs-vl    (ivl) (+ 3 ivl))       ; canvas row = 3 + inside-VL (past top/title/sep)
           (ivl-top-r (r)   (* 3 r))          ; inside-VL: top border of row r
           (ivl-cont  (r)   (+ (* 3 r) 1))    ; inside-VL: content of row r
           (ivl-bot-r (r)   (+ (* 3 r) 2))    ; inside-VL: bottom border of row r
           (cx-of     (c)   (+ mod-lx 1 (aref col-x c))))

        ;; Double-line box border: ╔═╗ top, ║ sides, ╠═╣ separator, ╚═╝ bottom
        ;; Arrow crossings keep ║ intact: dashes║──►  (no ╪/╫/╬)
        (box-diagram--put cv 0 mod-lx
          (concat "╔" (make-string inside-w ?═) "╗"))
        (let* ((tpad   (/ (- inside-w (length title)) 2))
               (tpad-r (- inside-w (length title) tpad)))
          (box-diagram--put cv 1 mod-lx
            (concat "║" (make-string tpad ?\s) title (make-string tpad-r ?\s) "║")))
        (box-diagram--put cv 2 mod-lx
          (concat "╠" (make-string inside-w ?═) "╣"))
        (box-diagram--put cv (1- n-vls) mod-lx
          (concat "╚" (make-string inside-w ?═) "╝"))

        ;; ║ side walls on every inside visual line (abs rows 3 .. n-vls-2)
        (dotimes (ivl n-in-vls)
          (box-diagram--put cv (abs-vl ivl) mod-lx "║")
          (box-diagram--put cv (abs-vl ivl) mod-rx "║"))

        ;; ---- Draw each inner box -------------------------------------------
        (dolist (id inner-order)
          (let* ((lbl    (box-diagram--label id defs))
                 (c      (gethash id cols 0))
                 (cx     (cx-of c))
                 (bw     (aref col-widths c))
                 (iports (gethash id in-ports  '()))
                 (oports (gethash id out-ports '()))
                 (prows  (sort (delete-dups
                                (append (list (gethash id rows 0))
                                        (mapcar #'cdr iports)
                                        (mapcar #'cdr oports)))
                               #'<))
                 (pr-min (car prows))
                 (pr-max (car (last prows)))
                 ;; Box drawing characters
                 (tl "\u250c") (tr "\u2510")
                 (bl "\u2514") (br "\u2518")
                 (vl "\u2502") (hl "\u2500")
                 (te "\u251c"))  ; T-junction right (for output port)

            ;; Top border
            (box-diagram--put cv (abs-vl (ivl-top-r pr-min)) cx
              (concat tl (make-string (- bw 2) (string-to-char hl)) tr))

            ;; Content rows
            (dolist (pr prows)
              (let* ((ivl-c   (ivl-cont pr))
                     (av      (abs-vl ivl-c))
                     (is-lbl  (= pr pr-min))
                     ;; Internal output: target is an inner node
                     (out-int (cl-some (lambda (p)
                                         (and (= (cdr p) pr)
                                              (gethash (car p) inner-set)))
                                       oports))
                     ;; External output: target is outside the module
                     (out-ext (cl-find-if
                               (lambda (p)
                                 (and (= (cdr p) pr)
                                      (not (gethash (car p) inner-set))))
                               oports))
                     (has-out (or out-int out-ext))
                     (has-in  (cl-some (lambda (p) (= (cdr p) pr)) iports))
                     (inner   (if is-lbl
                                  (let* ((s   (concat " " lbl " "))
                                         (pad (- bw 2 (length s))))
                                    (concat s (make-string (max 0 pad) ?\s)))
                                (make-string (- bw 2) ?\s)))
                     (rch     (if has-out te vl)))
                ;; Box cell
                (box-diagram--put cv av cx (concat vl inner rch))
                ;; Input arrow: --> just before left wall
                (when has-in
                  (box-diagram--put cv av (- cx 3) arrow))
                ;; Internal output: --> just after right wall
                (when out-int
                  (box-diagram--put cv av (+ cx bw) arrow))
                ;; External output: fill horiz lines from right wall to | wall,
                ;; then --> label after |.
                ;; Pattern: box+-----|-->  label
                (when out-ext
                  (let* ((fill-x (+ cx bw))
                         (fill-n (- mod-rx fill-x))
                         (to-lbl (box-diagram--label (car out-ext) defs)))
                    (when (> fill-n 0)
                      (box-diagram--put cv av fill-x
                        (make-string fill-n (string-to-char hl))))
                    (box-diagram--put cv av (+ mod-rx 1)
                      (concat arrow " " to-lbl))))))

            ;; Bottom border
            (box-diagram--put cv (abs-vl (ivl-bot-r pr-max)) cx
              (concat bl (make-string (- bw 2) (string-to-char hl)) br))

            ;; Walls between consecutive content rows (tall multi-row boxes)
            (when (> pr-max pr-min)
              (let ((pr pr-min))
                (while (< pr pr-max)
                  (let* ((ivl-a (ivl-cont pr))
                         (ivl-b (ivl-cont (1+ pr))))
                    (let ((ivl (1+ ivl-a)))
                      (while (< ivl ivl-b)
                        (box-diagram--put cv (abs-vl ivl) cx           vl)
                        (box-diagram--put cv (abs-vl ivl) (+ cx bw -1) vl)
                        (setq ivl (1+ ivl)))))
                  (setq pr (1+ pr)))))))

        ;; ---- Draw outside-left entries -------------------------------------
        ;; Pattern: "S0_AXI -------|-->" + (| already drawn) + inside "-->| box"
        (maphash
         (lambda (pr from-id)
           (let* ((lbl (box-diagram--label from-id defs))
                  (pad (make-string (- max-llbl (length lbl)) ?\s))
                  (av  (abs-vl (ivl-cont pr))))
             ;; Dashes approach the | from the left (last char is dash, not arrowhead)
             (box-diagram--put cv av 0 (concat pad lbl dashes-out))
             ;; --> enters the module on the right side of |
             (box-diagram--put cv av (+ mod-lx 1) arrow)))
         oleft)

        (box-diagram--canvas-to-lines cv)))))

;;; ---- Simple chain renderer (fallback when no double-box) ------------------

(defun box-diagram--spaces (n)
  "Return a string of N space characters."
  (make-string (max 0 n) ?\s))

(defconst box-diagram--arrow
  (concat (make-string 2 ?\u2500) "\u25ba")
  "Arrow drawn between nodes in the simple renderer.")

(defun box-diagram--node-triplet (node defs rendered)
  "Return (TOP MID BOT) for NODE in the simple chain renderer."
  (let* ((def  (cdr (assoc node defs)))
         (type (and def (plist-get def :type)))
         (base (if (and def (plist-get def :label)) (plist-get def :label) node))
         (tl "\u250c") (tr "\u2510")
         (bl "\u2514") (hl "\u2500") (vl "\u2502"))
    (cond
     ((or (null def) (eq type 'text))
      (list (box-diagram--spaces (length base)) base
            (box-diagram--spaces (length base))))
     ((eq type 'box)
      (let* ((label (if (gethash node rendered) (concat "[" base "]") base))
             (w     (length label)))
        (puthash node t rendered)
        (list (concat tl (make-string (+ w 2) (string-to-char hl)) tr)
              (concat vl " " label " " vl)
              (concat bl (make-string (+ w 2) (string-to-char hl)) "\u2518"))))
     (t (list (box-diagram--spaces (length base)) base
              (box-diagram--spaces (length base)))))))

(defun box-diagram--render-chain (chain defs rendered)
  "Render CHAIN into (TOP MID BOT) using the simple renderer."
  (let ((top "") (mid "") (bot "")
        (aw (length box-diagram--arrow)) (first t))
    (dolist (node chain)
      (unless first
        (setq top (concat top (box-diagram--spaces aw)))
        (setq mid (concat mid box-diagram--arrow))
        (setq bot (concat bot (box-diagram--spaces aw))))
      (setq first nil)
      (let ((trip (box-diagram--node-triplet node defs rendered)))
        (setq top (concat top (nth 0 trip)))
        (setq mid (concat mid (nth 1 trip)))
        (setq bot (concat bot (nth 2 trip)))))
    (list top mid bot)))

;;; ---- Top-level renderer ---------------------------------------------------

(defun box-diagram--render (text)
  "Parse TEXT and return a list of strings for the rendered diagram."
  (let* ((all-lines  (split-string text "\n"))
         (def-lines  '()) (conn-lines '()) (in-conn nil))
    (dolist (line all-lines)
      (if in-conn (push line conn-lines)
        (if (string-match-p "^[ \t]*$" line) (setq in-conn t)
          (push line def-lines))))
    (let* ((def-text  (mapconcat #'identity (nreverse def-lines)  "\n"))
           (conn-text (mapconcat #'identity (nreverse conn-lines) "\n"))
           (defs      (box-diagram--parse-defs def-text))
           (outer-box nil))
      ;; Detect optional bare double-box ID on first non-blank connection line
      (let ((first-nl nil))
        (dolist (l (split-string conn-text "\n"))
          (when (and (not first-nl) (> (length (string-trim l)) 0))
            (setq first-nl l)))
        (when (and first-nl
                   (string-match "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*$" first-nl))
          (let* ((id  (match-string 1 first-nl))
                 (def (cdr (assoc id defs))))
            (when (and def (eq (plist-get def :type) 'double-box))
              (setq outer-box def)
              (let ((removed nil) (new '()))
                (dolist (l (split-string conn-text "\n"))
                  (if (and (not removed) (string= (string-trim l) id))
                      (setq removed t)
                    (push l new)))
                (setq conn-text (mapconcat #'identity (nreverse new) "\n")))))))
      (let ((chains (box-diagram--parse-chains conn-text)))
        (if outer-box
            (box-diagram--render-graph chains defs outer-box)
          ;; Simple chain-by-chain fallback
          (let ((rendered (make-hash-table :test 'equal)) (output '()))
            (dolist (chain chains)
              (when output (push "" output))
              (let ((trip (box-diagram--render-chain chain defs rendered)))
                (push (nth 0 trip) output)
                (push (nth 1 trip) output)
                (push (nth 2 trip) output)))
            (nreverse output)))))))

;;; ---- Interactive commands -------------------------------------------------

;;;###autoload
(defun box-diagram-render ()
  "Parse current buffer as a box diagram and display the result."
  (interactive)
  (let* ((text   (buffer-substring-no-properties (point-min) (point-max)))
         (output (mapconcat #'identity (box-diagram--render text) "\n")))
    (with-current-buffer (get-buffer-create "*Box Diagram*")
      (erase-buffer) (insert output) (goto-char (point-min)))
    (display-buffer "*Box Diagram*")))

;;;###autoload
(defun box-diagram-render-region (start end)
  "Parse the selected region as a box diagram and display the result."
  (interactive "r")
  (let* ((text   (buffer-substring-no-properties start end))
         (output (mapconcat #'identity (box-diagram--render text) "\n")))
    (with-current-buffer (get-buffer-create "*Box Diagram*")
      (erase-buffer) (insert output) (goto-char (point-min)))
    (display-buffer "*Box Diagram*")))

(provide 'box-diagram)
