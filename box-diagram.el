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
;;   Unicode right-arrow (→) may be used instead of ->.
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
Lines with -> (or Unicode →) form chains.  Semicolons split multiple
chains on one line."
  (let (chains)
    (dolist (line (split-string text "\n"))
      (let ((trimmed (string-trim line)))
        (when (and (> (length trimmed) 0)
                   (string-match-p "->\\|→" trimmed)
                   (not (string-match-p ":=" trimmed)))
          (dolist (seg (split-string trimmed ";"))
            (let* ((seg   (string-trim seg))
                   (nodes (and (string-match-p "->\\|→" seg)
                               (mapcar #'string-trim
                                       (split-string seg
                                                     "[ \t]*\\(->\\|→\\)[ \t]*" t)))))
              (when (and nodes (cdr nodes))
                (push nodes chains)))))))
    (nreverse chains)))

;;; ---- Graph-based layout (used when a double-box container is declared) ----

(defun box-diagram--label (id defs)
  "Return the display label for ID."
  (let ((def (cdr (assoc id defs))))
    (if (and def (plist-get def :label)) (plist-get def :label) id)))

(defun box-diagram--bw (label)
  "Outer character width of a box: │ + space + label + space + │ = len+4."
  (+ (length label) 4))

(defun box-diagram--assign-rows (chains)
  "Return a hash id→row-number.
Each chain seeds its new nodes on the same row.  The row is determined
by the maximum row of any already-placed node in the chain plus 1, or
the next free row if no nodes are placed yet."
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
  "Return a hash id→column for nodes in INNER-SET.
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
  "Return (IN-PORTS . OUT-PORTS) where each is a hash id→list of (peer . port-row).
Port-row for edge (f→t) = max(row[f], row[t])."
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
    ;; Sort by port-row
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
  "Canvas → list of right-trimmed strings."
  (let (r)
    (dotimes (i (length canvas))
      (push (replace-regexp-in-string "[ \t]+$" "" (aref canvas i)) r))
    (nreverse r)))

;;; ---- Graph renderer --------------------------------------------------------

(defun box-diagram--render-graph (chains defs outer-def)
  "Render diagram from CHAINS/DEFS using graph-based layout.
OUTER-DEF is the double-box plist (:label :children).
Returns a list of strings."
  (let* ((inner-ids (plist-get outer-def :children))
         (title     (plist-get outer-def :label))
         ;; Sets
         (inner-set (let ((h (make-hash-table :test 'equal)))
                      (dolist (id inner-ids) (puthash id t h)) h))
         ;; Layout
         (rows (box-diagram--assign-rows chains))
         (cols (box-diagram--assign-cols chains inner-set))
         (ports (box-diagram--collect-ports chains rows inner-set))
         (in-ports  (car ports))
         (out-ports (cdr ports))
         ;; Enumerate inner nodes in order of first appearance
         (inner-order
          (let (lst)
            (dolist (chain chains)
              (dolist (id chain)
                (when (and (gethash id inner-set) (not (member id lst)))
                  (push id lst))))
            (nreverse lst)))
         ;; Column count and widths
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
         ;; Number of grid rows
         (num-rows
          (let ((seen (make-hash-table :test 'eql)))
            (maphash (lambda (_ v) (puthash v t seen)) rows)
            (hash-table-count seen)))
         ;; Outside-left/right per port-row
         ;; oleft[port-row] = outside-node id entering at that row
         ;; oright[port-row] = outside-node id exiting at that row
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
    ;; Left outside: label + " ──────►" (8 chars) flush-left padded
    (let* ((arrow-out " ──────►")       ; 8 chars
           (left-lbls (let (ls) (maphash (lambda (_ id)
                                           (push (box-diagram--label id defs) ls))
                                         oleft) ls))
           (max-llbl  (if left-lbls (apply #'max (mapcar #'length left-lbls)) 0))
           (left-w    (+ max-llbl (length arrow-out)))  ; width of left outside section
           ;; Inside geometry
           ;; Layout of each canvas row within the inside region (x=0 at ║):
           ;;   [0..1]            2-char "entry" slot (── or spaces or ─►)
           ;;   [2..2+cw0-1]      col-0 box
           ;;   [2+cw0..2+cw0+2]  3-char gap (──► or spaces) between col 0 and 1
           ;;   ... etc.
           ;;   after last col: 2-char exit padding
           (gap 3)
           (entry-w 2)
           (exit-pad 2)
           (col-x                        ; start x of each col within inside region
            (let ((v (make-vector num-cols 0)) (x entry-w))
              (dotimes (c num-cols)
                (aset v c x)
                (setq x (+ x (aref col-widths c) gap)))
              v))
           (inside-w
            (+ entry-w
               (apply #'+ (mapcar #'identity (cl-coerce col-widths 'list)))
               (* gap (max 0 (1- num-cols)))
               exit-pad))
           ;; Ensure inside-w >= title length
           (inside-w (max inside-w (+ (length title) 4)))

           ;; Visual line structure (inside the separator):
           ;;   Row 0 contributes: VL_top (top borders), VL_content
           ;;   Between row r and r+1:  VL_trans (bottoms of single-row / walls of multi)
           ;;   Row r>0 contributes:
           ;;     VL_conn  (row-r connection line: new tops + continuing content-row-r)
           ;;     VL_label (labels of new row-r boxes, bottoms of ending multi-row boxes)
           ;;   Footer after last row: VL_foot (bottoms of everything still open)
           ;;
           ;; Mapping (0-based within the inside region):
           ;;   row 0 top-borders : 0
           ;;   row 0 content     : 1
           ;;   transition 0→1    : 2
           ;;   row 1 conn        : 3
           ;;   row 1 label       : 4
           ;;   transition 1→2    : 5
           ;;   ...
           ;;   footer after last row:
           ;;     if num_rows=1: 2  (= transition 0→(nothing) = footer)
           ;;     if num_rows=2: 5
           ;; General:
           ;;   row-r top-borders (r=0 only): 0
           ;;   row-r content:  1 + 3*r   (but 0-top at position 0 is special)
           ;;   Actually for r=0: top=0, content=1
           ;;   For r>0:  conn=3*r, label=3*r+1
           ;;   Transition after r: 3*r+2  (for r=0: pos 2; for r=1: pos 5; ...)
           ;; Total inside vls: 3*num_rows
           (n-in-vls (* 3 (max num-rows 1)))
           ;; Total canvas rows: 3 header + n-in-vls + 1 footer = n-in-vls+4
           (n-vls (+ n-in-vls 4))
           ;; Canvas width
           (canvas-w (+ left-w 1 inside-w 1 40))
           (cv (box-diagram--canvas n-vls canvas-w))
           ;; Absolute x of module left/right borders
           (mod-lx left-w)
           (mod-rx (+ mod-lx 1 inside-w)))

      ;; Helper: absolute canvas row for an inside visual line index
      (cl-flet ((abs-vl (ivl) (+ 3 ivl))
                ;; inside-vl for row R, content row
                (ivl-content (r) (if (= r 0) 1 (* 3 r)))
                ;; inside-vl for row 0 top-borders
                (ivl-top0 () 0)
                ;; inside-vl for transition after row R
                (ivl-trans (r) (+ (* 3 r) 2))
                ;; inside-vl for row R connection (R>0, same vl as "between" row)
                ;; = ivl-trans of R-1 + 1, but let's just use 3*r-1 for R>0
                ;; Actually: for R>0, the "connection" line = 3*R-1 (one before ivl-content)
                ;;   R=1: 3*1-1=2 -- but that's the transition after row 0!
                ;; Let me redefine more carefully:
                ;;   The "connection" visual line for row R>0 IS the transition vl of row R-1
                ;;   (i.e. they share the same canvas row).
                ;;   ivl_trans(r) = 3*r+2
                ;;   ivl_content(r>0) = 3*r
                ;;   The "header" of row r>0 (top borders of new boxes, content-r of multi)
                ;;   = ivl_trans(r-1)+1 = 3*(r-1)+3 = 3*r  -- same as ivl_content for r>0!
                ;; So for r>0, the connection/header line = 3*r, and label line = 3*r+1.
                ;; This unifies cleanly:
                ;;   ivl_content(0) = 1, ivl_content(r>0) = 3*r
                ;; Footer = ivl_trans(num_rows-1) = 3*(num_rows-1)+2 = 3*num_rows-1
                ;; Total = 3*num_rows lines (0..3*num_rows-1) ✓
                (cx-of (c) (+ mod-lx 1 (aref col-x c))))

        ;; Draw module outer borders (rows 0,1,2 are header; last row is footer)
        (box-diagram--put cv 0 mod-lx
          (concat "╔" (make-string inside-w ?═) "╗"))
        (let* ((tpad   (/ (- inside-w (length title)) 2))
               (tpad-r (- inside-w (length title) tpad)))
          (box-diagram--put cv 1 mod-lx
            (concat "║" (make-string tpad ?\s) title
                    (make-string tpad-r ?\s) "║")))
        (box-diagram--put cv 2 mod-lx
          (concat "╠" (make-string inside-w ?═) "╣"))
        (box-diagram--put cv (1- n-vls) mod-lx
          (concat "╚" (make-string inside-w ?═) "╝"))

        ;; Draw ║ on every inside visual line (filled over by connections later)
        (dotimes (ivl n-in-vls)
          (box-diagram--put cv (abs-vl ivl) mod-lx  "║")
          (box-diagram--put cv (abs-vl ivl) mod-rx "║"))

        ;; ---- Draw each inner node ------------------------------------------
        (dolist (id inner-order)
          (let* ((lbl    (box-diagram--label id defs))
                 (c      (gethash id cols 0))
                 (cx     (cx-of c))
                 (cw     (aref col-widths c))
                 ;; Use full column width for the box (pad short labels)
                 (bw     cw)
                 (iports (gethash id in-ports  '()))
                 (oports (gethash id out-ports '()))
                 ;; All port rows for this node
                 (prows  (sort (delete-dups
                                (append (list (gethash id rows 0))
                                        (mapcar #'cdr iports)
                                        (mapcar #'cdr oports)))
                               #'<))
                 (pr-min (car prows))
                 (pr-max (car (last prows)))
                 ;; Inside VL for top border: one line before first content
                 (ivl-top (if (= pr-min 0)
                              (ivl-top0)
                            (1- (ivl-content pr-min))))
                 ;; Inside VL for bottom border: one line after last content
                 (ivl-bot (1+ (ivl-content pr-max))))

            ;; Top border
            (box-diagram--put cv (abs-vl ivl-top) cx
              (concat "┌" (make-string (- bw 2) ?─) "┐"))

            ;; Content rows
            (dolist (pr prows)
              (let* ((is-lbl (= pr pr-min))
                     (has-out (cl-some (lambda (p) (= (cdr p) pr)) oports))
                     (has-in  (cl-some (lambda (p) (= (cdr p) pr)) iports))
                     (ivl-c   (ivl-content pr))
                     (inner   (if is-lbl
                                  (let* ((s   (concat " " lbl " "))
                                         (pad (- bw 2 (length s))))
                                    (concat s (make-string (max 0 pad) ?\s)))
                                (make-string (- bw 2) ?\s)))
                     (rch     (if has-out "├" "│")))
                ;; Box cell: │<inner><rch>
                (box-diagram--put cv (abs-vl ivl-c) cx
                  (concat "│" inner rch))
                ;; Input arrow left of box
                (when has-in
                  (box-diagram--put cv (abs-vl ivl-c) (- cx 3) "──►"))
                ;; Output arrow right of box
                (when has-out
                  (box-diagram--put cv (abs-vl ivl-c) (+ cx bw) "──►"))))

            ;; Bottom border
            (box-diagram--put cv (abs-vl ivl-bot) cx
              (concat "└" (make-string (- bw 2) ?─) "┘"))

            ;; Walls on visual lines between content rows (for tall boxes)
            (when (> pr-max pr-min)
              (let ((pr pr-min))
                (while (< pr pr-max)
                  (let* ((ivl-this (ivl-content pr))
                         (ivl-next (ivl-content (1+ pr))))
                    (let ((ivl (1+ ivl-this)))
                      (while (< ivl ivl-next)
                        (box-diagram--put cv (abs-vl ivl) cx           "│")
                        (box-diagram--put cv (abs-vl ivl) (+ cx bw -1) "│")
                        (setq ivl (1+ ivl)))))
                  (setq pr (1+ pr)))))))

        ;; ---- Draw outside-left entries -------------------------------------
        (maphash
         (lambda (pr from-id)
           (let* ((lbl    (box-diagram--label from-id defs))
                  (pad    (make-string (- max-llbl (length lbl)) ?\s))
                  (ivl-c  (ivl-content pr))
                  (a-vl   (abs-vl ivl-c))
                  ;; "S0_AXI ──────►║─►"
                  (str    (concat pad lbl arrow-out "║─►")))
             (box-diagram--put cv a-vl 0 str)))
         oleft)

        ;; ---- Draw outside-right exits -------------------------------------
        (maphash
         (lambda (pr to-id)
           (let* ((lbl   (box-diagram--label to-id defs))
                  (ivl-c (ivl-content pr))
                  (a-vl  (abs-vl ivl-c))
                  ;; At mod-rx: replace ║ with ╬, then "──► label"
                  (str   (concat "╬──► " lbl)))
             (box-diagram--put cv a-vl mod-rx str)))
         oright)

        ;; ---- Pad all inside lines to reach right border on exit rows ------
        ;; (already handled by the ║ pre-draw and the ╬ overwrite)

        (box-diagram--canvas-to-lines cv)))))

;;; ---- Simple chain renderer (fallback when no double-box) ------------------

(defun box-diagram--spaces (n)
  "Return a string of N space characters."
  (make-string (max 0 n) ?\s))

(defconst box-diagram--arrow "──►"
  "Arrow drawn between nodes in the simple renderer.")

(defun box-diagram--node-triplet (node defs rendered)
  "Return (TOP MID BOT) for NODE in the simple chain renderer."
  (let* ((def  (cdr (assoc node defs)))
         (type (and def (plist-get def :type)))
         (base (if (and def (plist-get def :label)) (plist-get def :label) node)))
    (cond
     ((or (null def) (eq type 'text))
      (list (box-diagram--spaces (length base)) base
            (box-diagram--spaces (length base))))
     ((eq type 'box)
      (let* ((label (if (gethash node rendered) (concat "[" base "]") base))
             (w     (length label)))
        (puthash node t rendered)
        (list (concat "┌" (make-string (+ w 2) ?─) "┐")
              (concat "│ " label " │")
              (concat "└" (make-string (+ w 2) ?─) "┘"))))
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
