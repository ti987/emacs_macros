;;
;; box-diagram.el - Draw Unicode block diagrams from text input
;;
;; Syntax:
;;
;;   -- Definition section (before the first blank line) --
;;
;;   ID := box("label")
;;       Single-border box.  Use \n in label for multi-line text.
;;
;;   ID := r-box("label")
;;       Rounded-corner box (╭─╮ / ╰─╯).  Use \n for multi-line text.
;;
;;   ID := double-box("label", [ID1, ID2, ...])
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
;;   Bracket fan-out: a final [A, B] target splits one source into many:
;;
;;     ID1 -> ID2 -> [ID3, ID4]
;;
;;   Unicode right-arrow (U+2192) may be used instead of ->.
;;
;;   Bidirectional arrow <-> draws ◄─► (triangles at both ends):
;;
;;     ID1 <-> ID2
;;
;;   The very first non-blank line of the connection section may be a bare
;;   identifier naming a double-box container (trailing ; is allowed).  The
;;   graph-based renderer is then used: each box appears exactly once; text
;;   nodes appear outside the module border.
;;
;;   The double-box ID may also appear in chains for module-level connections:
;;
;;     I3 -> F -> O3       (arrow enters/exits the module border)
;;
;;   Dot-edge attributes on a node override the default left/right entry/exit:
;;
;;     ID1 -> ID2.b    ; arrow from ID1 right to ID2 bottom  (▲ near ID2 bottom)
;;     ID1 -> ID2.t    ; arrow from ID1 right to ID2 top     (▼ near ID2 top)
;;     ID1.b -> ID2    ; arrow from ID1 bottom to ID2 left   (▼ near ID2 entry)
;;     ID1.t -> ID2    ; arrow from ID1 top to ID2 left      (▲ near ID2 entry)
;;   Triangle arrowheads are placed near the destination, not the source.
;;
;;   .l (left) and .r (right) are accepted but are equivalent to the defaults.
;;
;;   Comments: anything following ## (with or without a space) to the end of
;;   a line is treated as a comment and ignored.  A line that starts with ##
;;   is treated as a whole-line comment (the line is ignored entirely).
;;   Blank lines (empty or only whitespace after comment stripping) are also
;;   ignored.
;;
;;     ## This whole line is a comment.
;;     A := box("Raster")  ## inline comment after definition
;;     I1 -> A -> B        ## inline comment after chain
;;
;;   Line-prefix stripping: lines whose first non-whitespace characters are
;;   "//", "--", or a single "#" (but NOT "##") have that prefix removed
;;   before parsing.  No trailing space after the prefix is required.  This
;;   lets you embed diagram expressions as comments in many programming
;;   languages:
;;
;;     //A := box("Input")        ← C/Java/JS comment (no space needed)
;;     -- B := box("Output")      ← SQL / Haskell comment
;;     #A -> B                    ← shell/Python single-# comment line
;;
;;   A line containing only "//", "--", or "#" (optionally with whitespace)
;;   is treated as a blank separator between the definition section and the
;;   connection section, just like an empty line.
;;
;; Usage:
;;   M-x box-diagram-render        - render the current buffer
;;   M-x box-diagram-render-region - render the selected region

(require 'cl-lib)
(require 'subr-x)

;;; ---- Parsers ---------------------------------------------------------------

(defun box-diagram--strip-line-prefix (line)
  "Strip a language comment prefix from the start of LINE, if present.
Strips leading '//', '--', or a single '#' (but NOT '##') so that diagram
expressions can be embedded as comments in many programming languages.
No trailing space after the prefix is required.
Leading whitespace before the prefix is re-attached to the result so
relative indentation is preserved.
'##' is intentionally NOT stripped here — it is the box-diagram comment
marker and is handled by the inline-comment stripper."
  (cond
   ;; '//' or '--': strip the two-char prefix unconditionally.
   ((string-match "^\\([ \t]*\\)\\(//\\|--\\)" line)
    (concat (match-string 1 line) (substring line (match-end 2))))
   ;; '#' but NOT '##': strip the single '#'; preserve what follows it.
   ;; When '##' is present, [^#] fails on the second '#' and $ fails
   ;; (not end-of-string), so the match fails and line is returned as-is.
   ((string-match "^\\([ \t]*\\)#\\([^#]\\|$\\)" line)
    (concat (match-string 1 line) (substring line (match-beginning 2))))
   (t line)))

(defun box-diagram--parse-defs (text)
  "Return alist (ID . PLIST) from definition lines in TEXT.
Plist keys: :type (box|double-box|text), :label, :children (double-box)."
  (let (defs)
    (dolist (line (split-string text "\n"))
      (let ((line (replace-regexp-in-string "##.*$" ""
                    (box-diagram--strip-line-prefix line))))
      (cond
       ((string-match
         "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*:=[ \t]*box(\"\\([^\"]+\\)\")" line)
        (push (cons (match-string 1 line)
                    (list :type 'box :label
                          (replace-regexp-in-string "\\\\n" "\n" (match-string 2 line)))) defs))
       ((string-match
         "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*:=[ \t]*r-box(\"\\([^\"]+\\)\")" line)
        (push (cons (match-string 1 line)
                    (list :type 'r-box :label
                          (replace-regexp-in-string "\\\\n" "\n" (match-string 2 line)))) defs))
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
               ;; Children enclosed in [ ]: double-box("Label", [A, B, C])
               (kids  (when (string-match
                             ",[ \t]*\\[[ \t]*\\([^]]*\\)[ \t]*\\]" rest)
                        (delq nil
                              (mapcar (lambda (s)
                                        (let ((s (string-trim s)))
                                          (when (string-match "^[A-Za-z0-9_]+$" s) s)))
                                      (split-string (match-string 1 rest) ","))))))
          (push (cons id (list :type 'double-box :label label :children kids)) defs))))))
    (nreverse defs)))

(defun box-diagram--split-node (s)
  "Split 'ID' or 'ID.EDGE' string into (ID . EDGE-OR-NIL).
EDGE is one of \"b\" (bottom), \"t\" (top), \"l\" (left), \"r\" (right)."
  (let ((s (string-trim s)))
    (if (string-match "^\\([A-Za-z0-9_]+\\)\\.\\([btlr]\\)$" s)
        (cons (match-string 1 s) (match-string 2 s))
      (cons s nil))))

(defun box-diagram--parse-chains (text)
  "Return (CHAINS BRANCH-GROUPS EDGE-ALIST BIDIR-SET) from TEXT.
CHAINS is a list of chains (lists of plain ID strings).  A final [A, B]
segment is expanded into one chain per target.
BRANCH-GROUPS is an alist (SOURCE-ID . (TARGET-ID ...)) for fan-outs.
EDGE-ALIST is an alist ((SRC-ID . TGT-ID) . (SRC-EDGE . TGT-EDGE)) where
each EDGE is nil or one of \"b\", \"t\", \"l\", \"r\" (see box-diagram--split-node).
BIDIR-SET is a hash (SRC-ID . TGT-ID) -> t for bidirectional edges (<->)."
  (let ((chains nil) (branch-groups nil) (edge-alist nil)
        (bidir-set (make-hash-table :test 'equal))
        ;; Separator regex matches <->, -> or → (try <-> before -> so it wins).
        (sep-re "[ \t]*\\(<->\\|->\\|\u2192\\)[ \t]*"))
    (dolist (line (split-string text "\n"))
      (let* ((line    (replace-regexp-in-string "##.*$" ""
                        (box-diagram--strip-line-prefix line)))
             (trimmed (string-trim line)))
        (when (and (> (length trimmed) 0)
                   (string-match-p "<->\\|->\\|\u2192" trimmed)
                   (not (string-match-p ":=" trimmed)))
          (dolist (seg (split-string trimmed ";"))
            (let ((seg (string-trim seg)))
              (when (string-match-p "<->\\|->\\|\u2192" seg)
                (let* ((raw-parts (mapcar #'string-trim
                                          (split-string seg sep-re t)))
                       ;; Extract separators by rescanning the segment
                       (seps (let ((ss nil) (scan seg))
                               (while (string-match sep-re scan)
                                 (push (match-string 1 scan) ss)
                                 (setq scan (substring scan (match-end 0))))
                               (nreverse ss))))
                  (when (cdr raw-parts)
                    (let* ((last-raw  (car (last raw-parts)))
                           (pfx-raw   (butlast raw-parts)))
                      (if (string-match
                           "^\\[[ \t]*\\([^]]+\\)[ \t]*\\]$" last-raw)
                          ;; Bracket group: fan out from last prefix node
                          (let* ((inner      (match-string 1 last-raw))
                                 (tgt-raw    (mapcar #'string-trim
                                                     (split-string inner ",")))
                                 (pfx-split  (mapcar #'box-diagram--split-node pfx-raw))
                                 (pfx-ids    (mapcar #'car pfx-split))
                                 (pfx-edges  (mapcar #'cdr pfx-split))
                                 (tgt-split  (mapcar #'box-diagram--split-node tgt-raw))
                                 (tgt-ids    (mapcar #'car tgt-split))
                                 (tgt-edges  (mapcar #'cdr tgt-split))
                                 (source     (car (last pfx-ids)))
                                 (src-edge   (car (last pfx-edges))))
                            ;; Record edges for consecutive prefix pairs
                            (let ((i 0))
                              (while (< i (1- (length pfx-ids)))
                                (let ((fe (nth i pfx-edges))
                                      (te (nth (1+ i) pfx-edges)))
                                  (when (or fe te)
                                    (push (cons (cons (nth i pfx-ids)
                                                      (nth (1+ i) pfx-ids))
                                                (cons fe te))
                                          edge-alist)))
                                (setq i (1+ i))))
                            ;; Record branch group
                            (when source
                              (let ((existing (assoc source branch-groups)))
                                (if existing
                                    (setcdr existing
                                            (cl-union (cdr existing) tgt-ids
                                                      :test #'equal))
                                  (push (cons source tgt-ids) branch-groups))))
                            ;; Expand: one chain per target
                            (let ((i 0))
                              (while (< i (length tgt-ids))
                                (let* ((tgt-id (nth i tgt-ids))
                                       (tgt-e  (nth i tgt-edges))
                                       (chain  (append pfx-ids (list tgt-id))))
                                  (when (cdr chain) (push chain chains))
                                  (when (or src-edge tgt-e)
                                    (push (cons (cons source tgt-id)
                                                (cons src-edge tgt-e))
                                          edge-alist)))
                                (setq i (1+ i)))))
                        ;; Normal chain
                        (let* ((split-parts (mapcar #'box-diagram--split-node raw-parts))
                               (ids   (mapcar #'car split-parts))
                               (edges (mapcar #'cdr split-parts)))
                          (push ids chains)
                          (let ((i 0))
                            (while (< i (1- (length ids)))
                              (let ((fe  (nth i edges))
                                    (te  (nth (1+ i) edges))
                                    (sep (nth i seps))
                                    (f   (nth i ids))
                                    (t_  (nth (1+ i) ids)))
                                (when (or fe te)
                                  (push (cons (cons f t_) (cons fe te)) edge-alist))
                                (when (equal sep "<->")
                                  (puthash (cons f t_) t bidir-set)))
                              (setq i (1+ i)))))))))))))))
    (list (nreverse chains) branch-groups edge-alist bidir-set)))

;;; ---- Graph-based layout (used when a double-box container is declared) ----

(defun box-diagram--label (id defs)
  "Return the display label for ID."
  (let ((def (cdr (assoc id defs))))
    (if (and def (plist-get def :label)) (plist-get def :label) id)))

(defun box-diagram--label-lines (label)
  "Split LABEL on newline characters; return list of line strings."
  (split-string label "\n"))

(defun box-diagram--bw (label)
  "Outer character width of a box: max line width + 4."
  (+ (apply #'max (mapcar #'length (box-diagram--label-lines label))) 4))

(defun box-diagram--assign-rows (chains &optional cols inner-set)
  "Return a hash id->row-number.
Each chain seeds its new nodes on the same row determined by the maximum
row of any already-placed node in the chain plus 1, or the next free row.
When COLS (hash id->col) and INNER-SET (hash id->t) are provided, the base
row is bumped until no already-placed inner node shares both the same column
and the same row as any new inner node in this chain."
  (let ((rows (make-hash-table :test 'equal))
        (next 0))
    (dolist (chain chains)
      (let ((max-placed -1) (has-placed nil))
        (dolist (id chain)
          (when (gethash id rows)
            (setq has-placed t)
            (setq max-placed (max max-placed (gethash id rows)))))
        (let* ((candidate (if has-placed (1+ max-placed) next))
               (base
                (if (and has-placed cols inner-set)
                    ;; Column-aware conflict avoidance: collect (col . row) pairs
                    ;; for already-placed inner nodes not in the current chain.
                    (let* ((placed-cr
                            (let (ps)
                              (maphash (lambda (k v)
                                         (when (and (not (member k chain))
                                                    (gethash k inner-set))
                                           (push (cons (gethash k cols 0) v) ps)))
                                       rows)
                              ps))
                           ;; Columns of new inner nodes we are about to place
                           (new-cols
                            (delq nil
                                  (mapcar (lambda (id)
                                            (when (and (not (gethash id rows))
                                                       (gethash id inner-set))
                                              (gethash id cols 0)))
                                          chain)))
                           (b candidate))
                      (while (cl-some (lambda (c)
                                        (member (cons c b) placed-cr))
                                      new-cols)
                        (setq b (1+ b)))
                      b)
                  candidate)))
          (when (>= base next) (setq next (1+ base)))
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

(defun box-diagram--bump-vertical-rows (chains rows inner-set edge-alist)
  "Ensure targets of .b/.t edges are placed below source's max horizontal port-row.
CHAINS is the list of connection chains.
ROWS is a hash id->row-number, modified in place.
INNER-SET is a hash id->t of nodes rendered inside the double-box border.
EDGE-ALIST maps (SRC-ID . TGT-ID) -> (SRC-EDGE . TGT-EDGE) for dot-edge attrs.
Call this after `box-diagram--assign-rows' and `box-diagram--assign-cols'."
  (dolist (entry edge-alist)
    (let* ((key    (car entry))
           (src-id (car key))
           (tgt-id (cdr key))
           (src-e  (cadr entry)))
      ;; Only handle src.b / src.t edges where both endpoints are inner nodes
      (when (and (member src-e '("b" "t"))
                 (gethash src-id inner-set)
                 (gethash tgt-id inner-set))
        ;; Compute src's max port-row considering only horizontal connections
        (let ((src-max-pr (gethash src-id rows 0)))
          (dolist (chain chains)
            (let ((rest chain))
              (while (cdr rest)
                (let* ((f   (car rest))
                       (t_  (cadr rest))
                       (ee  (cdr (cl-assoc (cons f t_) edge-alist :test #'equal)))
                       (fe  (car ee)))
                  ;; Horizontal exits from src (fe not b/t, target must be inner)
                  (when (and (equal f src-id)
                             (gethash t_ inner-set)
                             (not (member fe '("b" "t"))))
                    (let ((pr (max (gethash f rows 0) (gethash t_ rows 0))))
                      (when (> pr src-max-pr) (setq src-max-pr pr))))
                  ;; Any inner-node connection into src counts for its port-row
                  (when (and (equal t_ src-id)
                             (gethash f inner-set))
                    (let ((pr (max (gethash f rows 0) (gethash t_ rows 0))))
                      (when (> pr src-max-pr) (setq src-max-pr pr)))))
                (setq rest (cdr rest)))))
          ;; If tgt would overlap src's span, bump tgt and later inner nodes
          (let ((tgt-row (gethash tgt-id rows 0)))
            (when (<= tgt-row src-max-pr)
              (let* ((new-row (1+ src-max-pr))
                     (delta   (- new-row tgt-row)))
                (maphash (lambda (id r)
                           (when (and (>= r tgt-row)
                                      (not (equal id src-id))
                                      (gethash id inner-set))
                             (puthash id (+ r delta) rows)))
                         rows)))))))))

(defun box-diagram--collect-ports (chains rows inner-set &optional edge-alist)
  "Return (IN-PORTS . OUT-PORTS) where each is a hash id->list of (peer . port-row).
Port-row for edge (f->t): max(row[f], row[t]) when both are inner nodes;
the inner node's own row when one endpoint is a text (non-inner) node.
Connections whose entry in EDGE-ALIST carries a .b or .t edge qualifier have
their normal horizontal port suppressed; they are rendered as vertical arrows."
  (let ((in-p  (make-hash-table :test 'equal))
        (out-p (make-hash-table :test 'equal)))
    (dolist (chain chains)
      (let ((rest chain))
        (while (cdr rest)
          (let* ((f    (car rest))
                 (t_   (cadr rest))
                 (eq-e (cdr (cl-assoc (cons f t_) edge-alist :test #'equal)))
                 (f-e  (car eq-e))
                 (t-e  (cdr eq-e))
                 ;; Suppress horizontal port for top/bottom edge qualifiers
                 (skip-out (member f-e '("b" "t")))
                 (skip-in  (member t-e '("b" "t")))
                 ;; Port-row: use inner-node's own row when one endpoint is text
                 (pr (cond
                       ((and (gethash f inner-set) (gethash t_ inner-set))
                        (max (gethash f rows 0) (gethash t_ rows 0)))
                       ((gethash f inner-set) (gethash f rows 0))
                       ((gethash t_ inner-set) (gethash t_ rows 0))
                       (t 0))))
            (unless skip-out
              (let ((cur (gethash f out-p '())))
                (unless (assoc t_ cur)
                  (puthash f (append cur (list (cons t_ pr))) out-p))))
            (unless skip-in
              (let ((cur (gethash t_ in-p '())))
                (unless (assoc f cur)
                  (puthash t_ (append cur (list (cons f pr))) in-p)))))
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

(defun box-diagram--render-graph (chains defs outer-def mod-ins mod-outs branch-groups edge-alist bidir-set)
  "Render diagram using graph-based layout.
CHAINS/DEFS describe the inner topology.
OUTER-DEF is the double-box plist (:label :children).
MOD-INS / MOD-OUTS are lists of IDs for module-level border connections.
BRANCH-GROUPS is alist (SOURCE . (TARGET ...)) for bracket fan-out groups.
EDGE-ALIST is alist ((SRC . TGT) . (SRC-EDGE . TGT-EDGE)) for dot-edge attrs.
BIDIR-SET is a hash (SRC . TGT) -> t for bidirectional edges (<->).
Returns a list of strings."
  (let* ((inner-ids (plist-get outer-def :children))
         (title     (plist-get outer-def :label))
         (inner-set (let ((h (make-hash-table :test 'equal)))
                      (dolist (id inner-ids) (puthash id t h)) h))
         (cols (box-diagram--assign-cols chains inner-set))
         (rows (let ((r (box-diagram--assign-rows chains cols inner-set)))
                 (box-diagram--bump-vertical-rows chains r inner-set edge-alist)
                 r))
         (ports (box-diagram--collect-ports chains rows inner-set edge-alist))
         (in-ports  (car ports))
         (out-ports (cdr ports))
         ;; Inner nodes in first-appearance order across chains
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
          (if inner-order
              (1+ (apply #'max (mapcar (lambda (id) (gethash id rows 0)) inner-order)))
            1))
         ;; Outside-left/right maps: port-row -> outside-node-id
         (oleft  (make-hash-table :test 'eql))
         (oright (make-hash-table :test 'eql))
         ;; Module-level port count: one canvas row per (mod-in or mod-out)
         (n-mod-ports (max (length mod-ins) (length mod-outs))))

    ;; Populate oleft / oright from inner chains
    (dolist (chain chains)
      (let ((rest chain))
        (while (cdr rest)
          (let* ((f  (car rest)) (t_ (cadr rest))
                 ;; Use inner-node's own row when one side is a text node
                 (pr (if (gethash f inner-set)
                         (gethash f rows 0)
                       (gethash t_ rows 0))))
            (when (and (not (gethash f inner-set)) (gethash t_ inner-set))
              (puthash pr f oleft))
            (when (and (gethash f inner-set) (not (gethash t_ inner-set)))
              (puthash pr t_ oright)))
          (setq rest (cdr rest)))))

    ;; ---- Geometry -----------------------------------------------------------
    (let* ((dashes-out (concat " " (make-string 7 ?\u2500)))
           ;; max-llbl: widest label among outside-left entries AND module inputs
           (left-lbls
            (let (ls) (maphash (lambda (_ id)
                                 (push (box-diagram--label id defs) ls))
                               oleft) ls))
           (max-llbl
            (apply #'max
                   (cons 0 (mapcar #'length
                                   (append left-lbls
                                           (mapcar (lambda (id)
                                                     (box-diagram--label id defs))
                                                   mod-ins))))))
           (left-w   (+ max-llbl (length dashes-out)))
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
           ;; Dynamic VL scheme: n-mod-ports module rows at the top,
           ;; then for each grid row r: top + row-heights[r] content + bottom
           (row-heights
            (let ((v (make-vector num-rows 1)))
              (dolist (id inner-order)
                (let* ((r (gethash id rows 0))
                       (h (length (box-diagram--label-lines
                                   (box-diagram--label id defs)))))
                  (when (> h (aref v r)) (aset v r h))))
              v))
           ;; ivl-base[r]: first inside-VL for grid row r.
           ;; Layout per row: top-border + rh content rows + bottom-border + 1 gap.
           ;; The gap row gives space for ▼/▲ indicators outside box borders.
           (ivl-base
            (let ((v (make-vector num-rows 0)) (acc n-mod-ports))
              (dotimes (r num-rows)
                (aset v r acc)
                (setq acc (+ acc (aref row-heights r) 3)))
              v))
           (n-in-vls
            (+ n-mod-ports
               (let ((acc 0))
                 (dotimes (r num-rows) (setq acc (+ acc (aref row-heights r) 3)))
                 acc)))
           (n-vls    (+ n-in-vls 2))
           (canvas-w (+ left-w 1 inside-w 1 40))
           (cv       (box-diagram--canvas n-vls canvas-w))
           (mod-lx   left-w)
           (mod-rx   (+ mod-lx 1 inside-w))
           (arrow    (concat (make-string 2 ?\u2500) "\u25ba"))
           ;; secondary-branch-pts: (src-id . pr) → t for all but the topmost
           ;; branch-pr of each fan-out source (those rows get │ not ├ on box wall).
           (secondary-branch-pts
            (let ((h (make-hash-table :test #'equal)))
              (dolist (bg branch-groups)
                (when (gethash (car bg) inner-set)
                  (let* ((source  (car bg))
                         (targets (cdr bg))
                         (bprs
                          (sort (delq nil
                                      (mapcar (lambda (tgt)
                                                (let ((p (assoc tgt
                                                                (gethash source
                                                                         out-ports '()))))
                                                  (when p (cdr p))))
                                              targets))
                                #'<)))
                    (dolist (pr (cdr bprs))
                      (puthash (cons source pr) t h)))))
              h)))

      (cl-flet
          ((abs-vl    (ivl) (+ 1 ivl))
           (ivl-top-r (r)   (aref ivl-base r))
           (ivl-cont  (r l) (+ (aref ivl-base r) 1 l))
           (ivl-bot-r (r)   (+ (aref ivl-base r) (aref row-heights r) 1))
           (cx-of     (c)   (+ mod-lx 1 (aref col-x c))))

        ;; Top border: ╔══ Title ══╗
        (let* ((tpad   (/ (- inside-w (length title) 2) 2))
               (tpad-r (- inside-w (length title) 2 tpad)))
          (box-diagram--put cv 0 mod-lx
            (concat "\u2554" (make-string tpad ?\u2550) " " title " "
                    (make-string tpad-r ?\u2550) "\u2557")))
        ;; Bottom border: ╚═══╝
        (box-diagram--put cv (1- n-vls) mod-lx
          (concat "\u255a" (make-string inside-w ?\u2550) "\u255d"))

        ;; ║ side walls on every inside row
        (dotimes (ivl n-in-vls)
          (box-diagram--put cv (abs-vl ivl) mod-lx "\u2551")
          (box-diagram--put cv (abs-vl ivl) mod-rx "\u2551"))

        ;; ---- Module-level port rows (above inner grid rows) ------------------
        ;; Pattern (mod-in): "label ──────►║"  arrow ends AT left wall
        ;; Pattern (mod-out): "║──► label"  arrow exits right wall; no interior fill
        (dotimes (i n-mod-ports)
          (let ((av (abs-vl i)))
            ;; Left: module input — arrow terminates at the left wall
            (when (< i (length mod-ins))
              (let* ((lbl (box-diagram--label (nth i mod-ins) defs))
                     (pad (make-string (- max-llbl (length lbl)) ?\s)))
                (box-diagram--put cv av 0
                  (concat pad lbl " "
                          (make-string (- left-w max-llbl 1 (length arrow)) ?\u2500)
                          arrow))))
            ;; Right: module output — exits right wall with no interior fill
            (when (< i (length mod-outs))
              (let ((lbl (box-diagram--label (nth i mod-outs) defs)))
                (box-diagram--put cv av (+ mod-rx 1)
                  (concat arrow " " lbl))))))

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
                 (def-type (let ((d (cdr (assoc id defs))))
                             (and d (plist-get d :type))))
                 (tl (if (eq def-type 'r-box) "\u256d" "\u250c"))
                 (tr (if (eq def-type 'r-box) "\u256e" "\u2510"))
                 (bl (if (eq def-type 'r-box) "\u2570" "\u2514"))
                 (br (if (eq def-type 'r-box) "\u256f" "\u2518"))
                 (hl "\u2500")
                 (vl "\u2502") (te "\u251c"))

            ;; Top border
            (box-diagram--put cv (abs-vl (ivl-top-r pr-min)) cx
              (concat tl (make-string (- bw 2) (string-to-char hl)) tr))

            ;; Content rows
            (let* ((lbls   (box-diagram--label-lines lbl))
                   (n-lbls (length lbls)))
              (dolist (pr prows)
                (let* ((out-int (cl-some (lambda (p)
                                           (and (= (cdr p) pr)
                                                (gethash (car p) inner-set)))
                                         oports))
                       (out-ext (cl-find-if
                                 (lambda (p)
                                   (and (= (cdr p) pr)
                                        (not (gethash (car p) inner-set))))
                                 oports))
                       (has-out (or out-int out-ext))
                       (has-in  (cl-some (lambda (p) (= (cdr p) pr)) iports))
                       ;; Use ◄─► if any in-source at this pr has a bidir edge
                       (bidir-in (cl-some (lambda (p)
                                            (and (= (cdr p) pr)
                                                 (gethash (cons (car p) id) bidir-set)))
                                          iports))
                       (in-arw  (if bidir-in box-diagram--bidir-arrow arrow))
                       ;; Use │ instead of ├ when outgoing edge is bidir
                       (bidir-out (cl-some (lambda (p)
                                             (and (= (cdr p) pr)
                                                  (gethash (cons id (car p)) bidir-set)))
                                           oports))
                       (rh      (aref row-heights pr)))
                  (dotimes (l rh)
                    (let* ((av      (abs-vl (ivl-cont pr l)))
                           (lbl-line (and (= pr pr-min) (< l n-lbls)))
                           (inner   (if lbl-line
                                        (let* ((s   (concat " " (nth l lbls) " "))
                                               (pad (- bw 2 (length s))))
                                          (concat s (make-string (max 0 pad) ?\s)))
                                      (make-string (- bw 2) ?\s)))
                           (rch     (if (and has-out (= l 0)
                                            (not bidir-out)
                                            (not (gethash (cons id pr)
                                                          secondary-branch-pts)))
                                        te vl)))
                      (box-diagram--put cv av cx (concat vl inner rch))
                      (when (= l 0)
                        (when has-in
                          (box-diagram--put cv av (- cx 3) in-arw))
                        (when out-int
                          (box-diagram--put cv av (+ cx bw) arrow))
                        (when out-ext
                          (let* ((fill-x (+ cx bw))
                                 (fill-n (- mod-rx fill-x))
                                 (to-lbl (box-diagram--label (car out-ext) defs)))
                            (when (> fill-n 0)
                              (box-diagram--put cv av fill-x
                                (make-string fill-n (string-to-char hl))))
                            (box-diagram--put cv av (+ mod-rx 1)
                              (concat arrow " " to-lbl))))))))))

            ;; Bottom border
            (box-diagram--put cv (abs-vl (ivl-bot-r pr-max)) cx
              (concat bl (make-string (- bw 2) (string-to-char hl)) br))

            ;; Side walls between consecutive port-rows
            (when (> pr-max pr-min)
              (let ((pr pr-min))
                (while (< pr pr-max)
                  (let* ((ivl-after  (1+ (ivl-cont pr (1- (aref row-heights pr)))))
                         (ivl-before (ivl-cont (1+ pr) 0)))
                    (let ((ivl ivl-after))
                      (while (< ivl ivl-before)
                        (box-diagram--put cv (abs-vl ivl) cx           vl)
                        (box-diagram--put cv (abs-vl ivl) (+ cx bw -1) vl)
                        (setq ivl (1+ ivl)))))
                  (setq pr (1+ pr)))))))

        ;; ---- Draw outside-left entries (text -> inner box) ------------------
        (maphash
         (lambda (pr from-id)
           (let* ((lbl (box-diagram--label from-id defs))
                  (pad (make-string (- max-llbl (length lbl)) ?\s))
                  (av  (abs-vl (ivl-cont pr 0))))
             (box-diagram--put cv av 0 (concat pad lbl dashes-out))
             (box-diagram--put cv av (+ mod-lx 1) arrow)))
         oleft)

        ;; ---- Draw branch connectors (from bracket fan-out groups) -----------
        ;; For source S with branch targets [T1, T2, ...] at port-rows [pr0, pr1, ...]:
        ;; - Draw ┬ at S's output column on the first branch port-row
        ;; - Draw ├ on middle port-rows, └ on the last port-row
        ;; - Draw │ in the gap rows between consecutive branch port-rows
        (dolist (bg branch-groups)
          (let* ((source  (car bg))
                 (targets (cdr bg)))
            (when (gethash source inner-set)
              (let* ((c        (gethash source cols 0))
                     (cx       (cx-of c))
                     (bw       (aref col-widths c))
                     (bx       (+ cx bw))   ; column of the branch indicator
                     (s-oports (gethash source out-ports '()))
                     ;; port-rows for each branch target, sorted ascending
                     (branch-prs
                      (sort
                       (delq nil
                             (mapcar (lambda (tgt)
                                       (let ((p (assoc tgt s-oports)))
                                         (when p (cdr p))))
                                     targets))
                       #'<)))
                (when (>= (length branch-prs) 2)
                  (let ((n (length branch-prs)))
                    ;; Overwrite first char of arrow at each branch port-row
                    (dotimes (i n)
                      (let* ((pr (nth i branch-prs))
                             (av (abs-vl (ivl-cont pr 0)))
                             ;; ┬ = T-down, ├ = T-right for middle, └ = corner
                             (ch (cond ((= i 0)      "\u252c")
                                       ((= i (1- n)) "\u2514")
                                       (t            "\u251c"))))
                        (box-diagram--put cv av bx ch)))
                    ;; Vertical │ in canvas rows between consecutive port-rows.
                    ;; Start right after the first content row (where ┬ was drawn)
                    ;; so there is no gap in multi-height rows.
                    (let ((i 0))
                      (while (< i (1- n))
                        (let* ((pr0 (nth i       branch-prs))
                               (pr1 (nth (1+ i) branch-prs))
                               (ivl-start (1+ (ivl-cont pr0 0)))
                               (ivl-end   (ivl-cont pr1 0)))
                          (let ((ivl ivl-start))
                            (while (< ivl ivl-end)
                              (box-diagram--put cv (abs-vl ivl) bx "\u2502")
                              (setq ivl (1+ ivl)))))
                        (setq i (1+ i))))))))))

        ;; ---- Draw edge-qualified (vertical) connections ---------------------
        (dolist (entry edge-alist)
          (let* ((key    (car entry))
                 (src-id (car key))
                 (tgt-id (cdr key))
                 (src-e  (cadr entry))
                 (tgt-e  (cddr entry))
                 (r-src  (gethash src-id rows))
                 (c-src  (gethash src-id cols))
                 (r-tgt  (gethash tgt-id rows))
                 (c-tgt  (gethash tgt-id cols)))
            (when (and r-src c-src r-tgt c-tgt)
              (let* ((cx-src      (cx-of c-src))
                     (bw-src      (aref col-widths c-src))
                     (cx-tgt      (cx-of c-tgt))
                     (bw-tgt      (aref col-widths c-tgt))
                     (vc-tgt      (+ cx-tgt (/ bw-tgt 2)))
                     (vc-src      (+ cx-src (/ bw-src 2)))
                     (src-right   (+ cx-src bw-src))
                     (av-src-cont (abs-vl (ivl-cont r-src 0)))
                     ;; Use actual box extents for ▼/▲ so they land in gap rows
                     (src-pr-max  (let ((mx r-src))
                                    (dolist (p (gethash src-id in-ports  '()))
                                      (when (> (cdr p) mx) (setq mx (cdr p))))
                                    (dolist (p (gethash src-id out-ports '()))
                                      (when (> (cdr p) mx) (setq mx (cdr p))))
                                    mx))
                     (src-pr-min  (let ((mn r-src))
                                    (dolist (p (gethash src-id in-ports  '()))
                                      (when (< (cdr p) mn) (setq mn (cdr p))))
                                    (dolist (p (gethash src-id out-ports '()))
                                      (when (< (cdr p) mn) (setq mn (cdr p))))
                                    mn))
                     (tgt-pr-max  (let ((mx r-tgt))
                                    (dolist (p (gethash tgt-id in-ports  '()))
                                      (when (> (cdr p) mx) (setq mx (cdr p))))
                                    (dolist (p (gethash tgt-id out-ports '()))
                                      (when (> (cdr p) mx) (setq mx (cdr p))))
                                    mx))
                     (tgt-pr-min  (let ((mn r-tgt))
                                    (dolist (p (gethash tgt-id in-ports  '()))
                                      (when (< (cdr p) mn) (setq mn (cdr p))))
                                    (dolist (p (gethash tgt-id out-ports '()))
                                      (when (< (cdr p) mn) (setq mn (cdr p))))
                                    mn))
                     ;; av-* derived from actual pr-max/min for correct ▼/▲ placement
                     (av-tgt-top  (abs-vl (ivl-top-r tgt-pr-min)))
                     (av-tgt-bot  (abs-vl (ivl-bot-r tgt-pr-max)))
                     (av-src-top  (abs-vl (ivl-top-r src-pr-min)))
                     (av-src-bot  (abs-vl (ivl-bot-r src-pr-max))))
                (cl-flet ((hfill (av x1 x2)
                            (dotimes (k (max 0 (- x2 x1)))
                              (box-diagram--put cv av (+ x1 k) "─"))))
                  (cond
                   ;; tgt.t : signal flows DOWN from src (above) to tgt top.
                   ;;   ▼ placed in gap row just above tgt top border.
                   ((and (equal tgt-e "t") (< r-src r-tgt))
                    (box-diagram--put cv (1- av-tgt-top) vc-tgt "▼") ; ▼ in gap
                    (let ((r (1+ av-src-cont)))
                      (while (< r (1- av-tgt-top))
                        (box-diagram--put cv r vc-tgt "│") ; │
                        (setq r (1+ r))))
                    (box-diagram--put cv av-src-cont vc-tgt "┐") ; ┐
                    (hfill av-src-cont src-right vc-tgt)
                    (box-diagram--put cv av-src-cont (1- src-right) "├")) ; ├

                   ;; tgt.b : signal flows UP from src (below) to tgt bottom.
                   ;;   ▲ placed in gap row just below tgt bottom border.
                   ((and (equal tgt-e "b") (> r-src r-tgt))
                    (box-diagram--put cv (1+ av-tgt-bot) vc-tgt "▲") ; ▲ in gap
                    (let ((r (+ 2 av-tgt-bot)))
                      (while (< r av-src-cont)
                        (box-diagram--put cv r vc-tgt "│") ; │
                        (setq r (1+ r))))
                    (box-diagram--put cv av-src-cont vc-tgt "┘") ; ┘
                    (hfill av-src-cont src-right vc-tgt)
                    (box-diagram--put cv av-src-cont (1- src-right) "├")) ; ├

                   ;; src.b : signal exits src bottom, flows DOWN to tgt (below).
                   ;;   │ descends from src bottom all the way to └──► (no extra ▼).
                   ((and (equal src-e "b") (< r-src r-tgt))
                    (let ((av-tgt-cont (abs-vl (ivl-cont r-tgt 0)))
                          (tgt-entry   (- cx-tgt 3)))
                      (let ((r (1+ av-src-bot)))
                        (while (< r av-tgt-cont)
                          (box-diagram--put cv r vc-src "│") ; │
                          (setq r (1+ r))))
                      (box-diagram--put cv av-tgt-cont vc-src "└") ; └
                      (hfill av-tgt-cont (1+ vc-src) tgt-entry)
                      (box-diagram--put cv av-tgt-cont tgt-entry arrow)))

                   ;; src.t : signal exits src top, flows UP to tgt (above).
                   ;;   │ fills gap below ┌──► connection (no extra ▲).
                   ((and (equal src-e "t") (> r-src r-tgt))
                    (let ((av-tgt-cont (abs-vl (ivl-cont r-tgt 0)))
                          (tgt-entry   (- cx-tgt 3)))
                      (box-diagram--put cv av-tgt-cont vc-src "┌") ; ┌
                      (hfill av-tgt-cont (1+ vc-src) tgt-entry)
                      (box-diagram--put cv av-tgt-cont tgt-entry arrow)
                      (let ((r (1+ av-tgt-cont)))
                        (while (< r av-src-top)
                          (box-diagram--put cv r vc-src "│") ; │
                          (setq r (1+ r))))))))))))

        (box-diagram--canvas-to-lines cv)))))

;;; ---- Simple chain renderer (fallback when no double-box) ------------------

(defun box-diagram--spaces (n)
  "Return a string of N space characters."
  (make-string (max 0 n) ?\s))

(defconst box-diagram--arrow
  (concat (make-string 2 ?\u2500) "\u25ba")
  "Unidirectional arrow: ──►")

(defconst box-diagram--bidir-arrow
  (concat "\u25c4" "\u2500" "\u25ba")
  "Bidirectional arrow: ◄─►  (same width as box-diagram--arrow).")

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
     ((eq type 'r-box)
      (let* ((label (if (gethash node rendered) (concat "[" base "]") base))
             (w     (length label)))
        (puthash node t rendered)
        (list (concat "\u256d" (make-string (+ w 2) (string-to-char hl)) "\u256e")
              (concat vl " " label " " vl)
              (concat "\u2570" (make-string (+ w 2) (string-to-char hl)) "\u256f"))))
     (t (list (box-diagram--spaces (length base)) base
              (box-diagram--spaces (length base)))))))

(defun box-diagram--render-chain (chain defs rendered &optional bidir-set)
  "Render CHAIN into (TOP MID BOT) using the simple renderer.
BIDIR-SET is a hash (SRC . TGT) -> t for bidirectional edges."
  (let ((top "") (mid "") (bot "")
        (aw (length box-diagram--arrow)) (first t) (prev nil))
    (dolist (node chain)
      (unless first
        (let ((arw (if (and bidir-set
                            (gethash (cons prev node) bidir-set))
                       box-diagram--bidir-arrow
                     box-diagram--arrow)))
          (setq top (concat top (box-diagram--spaces aw)))
          (setq mid (concat mid arw))
          (setq bot (concat bot (box-diagram--spaces aw)))))
      (setq first nil)
      (setq prev node)
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
        (if (string-match-p "^[ \t]*$"
              (replace-regexp-in-string "##.*$" ""
                (box-diagram--strip-line-prefix line)))
            (setq in-conn t)
          (push line def-lines))))
    (let* ((def-text  (mapconcat #'identity (nreverse def-lines)  "\n"))
           (conn-text (mapconcat #'identity (nreverse conn-lines) "\n"))
           (defs      (box-diagram--parse-defs def-text))
           (outer-box nil)
           (outer-id  nil))
      ;; Detect optional bare double-box ID on first non-blank connection line.
      ;; A trailing semicolon is accepted (e.g. "F;" is treated like "F").
      ;; ## comments are stripped before the check.
      (let ((first-nl nil))
        (dolist (l (split-string conn-text "\n"))
          (let ((l (replace-regexp-in-string "##.*$" ""
                     (box-diagram--strip-line-prefix l))))
            (when (and (not first-nl) (> (length (string-trim l)) 0))
              (setq first-nl l))))
        (when (and first-nl
                   (string-match "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*;?[ \t]*$" first-nl))
          (let* ((id  (match-string 1 first-nl))
                 (def (cdr (assoc id defs))))
            (when (and def (eq (plist-get def :type) 'double-box))
              (setq outer-box def)
              (setq outer-id  id)
              ;; Remove the bare-ID line from conn-text.
              ;; Strip line prefix, ## comment, and trailing ; before comparing.
              (let ((removed nil) (new '()))
                (dolist (l (split-string conn-text "\n"))
                  (if (and (not removed)
                           (string= (replace-regexp-in-string
                                     ";[ \t]*$" ""
                                     (string-trim
                                      (replace-regexp-in-string "##.*$" ""
                                        (box-diagram--strip-line-prefix l))))
                                    id))
                      (setq removed t)
                    (push l new)))
                (setq conn-text (mapconcat #'identity (nreverse new) "\n")))))))
      ;; Parse chains (may contain outer-id for module-level connections)
      (let* ((parsed        (box-diagram--parse-chains conn-text))
             (all-chains    (nth 0 parsed))
             (branch-groups (nth 1 parsed))
             (edge-alist    (nth 2 parsed))
             (bidir-set     (nth 3 parsed)))
        (if outer-box
            ;; Split chains: those touching outer-id go to mod-ins/mod-outs;
            ;; the rest are inner chains for graph layout.
            (let (inner-chains mod-ins mod-outs)
              (dolist (chain all-chains)
                (let ((fidx (cl-position outer-id chain :test #'equal)))
                  (if fidx
                      ;; Chain touches the module box
                      (progn
                        (when (> fidx 0)
                          (push (nth (1- fidx) chain) mod-ins))
                        (when (< fidx (1- (length chain)))
                          (push (nth (1+ fidx) chain) mod-outs)))
                    ;; Regular inner chain
                    (push chain inner-chains))))
              (box-diagram--render-graph
               (nreverse inner-chains) defs outer-box
               (nreverse mod-ins) (nreverse mod-outs) branch-groups edge-alist bidir-set))
          ;; Simple chain-by-chain fallback (no double-box)
          (let ((rendered (make-hash-table :test 'equal)) (output '()))
            (dolist (chain all-chains)
              (when output (push "" output))
              (let ((trip (box-diagram--render-chain chain defs rendered bidir-set)))
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
