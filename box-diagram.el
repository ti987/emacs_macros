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
;;   The very first non-blank line of the connection section may be a bare
;;   identifier naming a double-box container.  The graph-based renderer is
;;   then used: each box appears exactly once; text nodes appear outside the
;;   module border.
;;
;;   The double-box ID may also appear in chains for module-level connections:
;;
;;     I3 -> F -> O3       (arrow enters/exits the module border)
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
                    (list :type 'box :label
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
          (push (cons id (list :type 'double-box :label label :children kids)) defs)))))
    (nreverse defs)))

(defun box-diagram--parse-chains (text)
  "Return (CHAINS . BRANCH-GROUPS) from TEXT.
CHAINS is a list of chains (lists of ID strings).  A final [A, B, C] segment
is expanded into one chain per target.
BRANCH-GROUPS is an alist (SOURCE-ID . (TARGET-ID ...)) recording each
bracket fan-out group so the renderer can draw branch connectors."
  (let (chains branch-groups)
    (dolist (line (split-string text "\n"))
      (let ((trimmed (string-trim line)))
        (when (and (> (length trimmed) 0)
                   (string-match-p "->\\|\u2192" trimmed)
                   (not (string-match-p ":=" trimmed)))
          (dolist (seg (split-string trimmed ";"))
            (let ((seg (string-trim seg)))
              (when (string-match-p "->\\|\u2192" seg)
                (let* ((parts (mapcar #'string-trim
                                      (split-string seg
                                                    "[ \t]*\\(->\\|\u2192\\)[ \t]*" t))))
                  (when (cdr parts)
                    (let* ((last-part (car (last parts)))
                           (prefix    (butlast parts)))
                      (if (string-match
                           "^\\[[ \t]*\\([^]]+\\)[ \t]*\\]$" last-part)
                          ;; Bracket group: fan out from last prefix node
                          (let* ((inner   (match-string 1 last-part))
                                 (targets (mapcar #'string-trim
                                                  (split-string inner ",")))
                                 (source  (car (last prefix))))
                            ;; Record branch group for visual rendering
                            (when source
                              (let ((existing (assoc source branch-groups)))
                                (if existing
                                    (setcdr existing
                                            (cl-union (cdr existing) targets
                                                      :test #'equal))
                                  (push (cons source targets) branch-groups))))
                            ;; Expand: one chain per target
                            (dolist (tgt targets)
                              (let ((chain (append prefix (list tgt))))
                                (when (cdr chain)
                                  (push chain chains)))))
                        ;; Normal chain
                        (push parts chains)))))))))))
    (cons (nreverse chains) branch-groups)))

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

(defun box-diagram--render-graph (chains defs outer-def mod-ins mod-outs branch-groups)
  "Render diagram using graph-based layout.
CHAINS/DEFS describe the inner topology.
OUTER-DEF is the double-box plist (:label :children).
MOD-INS / MOD-OUTS are lists of IDs for module-level border connections.
BRANCH-GROUPS is alist (SOURCE . (TARGET ...)) for bracket fan-out groups.
Returns a list of strings."
  (let* ((inner-ids (plist-get outer-def :children))
         (title     (plist-get outer-def :label))
         (inner-set (let ((h (make-hash-table :test 'equal)))
                      (dolist (id inner-ids) (puthash id t h)) h))
         (rows (box-diagram--assign-rows chains))
         (cols (box-diagram--assign-cols chains inner-set))
         (ports (box-diagram--collect-ports chains rows inner-set))
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
          (let ((seen (make-hash-table :test 'eql)))
            (maphash (lambda (_ v) (puthash v t seen)) rows)
            (max 1 (hash-table-count seen))))
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
                 (pr (max (gethash f rows 0) (gethash t_ rows 0))))
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
           ;; ivl-base[r]: first inside-VL for grid row r
           ;; (offset by n-mod-ports so module port rows come first)
           (ivl-base
            (let ((v (make-vector num-rows 0)) (acc n-mod-ports))
              (dotimes (r num-rows)
                (aset v r acc)
                (setq acc (+ acc (aref row-heights r) 2)))
              v))
           (n-in-vls
            (+ n-mod-ports
               (let ((acc 0))
                 (dotimes (r num-rows) (setq acc (+ acc (aref row-heights r) 2)))
                 acc)))
           (n-vls    (+ n-in-vls 2))
           (canvas-w (+ left-w 1 inside-w 1 40))
           (cv       (box-diagram--canvas n-vls canvas-w))
           (mod-lx   left-w)
           (mod-rx   (+ mod-lx 1 inside-w))
           (arrow    (concat (make-string 2 ?\u2500) "\u25ba")))

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
        ;; Pattern (mod-in): "label ────────║──►" entering left wall
        ;; Pattern (mod-out): "─────────────║──► label" exiting right wall
        (dotimes (i n-mod-ports)
          (let ((av (abs-vl i)))
            ;; Left: module input
            (when (< i (length mod-ins))
              (let* ((lbl (box-diagram--label (nth i mod-ins) defs))
                     (pad (make-string (- max-llbl (length lbl)) ?\s)))
                (box-diagram--put cv av 0 (concat pad lbl dashes-out))
                (box-diagram--put cv av (+ mod-lx 1) arrow)))
            ;; Right: module output
            (when (< i (length mod-outs))
              (let* ((lbl       (box-diagram--label (nth i mod-outs) defs))
                     (has-input (< i (length mod-ins)))
                     ;; Start fill after the --> if there is also a mod-in
                     (fill-x   (+ mod-lx (if has-input 4 1)))
                     (fill-n   (- mod-rx fill-x)))
                (when (> fill-n 0)
                  (box-diagram--put cv av fill-x
                    (make-string fill-n ?\u2500)))
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
                 (tl "\u250c") (tr "\u2510")
                 (bl "\u2514") (hl "\u2500")
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
                       (rh      (aref row-heights pr)))
                  (dotimes (l rh)
                    (let* ((av      (abs-vl (ivl-cont pr l)))
                           (lbl-line (and (= pr pr-min) (< l n-lbls)))
                           (inner   (if lbl-line
                                        (let* ((s   (concat " " (nth l lbls) " "))
                                               (pad (- bw 2 (length s))))
                                          (concat s (make-string (max 0 pad) ?\s)))
                                      (make-string (- bw 2) ?\s)))
                           (rch     (if (and has-out (= l 0)) te vl)))
                      (box-diagram--put cv av cx (concat vl inner rch))
                      (when (= l 0)
                        (when has-in
                          (box-diagram--put cv av (- cx 3) arrow))
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
              (concat bl (make-string (- bw 2) (string-to-char hl)) "\u2518"))

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
                    ;; Vertical │ in canvas rows between consecutive port-rows
                    (let ((i 0))
                      (while (< i (1- n))
                        (let* ((pr0 (nth i       branch-prs))
                               (pr1 (nth (1+ i) branch-prs))
                               (ivl-start (1+ (ivl-cont pr0
                                                        (1- (aref row-heights pr0)))))
                               (ivl-end   (ivl-cont pr1 0)))
                          (let ((ivl ivl-start))
                            (while (< ivl ivl-end)
                              (box-diagram--put cv (abs-vl ivl) bx "\u2502")
                              (setq ivl (1+ ivl)))))
                        (setq i (1+ i))))))))))

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
           (outer-box nil)
           (outer-id  nil))
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
              (setq outer-id  id)
              ;; Remove the bare-ID line from conn-text
              (let ((removed nil) (new '()))
                (dolist (l (split-string conn-text "\n"))
                  (if (and (not removed) (string= (string-trim l) id))
                      (setq removed t)
                    (push l new)))
                (setq conn-text (mapconcat #'identity (nreverse new) "\n")))))))
      ;; Parse chains (may contain outer-id for module-level connections)
      (let* ((parsed       (box-diagram--parse-chains conn-text))
             (all-chains   (car parsed))
             (branch-groups (cdr parsed)))
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
               (nreverse mod-ins) (nreverse mod-outs) branch-groups))
          ;; Simple chain-by-chain fallback (no double-box)
          (let ((rendered (make-hash-table :test 'equal)) (output '()))
            (dolist (chain all-chains)
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
