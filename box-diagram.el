;;
;; box-diagram.el - Draw Unicode block diagrams from text input
;;
;; Syntax:
;;
;;   -- Definition section (before the first blank line) --
;;
;;   ID := box("label")
;;       Draw a single-border box labelled 'label'.
;;
;;   ID := double-box("label", ID1, ID2, ...)
;;       Draw a double-border container.  The listed IDs are the contained
;;       elements (used for documentation only; the layout is driven by the
;;       connection section).
;;
;;   ID := text("label")
;;       A plain text label rendered inline (no box drawn around it).
;;
;;   -- Connection section (after the first blank line) --
;;
;;   Each non-blank line defines one independent connection chain:
;;
;;     ID1 -> ID2 -> ID3
;;
;;   Multiple chains can appear on a single line, separated by semicolons:
;;
;;     ID1 -> ID2 ; ID3 -> ID4
;;
;;   Unicode right-arrow (→) may be used instead of ->.
;;
;;   Optionally, the very first non-blank line of the connection section may
;;   be a bare identifier that names a double-box container.  When present,
;;   the rendered chains are wrapped inside that container:
;;
;;     F
;;     I1 -> A -> B -> C -> O1
;;     I2 -> D -> B
;;     C  -> E -> O2
;;
;;   A node that already appeared in an earlier chain is rendered as [label]
;;   to show it is a back-reference rather than a fresh element.
;;
;; Usage:
;;   M-x box-diagram-render        - render the current buffer
;;   M-x box-diagram-render-region - render the selected region

(defconst box-diagram--arrow "──►"
  "Arrow string drawn between connected nodes.")

(defun box-diagram--spaces (n)
  "Return a string of N space characters."
  (make-string (max 0 n) ?\s))

;;; ---- Parsers ---------------------------------------------------------------

(defun box-diagram--parse-defs (text)
  "Parse definition lines in TEXT and return an alist of (ID . PLIST).
Each plist has :type (one of box, double-box, text) and :label."
  (let (defs)
    (dolist (line (split-string text "\n"))
      (cond
       ;; box("label")
       ((string-match
         "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*:=[ \t]*box(\"\\([^\"]+\\)\")" line)
        (push (cons (match-string 1 line)
                    (list :type 'box :label (match-string 2 line)))
              defs))
       ;; text("label")
       ((string-match
         "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*:=[ \t]*text(\"\\([^\"]+\\)\")" line)
        (push (cons (match-string 1 line)
                    (list :type 'text :label (match-string 2 line)))
              defs))
       ;; double-box("label", ID1, ID2, ...)
       ((string-match
         "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*:=[ \t]*double-box(\"\\([^\"]+\\)\"\\(.*\\))"
         line)
        (let* ((id    (match-string 1 line))
               (label (match-string 2 line))
               (rest  (match-string 3 line))
               (children
                (when (string-match ",\\(.*\\)" rest)
                  (delq nil
                        (mapcar (lambda (s)
                                  (let ((s (string-trim s)))
                                    (when (string-match "^[A-Za-z0-9_]+$" s) s)))
                                (split-string (match-string 1 rest) ","))))))
          (push (cons id (list :type 'double-box :label label :children children))
                defs)))))
    (nreverse defs)))

(defun box-diagram--parse-chains (text)
  "Parse connection chains from TEXT.
Each non-blank line with -> (or →) is treated as one or more chains.
Multiple chains on one line may be separated by semicolons.
Returns a list of chains; each chain is a list of ID strings."
  (let (chains)
    (dolist (line (split-string text "\n"))
      (let ((trimmed (string-trim line)))
        (when (and (> (length trimmed) 0)
                   (string-match-p "->\\|→" trimmed)
                   (not (string-match-p ":=" trimmed)))
          ;; A semicolon separates independent chains on the same line
          (dolist (seg (split-string trimmed ";"))
            (let* ((seg   (string-trim seg))
                   (nodes (and (string-match-p "->\\|→" seg)
                               (mapcar #'string-trim
                                       (split-string seg
                                                     "[ \t]*\\(->\\|→\\)[ \t]*"
                                                     t)))))
              (when (and nodes (cdr nodes))   ; need at least two nodes
                (push nodes chains)))))))
    (nreverse chains)))

;;; ---- Rendering helpers -----------------------------------------------------

(defun box-diagram--node-triplet (node defs rendered)
  "Return (TOP MID BOT) strings for NODE.
DEFS is an alist (id . plist).
RENDERED is a hash-table of already-rendered node IDs (box nodes only).
Rendering a box node marks it in RENDERED so later occurrences show as [label]."
  (let* ((def       (cdr (assoc node defs)))
         (type      (and def (plist-get def :type)))
         (base-label (cond
                      ((and def (plist-get def :label)) (plist-get def :label))
                      (t node))))
    (cond
     ;; text node or unknown identifier: appears only on the middle row
     ((or (null def) (eq type 'text))
      (list (box-diagram--spaces (length base-label))
            base-label
            (box-diagram--spaces (length base-label))))

     ;; box node
     ((eq type 'box)
      (let* ((label (if (gethash node rendered)
                        (concat "[" base-label "]")
                      base-label))
             (w   (length label))
             (top (concat "┌" (make-string (+ w 2) ?─) "┐"))
             (mid (concat "│ " label " │"))
             (bot (concat "└" (make-string (+ w 2) ?─) "┘")))
        (puthash node t rendered)
        (list top mid bot)))

     ;; double-box or anything else: render like text
     (t
      (list (box-diagram--spaces (length base-label))
            base-label
            (box-diagram--spaces (length base-label)))))))

(defun box-diagram--render-chain (chain defs rendered)
  "Render CHAIN (list of node ID strings) into a (TOP MID BOT) string triple."
  (let ((top "")
        (mid "")
        (bot "")
        (aw  (length box-diagram--arrow))
        (first t))
    (dolist (node chain)
      (unless first
        (setq top (concat top (box-diagram--spaces aw)))
        (setq mid (concat mid box-diagram--arrow))
        (setq bot (concat bot (box-diagram--spaces aw))))
      (setq first nil)
      (let ((triplet (box-diagram--node-triplet node defs rendered)))
        (setq top (concat top (nth 0 triplet)))
        (setq mid (concat mid (nth 1 triplet)))
        (setq bot (concat bot (nth 2 triplet)))))
    (list top mid bot)))

(defun box-diagram--wrap-double-box (title lines)
  "Wrap LINES (list of strings) in a double-border box headed by TITLE.
Returns a new list of strings."
  (let* ((content-w  (apply #'max (cons 0 (mapcar #'length lines))))
         (title-len  (length title))
         (w          (max content-w title-len))
         (pad-left   (/ (- w title-len) 2))
         (pad-right  (- w title-len pad-left))
         (top-line   (concat "╔" (make-string (+ w 4) ?═) "╗"))
         (bot-line   (concat "╚" (make-string (+ w 4) ?═) "╝"))
         (sep-line   (concat "╠" (make-string (+ w 4) ?═) "╣"))
         (title-line (concat "║  "
                             (box-diagram--spaces pad-left)
                             title
                             (box-diagram--spaces pad-right)
                             "  ║"))
         result)
    (push top-line  result)
    (push title-line result)
    (push sep-line  result)
    (dolist (line lines)
      (push (concat "║  "
                    line
                    (box-diagram--spaces (- w (length line)))
                    "  ║")
            result))
    (push bot-line result)
    (nreverse result)))

;;; ---- Top-level renderer ----------------------------------------------------

(defun box-diagram--render (text)
  "Parse TEXT and return a list of strings forming the rendered diagram."
  ;; Split at first blank line: definitions above, connections below
  (let* ((all-lines  (split-string text "\n"))
         (def-lines  '())
         (conn-lines '())
         (in-conn    nil))
    (dolist (line all-lines)
      (if in-conn
          (push line conn-lines)
        (if (string-match-p "^[ \t]*$" line)
            (setq in-conn t)
          (push line def-lines))))

    (let* ((def-text  (mapconcat #'identity (nreverse def-lines)  "\n"))
           (conn-text (mapconcat #'identity (nreverse conn-lines) "\n"))
           (defs      (box-diagram--parse-defs def-text))
           (rendered  (make-hash-table :test 'equal))
           (outer-box nil))

      ;; Detect an optional outer double-box reference on the first non-blank
      ;; connection line (a bare identifier with no -> on the line).
      (let ((first-line nil))
        (dolist (l (split-string conn-text "\n"))
          (when (and (not first-line) (> (length (string-trim l)) 0))
            (setq first-line l)))
        (when (and first-line
                   (string-match "^[ \t]*\\([A-Za-z0-9_]+\\)[ \t]*$" first-line))
          (let* ((id  (match-string 1 first-line))
                 (def (cdr (assoc id defs))))
            (when (and def (eq (plist-get def :type) 'double-box))
              (setq outer-box def)
              ;; Remove that line from conn-text
              (let ((removed nil)
                    (new-lines '()))
                (dolist (l (split-string conn-text "\n"))
                  (if (and (not removed)
                           (string= (string-trim l) id))
                      (setq removed t)
                    (push l new-lines)))
                (setq conn-text
                      (mapconcat #'identity (nreverse new-lines) "\n")))))))

      ;; Render each chain into three-line groups
      (let ((output '()))
        (dolist (chain (box-diagram--parse-chains conn-text))
          (when output
            (push "" output))         ; blank separator between chains
          (let ((triplet (box-diagram--render-chain chain defs rendered)))
            (push (nth 0 triplet) output)
            (push (nth 1 triplet) output)
            (push (nth 2 triplet) output)))

        (let ((result (nreverse output)))
          (if outer-box
              (box-diagram--wrap-double-box (plist-get outer-box :label) result)
            result))))))

;;; ---- Interactive commands --------------------------------------------------

;;;###autoload
(defun box-diagram-render ()
  "Parse the current buffer as a box diagram definition and display the result."
  (interactive)
  (let* ((text   (buffer-substring-no-properties (point-min) (point-max)))
         (output (mapconcat #'identity (box-diagram--render text) "\n")))
    (with-current-buffer (get-buffer-create "*Box Diagram*")
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (display-buffer "*Box Diagram*")))

;;;###autoload
(defun box-diagram-render-region (start end)
  "Parse the selected region as a box diagram definition and display the result."
  (interactive "r")
  (let* ((text   (buffer-substring-no-properties start end))
         (output (mapconcat #'identity (box-diagram--render text) "\n")))
    (with-current-buffer (get-buffer-create "*Box Diagram*")
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (display-buffer "*Box Diagram*")))

(provide 'box-diagram)
