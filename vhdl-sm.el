;;; vhdl-sm.el --- VHDL State Machine Analysis
;; T. Isogai 2025
;;
;; Find state machines in a VHDL buffer and write a summary into the
;; file's head comment block.
;;
;; Algorithm (simplified):
;;   1. Scan the buffer for `process' keywords.
;;   2. Within each process, search for `case SIGNAL is' statements.
;;   3. If SIGNAL matches the state-signal regexp the process contains a
;;      state machine.  Collect all `when STATE =>' clauses (excluding
;;      `others') and all state transitions (assignments `<= KNOWN_STATE'
;;      within each `when' block).
;;   4. Write (or refresh) a `-- %DSL_START' ... `-- %DSL_END' block at
;;      the end of the file's leading comment section.
;;
;; Output format (state-machine DSL; see §5 grammar above):
;;   Node declarations:  -- node STATE "STATE\nLINE"
;;   Transition arrows:  -- FROM -> TO "LINE"
;;
;; Usage:
;;   M-x extract-state-machines   (run in a VHDL buffer)
;;
;; Configuration:
;;   vhdl-sm-state-regexp  - regexp that identifies state-signal names

;; 5. DSL Grammar Reference
;; 5.1 Lexical conventions
;; Element      Pattern / description
;; Identifier (NAME)    [A-Za-z_][A-Za-z0-9_]* — e.g. A, Start, proc_1
;; String literal       "…" — double-quoted; supports \n for a newline inside the label
;; Comment      # to end of line — everything after # is ignored
;; Statement separator  Newline or ; — multiple statements may appear on one line
;; Whitespace   Spaces and tabs are ignored except inside string literals
;; 5.2 Node declarations
;; node NAME
;; node NAME "Label"
;; node NAME "Label" PLACEMENT
;; node NAME PLACEMENT
;; A node statement defines a named box.
;; 
;; Part Description
;; NAME Identifier for the node; also used as the default label if "Label" is omitted.
;; "Label"      Text displayed inside the box. Supports \n for multi-line content.
;; PLACEMENT    Optional position relative to another node (see below).
;; Placement options:
;; 
;; Syntax       Meaning
;; (omitted)    Place to the right of the previously declared node, on the same row. The first node in the file starts at the default position.
;; right of REF Same row as REF, one column step to the right.
;; left of REF  Same row as REF, one column step to the left.
;; below REF    Same column as REF, one row step below.
;; above REF    Same column as REF, one row step above.
;; below and right of REF       One row step below and one column step to the right of REF.
;; below and left of REF        One row step below and one column step to the left of REF.
;; above and right of REF       One row step above and one column step to the right of REF.
;; above and left of REF        One row step above and one column step to the left of REF.
;; at X Y       Absolute grid coordinates (integers). Used by Save DSL with positions.
;; REF must be the NAME of a node that has already been declared in the same file.
;; 
;; Implicit nodes: a node name used only in edge statements (never declared with node) is auto-created with its name as label and placed automatically on the default row.
;; 
;; Examples:
;; 
;; node Start "Start"
;; node Process "Process" right of Start
;; node End "End" right of Process
;; node Monitor "Monitor" above Process
;; node Audit "Audit" below and right of Start
;; node Hidden                          # no label → uses name "Hidden"
;; 5.3 Container declarations
;; Webapp only: container rendering and connections to/from containers are supported in the web application. The CLI parses container syntax without errors but ignores containers during layout and routing.
;; 
;; A container groups a set of nodes (or other containers) inside a labelled border.
;; 
;; container NAME
;; container NAME "Label"
;; container NAME "Label" PLACEMENT
;; The syntax is identical to a node declaration. The PLACEMENT is optional and informational (the actual border is computed from the bounding box of member nodes; see §5.4).
;; 
;; Connections can target a container just like a node — the routing will approach the container's border from outside.
;; 
;; Constraints:
;; 
;; Containers must be completely inside or completely outside each other (no partial overlaps).
;; Nodes must be completely inside or completely outside each container.
;; 5.4 Container member assignment
;; Webapp only: container member assignment is parsed by the CLI but has no effect on CLI output.
;; 
;; Members are assigned to a container in a separate statement:
;; 
;; NAME := [member1, member2, ...]
;; NAME is the container identifier. Each member may be a node name or another container name (nested containers are supported). The := statement may appear before or after the container declaration.
;; 
;; Example:
;; 
;; node B "Node B" below A
;; node D "Node D" right of B
;; container C1 "Backend"
;; C1 := [B, D]
;; 5.5 Edge (connection) declarations
;; FROM -> TO
;; FROM -> TO "label"
;; FROM <-> TO
;; FROM <-> TO "label"
;; Part Description
;; FROM Source node or container name, optionally with a port specification.
;; TO   Destination node or container name, optionally with a port specification.
;; ->   Directed connection (arrowhead at TO).
;; <->  Bidirectional connection (arrowheads at both ends).
;; "label"      Optional text label drawn alongside the connection.
;; Port specifications are appended directly to the node name (no spaces):
;; 
;; FROM[.side[.N]] -> TO[.side[.N]] ["label" [at X Y]]
;; 5.6 Port side notation
;; By default the router exits a node from its right side and enters from its left side. You can override this with a dot (.) or colon (:) separator followed by a side keyword:
;; 
;; Short form   Full form       Side
;; .t   .top    Top edge
;; .b   .bottom Bottom edge
;; .l   .left   Left edge
;; .r   .right  Right edge
;; Both separators (. and :) are accepted for compatibility with legacy DSL files.
;; 
;; Examples:
;; 
;; A.b  -> B.t          # A bottom → B top
;; A.r  -> B.l          # A right → B left (same as default)
;; A.top -> B.bottom    # full names also work
;; A:right -> B:left    # colon separator (legacy)
;; 5.7 Port number selection
;; When a node has multiple ports on the same side, you can select a specific one with a 1-based index appended after the side:
;; 
;; NAME.SIDE.N
;; Numbering order:
;; 
;; Left/Right sides: port 1 is the topmost, port 2 the next one down, etc.
;; Top/Bottom sides: port 1 is the leftmost, port 2 the next one to the right, etc.
;; If N exceeds the number of available ports on that side, the tool reports an error.
;; 
;; Examples:
;; 
;; A.l.1 -> B.r.1      # first left port of A → first right port of B
;; A.t.2 -> B.b.1      # second top port of A → first bottom port of B
;; 5.8 Edge labels
;; A quoted string at the end of an edge statement is rendered as a label next to the connection:
;; 
;; A -> B "step 1"
;; A.b -> C.t "step 2"
;; Explicit label position: append at X Y after the label string to pin the label to absolute grid coordinates, overriding the automatic placement:
;; 
;; A -> B "step 1" at 20 5
;; The at X Y values are integers in grid-cell units. The webapp stores these coordinates automatically when you drag a label interactively, and Save DSL with positions serialises them.
;; 
;; 5.9 Chained edges
;; Multiple nodes can be connected in a single statement:
;; 
;; A -> B -> C
;; A.r -> B.b -> C.t
;; This is equivalent to separate edge statements:
;; 
;; A -> B
;; B -> C
;; A label at the end of a chained statement is attached only to the last segment:
;; 
;; A -> B -> C "final step"
;; # equivalent to:
;; # A -> B
;; # B -> C "final step"
;; 5.10 Bidirectional connections
;; Use <-> instead of -> to draw arrowheads at both ends:
;; 
;; Client <-> Server "RPC"
;; 5.11 Multiline labels
;; Use the escape sequence \n inside a double-quoted string to produce a multi-line label:
;; 
;; node Src "Source\nNode"
;; This renders the box with two lines of text:
;; 
;; ┌───────┐
;; │Source │
;; │Node   │
;; └───────┘
;; 5.12 Complete grammar summary
;; diagram       ::= statement*
;; statement     ::= node_stmt
;;                 | container_stmt
;;                 | members_stmt
;;                 | edge_stmt
;;                 | ""                       # blank line
;; 
;; node_stmt     ::= "node" NAME string? placement?
;; container_stmt::= "container" NAME string? placement?
;; members_stmt  ::= NAME ":=" "[" (NAME ("," NAME)*)? "]"
;; 
;; placement     ::= "right" "of" NAME
;;                 | "left" "of" NAME
;;                 | "below" NAME
;;                 | "above" NAME
;;                 | "below" "and" "right" "of" NAME
;;                 | "below" "and" "left"  "of" NAME
;;                 | "above" "and" "right" "of" NAME
;;                 | "above" "and" "left"  "of" NAME
;;                 | "at" INT INT
;; 
;; edge_stmt     ::= endpoint ("->" | "<->") endpoint
;;                       (("->" | "<->") endpoint)*
;;                       (string ("at" INT INT)?)?
;; 
;; endpoint      ::= NAME
;;                 | NAME sep SIDE
;;                 | NAME sep SIDE sep PORTNUM
;; 
;; sep           ::= "." | ":"
;; SIDE          ::= "t" | "b" | "l" | "r"
;;                 | "top" | "bottom" | "left" | "right"
;; PORTNUM       ::= [1-9][0-9]*
;; 
;; string        ::= '"' [^"]* '"'        # supports \n escape
;; NAME          ::= [A-Za-z_][A-Za-z0-9_]*
;; INT           ::= -?[0-9]+
;; Lines may contain multiple statements separated by ;. A # character begins a comment that extends to the end of the line.
;; 

(require 'cl-lib)

;;; ---------------------------------------------------------------------------
;;; User-configurable variable
;;; ---------------------------------------------------------------------------

(defvar vhdl-sm-state-regexp "\\w+state\\>"
  "Regexp matched against a VHDL `case' control variable to decide whether
the enclosing process is a state machine.
The default \"\\\\w+state\\\\>\" matches identifiers that end with the word
\"state\", such as `cur_state', `next_state', `main_state', etc.")

;;; ---------------------------------------------------------------------------
;;; Public entry point
;;; ---------------------------------------------------------------------------

(defun extract-state-machines (&optional state-regexp)
  "Find state machines in the current VHDL buffer and annotate the file header.
STATE-REGEXP is a regexp matched against `case' control variables to
identify state machines (default: `vhdl-sm-state-regexp').

For each process that contains a `case SIGNAL is' statement where SIGNAL
matches STATE-REGEXP, the process label (if any), the state-signal name,
the line number, and the list of enumerated states are collected.

The result is written as a comment block at the end of the file's leading
comment section.  The block is delimited by:
  -- %DSL_START extract-state-machines
  ...
  -- %DSL_END
If such a block already exists it is replaced in-place."
  (interactive)
  (let* ((regexp  (or state-regexp vhdl-sm-state-regexp))
         (sm-list (vhdl-sm--scan-buffer regexp))
         (output  (vhdl-sm--format-output
                   sm-list
                   (or (buffer-file-name) (buffer-name)))))
    (vhdl-sm--write-head-comment output)
    (message "extract-state-machines: %d state machine(s) found."
             (length sm-list))))

;;; ---------------------------------------------------------------------------
;;; Buffer scanner
;;; ---------------------------------------------------------------------------

(defun vhdl-sm--scan-buffer (state-regexp)
  "Scan the current VHDL buffer for state machines.
Returns a list of plists.  Each plist has the keys:
  :process-label  - label of the enclosing process, or nil
  :process-line   - line number of the `process' keyword
  :sm-signal      - name of the `case' control variable
  :sm-line        - line number of the `case' statement
  :states         - list of (STATE-ID LINE-NUMBER) pairs
  :transitions    - list of (FROM-STATE TO-STATE LINE-NUMBER) triples (deduplicated)"
  (let ((results '())
        (eof (point-max)))
    (save-excursion
      (goto-char (point-min))
      ;; Match `process' keyword at the start of a line (after optional whitespace
      ;; and optional LABEL :).  The `^[ \t]*' anchor prevents matching occurrences
      ;; of the word "process" that appear inside VHDL comments.
      (while (re-search-forward
              "^[ \t]*\\(?:\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t\n]*:[ \t\n]*\\)?\\<process\\>"
              eof t)
        (let* ((proc-label (match-string-no-properties 1))
               (proc-line  (line-number-at-pos (match-beginning 0)))
               (proc-start (point))
               ;; Locate the matching `end process'
               (proc-end
                (save-excursion
                  (when (re-search-forward
                         "^[ \t]*\\<end\\>[ \t\n]+\\<process\\>"
                         eof t)
                    (point)))))
          (when proc-end
            ;; Look for `case SIGNAL is' within this process
            (save-excursion
              (goto-char proc-start)
              (while (re-search-forward
                      (concat "^[ \t]*\\<case\\>[ \t\n]+"
                              "\\([a-zA-Z][a-zA-Z0-9_.]*\\)"
                              "[ \t\n]+\\<is\\>")
                      proc-end t)
                (let* ((case-var       (match-string-no-properties 1))
                       (case-line      (line-number-at-pos (match-beginning 0)))
                       (case-body-start (point)))  ; position right after "case X is"
                  (when (vhdl-sm--match-regexp state-regexp case-var)
                    ;; Collect `when STATE =>' clauses at nesting depth 1
                    (let ((states '())
                          (nest 1))
                      (while (and (zerop (forward-line))
                                  (> nest 0)
                                  (< (point) proc-end))
                        (cond
                         ((looking-at "^[ \t]*\\<case\\>[ \t]")
                          (setq nest (1+ nest)))
                         ((looking-at "^[ \t]*\\<end\\>[ \t]+\\<case\\>")
                          (setq nest (1- nest)))
                         ((and (= nest 1)
                               (looking-at
                                (concat "^[ \t]*\\<when\\>[ \t]+"
                                        "\\([a-zA-Z][a-zA-Z0-9_]*\\)"
                                        "[ \t]*=>")))
                          (let ((state-id   (match-string-no-properties 1))
                                (state-line (line-number-at-pos (point))))
                            (unless (string-equal (downcase state-id) "others")
                              (push (list state-id state-line) states))))))
                      ;; Scan transitions using the collected state IDs
                      (let* ((state-ids  (mapcar #'car states))
                             (transitions
                              (save-excursion
                                (goto-char case-body-start)
                                (vhdl-sm--collect-transitions
                                 state-ids proc-end))))
                        (push (list :process-label proc-label
                                    :process-line  proc-line
                                    :sm-signal     case-var
                                    :sm-line       case-line
                                    :states        (nreverse states)
                                    :transitions   transitions)
                              results)))))))))))
    (nreverse results)))

;;; ---------------------------------------------------------------------------
;;; Output formatter
;;; ---------------------------------------------------------------------------

(defun vhdl-sm--format-output (sm-list buf-name)
  "Format SM-LIST as a state-machine DSL comment block string.
BUF-NAME is used in the block header for identification.

The output follows the DSL grammar defined in this file's head comment (§5):
  Node declarations:  -- node STATE_NAME \"STATE_ID\\nLINE\"
  Transition arrows:  -- FROM -> TO \"LINE\"
The leading `--' prefix is stripped by the DSL renderer; `#' begins a
comment in the DSL, so section headers are written as `-- # ...'."
  (let ((lines
         (list
          "-- %DSL_START "
          (concat "-- # Generated: " (format-time-string "%Y-%m-%d %H:%M:%S %Z"))
          (concat "-- # File: " (file-name-nondirectory buf-name))
          "--")))
    (if (null sm-list)
        (setq lines (append lines (list "-- No state machines found." "--")))
      (let ((i 1))
        (dolist (sm sm-list)
          (let* ((label       (plist-get sm :process-label))
                 (pline       (plist-get sm :process-line))
                 (sig         (plist-get sm :sm-signal))
                 (sline       (plist-get sm :sm-line))
                 (states      (plist-get sm :states))
                 (transitions (plist-get sm :transitions))
                 (proc-str
                  (if label
                      (format "%s (line %d)" label pline)
                    (format "unnamed process (line %d)" pline))))
            ;; Section header (# is the DSL comment marker per §5.1)
            (setq lines
                  (append lines
                          (list (format
                                 "-- # %d. state signal: %s  [process: %s, case line: %d]"
                                 i sig proc-str sline))))
            ;; Node declarations: node STATE_NAME "STATE_ID\nLINE"  (§5.2)
            (dolist (s states)
              (let* ((state-id   (car s))
                     (state-line (cadr s))
                     (label-str  (format "%s\\n%d" state-id state-line)))
                (setq lines
                      (append lines
                              (list (format "-- node %s \"%s\""
                                            state-id label-str))))))
            ;; Blank separator (box-diagram: lone -- acts as section separator)
            (setq lines (append lines (list "--")))
            ;; Transition arrows: FROM -> TO "LINE"
            (if transitions
                (progn
                  (dolist (tr transitions)
                    (setq lines
                          (append lines
                                  (list (format "-- %s -> %s \"%d\""
                                                (car tr) (cadr tr) (caddr tr))))))
                  (setq lines (append lines (list "--"))))
              ;; No transitions detected
              (setq lines (append lines (list "-- # (no transitions detected)" "--"))))
            (setq i (1+ i))))))
    (setq lines (append lines (list "-- %DSL_END")))
    (mapconcat #'identity lines "\n")))

;;; ---------------------------------------------------------------------------
;;; Transition scanner
;;; ---------------------------------------------------------------------------

(defun vhdl-sm--collect-transitions (state-ids end)
  "Scan from current position to END, collecting state transitions.
STATE-IDS is the list of known state identifier strings for this machine.
Returns a deduplicated list of (FROM-STATE TO-STATE LINE-NUMBER) triples.

The algorithm tracks which `when STATE =>' block is active and, within
each block, looks for signal assignment lines `SIGNAL <= KNOWN_STATE'
where KNOWN_STATE is a member of STATE-IDS.  Assignments inside nested
`if'/`case' structures are still attributed to the enclosing `when'
state, capturing all transitions reachable from each state."
  (let ((current-from nil)
        (transitions  '())
        (nest         1))
    (while (and (zerop (forward-line))
                (> nest 0)
                (< (point) end))
      (cond
       ;; Inner case: increase nesting depth
       ((looking-at "^[ \t]*\\<case\\>[ \t]")
        (setq nest (1+ nest)))
       ;; End of a case block: decrease nesting depth
       ((looking-at "^[ \t]*\\<end\\>[ \t]+\\<case\\>")
        (setq nest (1- nest)))
       ;; New `when STATE =>' at depth 1: update current-from state
       ((and (= nest 1)
             (looking-at
              (concat "^[ \t]*\\<when\\>[ \t]+"
                      "\\([a-zA-Z][a-zA-Z0-9_]*\\)"
                      "[ \t]*=>")))
        (let ((state-id (match-string-no-properties 1)))
          (setq current-from
                (unless (string-equal (downcase state-id) "others")
                  state-id))))
       ;; Signal assignment `SIGNAL <= IDENTIFIER': record if IDENTIFIER is a
       ;; known state and current-from is set (i.e., not inside `others')
       ((and current-from
             (looking-at
              (concat "^[ \t]*[a-zA-Z][a-zA-Z0-9_]*[ \t]*"
                      "<=[ \t]*"
                      "\\([a-zA-Z][a-zA-Z0-9_]*\\)"
                      "[ \t]*;")))
        (let ((to-state   (match-string-no-properties 1))
              (trans-line (line-number-at-pos (point))))
          (when (and (member to-state state-ids)
                     (not (cl-some (lambda (tr)
                                     (and (string-equal (car tr) current-from)
                                          (string-equal (cadr tr) to-state)))
                                   transitions)))
            (push (list current-from to-state trans-line) transitions))))))
    (nreverse transitions)))

;;; ---------------------------------------------------------------------------
;;; Head-comment writer
;;; ---------------------------------------------------------------------------

(defun vhdl-sm--write-head-comment (output)
  "Insert or replace the DSL block in the current buffer's head comment.
If a `-- %DSL_START extract-state-machines' ... `-- %DSL_END' block
already exists (anywhere in the buffer) it is removed first.  The new
OUTPUT is then inserted at the end of the leading comment block (the
consecutive `--' lines at the very start of the file)."
  (save-excursion
    ;; 1. Remove any existing DSL block
    (goto-char (point-min))
    (when (re-search-forward
           "^--[ \t]*%DSL_START[ \t]+extract-state-machines"
           nil t)
      (let ((start (progn (beginning-of-line) (point)))
            (end   nil))
        (when (re-search-forward "^--[ \t]*%DSL_END" nil t)
          (forward-line 1)
          (setq end (point)))
        (when (and start end)
          (delete-region start end))))

    ;; 2. Find the end of the leading comment block
    (goto-char (point-min))
    (while (and (not (eobp))
                (looking-at "^--"))
      (forward-line))
    ;; Point is now at the first non-comment line (or eob).

    ;; 3. Insert the new DSL block
    (insert output "\n")))

;;; ---------------------------------------------------------------------------
;;; Internal utilities
;;; ---------------------------------------------------------------------------

(defun vhdl-sm--match-regexp (regexp name)
  "Return non-nil when NAME matches REGEXP, treating `_' as a word character.
VHDL identifiers may contain underscores.  Many Emacs modes (including
`vhdl-mode') classify `_' as a symbol constituent rather than a word
constituent, which means `\\w' would not match it.  This function
temporarily promotes `_' to a word constituent so that patterns like
\"\\\\w+state\\\\>\" correctly match identifiers such as \"cur_state\"."
  (let ((tbl (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" tbl)
    (with-syntax-table tbl
      (string-match-p regexp name))))

(defun vhdl-sm--trim (s)
  "Remove leading and trailing whitespace from string S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (setq s (substring s (match-end 0))))
  (if (string-match "[ \t\n\r]+\\'" s)
      (setq s (substring s 0 (match-beginning 0))))
  s)

(provide 'vhdl-sm)
;;; vhdl-sm.el ends here
