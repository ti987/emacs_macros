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
;;      `others').
;;   4. Write (or refresh) a `-- %DSL_START' ... `-- %DSL_END' block at
;;      the end of the file's leading comment section.
;;
;; Usage:
;;   M-x extract-state-machines   (run in a VHDL buffer)
;;
;; Configuration:
;;   vhdl-sm-state-regexp  - regexp that identifies state-signal names

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
  :states         - list of (STATE-ID COMMENT-OR-NIL) pairs"
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
                (let* ((case-var  (match-string-no-properties 1))
                       (case-line (line-number-at-pos (match-beginning 0))))
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
                          (let ((state-id  (match-string-no-properties 1))
                                (state-cmt nil))
                            (unless (string-equal (downcase state-id) "others")
                              ;; Capture an optional comment on the next line
                              (save-excursion
                                (forward-line)
                                (when (looking-at "^[ \t]*--[ \t]*\\(.*\\)$")
                                  (setq state-cmt
                                        (vhdl-sm--trim
                                         (match-string-no-properties 1)))))
                              (push (list state-id state-cmt) states))))))
                      (push (list :process-label proc-label
                                  :process-line  proc-line
                                  :sm-signal     case-var
                                  :sm-line       case-line
                                  :states        (nreverse states))
                            results)))))))))
    (nreverse results))))

;;; ---------------------------------------------------------------------------
;;; Output formatter
;;; ---------------------------------------------------------------------------

(defun vhdl-sm--format-output (sm-list buf-name)
  "Format SM-LIST as a VHDL comment block string.
BUF-NAME is used in the block header for identification."
  (let ((lines
         (list
          "-- %DSL_START extract-state-machines"
          (concat "-- Generated: " (format-time-string "%Y-%m-%d %H:%M:%S %Z"))
          (concat "-- File: " (file-name-nondirectory buf-name))
          "--")))
    (if (null sm-list)
        (setq lines (append lines (list "-- No state machines found." "--")))
      (let ((i 1))
        (dolist (sm sm-list)
          (let* ((label  (plist-get sm :process-label))
                 (pline  (plist-get sm :process-line))
                 (sig    (plist-get sm :sm-signal))
                 (sline  (plist-get sm :sm-line))
                 (states (plist-get sm :states))
                 (proc-str
                  (if label
                      (format "%s (line %d)" label pline)
                    (format "unnamed process (line %d)" pline))))
            (setq lines
                  (append lines
                          (list (format
                                 "-- %d. state signal: %s  [process: %s, case line: %d]"
                                 i sig proc-str sline))))
            (when states
              (setq lines (append lines (list "--    states:")))
              (dolist (s states)
                (let ((state-id  (car s))
                      (state-cmt (cadr s)))
                  (setq lines
                        (append lines
                                (list (if state-cmt
                                          (format "--      %s  -- %s"
                                                  state-id state-cmt)
                                        (format "--      %s" state-id))))))))
            (setq lines (append lines (list "--")))
            (setq i (1+ i))))))
    (setq lines (append lines (list "-- %DSL_END")))
    (mapconcat #'identity lines "\n")))

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
