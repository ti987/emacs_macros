;;
;; collection of generic regexp constant declarations
;; Toshi Isogai 2009-2018
;;

;;
;; primitives
;;
(provide 'regexp-const)

(defconst re-OR "\\|")
(defconst re-bol "^\\s-*")
(defconst re-close "\\)")
(defconst re-close-grp "\\)") ; same as re-close
(defconst re-close-opt "\\)?")
(defconst re-close-opt0 "\\)*")
(defconst re-close-opt1 "\\)+")
(defconst re-close-short "\\)*?")
(defconst re-lead-space0 "^\\s-*")
(defconst re-tail-space0 "\\s-*$")
(defconst re-non-semi "[^;]+")
(defconst re-non-space "\\S-+")
(defconst re-op-4 "[-*/+]")
(defconst re-open  "\\(")
(defconst re-open-grp  "\\(?:")
(defconst re-open-opt  "\\(")  ;; same as re-open. 
(defconst re-open-opt-grp  "\\(?:")  ;; same as re-open-grp. 
(defconst re-space0 "\\s-*")
(defconst re-space1 "\\s-+")
(defconst re-word-beg "\\<")
(defconst re-word-end "\\>")
(defconst re-wspace0 "\\(?:\\s-\\|\n\\)*")
(defconst re-wspace1 "\\(?:\\s-\\|\n\\)+")

(defconst re-any (concat re-open-grp "." re-OR "\n" re-close )) 
(defconst re-rest (concat re-open ".*" re-close )
  "rest of the line. backreferenced"
  ) 
