;;; proverif-mode.el --- ProVerif major mode -*- lexical-binding: t; -*-;;

(defvar proverif-pv--kw '("lemma" "axiom" "restriction" "among" "channel" "choice" "clauses" "const" "def" "diff" "do" "elimtrue" "else" "equation" "equivalence" "event" "expand" "fail" "for" "forall" "foreach" "free" "fun" "get" "if" "implementation" "in" "inj-event" "insert" "let" "letfun" "letproba" "new" "noninterf" "noselect" "not" "nounif" "or" "otherwise" "out" "param" "phase" "pred" "proba" "process" "proof" "public_vars" "putbegin" "query" "reduc" "secret" "select" "set" "suchthat" "sync" "table" "then" "type" "weaksecret" "yield") "ProVerif keywords")

(defvar proverif-pv--builtin '("private" "data" "typeConverter" "reachability" "pv_reachability" "real_or_random" "pv_real_or_random" "memberOptim" "decompData" "decompDataSelect" "block" "attacker" "mess" "maxSubset" "proveAll" "noneSat" "noneVerif" "discardSat" "discardVerif" "instantiateSat" "instantiateVerif" "fullSat" "fullVerif" "removeEvents" "keepEvents" "induction" "noInduction" "precise" "hypothesis" "conclusion" "ignoreAFewTimes" "inductionOn") "ProVerif builtins")

(defvar proverif-pv--kw-regexp (regexp-opt proverif-pv--kw 'words))
(defvar proverif-pv--builtin-regexp (regexp-opt proverif-pv--builtin 'words))

(defvar proverif-pv--connectives-regexp "\|\|\\|&&\\|->\\|<->\\|<=>\\|<-R\\|<-\\|==>\\|<=\\|!")

(setq proverif-pvKeywords
 `((,proverif-pv--kw-regexp . font-lock-keyword-face)
   (,proverif-pv--builtin-regexp . font-lock-builtin-face)
   (,proverif-pv--connectives-regexp . font-lock-reference-face)))

(defvar proverif-pv-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Define `(* ... *)` style comments
    (modify-syntax-entry ?\( "()1n" st)   ; '(' is open paren and comment starter
    (modify-syntax-entry ?\) ")(4n" st)   ; ')' is close paren and comment ender
    (modify-syntax-entry ?*  ". 23n" st)  ; '*' is comment delimiter (2=start, 3=end)
    ;; Define word syntax, etc. as needed
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `mylang-mode`.")

(define-derived-mode proverif-pv-mode prog-mode
  :syntax-table proverif-pv--mode-syntax-table
  (setq font-lock-defaults '(proverif-pvKeywords))
  (setq mode-name "ProVerif Typed Pi")
  (setq-local evil-shift-width 2)
  (setq-local tab-width 2)
  (setq-local comment-start "(*")
  (setq-local comment-end "*)")
  (setq-local comment-start-skip "(\\*+\\s-*")
  (setq-local comment-end-skip "\\s-*\\*+)"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pv[l]?$" . proverif-pv-mode))

(provide 'proverif-mode)
