;;; parser.el --- LR(k) Parser -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


;;; Variables:


(defvar parser--debug
  nil
  "Whether to print debug messages or not.")

(defvar parser--table-non-terminal-p
  nil
  "Hash-table of terminals for quick checking.")

(defvar parser--table-productions
  nil
  "Hash-table of productions for quick retrieving.")

(defvar parser--table-terminal-p
  nil
  "Hash-table of non-terminals for quick checking.")

(defvar parser--grammar
  nil
  "Current grammar used in parser.")

(defvar parser--look-ahead-number
  nil
  "Current look-ahead number used.")

(defvar parser--f-sets
  nil
  "Generated F-sets for grammar.")


;; Macros


(defmacro parser--debug (&rest message)
  "Output MESSAGE but only if debug is enabled."
  `(when parser--debug
     ,@message))


;; Helper Functions


(defun parser--clear-cache ()
  "Clear cache."
  (setq parser--f-sets nil))

(defun parser--distinct (elements)
  "Return distinct of ELEMENTS."
  (let ((processed (make-hash-table :test 'equal))
        (new-elements))
    (dolist (element elements)
      (unless (gethash element processed)
        (puthash element t processed)
        (push element new-elements)))
    (nreverse new-elements)))

(defun parser--get-grammar-non-terminals (&optional G)
  "Return non-terminals of grammar G."
  (unless G
    (if parser--grammar
        (setq G parser--grammar)
      (error "No grammar G defined!")))
  (nth 0 G))

(defun parser--get-grammar-productions (&optional G)
  "Return productions of grammar G."
  (unless G
    (if parser--grammar
        (setq G parser--grammar)
      (error "No grammar G defined!")))
  (nth 2 G))

(defun parser--get-grammar-rhs (lhs)
  "Return right hand sides of LHS if there is any."
  (gethash lhs parser--table-productions))

(defun parser--get-grammar-start (&optional G)
  "Return start of grammar G."
  (unless G
    (if parser--grammar
        (setq G parser--grammar)
      (error "No grammar G defined!")))
  (nth 3 G))

(defun parser--get-grammar-terminals (&optional G)
  "Return terminals of grammar G."
  (unless G
    (if parser--grammar
        (setq G parser--grammar)
      (error "No grammar G defined!")))
  (nth 1 G))

(defun parser--load-symbols ()
  "Load terminals and non-terminals in grammar."
  (let ((terminals (parser--get-grammar-terminals)))
    (setq parser--table-terminal-p (make-hash-table :test 'equal))
    (dolist (terminal terminals)
      (puthash terminal t parser--table-terminal-p)))
  (let ((non-terminals (parser--get-grammar-non-terminals)))
    (setq parser--table-non-terminal-p (make-hash-table :test 'equal))
    (dolist (non-terminal non-terminals)
      (puthash non-terminal t parser--table-non-terminal-p)))
  (let ((productions (parser--get-grammar-productions)))
    (setq parser--table-productions (make-hash-table :test 'equal))
    (dolist (p productions)
      (let ((lhs (car p))
            (rhs (cdr p)))
        (let ((new-value (gethash lhs parser--table-productions)))
          (dolist (rhs-element rhs)
            (unless (listp rhs-element)
              (setq rhs-element (list rhs-element)))
            (push rhs-element new-value))
          (puthash lhs (nreverse new-value) parser--table-productions))))))

(defun parser--set-look-ahead-number (k)
  "Set look-ahead number K."
  (unless (parser--valid-look-ahead-number-p k)
    (error "Invalid look-ahead number k!"))
  (setq parser--look-ahead-number k)
  (parser--clear-cache))

(defun parser--set-grammar (G)
  "Set grammar G.."
  (unless (parser--valid-grammar-p G)
    (error "Invalid grammar G!"))
  (setq parser--grammar G)
  (parser--clear-cache)
  (parser--load-symbols))

(defun parser--sort-list (a b)
  "Return non-nil if a element in A is greater than a element in B in lexicographic order."
  (let ((length (min (length a) (length b)))
        (index 0)
        (continue t)
        (response nil))
    (while (and
            continue
            (< index length))
      (let ((a-element (nth index a))
            (b-element (nth index b)))
        (if (string-greaterp a-element b-element)
            (setq continue nil)
          (when (string-greaterp b-element a-element)
            (setq response t)
            (setq continue nil))))
      (setq index (1+ index)))
    response))

(defun parser--valid-e-p (symbol)
  "Return whether SYMBOL is the e identifier or not."
  (eq symbol 'e))

(defun parser--valid-grammar-p (G)
  "Return if grammar G is valid or not.  Grammar should contain list with 4 elements: non-terminals (N), terminals (T), productions (P), start (S) where N, T and P are lists containing symbols and/or strings and S is a symbol or string."
  (let ((valid-p t))
    (unless (listp G)
      (setq valid-p nil))
    (when (and
           valid-p
           (not (= (length G) 4)))
      (setq valid-p nil))
    (when (and
           valid-p
           (or
            (not (listp (nth 0 G)))
            (not (listp (nth 1 G)))
            (not (listp (nth 2 G)))
            (not (or
                  (stringp (nth 3 G))
                  (symbolp (nth 3 G))))))
      (setq valid-p nil))
    (when valid-p

      ;; Check every non-terminal
      (let ((non-terminals (nth 0 G)))
        (let ((non-terminal-count (length non-terminals))
              (non-terminal-index 0))
          (while (and
                  valid-p
                  (< non-terminal-index non-terminal-count))
            (let ((non-terminal (nth non-terminal-index non-terminals)))
              (unless (or
                       (symbolp non-terminal)
                       (stringp non-terminal))
                (setq valid-p nil)))
            (setq non-terminal-index (1+ non-terminal-index)))))

      ;; Check every terminal
      (let ((terminals (nth 1 G)))
        (let ((terminal-count (length terminals))
              (terminal-index 0))
          (while (and
                  valid-p
                  (< terminal-index terminal-count))
            (let ((terminal (nth terminal-index terminals)))
              (unless (or
                       (symbolp terminal)
                       (stringp terminal))
                (setq valid-p nil)))
            (setq terminal-index (1+ terminal-index)))))

      ;; Check every production
      (let ((productions (nth 2 G)))
        (let ((production-count (length productions))
              (production-index 0))
          (while (and
                  valid-p
                  (< production-index production-count))
            (let ((production (nth production-index productions)))
              (unless (parser--valid-production-p production)
                (setq valid-p nil)))
            (setq production-index (1+ production-index)))))

      ;; Check start
      (let ((start (nth 3 G)))
        (when (and
               valid-p
               (not (or (stringp start) (symbolp start))))
          (setq valid-p nil))))
    valid-p))

(defun parser--valid-look-ahead-number-p (k)
  "Return if look-ahead number K is valid or not."
  (and
   (integerp k)
   (>= k 0)))

(defun parser--valid-non-terminal-p (symbol)
  "Return whether SYMBOL is a non-terminal in grammar or not."
  (unless parser--table-non-terminal-p
    (error "Table for non-terminals is undefined!"))
  (if (gethash symbol parser--table-non-terminal-p)
      t
    nil))

(defun parser--valid-production-p (production)
  "Return whether PRODUCTION is valid or not."
  (let ((is-valid t))
    (unless (listp production)
      (setq is-valid nil))
    (when (and is-valid
               (not (> (length production) 1)))
      (setq is-valid nil))
    (when (and is-valid
               (not (or
                     (stringp (car production))
                     (symbolp (car production))
                     (listp (car production)))))
      (setq is-valid nil))

    ;; Validate left-hand-side (LHS) of production
    (when (and is-valid
               (listp (car production)))
      (let ((lhs (car production)))
        (let ((lhs-index 0)
              (lhs-length (length lhs)))
          (while (and is-valid
                      (< lhs-index lhs-length))
            (let ((p (nth lhs-index lhs)))
              (unless (or
                       (stringp p)
                       (symbolp p))
                (setq is-valid nil)))
            (setq lhs-index (1+ lhs-index))))))

    ;; Validate that RHS is a list or symbol or a string
    (when (and is-valid
               (not (or
                     (listp (car (cdr production)))
                     (symbolp (car (cdr production)))
                     (stringp (car (cdr production))))))
      (message "RHS is invalid")
      (setq is-valid nil))

    ;; Validate right-hand-side (RHS) of production
    (when is-valid
      (let ((rhs (cdr production)))
        (let ((rhs-index 0)
              (rhs-length (length rhs)))
          (while (and is-valid
                      (< rhs-index rhs-length))
            (let ((rhs-element (nth rhs-index rhs)))
              (cond
               ((stringp rhs-element))
               ((symbolp rhs-element))
               ((listp rhs-element)
                (dolist (rhs-sub-element rhs-element)
                  (unless (or
                           (stringp rhs-sub-element)
                           (symbolp rhs-sub-element))
                    (setq is-valid nil))))
               (t (setq is-valid nil)))
              (setq rhs-index (1+ rhs-index)))))))
    is-valid))

(defun parser--valid-sentential-form-p (symbols)
  "Return whether SYMBOLS is a valid sentential form in grammar or not."
  (let ((is-valid t))
    (let ((symbols-length (length symbols))
          (symbol-index 0))
      (while (and
              is-valid
              (< symbol-index symbols-length))
        (let ((symbol (nth symbol-index symbols)))
          (unless (parser--valid-symbol-p symbol)
            (setq is-valid nil)))
        (setq symbol-index (1+ symbol-index))))
    is-valid))

(defun parser--valid-symbol-p (symbol)
  "Return whether SYMBOL is valid or not."
  (let ((is-valid t))
    (unless (or
             (parser--valid-e-p symbol)
             (parser--valid-non-terminal-p symbol)
             (parser--valid-terminal-p symbol))
      (setq is-valid nil))
    is-valid))

(defun parser--valid-terminal-p (symbol)
  "Return whether SYMBOL is a terminal in grammar or not."
  (unless parser--table-terminal-p
    (error "Table for terminals is undefined!"))
  (if (gethash symbol parser--table-terminal-p)
      t
    nil))


;; Main Algorithms


;; p. 381
(defun parser--e-free-first (α)
  "For sentential string Α, Calculate e-free-first k terminals in grammar."
  (parser--first α t))

;; p. 358
(defun parser--f-set (input-tape state stack)
  "A deterministic push-down transducer (DPDT) for building F-sets from INPUT-TAPE, STATE and STACK."
  (unless (listp input-tape)
    (setq input-tape (list input-tape)))
  (parser--debug
   (message "(parser--f-set)")
   (message "input-tape: %s" input-tape)
   (message "state: %s" state)
   (message "stack: %s" stack))

  (let ((f-set)
        (input-tape-length (length input-tape))
        (k (nth 0 state))
        (i (nth 1 state))
        (f-sets (nth 2 state))
        (disallow-e-first (nth 3 state)))
    (parser--debug
     (message "input-tape-length: %s" input-tape-length)
     (message "k: %s" k)
     (message "i: %s" i))
    (while stack
      (let ((stack-symbol (pop stack)))
        (parser--debug
         (message "Stack-symbol: %s" stack-symbol))
        (let ((leading-terminals (nth 0 stack-symbol))
              (all-leading-terminals-p (nth 1 stack-symbol))
              (input-tape-index (nth 2 stack-symbol))
              (e-first-p nil))
          (parser--debug
           (message "leading-terminals: %s" leading-terminals)
           (message "all-leading-terminals-p: %s" all-leading-terminals-p)
           (message "input-tape-index: %s" input-tape-index))

          ;; Flag whether leading-terminal is empty or not
          (when (parser--valid-e-p leading-terminals)
            (setq e-first-p t))

          (parser--debug (message "e-first-p: %s" e-first-p))

          ;; If leading terminal is empty and we have input-tape left, disregard it
          (when (and
                 (not disallow-e-first)
                 e-first-p
                 (< input-tape-index input-tape-length))
            (parser--debug (message "Disregarding empty first terminal"))
            (setq leading-terminals nil))

          (let ((leading-terminals-count (length leading-terminals)))
            (parser--debug (message "leading-terminals-count: %s" leading-terminals-count))
            (while (and
                    (< input-tape-index input-tape-length)
                    (< leading-terminals-count k)
                    all-leading-terminals-p)
              (let ((rhs-element (nth input-tape-index input-tape))
                    (rhs-type))
                (parser--debug (message "rhs-element: %s" rhs-element))

                ;; Determine symbol type
                (cond
                 ((parser--valid-non-terminal-p rhs-element)
                  (setq rhs-type 'NON-TERMINAL))
                 ((parser--valid-e-p rhs-element)
                  (setq rhs-type 'EMPTY))
                 ((parser--valid-terminal-p rhs-element)
                  (setq rhs-type 'TERMINAL))
                 (t (error (format "Invalid symbol %s" rhs-element))))
                (parser--debug (message "rhs-type: %s" rhs-type))

                (cond

                 ((equal rhs-type 'NON-TERMINAL)
                  (if (> i 0)
                      (let ((sub-terminal-sets (gethash rhs-element (gethash (1- i) f-sets))))
                        (if sub-terminal-sets
                            (progn
                              (parser--debug
                               (message "Sub-terminal-sets F_%s_%s(%s) = %s (%d)" (1- i) k rhs-element sub-terminal-sets (length sub-terminal-sets)))
                              (let ((sub-terminal-set (car sub-terminal-sets)))

                                (unless (= (length sub-terminal-sets) 1)
                                  ;; Should branch off here, each unique permutation should be included in set
                                  ;; Follow first alternative in this scope but follow the rest in separate scopes
                                  (let ((sub-terminal-index 0))
                                    (dolist (sub-terminal-alternative-set sub-terminal-sets)
                                      (unless (= sub-terminal-index 0)
                                        (let ((alternative-all-leading-terminals-p all-leading-terminals-p))
                                          (parser--debug (message "Sub-terminal-alternative-set: %s" sub-terminal-alternative-set))

                                          ;; When sub-set only contains the e symbol
                                          (when (parser--valid-e-p (car sub-terminal-alternative-set))
                                            (parser--debug (message "alternative-set is e symbol"))
                                            (if disallow-e-first
                                                (when (= leading-terminals-count 0)
                                                  (setq alternative-all-leading-terminals-p nil))
                                              (when (or
                                                     (> leading-terminals-count 0)
                                                     (< input-tape-index (1- input-tape-length)))
                                                (setq sub-terminal-alternative-set nil)
                                                (parser--debug (message "Cleared sub-terminal-alternative-set")))))

                                          (let ((sub-rhs-leading-terminals (append leading-terminals sub-terminal-alternative-set)))
                                            (parser--debug (message "sub-rhs-leading-terminals: %s" sub-rhs-leading-terminals))
                                            (when (> (length sub-rhs-leading-terminals) k)
                                              (setq sub-rhs-leading-terminals (butlast sub-rhs-leading-terminals (- (length sub-rhs-leading-terminals) k))))
                                            (push `(,sub-rhs-leading-terminals ,alternative-all-leading-terminals-p ,(1+ input-tape-index)) stack))))
                                      (setq sub-terminal-index (1+ sub-terminal-index)))))

                                (parser--debug (message "Sub-terminal-set: %s" sub-terminal-set))
                                (when (or
                                       (not (parser--valid-e-p (car sub-terminal-set)))
                                       (= input-tape-index (1- input-tape-length)))
                                  (setq leading-terminals (append leading-terminals sub-terminal-set))
                                  (setq leading-terminals-count (+ leading-terminals-count (length sub-terminal-set)))
                                  (when (> leading-terminals-count k)
                                    (setq leading-terminals (butlast leading-terminals (- leading-terminals-count k)))
                                    (setq leading-terminals-count k)))))
                          (parser--debug
                           (message "Found no subsets for %s %s" rhs-element (1- i)))))
                    (setq all-leading-terminals-p nil)))

                 ((equal rhs-type 'EMPTY)
                  (if disallow-e-first
                      (when (= leading-terminals-count 0)
                        (setq all-leading-terminals-p nil))
                    (when (and
                           (= leading-terminals-count 0)
                           (= input-tape-index (1- input-tape-length)))
                      (setq leading-terminals (append leading-terminals rhs-element))
                      (setq leading-terminals-count (1+ leading-terminals-count)))))

                 ((equal rhs-type 'TERMINAL)
                  (when all-leading-terminals-p
                    (setq leading-terminals (append leading-terminals (list rhs-element)))
                    (setq leading-terminals-count (1+ leading-terminals-count))))))
              (setq input-tape-index (1+ input-tape-index)))
            (when (> leading-terminals-count 0)
              (unless (listp leading-terminals)
                (setq leading-terminals (list leading-terminals)))
              (push leading-terminals f-set))))))
    f-set))

;; Algorithm 5.5, p. 357
(defun parser--first (β &optional disallow-e-first)
  "For sentential-form Β, calculate first terminals, optionally DISALLOW-E-FIRST."
  (unless (listp β)
    (setq β (list β)))
  (unless (parser--valid-sentential-form-p β)
    (error "Invalid sentential form β!"))
  (let ((productions (parser--get-grammar-productions))
        (k parser--look-ahead-number))
    (let ((i-max (length productions)))
      ;; Generate F-sets only once per grammar
      (unless parser--f-sets
        (let ((f-sets (make-hash-table :test 'equal))
              (i 0))
          (while (< i i-max)
            (parser--debug (message "i = %s" i))
            (let ((f-set (make-hash-table :test 'equal)))

              ;; Iterate all productions, set F_i
              (dolist (p productions)
                (let ((production-lhs (car p))
                      (production-rhs (cdr p)))
                  (parser--debug
                   (message "Production: %s -> %s" production-lhs production-rhs))

                  ;; Iterate all blocks in RHS
                  (let ((f-p-set))
                    (dolist (rhs-p production-rhs)
                      (let ((rhs-string rhs-p))
                        (let ((rhs-leading-terminals
                               (parser--f-set rhs-string `(,k ,i ,f-sets ,disallow-e-first) '(("" t 0)))))
                          (parser--debug
                           (message "Leading %d terminals at index %s (%s) -> %s = %s" k i production-lhs rhs-string rhs-leading-terminals))
                          (when rhs-leading-terminals
                            (when (and
                                   (listp rhs-leading-terminals)
                                   (> (length rhs-leading-terminals) 0))
                              (dolist (rhs-leading-terminals-element rhs-leading-terminals)
                                (push rhs-leading-terminals-element f-p-set)))))))

                    ;; Make set distinct
                    (setq f-p-set (parser--distinct f-p-set))
                    (parser--debug
                     (message "F_%s_%s(%s) = %s" i k production-lhs f-p-set))
                    (puthash production-lhs (nreverse f-p-set) f-set))))
              (puthash i f-set f-sets)
              (setq i (+ i 1))))
          (setq parser--f-sets f-sets)))

      (parser--debug
       (message "Generated F-sets"))

      (let ((first-list nil))
        ;; Iterate each symbol in β using a PDA algorithm
        (let ((input-tape β)
              (input-tape-length (length β))
              (stack '((0 0 nil))))
          (while stack
            (let ((stack-topmost (pop stack)))
              (parser--debug
               (message "stack-topmost: %s" stack-topmost))
              (let ((input-tape-index (car stack-topmost))
                    (first-length (car (cdr stack-topmost)))
                    (first (car (cdr (cdr stack-topmost)))))
                (while (and
                        (< input-tape-index input-tape-length)
                        (< first-length k))
                  (let ((symbol (nth input-tape-index input-tape)))
                    (cond
                     ((parser--valid-terminal-p symbol)
                      (setq first (append first (list symbol)))
                      (setq first-length (1+ first-length)))
                     ((parser--valid-non-terminal-p symbol)
                      (parser--debug
                       (message "non-terminal symbol: %s" symbol))
                      (let ((symbol-f-set (gethash symbol (gethash (1- i-max) parser--f-sets))))
                        (parser--debug
                         (message "symbol-f-set: %s" symbol-f-set))
                        (when (> (length symbol-f-set) 1)
                          ;; Handle this scenario here were a non-terminal can result in different FIRST sets
                          (let ((symbol-f-set-index 1)
                                (symbol-f-set-length (length symbol-f-set)))
                            (while (< symbol-f-set-index symbol-f-set-length)
                              (let ((symbol-f-set-element (nth symbol-f-set-index symbol-f-set)))
                                (let ((alternative-first-length (+ first-length (length symbol-f-set-element)))
                                      (alternative-first (append first symbol-f-set-element))
                                      (alternative-tape-index (1+ input-tape-index)))
                                  (parser--debug
                                   (message "alternative-first: %s" alternative-first))
                                  (push `(,alternative-tape-index ,alternative-first-length ,alternative-first) stack)))
                              (setq symbol-f-set-index (1+ symbol-f-set-index)))))
                        (parser--debug
                         (message "main-symbol-f-set: %s" (car symbol-f-set)))
                        (setq first-length (+ first-length (length (car symbol-f-set))))
                        (setq first (append first (car symbol-f-set)))))))
                  (setq input-tape-index (1+ input-tape-index)))
                (when (> first-length 0)
                  (push first first-list))))))
        (setq first-list (sort first-list 'parser--sort-list))
        first-list))))

;; Definition at p. 343
(defun parser--follow (β)
  "Calculate follow-set of Β.  FOLLOW(β) = w, w is the set {w | S =>* αβγ and w is in FIRST(γ)}."
  ;; Make sure argument is a list
  (unless (listp β)
    (setq β (list β)))
  (let ((follow-set nil)
        (match-length (length β)))
    ;; Iterate all productions in grammar
    (let ((productions (parser--get-grammar-productions)))
      (dolist (p productions)
        ;; Iterate all RHS of every production
        (let ((production-rhs (cdr p))
              (match-index 0))
          (dolist (rhs production-rhs)

            ;; Make sure RHS is a list
            (unless (listp rhs)
              (setq rhs (list rhs)))

            ;; Iterate every symbol in RHS
            (let ((rhs-count (length rhs))
                  (rhs-index 0))
              (while (< rhs-index rhs-count)
                (let ((rhs-element (nth rhs-index rhs)))

                  ;; Search for all symbols β in RHS
                  (if (eq rhs-element (nth match-index β))
                      ;; Is symbols exists in RHS
                      (progn
                        (setq match-index (1+ match-index))
                        (when (= match-index match-length)
                          (if (= rhs-index (1- rhs-count))
                              ;; If rest of RHS is empty add e in follow-set
                              (push '(e) follow-set)
                            ;; Otherwise add FOLLOW(rest) to follow-set
                            (let ((rest (nthcdr (1+ rhs-index) rhs)))
                              (let ((first-set (parser--first rest)))
                                (setq follow-set (append first-set follow-set)))))
                          (setq match-index 0)))
                    (when (> match-index 0)
                      (setq match-index 0))))
                (setq rhs-index (1+ rhs-index))))))))
    (when (> (length follow-set) 0)
      (setq follow-set (parser--distinct follow-set)))
    follow-set))

;; Algorithm 5.8, p. 386
(defun parser--lr-items (γ)
  "Calculate valid LR-items for the viable prefix Γ."
  (let ((lr-items (make-hash-table :test 'equal))
        (start (parser--get-grammar-start)))
    (unless (listp γ)
      (setq γ (list γ)))
    (unless (parser--valid-sentential-form-p γ)
      (error "Invalid sentential form γ!"))
    (let ((prefix-length (length γ))
          (lr-item-exists (make-hash-table :test 'equal)))

      ;; 1

      ;; Iterate all productions in grammar
      (let ((lr-items-e)
            (start-productions (parser--get-grammar-rhs start)))

        ;; a
        (dolist (production-rhs start-productions)
          (dolist (rhs production-rhs)
            ;; Add [S -> . α] to V(e)
            (push `(,start nil ,rhs e) lr-items-e)
            (puthash `(e ,start nil ,rhs e) t lr-item-exists)))

        ;; b, c
        ;; 1.b. iterate every item in v-set(e), if [A -> . Bα, u] is an item and B -> β is in P
        ;; then foreach x in FIRST(αu) add [B -> . β, x] to v-set(e), provided it is not already there
        (let ((found-new t))

          ;; Repeat this until no new item is found
          (while found-new
            (setq found-new nil)

            ;; Iterate every item in V(e)
            (dolist (item lr-items-e)
              (let ((lhs (nth 0 item))
                    (prefix (nth 1 item))
                    (rhs (nth 2 item))
                    (suffix (nth 3 item)))

                ;; Without prefix
                (unless prefix

                  ;; Check if RHS starts with a non-terminal
                  (let ((rhs-first (car rhs)))
                    (when (parser--valid-terminal-p rhs-first)
                      (let ((rhs-rest (append (cdr rhs) suffix)))
                        (let ((rhs-first (parser--first rhs-rest)))
                          (message "FIRST(%s) = %s" rhs-rest rhs-first)
                          (let ((sub-production (parser--get-grammar-rhs rhs-first)))

                            ;; For each production with B as LHS
                            (dolist (sub-rhs sub-production)

                              ;; For each x in FIRST(αu)
                              (dolist (f rhs-first)

                                ;; Add [B -> . β, x] to v-set(e), provided it is not already there
                                (unless (gethash `(e ,rhs-first nil ,sub-rhs ,f) lr-item-exists)
                                  (push `(,rhs-first nil ,sub-rhs ,f) lr-items-e)

                                  ;; 1.c. repeat b until no more items can be added to v-set(e)
                                  (setq found-new t)))))))))))))))

      ;; 2
      ;; a
      ;; b
      ;; c

      lr-items)))


(provide 'parser)

;;; parser.el ends here
