;;; parser.el --- LR(k) Parser -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


;;; Variables:


(defvar parser--debug
  nil
  "Whether to print debug messages or not.")

(defvar parser--table-terminal-p
  nil
  "Hash-table of non-terminals for quick checking.")

(defvar parser--table-non-terminal-p
  nil
  "Hash-table of terminals for quick checking.")

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
      (puthash non-terminal t parser--table-non-terminal-p))))

(defun parser--set-grammar (G k)
  "Set grammar G with look-ahead number K."
  (unless (parser--valid-grammar-p G)
    (error "Invalid grammar G!"))
  (unless (parser--valid-look-ahead-number-p k)
    (error "Invalid look-ahead number k!"))
  (setq parser--grammar G)
  (setq parser--look-ahead-number k)
  (setq parser--f-sets nil)
  (parser--load-symbols))

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

      ;; TODO Check every production
      (let ((productions (nth 2 G)))
        (let ((production-count (length productions))
              (production-index 0))
          (while (and
                  valid-p
                  (< production-index production-count))
            (let ((production (nth production-index productions)))
              (unless (or
                       (symbolp production)
                       (stringp production))
                (setq valid-p nil)))
            (setq production-index (1+ production-index)))))

      ;; Check start
      (let ((start (nth 3 G)))
        (when (and
               valid-p
               (not (or (stringp start) (symbolp start))))
          (setq valid-p nil)))
      )
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

(defun parser--valid-sentential-form-p (symbols)
  "Return whether SYMBOLS is a valid sentential form in grammar or not."
  (let ((is-valid t))
    (let ((symbols-length (length symbols))
          (symbol-index 0))
      (while (and
              is-valid
              (< symbol-index symbols-length))
        (let ((symbol (nth symbol-index symbols)))
          (unless (or
                   (parser--valid-e-p symbol)
                   (parser--valid-non-terminal-p symbol)
                   (parser--valid-terminal-p symbol))
            (setq is-valid nil)))))
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
            (setq leading-terminals ""))

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
                  (setq rhs-type 'TERMINAL)))
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
                                    (dolist (sub-terminal-set sub-terminal-sets)
                                      (unless (= sub-terminal-index 0)

                                        ;; When we have a leading terminal and sub-terminal set is empty, don't append it
                                        (when (and
                                               (> leading-terminals-count 0)
                                               (parser--valid-e-p sub-terminal-set))
                                          (setq sub-terminal-set nil))

                                        (let ((sub-rhs-leading-terminals (append leading-terminals sub-terminal-set)))
                                          (when (> (length sub-rhs-leading-terminals) k)
                                            (setq sub-rhs-leading-terminals (butlast sub-rhs-leading-terminals (- (length sub-rhs-leading-terminals) k))))
                                          (push `(,sub-rhs-leading-terminals ,all-leading-terminals-p ,(1+ input-tape-index)) stack)))
                                      (setq sub-terminal-index (1+ sub-terminal-index)))))

                                (parser--debug (message "Sub-terminal-set: %s" sub-terminal-set))
                                (when (or
                                       (not (parser--valid-e-p sub-terminal-set))
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
                  (if all-leading-terminals-p
                      (if disallow-e-first
                          (when (= leading-terminals-count 0)
                            (setq all-leading-terminals-p nil))
                        (when (and
                               (= leading-terminals-count 0)
                               (= input-tape-index (1- input-tape-length)))
                          (setq leading-terminals (append leading-terminals rhs-element))
                          (setq leading-terminals-count (1+ leading-terminals-count))))
                    (setq all-leading-terminals-p nil)))

                 ((equal rhs-type 'TERMINAL)
                  (when all-leading-terminals-p
                    (setq leading-terminals (append leading-terminals rhs-element))
                    (setq leading-terminals-count (1+ leading-terminals-count))))))
              (setq input-tape-index (1+ input-tape-index)))
            (when (> leading-terminals-count 0)
              (push leading-terminals f-set))))))
    f-set))

;; Algorithm 5.5, p. 357
(defun parser--first (β &optional disallow-e-first)
  "For sentential-form Β, in grammar, calculate first k terminals, optionally DISALLOW-E-FIRST."
  (unless (parser--valid-sentential-form-p β)
    (error "Invalid sentential form β!"))
  (let* ((productions (parser--get-grammar-productions))
         (k parser--look-ahead-number)
         (i-max (length productions)))

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
                 (message "Production-LHS: %s" production-lhs)
                 (message "Production-RHS: %s" production-rhs))

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
                            (dolist (rhs-leading-terminals-string rhs-leading-terminals)
                              (when (and
                                     (stringp rhs-leading-terminals-string)
                                     (> (length rhs-leading-terminals-string) 0))
                                (push rhs-leading-terminals-string f-p-set))))))))

                  ;; Make set distinct
                  (setq f-p-set (parser--distinct f-p-set))
                  (parser--debug
                   (message "F_%s_%s(%s) = %s" i k production-lhs f-p-set))
                  (puthash production-lhs (nreverse f-p-set) f-set))))
            (puthash i f-set f-sets)
            (setq i (+ i 1))))
        (setq parser--f-sets f-sets)))

    ;; Iterate each symbol in β using a PDA algorithm
    (let ((state 'parsing)
          (input-tape β)
          (input-tape-length (length β))
          (stack '((0 0 nil)))
          (first-list nil))
      (while stack
        (let ((stack-topmost (pop stack)))
          (let ((input-tape-index (car stack-topmost))
                (first-length (car (cdr stack-topmost)))
                (first (car (cdr (cdr stack-topmost)))))
            (while (and
                    (< input-tape-index input-tape-length)
                    (< first-length k))
              (let ((symbol (nth input-tape-index input-tape)))
                (cond
                 ((parser--valid-terminal-p symbol)
                  (push symbol first)
                  (setq first-length (1+ first-length)))
                 ((parser--valid-non-terminal-p symbol)
                  (let ((symbol-f-set (sort (gethash symbol (gethash (1- i-max) parser--f-sets)) 'string<)))
                    (when (> (length symbol-f-set) 0)
                      ;; Handle this scenario here were a non-terminal can result in different FIRST sets
                      (let ((symbol-f-set-index 1)
                            (symbol-f-set-length (length symbol-f-set)))
                        (while (< symbol-f-set-index symbol-f-set-length)
                          (let ((symbol-f-set-element (nth symbol-f-set-index symbol-f-set)))
                            (let ((alternative-first-length (+ first-length (length symbol-f-set-element)))
                                  (alternative-first (append first symbol-f-set-element))
                                  (alternative-tape-index (1+ input-tape-index)))
                              (push `(,alternative-tape-index ,alternative-first-length ,alternative-first) stack))))))
                    (setq first-length (+ first-length (length (car symbol-f-set))))
                    (setq first (append first (car symbol-f-set)))))))
              (setq input-tape-index (1+ input-tape-index)))
            (when (> first-length 0)
              (push first first-list)))))
      first-list)))

(defun parser--v-set (y)
  "Calculate valid LRk-sets for the viable-prefix Y in grammar G with look-ahead K."
  (let ((v-set))
    (unless (parser--valid-grammar-p G)
      (error "Invalid grammar G!"))
    
    v-set))


(provide 'parser)

;;; parser.el ends here
