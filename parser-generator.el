;;; parser-generator.el --- Parser Generator library -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


;;; Variables:


(defvar parser-generator--debug
  t
  "Whether to print debug messages or not.")

(defvar parser-generator--e-identifier
  'e
  "The identifier used for e-symbol.  Default value 'e.")

(defvar parser-generator--grammar
  nil
  "Current grammar used in parser.")

(defvar parser-generator--f-sets
  nil
  "Generated F-sets for grammar.")

(defvar parser-generator--f-free-sets
  nil
  "Generated e-free F-sets for grammar.")

(defvar parser-generator--look-ahead-number
  nil
  "Current look-ahead number used.")

(defvar parser-generator--table-look-aheads-p
  nil
  "Hash-table of look-aheads for quick checking.")

(defvar parser-generator--table-non-terminal-p
  nil
  "Hash-table of terminals for quick checking.")

(defvar parser-generator--table-productions-rhs
  nil
  "Hash-table of productions RHS indexed by LHS for quick retrieving.")

(defvar parser-generator--table-productions-number
  nil
  "Hash-table indexed by production and value is production-number.")

(defvar parser-generator--table-productions-number-reverse
  nil
  "Hash-table indexed by production-number and value is production.")

(defvar parser-generator--table-terminal-p
  nil
  "Hash-table of non-terminals for quick checking.")

(defvar parser-generator--table-translations
  nil
  "Hash-table indexed by production-number and value is translation function.")


;; Macros


(defmacro parser-generator--debug (&rest message)
  "Output MESSAGE but only if debug is enabled."
  `(when parser-generator--debug
     ,@message))


;; Helper Functions


(defun parser-generator--clear-cache ()
  "Clear cache."
  (setq parser-generator--f-sets nil)
  (setq parser-generator--f-free-sets nil))

(defun parser-generator--distinct (elements)
  "Return distinct of ELEMENTS."
  (let ((processed (make-hash-table :test 'equal))
        (new-elements))
    (dolist (element elements)
      (unless (gethash element processed)
        (puthash element t processed)
        (push element new-elements)))
    (nreverse new-elements)))

(defun parser-generator--get-grammar-look-aheads ()
  "Return all possible look-ahead set."
  (unless parser-generator--look-ahead-number
    (error "No look-ahead number defined!"))
  (let ((terminals (parser-generator--get-grammar-terminals))
        (look-aheads)
        (k parser-generator--look-ahead-number)
        (stack '((0 0 nil)))
        (marked-paths (make-hash-table :test 'equal))
        (added-look-aheads (make-hash-table :test 'equal)))
    (let ((terminals-max-index (1- (length terminals)))
          (terminal-index)
          (look-ahead-length)
          (look-ahead))
      (while stack
        (let ((item (pop stack)))
          (setq terminal-index (nth 0 item))
          (setq look-ahead-length (nth 1 item))
          (setq look-ahead (nth 2 item))

          (while (and
                  (< look-ahead-length k)
                  (<= terminal-index terminals-max-index))
            (let ((potential-look-ahead look-ahead)
                  (next-terminal (nth terminal-index terminals)))
              (push next-terminal potential-look-ahead)
              (if (gethash potential-look-ahead marked-paths)
                  (setq terminal-index (1+ terminal-index))
                (puthash potential-look-ahead t marked-paths)

                (push `(,terminal-index ,look-ahead-length,look-ahead) stack)

                (setq look-ahead-length (1+ look-ahead-length))
                (setq look-ahead potential-look-ahead)
                (setq terminal-index 0))))

          (let ((look-ahead-to-add))
            (if look-ahead
                (progn

                  (when (= look-ahead-length k)
                    (setq look-ahead-to-add (reverse look-ahead)))

                  (when (= look-ahead-length (1- k))
                    (push parser-generator--e-identifier look-ahead)
                    (setq look-ahead-to-add (reverse look-ahead))))

              (when (= k 1)
                (setq look-ahead-to-add `(,parser-generator--e-identifier))))

            (when (and look-ahead-to-add
                       (not (gethash look-ahead-to-add added-look-aheads)))
              (puthash look-ahead-to-add t added-look-aheads)
              (push look-ahead-to-add look-aheads))))))

    (sort look-aheads 'parser-generator--sort-list)))

(defun parser-generator--get-grammar-non-terminals (&optional G)
  "Return non-terminals of grammar G."
  (unless G
    (if parser-generator--grammar
        (setq G parser-generator--grammar)
      (error "No grammar G defined!")))
  (nth 0 G))

(defun parser-generator--get-grammar-production-number (production)
  "If PRODUCTION exist, return it's number."
  (unless parser-generator--table-productions-number
    (error "Table for production-numbers is undefined!"))
  (gethash production parser-generator--table-productions-number))

(defun parser-generator--get-grammar-production-by-number (production-number)
  "If PRODUCTION-NUMBER exist, return it's production."
  (unless parser-generator--table-productions-number-reverse
    (error "Table for reverse production-numbers is undefined!"))
  (gethash production-number parser-generator--table-productions-number-reverse))

(defun parser-generator--get-grammar-productions (&optional G)
  "Return productions of grammar G."
  (unless G
    (if parser-generator--grammar
        (setq G parser-generator--grammar)
      (error "No grammar G defined!")))
  (nth 2 G))

(defun parser-generator--get-grammar-rhs (lhs)
  "Return right hand sides of LHS if there is any."
  (unless parser-generator--table-productions-rhs
    (error "Table for productions RHS indexed by LHS is undefined!"))
  (gethash lhs parser-generator--table-productions-rhs))

(defun parser-generator--get-grammar-start (&optional G)
  "Return start of grammar G."
  (unless G
    (if parser-generator--grammar
        (setq G parser-generator--grammar)
      (error "No grammar G defined!")))
  (nth 3 G))

(defun parser-generator--get-grammar-terminals (&optional G)
  "Return terminals of grammar G."
  (unless G
    (if parser-generator--grammar
        (setq G parser-generator--grammar)
      (error "No grammar G defined!")))
  (nth 1 G))

(defun parser-generator--get-grammar-translation-by-number (production-number)
  "If translation for PRODUCTION-NUMBER exist, return it."
  (unless parser-generator--table-translations
    (error "Table for translations by production-number is undefined!"))
  (gethash production-number parser-generator--table-translations))

(defun parser-generator--hash-to-list (hash-table &optional un-sorted)
  "Return a list that represent the HASH-TABLE.  Each element is a list: (list key value), optionally UN-SORTED."
  (let (result)
    (if (hash-table-p hash-table)
        (progn
          (maphash
           (lambda (k v) (push (list k v) result))
           hash-table)
          (if un-sorted
              (nreverse result)
            (sort (nreverse result) (lambda (a b) (< (car a) (car b))))))
      nil)))

(defun parser-generator--hash-values-to-list (hash-table &optional un-sorted)
  "Return a list that represent the HASH-TABLE.  Each element is a list: (list key value), optionally UN-SORTED."
  (let (result)
    (if (hash-table-p hash-table)
        (progn
          (maphash
           (lambda (_k v) (push v result))
           hash-table)
          (if un-sorted
              (nreverse result)
            (sort (nreverse result) (lambda (a b) (< (car a) (car b))))))
      nil)))

(defun parser-generator--load-symbols ()
  "Load terminals and non-terminals in grammar."
  (let ((terminals (parser-generator--get-grammar-terminals)))
    (setq parser-generator--table-terminal-p (make-hash-table :test 'equal))
    (dolist (terminal terminals)
      (puthash terminal t parser-generator--table-terminal-p)))

  (let ((non-terminals (parser-generator--get-grammar-non-terminals)))
    (setq parser-generator--table-non-terminal-p (make-hash-table :test 'equal))
    (dolist (non-terminal non-terminals)
      (puthash non-terminal t parser-generator--table-non-terminal-p)))

  (let ((productions (parser-generator--get-grammar-productions)))
    (setq parser-generator--table-productions-rhs (make-hash-table :test 'equal))
    (dolist (p productions)
      (let ((lhs (car p))
            (rhs (cdr p)))
        (let ((new-value (gethash lhs parser-generator--table-productions-rhs)))
          (dolist (rhs-element rhs)
            (unless (listp rhs-element)
              (setq rhs-element (list rhs-element)))
            (let ((new-rhs))
              (dolist (rhs-sub-element rhs-element)
                (unless (functionp rhs-sub-element)
                  (push rhs-sub-element new-rhs)))
              (push (nreverse new-rhs) new-value)))
          (puthash lhs (nreverse new-value) parser-generator--table-productions-rhs))))

    (setq parser-generator--table-productions-number (make-hash-table :test 'equal))
    (setq parser-generator--table-productions-number-reverse (make-hash-table :test 'equal))
    (setq parser-generator--table-translations (make-hash-table :test 'equal))
    (let ((production-index 0))
      (dolist (p productions)
        (let ((lhs (car p))
              (rhs (cdr p))
              (production)
              (translation))
          (let ((rhs-element-index 0)
                (rhs-length (length rhs))
                (rhs-element))
            (while (< rhs-element-index rhs-length)
              (setq rhs-element (nth rhs-element-index rhs))
              (unless (listp rhs-element)
                (setq rhs-element (list rhs-element)))

              (let ((sub-rhs-element-index 0)
                    (sub-rhs-element-length (length rhs-element))
                    (sub-rhs-element)
                    (new-rhs))
                (while (< sub-rhs-element-index sub-rhs-element-length)
                  (setq sub-rhs-element (nth sub-rhs-element-index rhs-element))
                  (if (functionp sub-rhs-element)
                      (setq translation sub-rhs-element)
                    (push sub-rhs-element new-rhs))
                  (setq sub-rhs-element-index (1+ sub-rhs-element-index)))
                (setq production (list lhs (nreverse new-rhs)))
                (parser-generator--debug
                 (message "Production %s: %s" production-index production)))
              (setq rhs-element-index (1+ rhs-element-index))
              (puthash production production-index parser-generator--table-productions-number)
              (puthash production-index production parser-generator--table-productions-number-reverse)
              (when translation
                (parser-generator--debug
                 (message "Translation %s: %s" production-index translation))
                (puthash production-index translation parser-generator--table-translations))
              (setq production-index (1+ production-index))))))))

  (let ((look-aheads (parser-generator--get-grammar-look-aheads)))
    (setq parser-generator--table-look-aheads-p (make-hash-table :test 'equal))
    (dolist (look-ahead look-aheads)
      (puthash look-ahead t parser-generator--table-look-aheads-p))))

(defun parser-generator-set-look-ahead-number (k)
  "Set look-ahead number K."
  (unless (parser-generator--valid-look-ahead-number-p k)
    (error "Invalid look-ahead number k!"))
  (setq parser-generator--look-ahead-number k))

(defun parser-generator-set-grammar (G)
  "Set grammar G.."
  (unless (parser-generator--valid-grammar-p G)
    (error "Invalid grammar G! %s" G))
  (setq parser-generator--grammar G))

(defun parser-generator-process-grammar ()
  "Process grammar."
  (parser-generator--clear-cache)
  (parser-generator--load-symbols))

(defun parser-generator--sort-list (a b)
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
        (while (and
                a-element
                (listp a-element))
          (setq a-element (car a-element)))
        (while (and
                b-element
                (listp b-element))
          (setq b-element (car b-element)))
        (when (and
               (or
                (stringp a-element)
                (symbolp a-element))
               (or
                (stringp b-element)
                (symbolp b-element)))
          (if (string-greaterp a-element b-element)
              (setq continue nil)
            (when (string-greaterp b-element a-element)
              (setq response t)
              (setq continue nil))))
        (when (and
               (numberp a-element)
               (numberp b-element))
          (if (> a-element b-element)
              (setq continue nil)
            (when (> b-element a-element)
              (setq response t)
              (setq continue nil)))))
      (setq index (1+ index)))
    response))

(defun parser-generator--valid-e-p (symbol)
  "Return whether SYMBOL is the e identifier or not."
  (eq symbol parser-generator--e-identifier))

(defun parser-generator--valid-grammar-p (G)
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
              (unless (parser-generator--valid-production-p production)
                (setq valid-p nil)))
            (setq production-index (1+ production-index)))))

      ;; Check start
      (let ((start (nth 3 G)))
        (when (and
               valid-p
               (not (or (stringp start) (symbolp start))))
          (setq valid-p nil))))
    valid-p))

(defun parser-generator--valid-look-ahead-p (symbol)
  "Return whether SYMBOL is a look-ahead in grammar or not."
  (unless parser-generator--table-look-aheads-p
    (error "Table for look-aheads is undefined!"))
  (unless (listp symbol)
    (setq symbol (list symbol)))
  (gethash symbol parser-generator--table-look-aheads-p))

(defun parser-generator--valid-look-ahead-number-p (k)
  "Return if look-ahead number K is valid or not."
  (and
   (integerp k)
   (>= k 0)))

(defun parser-generator--valid-non-terminal-p (symbol)
  "Return whether SYMBOL is a non-terminal in grammar or not."
  (unless parser-generator--table-non-terminal-p
    (error "Table for non-terminals is undefined!"))
  (gethash symbol parser-generator--table-non-terminal-p))

(defun parser-generator--valid-production-p (production)
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
               ((and (functionp rhs-element)
                     (= rhs-index (1- rhs-length))))
               ((and
                 (listp rhs-element)
                 (not (functionp rhs-element)))
                (let ((rhs-sub-index 0)
                      (rhs-sub-element)
                      (rhs-sub-length (length rhs-element)))
                  (while (and is-valid
                              (< rhs-sub-index rhs-sub-length))
                    (setq rhs-sub-element (nth rhs-sub-index rhs-element))
                    (cond
                     ((and
                       (listp rhs-sub-element)
                       (not (functionp rhs-sub-element)))
                      (unless (and
                               (or (stringp (car rhs-sub-element))
                                   (symbolp (car rhs-sub-element)))
                               (functionp (car (cdr rhs-sub-element))))
                        (setq is-valid nil)))
                     ((and (functionp rhs-sub-element)
                           (= rhs-sub-index (1- rhs-sub-length))))
                     ((or (stringp rhs-sub-element)
                          (symbolp rhs-sub-element)))
                     (t (setq is-valid nil)))
                    (setq rhs-sub-index (1+ rhs-sub-index)))))
               (t (setq is-valid nil)))
              (setq rhs-index (1+ rhs-index)))))))
    is-valid))

(defun parser-generator--valid-sentential-form-p (symbols)
  "Return whether SYMBOLS is a valid sentential form in grammar or not."
  (let ((is-valid t))
    (let ((symbols-length (length symbols))
          (symbol-index 0))
      (while (and
              is-valid
              (< symbol-index symbols-length))
        (let ((symbol (nth symbol-index symbols)))
          (unless (parser-generator--valid-symbol-p symbol)
            (setq is-valid nil)))
        (setq symbol-index (1+ symbol-index))))
    is-valid))

(defun parser-generator--valid-symbol-p (symbol)
  "Return whether SYMBOL is valid or not."
  (let ((is-valid t))
    (unless (or
             (parser-generator--valid-e-p symbol)
             (parser-generator--valid-non-terminal-p symbol)
             (parser-generator--valid-terminal-p symbol))
      (setq is-valid nil))
    is-valid))

(defun parser-generator--valid-terminal-p (symbol)
  "Return whether SYMBOL is a terminal in grammar or not."
  (unless parser-generator--table-terminal-p
    (error "Table for terminals is undefined!"))
  (gethash symbol parser-generator--table-terminal-p))


;; Main Algorithms


;; p. 381
(defun parser-generator--e-free-first (α)
  "For sentential string Α, Calculate e-free-first k terminals in grammar."
  (parser-generator--first α t))

(defun parser-generator--generate-f-sets ()
  "Generate F-sets for grammar."
  ;; Generate F-sets only once per grammar
  (unless (and
           parser-generator--f-sets
           parser-generator--f-free-sets)
    (parser-generator--debug (message "(parser-generator--generate-f-sets)"))
    (let ((productions (parser-generator--get-grammar-productions))
          (k parser-generator--look-ahead-number))
      (let ((disallow-set '(nil t)))
        (parser-generator--debug (message "disallow-set: %s" disallow-set))
        (dolist (disallow-e-first disallow-set)
          (parser-generator--debug (message "disallow-e-first: %s" disallow-e-first))
          (let ((f-sets (make-hash-table :test 'equal))
                (i 0)
                (expanded-all nil)
                (expanded-all-second nil))

            (while (or
                    (not expanded-all)
                    (not expanded-all-second))
              ;; Make one iteration after everything has been expanded
              (when expanded-all
                (setq expanded-all-second t))
              (when (> i 100)
                (error "Endless loop!"))
              (parser-generator--debug (message "i = %s" i))
              (setq expanded-all t)
              (let ((f-set (make-hash-table :test 'equal)))

                ;; Iterate all productions, set F_i
                (dolist (p productions)
                  (let ((production-lhs (car p))
                        (production-rhs (cdr p)))
                    (parser-generator--debug
                     (message
                      "Production: %s -> %s"
                      production-lhs
                      production-rhs))

                    ;; Iterate all blocks in RHS
                    (let ((f-p-set)
                          (rhs-expanded-full t))
                      (dolist (rhs-p production-rhs)
                        (let ((rhs-string rhs-p))
                          (let ((rhs-leading-terminals)
                                (f-set-return
                                 (parser-generator--f-set
                                  rhs-string
                                  `(
                                    ,k
                                    ,i
                                    ,f-sets
                                    ,disallow-e-first)
                                  '(("" t 0)))))

                            (parser-generator--debug
                             (message
                              "f-set-return: %s = %s"
                              rhs-string
                              f-set-return))

                            (unless (nth 0 f-set-return)
                              (parser-generator--debug
                               (message "rhs-expanded-full flagged negative"))
                              (setq rhs-expanded-full nil)
                              (setq expanded-all nil))
                            (setq rhs-leading-terminals
                                  (nth 1 f-set-return))

                            (parser-generator--debug
                             (message
                              "Leading %d terminals at index %s: %s -> %s = %s"
                              k
                              i
                              production-lhs
                              rhs-string
                              rhs-leading-terminals))
                            (parser-generator--debug
                             (message
                              "expanded-all: %s" expanded-all))

                            (when rhs-leading-terminals
                              (when (and
                                     (listp rhs-leading-terminals)
                                     (> (length rhs-leading-terminals) 0))
                                (dolist
                                    (rhs-leading-terminals-element rhs-leading-terminals)
                                  (push
                                   rhs-leading-terminals-element
                                   f-p-set)))))))

                      ;; If we have multiple equal LHS
                      ;; merge them
                      (when (gethash production-lhs f-set)
                        (let ((existing-f-set (gethash production-lhs f-set)))

                          ;; If another set has not been fully expanded
                          ;; mark LHS as not fully expanded
                          (unless (nth 0 existing-f-set)
                            (setq expanded-all nil)
                            (setq rhs-expanded-full nil))

                          (setq f-p-set
                                (append
                                 f-p-set
                                 (nth 1 existing-f-set)))))

                      ;; Make set distinct
                      (setq f-p-set (parser-generator--distinct f-p-set))
                      (parser-generator--debug
                       (message
                        "F_%s(%s) = %s"
                        i
                        production-lhs
                        (list rhs-expanded-full (reverse f-p-set))))
                      (puthash
                       production-lhs
                       (list rhs-expanded-full (reverse f-p-set))
                       f-set))))

                (puthash i f-set f-sets)
                (setq i (+ i 1))))

            (if disallow-e-first
                (progn
                  (parser-generator--debug
                   (message "Max-index: %s" (1- i)))
                  (setq parser-generator--f-free-sets (gethash (1- i) f-sets)))
              (parser-generator--debug
               (message "Max-index: %s" (1- i)))
              (setq parser-generator--f-sets (gethash (1- i) f-sets)))))))
    (parser-generator--debug
     (message "Generated F-sets"))))

;; p. 357
(defun parser-generator--f-set (input-tape state stack)
  "A deterministic push-down transducer (DPDT) for building F-sets from INPUT-TAPE, STATE and STACK."
  (unless (listp input-tape)
    (setq input-tape (list input-tape)))
  (parser-generator--debug
   (message "(parser-generator--f-set)")
   (message "input-tape: %s" input-tape)
   (message "stack: %s" stack))

  (let ((f-set)
        (input-tape-length (length input-tape))
        (k (nth 0 state))
        (i (nth 1 state))
        (f-sets (nth 2 state))
        (disallow-e-first (nth 3 state))
        (expanded-all t))
    (parser-generator--debug
     (message "disallow-3-first: %s" disallow-e-first)
     (message "input-tape-length: %s" input-tape-length)
     (message "k: %s" k)
     (message "i: %s" i))

    (while stack
      (let ((stack-symbol (pop stack)))
        (parser-generator--debug
         (message "Stack-symbol: %s" stack-symbol))
        (let ((leading-terminals (nth 0 stack-symbol))
              (all-leading-terminals-p (nth 1 stack-symbol))
              (input-tape-index (nth 2 stack-symbol))
              (e-first-p))
          (parser-generator--debug
           (message "leading-terminals: %s" leading-terminals)
           (message "all-leading-terminals-p: %s" all-leading-terminals-p)
           (message "input-tape-index: %s" input-tape-index))

          ;; Flag whether leading-terminal is the e-identifier or not
          (when (parser-generator--valid-e-p leading-terminals)
            (setq e-first-p t))

          (parser-generator--debug
           (message "e-first-p: %s" e-first-p))

          (let ((leading-terminals-count (length leading-terminals)))
            (parser-generator--debug
             (message "leading-terminals-count: %s" leading-terminals-count))

            (while (and
                    (< input-tape-index input-tape-length)
                    (< leading-terminals-count k)
                    all-leading-terminals-p)
              (let ((rhs-element (nth input-tape-index input-tape))
                    (rhs-type))
                (parser-generator--debug
                 (message "rhs-element: %s" rhs-element))

                ;; Determine symbol type
                (cond
                 ((parser-generator--valid-non-terminal-p rhs-element)
                  (setq rhs-type 'NON-TERMINAL))
                 ((parser-generator--valid-e-p rhs-element)
                  (setq rhs-type 'E-IDENTIFIER))
                 ((parser-generator--valid-terminal-p rhs-element)
                  (setq rhs-type 'TERMINAL))
                 (t (error (format "Invalid symbol %s" rhs-element))))
                (parser-generator--debug (message "rhs-type: %s" rhs-type))

                (cond

                 ((equal rhs-type 'NON-TERMINAL)
                  (if (> i 0)
                      (let ((sub-terminal-sets)
                            (sub-terminal-expanded)
                            (sub-terminal-data
                             (gethash
                              rhs-element
                              (gethash
                               (1- i)
                               f-sets))))
                        (parser-generator--debug
                               (message
                                "sub-terminal-data: %s = %s"
                                rhs-element
                                sub-terminal-data))
                                
                        (setq sub-terminal-expanded (nth 0 sub-terminal-data))
                        (setq sub-terminal-sets (nth 1 sub-terminal-data))

                        ;; When sub-set has not been fully expanded mark this set
                        ;; as not fully expanded either
                        (when (and
                               sub-terminal-data
                               (not sub-terminal-expanded))
                          (parser-generator--debug
                           (message
                            "Expanded-all negative set 1 from %s" rhs-element))
                          (setq expanded-all nil))

                        (if sub-terminal-sets
                            (progn
                              (parser-generator--debug
                               (message
                                "Sub-terminal-sets F_%s(%s) = %s (%d)"
                                (1- i)
                                rhs-element
                                sub-terminal-sets
                                (length sub-terminal-sets)))
                              (let ((sub-terminal-set (car sub-terminal-sets)))

                                (unless (= (length sub-terminal-sets) 1)

                                  ;; Should branch off here, each unique permutation should be included in set
                                  ;; Follow the first alternative in this scope but follow the rest in separate scopes
                                  (let ((sub-terminal-index 0))
                                    (dolist (sub-terminal-alternative-set sub-terminal-sets)
                                      (unless (= sub-terminal-index 0)
                                        (let ((alternative-all-leading-terminals-p all-leading-terminals-p))
                                          (parser-generator--debug
                                           (message "Sub-terminal-alternative-set: %s" sub-terminal-alternative-set))

                                          ;; When sub-set only contains the e identifier
                                          (when (parser-generator--valid-e-p
                                                 (car sub-terminal-alternative-set))
                                            (parser-generator--debug
                                             (message "alternative-set is the e identifier"))

                                            ;; Branch off here in two separate tracks, one with the e-identifier appended and one without
                                            (if disallow-e-first
                                                (progn
                                                  (when (and
                                                         all-leading-terminals-p
                                                         (> leading-terminals-count 0))
                                                    (parser-generator--debug (message "branched off 2"))
                                                    ;; Branch off here with a separate track where this e-identifier is ignored
                                                    (push `(
                                                            ,leading-terminals
                                                            ,all-leading-terminals-p
                                                            ,(1+ input-tape-index))
                                                          stack))

                                                  (setq alternative-all-leading-terminals-p nil))

                                              ;; Add e-identifier to leading terminals when
                                              ;; we have not found any leading terminals
                                              ;; and we are at the last symbol in input-tape

                                              (when all-leading-terminals-p
                                                (parser-generator--debug (message "branched off 1"))
                                                ;; Branch off here with a separate track where this e-identifier is ignored
                                                (push `(
                                                        ,leading-terminals
                                                        ,all-leading-terminals-p
                                                        ,(1+ input-tape-index))
                                                      stack))

                                              (setq alternative-all-leading-terminals-p nil)))

                                          (let ((sub-rhs-leading-terminals
                                                 (append
                                                  leading-terminals
                                                  sub-terminal-alternative-set)))
                                            (parser-generator--debug
                                             (message
                                              "sub-rhs-leading-terminals: %s"
                                              sub-rhs-leading-terminals))
                                            (when (> (length sub-rhs-leading-terminals) k)
                                              (setq
                                               sub-rhs-leading-terminals
                                               (butlast
                                                sub-rhs-leading-terminals
                                                (- (length sub-rhs-leading-terminals) k))))
                                            (parser-generator--debug (message "branched off 3"))
                                            (push `(
                                                    ,sub-rhs-leading-terminals
                                                    ,alternative-all-leading-terminals-p
                                                    ,(1+ input-tape-index))
                                                  stack))))
                                      (setq sub-terminal-index (1+ sub-terminal-index)))))

                                (parser-generator--debug
                                 (message "Sub-terminal-set: %s" sub-terminal-set))

                                ;; When sub-set only contains the e identifier
                                (if (parser-generator--valid-e-p
                                     (car sub-terminal-set))
                                    (progn
                                      (parser-generator--debug
                                       (message "sub-terminal-set is the e identifier"))

                                      ;; Branch off here in two separate tracks, one with the e-identifier appended and one without
                                      (if disallow-e-first
                                          (progn
                                            (when (and
                                                   all-leading-terminals-p
                                                   (> leading-terminals-count 0))
                                              ;; Branch off here with a separate track where this e-identifier is ignored
                                              (parser-generator--debug (message "branched off 4"))
                                              (push `(
                                                      ,leading-terminals
                                                      ,all-leading-terminals-p
                                                      ,(1+ input-tape-index))
                                                    stack))

                                            (setq all-leading-terminals-p nil))

                                        ;; Add e-identifier to leading terminals when
                                        ;; we have not found any leading terminals
                                        ;; and we are at the last symbol in input-tape

                                        (when all-leading-terminals-p
                                          ;; Branch off here with a separate track where this e-identifier is ignored
                                          (parser-generator--debug (message "branched off 5"))
                                          (push `(
                                                  ,leading-terminals
                                                  ,all-leading-terminals-p
                                                  ,(1+ input-tape-index))
                                                stack))

                                        (setq leading-terminals (append leading-terminals sub-terminal-set))
                                        (setq leading-terminals-count (+ leading-terminals-count (length sub-terminal-set)))
                                        (when (> leading-terminals-count k)
                                          (setq leading-terminals (butlast leading-terminals (- leading-terminals-count k)))
                                          (setq leading-terminals-count k))
                                        (setq all-leading-terminals-p nil)))

                                  (setq leading-terminals (append leading-terminals sub-terminal-set))
                                  (setq leading-terminals-count (+ leading-terminals-count (length sub-terminal-set)))
                                  (when (> leading-terminals-count k)
                                    (setq leading-terminals (butlast leading-terminals (- leading-terminals-count k)))
                                    (setq leading-terminals-count k)))))

                          (parser-generator--debug
                           (message "Found no subsets for %s %s" rhs-element (1- i)))
                          (setq all-leading-terminals-p nil)))

                    (parser-generator--debug
                     (message
                      "Expanded-all negative set 2"))
                    (setq expanded-all nil)
                    (setq all-leading-terminals-p nil)))

                 ((equal rhs-type 'E-IDENTIFIER)
                  (if disallow-e-first
                      (progn
                        (when (and
                               all-leading-terminals-p
                               (> leading-terminals-count 0))
                          ;; Branch off here with a separate track where this e-identifier is ignored
                          (parser-generator--debug (message "branched off 6"))
                          (push `(
                                  ,leading-terminals
                                  ,all-leading-terminals-p
                                  ,(1+ input-tape-index))
                                stack))

                        (setq all-leading-terminals-p nil))
                    ;; Add e-identifier to leading terminals when
                    ;; we have not found any leading terminals
                    ;; and we are at the last symbol in input-tape

                    (when all-leading-terminals-p
                      ;; Branch off here with a separate track where this e-identifier is ignored
                      (parser-generator--debug (message "branched off 7"))
                      (push `(
                              ,leading-terminals
                              ,all-leading-terminals-p
                              ,(1+ input-tape-index))
                            stack))

                      (setq leading-terminals (append leading-terminals rhs-element))
                      (setq leading-terminals-count (1+ leading-terminals-count))
                      (setq all-leading-terminals-p nil)))

                 ((equal rhs-type 'TERMINAL)
                  (setq leading-terminals (append leading-terminals (list rhs-element)))
                  (setq leading-terminals-count (1+ leading-terminals-count)))))
              (setq input-tape-index (1+ input-tape-index)))
            (when (> leading-terminals-count 0)
              (unless (listp leading-terminals)
                (setq leading-terminals (list leading-terminals)))
              (push leading-terminals f-set))))))
    (list expanded-all f-set)))

;; Algorithm 5.5, p. 357
(defun parser-generator--first (β &optional disallow-e-first)
  "For sentential-form Β, calculate first terminals, optionally DISALLOW-E-FIRST."
  (unless (listp β)
    (setq β (list β)))
  (unless (parser-generator--valid-sentential-form-p β)
    (error "Invalid sentential form β! %s" β))
  (let ((k parser-generator--look-ahead-number))

      ;; Generate F-sets only once per grammar
      (parser-generator--generate-f-sets)

      (let ((first-list nil))
        ;; Iterate each symbol in β using a PDA algorithm
        (let ((input-tape β)
              (input-tape-length (length β))
              (stack '((0 0 nil))))
          (while stack
            (let ((stack-topmost (pop stack)))
              (parser-generator--debug
               (message "stack-topmost: %s" stack-topmost))
              (let ((input-tape-index (car stack-topmost))
                    (first-length (car (cdr stack-topmost)))
                    (first (car (cdr (cdr stack-topmost))))
                    (keep-looking t))
                (while (and
                        keep-looking
                        (< input-tape-index input-tape-length)
                        (< first-length k))
                  (let ((symbol (nth input-tape-index input-tape)))
                    (parser-generator--debug
                     (message "symbol index: %s from %s is: %s" input-tape-index input-tape symbol))
                    (cond
                     ((parser-generator--valid-e-p symbol)
                      (if disallow-e-first
                          (when (> first-length 0)
                            (setq first (append first (list symbol)))
                            (setq first-length (1+ first-length)))
                        (setq first (append first (list symbol)))
                        (setq first-length (1+ first-length)))
                      (setq keep-looking nil))

                     ((parser-generator--valid-terminal-p symbol)
                      (setq first (append first (list symbol)))
                      (setq first-length (1+ first-length)))

                     ((parser-generator--valid-non-terminal-p symbol)
                      (parser-generator--debug
                       (message "non-terminal symbol: %s" symbol))
                      (let ((symbol-f-set))
                        (if (and
                             disallow-e-first
                             (= first-length 0))
                            (setq symbol-f-set (nth 1 (gethash symbol parser-generator--f-free-sets)))
                          (setq symbol-f-set (nth 1 (gethash symbol parser-generator--f-sets))))
                        (parser-generator--debug
                         (message "symbol-f-set: %s" symbol-f-set))
                        (if (not symbol-f-set)
                            (progn
                              (parser-generator--debug
                               (message "empty symbol-f-set, so stop looking"))
                              (setq keep-looking nil))

                          ;; Handle this scenario here were a non-terminal can result in different FIRST sets
                          (when (> (length symbol-f-set) 1)
                            (let ((symbol-f-set-index 1)
                                  (symbol-f-set-length (length symbol-f-set)))
                              (while (< symbol-f-set-index symbol-f-set-length)
                                (let ((symbol-f-set-element (nth symbol-f-set-index symbol-f-set)))
                                  (let ((alternative-first-length (+ first-length (length symbol-f-set-element)))
                                        (alternative-first (append first symbol-f-set-element))
                                        (alternative-tape-index (1+ input-tape-index)))
                                    (parser-generator--debug
                                     (message "alternative-first: %s" alternative-first))
                                    (push `(,alternative-tape-index ,alternative-first-length ,alternative-first) stack)))
                                (setq symbol-f-set-index (1+ symbol-f-set-index)))))

                          (parser-generator--debug
                           (message "main-symbol-f-set: %s" (car symbol-f-set)))
                          (setq first-length (+ first-length (length (car symbol-f-set))))
                          (setq first (append first (car symbol-f-set))))))))
                  (setq input-tape-index (1+ input-tape-index)))
                (when (> first-length 0)
                  (parser-generator--debug
                   (message "push to first-list: %s to %s" first first-list))
                  (push first first-list))))))

        (setq first-list (sort first-list 'parser-generator--sort-list))
        first-list)))

;; Definition at p. 343
(defun parser-generator--follow (β)
  "Calculate follow-set of Β.  FOLLOW(β) = w, w is the set {w | S =>* αβγ and w is in FIRST(γ)}."
  ;; Make sure argument is a list
  (unless (listp β)
    (setq β (list β)))
  (let ((follow-set nil)
        (match-length (length β)))
    ;; Iterate all productions in grammar
    (let ((productions (parser-generator--get-grammar-productions)))
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
                              (push `(,parser-generator--e-identifier) follow-set)
                            ;; Otherwise add FOLLOW(rest) to follow-set
                            (let ((rest (nthcdr (1+ rhs-index) rhs)))
                              (let ((first-set (parser-generator--first rest)))
                                (setq follow-set (append first-set follow-set)))))
                          (setq match-index 0)))
                    (when (> match-index 0)
                      (setq match-index 0))))
                (setq rhs-index (1+ rhs-index))))))))
    (when (> (length follow-set) 0)
      (setq follow-set (parser-generator--distinct follow-set)))
    follow-set))


(provide 'parser-generator)

;;; parser-generator.el ends here
