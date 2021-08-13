;;; parser-generator-lr.el --- LR(k) Parser Generator -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator)
(require 'parser-generator-lex-analyzer)


;;; Variables:


(defvar
  parser-generator-lr--action-tables
  nil
  "Action-tables for grammar.")

(defvar
  parser-generator-lr--distinct-action-tables
  nil
  "Distinct action-tables for grammar.")

(defvar
  parser-generator-lr--distinct-goto-tables
  nil
  "Distinct goto-tables.")

(defvar
  parser-generator-lr--goto-tables
  nil
  "Goto-tables for grammar.")

(defvar
  parser-generator-lr--context-sensitive-precedence-attribute
  nil
  "Attribute used for context-sensitive-precedence.")

(defvar
  parser-generator-lr--global-precedence-attributes
  nil
  "Global precedence attributes.")

(defvar
  parser-generator-lr--precedence-comparison-function
  nil
  "Function to calculate precedence.")

(defvar
  parser-generator-lr--symbol-precedence-value
  nil
  "Table of the precedence value of all symbols.")

(defvar
  parser-generator-lr--symbol-precedence-type
  nil
  "Table of the precedence type of all symbols.")

(defvar
  parser-generator-lr--production-number-precedence-value
  nil
  "Table of the precedence value of all production numbers.")

(defvar
  parser-generator-lr--production-number-precedence-type
  nil
  "Table of the precedence type of all production numbers.")


;; Main Algorithms


(defun parser-generator-lr--get-symbol-precedence-value (symbol)
  "Get the precedence value of SYMBOL."
  (unless parser-generator-lr--symbol-precedence-value
    (error "Missing table for symbol precedence value!"))
  (gethash
   symbol
   parser-generator-lr--symbol-precedence-value))

(defun parser-generator-lr--get-symbol-precedence-type (symbol)
  "Get the precedence type of SYMBOL."
  (unless parser-generator-lr--symbol-precedence-type
    (error "Missing table for symbol precedence type!"))
  (gethash
   symbol
   parser-generator-lr--symbol-precedence-type))

(defun parser-generator-lr--get-production-number-precedence-value (production-number)
  "Get the precedence value of PRODUCTION-NUMBER."
  (unless parser-generator-lr--production-number-precedence-value
    (error "Missing table for production number precedence value!"))
  (gethash
   production-number
   parser-generator-lr--production-number-precedence-value))

(defun parser-generator-lr--get-production-number-precedence-type (production-number)
  "Get the precedence type of PRODUCTION-NUMBER."
  (unless parser-generator-lr--production-number-precedence-type
    (error "Missing table for production number precedence type!"))
  (gethash
   production-number
   parser-generator-lr--production-number-precedence-type))

(defun parser-generator-lr--generate-precedence-tables ()
  "Generate tables needed to determine precedence."

  ;; Initialize hash-maps for precedence
  (setq
   parser-generator-lr--symbol-precedence-value
   (make-hash-table :test 'equal))
  (setq
   parser-generator-lr--symbol-precedence-type
   (make-hash-table :test 'equal))
  (setq
   parser-generator-lr--production-number-precedence-value
   (make-hash-table :test 'equal))
  (setq
   parser-generator-lr--production-number-precedence-type
   (make-hash-table :test 'equal))

  (let ((global-precedence-attributes-table
         (make-hash-table :test 'equal)))
    (when parser-generator-lr--global-precedence-attributes

      ;; Build hash-map of all precedence attributes
      (dolist (item parser-generator-lr--global-precedence-attributes)
        (puthash
         item
         t
         global-precedence-attributes-table))

      ;; Go through global declaration in search of precedence attributes
      (let ((line-index 0))
        (dolist (line parser-generator--global-declaration)
          (let ((attribute (car line))
                (items (cdr line)))

            ;; Is it a precedence-attribute?
            (when
                (gethash
                 attribute
                 global-precedence-attributes-table)
              (dolist (item items)

                ;; Store value
                (puthash
                 item
                 line-index
                 parser-generator-lr--symbol-precedence-value)

                ;; Store type
                (puthash
                 item
                 attribute
                 parser-generator-lr--symbol-precedence-type))))
          (setq
           line-index
           (1+ line-index))))

      ;; Go through production-numbers
      (let ((productions (parser-generator--get-grammar-productions))
            (production-number 0))
        (dolist (production productions)
          (let ((production-precedence-value)
                (production-precedence-type))

            ;; 1. Look for attributes
            ;; 2. Look for precedence-attribute
            ;; 3. Look for value and type of precedence-attribute
            (when parser-generator-lr--context-sensitive-precedence-attribute
              (let ((production-attributes
                     (parser-generator--get-grammar-context-sensitive-attributes-by-production-number
                      production-number)))
                (when production-attributes
                  (let ((production-precedence-attribute
                         (plist-get
                          production-attributes
                          parser-generator-lr--context-sensitive-precedence-attribute)))
                    (when production-precedence-attribute
                      (let ((production-precedence-attribute-value
                             (parser-generator-lr--get-symbol-precedence-value
                              production-precedence-attribute))
                            (production-precedence-attribute-type
                             (parser-generator-lr--get-symbol-precedence-type
                              production-precedence-attribute)))
                        (when (and
                               production-precedence-attribute-value
                               production-precedence-attribute-type)
                          (setq
                           production-precedence-value
                           production-precedence-attribute-value
                           )
                          (setq
                           production-precedence-type
                           production-precedence-attribute-type))))))))

            ;; 1. If none was found
            ;; 2. Iterate symbols of production RHS
            ;; 3. If found a last terminal of RHS
            ;; 4. Look for a precedence value and type of it
            (unless production-precedence-value
              (let ((rhs (car (cdr production)))
                    (rhs-last-terminal))
                (dolist (rhs-element rhs)
                  (when (parser-generator--valid-terminal-p
                         rhs-element)
                    (setq
                     rhs-last-terminal
                     rhs-element)))

                (when rhs-last-terminal
                  (let ((terminal-precedence-value
                         (parser-generator-lr--get-symbol-precedence-value
                          rhs-last-terminal))
                        (terminal-precedence-type
                         (parser-generator-lr--get-symbol-precedence-type
                          rhs-last-terminal)))
                    (when (and
                           terminal-precedence-value
                           terminal-precedence-type)
                      (setq
                       production-precedence-value
                       terminal-precedence-value)
                      (setq
                       production-precedence-type
                       terminal-precedence-type))))))

            (when (and
                   production-precedence-type
                   production-precedence-value)
              (puthash
               production-number
               production-precedence-value
               parser-generator-lr--production-number-precedence-value)
              (puthash
               production-number
               production-precedence-type
               parser-generator-lr--production-number-precedence-type))
            (setq
             production-number
             (1+ production-number)))))

      )))

(defun parser-generator-lr-generate-parser-tables ()
  "Generate parsing tables for grammar."
  (message "\nStarting generation of parser-tables..\n")
  (parser-generator-lr--generate-precedence-tables)
  (let ((table-lr-items
         (parser-generator-lr--generate-goto-tables)))
    (parser-generator-lr--generate-action-tables
     table-lr-items)
    (message "\nCompleted generation of parser-tables.\n")
    table-lr-items))

(defun parser-generator-lr--get-expanded-action-tables ()
  "Get expanded ACTION-tables."
  (let ((distinct-indexes
         (parser-generator--hash-to-list
          parser-generator-lr--action-tables))
        (action-tables))
    (dolist (action-row distinct-indexes)
      (let ((action-index (car action-row))
            (distinct-index (car (cdr action-row))))
        (push
         `(,action-index
           ,(gethash
              distinct-index
              parser-generator-lr--distinct-action-tables))
         action-tables)))
    (reverse action-tables)))

(defun parser-generator-lr--get-expanded-goto-tables ()
  "Get expanded GOTO-tables."
  (let ((distinct-indexes
         (parser-generator--hash-to-list
          parser-generator-lr--goto-tables))
        (goto-tables))
    (dolist (goto-row distinct-indexes)
      (let ((goto-index (car goto-row))
            (distinct-index (car (cdr goto-row))))
        (push
         `(,goto-index
           .  (,(gethash
                 distinct-index
                 parser-generator-lr--distinct-goto-tables)))
         goto-tables)))
    (reverse goto-tables)))

;; Algorithm 5.11, p. 393
(defun parser-generator-lr--generate-action-tables (table-lr-items)
  "Generate action-tables for lr-grammar based on TABLE-LR-ITEMS."
  (message "\nStarting generation of action-tables..\n")
  (let ((action-tables)
        (states
         '(shift reduce error))
        (added-actions
         (make-hash-table :test 'equal))
        (index-symbols
         (make-hash-table :test 'equal))
        (goto-tables
         (parser-generator--hash-to-list
          parser-generator-lr--goto-tables
          t))
        (found-accept))
    (dolist (goto-table goto-tables)
      (let ((goto-index (car goto-table))
            (found-action nil)
            (action-table))
        (let ((lr-items
               (gethash
                goto-index
                table-lr-items)))
          (let ((lr-items-length
                 (length lr-items)))

            ;; Where u is in (T U e)*k
            (dolist (state states)
              (let ((lr-item)
                    (lr-item-index 0)
                    (continue-loop t))
                (while (and
                        (< lr-item-index lr-items-length)
                        continue-loop)
                  (setq
                   lr-item
                   (nth lr-item-index lr-items))
                  (cond

                   ((eq state 'shift)
                    ;; (a) f(u) = shift if [A -> B . C, v] is in LR-items, C != e and u is in EFF(Cv)
                    (when (nth 2 lr-item)
                      (let ((C (nth 2 lr-item))
                            (v (nth 3 lr-item)))
                        (let ((Cv (append C v)))
                          (parser-generator--debug
                           (message
                            "Cv: %s from %s + %s"
                            Cv
                            C
                            v))
                          (when Cv
                            (let
                                ((eff
                                  (parser-generator--e-free-first
                                   Cv)))
                              (parser-generator--debug
                               (message
                                "E-FREE-FIRST %s = %s"
                                Cv
                                eff))
                              (if eff
                                  ;; Go through eff-items and see if any item is a valid look-ahead of grammar
                                  ;; in that case save in action table a shift action here
                                  (let ((eff-index 0)
                                        (eff-item)
                                        (eff-length (length eff)))
                                    (while
                                        (<
                                         eff-index
                                         eff-length)
                                      (setq
                                       eff-item
                                       (parser-generator--first-to-lookahead
                                        (nth eff-index eff)))
                                      (parser-generator--debug
                                       (message
                                        "eff-item: %s"
                                        eff-item))
                                      (if
                                          (parser-generator--valid-look-ahead-p
                                           eff-item)
                                          (let
                                              ((hash-key
                                                (format
                                                 "%s-%s-%S"
                                                 goto-index
                                                 state
                                                 eff-item)))
                                            (parser-generator--debug
                                             (message
                                              "Valid look-ahead: %s"
                                              eff-item))
                                            (if (gethash
                                                 hash-key
                                                 added-actions)
                                                (parser-generator--debug
                                                 (message
                                                  "Duplicate action: %s"
                                                  hash-key))
                                              (parser-generator--debug
                                               (message
                                                "New action: %s"
                                                hash-key))
                                              (puthash
                                               hash-key
                                               t
                                               added-actions)
                                              (if
                                                  (and
                                                   (=
                                                    parser-generator--look-ahead-number
                                                    0)
                                                   (equal
                                                    eff-item
                                                    `(,parser-generator--eof-identifier)))
                                                  ;; An extra column for '$' (end of input) is added to the action table that contains acc for every item set that contains an item of the form S → w • eof.
                                                  (let ((action-item
                                                         (list
                                                          eff-item
                                                          'accept)))
                                                    ;; Add symbol to hash-table to
                                                    ;; enable conflict resolution
                                                    (let ((index-hash-key
                                                           (format
                                                            "%s-%S"
                                                            goto-index
                                                            eff-item)))
                                                      (unless
                                                          (gethash
                                                           index-hash-key
                                                           index-symbols)
                                                        (puthash
                                                         index-hash-key
                                                         action-item
                                                         index-symbols)))
                                                    (push
                                                     action-item
                                                     action-table)
                                                    (setq
                                                     found-accept
                                                     t))
                                                (let ((action-item
                                                       (list
                                                        eff-item
                                                        'shift)))
                                                  ;; Add symbol to hash-table to
                                                  ;; enable conflict resolution
                                                  (let ((index-hash-key
                                                         (format
                                                          "%s-%S"
                                                          goto-index
                                                          eff-item)))
                                                    (unless
                                                        (gethash
                                                         index-hash-key
                                                         index-symbols)
                                                      (puthash
                                                       index-hash-key
                                                       action-item
                                                       index-symbols)))
                                                  (push
                                                   action-item
                                                   action-table))))
                                            (setq
                                             found-action
                                             t))
                                        (parser-generator--debug
                                         (message
                                          "Not valid look-ahead: %s"
                                          eff-item)))
                                      (setq
                                       eff-index
                                       (1+ eff-index))))
                                (parser-generator--debug
                                 (message
                                  "E-FREE-FIRST is empty for %s"
                                  Cv)))))))))

                   ((eq state 'reduce)
                    ;; (b) f(u) = reduce i if [A -> B ., u] is in a and A -> B is production i in P, i > 1
                    (when (and
                           (nth 0 lr-item)
                           (not (nth 2 lr-item)))
                      (let ((A (nth 0 lr-item))
                            (B (nth 1 lr-item))
                            (u (nth 3 lr-item)))
                        (unless B
                          (setq
                           B
                           (list
                            parser-generator--e-identifier)))
                        (if
                            (=
                             parser-generator--look-ahead-number
                             0)

                            ;; LR(0) uses a different algorithm for determining reduce actions
                            (unless (nth 2 lr-item)
                              (let ((production (list A B)))
                                (let
                                    ((production-number
                                      (parser-generator--get-grammar-production-number
                                       production)))
                                  (unless production-number
                                    (error
                                     "Expecting production number for %s from LR-item %s!"
                                     production
                                     lr-item))

                                  (parser-generator--debug
                                   (message "production: %s (%s)" production production-number)
                                   (message "u: %s" u))
                                  (push
                                   (list
                                    nil
                                    'reduce
                                    production-number)
                                   action-table)
                                  (setq
                                   found-action
                                   t))))

                          (when (parser-generator--valid-look-ahead-p u)
                            (let ((production (list A B)))
                              (let
                                  ((production-number
                                    (parser-generator--get-grammar-production-number
                                     production)))
                                (unless production-number
                                  (error
                                   "Expecting production number for %s from LR-item %s!"
                                   production
                                   lr-item))
                                (let ((skip-symbol)
                                      (hash-key
                                       (format
                                        "%s-%s-%S-%s"
                                        goto-index
                                        state
                                        u
                                        production-number)))

                                  ;; Add symbol to hash-table to
                                  ;; enable conflict resolution
                                  (let ((index-hash-key
                                         (format
                                          "%s-%S"
                                          goto-index
                                          u)))

                                    ;; Check if we have an action on this symbol already
                                    (when
                                        (gethash
                                         index-hash-key
                                         index-symbols)
                                      (if
                                          (and
                                           parser-generator-lr--precedence-comparison-function
                                           parser-generator-lr--global-precedence-attributes)
                                          (let ((a
                                                 (list u 'reduce production-number))
                                                (b
                                                 (gethash
                                                  index-hash-key
                                                  index-symbols)))
                                            (if
                                                (parser-generator-lr--action-takes-precedence-p
                                                 (car (last u))
                                                 production-number
                                                 (nth 2 b))
                                                (progn
                                                  (parser-generator--debug
                                                   (message
                                                    "'%s' takes precedence over '%s'"
                                                    a
                                                    b))
                                                  ;; Remove b from added-actions
                                                  (let ((new-action-table))
                                                    (dolist (action-item action-table)
                                                      (unless
                                                          (equal
                                                           action-item
                                                           b)
                                                        (push
                                                         action-item
                                                         new-action-table)))
                                                    (setq
                                                     action-table
                                                     (reverse
                                                      new-action-table))))
                                              (parser-generator--debug
                                               (message
                                                "'%s' takes precedence over '%s'"
                                                b
                                                a))
                                              ;; Skip rest of this iteration
                                              (setq
                                               skip-symbol
                                               t)))
                                        (let ((conflicted-item
                                               (gethash
                                                index-hash-key
                                                index-symbols)))
                                          (error
                                           "Reduce/%S conflict for %S in state %S"
                                           (car (cdr conflicted-item))
                                           u
                                           goto-index
                                           ))))

                                    (unless
                                        (or
                                         skip-symbol
                                         (gethash
                                          hash-key
                                          added-actions))
                                      (puthash
                                       hash-key
                                       t
                                       added-actions)

                                      (parser-generator--debug
                                       (message "production: %s (%s)" production production-number)
                                       (message "u: %s" u))

                                      (if (and
                                           (= production-number 0)
                                           (>= (length u) 1)
                                           (parser-generator--valid-eof-p
                                            (nth (1- (length u)) u)))
                                          (let ((action-item
                                                 (list
                                                  u
                                                  'accept)))
                                            (puthash
                                             index-hash-key
                                             action-item
                                             index-symbols)

                                            ;; Reduction by first production
                                            ;; of empty look-ahead means grammar has been accepted
                                            (push
                                             action-item
                                             action-table)
                                            (setq
                                             found-accept
                                             t)
                                            (setq
                                             found-action
                                             t))

                                        ;; save reduction action in action table
                                        (let ((action-item
                                               (list
                                                u
                                                'reduce
                                                production-number)))
                                          (puthash
                                           index-hash-key
                                           action-item
                                           index-symbols)
                                          (push
                                           action-item
                                           action-table)
                                          (setq
                                           found-action
                                           t)))))))))))))

                   ((eq state 'error)
                    (unless found-action
                      (error
                       "Failed to find any action in set %d: %s"
                       goto-index
                       lr-items))
                    (setq
                     continue-loop
                     nil)))
                  (setq
                   lr-item-index
                   (1+ lr-item-index)))))))
        (parser-generator--debug
         (message "%s actions %s" goto-index action-table))
        (when action-table
          (message
           "ACTION-TABLE (%d): %S\n"
           goto-index
           action-table)
          (push
           (list
            goto-index
            (sort action-table 'parser-generator--sort-list))
           action-tables))))
    (unless found-accept
      (error "Failed to find an accept action in the generated action-tables!"))
    (setq action-tables (nreverse action-tables))
    (setq
     parser-generator-lr--action-tables
     (make-hash-table :test 'equal))
    (setq
     parser-generator-lr--distinct-action-tables
     (make-hash-table :test 'equal))
    (let ((table-length (length action-tables))
          (table-index 0)
          (action-table-to-distinct-index
           (make-hash-table :test 'equal)))
      (while (< table-index table-length)
        (let ((action-table
               (car (cdr (nth table-index action-tables)))))
          (let ((action-table-hash
                 (format
                  "%S"
                  action-table)))
            (unless
                (gethash
                 action-table-hash
                 action-table-to-distinct-index)
              (puthash
               action-table-hash
               table-index
               action-table-to-distinct-index)
              (puthash
               table-index
               action-table
               parser-generator-lr--distinct-action-tables))
            (let ((action-table-index
                   (gethash
                    action-table-hash
                    action-table-to-distinct-index)))
              (puthash
               table-index
               action-table-index
               parser-generator-lr--action-tables))))
        (setq
         table-index
         (1+ table-index)))))
  (message "\nCompleted generation of action-tables..\n"))

;; Algorithm 5.9, p. 389
(defun parser-generator-lr--generate-goto-tables ()
  "Calculate set of valid LR(k) items for grammar and a GOTO-table."
  (message "\nStarting generation of goto-tables..\n")
  (parser-generator--debug
   (message "(parser-generator-lr--generate-goto-tables)"))
  (let ((lr-item-set-new-index 0)
        (marked-count 0)
        (total-count 1)
        (goto-table)
        (unmarked-lr-item-sets)
        (marked-lr-item-sets
         (make-hash-table :test 'equal))
        (next-symbols)
        (next-symbols-found
         (make-hash-table :test 'equal))
        (table-lr-items
         (make-hash-table :test 'equal)))

    (let ((e-set
           (parser-generator-lr--items-for-prefix
            parser-generator--e-identifier)))

      (parser-generator-lr--items-valid-p
       (list e-set)
       t)

      ;;(1) Place V(e) in S. The set V(e) is initially unmarked.
      (push
       `(,lr-item-set-new-index ,e-set)
       unmarked-lr-item-sets)
      (setq
       lr-item-set-new-index
       (1+ lr-item-set-new-index))
      (let ((e-set-hash-key
             (format "%S" e-set)))
        ;; Mark the initial set
        (puthash
         e-set-hash-key
         lr-item-set-new-index
         marked-lr-item-sets)))

    ;; (2) If a set of items a in S is unmarked
    ;; (3) Repeat step (2) until all sets of items in S are marked.
    (let ((popped-item)
          (lr-item-set-index)
          (lr-items)
          (goto-table-table))
      (while unmarked-lr-item-sets

        (setq
         popped-item
         (pop unmarked-lr-item-sets))
        (setq
         lr-item-set-index
         (car popped-item))
        (setq
         lr-items
         (car (cdr popped-item)))
        (parser-generator--debug
         (message "lr-item-set-index: %S" lr-item-set-index)
         (message "marked lr-items: %S" lr-items)
         (message "popped-item: %s" popped-item))

        (puthash
         lr-item-set-index
         lr-items
         table-lr-items)
        (setq
         goto-table-table
         nil)

        ;; Build list of possible next-symbols
        ;; here that follows current set
        (setq next-symbols nil)
        (dolist (lr-item lr-items)
          (let ((symbols (nth 2 lr-item)))
            (when symbols
              ;; Convert symbols in grammar with attributes to simple symbols
              (let ((next-symbol
                     (car symbols)))
                (let ((temp-hash-key
                       (format
                        "%S"
                        (list
                         lr-item-set-index
                         next-symbol))))
                  (when
                      (and
                       (or
                        (parser-generator--valid-terminal-p
                         next-symbol)
                        (parser-generator--valid-non-terminal-p
                         next-symbol))
                       (not
                        (gethash
                         temp-hash-key
                         next-symbols-found)))
                    (push
                     next-symbol
                     next-symbols)
                    (puthash
                     temp-hash-key
                     t
                     next-symbols-found)))))

            ;; Sort next-symbols for a more deterministic result
            (when next-symbols
              (setq
               next-symbols
               (sort
                next-symbols
                'string-lessp)))))
        (parser-generator--debug
         (message "next-symbols: %s" next-symbols))

        ;; (2) By computing for each X in N u E, GOTO (a, X). (Algorithm 5.8 can be used here.)
        ;; V(X1,...,Xi) = GOTO(V(X1,...,Xi-1), Xi)
        (when next-symbols
          (dolist (symbol next-symbols)
            (parser-generator--debug
             (message "goto-symbol: %s" symbol))

            (let ((prefix-lr-items
                   (parser-generator-lr--items-for-goto
                    lr-items
                    symbol)))

              ;; If a' = GOTO(a, X) is nonempty
              (when prefix-lr-items
                (let ((prefix-lr-items-hash-key
                       (format
                        "%S"
                        prefix-lr-items)))

                  (parser-generator--debug
                   (message
                    "GOTO(%S, %S) = %S"
                    lr-items
                    symbol
                    prefix-lr-items))

                  ;; and set is not already in S
                  (let ((goto
                         (gethash
                          prefix-lr-items-hash-key
                          marked-lr-item-sets)))
                    (if goto
                        (progn
                          (parser-generator--debug
                           (message
                            "Set already exists in: %s set: %s"
                            goto
                            prefix-lr-items))
                          (push
                           `(,symbol ,goto)
                           goto-table-table))

                      (parser-generator--debug
                       (message
                        "Set is new: %s"
                        prefix-lr-items))

                      (parser-generator-lr--items-valid-p
                       (list prefix-lr-items)
                       t)

                      ;; Note that GOTO(a, X) will always be empty if all items in a
                      ;; have the dot at the right end of the production

                      ;; then add a' to S as an unmarked set of items
                      (push
                       `(,symbol ,lr-item-set-new-index)
                       goto-table-table)
                      (push
                       `(,lr-item-set-new-index ,prefix-lr-items)
                       unmarked-lr-item-sets)
                      (setq
                       total-count
                       (1+ total-count))
                      ;; (2) Mark a
                      (puthash
                       prefix-lr-items-hash-key
                       lr-item-set-new-index
                       marked-lr-item-sets)
                      (setq
                       lr-item-set-new-index
                       (1+ lr-item-set-new-index)))))))))

        (setq
         goto-table-table
         (sort
          goto-table-table
          'parser-generator--sort-list))
        (message
         "GOTO-TABLE (%d): %S\n"
         lr-item-set-index
         goto-table-table)
        (push
         `(
           ,lr-item-set-index
           ,goto-table-table)
         goto-table)
        (setq
         marked-count
         (1+ marked-count))
        (message
         "Progress: %s / %s = %d%%\n"
         marked-count
         total-count
         (* 100 (/ (float marked-count) (float total-count))))))

    (setq
     goto-table
     (sort
      goto-table
      'parser-generator--sort-list))
    (setq
     parser-generator-lr--goto-tables
     (make-hash-table :test 'equal))
    (setq
     parser-generator-lr--distinct-goto-tables
     (make-hash-table :test 'equal))
    (let ((table-length (length goto-table))
          (table-index 0)
          (distinct-goto-table-index 0)
          (table-goto-table-to-index (make-hash-table :test 'equal)))
      (while (< table-index table-length)
        (let
            ((goto-table
              (car (cdr (nth table-index goto-table)))))
          (let
              ((goto-table-hash-key
                (format
                 "%S"
                 goto-table)))
            (unless
                (gethash
                 goto-table-hash-key
                 table-goto-table-to-index)
              (puthash
               goto-table-hash-key
               distinct-goto-table-index
               table-goto-table-to-index)
              (puthash
               distinct-goto-table-index
               goto-table
               parser-generator-lr--distinct-goto-tables)
              (setq
               distinct-goto-table-index
               (1+ distinct-goto-table-index)))
          (let
              ((goto-table-index
                (gethash
                 goto-table-hash-key
                 table-goto-table-to-index)))
            (puthash
             table-index
             goto-table-index
             parser-generator-lr--goto-tables))))
        (setq table-index (1+ table-index))))
    (message "\nCompleted generation of goto-tables.\n")
    table-lr-items))

;; Algorithm 5.10, p. 391
(defun parser-generator-lr--items-valid-p
    (
     lr-item-sets
     &optional signal-on-false
     )
  "Return whether the set collection LR-ITEM-SETS is valid or not, optionally SIGNAL-ON-FALSE."
  (parser-generator--debug
   (message "lr-item-sets: %s" lr-item-sets))
  (let ((valid-p t)
        (set-index 0)
        (set)
        (sets-length (length lr-item-sets))
        (set-length 0)
        (a)
        (a-look-ahead)
        (a-follow)
        (a-index 0)
        (a-production)
        (a-production-number)
        (b)
        (b-suffix)
        (b-follow)
        (b-suffix-follow)
        (b-suffix-follow-eff)
        (b-index 0)
        (b-production)
        (b-production-number))

    ;; Iterate each set
    (while (and
            valid-p
            (< set-index sets-length))
      (setq set (nth set-index lr-item-sets))
      (parser-generator--debug
       (message "set: %s" set))

      ;; Iterate each set
      (setq
       a-index
       0)
      (setq
       b-index
       0)
      (setq
       set-length
       (length set))
      (while (and
              valid-p
              (< a-index set-length))
        (setq
         a
         (nth a-index set))
        (setq
         a-look-ahead
         (nth 2 a))

        (parser-generator--debug
         (message "a: %S" a)
         (message "a-look-ahead: %s" a-look-ahead))

        ;; The only sets of LR items which need to be tested are those that contain a dot at the right end of a production
        ;; these states are points of reduction
        (when (and
               (nth 1 a)
               (not a-look-ahead))
          (setq
           a-follow
           (nth 3 a))
          (setq
           a-production
           (list
            (nth 0 a)
            (nth 1 a)))
          (setq
           a-production-number
           (parser-generator--get-grammar-production-number
            a-production))

          (parser-generator--debug
           (message "a-follow: %s" a-follow)
           (message "a-production: %S" a-production)
           (message "a-production-number: %S" a-production-number))

          ;; Iterate each set again
          (while (and
                  valid-p
                  (< b-index set-length))
            ;; Make sure it's not the same rule
            (unless
                (= a-index b-index)
              (setq
               b
               (nth b-index set))
              (parser-generator--debug
               (message "b: %s" b))

              (setq
               b-suffix
               (nth 2 b))
              (setq
               b-follow
               (nth 3 b))
              (setq
               b-suffix-follow
               (append
                b-suffix
                b-follow))
              (setq
               b-suffix-follow-eff
               (parser-generator--e-free-first
                b-suffix-follow))

              (let ((b-lhs)
                    (b-rhs))
                (if (listp (nth 0 b))
                    (setq b-lhs (nth 0 b))
                  (setq b-lhs (list (nth 0 b))))
                (if (nth 2 b)
                    (setq b-rhs (append (nth 1 b) (nth 2 b)))
                  (setq b-rhs (nth 1 b)))
                (setq b-production (list b-lhs b-rhs)))
              (setq
               b-production-number
               (parser-generator--get-grammar-production-number
                b-production))

              (parser-generator--debug
               (message "b-production: %S" b-production)
               (message "b-production-number: %S" b-production-number)
               (message "b-suffix: %s" b-suffix)
               (message "b-follow: %s" b-follow)
               (message "b-suffix-follow: %s" b-suffix-follow)
               (message "b-suffix-follow-eff: %s" b-suffix-follow-eff))

              (dolist
                  (b-suffix-follow-eff-item
                   b-suffix-follow-eff)
                (when (equal
                       a-follow
                       b-suffix-follow-eff-item)
                  (if
                      ;; If it's the same following symbol but we have
                      ;; any production-number we might be able to continue
                      ;; if there are precedence rules
                      (or
                       a-production-number
                       b-production-number)
                      (progn
                        (unless
                            (or
                             (parser-generator-lr--action-takes-precedence-p
                              a-follow
                              a-production-number
                              b-production-number)
                             (parser-generator-lr--action-takes-precedence-p
                              b-follow
                              b-production-number
                              a-production-number))
                          (when
                              signal-on-false
                            (error
                             "Inconsistent grammar! '%S' (index: %d) conflicts with '%S' (index: %d) with look-ahead '%S' in sets:\n%S"
                             a
                             a-index
                             b
                             b-index
                             b-suffix-follow-eff-item
                             lr-item-sets))
                          (setq valid-p nil)))
                    (when
                        signal-on-false
                      (error
                       "Inconsistent grammar! '%S' (index: %d) conflicts with '%S' (index: %d) with look-ahead '%S' in sets:\n%S"
                       a
                       a-index
                       b
                       b-index
                       b-suffix-follow-eff-item
                       lr-item-sets))
                    (setq valid-p nil)))))
            (setq b-index (1+ b-index))))
        (setq a-index (1+ a-index)))
      (setq set-index (1+ set-index)))
    valid-p))

(defun parser-generator-lr--action-takes-precedence-p (symbol a-production-number &optional b-production-number)
  "Return t if reduce action of SYMBOL at A-PRODUCTION-NUMBER takes precedence over shift action.  Optionally is b is a reduction at at B-PRODUCTION-NUMBER."
  (let* ((a-precedence-type
          (parser-generator-lr--get-symbol-precedence-type
           symbol))
         (a-precedence-value
          (parser-generator-lr--get-symbol-precedence-value
           symbol))
         (b-precedence-type
          a-precedence-type)
         (b-precedence-value
          a-precedence-value))

    ;; Context-sensitive precedence takes precedence over
    ;; global precedence
    (let ((a-production-precedence-value
           (parser-generator-lr--get-production-number-precedence-value
            a-production-number))
          (a-production-precedence-type
           (parser-generator-lr--get-production-number-precedence-type
            a-production-number)))
      (when (and
             a-production-precedence-type
             a-production-precedence-value)
        (setq
         a-precedence-type
         a-production-precedence-type)
        (setq
         a-precedence-value
         a-production-precedence-value)))

    (when b-production-number
      (let ((b-production-precedence-value
             (parser-generator-lr--get-production-number-precedence-value
              b-production-number))
            (b-production-precedence-type
             (parser-generator-lr--get-production-number-precedence-type
              b-production-number)))
        (when (and
               b-production-precedence-type
               b-production-precedence-value)
          (setq
           b-precedence-type
           b-production-precedence-type)
          (setq
           b-precedence-value
           b-production-precedence-value))))

    (funcall
     parser-generator-lr--precedence-comparison-function
     a-precedence-type
     a-precedence-value
     b-precedence-type
     b-precedence-value)))

;; Algorithm 5.8, p. 386
(defun parser-generator-lr--items-for-prefix (γ)
  "Calculate valid LR-items for the viable prefix Γ."
  (let ((start (parser-generator--get-grammar-start)))
    (unless (listp γ)
      (setq γ (list γ)))
    (unless (parser-generator--valid-sentential-form-p γ)
      (error "Invalid sentential form γ!"))
    (parser-generator--debug
     (message "γ: %s" γ))

    (let ((lr-item-exists (make-hash-table :test 'equal)))

      ;; 1

      ;; Iterate all productions in grammar
      (let ((lr-items-e)
            (start-productions
             (parser-generator--get-grammar-rhs start))
            (e-list parser-generator--e-identifier)
            (eof-list (parser-generator--generate-list-of-symbol
                       parser-generator--look-ahead-number
                       parser-generator--eof-identifier)))

        ;; (a)
        (dolist (rhs start-productions)
          ;; Add [S -> . α] to V(e)
          (if (= parser-generator--look-ahead-number 0)
              ;; A dot-look-ahead is only used for k >= 1
              (progn
                (push
                 `(,(list start) nil ,rhs)
                 lr-items-e)
                (puthash
                 `(,e-list ,(list start) nil ,rhs)
                 t
                 lr-item-exists))
            (push
             `(,(list start) nil ,rhs ,eof-list)
             lr-items-e)
            (puthash
             `(,e-list ,(list start) nil ,rhs ,eof-list)
             t
             lr-item-exists)))

        ;; (b) Iterate every item in v-set(e), if [A -> . Bα, u] is an item and B -> β is in P
        ;; then for each x in FIRST(αu) add [B -> . β, x] to v-set(e), provided it is not already there
        (let ((found-new t))

          ;; Repeat this until no new item is found
          (while found-new
            (setq found-new nil)

            ;; Iterate every item in V(e)
            (dolist (item lr-items-e)
              (let ((prefix (nth 1 item))
                    (rhs (nth 2 item))
                    (suffix (nth 3 item)))

                ;; Without prefix
                (unless prefix

                  ;; Check if RHS starts with a non-terminal
                  (let ((rhs-first (car rhs)))
                    (parser-generator--debug
                     (message "rhs-first: %s" rhs-first))
                    (if
                        (parser-generator--valid-non-terminal-p
                         rhs-first)
                        (let ((rhs-rest (append (cdr rhs) suffix)))
                          (let ((rhs-rest-first
                                 (parser-generator--first
                                  rhs-rest
                                  nil
                                  t
                                  t)))
                            (parser-generator--debug
                             (message "is non-terminal")
                             (message "rhs-rest: %s from %s + %s" rhs-rest (cdr rhs) suffix)
                             (message "rhs-rest-first: %s" rhs-rest-first))
                            (unless rhs-rest-first
                              (setq rhs-rest-first `(,eof-list)))
                            (let ((sub-production
                                   (parser-generator--get-grammar-rhs
                                    rhs-first)))
                              (parser-generator--debug
                               (message "sub-production: %s" sub-production))

                              ;; For each production with B as LHS
                              (dolist (sub-rhs sub-production)

                                ;; Set follow to nil if it's the e-identifier
                                (when (and
                                       (= (length sub-rhs) 1)
                                       (parser-generator--valid-e-p (car sub-rhs)))
                                  (setq sub-rhs nil))

                                (parser-generator--debug
                                 (message "sub-rhs: %s" sub-rhs))

                                ;; For each x in FIRST(αu)
                                (dolist (f rhs-rest-first)
                                  (parser-generator--debug
                                   (message "f: %s" f))

                                  ;; Add [B -> . β, x] to V(e), provided it is not already there
                                  (if (= parser-generator--look-ahead-number 0)

                                      ;; A dot look-ahead is only used for k >= 1
                                      (let ((temp-hash-key
                                             (format
                                              "%S"
                                              `(,e-list ,(list rhs-first) nil ,sub-rhs))))
                                        (unless
                                            (gethash
                                             temp-hash-key
                                             lr-item-exists)
                                          (puthash
                                           temp-hash-key
                                           t
                                           lr-item-exists)
                                          (push
                                           `(,(list rhs-first) nil ,sub-rhs)
                                           lr-items-e)

                                          ;; (c) Repeat (b) until no more items can be added to V(e)
                                          (setq found-new t)))

                                    (let ((temp-hash-key
                                           (format
                                            "%S"
                                            `(,e-list ,(list rhs-first) nil ,sub-rhs ,f))))
                                      (unless
                                          (gethash
                                           temp-hash-key
                                           lr-item-exists)
                                        (puthash
                                         temp-hash-key
                                         t
                                         lr-item-exists)
                                        (push
                                         `(,(list rhs-first) nil ,sub-rhs ,f)
                                         lr-items-e)

                                        ;; (c) Repeat (b) until no more items can be added to V(e)
                                        (setq found-new t)))))))))
                      (parser-generator--debug
                       (message "is not non-terminal")))))))))

        (parser-generator--debug
         (message "V(e) = %s" lr-items-e))

        (setq
         lr-items-e
         (sort
          lr-items-e
          'parser-generator--sort-list))

        ;; 2 Suppose that we have constructed V(X1,X2,...,Xi-1) we construct V(X1,X2,...,Xi) as follows:
        ;; Only do this step if prefix is not the e-identifier
        (let ((prefix-previous lr-items-e)
              (γ-length (length γ))
              (γ-index 0))
          (unless
              (and
               (= γ-length 1)
               (parser-generator--valid-e-p (car γ)))

            (while (and
                    (< γ-index γ-length)
                    prefix-previous)
              (let ((prefix))

                ;; Build next prefix of length k
                (setq prefix (nth γ-index γ))
                (setq γ-index (1+ γ-index))

                (let ((lr-new-item))
                  (setq
                   lr-new-item
                   (parser-generator-lr--items-for-goto
                    prefix-previous
                    prefix))

                  (parser-generator--debug
                   (message "prefix: %s" prefix)
                   (message "prefix-previous: %s" prefix-previous)
                   (message "lr-new-item: %s" lr-new-item))

                  (setq
                   prefix-previous
                   lr-new-item)))))

          (parser-generator--debug
           (message "γ: %s" γ))
          prefix-previous)))))

(defun parser-generator-lr--items-for-goto (previous-lr-item x)
  "Calculate LR-items for GOTO(PREVIOUS-LR-ITEM, X)."
  (let ((lr-new-item)
        (lr-item-exists
         (make-hash-table :test 'equal))
        (eof-list
         (parser-generator--generate-list-of-symbol
          parser-generator--look-ahead-number
          parser-generator--eof-identifier)))
    (parser-generator--debug
     (message "x: %s" x))

    ;; TODO Use caches to optimize this loop?
    (dolist (lr-item previous-lr-item)
      (let ((lr-item-lhs (nth 0 lr-item))
            (lr-item-prefix (nth 1 lr-item))
            (lr-item-suffix (nth 2 lr-item))
            (lr-item-look-ahead (nth 3 lr-item))
            (lr-item-suffix-first)
            (lr-item-suffix-rest))
        (setq
         lr-item-suffix-first
          (car lr-item-suffix))
        (setq
         lr-item-suffix-rest
         (cdr lr-item-suffix))

        (parser-generator--debug
         (message "lr-item: %s" lr-item)
         (message "lr-item-prefix: %s" lr-item-prefix)
         (message "lr-item-suffix: %s" lr-item-suffix)
         (message "lr-item-suffix-first: %s" lr-item-suffix-first)
         (message "lr-item-suffix-rest: %s" lr-item-suffix-rest)
         (message "lr-item-look-ahead: %s" lr-item-look-ahead))

        ;; (a) If [A -> a . XiB, u] is in V(X1,...,Xi-1)
        (when
            (equal
              lr-item-suffix-first
             x)

          ;; Add [A -> aXi . B, u] to V(X1,...,Xi)
          (let ((combined-prefix
                 (append
                  lr-item-prefix
                  (list
                   lr-item-suffix-first))))
            (let ((lr-new-item-1))
              (if
                  (=
                   parser-generator--look-ahead-number
                   0)
                  ;; Only k >= 1 needs dot look-ahead
                  (progn
                    (setq
                     lr-new-item-1
                     `(,lr-item-lhs
                       ,combined-prefix
                       ,lr-item-suffix-rest)))
                (setq
                 lr-new-item-1
                 `(,lr-item-lhs
                   ,combined-prefix
                   ,lr-item-suffix-rest
                   ,lr-item-look-ahead)))
              (parser-generator--debug
               (message
                "lr-new-item-1: %s"
                lr-new-item-1))
              (push
               lr-new-item-1
               lr-new-item))))))

    ;; (c) Repeat step (2b) until no more new items can be added to V(X1,...,Xi)
    (when lr-new-item
      (let ((added-new t))
        (while added-new
          (setq added-new nil)

          (dolist (lr-item lr-new-item)
            (let ((lr-item-suffix
                   (nth 2 lr-item)))
              (let ((lr-item-suffix-first
                     (car lr-item-suffix))
                    (lr-item-suffix-rest
                     (append
                      (cdr lr-item-suffix)
                      (nth 3 lr-item))))
                (parser-generator--debug
                 (message
                  "lr-item-suffix-first: %s from %s"
                  lr-item-suffix-first
                  lr-item-suffix)
                 (message
                  "lr-item-suffix-rest: %s from %s + %s"
                  lr-item-suffix-rest
                  (cdr lr-item-suffix)
                  (nth 3 lr-item)))

                ;; (b) If [A -> a . Bb, u] has been placed in V(X1,...,Xi)
                ;; and B -> D is in P
                (when
                    (and
                     lr-item-suffix-first
                     (parser-generator--valid-non-terminal-p
                      lr-item-suffix-first))

                  (let ((lr-item-suffix-rest-first
                         (parser-generator--first
                          lr-item-suffix-rest
                          nil
                          t
                          t)))
                    (parser-generator--debug
                     (message
                      "lr-item-suffix-rest-first (before): %s"
                      lr-item-suffix-rest-first))

                    ;; EOF-markers are always a possible look-ahead
                    (unless lr-item-suffix-rest-first
                      (setq
                       lr-item-suffix-rest-first
                       (list eof-list)))

                    (parser-generator--debug
                     (message
                      "lr-item-suffix-rest-first (after): %s"
                      lr-item-suffix-rest-first))
                    (let ((sub-production
                           (parser-generator--get-grammar-rhs
                            lr-item-suffix-first)))

                      ;; For each production with B as LHS
                      (dolist (sub-rhs sub-production)

                        ;; Transform e-productions into nil
                        (when (and
                               (= (length sub-rhs) 1)
                               (parser-generator--valid-e-p
                                (car sub-rhs)))
                          (setq sub-rhs nil))

                        ;; For each x in FIRST(αu)
                        (dolist (f lr-item-suffix-rest-first)

                          ;; then add [B -> . D, x] to V(X1,...,Xi) for each x in FIRST(bu)
                          ;; provided it is not already there
                          (let ((lr-item-to-add
                                 `(,(list lr-item-suffix-first) nil ,sub-rhs ,f)))
                            ;; Only k >= 1 needs dot a look-ahead
                            (when
                                (=
                                 parser-generator--look-ahead-number
                                 0)
                              (setq
                               lr-item-to-add
                               `(,(list lr-item-suffix-first) nil ,sub-rhs)))
                            (let ((temp-hash-key
                                   (format
                                    "%S"
                                    lr-item-to-add)))
                              (unless
                                  (gethash
                                   temp-hash-key
                                   lr-item-exists)
                                (setq
                                 added-new
                                 t)
                                (parser-generator--debug
                                 (message
                                  "lr-item-to-add: %s"
                                  lr-item-to-add))
                                (puthash
                                 temp-hash-key
                                 t
                                 lr-item-exists)
                                (push
                                 lr-item-to-add
                                 lr-new-item))))))))))))))

      ;; Sort result for a more deterministic result
      (setq
       lr-new-item
       (sort
        lr-new-item
        'parser-generator--sort-list)))

    lr-new-item))

(defun parser-generator-lr-parse
    (&optional
     input-tape-index
     pushdown-list
     output
     translation
     history)
  "Perform a LR-parse via lex-analyzer, optionally at INPUT-TAPE-INDEX with PUSHDOWN-LIST, OUTPUT, TRANSLATION and HISTORY."
  (let ((result
         (parser-generator-lr--parse
          input-tape-index
          pushdown-list
          output
          translation
          history)))
    (nth 0 result)))

(defun parser-generator-lr-translate
    (&optional
     input-tape-index
     pushdown-list
     output
     translation
     history)
  "Perform a LR-parse via lex-analyzer, optionally at INPUT-TAPE-INDEX with PUSHDOWN-LIST, OUTPUT, TRANSLATION and HISTORY."
  (let ((result
         (parser-generator-lr--parse
          input-tape-index
          pushdown-list
          output
          translation
          history)))
    (nth 1 result)))

;; Algorithm 5.7, p. 375
(defun parser-generator-lr--parse
    (&optional input-tape-index
               pushdown-list
               output
               translation
               translation-symbol-table-list
               history)
  "Perform a LR-parse via lex-analyzer, optionally at INPUT-TAPE-INDEX with PUSHDOWN-LIST, OUTPUT, TRANSLATION, TRANSLATION-SYMBOL-TABLE-LIST and HISTORY."
  (unless input-tape-index
    (setq input-tape-index 1))
  (unless pushdown-list
    (push 0 pushdown-list))
  (let ((translation-symbol-table
         (make-hash-table :test 'equal)))
    (when translation-symbol-table-list
      (dolist
          (item translation-symbol-table-list)
        (puthash
         (nth 0 item)
         (nth 1 item)
         translation-symbol-table)))

    (if (and
         input-tape-index
         (> input-tape-index 1))
        (setq
         parser-generator-lex-analyzer--index
         input-tape-index)
      (parser-generator-lex-analyzer--reset))

    ;; Make sure tables exists
    (unless parser-generator-lr--action-tables
      (error "Missing action-tables for grammar!"))
    (unless parser-generator-lr--distinct-action-tables
      (error "Missing distinct GOTO-tables for grammar!"))
    (unless parser-generator-lr--goto-tables
      (error "Missing GOTO-tables for grammar!"))
    (unless parser-generator-lr--distinct-goto-tables
      (error "Missing distinct GOTO-tables for grammar!"))

    (let ((accept)
          (pre-index 0))
      (while (not accept)

        ;; Save history when index has changed to enable incremental parsing / translating
        (when
            (>
             parser-generator-lex-analyzer--index
             pre-index)
          ;; We make a copy of the hash-table here to avoid passing same
          ;; hash-table every-time with pointer
          (let ((translation-symbol-table-list))
            (maphash
             (lambda (key value)
               (push
                `(,key ,value)
                translation-symbol-table-list))
             translation-symbol-table)
            (push
             `(,parser-generator-lex-analyzer--index
               ,pushdown-list
               ,output
               ,translation
               ,translation-symbol-table-list)
             history)
            (setq
             pre-index
             parser-generator-lex-analyzer--index)))

        ;; (1) The look-ahead string u, consisting of the next k input symbols, is determined.
        (let ((look-ahead
               (parser-generator-lex-analyzer--peek-next-look-ahead))
              (look-ahead-full))

          ;; Save token stream indexes in separate variable if needed later
          (setq look-ahead-full look-ahead)

          ;; Create simplified look-ahead for logic below
          (setq look-ahead nil)
          (dolist (look-ahead-item look-ahead-full)
            (if (listp look-ahead-item)
                (push (car look-ahead-item) look-ahead)
              (push look-ahead-item look-ahead)))
          (setq look-ahead (nreverse look-ahead))

          (parser-generator--debug
           (message "\nlook-ahead: %s" look-ahead)
           (message "look-ahead-full: %s" look-ahead-full))

          (let ((table-index
                 (car pushdown-list)))
            (let ((action-table-distinct-index
                   (gethash
                    table-index
                    parser-generator-lr--action-tables)))
              (let ((action-table
                     (gethash
                      action-table-distinct-index
                      parser-generator-lr--distinct-action-tables)))

                (unless action-table
                  (error
                   "Action-table with index %s is empty! Push-down-list: %s"
                   table-index
                   pushdown-list))

                (parser-generator--debug
                 (message
                  "Action-table %d: %s"
                  table-index
                  action-table))

                (let ((action-match nil)
                      (action-table-length (length action-table))
                      (action-index 0)
                      (possible-look-aheads))

                  ;; (2) The parsing action f of the table on top of the pushdown list is applied to the lookahead string u.
                  (while (and
                          (not action-match)
                          (< action-index action-table-length))
                    (let ((action (nth action-index action-table)))
                      (let ((action-look-ahead (car action)))
                        (push
                         action-look-ahead
                         possible-look-aheads)
                        (when
                            (equal
                             action-look-ahead
                             look-ahead)
                          (setq
                           action-match
                           (cdr action)))
                        (when
                            (and
                             (=
                              parser-generator--look-ahead-number
                              0)
                             (not
                              action-look-ahead))
                          ;; LR(0) reduce actions occupy entire row
                          ;; and is applied regardless of look-ahead
                          (setq
                           action-match
                           (cdr action))))
                      (setq
                       action-index
                       (1+ action-index))))

                  (unless action-match
                    ;; (c) If f(u) = error, we halt parsing (and, in practice
                    ;; transfer to an error recovery routine).

                    (error
                     (format
                      "Invalid syntax! Expected one of %S found %S at position %S"
                      possible-look-aheads
                      look-ahead
                      parser-generator-lex-analyzer--index)
                     possible-look-aheads
                     look-ahead
                     parser-generator-lex-analyzer--index))

                  (parser-generator--debug
                   (message "action-table: %s" action-table)
                   (message "action-match: %s" action-match))

                  (cond

                   ((equal action-match '(shift))
                    ;; (a) If f(u) = shift, then the next input symbol, say a
                    ;; is removed from the input and shifted onto the pushdown list.
                    ;; The goto function g of the table on top of the pushdown list
                    ;; is applied to a to determine the new table to be placed on
                    ;; top of the pushdown list. We then return to step(1). If
                    ;; there is no next input symbol or g(a) is undefined, halt
                    ;; and declare error.

                    (let ((a (list (car look-ahead)))
                          (a-full (list (car look-ahead-full))))
                      (parser-generator--debug
                       (message "shift a: %s" a)
                       (message "shift a-full: %s" a-full))
                      (let ((goto-table-distinct-index
                             (gethash
                              table-index
                              parser-generator-lr--goto-tables)))
                        (let ((goto-table
                               (gethash
                                goto-table-distinct-index
                                parser-generator-lr--distinct-goto-tables)))
                          (let ((goto-table-length
                                 (length goto-table))
                                (goto-index 0)
                                (searching-match t)
                                (next-index)
                                (possible-look-aheads))

                            (while (and
                                    searching-match
                                    (< goto-index goto-table-length))
                              (let ((goto-item (nth goto-index goto-table)))
                                (let ((goto-item-symbol (list (car goto-item)))
                                      (goto-item-next-index (car (cdr goto-item))))
                                  (push goto-item-symbol possible-look-aheads)

                                  (parser-generator--debug
                                   (message "shift goto-item: %s" goto-item)
                                   (message "shift goto-item-symbol: %s" goto-item-symbol))

                                  (when (equal
                                         goto-item-symbol
                                         a)
                                    (setq next-index goto-item-next-index)
                                    (setq searching-match nil))))

                              (setq goto-index (1+ goto-index)))

                            (parser-generator--debug
                             (message "shift next-index: %s" next-index))

                            (unless next-index
                              (error
                               "In shift, found no GOTO-item for %s at %s, expected one of %s"
                               a
                               parser-generator-lex-analyzer--index
                               possible-look-aheads))

                            (push (car a-full) pushdown-list)
                            (push next-index pushdown-list)
                            (parser-generator-lex-analyzer--pop-token))))))

                   ((equal (car action-match) 'reduce)
                    ;; (b) If f(u) = reduce i and production i is A -> a,
                    ;; then 2|a| symbols are removed from the top of the pushdown
                    ;; list, and production number i is placed in the output
                    ;; buffer. A new table T' is then exposed as the top table
                    ;; of the pushdown list, and the goto function of T' is applied
                    ;; to A to determine the next table to be placed on top of the
                    ;; pushdown list. We place A and this new table on top of the
                    ;; the pushdown list and return to step (1)

                    (let ((production-number (car (cdr action-match))))

                      (let ((production
                             (parser-generator--get-grammar-production-by-number
                              production-number)))
                        (let ((production-lhs (car production))
                              (production-rhs (car (cdr production)))
                              (popped-items-contents))
                          (parser-generator--debug
                           (message "production-lhs: %s" production-lhs)
                           (message "production-rhs: %s" production-rhs))
                          (unless (equal
                                   production-rhs
                                   (list parser-generator--e-identifier))
                            (let ((pop-items (* 2 (length production-rhs)))
                                  (popped-items 0)
                                  (popped-item))
                              (while (< popped-items pop-items)
                                (setq popped-item (pop pushdown-list))
                                (parser-generator--debug
                                 (message "popped-item-from-pushdownlist-list: %s" popped-item))
                                (when (and
                                       (listp popped-item)
                                       (parser-generator--valid-symbol-p
                                        (car popped-item)))
                                  (push
                                   popped-item
                                   popped-items-contents))
                                (setq popped-items (1+ popped-items)))))
                          (push production-number output)

                          (let ((popped-items-meta-contents))
                            (setq
                             popped-items-contents
                             (reverse popped-items-contents))
                            ;; Collect arguments for translation
                            (dolist (popped-item popped-items-contents)
                              (parser-generator--debug
                               (message
                                "popped-item: %s (for translation)"
                                popped-item))
                              (if (and
                                   (listp popped-item)
                                   (cdr popped-item))

                                  ;; If item is a terminal, use it's literal value
                                  (push
                                   (parser-generator-lex-analyzer--get-function
                                    popped-item)
                                   popped-items-meta-contents)

                                ;; If item is a non-terminal
                                (let ((temp-hash-key
                                       (format
                                        "%S"
                                        popped-item)))

                                  ;; If we have a translation for symbol, pop one
                                  ;; otherwise push nil on translation argument stack
                                  (if (gethash
                                       temp-hash-key
                                       translation-symbol-table)
                                      (let ((symbol-translations
                                             (gethash
                                              temp-hash-key
                                              translation-symbol-table)))
                                        (let ((symbol-translation
                                               (pop symbol-translations)))
                                          (push
                                           symbol-translation
                                           popped-items-meta-contents)
                                          (puthash
                                           temp-hash-key
                                           symbol-translations
                                           translation-symbol-table)))
                                    (push
                                     nil
                                     popped-items-meta-contents)))))

                            ;; If we just have one argument, pass it as a instead of a list
                            (when (= (length popped-items-meta-contents) 1)
                              (setq
                               popped-items-meta-contents
                               (car popped-items-meta-contents)))

                            (parser-generator--debug
                             (message
                              "Production arguments: %s -> %s = %s"
                              production-lhs
                              production-rhs
                              popped-items-meta-contents))

                            ;; Perform translation at reduction if specified
                            (if
                                (parser-generator--get-grammar-translation-by-number
                                 production-number)
                                (let ((partial-translation
                                       (funcall
                                        (parser-generator--get-grammar-translation-by-number
                                         production-number)
                                        popped-items-meta-contents)))
                                  (parser-generator--debug
                                   (message
                                    "translation-symbol-table: %S = %S (processed)"
                                    production-lhs
                                    partial-translation))
                                  (let ((temp-hash-key
                                         (format
                                          "%S"
                                          production-lhs)))
                                    (let ((symbol-translations
                                           (gethash
                                            temp-hash-key
                                            translation-symbol-table)))
                                      (push
                                       partial-translation
                                       symbol-translations)
                                      (puthash
                                       temp-hash-key
                                       symbol-translations
                                       translation-symbol-table)
                                      (setq
                                       translation
                                       partial-translation))))

                              ;; When no translation is specified just use popped contents as translation
                              (let ((partial-translation
                                     popped-items-meta-contents))
                                (parser-generator--debug
                                 (message
                                  "translation-symbol-table: %S = %S (generic)"
                                  production-lhs
                                  partial-translation))
                                (let ((temp-hash-key
                                       (format
                                        "%S"
                                        production-lhs)))
                                  (let ((symbol-translations
                                         (gethash
                                          temp-hash-key
                                          translation-symbol-table)))
                                    (push
                                     partial-translation
                                     symbol-translations)
                                    (puthash
                                     temp-hash-key
                                     symbol-translations
                                     translation-symbol-table)
                                    (setq
                                     translation
                                     partial-translation))))))

                          (let ((new-table-index (car pushdown-list)))
                            (let ((goto-table-distinct-index
                                   (gethash
                                    new-table-index
                                    parser-generator-lr--goto-tables)))
                              (let ((goto-table
                                     (gethash
                                      goto-table-distinct-index
                                      parser-generator-lr--distinct-goto-tables)))
                                (let ((goto-table-length
                                       (length goto-table))
                                      (goto-index 0)
                                      (searching-match t)
                                      (next-index))

                                  (while (and
                                          searching-match
                                          (< goto-index goto-table-length))
                                    (let ((goto-item (nth goto-index goto-table)))
                                      (let ((goto-item-symbol (list (car goto-item)))
                                            (goto-item-next-index (car (cdr goto-item))))
                                        (parser-generator--debug
                                         (message "reduce goto-item: %s" goto-item)
                                         (message "reduce goto-item-symbol: %s" goto-item-symbol))

                                        (when (equal
                                               goto-item-symbol
                                               production-lhs)
                                          (setq next-index goto-item-next-index)
                                          (setq searching-match nil))))

                                    (setq goto-index (1+ goto-index)))

                                  (parser-generator--debug
                                   (message "reduce next-index: %s" next-index))

                                  (when next-index
                                    (push production-lhs pushdown-list)
                                    (push next-index pushdown-list))))))))))

                   ((equal action-match '(accept))
                    ;;    (d) If f(u) = accept, we halt and declare the string
                    ;;    in the output buffer to be the right parse of the original
                    ;;    input string.

                    (setq accept t))

                   (t (error
                       "Invalid action-match: %s!"
                       action-match)))))))))
      (unless accept
        (error
         "Parsed entire string without getting accepting! Output: %s"
         (reverse output)))
      (when history
        (setq history (reverse history)))
      (when output
        (setq output (reverse output)))
      (let ((translation-symbol-table-list))
        (when translation-symbol-table
          (maphash
           (lambda (key value)
             (push
              `(,key ,value)
              translation-symbol-table-list))
           translation-symbol-table))
        (list
         output
         translation
         translation-symbol-table-list
         history)))))

(provide 'parser-generator-lr)

;;; parser-generator-lr.el ends here
