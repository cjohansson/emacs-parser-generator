;;; parser-generator-ll.el --- LL(k) Parser Generator -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator)
(require 'parser-generator-lex-analyzer)


;;; Variables:


(defvar
  parser-generator-ll--table
  nil
  "Table for grammar.")


;;; Functions


(defun parser-generator-ll-generate-table ()
  "Generate table for grammar."
  (let ((list-parsing-table)
        (hash-parsing-table (make-hash-table :test 'equal)))

    (if (> parser-generator--look-ahead-number 1)
        (progn
          (message "\n;; Starting generation of LL(k) tables..\n")

          (unless (parser-generator-ll--valid-grammar-k-gt-1-p)
            (error "Invalid LL(k) grammar specified!"))

          (setq
           list-parsing-table
           (parser-generator-ll--generate-action-table-k-gt-1
            (parser-generator-ll--generate-goto-table))))

      (message "\n;; Starting generation of LL(1) tables..\n")

      (unless (parser-generator-ll--valid-grammar-k-eq-1-p)
        (error "Invalid LL(1) grammar specified!"))

      (setq
       list-parsing-table
       (parser-generator-ll--generate-action-table-k-eq-1
        (parser-generator-ll--generate-goto-table))))

    ;; Convert list-structure to hash-map
    (dolist (state-list list-parsing-table)
      (let ((state-key (nth 0 state-list))
            (state-look-aheads (nth 1 state-list))
            (state-hash-table (make-hash-table :test 'equal)))
        (dolist (state-look-ahead-list state-look-aheads)
          (let ((state-look-ahead-string (nth 0 state-look-ahead-list))
                (state-look-ahead-action (nth 1 state-look-ahead-list)))
            (if (equal state-look-ahead-action 'reduce)
                (let ((state-look-ahead-reduction
                       (nth 2 state-look-ahead-list))
                      (state-look-ahead-production-number
                       (nth 3 state-look-ahead-list)))
                  (puthash
                   (format "%S" state-look-ahead-string)
                   (list
                    state-look-ahead-action
                    state-look-ahead-reduction
                    state-look-ahead-production-number)
                   state-hash-table))
              (puthash
               (format "%S" state-look-ahead-string)
               state-look-ahead-action
               state-hash-table))))
        (puthash
         (format "%S" state-key)
         state-hash-table
         hash-parsing-table)))
    (setq
     parser-generator-ll--table
     hash-parsing-table)

    (if (> parser-generator--look-ahead-number 1)
        (message "\n;; Completed generation of LL(k) tables.\n")
      (message "\n;; Completed generation of LL(1) tables.\n"))))

(defun parser-generator-ll-parse ()
  (let ((parse (parser-generator-ll--parse)))
    (car parse)))

(defun parser-generator-ll-translate ()
  (let ((parse (parser-generator-ll--parse t)))
    (car (cdr parse))))

;; Generally described at .p 339
(defun parser-generator-ll--parse (&optional translate-p)
  "Parse input via lex-analyzer and return parse trail."
  (let ((accept)
        (stack
         (if (> parser-generator--look-ahead-number 1)
             (list
              (list
               (list
                (parser-generator--get-grammar-start))
               (parser-generator--generate-list-of-symbol
                parser-generator--look-ahead-number
                parser-generator--eof-identifier))
              parser-generator--eof-identifier)
           (list
            (parser-generator--get-grammar-start)
            parser-generator--eof-identifier)))
        (output)
        (eof-look-ahead
         (parser-generator--generate-list-of-symbol
          parser-generator--look-ahead-number
          parser-generator--eof-identifier))
        (e-reduction
         (list parser-generator--e-identifier))
        (translation)
        (translation-stack)
        (translation-symbol-table
         (make-hash-table :test 'equal))
        (terminal-stack '()))
    (parser-generator-lex-analyzer--reset)
    (while (not accept)
      (let* ((state (car stack))
             (state-action-table
              (gethash
               (format "%S" state)
               parser-generator-ll--table))
             (look-ahead-list
              (parser-generator-lex-analyzer--peek-next-look-ahead))
             (look-ahead))
        (parser-generator--debug
         (message "\nstack: %S" stack)
         (message "translation-stack: %S" translation-stack)
         (message "output: %S" output)
         (message "state: %S" state)
         (message "state-action-table: %S" state-action-table))

        (unless state-action-table
          (signal
           'error
           (list
            (format
             "State action table lacks actions for state: '%S'!"
             state)
            state)))

        (if look-ahead-list
            (progn
              (parser-generator--debug
               (message "look-ahead-list: %S" look-ahead-list))
              (dolist (look-ahead-list-item look-ahead-list)
                (push (car look-ahead-list-item) look-ahead))
              (setq look-ahead (reverse look-ahead)))
          (setq
           look-ahead
           eof-look-ahead))

        (parser-generator--debug
         (message "look-ahead: %S" look-ahead))

        (unless (gethash
                 (format "%S" look-ahead)
                 state-action-table)
          (let ((possible-look-aheads))
            (maphash
             (lambda (k _v) (push k possible-look-aheads))
             state-action-table)
            (signal
             'error
             (list
              (format
               "Invalid look-ahead '%S' in state: '%S', valid look-aheads: '%S'"
               look-ahead
               state
               possible-look-aheads)
              look-ahead
              state
              possible-look-aheads))))

        (let* ((action
                (gethash
                 (format "%S" look-ahead)
                 state-action-table))
               (action-type action))
          (parser-generator--debug
           (message "action: %S" action))
          (when (listp action)
            (setq action-type (car action)))
          (parser-generator--debug
           (message "action-type: %S" action-type))
          (cond

           ((equal action-type 'pop)
            (parser-generator--debug
             (message "popped: %S" look-ahead))
            (let ((popped-tokens
                   (parser-generator-lex-analyzer--pop-token)))

              ;; Is it time for SDT?
              (when (and
                     translate-p
                     translation-stack
                     (string=
                      (car (car translation-stack))
                      (format "%S" stack)))
                (let* ((translation-item (pop translation-stack))
                       (partial-translation
                        (parser-generator-ll--perform-translation
                         (nth 1 translation-item)
                         translation-symbol-table
                         (reverse (pop terminal-stack)))))
                  (setq
                   translation
                   partial-translation)))

              (pop stack)

              (when translate-p
                (let ((token-data)
                      (old-terminal-stack (car terminal-stack)))
                  (dolist (popped-token popped-tokens)
                    (push
                     popped-token
                     token-data))
                  (push
                   token-data
                   old-terminal-stack)
                  (setf
                   (car terminal-stack)
                   old-terminal-stack)))

              ;; Is it time for SDT?
              (when (and
                     translate-p
                     translation-stack
                     (string=
                      (car (car translation-stack))
                      (format "%S" stack)))
                (let* ((translation-item (pop translation-stack))
                       (partial-translation
                        (parser-generator-ll--perform-translation
                         (nth 1 translation-item)
                         translation-symbol-table
                         (reverse (pop terminal-stack)))))
                  (setq
                   translation
                   partial-translation)))

              ))

           ((equal action-type 'reduce)
            (parser-generator--debug
             (message "reduced: %S -> %S" state (nth 1 action)))

            ;; Is it time for SDT?
            (when (and
                   translate-p
                   translation-stack
                   (string=
                    (car (car translation-stack))
                    (format "%S" stack)))
              (let* ((translation-item (pop translation-stack))
                     (partial-translation
                      (parser-generator-ll--perform-translation
                       (nth 1 translation-item)
                       translation-symbol-table
                       (reverse (pop terminal-stack)))))
                (setq
                 translation
                 partial-translation)))

            (pop stack)

            ;; Is it time for SDT?
            (when (and
                   translate-p
                   translation-stack
                   (string=
                    (car (car translation-stack))
                    (format "%S" stack)))
              (let* ((translation-item (pop translation-stack))
                     (partial-translation
                      (parser-generator-ll--perform-translation
                       (nth 1 translation-item)
                       translation-symbol-table
                       (reverse (pop terminal-stack)))))
                (setq
                 translation
                 partial-translation)))

            (when translate-p
              (push
               (list
                (format "%S" stack)
                (nth 2 action))
               translation-stack)
              (push
               '()
               terminal-stack))

            (unless (equal (nth 1 action) e-reduction)
              (dolist (reduce-item (reverse (nth 1 action)))
                (push reduce-item stack)))
            (push
             (nth 2 action)
             output))

           ((equal action-type 'accept)
            (setq accept t))))))
    (list
     (reverse output)
     translation)))

(defun parser-generator-ll--perform-translation (production-number symbol-table terminals)
  "Perform translation by PRODUCTION-NUMBER, with SYMBOL-TABLE and TERMINALS."
  (let* ((production
          (parser-generator--get-grammar-production-by-number
           production-number))
         (production-lhs
          (car (nth 0 production)))
         (production-rhs
          (nth 1 production))
         (translation)
         (args-1)
         (args-2))
    (parser-generator--debug
     (message
      "Perform translation %S %S %S = %S"
      production-number
      symbol-table
      terminals
      production-rhs))

    ;; Collect arguments for translation
    (let ((terminal-index 0))
      (dolist (rhs-item production-rhs)
        (cond

         ((parser-generator--valid-non-terminal-p
           rhs-item)
          (let* ((non-terminal-value-list
                 (gethash rhs-item symbol-table))
                (non-terminal-value
                 (pop non-terminal-value-list)))
            (push
             (car non-terminal-value)
             args-1)
            (push
             (car (cdr non-terminal-value))
             args-2)
            (puthash
             rhs-item
             non-terminal-value-list
             symbol-table)))

         ((parser-generator--valid-terminal-p
           rhs-item)
          (push
           (parser-generator-lex-analyzer--get-function
            (nth terminal-index terminals))
            args-1)
          (push
           (nth terminal-index terminals)
           args-2)
          (setq
           terminal-index
           (1+ terminal-index))))))
    (setq
     args-1
     (reverse args-1))
    (setq
     args-2
     (reverse args-2))

    (parser-generator--debug
     (message
      "Perform translation %d: %S -> %S via args-1: %S and args-2: %S"
      production-number
      production-lhs
      production-rhs
      args-1
      args-2))

    (if (parser-generator--get-grammar-translation-by-number
         production-number)
        (let ((partial-translation
               (funcall
                (parser-generator--get-grammar-translation-by-number
                 production-number)
                args-1
                args-2)))
          (parser-generator--debug
           (message
           "\ntranslation-symbol-table: %S = %S (processed)\n"
           production-lhs
           partial-translation))
          (let ((symbol-translations
                 (gethash
                  production-lhs
                  symbol-table)))
            (push
             (list
              partial-translation
              args-2)
             symbol-translations)
            (puthash
             production-lhs
             symbol-translations
             symbol-table)
            (setq
             translation
             partial-translation)))

      ;; When no translation is specified just use popped contents as translation
      (let ((partial-translation
             (list
              args-1
              args-2)))
        (parser-generator--debug
         (message
         "\ntranslation-symbol-table: %S = %S (generic)\n"
         production-lhs
         partial-translation))
        (let ((symbol-translations
               (gethash
                production-lhs
                symbol-table)))
          (push
           partial-translation
           symbol-translations)
          (puthash
           production-lhs
           symbol-translations
           symbol-table)
          (setq
           translation
           (car partial-translation)))))

    translation))


;;; Algorithms


(defun parser-generator-ll--generate-action-table-k-eq-1 (goto-table)
  "Generate action-table for LL(1) grammar using GOTO-TABLE."
  (let ((parsing-table))

    ;; Iterate all possible look-aheads
    ;; Add EOF symbol look-ahead
    (let ((eof-look-ahead
           (parser-generator--generate-list-of-symbol
            parser-generator--look-ahead-number
            parser-generator--eof-identifier))
          (terminal-mutations
           (parser-generator--get-grammar-look-aheads))
          (terminal-buffer)
          (last-terminal))
      (dolist (terminal-mutation terminal-mutations)
        (if (equal terminal-mutation eof-look-ahead)
            (push
             (list
              parser-generator--eof-identifier
              (list
               (list
                eof-look-ahead
                'accept)))
             parsing-table)
          (let ((stack-item (nth 0 terminal-mutation)))
            (when (and
                   last-terminal
                   (not (equal last-terminal stack-item)))
              (push
               (list
                last-terminal
                terminal-buffer)
               parsing-table)
              (setq
               terminal-buffer
               nil))
            (push
             (list terminal-mutation 'pop)
             terminal-buffer)
            (setq
             last-terminal
             stack-item))))
      (when (and
             last-terminal
             terminal-buffer)
        (push
         (list
          last-terminal
          terminal-buffer)
         parsing-table)))

    ;; Add non-terminal -> FIRST(non-terminal) -> reduce RHS, production-number
    (let ((non-terminal-look-ahead-p (make-hash-table :test 'equal))
          (non-terminal-look-ahead-list (make-hash-table :test 'equal)))
      (dolist (goto-row goto-table)
        (let* ((stack (nth 0 goto-row))
               (non-terminal (car (nth 0 stack)))
               (local-follows (nth 1 stack))
               (look-aheads (nth 1 goto-row)))
          (parser-generator--debug
           (message "\nnon-terminal: %S" non-terminal)
           (message "local-follows: %S" local-follows)
           (message "look-aheads: %S" look-aheads))
          (dolist (look-ahead look-aheads)
            (let* ((rhs
                    (nth 1 look-ahead))
                   (production
                    (list (list non-terminal) rhs))
                   (production-number
                    (parser-generator--get-grammar-production-number
                     production))
                   (look-ahead-terminal
                    (nth 0 look-ahead))
                   (hashmap-key
                    (format "%S-%S" non-terminal look-ahead-terminal)))
              (parser-generator--debug
               (message "\nrhs: %S" rhs)
               (message "production: %S" production)
               (message "production-number: %S" production-number)
               (message "hashmap-key: %S" hashmap-key))
              (unless (gethash hashmap-key non-terminal-look-ahead-p)
                (let ((old-non-terminal-look-aheads
                       (gethash
                        non-terminal
                        non-terminal-look-ahead-list)))
                  (push
                   (list
                    look-ahead-terminal
                    'reduce
                    rhs
                    production-number)
                   old-non-terminal-look-aheads)
                  (puthash
                   non-terminal
                   old-non-terminal-look-aheads
                   non-terminal-look-ahead-list)
                  (puthash
                   hashmap-key
                   t
                   non-terminal-look-ahead-p)))))))
      (maphash
       (lambda (non-terminal look-ahead)
         (push
          (list
           non-terminal
           look-ahead)
          parsing-table))
       non-terminal-look-ahead-list))

    parsing-table))

;; Algorithm 5.2 p. 350
(defun parser-generator-ll--generate-goto-table ()
  "Construction of LL(k) GOTO-table.  Output the set of LL(k) tables needed to construct a action table for the grammar G."
  (let ((tables (make-hash-table :test 'equal))
        (distinct-item-p (make-hash-table :test 'equal))
        (stack)
        (distinct-stack-item-p (make-hash-table :test 'equal))
        (stack-item))

    ;; (1) Construct T_0, the LL(k) table associated with S {e}
    (let* ((start (parser-generator--get-grammar-start))
           (start-rhss (parser-generator--get-grammar-rhs start)))
      (dolist (start-rhs start-rhss)
        (let* ((initial-stack-item
                (list
                 (list start)
                 start-rhs
                 (parser-generator--generate-list-of-symbol
                  parser-generator--look-ahead-number
                  parser-generator--eof-identifier))))
          (puthash
           initial-stack-item
           t
           distinct-stack-item-p)
          (push
           initial-stack-item
           stack))))

    (setq stack (nreverse stack))
    (parser-generator--debug
     (message "stack: %S" stack))

    (while stack
      (setq stack-item (pop stack))
      (let* ((production-lhs
              (nth 0 stack-item))
             (production-rhs
              (nth 1 stack-item))
             (parent-follow
              (nth 2 stack-item))
             (concatenated-follow
              (append production-rhs parent-follow))
             (first-concatenated-follow
              (parser-generator--first concatenated-follow nil t t))
             (look-aheads
              (parser-generator--merge-max-terminal-sets
               first-concatenated-follow))
             (sets))

        (parser-generator--debug
         (message "\nproduction-lhs: %S" production-lhs)
         (message "production-rhs: %S" production-rhs)
         (message "parent-follow: %S" parent-follow)
         (message "concatenated-follow: %S" concatenated-follow)
         (message "first-concatenated-follow: %S" first-concatenated-follow)
         (message "look-aheads: %S" look-aheads))

        ;; For each non-terminal in the production right-hand side
        ;; push a new item to stack with a local-follow
        ;; and a new left-hand-side
        (let ((sub-symbol-index 0)
              (sub-symbol-length (length production-rhs)))
          (while (< sub-symbol-index sub-symbol-length)
            (let ((sub-symbol (nth sub-symbol-index production-rhs)))
              (when (parser-generator--valid-non-terminal-p
                     sub-symbol)
                (let* ((follow-set
                        (nthcdr (1+ sub-symbol-index) production-rhs))
                       (concatenated-follow-set
                        (append follow-set parent-follow))
                       (first-concatenated-follow-set
                        (parser-generator--first concatenated-follow-set nil t t))
                       (local-follow-set
                        (parser-generator--merge-max-terminal-sets
                         first-concatenated-follow-set
                         nil
                         t))
                       (sub-symbol-rhss
                        (parser-generator--get-grammar-rhs
                         sub-symbol)))
                  (parser-generator--debug
                   (message
                    "\nnon-terminal sub-symbol: %S" sub-symbol)
                   (message
                    "follow-set: %S for %S in %S"
                    follow-set
                    (nth sub-symbol-index production-rhs)
                    production-rhs)
                   (message
                    "concatenated-follow-set: %S"
                    concatenated-follow-set)
                   (message
                    "first-concatenated-follow-set: %S"
                    first-concatenated-follow-set)
                   (message
                    "local-follow-set: %S"
                    local-follow-set)
                   (message
                    "sub-symbol-rhss: %S"
                    sub-symbol-rhss))
                  (unless local-follow-set
                    (setq local-follow-set '(nil)))

                  (push
                   local-follow-set
                   sets)
                  (parser-generator--debug
                   (message
                    "pushed local follow set to sets: %S"
                    local-follow-set))
                  (dolist (local-follow local-follow-set)
                    (dolist (sub-symbol-rhs sub-symbol-rhss)
                      (let* ((new-stack-item
                              (list
                               (list sub-symbol)
                               sub-symbol-rhs
                               local-follow)))
                        (unless (gethash
                                 new-stack-item
                                 distinct-stack-item-p)
                          (parser-generator--debug
                           (message
                            "new-stack-item: %S"
                            new-stack-item))
                          (puthash
                           new-stack-item
                           t
                           distinct-stack-item-p)
                          (push
                           new-stack-item
                           stack))))))))
            (setq
             sub-symbol-index
             (1+ sub-symbol-index))))

        (setq sets (reverse sets))
        (parser-generator--debug
         (message
          "\nsets: %S"
          sets))

        ;; Add all distinct combinations of left-hand-side,
        ;; look-aheads and parent-follow to tables list here
        (when look-aheads
          (dolist (look-ahead look-aheads)
            (let ((table
                   (list
                    look-ahead
                    production-rhs
                    sets))
                  (item-hash-key
                   (format
                    "%S-%S-%S"
                    production-lhs
                    parent-follow
                    look-ahead))
                  (table-hash-key
                   (list
                    production-lhs
                    parent-follow)))

              ;; Only add distinct items
              (unless (gethash item-hash-key distinct-item-p)
                (puthash
                 item-hash-key
                 t
                 distinct-item-p)
                (parser-generator--debug
                 (message "\nnew table: %S" table))
                (if (gethash
                     table-hash-key
                     tables)
                    (puthash
                     table-hash-key
                     (push
                      table
                      (gethash
                       table-hash-key
                       tables))
                     tables)
                  (puthash
                   table-hash-key
                   (list table)
                   tables))))))))

    (let ((sorted-tables))
      (maphash
       (lambda (k v)
         (push
          (list k (sort v 'parser-generator--sort-list))
          sorted-tables))
       tables)
      sorted-tables)))

;; Algorithm 5.3 p. 351
(defun parser-generator-ll--generate-action-table-k-gt-1 (tables)
  "Generate a action table for an LL(k) grammar G and TABLES.  Output M, a valid parsing table for G."
  (let ((parsing-table))

    ;; (3) M($, e) = accept
    ;; (2) M(a, av) = pop for all v in E where |E| = k-1
    (let ((eof-look-ahead
           (parser-generator--generate-list-of-symbol
            parser-generator--look-ahead-number
            parser-generator--eof-identifier))
          (terminal-mutations
           (parser-generator--get-grammar-look-aheads))
          (terminal-buffer)
          (last-terminal))
      (dolist (terminal-mutation terminal-mutations)
        (if (equal terminal-mutation eof-look-ahead)
            (push
             (list
              parser-generator--eof-identifier
              (list
               (list
                eof-look-ahead
                'accept)))
             parsing-table)
          (let ((stack-item (nth 0 terminal-mutation)))
            (when (and
                   last-terminal
                   (not (equal last-terminal stack-item)))
              (push
               (list
                last-terminal
                terminal-buffer)
               parsing-table)
              (setq
               terminal-buffer
               nil))

            (push
             (list terminal-mutation 'pop)
             terminal-buffer)
            (setq
             last-terminal
             stack-item))))
      (when (and
             last-terminal
             terminal-buffer)
        (push
         (list
          last-terminal
          terminal-buffer)
         parsing-table)))

    (dolist (table tables)
      (let* ((key (nth 0 table))
             (value (nth 1 table))
             (left-hand-side (nth 0 key))
             (parse-table))
        (dolist (look-ahead-row value)
          (let* ((look-ahead (nth 0 look-ahead-row))
                 (right-hand-side (nth 1 look-ahead-row))
                 (local-follow-sets (nth 2 look-ahead-row))
                 (non-terminal-index 0)
                 (sub-symbol-index 0)
                 (sub-symbol-length (length right-hand-side))
                 (production (list left-hand-side right-hand-side))
                 (production-number
                  (parser-generator--get-grammar-production-number
                   production))
                 (modified-right-hand-side))
            (while (< sub-symbol-index sub-symbol-length)
              (let ((sub-symbol (nth sub-symbol-index right-hand-side)))
                (if (parser-generator--valid-non-terminal-p
                     sub-symbol)
                    (let ((local-follow
                           (car (nth non-terminal-index local-follow-sets))))
                      (push
                       (list
                        (list sub-symbol)
                        local-follow)
                       modified-right-hand-side)
                      (setq
                       non-terminal-index
                       (1+ non-terminal-index)))
                  (push
                   sub-symbol
                   modified-right-hand-side)))
              (setq
               sub-symbol-index
               (1+ sub-symbol-index)))
            (setq
             modified-right-hand-side
             (reverse modified-right-hand-side))

            (push
             (list
              look-ahead
              'reduce
              modified-right-hand-side
              production-number)
             parse-table)))
        (push
         (list
          key
          parse-table)
         parsing-table)))

    parsing-table))

(defun parser-generator-ll--valid-grammar-k-eq-1-p ()
  "Test for LL(1)-ness.  Output t if grammar is LL(1), nil otherwise."
  (let* ((non-terminals (parser-generator--get-grammar-non-terminals))
         (non-terminal-length (length non-terminals))
         (non-terminal-index 0)
         (non-terminal)
         (valid t))
    (while (and
            valid
            (< non-terminal-index non-terminal-length))
      (setq non-terminal (nth non-terminal-index non-terminals))
      (let* ((rhss (parser-generator--get-grammar-rhs non-terminal))
             (rhss-length (length rhss))
             (rhss-index 0)
             (rhs)
             (look-aheads (make-hash-table :test 'equal)))
        (while (and
                valid
                (< rhss-index rhss-length))
          (setq rhs (nth rhss-index rhss))
          (let* ((firsts-rhs (parser-generator--first rhs))
                 (firsts-rhs-length (length firsts-rhs))
                 (firsts-index 0)
                 (first-rhs))
            (while (and
                    valid
                    (< firsts-index firsts-rhs-length))
              (setq first-rhs (nth firsts-index firsts-rhs))
              (let ((first-rhs-hash (format "%S" first-rhs)))
                (if (gethash first-rhs-hash look-aheads)
                    (setq valid nil)
                  (puthash first-rhs-hash t look-aheads)))
              (setq firsts-index (1+ firsts-index))))
          (setq rhss-index (1+ rhss-index))))
      (setq non-terminal-index (1+ non-terminal-index)))
    valid))

;; Algorithm 5.4 p. 357
(defun parser-generator-ll--valid-grammar-k-gt-1-p ()
  "Test for LL(k)-ness.  Output t if grammar is LL(k), nil otherwise."
  (let ((stack)
        (stack-item)
        (distinct-production-p (make-hash-table :test 'equal))
        (valid t))

    ;; (1) Construct T_0, the LL(k) table associated with S {e}
    (let* ((start (parser-generator--get-grammar-start))
           (start-rhss (parser-generator--get-grammar-rhs start)))
      (dolist (start-rhs start-rhss)
        (let* ((production (list (list start) start-rhs)))
          (push
           production
           stack)
          (puthash
           production
           t
           distinct-production-p))))
    (setq stack (nreverse stack))
    (parser-generator--debug
     (message "stack: %S" stack))

    (while (and
            stack
            valid)
      (setq stack-item (pop stack))
      (let ((production-rhs
             (nth 1 stack-item)))

        ;; For each non-terminal in the production right-hand side
        ;; push a new item to stack with a local-follow
        ;; and a new left-hand-side
        (let ((sub-symbol-index 0)
              (sub-symbol-length (length production-rhs)))
          (while (< sub-symbol-index sub-symbol-length)
            (let ((sub-symbol (nth sub-symbol-index production-rhs)))
              (when (parser-generator--valid-non-terminal-p
                     sub-symbol)
                (let* ((local-follow
                        (nthcdr (1+ sub-symbol-index) production-rhs))
                       (first-local-follow-sets
                        (parser-generator--first local-follow nil t t))
                       (sub-symbol-rhss
                        (parser-generator--get-grammar-rhs sub-symbol))
                       (distinct-item-p
                        (make-hash-table :test 'equal)))
                  (parser-generator--debug
                   (message "\nsub-symbol: %S" sub-symbol)
                   (message "local-follow: %S" local-follow)
                   (message "first-local-follow-sets: %S" first-local-follow-sets)
                   (message "sub-symbol-rhss: %S" sub-symbol-rhss))

                  ;; Calculate following terminals to see if there is a conflict
                  (dolist (sub-symbol-rhs sub-symbol-rhss)
                    (let ((first-sub-symbol-rhs
                           (parser-generator--first sub-symbol-rhs nil t t)))
                      (let ((merged-terminal-sets
                             (parser-generator--merge-max-terminal-sets
                              first-sub-symbol-rhs
                              first-local-follow-sets)))
                        (parser-generator--debug
                         (message "sub-symbol-rhs: %S" sub-symbol-rhs)
                         (message "first-sub-symbol-rhs: %S" first-sub-symbol-rhs)
                         (message "merged-terminal-sets: %S" merged-terminal-sets))
                        (dolist (merged-terminal-set merged-terminal-sets)
                          (if (gethash
                               merged-terminal-set
                               distinct-item-p)
                              (progn
                                (setq valid nil)
                                (parser-generator--debug
                                 (message
                                  "merged-terminal-set: %S was not distinct"
                                  merged-terminal-set)))
                            (puthash
                             merged-terminal-set
                             t
                             distinct-item-p)))))

                    ;; Add production to stack if it has not been added already
                    (let ((production
                           (list
                            (list sub-symbol)
                            sub-symbol-rhs)))
                      (unless
                          (gethash
                           production
                           distinct-production-p)
                        (push
                         production
                         stack)
                        (puthash
                         production
                         t
                         distinct-production-p)))))))
            (setq
             sub-symbol-index
             (1+ sub-symbol-index))))))
    valid))


(provide 'parser-generator-ll)

;;; parser-generator-ll.el ends here
