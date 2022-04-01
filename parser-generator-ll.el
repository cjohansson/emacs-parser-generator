;;; parser-generator-ll.el --- LL(k) Parser Generator -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator)
(require 'parser-generator-lex-analyzer)


;;; Variables:


(defvar
  parser-generator-ll--parsing-table
  nil
  "Parsing-table for grammar.")


;;; Functions


(defun parser-generator-ll-generate-parser-tables ()
  "Generate parsing tables for grammar."
  (message "\n;; Starting generation of LL(k) parser-tables..\n")
  (unless (parser-generator-ll--valid-llk-p parser-generator--grammar)
    (error "Invalid grammar specified!"))
  (let* ((tables (parser-generator-ll--generate-tables))
         (parsing-table (parser-generator-ll--generate-parsing-table)))
    (setq
     parser-generator-ll--parsing-table
     parsing-table)
    (message "\n;; Completed generation of LL(k) parser-tables.\n")))


;;; Algorithms


;; Algorithm 5.2 p. 350
(defun parser-generator-ll--generate-tables ()
  "Construction of LL(k)-tables.  Output the set of LL(k) tables needed to construct a parsing table for the grammar G."
  (let ((tables (make-hash-table :test 'equal))
        (distinct-item-p (make-hash-table :test 'equal))
        (stack)
        (distinct-stack-item-p (make-hash-table :test 'equal))
        (stack-item)
        (k (max 1 parser-generator--look-ahead-number)))

    ;; (1) Construct T_0, the LL(k) table associated with S {e}
    (let* ((start (parser-generator--get-grammar-start))
           (start-rhss (parser-generator--get-grammar-rhs start)))
      (dolist (start-rhs start-rhss)
        (let* ((production
                (list (list start) start-rhs))
               (initial-stack-item
                (list
                 (list start)
                 start-rhs
                 (list parser-generator--eof-identifier))))
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
             (first-rhs
              (parser-generator--first production-rhs nil t t))
             (satured-first-rhs
              (parser-generator-generate-terminal-saturated-first-set
               first-rhs))
             (first-parent-follow
              (parser-generator--first parent-follow nil t t))
             (look-aheads)
             (sets))
        (parser-generator--debug
         (message "\nproduction-lhs: %S" production-lhs)
         (message "production-rhs: %S" production-rhs)
         (message "parent-follow: %S" parent-follow)
         (message "first-rhs: %S" first-rhs)
         (message "satured-first-rhs: %S" satured-first-rhs))

        ;; TODO Remove items in first-rhs that ends with the e-identifier
        ;; TODO but only if it has other items that does not end with the e-identifier
        ;; F('((a e) (a a))) = ((a a))

        (cond
         ((and satured-first-rhs
               (not first-parent-follow))
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            satured-first-rhs
            nil)))
         ((and first-parent-follow
               (not satured-first-rhs))
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            nil
            first-parent-follow)))
         ((and satured-first-rhs
               first-parent-follow)
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            satured-first-rhs
            first-parent-follow)))
         (t (error
             "Unexpected empty FIRST for production: %S and parent-follow: %S"
             production
             parent-follow)))

        (parser-generator--debug
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
                (parser-generator--debug
                 (message
                  "\nnon-terminal sub-symbol: %S" sub-symbol))
                (let* ((follow-set
                        (nthcdr (1+ sub-symbol-index) production-rhs))
                       (merged-follow
                        (append follow-set parent-follow))
                       (local-follow-set
                        (parser-generator--first merged-follow nil t t))
                       (sub-symbol-rhss
                        (parser-generator--get-grammar-rhs
                         sub-symbol)))
                  (parser-generator--debug
                   (message
                    "follow-set: %S for %S in %S"
                    follow-set
                    (nth sub-symbol-index production-rhs)
                    production-rhs)
                   (message
                    "merged-follow: %S"
                    follow-set)
                   (message
                    "local-follow-set: %S"
                    local-follow-set)
                   (message
                    "sub-symbol-rhss: %S"
                    sub-symbol-rhss))
                  (dolist (local-follow local-follow-set)
                    (push
                     local-follow
                     sets)
                    (dolist (sub-symbol-rhs sub-symbol-rhss)
                      (let* ((sub-symbol-production
                              (list (list sub-symbol) sub-symbol-rhs))
                             (new-stack-item
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

        ;; Add all distinct combinations of left-hand-side,
        ;; look-ahead and parent-follow to tables list here
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
                 (message "new table: %S" table))
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
                   tables)
                  )))))

        (parser-generator--debug
         (message "\nproduction-lhs: %S" production-lhs)
         (message "production-rhs: %S" production-rhs)
         (message "parent-follow: %S" parent-follow)
         (message "first-rhs: %S" first-rhs)
         (message "first-parent-follow: %S" first-parent-follow)
         (message "look-aheads: %S" look-aheads))))

    (let ((sorted-tables))
      (maphash
       (lambda (k v)
         (push
          (list k (sort v 'parser-generator--sort-list))
          sorted-tables))
       tables)
      sorted-tables)))


;; Algorithm 5.3 p. 351
(defun parser-generator-ll--generate-parsing-table (tables)
  "Generate a parsing table for an LL(k) grammar G and TABLES.  Output M, a valid parsing table for G."
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
               eof-look-ahead
               'accept))
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
             (key-stack-symbol (car (nth 0 key)))
             (key-parent-follow-set (nth 1 key))
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
                    (let ((local-follow (nth non-terminal-index local-follow-sets)))
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


;; Algorithm 5.4 p. 357
(defun parser-generator-ll--valid-grammar-p ()
  "Test for LL(k)-ness.  Output t if grammar is LL(k).  nil otherwise."
  (let ((stack)
        (stack-item)
        (k (max 1 parser-generator--look-ahead-number))
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
      (let ((production-lhs
             (nth 0 stack-item))
            (production-rhs
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
                    (let ((first-sub-symbol-rhs (parser-generator--first sub-symbol-rhs nil t t)))
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
