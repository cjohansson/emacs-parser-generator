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
        (table-count 0)
        (distinct-table-number (make-hash-table :test 'equal))
        (distinct-item-p (make-hash-table :test 'equal))
        (stack)
        (stack-item)
        (k (max 1 parser-generator--look-ahead-number)))

    ;; (1) Construct T_0, the LL(k) table associated with S {e}
    (let* ((start (parser-generator--get-grammar-start))
           (start-rhss (parser-generator--get-grammar-rhs start)))
      (dolist (start-rhs start-rhss)
        (let* ((production (list (list start) start-rhs)))
          (push
           (list
            (list start)
            start-rhs
            nil)
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
             (first-parent-follow
              (parser-generator--first parent-follow nil t t))
             (look-aheads))

        (cond
         ((and first-rhs
               (not first-parent-follow))
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            first-rhs
            nil)))
         ((and first-parent-follow
               (not first-rhs))
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            nil
            first-parent-follow)))
         ((and first-rhs
               first-parent-follow)
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            first-rhs
            first-parent-follow)))
         (t (error
             "Unexpected empty FIRST for production: %S and parent-follow: %S"
             production
             parent-follow)))

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
                       (merged-follow
                        (append follow-set parent-follow))
                       (local-follow-set
                        (parser-generator--first merged-follow nil t t))
                       (sub-symbol-rhss
                        (parser-generator--get-grammar-rhs
                         sub-symbol)))
                  (parser-generator--debug
                   (message "\nfollow-set: %S for %S in %S" follow-set (nth sub-symbol-index production-rhs) production-rhs)
                   (message "merged-follow: %S" follow-set)
                   (message "local-follow-set: %S" local-follow-set)
                   (message "sub-symbol-rhss: %S" sub-symbol-rhss))
                  (dolist (local-follow local-follow-set)
                    (dolist (sub-symbol-rhs sub-symbol-rhss)
                      (let* ((sub-symbol-production
                              (list (list sub-symbol) sub-symbol-rhs))
                             (new-stack-item
                              (list
                               (list sub-symbol)
                               sub-symbol-rhs
                               local-follow)))
                        (parser-generator--debug
                         (message "new-stack-item: %S" new-stack-item))
                        (push
                         new-stack-item
                         stack)))))))
            (setq
             sub-symbol-index
             (1+ sub-symbol-index))))

        ;; Add all distinct combinations of left-hand-side,
        ;; look-ahead and parent-follow to tables list here
        (when look-aheads
          (dolist (look-ahead look-aheads)
            (let ((table
                   (list
                    production-lhs
                    parent-follow
                    look-ahead
                    production-rhs))
                  (item-hash-key
                   (format
                    "%S-%S-%S"
                    production-lhs
                    parent-follow
                    look-ahead))
                  (table-hash-key
                   (format
                    "%S-%S"
                    production-lhs
                    parent-follow)))
              (unless (gethash item-hash-key distinct-item-p)
                (puthash
                 item-hash-key
                 t
                 distinct-item-p)
                (let ((existing-table-number
                       (gethash
                        table-hash-key
                        distinct-table-number)))
                  (if existing-table-number
                      (puthash
                       existing-table-number
                       (push
                        table
                        (gethash
                         existing-table-number
                         tables))
                       tables)
                    (puthash
                     table-hash-key
                     table-count
                     distinct-table-number)
                    (puthash
                     table-count
                     (list table)
                     tables)
                    (setq
                     table-count
                     (1+ table-count))))))))

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
      (sort
       (reverse sorted-tables)
       (lambda (a b) (< (car a) (car b)))))))


;; TODO
;; Algorithm 5.3 p. 351
(defun parser-generator-ll--generate-parsing-table (tables)
  "Generate a parsing table for an LL(k) grammar G and TABLES.  Output M, a valid parsing table for G."
  (let ((parsing-table))

    ;; (2) M(a, av) = pop for all v in E where |E| = k-1

    ;; (3) M($, e) = accept
    (push
     `(,parser-generator--eof-identifier
       (
        ,(parser-generator--generate-list-of-symbol
          parser-generator--look-ahead-number
          parser-generator--eof-identifier)
        accept)
       )
     parsing-table)

    parsing-table))


;; TODO
;; Algorithm 5.4 p. 357
(defun parser-generator-ll--valid-grammar-p ()
  "Test for LL(k)-ness.  Output t if grammar G is LL(k).  nil otherwise."
  )


(provide 'parser-generator-ll)

;;; parser-generator-ll.el ends here
