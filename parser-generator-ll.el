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


;; TODO
;; Algorithm 5.2 p. 350
(defun parser-generator-ll--generate-tables ()
  "Construction of LL(k)-tables.  Output the set of LL(k) tables needed to construct a parsing table for the grammar G."

  (let ((tables)
        (distinct-table-p (make-hash-table :test 'equal))
        (stack)
        (stack-item)
        (k (max 1 parser-generator--look-ahead-number)))

    ;; (1) Construct T_0, the LL(k) table associated with S {e}
    (let* ((start (parser-generator--get-grammar-start))
           (start-rhss (parser-generator--get-grammar-rhs start)))
      (dolist (start-rhs start-rhss)
        (let* ((production (list (list start) start-rhs))
               (production-number
                (parser-generator--get-grammar-production-number
                 production)))
          (push
           (list (list start) start-rhs production-number nil)
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
             (production-number
              (nth 2 stack-item))
             (dot-look-ahead
              (nth 3 stack-item))
             (first-rhs
              (parser-generator--first production-rhs nil t t))
             (first-dot-look-ahead
              (parser-generator--first dot-look-ahead nil t t))
             (look-aheads))

        (cond
         ((and first-rhs
               (not first-dot-look-ahead))
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            first-rhs
            nil)))
         ((and first-dot-look-ahead
               (not first-rhs))
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            nil
            first-dot-look-ahead)))
         ((and first-rhs
               first-dot-look-ahead)
          (setq
           look-aheads
           (parser-generator--merge-max-terminal-sets
            first-rhs
            first-dot-look-ahead)))
         (t (error
             "Unexpected empty FIRST for production: %S and dot-look-ahead: %S"
             production
             dot-look-ahead)))

        ;; For each non terminal in the right-hand side
        ;; push to stack with a local look ahead
        (let ((sub-symbol-index 0))
          (dolist (sub-symbol production-rhs)
            (when (parser-generator--valid-non-terminal-p
                   sub-symbol)
              (let ((local-look-ahead
                     (nthcdr (1+ sub-symbol-index) production-rhs))
                    (sub-symbol-rhss
                     (parser-generator--get-grammar-rhs
                      sub-symbol)))
                (dolist (sub-symbol-rhs sub-symbol-rhss)
                  (let* ((sub-symbol-production
                          (list (list sub-symbol) sub-symbol-rhs))
                         (sub-symbol-production-number
                          (parser-generator--get-grammar-production-number
                           sub-symbol-production)))
                    (push
                     (list
                      (list sub-symbol)
                      sub-symbol-rhs
                      sub-symbol-production-number
                      local-look-ahead)
                     stack))))))
          (setq
           sub-symbol-index
           (1+ sub-symbol-index)))

        ;; Add all distinct combinations of LHS, local-look-ahead and look-ahead
        ;; to tables list here
        (when look-aheads
          (dolist (look-ahead look-aheads)
            (let ((table
                   (list
                    production-lhs
                    production-rhs
                    production-number
                    look-ahead
                    dot-look-ahead))
                  (table-hash-key
                   (format
                    "%S"
                    (list dot-look-ahead production-lhs look-ahead))))
              (if
                  (gethash
                   table-hash-key
                   distinct-table-p)
                  (message
                   "\nConflicting LL-items: %S vs %S"
                   table
                   (gethash
                    table-hash-key
                    distinct-table-p))
                (push
                 table
                 tables)
                (puthash
                 table-hash-key
                 table
                 distinct-table-p)))))

        (parser-generator--debug
         (message "\nproduction-lhs: %S" production-lhs)
         (message "production-rhs: %S" production-rhs)
         (message "production-number: %S" production-number)
         (message "dot-look-ahead: %S" dot-look-ahead)
         (message "first-rhs: %S" first-rhs)
         (message "first-dot-look-ahead: %S" first-dot-look-ahead)
         (message "look-aheads: %S" look-aheads))))

    (sort
     tables
     'parser-generator--sort-list)))


;; TODO
;; Algorithm 5.3 p. 351
(defun parser-generator-ll--generate-parsing-table ()
  "A parsing table for an LL(k) grammar G.  Output M, a valid parsing table for G."
  )


;; TODO
;; Algorithm 5.4 p. 357
(defun parser-generator-ll--valid-grammar-p ()
  "Test for LL(k)-ness.  Output t if grammar G is LL(k).  nil otherwise."
  )


(provide 'parser-generator-ll)

;;; parser-generator-ll.el ends here
