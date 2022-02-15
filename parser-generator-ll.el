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
  )


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
