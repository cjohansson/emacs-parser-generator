;;; parser-generator.el --- Parser Generator library -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Free Software Foundation, Inc.

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 10 Oct 2020
;; Modified: 20 Nov 2021
;; Version: 0.1.3
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-parser-generator

;; Package-Requires: ((emacs "26"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; The idea of this plugin is to provide functions for various kinds of context-free grammar parser generations with support for syntax-directed-translations (SDT) and semantic actions (SA) and the possibility of exporting parsers and translators (as generated stand-alone elisp code) to enable Emacs plugin-agnostic usage.

;;; Code:


;;; Variables:


(defvar
  parser-generator--context-sensitive-attributes
  nil
  "List of valid context-sensitive attributes.")

(defvar
  parser-generator--debug
  t
  "Whether to print debug messages or not.")

(defvar
  parser-generator--e-identifier
  'e
  "The identifier used for ε-symbol.  Default value 'e.")

(defvar
  parser-generator--eof-identifier
  '$
  "The identifier used for end of file identifier.  Default value is '$.")

(defvar
  parser-generator--global-attributes
  nil
  "List of valid global attributes.")

(defvar
  parser-generator--global-declaration
  nil
  "Global declaration for grammar.")

(defvar
  parser-generator--grammar
  nil
  "Current grammar used in parser.")

(defvar
  parser-generator--f-sets
  nil
  "Generated F-sets for grammar.")

(defvar
  parser-generator--look-ahead-number
  nil
  "Current look-ahead number used.")

(defvar
  parser-generator--table-context-sensitive-attributes-p
  nil
  "Hash-table of context-sensitive-attributes.")

(defvar
  parser-generator--table-global-attributes-p
  nil
  "Hash-table of global-attributes.")

(defvar
  parser-generator--table-firsts
  nil
  "Hash-table of calculated firsts for quicker parser generation.")

(defvar
  parser-generator--table-look-aheads-p
  nil
  "Hash-table of look-aheads for quick checking.")

(defvar
  parser-generator--table-non-terminal-p
  nil
  "Hash-table of terminals for quick checking.")

(defvar
  parser-generator--table-productions-rhs
  nil
  "Hash-table of productions RHS indexed by LHS for quick retrieving.")

(defvar
  parser-generator--table-productions-number
  nil
  "Hash-table indexed by production and value is production-number.")

(defvar
  parser-generator--table-productions-number-reverse
  nil
  "Hash-table indexed by production-number and value is production.")

(defvar
  parser-generator--table-terminal-p
  nil
  "Hash-table of non-terminals for quick checking.")

(defvar
  parser-generator--table-translations
  nil
  "Hash-table indexed by production-number and value is translation function.")

(defvar
  parser-generator--table-productions-attributes
  nil
  "Hash-table of attributes related to productions.")


;; Macros


(defmacro parser-generator--debug (&rest message)
  "Output MESSAGE but only if debug is enabled."
  `(when parser-generator--debug
     ,@message))


;; Helper Functions


(defun parser-generator--clear-cache ()
  "Clear cache."
  (setq
   parser-generator--f-sets
   nil)
  (setq
   parser-generator--table-firsts
   (make-hash-table :test 'equal)))

(defun parser-generator--distinct (elements)
  "Return distinct of ELEMENTS."
  (let ((processed (make-hash-table :test 'equal))
        (new-elements))
    (dolist (element elements)
      (unless (gethash element processed)
        (puthash element t processed)
        (push element new-elements)))
    (nreverse
     new-elements)))

(defun parser-generator--generate-list-of-symbol (k symbol)
  "Generate list of K number of SYMBOL."
  (let ((list-index 0)
        (list))
    (while (< list-index k)
      (push symbol list)
      (setq list-index (1+ list-index)))
    list))

(defun parser-generator--get-grammar-look-aheads ()
  "Return all possible look-ahead set."
  (unless parser-generator--look-ahead-number
    (error "No look-ahead number defined!"))
  (let ((terminals
         (parser-generator--get-grammar-terminals))
        (look-aheads)
        (k (max
            1
            parser-generator--look-ahead-number))
        (stack '((0 0 nil)))
        (marked-paths (make-hash-table :test 'equal))
        (added-look-aheads (make-hash-table :test 'equal)))
    (let ((terminals-max-index
           (1- (length terminals)))
          (terminal-index)
          (look-ahead-length)
          (look-ahead)
          (eof-list
           (parser-generator--generate-list-of-symbol
            k
            parser-generator--eof-identifier)))

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
                (puthash
                 potential-look-ahead
                 t
                 marked-paths)

                (push
                 `(,terminal-index ,look-ahead-length,look-ahead)
                 stack)

                (setq look-ahead-length (1+ look-ahead-length))
                (setq look-ahead potential-look-ahead)
                (setq terminal-index 0))))

          (let ((look-ahead-to-add))
            (if look-ahead
                (progn

                  ;; If length of look-ahead is below k, append EOF identifiers
                  (while (< look-ahead-length k)
                    (push
                     parser-generator--eof-identifier
                     look-ahead)
                    (setq
                     look-ahead-length
                     (1+ look-ahead-length)))
                  (setq
                   look-ahead-to-add
                   (reverse look-ahead)))

              (setq
               look-ahead-to-add
               eof-list))

            (when
                (and look-ahead-to-add
                     (not
                      (gethash
                       look-ahead-to-add
                       added-look-aheads)))
              (puthash
               look-ahead-to-add
               t
               added-look-aheads)
              (push
               look-ahead-to-add
               look-aheads))))))

    (sort
     look-aheads
     'parser-generator--sort-list)))

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
  (gethash
   production
   parser-generator--table-productions-number))

(defun parser-generator--get-grammar-production-by-number (production-number)
  "If PRODUCTION-NUMBER exist, return it's production."
  (unless parser-generator--table-productions-number-reverse
    (error "Table for reverse production-numbers is undefined!"))
  (gethash
   production-number
   parser-generator--table-productions-number-reverse))

(defun parser-generator--get-grammar-context-sensitive-attributes-by-production-number (production-number)
  "Get context-sensitive attributes by PRODUCTION-NUMBER, if any."
  (gethash
   production-number
   parser-generator--table-productions-attributes))

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
  (unless (listp lhs)
    (setq lhs (list lhs)))
  (gethash
   lhs
   parser-generator--table-productions-rhs))

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

(defun parser-generator--get-list-permutations (list k)
  "Return all possible LIST permutations length K."
  (let ((permutations)
        (permutations-length 1))
    (let ((list-length (length list))
          (i 0))
      (while (< i k)

        (let ((times (expt list-length (- k (1+ i))))
              (global-i 0))
          (while (< global-i permutations-length)
            ;; For each list..
            (let ((list-i 0))
              (while (< list-i list-length)

                ;; Add it |list| ^ (k - i) times to list
                (let ((times-i 0))
                  (while (< times-i times)
                    (if (= i 0)
                        (push (list (nth list-i list)) permutations)
                      (push (nth list-i list) (nth global-i permutations)))
                    (setq global-i (1+ global-i))
                    (setq times-i (1+ times-i))))
                (setq list-i (1+ list-i))))

            (when (= i 0)
              (setq permutations-length (length permutations)))))

        (setq i (1+ i))))
    (sort permutations 'parser-generator--sort-list)))

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
  "Load all symbols of grammar."

  ;; Build hash-table of all terminals of grammar
  (let ((terminals
         (parser-generator--get-grammar-terminals)))
    (setq
     parser-generator--table-terminal-p
     (make-hash-table :test 'equal))
    (dolist (terminal terminals)
      (puthash
       terminal
       t
       parser-generator--table-terminal-p)))

  ;; Build hash-table of all non-terminals
  (let ((non-terminals
         (parser-generator--get-grammar-non-terminals)))
    (setq
     parser-generator--table-non-terminal-p
     (make-hash-table :test 'equal))
    (dolist (non-terminal non-terminals)
      (puthash
       non-terminal
       t
       parser-generator--table-non-terminal-p)))

  ;; Build hash-tables of context-sensitive attributes
  (setq
   parser-generator--table-context-sensitive-attributes-p
   (make-hash-table :test 'equal))
  (dolist
      (attribute
       parser-generator--context-sensitive-attributes)
    (puthash
     attribute
     t
     parser-generator--table-context-sensitive-attributes-p))

  ;; Build hash-table of global attributes
  (setq
   parser-generator--table-global-attributes-p
   (make-hash-table :test 'equal))
  (dolist
      (attribute
       parser-generator--global-attributes)
    (puthash
     attribute
     t
     parser-generator--table-global-attributes-p))

  ;; Validate global declaration
  (when
      parser-generator--global-declaration
    (dolist
        (item
         parser-generator--global-declaration)
      (unless
          (parser-generator--valid-global-attribute-p
           (car item))
        (error
         "Invalid global declaration '%S' in grammar!"
         (car item)))))

  (let ((productions
         (parser-generator--get-grammar-productions)))

    ;; Build hash-table of all right-hand-sides of
    ;; a given left-hand-side of a production
    ;; exclude all functions that are used for translations
    (setq
     parser-generator--table-productions-rhs
     (make-hash-table :test 'equal))

    ;; Build hash-table of production -> production number
    ;; and production-number -> production
    ;; and a new set of productions that excludes translations
    ;; and always has the left-hand-side as a list
    ;; and verify each element in RHS belonging to terminals
    ;; or non-terminals
    (setq
     parser-generator--table-productions-number
     (make-hash-table :test 'equal))
    (setq
     parser-generator--table-productions-number-reverse
     (make-hash-table :test 'equal))
    (setq
     parser-generator--table-translations
     (make-hash-table :test 'equal))
    (setq
     parser-generator--table-productions-attributes
     (make-hash-table :test 'equal))

    (let ((production-index 0)
          (new-productions))

      ;; Iterate each production
      (dolist (p productions)
        (let ((lhs (car p))
              (rhs (cdr p))
              (production))
          (unless (listp lhs)
            (setq lhs (list lhs)))
          (let ((new-value
                 (gethash
                  lhs
                  parser-generator--table-productions-rhs))
                (rhs-element-index 0)
                (rhs-length (length rhs))
                (rhs-element))

            ;; Iterate each symbol in RHS
            (while
                (<
                 rhs-element-index
                 rhs-length)
              (let ((translation)
                   (production-attributes))
                (setq
                 rhs-element
                 (nth
                  rhs-element-index
                  rhs))
                (when (functionp rhs-element)
                  (error
                   "Unexpected function element %s in RHS %s of LHS %s"
                   rhs-element
                   rhs
                   lhs))

                ;; Potentially each symbol in RHS could be a separate RHS
                (unless (listp rhs-element)
                  (setq rhs-element (list rhs-element)))

                (let ((sub-rhs-element-index 0)
                      (sub-rhs-element-length (length rhs-element))
                      (sub-rhs-element)
                      (new-rhs))

                  ;; Iterate each symbol in SUB-RHS
                  (while
                      (<
                       sub-rhs-element-index
                       sub-rhs-element-length)
                    (setq
                     sub-rhs-element
                     (nth
                      sub-rhs-element-index
                      rhs-element))
                    (if (and
                         (listp sub-rhs-element)
                         (functionp sub-rhs-element))
                        (setq
                         translation
                         sub-rhs-element)
                      (unless
                          (or
                           (parser-generator--valid-terminal-p sub-rhs-element)
                           (parser-generator--valid-non-terminal-p sub-rhs-element)
                           (parser-generator--valid-e-p sub-rhs-element)
                           (parser-generator--valid-eof-p sub-rhs-element)
                           (parser-generator--valid-context-sensitive-attribute-p sub-rhs-element))
                        (error
                         "Symbol %s in RHS %s of production %s is not a valid terminal, non-terminal, context-sensitive attribute, e-identifier or EOF-identifier!"
                         sub-rhs-element
                         rhs-element
                         lhs))
                      (if (parser-generator--valid-context-sensitive-attribute-p
                           sub-rhs-element)
                          (progn
                            (when (=
                                   sub-rhs-element-index
                                   (1- sub-rhs-element-length))
                              (error "Expecting value for context-sensitive attribute %S!" sub-rhs-element))
                            (let ((attribute-value
                                   (nth
                                    (1+ sub-rhs-element-index)
                                    rhs-element)))
                              (if production-attributes
                                  (setq
                                   production-attributes
                                   (append
                                    production-attributes
                                    sub-rhs-element
                                    attribute-value))
                                (setq
                                 production-attributes
                                 (list
                                  sub-rhs-element
                                  attribute-value)))
                              (setq
                               sub-rhs-element-index
                               (1+ sub-rhs-element-index))))
                        (push
                         sub-rhs-element
                         new-rhs)))
                    (setq
                     sub-rhs-element-index
                     (1+ sub-rhs-element-index)))
                  (setq
                   production
                   (list
                    lhs
                    (reverse new-rhs)))
                  (message
                   "Production %s: %S"
                   production-index
                   production)
                  (push
                   (reverse new-rhs)
                   new-value)
                  (puthash
                   lhs
                   (reverse new-value)
                   parser-generator--table-productions-rhs))

                (setq
                 rhs-element-index
                 (1+ rhs-element-index))
                (puthash
                 production
                 production-index
                 parser-generator--table-productions-number)
                (puthash
                 production-index
                 production
                 parser-generator--table-productions-number-reverse)
                (puthash
                 production-index
                 production-attributes
                 parser-generator--table-productions-attributes)
                (push
                 production
                 new-productions)
                (when translation
                  (parser-generator--debug
                   (message
                    "Translation %S: %S"
                    production-index
                    translation))
                  (puthash
                   production-index
                   translation
                   parser-generator--table-translations))
                (setq
                 production-index
                 (1+ production-index)))))))
      (setq
       new-productions
       (nreverse new-productions))
      (setcar
       (nthcdr
        2
        parser-generator--grammar)
       new-productions)))

  (let ((look-aheads
         (parser-generator--get-grammar-look-aheads)))
    (setq
     parser-generator--table-look-aheads-p
     (make-hash-table :test 'equal))
    (dolist (look-ahead look-aheads)
      (puthash
       look-ahead
       t
       parser-generator--table-look-aheads-p))))

(defun parser-generator-set-e-identifier (e-identifier)
  "Set E-IDENTIFIER."
  (unless (or
           (stringp e-identifier)
           (symbolp e-identifier))
    (error "E-identifier must be a symbol or string!"))
  (setq parser-generator--e-identifier e-identifier))

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
  (message "\nStarting process of grammar..\n")
  (parser-generator--clear-cache)
  (unless parser-generator--look-ahead-number
    (error "No look-ahead-number defined!"))
  (unless
      (parser-generator--valid-look-ahead-number-p
       parser-generator--look-ahead-number)
    (error "Invalid look-ahead number k!"))
  (unless parser-generator--grammar
    (error "No grammar defined!"))
  (unless
      (parser-generator--valid-grammar-p
       parser-generator--grammar)
    (error "Invalid grammar G!"))
  (parser-generator--load-symbols)
  (message "\nCompleted process of grammar\n"))

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

(defun parser-generator--valid-context-sensitive-attribute-p (attribute)
  "Check if ATTRIBUTE is a valid context-sensitive attribute."
  (gethash
   attribute
   parser-generator--table-context-sensitive-attributes-p))

(defun parser-generator--valid-context-sensitive-attributes-p (attributes)
  "Check if all ATTRIBUTES are valid context-sensitive attributes."
  (let ((is-valid t)
        (length (length attributes))
        (index 0))
    (unless (listp attributes)
      (setq is-valid nil))
    (while (and
            is-valid
            (< index length))
      (let ((element
             (nth index attributes)))
        (unless
            (parser-generator--valid-context-sensitive-attribute-p
             element)
          (setq
           is-valid
           nil)))
      (setq index (+ index 2)))
    is-valid))

(defun parser-generator--valid-global-attribute-p (attribute)
  "Check if ATTRIBUTE is a valid global attribute."
  (gethash
   attribute
   parser-generator--table-global-attributes-p))

(defun parser-generator--valid-global-attributes-p (attributes)
  "Check if all ATTRIBUTES are valid global attributes."
  (let ((is-valid t)
        (length (length attributes))
        (index 0))
    (unless (listp attributes)
      (setq is-valid nil))
    (while (and
            is-valid
            (< index length))
      (let ((element
             (nth index attributes)))
        (unless
            (parser-generator--valid-global-attribute-p
             element)
          (setq
           is-valid
           nil)))
      (setq index (+ index 2)))
    is-valid))

(defun parser-generator--valid-e-p (symbol)
  "Return whether SYMBOL is the e identifier or not."
  (eq symbol parser-generator--e-identifier))

(defun parser-generator--valid-eof-p (symbol)
  "Return whether SYMBOL is the EOF identifier or not."
  (eq symbol parser-generator--eof-identifier))

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
            (let ((non-terminal
                   (nth non-terminal-index non-terminals)))
              (unless
                  (or
                   (symbolp non-terminal)
                   (stringp non-terminal))
                (setq valid-p nil)))
            (setq
             non-terminal-index
             (1+ non-terminal-index)))))

      ;; Check every terminal
      (let ((terminals (nth 1 G)))
        (let ((terminal-count (length terminals))
              (terminal-index 0))
          (while
              (and
               valid-p
               (< terminal-index terminal-count))
            (let ((terminal (nth terminal-index terminals)))
              (unless
                  (or
                   (symbolp terminal)
                   (stringp terminal))
                (setq valid-p nil)))
            (setq
             terminal-index
             (1+ terminal-index)))))

      ;; Check every production
      (let ((productions (nth 2 G)))
        (let ((production-count (length productions))
              (production-index 0))
          (while (and
                  valid-p
                  (<
                   production-index
                   production-count))
            (let ((production
                   (nth
                    production-index
                    productions)))
              (unless
                  (parser-generator--valid-production-p
                   production)
                (setq valid-p nil)))
            (setq
             production-index
             (1+ production-index)))))

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
  (gethash
   symbol
   parser-generator--table-look-aheads-p))

(defun parser-generator--valid-look-ahead-number-p (k)
  "Return if look-ahead number K is valid or not."
  (and
   (integerp k)
   (>= k 0)))

(defun parser-generator--valid-non-terminal-p (symbol)
  "Return whether SYMBOL is a non-terminal in grammar or not."
  (unless parser-generator--table-non-terminal-p
      (error "Table for non-terminals is undefined!"))
  (gethash
   symbol
   parser-generator--table-non-terminal-p))

(defun parser-generator--valid-production-p (production)
  "Return whether PRODUCTION is valid or not."
  (let ((is-valid t))
    (unless (listp production)
      (setq is-valid nil))
    (when (and is-valid
               (not (> (length production) 1)))
      (setq is-valid nil))
    (when (and
           is-valid
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

    ;; Validate that RHS is a list or symbol or a string or a number
    (when (and is-valid
               (not (or
                     (listp (car (cdr production)))
                     (symbolp (car (cdr production)))
                     (stringp (car (cdr production)))
                     (numberp (car (cdr production))))))
      (setq is-valid nil))

    ;; Validate right-hand-side (RHS) of production
    (when is-valid
      (let ((rhs (cdr production)))
        (let ((rhs-index 0)
              (rhs-length (length rhs)))
          (while (and
                  is-valid
                  (< rhs-index rhs-length))
            (let ((rhs-element (nth rhs-index rhs)))
              (cond
               ((stringp rhs-element))
               ((symbolp rhs-element))
               ((and (listp rhs-element)
                     (functionp rhs-element)
                     (= rhs-index (1- rhs-length))))
               ((and
                 (listp rhs-element)
                 (not (functionp rhs-element)))

                (let ((rhs-sub-index 0)
                      (rhs-sub-element)
                      (rhs-sub-length (length rhs-element)))
                  (while (and
                          is-valid
                          (< rhs-sub-index rhs-sub-length))
                    (setq
                     rhs-sub-element
                     (nth rhs-sub-index rhs-element))

                    (cond
                     ((and (listp rhs-sub-element)
                           (functionp rhs-sub-element)
                           (= rhs-sub-index (1- rhs-sub-length))))
                     ((or (stringp rhs-sub-element)
                          (symbolp rhs-sub-element)
                          (numberp rhs-sub-element)))
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
             (parser-generator--valid-eof-p symbol)
             (parser-generator--valid-non-terminal-p symbol)
             (parser-generator--valid-terminal-p symbol))
      (setq is-valid nil))
    is-valid))

(defun parser-generator--valid-terminal-p (symbol)
  "Return whether SYMBOL is a terminal in grammar or not."
  (unless parser-generator--table-terminal-p
    (error "Table for terminals is undefined!"))
  (gethash
   symbol
   parser-generator--table-terminal-p))


;; Main Algorithms


;; p. 381
(defun parser-generator--e-free-first (α)
  "For sentential string Α, Calculate e-free-first k terminals in grammar."
  (parser-generator--first α t))

(defun parser-generator--generate-f-sets ()
  "Generate F-sets for grammar."
  ;; Generate F-sets only once per grammar
  (unless parser-generator--f-sets
    (parser-generator--debug
     (message "(parser-generator--generate-f-sets)"))
    (let ((productions
           (parser-generator--get-grammar-productions))
          (k
           (max
            1
            parser-generator--look-ahead-number)))
      (let ((f-sets (make-hash-table :test 'equal))
            (i 0)
            (expanded-all nil)
            (expanded-all-second nil))

        (while (or
                (not expanded-all)
                (not expanded-all-second))
          ;; Make one iteration after everything has been expanded
          (when expanded-all
            (setq
             expanded-all-second
             t))
          (when (> i 100)
            (error "Endless loop!"))
          (parser-generator--debug
           (message "i = %s" i))
          (setq
           expanded-all
           t)
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
                                ,production-lhs)
                              '((nil nil 0)))))

                        (parser-generator--debug
                         (message
                          "\nf-set-return: %s = %s"
                          rhs-string
                          f-set-return))

                        ;; Unless set was fully expanded..
                        (if (nth 0 f-set-return)
                            (parser-generator--debug
                             (message
                              "Production '%S' fully expanded,"
                              production-lhs))

                          ;; Get unexpanded non-terminal
                          (let ((unexpanded-non-terminal
                                 (nth 1 f-set-return)))
                            (cond

                             ((equal
                               unexpanded-non-terminal
                               production-lhs)
                              (parser-generator--debug
                               (message
                                "Production '%S' un-expanded due to self-reference, ignore flag."
                                production-lhs)))

                             ((gethash
                               unexpanded-non-terminal
                               f-set)
                              (parser-generator--debug
                               (message
                                "Production '%S' is un-expanded due to reference to previously processed production '%S', ignore flag."
                                production-lhs
                                unexpanded-non-terminal)))

                             (t
                              (parser-generator--debug
                               (message
                                "Production 'S' is un-expanded due to reference to un-expanded non-terminal '%S'"
                                production-lhs
                                unexpanded-non-terminal))

                              (setq
                               rhs-expanded-full
                               nil)
                              (setq
                               expanded-all
                               nil)))))

                        (setq
                         rhs-leading-terminals
                         (nth 2 f-set-return))

                        (parser-generator--debug
                         (message
                          "Leading %d terminals at index %s: %s -> %s = %s"
                          k
                          i
                          production-lhs
                          rhs-string
                          rhs-leading-terminals))

                        (when rhs-leading-terminals
                          (when (and
                                 (listp rhs-leading-terminals)
                                 (> (length rhs-leading-terminals) 0))
                            (dolist
                                (rhs-leading-terminals-element
                                 rhs-leading-terminals)
                              (push
                               rhs-leading-terminals-element
                               f-p-set)))))))

                  ;; If we have multiple equal LHS merge them
                  (when (gethash
                         production-lhs
                         f-set)
                    (let ((existing-f-set
                           (gethash
                            production-lhs
                            f-set)))
                      (parser-generator--debug
                       (message
                        "existing-f-set: %S"
                        existing-f-set))

                      ;; If another RHS has not been fully expanded
                      ;; mark LHS as not fully expanded
                      (if (nth 0 existing-f-set)
                          (parser-generator--debug
                           (message
                            "Previous RHS has been fully expanded as well."))

                        (parser-generator--debug
                         (message
                          "Previous RHS has not been fully expanded so mark '%S' as not expanded."
                          production-lhs))
                        (setq
                         expanded-all
                         nil)
                        (setq
                         rhs-expanded-full
                         nil))

                      (setq
                       f-p-set
                       (append
                        f-p-set
                        (nth 1 existing-f-set)))))

                  ;; Make set distinct
                  (setq
                   f-p-set
                   (parser-generator--distinct
                    f-p-set))
                  (puthash
                   production-lhs
                   (list
                    rhs-expanded-full
                    (reverse f-p-set))
                   f-set)
                  (parser-generator--debug
                   (message
                    "F_%s%s = %s"
                    i
                    production-lhs
                    (gethash
                     production-lhs
                     f-set))))))

            (puthash
             i
             f-set
             f-sets)
            (setq
             i
             (+ i 1))))

        (setq
         parser-generator--f-sets
         (gethash
          (1- i)
          f-sets))
        (parser-generator--debug
         (message
          "FIRST max-index: %s, contents: %s"
          (1- i)
          parser-generator--f-sets))))
    (parser-generator--debug
     (message "Generated F-sets"))))

(defun parser-generator--first-to-lookahead (first)
  "Replace all e-identifiers with eof-identifiers in FIRST."
  (let ((look-ahead))
    (dolist (symbol first)
      (if (parser-generator--valid-e-p
           symbol)
          (push
           parser-generator--eof-identifier
           look-ahead)
        (push
         symbol
         look-ahead)))
    (nreverse look-ahead)))

(defun parser-generator--merge-max-terminals (a b k)
  "Merge terminals from A and B to a maximum length of K."
  (let ((merged)
        (merge-count 0)
        (continue t)
        (a-element)
        (a-index 0)
        (a-length (length a))
        (b-element)
        (b-index 0)
        (b-length (length b)))
    (while (and
            (< a-index a-length)
            (< merge-count k)
            continue)
      (setq a-element (nth a-index a))
      (when (parser-generator--valid-e-p a-element)
        (setq continue nil))
      (push a-element merged)
      (setq a-index (1+ a-index)))
    (while (and
            (< b-index b-length)
            (< merge-count k)
            continue)
      (setq b-element (nth b-index b))
      (when (parser-generator--valid-e-p b-element)
        (setq continue nil))
      (push b-element merged)
      (setq b-index (1+ b-index)))
    (nreverse merged)))

;; p. 357
(defun parser-generator--f-set (input-tape state stack)
  "A deterministic push-down transducer (DPDT) for building F-sets from INPUT-TAPE, STATE and STACK."
  (unless (listp input-tape)
    (setq input-tape (list input-tape)))
  (parser-generator--debug
   (message "\n(parser-generator--f-set)")
   (message "input-tape: %s" input-tape)
   (message "stack: %s" stack)
   (message "state: %S" state))

  (let ((f-set)
        (input-tape-length (length input-tape))
        (k (nth 0 state))
        (i (nth 1 state))
        (f-sets (nth 2 state))
        (lhs (nth 3 state))
        (expanded-all t)
        (unexpanded-non-terminal nil))
    (parser-generator--debug
     (message
      "input-tape-length: %s"
      input-tape-length)
     (message "k: %s" k)
     (message "i: %s" i))

    (while stack
      (let ((stack-symbol (pop stack)))
        (parser-generator--debug
         (message
          "Stack-symbol: %s"
          stack-symbol))
        (let ((leading-symbols (nth 0 stack-symbol))
              (leading-terminals (nth 1 stack-symbol))
              (input-tape-index (nth 2 stack-symbol)))
          (parser-generator--debug
           (message
            "leading-symbols 0: %s"
            leading-symbols)
           (message
            "leading-terminals 0: %s"
            leading-terminals)
           (message
            "input-tape-index: %s"
            input-tape-index))

          (let ((leading-terminals-count
                 (length leading-terminals))
                (leading-symbols-count
                 (length leading-symbols)))
            (parser-generator--debug
             (message
              "leading-terminals-count: %s"
              leading-terminals-count))

            (while (and
                    (< input-tape-index input-tape-length)
                    (< leading-terminals-count k))
              (let ((rhs-element (nth input-tape-index input-tape))
                    (rhs-type))
                (parser-generator--debug
                 (message
                  "rhs-element: %s"
                  rhs-element))

                ;; Determine symbol type
                (cond
                 ((parser-generator--valid-non-terminal-p rhs-element)
                  (setq rhs-type 'NON-TERMINAL))
                 ((parser-generator--valid-e-p rhs-element)
                  (setq rhs-type 'E-IDENTIFIER))
                 ((parser-generator--valid-terminal-p rhs-element)
                  (setq rhs-type 'TERMINAL))
                 (t (error (format "Invalid symbol %s" rhs-element))))
                (parser-generator--debug
                 (message
                  "rhs-type: %s"
                  rhs-type))

                (cond

                 ((equal rhs-type 'NON-TERMINAL)
                  (if (> i 0)
                      (let ((sub-terminal-sets)
                            (sub-terminal-expanded)
                            (sub-terminal-data
                             (gethash
                              (list rhs-element)
                              (gethash
                               (1- i)
                               f-sets))))
                        (parser-generator--debug
                         (message
                          "sub-terminal-data: %s = %s"
                          rhs-element
                          sub-terminal-data))
                        
                        (setq
                         sub-terminal-expanded
                         (nth 0 sub-terminal-data))
                        (setq
                         sub-terminal-sets
                         (nth 1 sub-terminal-data))

                        ;; When sub-set has not been fully expanded mark this set
                        ;; as not fully expanded either
                        (when (and
                               (not sub-terminal-expanded)
                               sub-terminal-data)
                          (parser-generator--debug
                           (message
                            "Can't expand '%S' because sub-terminals of '%S' has not been fully expanded"
                            lhs
                            rhs-element))
                          (setq
                           unexpanded-non-terminal
                           (list rhs-element))
                          (setq
                           expanded-all
                           nil))

                        (if sub-terminal-sets
                            (progn
                              (parser-generator--debug
                               (message
                                "Sub-terminal-sets F_%s(%s) = %s (%d)"
                                (1- i)
                                rhs-element
                                sub-terminal-sets
                                (length sub-terminal-sets)))

                              ;; Should branch off here, each unique permutation should be included in set
                              ;; Follow the first alternative in this scope but follow the rest in separate scopes
                              (let ((sub-terminal-index 0))
                                (dolist (sub-symbol-alternative-set sub-terminal-sets)
                                  (parser-generator--debug
                                   (message
                                    "sub-symbol-alternative-set: %s"
                                    sub-symbol-alternative-set))

                                  (let ((sub-symbol-index 0)
                                        (sub-symbol-length
                                         (length
                                          sub-symbol-alternative-set))
                                        (sub-symbol)
                                        (sub-terminal)
                                        (sub-symbols
                                         (reverse leading-symbols))
                                        (sub-terminals
                                         (reverse leading-terminals)))
                                    (while (and
                                            (< sub-symbol-index sub-symbol-length)
                                            (< (length sub-terminals) k))
                                      (setq
                                       sub-symbol
                                       (nth
                                        sub-symbol-index
                                        sub-symbol-alternative-set))
                                      (push
                                       sub-symbol
                                       sub-symbols)
                                      (unless (parser-generator--valid-e-p sub-terminal)
                                        (push
                                         sub-terminal
                                         sub-terminals))
                                      (setq
                                       sub-symbol-index
                                       (1+ sub-symbol-index)))
                                    (setq
                                     sub-symbols
                                     (reverse sub-symbols))
                                    (setq
                                     sub-terminals
                                     (reverse sub-terminals))
                                    (let ((branch
                                           `(
                                             ,sub-symbols
                                             ,sub-terminals
                                             ,(1+ input-tape-index))))
                                      (parser-generator--debug
                                       (message
                                        "branched off 3: %s"
                                        branch))
                                      (push
                                       branch
                                       stack)))
                                  (setq
                                   sub-terminal-index
                                   (1+ sub-terminal-index)))))

                          (parser-generator--debug
                           (message
                            "Found no subsets for %s %s"
                            rhs-element
                            (1- i)))
                          (setq
                           unexpanded-non-terminal
                           (list rhs-element))))

                    (parser-generator--debug
                     (message
                      "Expanded-all negative set for '%s' because symbol '%s' is a non-terminal and i is zero"
                      lhs
                      rhs-element))
                    (setq
                     expanded-all
                     nil)
                    (setq
                     unexpanded-non-terminal
                     (list rhs-element))))

                 ((equal rhs-type 'E-IDENTIFIER)
                  (setq
                   leading-symbols
                   (append
                    leading-symbols
                    rhs-element))
                  (setq
                   leading-symbols-count
                   (1+ leading-symbols-count)))

                 ((equal rhs-type 'TERMINAL)
                  (setq
                   leading-symbols
                   (append
                    leading-symbols
                    (list rhs-element)))
                  (setq
                   leading-symbols-count
                   (1+ leading-symbols-count))
                  (setq
                   leading-terminals
                   (append
                    leading-terminals
                    (list rhs-element)))
                  (setq
                   leading-terminals-count
                   (1+ leading-terminals-count)))))
              (setq
               input-tape-index
               (1+ input-tape-index)))

            (when (> leading-symbols-count 0)
              (unless (listp leading-symbols)
                (setq
                 leading-symbols
                 (list leading-symbols)))
              (parser-generator--debug
               (message "leading-symbols 5: %s" leading-symbols)
               (message "leading-terminals 5: %s" leading-terminals))
              (push
               leading-symbols
               f-set))))))
    (parser-generator--debug
     (message "expanded-all: %s" expanded-all))
    (list
     expanded-all
     unexpanded-non-terminal
     f-set)))

;; Algorithm 5.5, p. 357
(defun parser-generator--first
    (
     β
     &optional
     disallow-e-first
     ignore-validation
     skip-sorting)
  "For sentential-form Β, calculate first terminals, optionally DISALLOW-E-FIRST, IGNORE-VALIDATION and SKIP-SORTING."
  (let ((hash-key
         (format
          "%S-%s"
          β
          disallow-e-first)))
    (unless (gethash
             hash-key
             parser-generator--table-firsts)
      (unless (listp β)
        (setq β (list β)))
      (unless (or
               ignore-validation
               (parser-generator--valid-sentential-form-p β))
        (error "Invalid sentential form β! %s" β))
      (let ((k (max
                1
                parser-generator--look-ahead-number)))

        ;; Generate F-sets only once per grammar
        (parser-generator--generate-f-sets)

        (let ((first-list nil)
              (first-items (make-hash-table :test 'equal)))
          ;; Iterate each symbol in β using a PDA algorithm
          (let ((input-tape β)
                (input-tape-length (length β))
                (stack '((0 0 nil))))
            (while stack
              (let ((stack-topmost (pop stack)))
                (parser-generator--debug
                 (message
                  "\nstack-topmost: %s"
                  stack-topmost))
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
                       (message
                        "symbol index: %s from %s is: %s"
                        input-tape-index
                        input-tape symbol))
                      (cond

                       ((parser-generator--valid-e-p symbol)

                        ;; When there a symbols left on stack, make alternative trail by skipping this symbol
                        (unless (or
                                 disallow-e-first
                                 (= input-tape-index (1- input-tape-length)))
                          (parser-generator--debug
                           (message
                            "Pushed alternative trail to stack since symbol is e-identifier: %s"
                            `(,(1+ input-tape-index) ,first-length ,first)))
                          (push
                           `(,(1+ input-tape-index) ,first-length ,first)
                           stack))

                        (if disallow-e-first
                            (when (> first-length 0)
                              (setq first (append first (list symbol)))
                              (setq first-length (1+ first-length)))
                          (setq first (append first (list symbol)))
                          (setq first-length (1+ first-length)))

                        (setq keep-looking nil))

                       ((parser-generator--valid-eof-p symbol)
                        (setq first (append first (list symbol)))
                        (setq first-length (1+ first-length)))

                       ((parser-generator--valid-terminal-p symbol)
                        (setq first (append first (list symbol)))
                        (setq first-length (1+ first-length)))

                       ((parser-generator--valid-non-terminal-p symbol)
                        (parser-generator--debug
                         (message "non-terminal symbol: %s" symbol))
                        (setq
                         symbol
                         (list symbol))
                        (parser-generator--debug
                         (message "non-terminal symbol production: %s" symbol))
                        (let ((symbol-f-set))

                          ;; Load the pre-generated F-set
                          ;; if it's the first symbol and we are using
                          ;; E-FREE-FIRST then use separate hash-table
                          (parser-generator--debug
                           (message
                            "gethash: %s"
                            (gethash
                             symbol
                             parser-generator--f-sets)))
                          (setq
                           symbol-f-set
                           (nth
                            1
                            (gethash
                             symbol
                             parser-generator--f-sets)))
                          (parser-generator--debug
                           (message
                            "symbol-f-set: %s"
                            symbol-f-set))

                          (if (and
                               (not symbol-f-set)
                               disallow-e-first
                               (= first-length 0))
                              (progn
                                (parser-generator--debug
                                 (message
                                  "stopped looking since non-terminal starts with e-identifier: %s"
                                  symbol-f-set))
                                (setq
                                 keep-looking
                                 nil))

                            ;; Handle this scenario here were a non-terminal can result in different FIRST sets
                            (let ((symbol-f-set-index 0)
                                  (symbol-f-set-length
                                   (length symbol-f-set))
                                  (found-e-trail)
                                  (e-trail-is-viable-p
                                   (< input-tape-index (1- input-tape-length)))
                                  (original-first first)
                                  (original-first-length first-length))
                              (while (< symbol-f-set-index symbol-f-set-length)
                                (let ((symbol-f-set-element (nth symbol-f-set-index symbol-f-set)))
                                  (let ((alternative-first-length
                                         (+ original-first-length (length symbol-f-set-element)))
                                        (alternative-first
                                         (append original-first symbol-f-set-element))
                                        (alternative-tape-index
                                         (1+ input-tape-index)))
                                    (parser-generator--debug
                                     (message
                                      "alternative-first: %s"
                                      alternative-first))

                                    ;; When the e-identifier is an alternative trail
                                    ;; and there a symbols left on stack
                                    ;; make alternative trail by skipping this symbol
                                    ;; but only if there are more symbols in the input tape
                                    (when (and
                                           e-trail-is-viable-p
                                           (not found-e-trail)
                                           (or
                                            (not disallow-e-first)
                                            (> original-first-length 0))
                                           (parser-generator--valid-e-p
                                            (car alternative-first)))
                                      (push
                                       `(,(1+ input-tape-index) ,original-first-length ,original-first)
                                       stack)
                                      (parser-generator--debug
                                       (message
                                        "Pushed alternative trail from non-terminal expansion to stack since first symbol is the e-identifier: %s"
                                        `(,(1+ input-tape-index) ,original-first-length ,original-first)))
                                      (setq
                                       found-e-trail
                                       t))

                                    (if (= symbol-f-set-index 0)
                                        (progn
                                          (setq
                                           first-length
                                           (+ original-first-length (length alternative-first)))
                                          (setq
                                           first
                                           (append original-first alternative-first)))
                                      (push
                                       `(
                                         ,alternative-tape-index
                                         ,alternative-first-length
                                         ,alternative-first)
                                       stack))))
                                (setq
                                 symbol-f-set-index
                                 (1+ symbol-f-set-index)))))))))
                    (setq
                     input-tape-index
                     (1+ input-tape-index)))
                  (when (> first-length 0)

                    ;; If length exceeds k, strip trailing symbols
                    (when (> (length first) k)
                      (setq first (reverse first))
                      (while (> (length first) k)
                        (pop first))
                      (setq first (reverse first)))

                    ;; When length of terminals list is below K
                    ;; fill up with e-identifiers
                    (when (and
                           (< (length first) k))
                      ;; (message "first-before-fill: %s" first)
                      (setq first (reverse first))
                      (while (< (length first) k)
                        (push parser-generator--e-identifier first))
                      (setq first (reverse first))
                      ;; (message "first-after-fill: %s" first)
                      )
                    (unless
                        (gethash
                         first
                         first-items)
                      (parser-generator--debug
                       (message
                        "push to first-list: %s to %s"
                        first
                        first-list))
                      (puthash
                       first
                       t
                       first-items)
                      (push
                       first
                       first-list)))))))
          (unless skip-sorting
            (setq
             first-list
             (sort
              first-list
              'parser-generator--sort-list)))
          (puthash
           hash-key
           first-list
           parser-generator--table-firsts))))
    (gethash
     hash-key
     parser-generator--table-firsts)))

;; TODO Should add support for e-identifiers
;; Definition at p. 343
(defun parser-generator--follow (β)
  "Calculate follow-set of Β.  FOLLOW(β) = w, w is the set {w | S =>* αβγ and w is in FIRST(γ)}."
  ;; Make sure argument is a list
  (unless (listp β)
    (setq β (list β)))
  (let ((follow-set nil)
        (match-length (length β)))
    ;; Iterate all productions in grammar
    (let ((productions
           (parser-generator--get-grammar-productions)))
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
      (setq follow-set
            (parser-generator--distinct follow-set)))
    follow-set))


(provide 'parser-generator)

;;; parser-generator.el ends here
