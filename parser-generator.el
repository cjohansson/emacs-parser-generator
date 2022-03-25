;;; parser-generator.el --- Parser Generator library -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 10 Oct 2020
;; Modified: 12 Feb 2021
;; Version: 0.1.4
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
  nil
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
   nil))

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
                   ";; Production %s: %S"
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

(defun parser-generator-set-eof-identifier (eof-identifier)
  "Set EOF-IDENTIFIER."
  (unless (or
           (stringp eof-identifier)
           (symbolp eof-identifier))
    (error "EOF-identifier must be a symbol or string!"))
  (setq parser-generator--eof-identifier eof-identifier))

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
  (message "\n;; Starting process of grammar..\n")
  (parser-generator--clear-cache)
  (unless parser-generator--look-ahead-number
    (error "No look-ahead-number defined!"))
  (unless
      (parser-generator--valid-look-ahead-number-p
       parser-generator--look-ahead-number)
    (error "Invalid look-ahead number k!"))
  (message ";; k = %d" parser-generator--look-ahead-number)
  (unless parser-generator--grammar
    (error "No grammar defined!"))
  (unless
      (parser-generator--valid-grammar-p
       parser-generator--grammar)
    (error "Invalid grammar G!"))
  (parser-generator--load-symbols)
  (message "\n;; Completed process of grammar\n"))

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
          (k (max 1 parser-generator--look-ahead-number)))
      (let ((f-sets (make-hash-table :test 'equal))
            (i 0)
            (max-i 100)
            (expanded-all))

        (while (not expanded-all)
          (when (> i max-i)
            (error "Endless loop!"))
          (parser-generator--debug
           (message "i = %s" i))
          (let ((f-set
                 (make-hash-table :test 'equal))
                (distinct-lhs)
                (distinct-lhs-p
                 (make-hash-table :test 'equal))
                (previous-f-set))
            (when (> i 0)
              (setq
               expanded-all
               t)
              (setq
               previous-f-set
               (gethash
                (1- i)
                f-sets)))

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
                (dolist (rhs-p production-rhs)
                  (let ((rhs-string rhs-p))
                    (let ((rhs-leading-symbols
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
                        "\nrhs-leading-symbols: %S = %S"
                        rhs-string
                        rhs-leading-symbols))

                      (parser-generator--debug
                       (message
                        "Leading %d symbols at index %s: %s -> %s = %s"
                        k
                        i
                        production-lhs
                        rhs-string
                        rhs-leading-symbols))

                      (when rhs-leading-symbols
                        (if (gethash
                             production-lhs
                             f-set)
                            (puthash
                             production-lhs
                             (append
                              (gethash
                               production-lhs
                               f-set)
                              rhs-leading-symbols)
                             f-set)
                          (puthash
                           production-lhs
                           rhs-leading-symbols
                           f-set))

                        (unless (gethash
                                 production-lhs
                                 distinct-lhs-p)
                          (puthash
                           production-lhs
                           t
                           distinct-lhs-p)
                          (push
                           production-lhs
                           distinct-lhs))))))))

            ;; Iterate productions again
            ;; make distinct sets and check if we found anything new
            (dolist (production-lhs distinct-lhs)
              (when (gethash
                     production-lhs
                     f-set)

                ;; Make set distinct
                (puthash
                 production-lhs
                 (parser-generator--distinct
                  (gethash
                   production-lhs
                   f-set))
                 f-set)

                ;; Sort it for a more deterministic result
                (puthash
                 production-lhs
                 (sort
                  (parser-generator--distinct
                   (gethash
                    production-lhs
                    f-set))
                  'parser-generator--sort-list)
                 f-set)

                ;; If this set differs from the last, keep expanding
                (when (and
                       expanded-all
                       previous-f-set
                       (not
                        (equal
                         (gethash
                          production-lhs
                          f-set)
                         (gethash
                          production-lhs
                          previous-f-set))))
                  (parser-generator--debug
                   (message
                    "F_%s%s = %S is new compared to F_%s%s = %S"
                    i
                    production-lhs
                    (gethash
                     production-lhs
                     f-set)
                    (1- i)
                    production-lhs
                    (gethash
                     production-lhs
                     previous-f-set)))
                  (setq
                   expanded-all
                   nil))))

            (puthash
             i
             f-set
             f-sets)
            (parser-generator--debug
             (message
              "F_%s = %s"
              i
              f-set))
            (setq
             i
             (1+ i))))

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

(defun parser-generator--merge-max-terminal-sets (a b)
  "Calculate list of all lists of L1 (+) L2 which is a merge of all terminals in lists A combined with all terminals in lists B but with maximum length of the set look-ahead number."
  (let ((a-length (length a))
        (a-index 0)
        (b-length (length b))
        (merged-lists))
    (cond
     ((and a b)
      (while (< a-index a-length)
        (let ((a-element (nth a-index a))
              (b-index 0))
          (while (< b-index b-length)
            (let ((b-element (nth b-index b)))
              (when-let
                  ((merged-element
                    (parser-generator--merge-max-terminals
                     a-element
                     b-element)))
                (if merged-lists
                    (setq
                     merged-lists
                     (append
                      merged-lists
                      (list merged-element)))
                  (setq
                   merged-lists
                   (list merged-element)))))
            (setq b-index (1+ b-index)))
          (setq a-index (1+ a-index)))))
     (a
      (while (< a-index a-length)
        (let ((a-element (nth a-index a)))
          (when-let
              ((merged-element
                (parser-generator--merge-max-terminals
                 a-element
                 nil)))
            (if merged-lists
                (setq
                 merged-lists
                 (append
                  merged-lists
                  (list merged-element)))
              (setq
               merged-lists
               (list merged-element)))))
        (setq a-index (1+ a-index))))

     (b
      (let ((b-index 0))
        (while (< b-index b-length)
          (let ((b-element (nth b-index b)))
            (when-let
                ((merged-element
                  (parser-generator--merge-max-terminals
                   nil
                   b-element)))
              (if merged-lists
                  (setq
                   merged-lists
                   (append
                    merged-lists
                    (list merged-element)))
                (setq
                 merged-lists
                 (list merged-element)))))
          (setq b-index (1+ b-index))))))
    (setq
     merged-lists
     (parser-generator--distinct
      merged-lists))
    (setq
     merged-lists
     (sort
      merged-lists
      'parser-generator--sort-list))
    merged-lists))

;; Lemma 5.1 p. 348
(defun parser-generator--merge-max-terminals (a b)
  "Calculate L1 (+) L2 which is a merge of all terminals in A and B but with exactly length of the set look-ahead number."
  (let ((k (max 1 parser-generator--look-ahead-number))
        (merged)
        (merge-count 0)
        (a-element)
        (a-index 0)
        (a-length (length a))
        (b-element)
        (b-index 0)
        (b-length (length b)))
    (while (and
            (< a-index a-length)
            (< merge-count k))
      (setq a-element (nth a-index a))
      (unless (parser-generator--valid-e-p a-element)
        (push a-element merged)
        (setq merge-count (1+ merge-count)))
      (setq a-index (1+ a-index)))
    (while (and
            (< b-index b-length)
            (< merge-count k))
      (setq b-element (nth b-index b))
      (unless (parser-generator--valid-e-p b-element)
        (push b-element merged)
        (setq merge-count (1+ merge-count)))
      (setq b-index (1+ b-index)))
    (if (= merge-count k)
        (nreverse merged)
      nil)))

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
        (f-sets (nth 2 state)))
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
                 (length leading-symbols))
                (keep-iterating t))
            (parser-generator--debug
             (message
              "leading-terminals-count: %s"
              leading-terminals-count))

            (while (and
                    keep-iterating
                    (< input-tape-index input-tape-length)
                    (< leading-terminals-count k))
              (let ((rhs-element
                     (nth input-tape-index input-tape))
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
                 ((parser-generator--valid-eof-p rhs-element)
                  (setq rhs-type 'EOF))
                 (t (error (format "Invalid symbol %s!" rhs-element))))
                (parser-generator--debug
                 (message
                  "rhs-type: %s"
                  rhs-type))

                (cond

                 ((equal rhs-type 'NON-TERMINAL)
                  (if (> i 0)
                      (let ((sub-terminal-sets
                             (gethash
                              (list rhs-element)
                              (gethash
                               (1- i)
                               f-sets))))
                        (parser-generator--debug
                         (message
                          "sub-terminal-sets: %s = %s"
                          (list rhs-element)
                          sub-terminal-sets))

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
                              (let ((sub-symbols-set-index 0)
                                    (original-leading-symbols
                                     leading-symbols)
                                    (original-leading-terminals
                                     leading-terminals))
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
                                        (sub-symbols
                                         (reverse original-leading-symbols))
                                        (sub-terminals
                                         (reverse original-leading-terminals)))
                                    (while (and
                                            (< sub-symbol-index sub-symbol-length)
                                            (< (length sub-terminals) k))
                                      (setq
                                       sub-symbol
                                       (nth
                                        sub-symbol-index
                                        sub-symbol-alternative-set))
                                      (parser-generator--debug
                                       (message
                                        "sub-symbol: %S"
                                        sub-symbol))
                                      (push
                                       sub-symbol
                                       sub-symbols)
                                      (unless (parser-generator--valid-e-p sub-symbol)
                                        (push
                                         sub-symbol
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

                                    ;; The first iteration does not branch off
                                    (if (= sub-symbols-set-index 0)
                                        (progn
                                          (setq
                                           leading-symbols
                                           sub-symbols)
                                          (setq
                                           leading-symbols-count
                                           (length leading-symbols))
                                          (setq
                                           leading-terminals
                                           sub-terminals)
                                          (setq
                                           leading-terminals-count
                                           (length leading-terminals)))
                                      (let (
                                            (branch
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
                                         stack))))
                                  (setq
                                   sub-symbols-set-index
                                   (1+ sub-symbols-set-index)))))

                          (parser-generator--debug
                           (message
                            "Found no subsets for %s %s"
                            rhs-element
                            (1- i)))
                          (setq
                           keep-iterating
                           nil)))
                    (setq
                     keep-iterating
                     nil)))

                 ((equal rhs-type 'E-IDENTIFIER)
                  (setq
                   leading-symbols
                   (append
                    leading-symbols
                    rhs-element))
                  (setq
                   leading-symbols-count
                   (1+ leading-symbols-count)))

                 ((or
                   (equal rhs-type 'TERMINAL)
                   (equal rhs-type 'EOF))
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
    f-set))

;; Algorithm 5.5, p. 357
(defun parser-generator--first
    (β &optional disallow-e-first ignore-validation skip-sorting)
  "For sentential-form Β, calculate first terminals, optionally DISALLOW-E-FIRST, IGNORE-VALIDATION and SKIP-SORTING."

  ;; Make sure we are dealing with a list of symbols
  (unless (listp β)
    (setq β (list β)))

  (parser-generator--debug
   (if disallow-e-first
       (message
        "\nE-FREE-FIRST%S"
        β)
     (message
      "\nFIRST%S"
      β)))

  ;; Perform optional validation of inpuit
  (unless (or
           ignore-validation
           (parser-generator--valid-sentential-form-p β))
    (error "Invalid sentential form β! %s" β))

  ;; Generate F-sets only once per grammar
  (parser-generator--generate-f-sets)

  ;; Algorithm
  ;; 1. Iterate each symbol of input and expand into list of lists of terminals and the e-identifier
  ;;     if input symbol is a terminal, the e-identifier or the EOF-identifier push it to each expanded list
  ;;     if input symbol is a non-terminal, expand it and push each possible expansion onto each expanded list
  ;; 2. Reverse each expanded list and place each list on a stack of unprocessed lists each with a input-index to zero
  ;; 3. Process each unprocessed list and expand into a list of lists of terminals and the e-identifier
  ;;        pop a unprocessed list from the stack of unprocessed lists
  ;;            create a new empty list
  ;;            set skip-flag to false
  ;;            set loop-flag to true
  ;;            loop while index is below length and skip-flag is false and loop-flag is true
  ;;                if a list starts with the e-identifier and it is disallowed, set skip-flag to true to stop iterating
  ;;                if a symbol on a list is a terminal push it onto the new list
  ;;                if a symbol on a the list is the e-identifier
  ;;                    push a copy of the new list on the unprocessed stack but increase it's input-index by one
  ;;                    push the e-identifier onto the new list and set loop-flag to false to stop iterating
  ;;                increase index with one
  ;;            if skip-flag is false place new list onto the list of processed lists
  ;; 4. Reverse each processed list
  ;; 5. Return processed lists

  (let ((expanded-lists nil)
        (processed-lists)
        (k (max 1 parser-generator--look-ahead-number)))

    ;; 1. Iterate each symbol of input and expand into list of lists of terminals and the e-identifier
    (let ((input-tape β)
          (input-tape-index 0)
          (input-tape-length (length β))
          (input-symbol)
          (still-looking t))

      (parser-generator--debug
       (message
        "\nExpanding symbols.. %S"
        input-tape)
       (message
        "Length: %S"
        input-tape-length))

      (while (and
              (< input-tape-index input-tape-length)
              still-looking)
        (setq
         input-symbol
         (nth input-tape-index input-tape))
        (parser-generator--debug
         (message
          "\ninput-symbol: %S"
          input-symbol))
        (cond

         ;; if input symbol is a non-terminal, expand it and push each possible expansion onto each expanded list
         ((parser-generator--valid-non-terminal-p input-symbol)
          (parser-generator--debug
           (message
            "input-symbol is non-terminal"))
          (let ((expanded-non-terminal-lists
                 (gethash
                  (list input-symbol)
                  parser-generator--f-sets)))
            (let ((expanded-list-index)
                  (expanded-list-count
                   (length expanded-lists)))
              (parser-generator--debug
               (message
                "non-terminal expands into: %S with count: %d"
                expanded-non-terminal-lists
                (length expanded-non-terminal-lists)))

              (if (= expanded-list-count 0)
                  (dolist (expanded-non-terminal-list expanded-non-terminal-lists)
                    (push
                     (reverse expanded-non-terminal-list)
                     expanded-lists))

                (let ((new-expanded-lists))
                  (dolist (expanded-non-terminal-list expanded-non-terminal-lists)
                    (setq expanded-list-index 0)
                    (let ((reversed-expanded-non-terminal-list
                           (reverse expanded-non-terminal-list)))
                      (while (< expanded-list-index expanded-list-count)
                        (push
                         (append
                          reversed-expanded-non-terminal-list
                          (nth expanded-list-index expanded-lists))
                         new-expanded-lists)
                        (setq
                         expanded-list-index
                         (1+ expanded-list-index)))))
                  (setq
                   expanded-lists
                   new-expanded-lists)))
              (parser-generator--debug
               (message
                "expanded-lists after adding: %S"
                expanded-lists)))))

         ;; if input symbol is a terminal
         ;; or the e-identifier
         ;; or the eof-identifier
         ;; push it to each expanded list
         ((or
           (parser-generator--valid-e-p input-symbol)
           (parser-generator--valid-eof-p input-symbol)
           (parser-generator--valid-terminal-p input-symbol))
          (parser-generator--debug
           (message
            "symbol is a terminal, the e-identifier or the EOF-identifier"))
          (let ((expanded-list-index 0)
                (expanded-list-count
                 (length expanded-lists)))
            (if (= expanded-list-count 0)
                (setq
                 expanded-lists
                 (list (list input-symbol)))
              (while (< expanded-list-index expanded-list-count)
                (setf
                 (nth expanded-list-index expanded-lists)
                 (append
                  (list input-symbol)
                  (nth expanded-list-index expanded-lists)))
                (setq
                 expanded-list-index
                 (1+ expanded-list-index))))
            (parser-generator--debug
             (message
              "expanded-lists after adding: %S"
              expanded-lists)))))

        ;; Iterate all expanded lists to determine if there is
        ;; a point in keep expanding symbols or if we already have enough
        ;; of terminals
        (let ((minimum-terminal-count)
              (expanded-lists-index 0)
              (expanded-list)
              (expanded-lists-length (length expanded-lists)))
          (while (< expanded-lists-index expanded-lists-length)
            (setq
             expanded-list
             (reverse (nth expanded-lists-index expanded-lists)))
            (let ((expanded-list-terminal-count 0)
                  (expanded-list-index 0)
                  (expanded-symbol)
                  (expanded-list-length (length expanded-list)))
              (while (< expanded-list-index expanded-list-length)
                (setq
                 expanded-symbol
                 (nth expanded-list-index expanded-list))
                (when (or
                       (parser-generator--valid-terminal-p
                        expanded-symbol)
                       (parser-generator--valid-eof-p
                        expanded-symbol))
                  (setq
                   expanded-list-terminal-count
                   (1+ expanded-list-terminal-count)))
                (setq
                 expanded-list-index
                 (1+ expanded-list-index)))
              (when (or
                     (not minimum-terminal-count)
                     (< expanded-list-terminal-count minimum-terminal-count))
                (setq
                 minimum-terminal-count
                 expanded-list-terminal-count)))
            (setq
             expanded-lists-index
             (1+ expanded-lists-index)))
          (when (and
                 minimum-terminal-count
                 (>= minimum-terminal-count k))
            (setq still-looking nil)
            (parser-generator--debug
             (message
              "Has expanded k=%d enough symbols after %d iterations"
              k
              (1+ input-tape-index)))))

        (setq
         input-tape-index
         (1+ input-tape-index))))

    (if expanded-lists
        (let ((unprocessed-lists)
              (distinct-processed-lists (make-hash-table :test 'equal)))
          (parser-generator--debug
           (message
            "\nExpanded symbols: %S (in reverse order)"
            expanded-lists))

          ;; 2. Place each expanded list on a stack of unprocessed lists
          ;; each with a input-index to zero and an empty processed list
          (let ((expanded-list-index 0)
                (expanded-list-count
                 (length expanded-lists)))
            (while (< expanded-list-index expanded-list-count)
              (push
               (list
                (reverse (nth expanded-list-index expanded-lists))
                0
                nil)
               unprocessed-lists)
              (setq
               expanded-list-index
               (1+ expanded-list-index))))

          ;; 3. Process each unprocessed list and expand into a list of lists of terminals and the e-identifier
          (let ((unprocessed-data)
                (unprocessed-list)
                (unprocessed-list-length)
                (unprocessed-list-index)
                (processed-list))
            (while unprocessed-lists
              (setq
               unprocessed-data
               (pop unprocessed-lists))
              (setq
               unprocessed-list
               (nth 0 unprocessed-data))
              (setq
               unprocessed-list-index
               (nth 1 unprocessed-data))
              (setq
               unprocessed-list-length
               (length unprocessed-list))
              (setq
               processed-list
               (nth 2 unprocessed-data))
              (parser-generator--debug
               (message
                "\nunprocessed-list: %S"
                unprocessed-list)
               (message
                "unprocessed-list-index: %S"
                unprocessed-list-index)
               (message
                "unprocessed-list-length: %S"
                unprocessed-list-length))

              (let ((skip-flag)
                    (loop-flag t))
                (while (and
                        (not skip-flag)
                        loop-flag
                        (< unprocessed-list-index unprocessed-list-length))
                  (let ((unprocessed-list-symbol
                         (nth unprocessed-list-index unprocessed-list)))

                    ;; if a list starts with the e-identifier and it is disallowed
                    ;; set skip-flag to true to stop iterating
                    (if (and
                         disallow-e-first
                         (= unprocessed-list-index 0)
                         (parser-generator--valid-e-p
                          unprocessed-list-symbol))
                        (progn
                          (setq
                           skip-flag
                           t)
                          (parser-generator--debug
                           (message
                            "Unprocessed list starts with e-identifier, skipping")))

                      (cond

                       ;; if a symbol on a the list is the e-identifier
                       ((parser-generator--valid-e-p
                         unprocessed-list-symbol)

                        ;; push a copy of the new list on the unprocessed stack but increase it's input-index by one
                        (let ((unprocessed-branch
                               (list
                                unprocessed-list
                                (1+ unprocessed-list-index)
                                processed-list)))
                          (parser-generator--debug
                           (message
                            "Pushed unprocessed-branch to unprocessed-lists: %S"
                            unprocessed-branch))
                          (push
                           unprocessed-branch
                           unprocessed-lists))

                        (parser-generator--debug
                         (message
                          "Added e-identifier to processed list: %S"
                          processed-list))
                        (push
                         unprocessed-list-symbol
                         processed-list)
                        (setq
                         loop-flag
                         nil))

                       (t
                        (push
                         unprocessed-list-symbol
                         processed-list)
                        (parser-generator--debug
                         (message
                          "Added terminal %S to processed list: %S"
                          unprocessed-list-symbol
                          processed-list)))))

                    (setq
                     unprocessed-list-index
                     (1+ unprocessed-list-index))))

                ;; if skip-flag is false place reversed new list onto the list of processed lists
                (if skip-flag
                    (progn
                      (parser-generator--debug
                       (message
                        "Skip flag is set, ignoring resulted list: %S with length: %d"
                        processed-list
                        (length processed-list))))

                  (parser-generator--debug
                   (message
                    "Skip flag is not set, proceeding with resulted list: %S with length: %d"
                    processed-list
                    (length processed-list)))

                  ;; If length of a set is below K fill it up with e-identifiers
                  (when (< (length processed-list) k)
                    (let ((missing-symbol-count
                           (- k (length processed-list)))
                          (missing-symbol-index 0))
                      (while (< missing-symbol-index missing-symbol-count)
                        (push
                         parser-generator--e-identifier
                         processed-list)
                        (setq
                         missing-symbol-index
                         (1+ missing-symbol-index)))
                      (parser-generator--debug
                       (message
                        "Added %d trailing e-identifiers to set"
                        missing-symbol-count))))

                  (when (> (length processed-list) k)
                    (let ((obsolete-symbol-count
                           (- (length processed-list) k))
                          (obsolete-symbol-index 0))
                      (while (< obsolete-symbol-index obsolete-symbol-count)
                        (pop
                         processed-list)
                        (setq
                         obsolete-symbol-index
                         (1+ obsolete-symbol-index)))
                      (parser-generator--debug
                       (message
                        "Stripped away %d trailing symbols from set"
                        obsolete-symbol-count))))

                  (parser-generator--debug
                   (message
                    "processed-list: %S"
                    processed-list))

                  ;; Reverse list
                  (setq
                   processed-list
                   (reverse
                    processed-list))

                  ;; Make sure only distinct sets are added to list
                  (let ((processed-list-hash-key
                         (format
                          "%S"
                          processed-list)))
                    (if (gethash
                         processed-list-hash-key
                         distinct-processed-lists)
                        (progn
                          (parser-generator--debug
                           (message
                            "Processed list already existed in set, skipping %S"
                            processed-list)))

                      (push
                       processed-list
                       processed-lists)
                      (puthash
                       processed-list-hash-key
                       t
                       distinct-processed-lists)
                      (parser-generator--debug
                       (message
                        "Processed list is new, added to set %S"
                        processed-list)))))))))

      (parser-generator--debug
       (message
        "\nFailed to expand symbols!")))

    ;; Optional sorting
    (when (and
           processed-lists
           (not skip-sorting))
      (setq
       processed-lists
       (sort
        processed-lists
        'parser-generator--sort-list)))

    (parser-generator--debug
     (message
      "processed-lists: %S"
      processed-lists))

    processed-lists))

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
