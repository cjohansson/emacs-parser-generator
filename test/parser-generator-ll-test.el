;; parser-generator-ll-test.el --- Tests for LL(k) Parser Generator -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator-ll)
(require 'ert)

(defun parser-generator-ll-test--generate-goto-table ()
  "Test `parser-generator-ll--generate-goto-table'."
  (message "Started tests for (parser-generator-ll--generate-goto-table)")

  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S (a A a a) (b A b a))
      (A b e)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (let ((tables (parser-generator-ll--generate-goto-table)))
    ;; (message "tables: %S" tables)
    (should
     (equal
      tables
      '(
        (
         ((A) (b a)) ;; T A,{ba}
         (
          ((b b) (b) nil)
          ((b a) (e) nil)
          )
         )
        (
         ((A) (a a)) ;; T A,{aa}
         (
          ((a a) (e) nil)
          ((b a) (b) nil)
          )
         )
        (
         ((S) ($ $)) ;; T0
         (
          ((a b) (a A a a) (((a a))))
          ((a a) (a A a a) (((a a))))
          ((b b) (b A b a) (((b a))))
          )
         )
        )
      )
     ))
  (message "Passed Example 5.14 p. 350 and 5.15 p. 351")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S e (a b A))
      (A (S a a) b)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (let* ((tables
          (parser-generator-ll--generate-goto-table)))
    ;; (message "tables: %S" tables)
    (should
     (equal
      tables
      '(
        (
         ((A) (a a)) ;; T3
         (
          ((a b) (S a a) (((a a))))
          ((a a) (S a a) (((a a))))
          ((b a) (b) nil)
          )
         )
        (
         ((S) (a a)) ;; T2
         (
          ((a b) (a b A) (((a a))))
          ((a a) (e) nil)
          )
         )
        (
         ((A) ($ $)) ;; T1
         (
          ((a b) (S a a) (((a a))))
          ((a a) (S a a) (((a a))))
          ((b $) (b) nil)
          )
         )
        (
         ((S) ($ $)) ;; T0
         (
          (($ $) (e) nil)
          ((a b) (a b A) ((($ $))))
          )
         )
        )
      ))
    )
  (message "Passed Example 5.17 p. 354")

  (message "Passed tests for (parser-generator-ll--generate-goto-table)"))

(defun parser-generator-ll-test--generate-action-table-k-gt-1 ()
  "Test `parser-generator-ll--generate-action-table-k-gt-1'."
  (message "Started tests for (parser-generator-ll--generate-action-table-k-gt-1)")

  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S (a A a a) (b A b a))
      (A b e)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (let* ((goto-table
          (parser-generator-ll--generate-goto-table))
         (action-table
          (parser-generator-ll--generate-action-table-k-gt-1
           goto-table)))
    ;; (message "goto-table: %S" goto-table)
    ;; (message "action-table: %S" action-table)
    (should
     (equal
      '(
        (
         ((S) ($ $)) ;; T0
         (
          ((b b) reduce (b ((A) (b a)) b a) 1)
          ((a a) reduce (a ((A) (a a)) a a) 0)
          ((a b) reduce (a ((A) (a a)) a a) 0)
          )
         )
        (
         ((A) (a a)) ;; T1
         (
          ((b a) reduce (b) 2)
          ((a a) reduce (e) 3)
          )
         )
        (
         ((A) (b a)) ;; T2
         (
          ((b a) reduce (e) 3)
          ((b b) reduce (b) 2)
          )
         )
        (b (((b b) pop) ((b a) pop) ((b $) pop)))
        (a (((a b) pop) ((a a) pop) ((a $) pop)))
        ($ ((($ $) accept)))
        )
      action-table)))
  (message "Passed Example 5.15 p. 351 and 5.16 p. 352")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S e (a b A))
      (A (S a a) b)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (let* ((goto-table
          (parser-generator-ll--generate-goto-table))
         (action-table
          (parser-generator-ll--generate-action-table-k-gt-1
           goto-table)))
    ;; (message "goto-tables: %S" goto-table)
    ;; (message "action-table: %S" action-table)
    (should
     (equal
      '(
        (
         ((S) ($ $)) ;; T0
         (
          ((a b) reduce (a b ((A) ($ $))) 1)
          (($ $) reduce (e) 0)
          )
         )
        (
         ((A) ($ $)) ;; T1
         (
          ((b $) reduce (b) 3)
          ((a a) reduce (((S) (a a)) a a) 2)
          ((a b) reduce (((S) (a a)) a a) 2)
          )
         )
        (
         ((S) (a a)) ;; T2
         (
          ((a a) reduce (e) 0)
          ((a b) reduce (a b ((A) (a a))) 1)
          )
         )
        (
         ((A) (a a)) ;; T3
         (
          ((b a) reduce (b) 3)
          ((a a) reduce (((S) (a a)) a a) 2)
          ((a b) reduce (((S) (a a)) a a) 2)
          )
         )
        (b (((b b) pop) ((b a) pop) ((b $) pop)))
        (a (((a b) pop) ((a a) pop) ((a $) pop)))
        ($ ((($ $) accept)))
        )
      action-table)))
  (message "Passed Example 5.17 p. 356")

  (message "Passed tests for (parser-generator-ll--generate-action-table-k-gt-1)"))

(defun parser-generator-ll-test-parse ()
  "Test `parser-generator-ll-parse'."
  (message "Started tests for (parser-generator-ll-parse)")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S (a A a a) (b A b a))
      (A b e)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((b 1 . 2) (b 2 . 3) (a 3 . 4)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should
   (equal
    '(1 3) ;; Example is indexed from 1 so that is why they have '(2 4)
    (parser-generator-ll-parse)))
  (message "Passed example 5.16 p. 352")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((b 1 . 2) (b 2 . 3)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should-error
   (parser-generator-ll-parse))
  (message "Passed failing variant of example 5.16 p. 352")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S e (a b A))
      (A (S a a) b)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((a 1 . 2) (b 2 . 3) (a 3 . 4) (a 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should
   (equal
    '(1 2 0) ;; Example is indexed from 1 so that is why they have '(2 3 1)
    (parser-generator-ll-parse)))
  (message "Passed example 5.17 p. 355")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S (a A S) b)
      (A a (b S A))
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((a 1 . 2) (b 2 . 3) (b 3 . 4) (a 4 . 5) (b 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should
   (equal
    '(0 3 1 2 1) ;; Example is indexed from 1 so that is why they have '(1 4 2 3 2)
    (parser-generator-ll-parse)))
  (message "Passed example 5.5 p. 340")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (E E2 T T2 F)
     ("a" "(" ")" "+" "*")
     (
      (E (T E2))
      (E2 ("+" T E2) e)
      (T (F T2))
      (T2 ("*" F T2) e)
      (F ("(" E ")") "a")
      )
     E
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '(("(" 1 . 2) ("a" 2 . 3) ("*" 3 . 4) ("a" 4 . 5) (")" 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should
   (equal
    '(0 3 6 0 3 7 4 7 5 2 5 2) ;; Example is 1-indexed '(1 4 7 1 4 8 5 8 6 3 6 3)
    (parser-generator-ll-parse)))
  (message "Passed example 5.12 p. 346-347")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (S F)
     ("(" "a" ")" "+")
     (
      (S F)
      (S ("(" S "+" F ")"))
      (F "a")
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '(("(" 1 . 2) ("a" 2 . 3) ("+" 3 . 4) ("a" 4 . 5) (")" 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should
   (equal
    '(1 0 2 2) ;; Example is 1 indexed '(2 1 3 3)
    (parser-generator-ll-parse)))
  (message "Passed example from Wikipedia")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '(("(" 1 . 2) ("a" 2 . 3) ("+" 3 . 4) ("a" 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should-error
   (parser-generator-ll-parse))
  (message "Passed failing variant of example from Wikipedia")

  (message "Passed tests for (parser-generator-ll-parse)"))

(defun parser-generator-ll-test-translate ()
  "Test `parser-generator-ll-translate'."
  (message "Started tests for (parser-generator-ll-translate)")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S
       (a A a a (lambda(a b) (format "alfa %s laval" (nth 1 a))))
       (b A b a (lambda(a b) (format "delta %s laval" (nth 1 a))))
       )
      (A
       (b (lambda(a b) "sven"))
       (e (lambda(a b) "ingrid"))
       )
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((b 1 . 2) (b 2 . 3) (a 3 . 4)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should
   (equal
    "delta ingrid laval"
    (parser-generator-ll-translate)))
  (message "Passed translation test 1")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((b 1 . 2) (b 2 . 3) (b 3 . 4) (a 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (should
   (equal
    "delta sven laval"
    (parser-generator-ll-translate)))
  (message "Passed translation test 2")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S
       (a A S (lambda(a b) (format "alfa %s %s" (nth 1 a) (nth 2 a))))
       (b (lambda(a b) "beta"))
       )
      (A
       (a (lambda(a b) "delta"))
       (b S A (lambda(a b) (format "gamma %s %s" (nth 1 a) (nth 2 a))))
       )
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((a 1 . 2) (b 2 . 3) (b 3 . 4) (a 4 . 5) (b 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (should
   (equal
    "beta"
    (parser-generator-ll-translate)))
  (message "Passed translation test 3")

  (message "Passed tests for (parser-generator-ll-translate)"))

(defun parser-generator-ll-test-generate-table ()
  "Test `parser-generator-ll-generate-table'."
  (message "Started tests for (parser-generator-ll-generate-table)")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S e (a b A))
      (A (S a a) b)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  ;; (message "parsing-table: %S" (parser-generator--hash-to-list parser-generator-ll--table t))
  (should
   (equal
    '(
      ("((S) ($ $))"
       (
        ("(a b)" (reduce (a b ((A) ($ $))) 1))
        ("($ $)" (reduce (e) 0))
        )
       )
      ("((A) ($ $))"
       (
        ("(b $)" (reduce (b) 3))
        ("(a a)" (reduce (((S) (a a)) a a) 2))
        ("(a b)" (reduce (((S) (a a)) a a) 2))
        )
       )
      ("((S) (a a))"
       (
        ("(a a)" (reduce (e) 0))
        ("(a b)" (reduce (a b ((A) (a a))) 1))
        )
       )
      ("((A) (a a))"
       (
        ("(b a)" (reduce (b) 3))
        ("(a a)" (reduce (((S) (a a)) a a) 2))
        ("(a b)" (reduce (((S) (a a)) a a) 2))
        )
       )
      ("b" (("(b b)" pop) ("(b a)" pop) ("(b $)" pop)))
      ("a" (("(a b)" pop) ("(a a)" pop) ("(a $)" pop)))
      ("$" (("($ $)" accept)))
      )
    (parser-generator--hash-to-list
     parser-generator-ll--table
     t)))

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S (a A S) b)
      (A a (b S A))
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (parser-generator-ll-generate-table)
  ;; (message "parsing-table: %S" (parser-generator--hash-to-list parser-generator-ll--table t))
  (should
   (equal
    '(
      ("S"
       (
        ("(b)" (reduce (b) 1))
        ("(a)" (reduce (a A S) 0))
        )
       )
      ("A"
       (
        ("(b)" (reduce (b S A) 3))
        ("(a)" (reduce (a) 2))
        )
       )
      ("b" (("(b)" pop)))
      ("a" (("(a)" pop)))
      ("$" (("($)" accept)))
      )
    (parser-generator--hash-to-list
     parser-generator-ll--table
     t)))

  (message "Passed tests for (parser-generator-ll-generate-table)"))

(defun parser-generator-ll-test--valid-grammar-k-gt-1-p ()
  "Test `parser-generator-ll--valid-grammar-k-gt-1-p'."
  (message "Started tests for (parser-generator-ll--valid-grammar-k-gt-1-p)")

  ;; Example 5.14 p. 350
  ;; Example 5.15 p. 351
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S (a A a a) (b A b a))
      (A b e)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (should
   (equal
    (parser-generator-ll--valid-grammar-k-gt-1-p)
    t))
  (message "Passed first valid test")

  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S (a A a a) (b A b a))
      (A b e a)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (should
   (equal
    (parser-generator-ll--valid-grammar-k-gt-1-p)
    nil))
  (message "Passed second valid test")

  (message "Passed tests for (parser-generator-ll--valid-grammar-k-gt-1-p)"))

(defun parser-generator-ll-test--generate-action-table-k-eq-1 ()
  "Test `parser-generator-ll--generate-action-table-k-eq-1'."
  (message "Started tests for (parser-generator-ll--generate-action-table-k-eq-1)")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (S A)
     (a b)
     (
      (S (a A S) b)
      (A a (b S A))
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (let* ((tables
          (parser-generator-ll--generate-action-table-k-eq-1
           (parser-generator-ll--generate-goto-table))))
    ;; (message "tables: %S" tables)
    (should
     (equal
      '(
        (S
         (
          ((b) reduce (b) 1)
          ((a) reduce (a A S) 0)
          )
         )
        (A
         (
          ((b) reduce (b S A) 3)
          ((a) reduce (a) 2)
          )
         )
        (b (((b) pop)))
        (a (((a) pop)))
        ($ ((($) accept)))
        )
      tables
     )))
  (message "Passed Example 5.5 p. 340")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (E E2 T T2 F)
     ("a" "(" ")" "+" "*")
     (
      (E (T E2))
      (E2 ("+" T E2) e)
      (T (F T2))
      (T2 ("*" F T2) e)
      (F ("(" E ")") "a")
      )
     E
     )
   )
  (parser-generator-process-grammar)
  (let ((tables
         (parser-generator-ll--generate-action-table-k-eq-1
          (parser-generator-ll--generate-goto-table))))
    ;; (message "tables: %S" tables)
    (should
     (equal
      '(
        (E
         (
          (("a") reduce (T E2) 0)
          (("(") reduce (T E2) 0)
          )
         )
        (E2
         (
          (($) reduce (e) 2)
          (("+") reduce ("+" T E2) 1)
          ((")") reduce (e) 2)
          )
         )
        (T
         (
          (("a") reduce (F T2) 3)
          (("(") reduce (F T2) 3)
          )
         )
        (T2
         (
          (("+") reduce (e) 5)
          ((")") reduce (e) 5)
          (("*") reduce ("*" F T2) 4)
          (($) reduce (e) 5)
          )
         )
        (F
         (
          (("a") reduce ("a") 7)
          (("(") reduce ("(" E ")") 6)
          )
         )
        ("a" ((("a") pop)))
        ("+" ((("+") pop)))
        ("*" ((("*") pop)))
        (")" (((")") pop)))
        ("(" ((("(") pop)))
        ($ ((($) accept)))
        )
      tables)))
  (message "Passed Example 5.12 p. 346-347")

  (message "Passed tests for (parser-generator-ll--generate-action-table-k-eq-1)"))

(defun parser-generator-ll-test--valid-grammar-k-eq-1-p ()
  "Test `parser-generator-ll--valid-grammar-k-eq-1-p'."
  (message "Started tests for (parser-generator-ll--valid-grammar-k-eq-1-p)")

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (S A B)
     (a b)
     (
      (S (a A S) b B)
      (A a (b S A))
      (B a)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (should
   (equal
    nil
    (parser-generator-ll--valid-grammar-k-eq-1-p)))

  (parser-generator-set-eof-identifier '$)
  (parser-generator-set-e-identifier 'e)
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar
   '(
     (S A B)
     (a b c)
     (
      (S (a A S) b B)
      (A a (b S A))
      (B c)
      )
     S
     )
   )
  (parser-generator-process-grammar)
  (should
   (equal
    t
    (parser-generator-ll--valid-grammar-k-eq-1-p)))

  (message "Passed tests for (parser-generator-ll--valid-grammar-k-eq-1-p)"))


(defun parser-generator-ll-test ()
  "Run test."

  ;; Helpers
  (parser-generator-ll-test--generate-goto-table)

  ;; k > 1
  (parser-generator-ll-test--generate-action-table-k-gt-1)
  (parser-generator-ll-test--valid-grammar-k-gt-1-p)

  ;; k = 1
  (parser-generator-ll-test--generate-action-table-k-eq-1)
  (parser-generator-ll-test--valid-grammar-k-eq-1-p)

  ;; Main stuff
  (parser-generator-ll-test-generate-table)
  (parser-generator-ll-test-parse)
  (parser-generator-ll-test-translate))


(provide 'parser-generator-ll-test)

;;; parser-generator-ll-test.el ends here
