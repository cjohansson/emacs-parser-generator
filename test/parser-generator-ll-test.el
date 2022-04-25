;; parser-generator-ll-test.el --- Tests for LL(k) Parser Generator -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator-ll)
(require 'ert)

(defun parser-generator-ll-test--generate-tables ()
  "Test `parser-generator-ll--generate-tables'."
  (message "Started tests for (parser-generator-ll--generate-tables)")

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
  (let ((tables (parser-generator-ll--generate-tables)))
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
         ((S) ($)) ;; T0
         (
          ((a b) (a A a a) ((a a)))
          ((a a) (a A a a) ((a a)))
          ((b b) (b A b a) ((b a)))
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
          (parser-generator-ll--generate-tables)))
    (should
     (equal
      tables
      '(
        (
         ((A) (a a)) ;; T3
         (
          ((a b) (S a a) ((a a)))
          ((a a) (S a a) ((a a)))
          ((b a) (b) nil)
          )
         )
        (
         ((S) (a a)) ;; T2
         (
          ((a b) (a b A) ((a a)))
          ((a a) (e) nil)
          )
         )
        (
         ((A) ($)) ;; T1
         (
          ((a b) (S a a) ((a a)))
          ((a a) (S a a) ((a a)))
          ((b $) (b) nil)
          )
         )
        (
         ((S) ($)) ;; T0
         (
          (($ $) (e) nil)
          ((a b) (a b A) (($)))
          )
         )
        )
      ))
    )
  (message "Passed Example 5.17 p. 354")

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
          (parser-generator-ll--generate-tables)))
    (message "tables: %S" tables)
    (should
     (equal
      tables
      '(
        (
         ((A) (a))
         (
          ((a) (a) nil)
          ((b) (b S A) ((a) (b) (a)))
          )
         )
        (
         ((S) (a))
         (
          ((a) (a A S) ((a) (b) (a)))
          ((b) (b) nil)
          )
         )
        (
         ((S) (b))
         (
          ((a) (a A S) ((b) (b) (a)))
          ((b) (b) nil)
          )
         )
        (
         ((A) (b))
         (
          ((a) (a) nil)
          ((b) (b S A) ((b) (b) (a)))
          )
         )
        (
         ((S) ($))
         (
          ((a) (a A S) (($) (b) (a))) ;; WEIRD?
          ((b) (b) nil)
          )
         )
        )
      ))
    )
  ;; TODO Verify above
  (message "Passed Example 5.5 p. 340")

  ;; TODO Example 5.12 p. 346-347

  (message "Passed tests for (parser-generator-ll--generate-tables)"))

(defun parser-generator-ll-test--generate-parsing-table ()
  "Test `parser-generator-ll--generate-parsing-table'."
  (message "Started tests for (parser-generator-ll--generate-parsing-table)")

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
  (let ((parser-tables
         (parser-generator-ll--generate-parsing-table
          (parser-generator-ll--generate-tables))))
    ;; (message "parser-tables: %S" parser-tables)
    (should
     (equal
      '(
        (
         ((S) ($)) ;; T0
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
        (
         b
         (
          ((b b) pop)
          ((b a) pop)
          ((b $) pop)
          )
         )
        (
         a
         (
          ((a b) pop)
          ((a a) pop)
          ((a $) pop)
          )
         )
        (
         $
         (
          (($ $) accept)
          )
         )
        )
      parser-tables)))
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
  (let ((parser-tables
         (parser-generator-ll--generate-parsing-table
          (parser-generator-ll--generate-tables))))
    ;; (message "parser-tables: %S" parser-tables)
    (should
     (equal
      '(
        (
         ((S) ($)) ;; T0
         (
          ((a b) reduce (a b ((A) ($))) 1)
          (($ $) reduce (e) 0)
          )
         )
        (
         ((A) ($)) ;; T1
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
        (
         b
         (
          ((b b) pop)
          ((b a) pop)
          ((b $) pop)
          )
         )
        (
         a
         (
          ((a b) pop)
          ((a a) pop)
          ((a $) pop)
          )
         )
        (
         $
         (
          (($ $) accept)
          )
         )
        )
      parser-tables)))
  (message "Passed example 5.17 p. 356")

  (message "Passed tests for (parser-generator-ll--generate-parsing-table)"))

(defun parser-generator-ll-test--parse ()
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
  (parser-generator-ll-generate-parser-tables)
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
  (parser-generator-ll-generate-parser-tables)
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
  (parser-generator-ll-parse)
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
  (parser-generator-ll-generate-parser-tables)
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
  (parser-generator-ll-parse)
  (should
   (equal
    '(0 3 1 2 1) ;; Example is indexed from 1 so that is why they have '(1 4 2 3 2)
    (parser-generator-ll-parse)))
  (message "Passed example 5.5 p. 340")
  ;; TODO Make this pass

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
  (let ((tables (parser-generator-ll--generate-tables)))
    (message "tables: %S" tables)
    '(
      (
       ((E2) (")"))
       (
        ((")") (e) nil)
        (("+") ("+" T E2) ((")") ("+")))
        )
       )
      (
       ((E) (")"))
       (
        (("(") (T E2) ((")") ("+")))
        (("a") (T E2) ((")") ("+")))
        )
       )
      (
       ((F) ("*"))
       (
        (("(") ("(" E ")") ((")")))
        (("a") ("a") nil)
        )
       )
      (
       ((T2) ("+"))
       (
        (("*") ("*" F T2) (("+") ("*")))
        (("+") (e) nil)
        )
       )
      (
       ((T) ("+"))
       (
        (("(") (F T2) (("+") ("*")))
        (("a") (F T2) (("+") ("*")))
        )
       )
      (
       ((E2) ($))
       (
        (($) (e) nil)
        (("+") ("+" T E2) (($) ("+")))
        )
       )
      (
       ((E) ($))
       (
        (("(") (T E2) (($) ("+")))
        (("a") (T E2) (($) ("+")))
        )
       )
      ))
  (parser-generator-ll-generate-parser-tables)
  (message
   "parser-generator-ll--parsing-table: %S"
   (parser-generator--hash-to-list
    parser-generator-ll--parsing-table
    t))
  ;; Local-follow-should exceed k
  '(
    ("((E) ($))"
     (
      ("(\"a\")" (reduce (((T) ($)) ((E2) ("+"))) 0))
      ("(\"(\")" (reduce (((T) ($)) ((E2) ("+"))) 0))
      )
     )
    ("((E2) ($))"
     (
      ("(\"+\")" (reduce ("+" ((T) ($)) ((E2) ("+"))) 1))
      ("($)" (reduce (e) 2))
      )
     )
    ("((T) (\"+\"))"
     (
      ("(\"a\")" (reduce (((F) ("+")) ((T2) ("*"))) 3))
      ("(\"(\")" (reduce (((F) ("+")) ((T2) ("*"))) 3))
      )
     )
    ("((T2) (\"+\"))"
     (
      ("(\"+\")" (reduce (e) 5))
      ("(\"*\")" (reduce ("*" ((F) ("+")) ((T2) ("*"))) 4))
      )
     )
    ("((F) (\"*\"))"
     (
      ("(\"a\")" (reduce ("a") 7))
      ("(\"(\")" (reduce ("(" ((E) (")")) ")") 6))
      )
     )
    ("((E) (\")\"))"
     (
      ("(\"a\")" (reduce (((T) (")")) ((E2) ("+"))) 0))
      ("(\"(\")" (reduce (((T) (")")) ((E2) ("+"))) 0))
      )
     )
    ("((E2) (\")\"))"
     (
      ("(\"+\")" (reduce ("+" ((T) (")")) ((E2) ("+"))) 1))
      ("(\")\")" (reduce (e) 2))
      )
     )
    ("\"a\""(("(\"a\")" pop)))
    ("\"+\"" (("(\"+\")" pop)))
    ("\"*\"" (("(\"*\")" pop)))
    ("\")\"" (("(\")\")" pop)))
    ("\"(\"" (("(\"(\")" pop)))
    ("$" (("($)" accept)))
    )
  ;; (message "parser-generator-ll--parsing-table: %S" parser-generator-ll--parsing-table)
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
  (parser-generator-ll-parse)
  (should
   (equal
    '(0 3 6 0 3 7 5 2 5) ;; Example is 1-indexed '(1 4 7 1 4 8 5 8 6 3 6 3)
    (parser-generator-ll-parse)))
  (message "Passed example 5.12 p. 346-347")
  ;; TODO Make this pass

  (message "Passed tests for (parser-generator-ll-parse)"))

(defun parser-generator-ll-test--generate-parser-tables ()
  "Test `parser-generator-ll-generate-parser-tables'."
  (message "Started tests for (parser-generator-ll-generate-parser-tables)")

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
  (parser-generator-ll-generate-parser-tables)
  (should
   (equal
    '(
      ("((S) ($))"
       (
        ("(a b)" (reduce (a b ((A) ($))) 1))
        ("($ $)" (reduce (e) 0))
        )
       )
      ("((A) ($))"
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
      ("b"
       (
        ("(b b)" pop)
        ("(b a)" pop)
        ("(b $)" pop)
        )
       )
      ("a"
       (
        ("(a b)" pop)
        ("(a a)" pop)
        ("(a $)" pop)
        )
       )
      ("$"
       (
        ("($ $)" accept)
        )
       )
      )
    (parser-generator--hash-to-list
     parser-generator-ll--parsing-table
     t)))

  (message "Passed tests for (parser-generator-ll-generate-parser-tables)"))

(defun parser-generator-ll-test--valid-grammar-p ()
  "Test `parser-generator-ll--valid-grammar-p'."
  (message "Started tests for (parser-generator-ll--valid-grammar-p)")

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
    (parser-generator-ll--valid-grammar-p)
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
    (parser-generator-ll--valid-grammar-p)
    nil))
  (message "Passed second valid test")

  ;; TODO Example 5.19

  (message "Passed tests for (parser-generator-ll--valid-grammar-p)"))


(defun parser-generator-ll-test ()
  "Run test."
  ;; Helpers
  (parser-generator-ll-test--generate-tables)
  (parser-generator-ll-test--generate-parsing-table)
  (parser-generator-ll-test--valid-grammar-p)

  ;; Main stuff
  (parser-generator-ll-test--generate-parser-tables)
  (parser-generator-ll-test--parse))


(provide 'parser-generator-ll-test)

;;; parser-generator-ll-test.el ends here
