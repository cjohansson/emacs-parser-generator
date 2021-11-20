;;; parser-generator-test.el --- Tests for Parser Generator -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator)
(require 'ert)

(defun parser-generator-test--valid-look-ahead-p ()
  "Test `parser-generator--valid-look-ahead-p'."
  (message "Starting tests for (parser-generator--valid-look-ahead-p)")

  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser-generator-process-grammar)

  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p '("a" "b"))))
  (should
   (equal
    t
    (parser-generator--valid-look-ahead-p "a")))
  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p "A")))
  (should
   (equal
    t
    (parser-generator--valid-look-ahead-p "b")))
  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p "c")))
  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p "d")))
  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p 'f)))
  (should
   (equal
    t
    (parser-generator--valid-look-ahead-p '$)))

  (message "Passed with look-ahead number is 1")

  (parser-generator-set-look-ahead-number 2)
  (parser-generator-set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser-generator-process-grammar)
  (should
   (equal
    t
    (parser-generator--valid-look-ahead-p '("a" "a"))))
  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p "a")))
  (should
   (equal
    t
    (parser-generator--valid-look-ahead-p '("b" "b"))))
  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p '("a" "c"))))
  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p '("a" "d"))))
  (should
   (equal
    nil
    (parser-generator--valid-look-ahead-p '(f $))))
  (should
   (equal
    t
    (parser-generator--valid-look-ahead-p '($ $))))

  (message "Passed with look-ahead number is 2")

  (message "Passed tests for (parser-generator--valid-look-ahead-p)"))

(defun parser-generator-test--get-grammar-look-aheads ()
  "Test `parser-generator--get-look-aheads'."
  (message "Starting tests for (parser-generator--get-grammar-look-aheads)")

  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser-generator-process-grammar)

  (should
   (equal
    '(($) ("a") ("b"))
    (parser-generator--get-grammar-look-aheads)))
  (message "Passed ((a) (b) ($))")

  (parser-generator-set-look-ahead-number 2)

  (should
   (equal
    '(($ $) ("a" $) ("a" "a") ("a" "b") ("b" $) ("b" "a") ("b" "b"))
    (parser-generator--get-grammar-look-aheads)))

  (message "Passed tests for (parser-generator--get-grammar-look-aheads)"))

(defun parser-generator-test--sort-list ()
  "Test `parser-generator--sort-list'."
  (message "Starting tests for (parser-generator-test--sort-list)")

  (should
   (equal
    '((a b c) (b c d) (c e f))
    (sort '((a b c) (c e f) (b c d)) 'parser-generator--sort-list)))

  (should
   (equal
    '((a b c) (a c c) (c e f))
    (sort '((a c c) (a b c) (c e f)) 'parser-generator--sort-list)))

  (should
   (equal
    '((a b) (a c c) (c e f g h))
    (sort '((a c c) (a b) (c e f g h)) 'parser-generator--sort-list)))

  (should
   (equal
    '((a) (b) (c))
    (sort '((a) (c) (b)) 'parser-generator--sort-list)))

  (message "Passed  tests for (parser-generator--distinct)"))

(defun parser-generator-test--distinct ()
  "Test `parser-generator--distinct'."
  (message "Starting tests for (parser-generator--distinct)")

  (should
   (equal
    '(a b c)
    (parser-generator--distinct '(a a b c))))

  (should
   (equal
    '("aa" "b" "cc" "c" "a")
    (parser-generator--distinct '("aa" "b" "cc" "c" "b" "a" "aa"))))
  (message "Passed tests for (parser-generator--distinct)"))

(defun parser-generator-test--follow ()
  "Test `parser-generator--follow'."
  (message "Starting tests for (parser-generator--follow)")

  (parser-generator-set-grammar '((S A) (b) ((S A) (A b)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)

  (should
   (equal
    '((e))
    (parser-generator--follow 'A)))
  (message "Passed follow 1 with intermediate grammar")

  (parser-generator-set-grammar '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)

  (should
   (equal
    '((a e))
    (parser-generator--follow 'A)))
  (message "Passed follow 2 with intermediate grammar")

  (parser-generator-set-grammar '((S A B) (a c d f) ((S (A a)) (A (B c d)) (B (c f) d)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)

  (should
   (equal
    '((c d))
    (parser-generator--follow 'B)))
  (message "Passed follow 3 with intermediate grammar")

  (message "Passed tests for (parser-generator--follow)"))

(defun parser-generator-test--first ()
  "Test `parser-generator--first'."
  (message "Starting tests for (parser-generator--first)")

  (parser-generator-set-grammar '((S) (a) ((S a)) S))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a))
    (parser-generator--first 'S)))
  (message "Passed first 1 with rudimentary grammar")

  (parser-generator-set-grammar '((S) (a) ((S a)) S))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a))
    (parser-generator--first '(S a))))
  (message "Passed first 1b with rudimentary grammar")

  (parser-generator-set-grammar '((S) (a) ((S a)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a a))
    (parser-generator--first '(S a))))
  (message "Passed first 1c with rudimentary grammar")

  (parser-generator-set-grammar '((S) (a) ((S a)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a e))
    (parser-generator--first '(a))))
  (message "Passed first 1d with rudimentary grammar")

  (parser-generator-set-grammar '((S) ("a" "b" "c") ((S ("a" "b" "c"))) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '(("a" "b"))
    (parser-generator--first 'S)))
  (message "Passed first 2 with rudimentary grammar")

  (parser-generator-set-grammar '((S) ("a" b "c") ((S ("a" b "c"))) S))
  (parser-generator-set-look-ahead-number 3)
  (parser-generator-process-grammar)
  (should
   (equal
    '(("a" b "c"))
    (parser-generator--first 'S)))
  (message "Passed first 3 with rudimentary grammar")

  (parser-generator-set-grammar '((S A) (b) ((S A) (A b)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((b e))
    (parser-generator--first 'S)))
  (message "Passed first 1 with intermediate grammar")

  (parser-generator-set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '(("b" "a"))
    (parser-generator--first 'S)))
  (message "Passed first 2 with intermediate grammar")

  (parser-generator-set-grammar '((S A) ("a" "b" "c" "d") ((S A) (A ("b" "a" "c" "d"))) S))
  (parser-generator-set-look-ahead-number 3)
  (parser-generator-process-grammar)
  (should
   (equal
    '(("b" "a" "c"))
    (parser-generator--first 'S)))
  (message "Passed first 3 with intermediate grammar")

  (parser-generator-set-grammar '((S A B) ("c" "d") ((S A) (A B) (B "c" "d")) S))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    '(("c") ("d"))
    (parser-generator--first 'S)))
  (message "Passed first 1 with semi-complex grammar")

  (parser-generator-set-grammar '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((c f) (d a))
    (parser-generator--first 'S)))
  (message "Passed first 2 with semi-complex grammar")

  (parser-generator-set-grammar '((S A B) ("a" "c" "d" "m") ((S A) (A (B "a" "m")) (B "c" "d")) S))
  (parser-generator-set-look-ahead-number 3)
  (parser-generator-process-grammar)
  (should
   (equal
    '(("c" "a" "m") ("d" "a" "m"))
    (parser-generator--first 'S)))
  (message "Passed first 3 with semi-complex grammar")

  (parser-generator-set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a) (b) (c) (e))
    (parser-generator--first 'S)))
  (message "Passed first 1 with complex grammar")

  ;; Example 5.28 p 382
  (parser-generator-set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a b) (a c) (a e) (b a) (b e) (c a) (c b) (c e) (e e))
    (parser-generator--first 'S)))
  (message "Passed first 2 with complex grammar")

  (parser-generator-set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser-generator-set-look-ahead-number 3)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a b e) (a c b) (a c e) (a e e) (b a b) (b a c) (b a e) (b e e) (c a b) (c a c) (c a e) (c b a) (c b e) (c e e) (e e e))
    (parser-generator--first 'S)))
  (message "Passed first 3 with complex grammar")

  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b) e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a) (e))
    (parser-generator--first 'S)))
  (message "Passed first 4 with complex grammar with starting e-identifier variant 1")

  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a) (e))
    (parser-generator--first 'S)))
  (message "Passed first 5 with complex grammar with starting e-identifier variant 2")
  
  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b) e)) Sp))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a a) (a b) (a e) (e e))
    (parser-generator--first 'S)))
  (message "Passed first 6 with complex grammar with starting e-identifier variant 1")

  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a a) (a b) (a e) (e e))
    (parser-generator--first 'S)))
  (message "Passed first 7 with complex grammar with starting e-identifier variant 2")

  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 3)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a a b) (a a e) (a b a) (a b e) (a e e) (e e e))
    (parser-generator--first 'S)))
  (message "Passed first 8 with complex grammar with starting e-identifier variant 2")

  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 4)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a a b b) (a a e e) (a b a a) (a b a b) (a b a e) (a b e e) (a e e e) (e e e e))
    (parser-generator--first 'S)))
  (message "Passed first 9 with complex grammar with starting e-identifier variant 2")

  (parser-generator-set-grammar
   '(
     (Sp S A B)
     (a b c)
     (
      (Sp S)
      (S A B)
      (A (a b A) (B c))
      (B S c)
      )
     Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a) (c))
    (parser-generator--first 'Sp)))
  (message "Passed first 10 with complex grammar that contains cyclic loops")

  (should
   (equal
    '((a) (c) (e))
    (parser-generator--first '(e Sp))))
  (message "Passed first 11 with multiple items with e-identifiers")

  (parser-generator-set-grammar
   '(
     (Sp S A B C)
     (a b c)
     (
      (Sp S)
      (S (A C B))
      (A a)
      (B b)
      (C e)
      )
     Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    (parser-generator--first '(C B))
    '((b) (e))))
  (message "Passed first 12 with multiple non-terminals and e-identifiers")

  (message "Passed tests for (parser-generator--first)"))

(defun parser-generator-test--e-free-first ()
  "Test `parser-generator--e-free-first'."
  (message "Starting tests for (parser-generator--e-free-first)")

  ;; Example 5.28 p 382
  (parser-generator-set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((c a) (c b))
    (parser-generator--e-free-first 'S)))
  (message "Passed empty-free-first 2 with complex grammar")

  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (should
   (equal
    '((c))
    (parser-generator--e-free-first '(S b a))))
  (message "Passed empty-free-first 1 with complex grammar")

  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
   (should
   (equal
    nil
    (parser-generator--e-free-first '(S b a))))
  (message "Passed empty-free-first 1 with complex grammar 2")

  (parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 2)
  (parser-generator-process-grammar)
  (should
   (equal
    '((a b))
    (parser-generator--e-free-first '(a b))))
  (message "Passed empty-free-first 2 with terminals")
  (should
   (equal
    '((a e))
    (parser-generator--e-free-first '(a e))))
  (message "Passed empty-free-first 2 with trailing e-identifier 1")
  (should
   (equal
    nil
    (parser-generator--e-free-first 'S)))
  (should
   (equal
    nil
    (parser-generator--e-free-first '(e a))))
  (should
   (equal
    '((a a) (a e))
    (parser-generator--e-free-first '(a S))))
  (message "Passed empty-free-first 2 with trailing e-identifier 2")
  (should
   (equal
    '((a a) (a e))
    (parser-generator--e-free-first '(a S b))))
  (message "Passed empty-free-first 2 with trailing e-identifier 1")

  (message "Passed tests for (parser-generator--empty-free-first)"))

(defun parser-generator-test--get-grammar-context-sensitive-attributes-by-production-number ()
  "Test `parser-generator--get-grammar-context-sensitive-attributes-by-production-number'."
  (message "Starting tests for (parser-generator--get-grammar-context-sensitive-attributes-by-production-number)")
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (parser-generator-set-grammar '((A B C) ("a" "b" "c") ((A ("a" %prec 1) ("b" "c" %prec D))) A))
  (parser-generator-process-grammar)

  (should
   (equal
    '(%prec 1)
    (parser-generator--get-grammar-context-sensitive-attributes-by-production-number 0)))
  (should
   (equal
    '((A) ("a"))
    (parser-generator--get-grammar-production-by-number 0)))
  (should
   (equal
    '(%prec D)
    (parser-generator--get-grammar-context-sensitive-attributes-by-production-number 1)))
  (should
   (equal
    '((A) ("b" "c"))
    (parser-generator--get-grammar-production-by-number 1)))
  (should
   (equal
    nil
    (parser-generator--get-grammar-context-sensitive-attributes-by-production-number 2)))
  (should
   (equal
    nil
    (parser-generator--get-grammar-production-by-number 2)))

  (message "Passed tests for (parser-generator--get-grammar-context-sensitive-attributes-by-production-number)"))

(defun parser-generator-test--valid-grammar-p ()
  "Test function `parser-generator--valid-grammar-p'."
  (message "Starting tests for (parser-generator--valid-grammar-p)")
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (parser-generator-process-grammar)

  (should
   (equal
    t
    (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a")) A))))
  (message "Passed valid grammar 1")

  (should
   (equal
    t
    (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A ("a" %prec 1))) A))))
  (message "Passed valid grammar 2 with context-sensitive attribute")

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a")) (A)))))

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p '((A B C) (("a" "b") "c") ((A "a")) A))))

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p '(((A B) C) ("a" "b" "c") ((A "a")) A))))

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p '(((A B) C) ("a" "b" "c") ((A)) A))))

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p "A")))

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p '(A B C))))

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p '((A B)))))

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p '((A B C) (a (b c) "c") (A ("a" "b") (a b)) (B b) (C "c")))))

  (should
   (equal
    t
    (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a" (lambda(a) (message "Was here: %s" a)))) A))))

  (should
   (equal
    nil
    (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a" (lambda(a) (message "Was here: %s" a)) "b")) A))))

  (should
   (equal
    t
    (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A ("a" (lambda(a) (message "Was here: %s" a))))) A))))

  (should
   (equal
    t
    (parser-generator--valid-grammar-p
     '((A B C) ("a" "b" "c") ((A ("a" (lambda(args) (message "a: %s" args))) ("b" (lambda(args) (message "b: %s" args))) ("c" (lambda(args) (message "c: %s" args))))) A))))

  (message "Passed tests for (parser-generator--valid-grammar-p)"))

(defun parser-generator-test--valid-look-ahead-number-p ()
  "Test function `parser-generator--valid-look-ahead-number-p'."
  (message "Starting tests for (parser-generator--valid-look-ahead-number-p)")

  (should (equal
           nil
           (parser-generator--valid-look-ahead-number-p 'A)))

  (should (equal
           nil
           (parser-generator--valid-look-ahead-number-p "A")))

  (should (equal
           nil
           (parser-generator--valid-look-ahead-number-p -2)))

  (should (equal
           nil
           (parser-generator--valid-look-ahead-number-p 3.3)))

  (should (equal
           t
           (parser-generator--valid-look-ahead-number-p 2)))

  (should (equal
           t
           (parser-generator--valid-look-ahead-number-p 1)))

  (message "Passed tests for (parser-generator--valid-look-ahead-number-p)"))

(defun parser-generator-test--valid-sentential-form-p ()
  "Test `parser-generator--valid-sentential-form-p'."
  (message "Starting tests for (parser-generator--valid-sentential-form-p)")

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (A ("b" "a")) (B ("b" (lambda(b) (message "Was here: %s" b))))) S))
  (parser-generator-process-grammar)

  (should
   (equal
    nil
    (parser-generator--valid-sentential-form-p
     '(B "b" (lambda(b) (message "Was here: %s" b))))))

  (should
   (equal
    t
    (parser-generator--valid-sentential-form-p
     '(B "b"))))

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (A (b "a")) (B ("b" (lambda(b) (message "Was here: %s" b))))) S))
  (should-error
   (parser-generator-process-grammar))

  (message "Passed tests for (parser-generator--valid-sentential-form-p)"))

(defun parser-generator-test--valid-production-p ()
  "Test `parser-generator--valid-production-p'."
  (message "Starting tests for (parser-generator--valid-production-p)")

  (should (equal
           t
           (parser-generator--valid-production-p '(A a))))

  (should (equal
           t
           (parser-generator--valid-production-p '(A (a)))))

  (should (equal
           nil
           (parser-generator--valid-production-p "A")))

  (should (equal
           nil
           (parser-generator--valid-production-p '((A a)))))

  (should (equal
           t
           (parser-generator--valid-production-p '(A a (lambda(a) (message "Here 1 %s"))))))

  (should (equal
           t
           (parser-generator--valid-production-p '(A (a (lambda(a) (message "Here 2 %s")))))))

  (should (equal
           t
           (parser-generator--valid-production-p '(A (a (lambda(a) (message "Here 3 %s"))) b))))

  (message "Passed tests for (parser-generator--valid-production-p)"))

(defun parser-generator-test--get-grammar-rhs ()
  "Test `parser-generator--get-grammar-rhs'."
  (message "Started tests for (parser-generator--get-grammar-rhs)")

  (parser-generator-set-grammar
   '((S A B) ("a" "b") ((S A) (A ("b" "a")) (B ("b" (lambda(b) (message "Was here: %s" b))))) S))
  (parser-generator-process-grammar)
  (should (equal
           '((A))
           (parser-generator--get-grammar-rhs 'S)))
  (should (equal
           '(("b" "a"))
           (parser-generator--get-grammar-rhs 'A)))
  (should (equal
           '(("b"))
           (parser-generator--get-grammar-rhs 'B)))
  (message "Passed first")

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (parser-generator-process-grammar)
  (should (equal
           '((A) (B))
           (parser-generator--get-grammar-rhs 'S)))
  (should (equal
           '(("a") ("b" "a"))
           (parser-generator--get-grammar-rhs 'A)))

  (parser-generator-set-grammar
   '((Sp S R T) ("a" "b" "c") ((Sp S) (S (R S) (R)) (R ("a" "b" T (lambda(args) (list "begin" (nth 2 args) "end")))) (T ("a" T (lambda() "test")) ("c") (e))) Sp))
  (parser-generator-process-grammar)
  (should
   (equal
    '(("a" T) ("c") (e))
    (parser-generator--get-grammar-rhs 'T)))

  (parser-generator-process-grammar)

  (message "Passed tests for (parser-generator--get-grammar-rhs)"))

(defun parser-generator-test--valid-non-terminal-p ()
  "Test `parser-generator--valid-non-terminal-p'."
  (message "Starting tests for (parser-generator--valid-non-terminal-p)")

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (parser-generator-process-grammar)

  (should
   (equal
    t
    (parser-generator--valid-non-terminal-p 'S)))
  (should
   (equal
    nil
    (parser-generator--valid-non-terminal-p '(S (%proc 1)))))
  (should
   (equal
    nil
    (parser-generator--valid-non-terminal-p '(S (%prec 1)))))
  (should
   (equal
    t
    (parser-generator--valid-non-terminal-p 'A)))
  (should
   (equal
    t
    (parser-generator--valid-non-terminal-p 'B)))
  (should
   (equal
    nil
    (parser-generator--valid-non-terminal-p 'C)))
  (should
   (equal
    nil
    (parser-generator--valid-non-terminal-p "a")))

  (message "Passed tests for (parser-generator--valid-non-terminal-p)"))

(defun parser-generator-test--valid-context-sensitive-attribute-p ()
  "Test `parser-generator--valid-context-sensitive-attribute-p'."
  (message "Starting tests for (parser-generator--valid-context-sensitive-attribute-p)")

  (parser-generator-set-grammar
   '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (setq
   parser-generator--context-sensitive-attributes
   '(%abc depth length))
  (parser-generator-process-grammar)

  (should
   (equal
    t
    (parser-generator--valid-context-sensitive-attribute-p
     'depth)))

  (should
   (equal
    t
    (parser-generator--valid-context-sensitive-attribute-p
     '%abc)))

  (should
   (equal
    nil
    (parser-generator--valid-context-sensitive-attribute-p
     '%prec)))

  (message "Passed tests for (parser-generator--valid-context-sensitive-attribute-p)"))

(defun parser-generator-test--valid-context-sensitive-attributes-p ()
  "Test `parser-generator--valid-context-sensitive-attributes-p'."
  (message "Starting tests for (parser-generator--valid-context-sensitive-attributes-p)")

  (parser-generator-set-grammar
   '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))

  (setq
   parser-generator--context-sensitive-attributes
   '(%abc depth length))
  (parser-generator-process-grammar)

  (should
   (equal
    t
    (parser-generator--valid-context-sensitive-attributes-p
     '(%abc 1 depth 0 length 3))))
  (should
   (equal
    nil
    (parser-generator--valid-context-sensitive-attributes-p
     '(%prec 0))))

  (message "Passed tests for (parser-generator--valid-context-sensitive-attributes-p)"))

(defun parser-generator-test--valid-terminal-p ()
  "Test `parser-generator--valid-terminal-p'."
  (message "Starting tests for (parser-generator--valid-terminal-p)")

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (parser-generator-process-grammar)

  (should
   (equal
    t
    (parser-generator--valid-terminal-p "a")))
  (should
   (equal
    nil
    (parser-generator--valid-terminal-p '("a" (%prec 3)))))
  (should
   (equal
    nil
    (parser-generator--valid-terminal-p '("a" (%proc 3)))))
  (should
   (equal
    t
    (parser-generator--valid-terminal-p "b")))
  (should
   (equal
    t
    (parser-generator--valid-terminal-p "a")))
  (should
   (equal
    nil
    (parser-generator--valid-terminal-p 'S)))
  (should
   (equal
    nil
    (parser-generator--valid-terminal-p 'A)))

  (message "Passed tests for (parser-generator--valid-terminal-p)"))

(defun parser-generator-test--merge-max-terminals ()
  "Test `parser-generator--merge-max-terminals'."
  (message "Starting tests for (parser-generator--merge-max-terminals)")

  (should
   (equal
    '(a b e)
    (parser-generator--merge-max-terminals
     '(a)
     '(b e)
     3)))

  (should
   (equal
    '(a e)
    (parser-generator--merge-max-terminals
     '(a e)
     '(b e)
     3)))

  (message "Passed tests for (parser-generator--merge-max-terminals)"))

(defun parser-generator-test--get-list-permutations ()
  "Test `parser-generator--get-list-permutations'."
  (message "Starting tests for (parser-generator--get-list-permutations)")

  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (parser-generator-process-grammar)

  (should
   (equal
    '((A) (B) (S) ("a") ("b"))
    (parser-generator--get-list-permutations
     (append
      (parser-generator--get-grammar-terminals)
      (parser-generator--get-grammar-non-terminals))
     parser-generator--look-ahead-number)))
  (message "Passed test for list permutations of length 1")

  (parser-generator-set-look-ahead-number 2)
  (should
   (equal
    '((A A) (A B) (A S) (A "a") (A "b")
      (B A) (B B) (B S) (B "a") (B "b")
      (S A) (S B) (S S) (S "a") (S "b")
      ("a" A) ("a" B) ("a" S) ("a" "a") ("a" "b")
      ("b" A) ("b" B) ("b" S) ("b" "a") ("b" "b"))
    (parser-generator--get-list-permutations
     (append
      (parser-generator--get-grammar-terminals)
      (parser-generator--get-grammar-non-terminals))
     parser-generator--look-ahead-number)))
  (message "Passed test for list permutations of length 2")

  (parser-generator-set-look-ahead-number 3)
  (should
   (equal
    '((A A A) (A A B) (A A S) (A A "a") (A A "b") (A B A) (A B B) (A B S) (A B "a") (A B "b") (A S A) (A S B) (A S S) (A S "a") (A S "b") (A "a" A) (A "a" B) (A "a" S) (A "a" "a") (A "a" "b") (A "b" A) (A "b" B) (A "b" S) (A "b" "a") (A "b" "b") (B A A) (B A B) (B A S) (B A "a") (B A "b") (B B A) (B B B) (B B S) (B B "a") (B B "b") (B S A) (B S B) (B S S) (B S "a") (B S "b") (B "a" A) (B "a" B) (B "a" S) (B "a" "a") (B "a" "b") (B "b" A) (B "b" B) (B "b" S) (B "b" "a") (B "b" "b") (S A A) (S A B) (S A S) (S A "a") (S A "b") (S B A) (S B B) (S B S) (S B "a") (S B "b") (S S A) (S S B) (S S S) (S S "a") (S S "b") (S "a" A) (S "a" B) (S "a" S) (S "a" "a") (S "a" "b") (S "b" A) (S "b" B) (S "b" S) (S "b" "a") (S "b" "b") ("a" A A) ("a" A B) ("a" A S) ("a" A "a") ("a" A "b") ("a" B A) ("a" B B) ("a" B S) ("a" B "a") ("a" B "b") ("a" S A) ("a" S B) ("a" S S) ("a" S "a") ("a" S "b") ("a" "a" A) ("a" "a" B) ("a" "a" S) ("a" "a" "a") ("a" "a" "b") ("a" "b" A) ("a" "b" B) ("a" "b" S) ("a" "b" "a") ("a" "b" "b") ("b" A A) ("b" A B) ("b" A S) ("b" A "a") ("b" A "b") ("b" B A) ("b" B B) ("b" B S) ("b" B "a") ("b" B "b") ("b" S A) ("b" S B) ("b" S S) ("b" S "a") ("b" S "b") ("b" "a" A) ("b" "a" B) ("b" "a" S) ("b" "a" "a") ("b" "a" "b") ("b" "b" A) ("b" "b" B) ("b" "b" S) ("b" "b" "a") ("b" "b" "b"))
    (parser-generator--get-list-permutations
     (append
      (parser-generator--get-grammar-terminals)
      (parser-generator--get-grammar-non-terminals))
     parser-generator--look-ahead-number)))
  (message "Passed test for list permutations of length 3")

  (message "Passed tests for (parser-generator--get-list-permutations)"))

(defun parser-generator-test--generate-list-of-symbol ()
  "Test `parser-generator--generate-list-of-symbol'."
  (message "Starting tests for (parser-generator-test--generate-list-of-symbol)")

  (should
   (equal
    '(a a a)
    (parser-generator--generate-list-of-symbol 3 'a)))

  (should
   (equal
    '((a b) (a b))
    (parser-generator--generate-list-of-symbol 2 '(a b))))

  (message "Passed tests for (parser-generator-test--generate-list-of-symbol)"))

(defun parser-generator-test ()
  "Run test."
  ;; (setq debug-on-error t)

  ;; Helpers
  (parser-generator-test--distinct)
  (parser-generator-test--generate-list-of-symbol)
  (parser-generator-test--get-grammar-look-aheads)
  (parser-generator-test--get-grammar-rhs)
  (parser-generator-test--get-list-permutations)
  (parser-generator-test--merge-max-terminals)
  (parser-generator-test--sort-list)
  (parser-generator-test--valid-context-sensitive-attribute-p)
  (parser-generator-test--valid-context-sensitive-attributes-p)
  (parser-generator-test--valid-grammar-p)
  (parser-generator-test--get-grammar-context-sensitive-attributes-by-production-number)
  (parser-generator-test--valid-look-ahead-number-p)
  (parser-generator-test--valid-look-ahead-p)
  (parser-generator-test--valid-non-terminal-p)
  (parser-generator-test--valid-production-p)
  (parser-generator-test--valid-sentential-form-p)
  (parser-generator-test--valid-terminal-p)

  ;; Algorithms
  (parser-generator-test--first)
  (parser-generator-test--e-free-first)
  (parser-generator-test--follow))


(provide 'parser-generator-test)

;;; parser-generator-test.el ends here
