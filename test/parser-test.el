;;; parser-test.el --- Tests for parser -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'parser)
(require 'ert)

(defun parser-test--sort-list ()
  "Test `parser--sort-list'."
  (message "Starting tests for (parser-test--sort-list)")

  (should
   (equal
    '((a b c) (b c d) (c e f))
    (sort '((a b c) (c e f) (b c d)) 'parser--sort-list)))

  (should
   (equal
    '((a b c) (a c c) (c e f))
    (sort '((a c c) (a b c) (c e f)) 'parser--sort-list)))

  (should
   (equal
    '((a b) (a c c) (c e f g h))
    (sort '((a c c) (a b) (c e f g h)) 'parser--sort-list)))

  (should
   (equal
    '((a) (b) (c))
    (sort '((a) (c) (b)) 'parser--sort-list)))

  (message "Passed  tests for (parser--distinct)"))

(defun parser-test--distinct ()
  "Test `parser--distinct'."
  (message "Starting tests for (parser--distinct)")

  (should
   (equal
    '(a b c)
    (parser--distinct '(a a b c))))

  (should
   (equal
    '("aa" "b" "cc" "c" "a")
    (parser--distinct '("aa" "b" "cc" "c" "b" "a" "aa"))))
  (message "Passed tests for (parser--distinct)"))

(defun parser-test--follow ()
  "Test `parser--follow'."
  (message "Starting tests for (parser--follow)")

  (parser--set-grammar '((S A) (b) ((S A) (A b)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((e))
    (parser--follow 'A)))
  (message "Passed follow 1 with intermediate grammar")

  (parser--set-grammar '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((a))
    (parser--follow 'A)))
  (message "Passed follow 2 with intermediate grammar")

  (parser--set-grammar '((S A B) (a c d f) ((S (A a)) (A (B c d)) (B (c f) d)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((c d))
    (parser--follow 'B)))
  (message "Passed follow 3 with intermediate grammar")

  (message "Passed tests for (parser--follow)"))

(defun parser-test--first ()
  "Test `parser--first'."
  (message "Starting tests for (parser--first)")

  (parser--set-grammar '((S) (a) ((S a)) S))
  (parser--set-look-ahead-number 1)
  (should
   (equal
    '((a))
    (parser--first 'S)))
  (message "Passed first 1 with rudimentary grammar")

  (parser--set-grammar '((S) (a) ((S a)) S))
  (parser--set-look-ahead-number 1)
  (should
   (equal
    '((a))
    (parser--first '(S a))))
  (message "Passed first 1b with rudimentary grammar")

  (parser--set-grammar '((S) (a) ((S a)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((a a))
    (parser--first '(S a))))
  (message "Passed first 1c with rudimentary grammar")

  (parser--set-grammar '((S) (a) ((S a)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((a))
    (parser--first '(a))))
  (message "Passed first 1d with rudimentary grammar")

  (parser--set-grammar '((S) ("a" "b" "c") ((S ("a" "b" "c"))) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '(("a" "b"))
    (parser--first 'S)))
  (message "Passed first 2 with rudimentary grammar")

  (parser--set-grammar '((S) ("a" b "c") ((S ("a" b "c"))) S))
  (parser--set-look-ahead-number 3)
  (should
   (equal
    '(("a" b "c"))
    (parser--first 'S)))
  (message "Passed first 3 with rudimentary grammar")

  (parser--set-grammar '((S A) (b) ((S A) (A b)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((b))
    (parser--first 'S)))
  (message "Passed first 1 with intermediate grammar")

  (parser--set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '(("b" "a"))
    (parser--first 'S)))
  (message "Passed first 2 with intermediate grammar")

  (parser--set-grammar '((S A) ("a" "b" "c" "d") ((S A) (A ("b" "a" "c" "d"))) S))
  (parser--set-look-ahead-number 3)
  (should
   (equal
    '(("b" "a" "c"))
    (parser--first 'S)))
  (message "Passed first 3 with intermediate grammar")

  (parser--set-grammar '((S A B) ("c" "d") ((S A) (A B) (B "c" "d")) S))
  (parser--set-look-ahead-number 1)
  (should
   (equal
    '(("c") ("d"))
    (parser--first 'S)))
  (message "Passed first 1 with semi-complex grammar")

  (parser--set-grammar '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((c f) (d a))
    (parser--first 'S)))
  (message "Passed first 2 with semi-complex grammar")

  (parser--set-grammar '((S A B) ("a" "c" "d" "m") ((S A) (A (B "a" "m")) (B "c" "d")) S))
  (parser--set-look-ahead-number 3)
  (should
   (equal
    '(("c" "a" "m") ("d" "a" "m"))
    (parser--first 'S)))
  (message "Passed first 3 with semi-complex grammar")

  (parser--set-grammar '((S A B C) (a b c) ((S A B) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser--set-look-ahead-number 1)
  (should
   (equal
    '((a) (b) (c) (e))
    (parser--first 'S)))
  (message "Passed first 1 with complex grammar")

  ;; Example 5.28 p 382
  (parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((a b) (a c) (a) (b a) (b) (c a) (c) (c b) (e))
    (parser--first 'S)))
  (message "Passed first 2 with complex grammar")

  (parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser--set-look-ahead-number 3)
  (should
   (equal
    '((a) (a b) (a c) (a c b) (b a) (b a b) (b a c) (b) (c a) (c a b) (c a c) (c b) (c) (c b a) (e))
    (parser--first 'S)))
  (message "Passed first 3 with complex grammar")

  (message "Passed tests for (parser--first)"))

;; Example 5.28 page 402
(defun parser-test--e-free-first ()
  "Test `parser--e-free-first'."
  (message "Starting tests for (parser--e-free-first)")

  ;; Example 5.28 p 402
  (parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser--set-look-ahead-number 2)
  (should
   (equal
    '((c a) (c b))
    (parser--e-free-first 'S)))
  (message "Passed empty-free-first 2 with complex grammar")

  (message "Passed tests for (parser--empty-free-first)"))

(defun parser-test--lr-items ()
  "Test `parser--lr-items'."
  (message "Starting tests for (parser--lr-items)")

  ;; Example 5.29 p 387
  (parser--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser--set-look-ahead-number 1)
  (message "Loaded grammar")
  (should
   (equal
    '((Sp (S) nil (e))
      (S (S) (a S b) (e))
      (S (S) (a S b) (a)))
    (parser--lr-items 'S)))

  (message "Passed tests for (parser--lr-items)"))

(defun parser-test--valid-grammar-p ()
  "Test function `parser--valid-grammar-p'."
  (message "Starting tests for (parser--valid-grammar-p)")

  (should (equal
           t
           (parser--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a")) A))))

  (should (equal
           nil
           (parser--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a")) (A)))))

  (should (equal
           nil
           (parser--valid-grammar-p '((A B C) (("a" "b") "c") ((A "a")) A))))

  (should (equal
           nil
           (parser--valid-grammar-p '(((A B) C) ("a" "b" "c") ((A "a")) A))))

  (should (equal
           nil
           (parser--valid-grammar-p '(((A B) C) ("a" "b" "c") ((A)) A))))

  (should (equal
           nil
           (parser--valid-grammar-p "A")))

  (should (equal
           nil
           (parser--valid-grammar-p '(A B C))))

  (should (equal
           nil
           (parser--valid-grammar-p '((A B)))))

  (should (equal
           nil
           (parser--valid-grammar-p '((A B C) (a (b c) "c") (A ("a" "b") (a b)) (B b) (C "c")))))

  (message "Passed tests for (parser--valid-grammar-p)"))

(defun parser-test--valid-look-ahead-number-p ()
  "Test function `parser--valid-look-ahead-number-p'."
  (message "Starting tests for (parser--valid-look-ahead-number-p)")

  (should (equal
           nil
           (parser--valid-look-ahead-number-p 'A)))

  (should (equal
           nil
           (parser--valid-look-ahead-number-p "A")))

  (should (equal
           nil
           (parser--valid-look-ahead-number-p -2)))

  (should (equal
           nil
           (parser--valid-look-ahead-number-p 3.3)))

  (should (equal
           t
           (parser--valid-look-ahead-number-p 2)))

  (should (equal
           t
           (parser--valid-look-ahead-number-p 1)))

  (message "Passed tests for (parser--valid-look-ahead-number-p)"))

(defun parser-test--valid-sentential-form-p ()
  "Test `parser--valid-sentential-form-p'."
  (message "Starting tests  for (parser--valid-sentential-form-p)")

  (message "Passed tests for (parser--valid-sentential-form-p)"))

(defun parser-test--valid-production-p ()
  "Test `parser--valid-production-p'."
  (message "Starting tests  for (parser--valid-production-p)")

  (should (equal
           t
           (parser--valid-production-p '(A a))))

  (should (equal
           nil
           (parser--valid-production-p "A")))

  (should (equal
           nil
           (parser--valid-production-p '((A a)))))

  (message "Passed tests  for (parser--valid-production-p)"))

(defun parser-test--get-grammar-rhs ()
  "Test `parser--get-grammar-rhs'."
  (message "Started tests  for (parser--get-grammar-rhs)")

  (parser--set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (should (equal
           '((A))
           (parser--get-grammar-rhs 'S)))
  (should (equal
           '(("b" "a"))
           (parser--get-grammar-rhs 'A)))

  (parser--set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (should (equal
           '((A) (B))
           (parser--get-grammar-rhs 'S)))
  (should (equal
           '(("a") ("b" "a"))
           (parser--get-grammar-rhs 'A)))

  (message "Passed tests  for (parser--get-grammar-rhs)"))

(defun parser-test ()
  "Run test."
  ;; (setq debug-on-error t)

  ;; Helpers
  (parser-test--valid-look-ahead-number-p)
  (parser-test--valid-production-p)
  (parser-test--valid-grammar-p)
  (parser-test--valid-sentential-form-p)
  (parser-test--distinct)
  (parser-test--sort-list)
  (parser-test--get-grammar-rhs)

  ;; Algorithms
  (parser-test--first)
  (parser-test--e-free-first)
  (parser-test--follow)
  (parser-test--lr-items))

(provide 'parser-test)

;;; parser-test.el ends here
