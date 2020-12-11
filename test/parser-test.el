;;; parser-test.el --- Tests for Parser -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'parser)
(require 'ert)

(defun parser-test--valid-look-ahead-p ()
  "Test `parser--valid-look-ahead-p'."
  (message "Starting tests for (parser--valid-look-ahead-p)")

  (parser--set-look-ahead-number 1)
  (parser--set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser--process-grammar)

  (should
   (equal
    t
    (parser--valid-look-ahead-p "a")))
  (should
   (equal
    t
    (parser--valid-look-ahead-p "b")))
  (should
   (equal
    nil
    (parser--valid-look-ahead-p "c")))
  (should
   (equal
    nil
    (parser--valid-look-ahead-p "d")))
  (should
   (equal
    t
    (parser--valid-look-ahead-p 'e)))

  (message "Passed tests for (parser--valid-look-ahead-p)"))

(defun parser-test--get-grammar-look-aheads ()
  "Test `parser--get-look-aheads'."
  (message "Starting tests for (parser--get-grammar-look-aheads)")

  (parser--set-look-ahead-number 1)
  (parser--set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser--process-grammar)

  (should
   (equal
    '(("a") ("b") (e))
    (parser--get-grammar-look-aheads)))
  (message "Passed ((a) (b) (e))")

  (parser--set-look-ahead-number 2)

  (should
   (equal
    '(("a" "a") ("a" "b") ("a" e) ("b" "a") ("b" "b") ("b" e))
    (parser--get-grammar-look-aheads)))

  (message "Passed tests for (parser--get-grammar-look-aheads)"))

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
  (parser--process-grammar)

  (should
   (equal
    '((e))
    (parser--follow 'A)))
  (message "Passed follow 1 with intermediate grammar")

  (parser--set-grammar '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

  (should
   (equal
    '((a))
    (parser--follow 'A)))
  (message "Passed follow 2 with intermediate grammar")

  (parser--set-grammar '((S A B) (a c d f) ((S (A a)) (A (B c d)) (B (c f) d)) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

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
  (parser--process-grammar)

  (should
   (equal
    '((a))
    (parser--first 'S)))
  (message "Passed first 1 with rudimentary grammar")

  (parser--set-grammar '((S) (a) ((S a)) S))
  (parser--set-look-ahead-number 1)
  (parser--process-grammar)

  (should
   (equal
    '((a))
    (parser--first '(S a))))
  (message "Passed first 1b with rudimentary grammar")

  (parser--set-grammar '((S) (a) ((S a)) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

  (should
   (equal
    '((a a))
    (parser--first '(S a))))
  (message "Passed first 1c with rudimentary grammar")

  (parser--set-grammar '((S) (a) ((S a)) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

  (should
   (equal
    '((a))
    (parser--first '(a))))
  (message "Passed first 1d with rudimentary grammar")

  (parser--set-grammar '((S) ("a" "b" "c") ((S ("a" "b" "c"))) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

  (should
   (equal
    '(("a" "b"))
    (parser--first 'S)))
  (message "Passed first 2 with rudimentary grammar")

  (parser--set-grammar '((S) ("a" b "c") ((S ("a" b "c"))) S))
  (parser--set-look-ahead-number 3)
  (parser--process-grammar)

  (should
   (equal
    '(("a" b "c"))
    (parser--first 'S)))
  (message "Passed first 3 with rudimentary grammar")

  (parser--set-grammar '((S A) (b) ((S A) (A b)) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

  (should
   (equal
    '((b))
    (parser--first 'S)))
  (message "Passed first 1 with intermediate grammar")

  (parser--set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

  (should
   (equal
    '(("b" "a"))
    (parser--first 'S)))
  (message "Passed first 2 with intermediate grammar")

  (parser--set-grammar '((S A) ("a" "b" "c" "d") ((S A) (A ("b" "a" "c" "d"))) S))
  (parser--set-look-ahead-number 3)
  (parser--process-grammar)

  (should
   (equal
    '(("b" "a" "c"))
    (parser--first 'S)))
  (message "Passed first 3 with intermediate grammar")

  (parser--set-grammar '((S A B) ("c" "d") ((S A) (A B) (B "c" "d")) S))
  (parser--set-look-ahead-number 1)
  (parser--process-grammar)

  (should
   (equal
    '(("c") ("d"))
    (parser--first 'S)))
  (message "Passed first 1 with semi-complex grammar")

  (parser--set-grammar '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

  (should
   (equal
    '((c f) (d a))
    (parser--first 'S)))
  (message "Passed first 2 with semi-complex grammar")

  (parser--set-grammar '((S A B) ("a" "c" "d" "m") ((S A) (A (B "a" "m")) (B "c" "d")) S))
  (parser--set-look-ahead-number 3)
  (parser--process-grammar)

  (should
   (equal
    '(("c" "a" "m") ("d" "a" "m"))
    (parser--first 'S)))
  (message "Passed first 3 with semi-complex grammar")

  (parser--set-grammar '((S A B C) (a b c) ((S A B) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser--set-look-ahead-number 1)
  (parser--process-grammar)

  (should
   (equal
    '((a) (b) (c) (e))
    (parser--first 'S)))
  (message "Passed first 1 with complex grammar")

  ;; Example 5.28 p 382
  (parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser--set-look-ahead-number 2)
  (parser--process-grammar)

  (should
   (equal
    '((a b) (a c) (a) (b a) (b) (c a) (c) (c b) (e))
    (parser--first 'S)))
  (message "Passed first 2 with complex grammar")

  (parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser--set-look-ahead-number 3)
  (parser--process-grammar)

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
  (parser--process-grammar)

  (should
   (equal
    '((c a) (c b))
    (parser--e-free-first 'S)))
  (message "Passed empty-free-first 2 with complex grammar")

  (message "Passed tests for (parser--empty-free-first)"))

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

  ;; TODO Add tests for this

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
  (parser--process-grammar)

  (should (equal
           '((A))
           (parser--get-grammar-rhs 'S)))
  (should (equal
           '(("b" "a"))
           (parser--get-grammar-rhs 'A)))

  (parser--set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (parser--process-grammar)

  (should (equal
           '((A) (B))
           (parser--get-grammar-rhs 'S)))
  (should (equal
           '(("a") ("b" "a"))
           (parser--get-grammar-rhs 'A)))

  (message "Passed tests  for (parser--get-grammar-rhs)"))

(defun parser-test--valid-non-terminal-p ()
  "Test `parser--valid-non-terminal-p'."
  (message "Starting tests  for (parser--valid-non-terminal-p)")

  (parser--set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (parser--process-grammar)

  (should
   (equal
    t
    (parser--valid-non-terminal-p 'S)))
  (should
   (equal
    t
    (parser--valid-non-terminal-p 'A)))
  (should
   (equal
    t
    (parser--valid-non-terminal-p 'B)))
  (should
   (equal
    nil
    (parser--valid-non-terminal-p 'C)))
  (should
   (equal
    nil
    (parser--valid-non-terminal-p "a")))

  (message "Passed tests  for (parser--valid-non-terminal-p)"))

(defun parser-test--valid-terminal-p ()
  "Test `parser--valid-terminal-p'."
  (message "Starting tests  for (parser--valid-terminal-p)")

  (parser--set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (parser--process-grammar)

  (should
   (equal
    t
    (parser--valid-terminal-p "a")))
  (should
   (equal
    t
    (parser--valid-terminal-p "b")))
  (should
   (equal
    t
    (parser--valid-terminal-p "a")))
  (should
   (equal
    nil
    (parser--valid-terminal-p 'S)))
  (should
   (equal
    nil
    (parser--valid-terminal-p 'A)))

  (message "Passed tests  for (parser--valid-terminal-p)"))

(defun parser-test ()
  "Run test."
  ;; (setq debug-on-error t)

  ;; Helpers
  (parser-test--valid-look-ahead-p)
  (parser-test--valid-look-ahead-number-p)
  (parser-test--valid-production-p)
  (parser-test--valid-grammar-p)
  (parser-test--valid-non-terminal-p)
  (parser-test--valid-sentential-form-p)
  (parser-test--valid-terminal-p)
  (parser-test--distinct)
  (parser-test--sort-list)
  (parser-test--get-grammar-rhs)
  (parser-test--get-grammar-look-aheads)

  ;; Algorithms
  (parser-test--first)
  (parser-test--e-free-first)
  (parser-test--follow))

(provide 'parser-test)

;;; parser-test.el ends here
