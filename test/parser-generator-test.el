;;; parser-generator-test.el --- Tests for Parser Generator -*- lexical-binding: t -*-


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
    t
    (parser-generator--valid-look-ahead-p "a")))
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
    t
    (parser-generator--valid-look-ahead-p 'e)))

  (message "Passed tests for (parser-generator--valid-look-ahead-p)"))

(defun parser-generator-test--get-grammar-look-aheads ()
  "Test `parser-generator--get-look-aheads'."
  (message "Starting tests for (parser-generator--get-grammar-look-aheads)")

  (parser-generator-set-look-ahead-number 1)
  (parser-generator-set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S))
  (parser-generator-process-grammar)

  (should
   (equal
    '(("a") ("b") (e))
    (parser-generator--get-grammar-look-aheads)))
  (message "Passed ((a) (b) (e))")

  (parser-generator-set-look-ahead-number 2)

  (should
   (equal
    '(("a" "a") ("a" "b") ("a" e) ("b" "a") ("b" "b") ("b" e))
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
    '((a))
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
    '((a))
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
    '((b))
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

  (parser-generator-set-grammar '((S A B C) (a b c) ((S A B) (A (B a) e) (B (C b) C) (C c e)) S))
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
    '((a b) (a c) (a) (b a) (b) (c a) (c) (c b) (e))
    (parser-generator--first 'S)))
  (message "Passed first 2 with complex grammar")

  (parser-generator-set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
  (parser-generator-set-look-ahead-number 3)
  (parser-generator-process-grammar)

  (should
   (equal
    '((a) (a b) (a c) (a c b) (b a) (b a b) (b a c) (b) (c a) (c a b) (c a c) (c b) (c) (c b a) (e))
    (parser-generator--first 'S)))
  (message "Passed first 3 with complex grammar")

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

  ;; TODO Test cases with trailing e-identifier here

  (message "Passed tests for (parser-generator--empty-free-first)"))

(defun parser-generator-test--valid-grammar-p ()
  "Test function `parser-generator--valid-grammar-p'."
  (message "Starting tests for (parser-generator--valid-grammar-p)")

  (should (equal
           t
           (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a")) A))))

  (should (equal
           nil
           (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a")) (A)))))

  (should (equal
           nil
           (parser-generator--valid-grammar-p '((A B C) (("a" "b") "c") ((A "a")) A))))

  (should (equal
           nil
           (parser-generator--valid-grammar-p '(((A B) C) ("a" "b" "c") ((A "a")) A))))

  (should (equal
           nil
           (parser-generator--valid-grammar-p '(((A B) C) ("a" "b" "c") ((A)) A))))

  (should (equal
           nil
           (parser-generator--valid-grammar-p "A")))

  (should (equal
           nil
           (parser-generator--valid-grammar-p '(A B C))))

  (should (equal
           nil
           (parser-generator--valid-grammar-p '((A B)))))

  (should (equal
           nil
           (parser-generator--valid-grammar-p '((A B C) (a (b c) "c") (A ("a" "b") (a b)) (B b) (C "c")))))

  (should (equal
           t
           (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a" (lambda(a) (message "Was here: %s" a)))) A))))

  (should (equal
           nil
           (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A "a" (lambda(a) (message "Was here: %s" a)) "b")) A))))

  (should (equal
           t
           (parser-generator--valid-grammar-p '((A B C) ("a" "b" "c") ((A ("a" (lambda(a) (message "Was here: %s" a))))) A))))

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
  (message "Starting tests  for (parser-generator--valid-sentential-form-p)")

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (A ("b" "a")) (B "b" (lambda(b) (message "Was here: %s" b)))) S))
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

  (message "Passed tests for (parser-generator--valid-sentential-form-p)"))

(defun parser-generator-test--valid-production-p ()
  "Test `parser-generator--valid-production-p'."
  (message "Starting tests  for (parser-generator--valid-production-p)")

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

  (message "Passed tests  for (parser-generator--valid-production-p)"))

(defun parser-generator-test--get-grammar-rhs ()
  "Test `parser-generator--get-grammar-rhs'."
  (message "Started tests  for (parser-generator--get-grammar-rhs)")

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (A ("b" "a")) (B "b" (lambda(b) (message "Was here: %s" b)))) S))
  (parser-generator-process-grammar)

  (should (equal
           '((A))
           (parser-generator--get-grammar-rhs 'S)))
  (should (equal
           '(("b" "a"))
           (parser-generator--get-grammar-rhs 'A)))

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (parser-generator-process-grammar)

  (should (equal
           '((A) (B))
           (parser-generator--get-grammar-rhs 'S)))
  (should (equal
           '(("a") ("b" "a"))
           (parser-generator--get-grammar-rhs 'A)))

  (message "Passed tests  for (parser-generator--get-grammar-rhs)"))

(defun parser-generator-test--valid-non-terminal-p ()
  "Test `parser-generator--valid-non-terminal-p'."
  (message "Starting tests  for (parser-generator--valid-non-terminal-p)")

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (parser-generator-process-grammar)

  (should
   (equal
    t
    (parser-generator--valid-non-terminal-p 'S)))
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

  (message "Passed tests  for (parser-generator--valid-non-terminal-p)"))

(defun parser-generator-test--valid-terminal-p ()
  "Test `parser-generator--valid-terminal-p'."
  (message "Starting tests  for (parser-generator--valid-terminal-p)")

  (parser-generator-set-grammar '((S A B) ("a" "b") ((S A) (S (B)) (B "a") (A "a") (A ("b" "a"))) S))
  (parser-generator-process-grammar)

  (should
   (equal
    t
    (parser-generator--valid-terminal-p "a")))
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

  (message "Passed tests  for (parser-generator--valid-terminal-p)"))

(defun parser-generator-test ()
  "Run test."
  ;; (setq debug-on-error t)

  ;; Helpers
  (parser-generator-test--valid-look-ahead-p)
  (parser-generator-test--valid-look-ahead-number-p)
  (parser-generator-test--valid-production-p)
  (parser-generator-test--valid-grammar-p)
  (parser-generator-test--valid-non-terminal-p)
  (parser-generator-test--valid-sentential-form-p)
  (parser-generator-test--valid-terminal-p)
  (parser-generator-test--distinct)
  (parser-generator-test--sort-list)
  (parser-generator-test--get-grammar-rhs)
  (parser-generator-test--get-grammar-look-aheads)

  ;; Algorithms
  (parser-generator-test--first)
  (parser-generator-test--e-free-first)
  (parser-generator-test--follow))


(provide 'parser-generator-test)

;;; parser-generator-test.el ends here
