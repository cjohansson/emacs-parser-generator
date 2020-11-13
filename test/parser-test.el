;;; parser-test.el --- Tests for parser -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'parser)
(require 'ert)

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

(defun parser-test--first ()
  "Test `parser--first'."
  (message "Starting tests for (parser--first)")

  (parser--set-grammar '((S) (a) ((S a)) S) 1)
  (should
   (equal
    '((a))
    (parser--first 'S)))
  (message "Passed first 1 with rudimentary grammar")

  (parser--set-grammar '((S) ("a" "b" "c") ((S ("a" "b" "c"))) S) 2)
  (should
   (equal
    '(("a" "b"))
    (parser--first 'S)))
  (message "Passed first 2 with rudimentary grammar")

  (parser--set-grammar '((S) ("a" b "c") ((S ("a" b "c"))) S) 3)
  (should
   (equal
    '(("a" b "c"))
    (parser--first 'S)))
  (message "Passed first 3 with rudimentary grammar")

  (parser--set-grammar '((S A) (b) ((S A) (A b)) S) 2)
  (should
   (equal
    '((b))
    (parser--first 'S)))
  (message "Passed first 1 with intermediate grammar")

  (parser--set-grammar '((S A) ("a" "b") ((S A) (A ("b" "a"))) S) 2)
  (should
   (equal
    '(("b" "a"))
    (parser--first 'S)))
  (message "Passed first 2 with intermediate grammar")

  (parser--set-grammar '((S A) ("a" "b" "c" "d") ((S A) (A ("b" "a" "c" "d"))) S) 3)
  (should
   (equal
    '(("b" "a" "c"))
    (parser--first 'S)))
  (message "Passed first 3 with intermediate grammar")

  (parser--set-grammar '((S A B) ("c" "d") ((S A) (A B) (B "c" "d")) S) 1)
  (should
   (equal
    '(("c") ("d"))
    (parser--first 'S)))
  (message "Passed first 1 with semi-complex grammar")

  (parser--set-grammar '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S) 2)
  (should
   (equal
    '((c f) (d a))
    (parser--first 'S)))
  (message "Passed first 2 with semi-complex grammar")

  (parser--set-grammar '((S A B) ("a" "c" "d" "m") ((S A) (A (B "a" "m")) (B "c" "d")) S) 3)
  (should
   (equal
    '(("c" "a" "m") ("d" "a" "m"))
    (parser--first 'S)))
  (message "Passed first 3 with semi-complex grammar")

  (parser--set-grammar '((S A B C) (a b c) ((S A B) (A (B a) e) (B (C b) C) (C c e)) S) 1)
  (should
   (equal
    '((a) (e) (c) (b) )
    (parser--first 'S)))
  (message "Passed first 1 with complex grammar")

  ;; Example 5.28 p 382
  (parser--set-grammar '((S A B C) ("a" "b" "c") ((S A B) (A (B "a") e) (B (C "b") C) (C "c" e)) S) 2)
  (should
   (equal
    '(("a") ("a" "b") ("a" "c") ("b") ("b" "a") ("c") ("c" "a") ("c" "b") (e))
    (parser--first 'S)))
  (message "Passed first 2 with complex grammar")

  (parser--set-grammar '((S A B C) ("a" "b" "c") ((S A B) (A (B "a") e) (B (C "b") C) (C "c" e)) S) 3)
  (should
   (equal
    '(("a") ("a" "b") ("a" "c") ("a" "c" "b") ("b") ("b" "a") ("b" "a" "b") ("b" "a" "c") "c" ("c" "a") ("c" "a" "b") ("c" "a" "c") ("c" "b") ("c" "b" "a") (e))
    (parser--first 'S)))
  (message "Passed first 3 with complex grammar")

  (message "Passed tests for (parser--first)"))

;; Example 5.28 page 402
(defun parser-test--e-free-first ()
  "Test `parser--e-free-first'."
  (message "Starting tests for (parser--e-free-first)")

  ;; Example 5.28 p 402
  (should
   (equal
    '(("c" "a") ("c" "b"))
    (parser--e-free-first
     2
     'S
     '((S (A B))
       (A (B "a") e)
       (B (C "b") C)
       (C "c" e)))))
  (message "Passed empty-free-first 2 with complex grammar")

  (message "Passed tests for (parser--empty-free-first)"))

;; (defun parser-test--v-set ()
;;   "Test `parser--v-set'."
;;   (message "Starting tests for (parser-test--v-set)")

;;   ;; Example 5.29 p 407
;;   (should
;;    (equal
;;     '("ca" "cb")
;;     (parser--v-set
;;      'e
;;      '((S' S)
;;        (S SaSb)
;;        (S e))
;;      'S')))
;;   (message "Passed empty-free-first 2 with complex grammar")

;;   (message "Passed tests for (parser-test--v-set)"))

;; TODO Re-implement this function
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

(defun parser-test ()
  "Run test."
  ;; (setq debug-on-error t)
  (parser-test--valid-look-ahead-number-p)
  (parser-test--valid-production-p)
  (parser-test--valid-grammar-p)
  (parser-test--distinct)
  (parser-test--valid-sentential-form-p)
  (parser-test--first)
  (parser-test--e-free-first)
  ;; (parser-test--v-set)
  )

(provide 'parser-test)

;;; parser-test.el ends here
