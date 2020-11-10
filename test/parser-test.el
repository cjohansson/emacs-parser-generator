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

  (should
   (equal
    '("a")
    (parser--first
     1
     'S
     '(
       (S a)))))
  (message "Passed first 1 with rudimentary grammar")

  (should
   (equal
    '("ab")
    (parser--first
     2
     'S
     '(
       (S abc)))))
  (message "Passed first 2 with rudimentary grammar")

  (should
   (equal
    '("abc")
    (parser--first
     3
     'S
     '(
       (S abc)))))
  (message "Passed first 3 with rudimentary grammar")

  (should
   (equal
    '("b")
    (parser--first
     1
     'S
     '(
       (S A)
       (A b)))))
  (message "Passed first 1 with intermediate grammar")

  (should
   (equal
    '("ba")
    (parser--first
     2
     'S
     '(
       (S A)
       (A ba)))))
  (message "Passed first 2 with intermediate grammar")

  (should
   (equal
    '("bac")
    (parser--first
     3
     'S
     '(
       (S A)
       (A bace)))))
  (message "Passed first 3 with intermediate grammar")

  (should
   (equal
    '("c" "d")
    (parser--first
     1
     'S
     '(
       (S A)
       (A B)
       (B c d)))))
  (message "Passed first 1 with semi-complex grammar")

  (should
   (equal
    '("cf" "da")
    (parser--first
     2
     'S
     '(
       (S Aa)
       (A B)
       (B cf d)))))
  (message "Passed first 2 with semi-complex grammar")

  (should
   (equal
    '("cam" "dam")
    (parser--first
     3
     'S
     '(
       (S A)
       (A Bam)
       (B c d)))))
  (message "Passed first 3 with semi-complex grammar")

  (should
   (equal
    '("a" "b" "c" "e")
    (parser--first
     1
     'S
     '(
       (S AB)
       (A Ba e)
       (B Cb C)
       (C c e)))))
  (message "Passed first 1 with complex grammar")

  ;; Example 5.28 p 402
  (should
   (equal
    '("a" "ab" "ac" "b" "ba" "c" "ca" "cb" "e")
    (parser--first
     2
     'S
     '(
       (S AB)
       (A Ba e)
       (B Cb C)
       (C c e)))))
  (message "Passed first 2 with complex grammar")

  (should
   (equal
    '("a" "ab" "ac" "acb" "b" "ba" "bab" "bac" "c" "ca" "cab" "cac" "cb" "cba" "e")
    (parser--first
     3
     'S
     '(
       (S AB)
       (A Ba e)
       (B Cb C)
       (C c e)))))
  (message "Passed first 3 with complex grammar")

  (message "Passed tests for (parser--first)"))

;; Example 5.28 page 402
(defun parser-test--empty-free-first ()
  "Test `parser--empty-free-first'."
  (message "Starting tests for (parser-test--empty-free-first)")

  ;; Example 5.28 p 402
  (should
   (equal
    '("ca" "cb")
    (parser--empty-free-first
     2
     'S
     '(
       (S AB)
       (A Ba e)
       (B Cb C)
       (C c e)))))
  (message "Passed empty-free-first 2 with complex grammar")

  (message "Passed tests for (parser-test--empty-free-first)"))

(defun parser-test--v-set ()
  "Test `parser--v-set'."
  (message "Starting tests for (parser-test--v-set)")

    ;; Example 5.29 p 407
  (should
   (equal
    '("ca" "cb")
    (parser--v-set
     'e
     '((S' S)
       (S SaSb)
       (S e))
     'S')))
  (message "Passed empty-free-first 2 with complex grammar")

  (message "Passed tests for (parser-test--v-set)"))


(defun parser-test ()
  "Run test."
  (parser-test--distinct)
  (parser-test--first)
  (parser-test--empty-free-first)
  (parser-test--v-set))

(provide 'parser-test)

;;; parser-test.el ends here
