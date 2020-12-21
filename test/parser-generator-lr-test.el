;;; parser-generator-lr-test.el --- Tests for LR(k) Parser Generator -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator-lr)
(require 'ert)

(defun parser-generator-lr-test--generate-action-tables ()
  "Test `parser-generator-lr--generate-action-tables'."
  (message "Starting tests for (parser-generator-lr--generate-action-tables)")

  ;; Example 5.32 p. 393
  (parser-generator--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator--set-look-ahead-number 1)
  (parser-generator--process-grammar)
  (parser-generator-lr-generate-parser-tables)

  ;; Fig. 5.9 p. 374
  (should
   (equal
    '((0 (((a) reduce 2) ((e) reduce 2)))
      (1 (((a) shift) ((e) accept)))
      (2 (((a) reduce 2) ((b) reduce 2)))
      (3 (((a) shift) ((b) shift)))
      (4 (((a) reduce 2) ((b) reduce 2)))
      (5 (((a) reduce 1) ((e) reduce 1)))
      (6 (((a) shift) ((b) shift)))
      (7 (((a) reduce 1) ((b) reduce 1))))
      (parser-generator--hash-to-list parser-generator-lr--action-tables)))

  (message "Ended tests for (parser-generator-lr--generate-action-tables)"))

(defun parser-generator-lr-test--generate-goto-tables ()
  "Test `parser-generator-lr--generate-goto-tables'."
  (message "Starting tests for (parser-generator-lr--generate-goto-tables)")

  ;; Example 5.30, p. 389
  (parser-generator--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator--set-look-ahead-number 1)
  (parser-generator--process-grammar)
  (let ((table-lr-items (parser-generator-lr-generate-parser-tables)))

    ;; (message "GOTO-table: %s" parser-generator-lr--goto-tables)
    ;; (message "LR-items: %s" (parser-generator--hash-to-list parser-generator-lr--items))

    (should
     (equal
      '((0 ((S 1)))
        (1 ((a 2)))
        (2 ((S 3)))
        (3 ((a 4) (b 5)))
        (4 ((S 6)))
        (5 nil)
        (6 ((a 4) (b 7)))
        (7 nil))
      (parser-generator--hash-to-list parser-generator-lr--goto-tables)))

    (should
     (equal
      '((0 ((S nil (S a S b) (a)) (S nil (S a S b) (e)) (S nil nil (a)) (S nil nil (e)) (Sp nil (S) (e))))
        (1 ((S (S) (a S b) (a)) (S (S) (a S b) (e)) (Sp (S) nil (e))))
        (2 ((S (S a) (S b) (a)) (S (S a) (S b) (e)) (S nil (S a S b) (a)) (S nil (S a S b) (b)) (S nil nil (a)) (S nil nil (b))))
        (3 ((S (S) (a S b) (a)) (S (S) (a S b) (b)) (S (S a S) (b) (a)) (S (S a S) (b) (e))))
        (4 ((S (S a) (S b) (a)) (S (S a) (S b) (b)) (S nil (S a S b) (a)) (S nil (S a S b) (b)) (S nil nil (a)) (S nil nil (b))))
        (5 ((S (S a S b) nil (a)) (S (S a S b) nil (e))))
        (6 ((S (S) (a S b) (a)) (S (S) (a S b) (b)) (S (S a S) (b) (a)) (S (S a S) (b) (b))))
        (7 ((S (S a S b) nil (a)) (S (S a S b) nil (b)))))
      (parser-generator--hash-to-list table-lr-items))))

  (message "Passed LR-items for example 5.30")

  ;; Example 5.30, p. 389 but with terminals as strings
  (parser-generator--set-grammar '((Sp S) ("a" "b") ((Sp S) (S (S "a" S "b")) (S e)) Sp))
  (parser-generator--set-look-ahead-number 1)
  (parser-generator--process-grammar)

  (let ((table-lr-items (parser-generator-lr-generate-parser-tables)))

    ;; (message "GOTO-table: %s" (parser-generator--hash-to-list parser-generator-lr--goto-tables))
    ;; (message "LR-items: %s" (parser-generator--hash-to-list parser-generator-lr--items))

    (should
     (equal
      '((0 ((S 1)))
        (1 (("a" 2)))
        (2 ((S 3)))
        (3 (("a" 4) ("b" 5)))
        (4 ((S 6)))
        (5 nil)
        (6 (("a" 4) ("b" 7)))
        (7 nil))
      (parser-generator--hash-to-list parser-generator-lr--goto-tables)))

    (should
     (equal
      '((0 ((S nil (S "a" S "b") ("a")) (S nil (S "a" S "b") (e)) (S nil nil ("a")) (S nil nil (e)) (Sp nil (S) (e))))
        (1 ((S (S) ("a" S "b") ("a")) (S (S) ("a" S "b") (e)) (Sp (S) nil (e))))
        (2 ((S (S "a") (S "b") ("a")) (S (S "a") (S "b") (e)) (S nil (S "a" S "b") ("a")) (S nil (S "a" S "b") ("b")) (S nil nil ("a")) (S nil nil ("b"))))
        (3 ((S (S) ("a" S "b") ("a")) (S (S) ("a" S "b") ("b")) (S (S "a" S) ("b") ("a")) (S (S "a" S) ("b") (e))))
        (4 ((S (S "a") (S "b") ("a")) (S (S "a") (S "b") ("b")) (S nil (S "a" S "b") ("a")) (S nil (S "a" S "b") ("b")) (S nil nil ("a")) (S nil nil ("b"))))
        (5 ((S (S "a" S "b") nil ("a")) (S (S "a" S "b") nil (e))))
        (6 ((S (S) ("a" S "b") ("a")) (S (S) ("a" S "b") ("b")) (S (S "a" S) ("b") ("a")) (S (S "a" S) ("b") ("b"))))
        (7 ((S (S "a" S "b") nil ("a")) (S (S "a" S "b") nil ("b")))))
      (parser-generator--hash-to-list table-lr-items))))

  (message "Passed LR-items for example 5.30 but with tokens as strings")

  (message "Passed tests for (parser-r--generate-goto-tables)"))

(defun parser-generator-lr-test--items-for-prefix ()
  "Test `parser-generator-lr--items-for-prefix'."
  (message "Starting tests for (parser-generator-lr--items-for-prefix)")

  ;; Example 5.29 p 387
  (parser-generator--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator--set-look-ahead-number 1)
  (parser-generator--process-grammar)

  (should
   (equal
    '((S nil (S a S b) (a))
      (S nil (S a S b) (e))
      (S nil nil (a))
      (S nil nil (e))
      (Sp nil (S) (e)))
    (parser-generator-lr--items-for-prefix 'e)))
  (message "Passed V(e)")

  (should
   (equal
    '((S (S) (a S b) (a))
      (S (S) (a S b) (e))
      (Sp (S) nil (e)))
    (parser-generator-lr--items-for-prefix 'S)))
  (message "Passed V(S)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix 'a)))
  (message "Passed V(a)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix 'b)))
  (message "Passed V(b)")

  (should
   (equal
    '((S (S a) (S b) (a))
      (S (S a) (S b) (e))
      (S nil (S a S b) (a))
      (S nil (S a S b) (b))
      (S nil nil (a))
      (S nil nil (b)))
    (parser-generator-lr--items-for-prefix '(S a))))
  (message "Passed V(Sa)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix '(S S))))
  (message "Passed V(SS)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix '(S b))))
  (message "Passed V(Sb)")

  ;; a3 p. 390
  (should
   (equal
    '((S (S) (a S b) (a))
      (S (S) (a S b) (b))
      (S (S a S) (b) (a))
      (S (S a S) (b) (e)))
    (parser-generator-lr--items-for-prefix '(S a S))))
  (message "Passed V(SaS)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix '(S a a))))
  (message "Passed V(Saa)")

  (should
   (equal
    nil
    (parser-generator-lr--items-for-prefix '(S a b))))
  (message "Passed V(Sab)")

  (message "Passed tests for (parser-generator-lr--items-for-prefix)"))

(defun parser-generator-lr-test--items-valid-p ()
  "Test `parser-generator-lr--items-valid-p'."
  (message "Started tests for (parser-generator-lr--items-valid-p)")

  (parser-generator--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator--set-look-ahead-number 1)
  

  (let ((table-lr-items (parser-generator--process-grammar)))

    (should
     (equal
      t
      (parser-generator-lr--items-valid-p (parser-generator--hash-values-to-list table-lr-items t))))

    (message "Passed first"))

  (should
   (equal
    nil
    (parser-generator-lr--items-valid-p
     '(((S nil (S a S b) (a)) (S nil (S a S b) (e)) (S nil nil (a)) (S nil nil (e)) (Sp nil (S) (e))) ((S (S) (a S b) (a)) (S (S) (a S b) (e)) (Sp (S) nil (e))) ((S (S a) (S b) (a)) (S (S a) (S b) (e)) (S nil (S a S b) (a)) (S nil (S a S b) (b)) (S nil nil (a)) (S nil nil (b))) ((S (S) (a S b) (a)) (S (S) (a S b) (b)) (S (S a S) (b) (a)) (S (S a S) (b) (e))) ((S (S a S b) nil (a)) (S (S a S b) (a) (a)) (S (S a S b) nil (e))) ((S (S a) (S b) (a)) (S (S a) (S b) (b)) (S nil (S a S b) (a)) (S nil (S a S b) (b)) (S nil nil (a)) (S nil nil (b))) ((S (S) (a S b) (a)) (S (S) (a S b) (b)) (S (S a S) (b) (a)) (S (S a S) (b) (b))) ((S (S a S b) nil (a)) (S (S a S b) nil (b)))))))

  (message "Passed tests for (parser-generator-lr--items-valid-p)"))

(defun parser-generator-lr-test--parse ()
  "Test `parser-generator-lr--parse'."
  (message "Started tests for (parser-generator-lr--parse)")

  (parser-generator--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator--set-look-ahead-number 1)
  (parser-generator--process-grammar)
  (parser-generator-lr-generate-parser-tables)

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((a 1 . 2) (a 2 . 3) (b 3 . 4) (b 4 . 5)))
            (string-length (length string))
            (max-index (1+ index))
            (tokens))
       (while (and
               (< index string-length)
               (< index max-index))
         (push (nth index string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))

  (should
   (equal
    '((2 2 2 1 1) nil)
    (parser-generator-lr--parse)))

  (message "Passed test with terminals as symbols")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((a 1 . 2) (a 2 . 3) (b 3 . 4) (b 4 . 5) (b 5 . 6)))
            (string-length (length string))
            (max-index (1+ index))
            (tokens))
       (while (and
               (< index string-length)
               (< index max-index))
         (push (nth index string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))

  (should-error
   (parser-generator-lr--parse))

  (message "Passed test with terminals as symbols, invalid syntax")

  ;; Test with terminals as strings here

  (parser-generator--set-grammar '((Sp S) ("a" "b") ((Sp S) (S (S "a" S "b")) (S e)) Sp))
  (parser-generator--set-look-ahead-number 1)
  (parser-generator--process-grammar)
  (parser-generator-lr-generate-parser-tables)

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '(("a" 1 . 2) ("a" 2 . 3) ("b" 3 . 4) ("b" 4 . 5)))
            (string-length (length string))
            (max-index (1+ index))
            (tokens))
       (while (and
               (< index string-length)
               (< index max-index))
         (push (nth index string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))

  (should
   (equal
    '((2 2 2 1 1) nil)
    (parser-generator-lr--parse)))

  (message "Passed test with terminals as string")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '(("a" 1 . 2) ("a" 2 . 3) ("b" 3 . 4) ("b" 4 . 5) ("b" 5 . 6)))
            (string-length (length string))
            (max-index (1+ index))
            (tokens))
       (while (and
               (< index string-length)
               (< index max-index))
         (push (nth index string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))

  (should-error
   (parser-generator-lr--parse))

  (message "Passed test with terminals as string, invalid syntax")

  (message "Passed tests for (parser-generator-lr--parse)"))

(defun parser-generator-lr-test ()
  "Run test."
  ;; (setq debug-on-error t)

  (parser-generator-lr-test--items-for-prefix)
  (parser-generator-lr-test--items-valid-p)
  (parser-generator-lr-test--generate-goto-tables)
  (parser-generator-lr-test--generate-action-tables)
  (parser-generator-lr-test--parse))


(provide 'parser-generator-lr-test)

;;; parser-generator-lr-test.el ends here
