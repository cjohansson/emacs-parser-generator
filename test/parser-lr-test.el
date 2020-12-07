;;; parser-lr-test.el --- Tests for LR(k) Parser -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:

(require 'parser-lr)
(require 'ert)

(defun parser-lr-test--generate-goto-tables ()
  "Test `parser-lr--generate-goto-tables'."
  (message "Starting tests for (parser-lr--generate-goto-tables)")

  ;; Example 5.30, p. 389
  (parser-lr--reset)
  (parser--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser--set-look-ahead-number 1)

  (parser-lr--generate-goto-tables)

  ;; (message "GOTO-table: %s" parser-lr--goto-tables)
  ;; (message "LR-items: %s" (parser--hash-to-list parser-lr--items))

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
    parser-lr--goto-tables))

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
    (parser--hash-to-list parser-lr--items)))

  (message "Passed LR-items for example 5.30")

  (message "Passed tests for (parser-r--generate-goto-tables)"))

(defun parser-lr-test--items-for-prefix ()
  "Test `parser-lr--items-for-prefix'."
  (message "Starting tests for (parser-lr--items-for-prefix)")

  ;; Example 5.29 p 387
  (parser-lr--reset)
  (parser--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser--set-look-ahead-number 1)

  (should
   (equal
    '((S nil (S a S b) (a))
      (S nil (S a S b) (e))
      (S nil nil (a))
      (S nil nil (e))
      (Sp nil (S) (e)))
    (parser-lr--items-for-prefix 'e)))
  (message "Passed V(e)")

  (should
   (equal
    '((S (S) (a S b) (a))
      (S (S) (a S b) (e))
      (Sp (S) nil (e)))
    (parser-lr--items-for-prefix 'S)))
  (message "Passed V(S)")

  (should
   (equal
    nil
    (parser-lr--items-for-prefix 'a)))
  (message "Passed V(a)")

  (should
   (equal
    nil
    (parser-lr--items-for-prefix 'b)))
  (message "Passed V(b)")

  (should
   (equal
    '((S (S a) (S b) (a))
      (S (S a) (S b) (e))
      (S nil (S a S b) (a))
      (S nil (S a S b) (b))
      (S nil nil (a))
      (S nil nil (b)))
    (parser-lr--items-for-prefix '(S a))))
  (message "Passed V(Sa)")

  (should
   (equal
    nil
    (parser-lr--items-for-prefix '(S S))))
  (message "Passed V(SS)")

  (should
   (equal
    nil
    (parser-lr--items-for-prefix '(S b))))
  (message "Passed V(Sb)")

  ;; a3 p. 390
  (should
   (equal
    '((S (S) (a S b) (a))
      (S (S) (a S b) (b))
      (S (S a S) (b) (a))
      (S (S a S) (b) (e)))
    (parser-lr--items-for-prefix '(S a S))))
  (message "Passed V(SaS)")

  (should
   (equal
    nil
    (parser-lr--items-for-prefix '(S a a))))
  (message "Passed V(Saa)")

  (should
   (equal
    nil
    (parser-lr--items-for-prefix '(S a b))))
  (message "Passed V(Sab)")

  (message "Passed tests for (parser-lr--items-for-prefix)"))

(defun parser-lr-test--items-valid-p ()
  "Test `parser-lr--items-valid-p'."
  (message "Started tests for (parser-lr--items-valid-p)")

  (parser-lr--reset)
  (parser--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser--set-look-ahead-number 1)
  (parser-lr--generate-goto-tables)
  (should
   (equal
    t
    (parser-lr--items-valid-p (parser--hash-values-to-list parser-lr--items t))))

  (message "Passed first")

  (should
   (equal
    nil
    (parser-lr--items-valid-p
     '(((S nil (S a S b) (a)) (S nil (S a S b) (e)) (S nil nil (a)) (S nil nil (e)) (Sp nil (S) (e))) ((S (S) (a S b) (a)) (S (S) (a S b) (e)) (Sp (S) nil (e))) ((S (S a) (S b) (a)) (S (S a) (S b) (e)) (S nil (S a S b) (a)) (S nil (S a S b) (b)) (S nil nil (a)) (S nil nil (b))) ((S (S) (a S b) (a)) (S (S) (a S b) (b)) (S (S a S) (b) (a)) (S (S a S) (b) (e))) ((S (S a S b) nil (a)) (S (S a S b) (a) (a)) (S (S a S b) nil (e))) ((S (S a) (S b) (a)) (S (S a) (S b) (b)) (S nil (S a S b) (a)) (S nil (S a S b) (b)) (S nil nil (a)) (S nil nil (b))) ((S (S) (a S b) (a)) (S (S) (a S b) (b)) (S (S a S) (b) (a)) (S (S a S) (b) (b))) ((S (S a S b) nil (a)) (S (S a S b) nil (b)))))))

  (message "Passed tests for (parser-lr--items-valid-p)"))

(defun parser-lr-test ()
  "Run test."
  ;; (setq debug-on-error t)

  (parser-lr-test--items-for-prefix)
  (parser-lr-test--generate-goto-tables)
  (parser-lr-test--items-valid-p))

(provide 'parser-lr-test)

;;; parser-lr-test.el ends here
