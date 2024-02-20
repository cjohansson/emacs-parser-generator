;; parser-generator-ll-export-test.el --- Tests for Exported Generated LL(k) Parser -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'parser-generator-ll-export)
(require 'ert)

(defun parser-generator-ll-export-test-to-elisp ()
  "Test `parser-generator-ll-export-to-elisp'."
  (message "Started tests for (parser-generator-ll-export-to-elisp)")

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
   (lambda (index _state)
     (let* ((string '((b 1 . 2) (b 2 . 3) (a 3 . 4)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (nreverse tokens) nil index nil))))

  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (let ((export (parser-generator-ll-export-to-elisp "ba")))
    (with-temp-buffer
      (insert export)
      (write-file "test-ll-2-exported-parser.el")
      (eval-buffer)
      (should
       (equal
        t
        (fboundp 'ba-parse)))
      (should
       (equal
        t
        (fboundp 'ba-translate)))
      (when (fboundp 'ba-parse)
        (should
         (equal
          '(1 3)
          (ba-parse))))
      (when (fboundp 'ba-translate)
        (should
         (equal
          "delta ingrid laval"
          (ba-translate))))))
  (message "Passed exported test for example 5.16 p. 352")

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '((b 1 . 2) (b 2 . 3) (b 3 . 4) (a 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (nreverse tokens) nil index nil))))

  (let ((export (parser-generator-ll-export-to-elisp "ba2")))
    (with-temp-buffer
      (insert export)
      (eval-buffer)
      (should
       (equal
        t
        (fboundp 'ba2-parse)))
      (should
       (equal
        t
        (fboundp 'ba2-translate)))
      (when (fboundp 'ba2-translate)
        (should
         (equal
          "delta sven laval"
          (ba2-translate))))))
  (message "Passed exported test failing parse")

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
   (lambda (index _state)
     (let* ((string '((a 1 . 2) (b 2 . 3) (b 3 . 4) (a 4 . 5) (b 5 . 6)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (nreverse tokens) nil index nil))))

  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (car token)))
  (let ((export (parser-generator-ll-export-to-elisp "ba3")))
    (with-temp-buffer
      (insert export)
      (write-file "test-ll-1-exported-parser.el")
      (eval-buffer)
      (should
       (equal
        t
        (fboundp 'ba3-parse)))
      (should
       (equal
        t
        (fboundp 'ba3-translate)))
      (when (fboundp 'ba3-parse)
        (should
         (equal
          '(0 3 1 2 1)
          (ba3-parse))))))
  (message "Passed exported test for example 5.5 p. 340")

  (message "Passed tests for (parser-generator-ll-export-to-elisp)"))


(defun parser-generator-ll-export-test ()
  "Run test."
  (parser-generator-ll-export-test-to-elisp))


(provide 'parser-generator-ll-export-test)

;;; parser-generator-ll-export-test.el ends here
