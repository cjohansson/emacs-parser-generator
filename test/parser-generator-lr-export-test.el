;;; parser-generator-lr-export-test.el --- Tests for LR(k) Parser Export -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator-lr-export)
(require 'ert)

(defun parser-generator-lr-export-test-to-elisp ()
  "Test `parser-generator-lr-export'."
  (message "Started tests for (parser-generator-lr-export-to-elisp)")

  ;; Generate parser
  (parser-generator-set-grammar
   '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
  (parser-generator-set-look-ahead-number 1)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (let* ((string '((a 1 . 2) (a 2 . 3) (b 3 . 4) (b 4 . 5)))
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

  ;; Test parser
  (should
   (equal
    '(2 2 2 1 1)
    (parser-generator-lr-parse)))

  ;; Export parser
  (let ((export (parser-generator-lr-export-to-elisp "e--")))

    (with-temp-buffer
      (insert export)
      (eval-buffer)
      (should
       (equal
        t
        (fboundp 'e---parse)))

      (when (fboundp 'e---parse)
        (should
         (equal
          '(2 2 2 1 1)
          (e---parse))))
      (message "Passed parse for exported parser")))

  (message "Passed tests for (parser-generator-lr-export-to-elisp)"))

(defun parser-generator-lr-export-test ()
  "Run test."
  (parser-generator-lr-export-test-to-elisp))


(provide 'parser-generator-lr-export-test)

;;; parser-generator-lr-export-test.el ends here
