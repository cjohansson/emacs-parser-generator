;;; parser-generator-lex-analyzer-test.el --- Tests for lex-analyzer -*- lexical-binding: t -*-


;;; Commentary:


;;; Code:


(require 'parser-generator-lex-analyzer)
(require 'ert)

(defun parser-generator-lex-analyzer-test--peek-next-look-ahead ()
  "Test `parser-generator-lex-analyzer--peek-next-look-ahead'."
  (message "Starting tests for (parser-generator-lex-analyzer--peek-next-look-ahead)")

  (setq parser-generator-lex-analyzer--index nil)
  (setq parser-generator-lex-analyzer--function nil)
  (setq parser-generator--look-ahead-number nil)

  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))
  (setq parser-generator-lex-analyzer--function (lambda (index length) (substring "abcdefghijklmnopqrstuvxz" index (+ index length))))
  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))
  (parser-generator-lex-analyzer--reset)
  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))

  (setq parser-generator--look-ahead-number 1)
  (should
   (equal
    "a"
    (parser-generator-lex-analyzer--peek-next-look-ahead)))

  (message "Ended tests for (parser-generator-lex-analyzer--peek-next-look-ahead)"))

(defun parser-generator-lex-analyzer-test--pop-token ()
  "Test `parser-generator-lex-analyzer--pop-token'."
  (message "Starting tests for (parser-generator-lex-analyzer--pop-token)")

  (setq parser-generator-lex-analyzer--index nil)
  (setq parser-generator-lex-analyzer--function nil)
  (setq parser-generator--look-ahead-number nil)

  (should-error
   (parser-generator-lex-analyzer--pop-token))
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index length)
     (let* ((string "ab")
            (string-length (length string)))
       (when (> string-length index)
         (if (>= string-length (+ index length))
             (substring string index (+ index length))
           (substring string index (1- string-length)))))))
  (should-error
   (parser-generator-lex-analyzer--pop-token))
  (parser-generator-lex-analyzer--reset)

  (should
   (equal
    "a"
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    "b"
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    nil
    (parser-generator-lex-analyzer--pop-token)))

  (message "Ended tests for (parser-generator-lex-analyzer--pop-token)"))

(defun parser-generator-lex-analyzer-test ()
  "Run test."
  (parser-generator-lex-analyzer-test--peek-next-look-ahead)
  (parser-generator-lex-analyzer-test--pop-token))


(provide 'parser-generator-lex-analyzer-test)

;;; parser-generator-lex-analyzer-test.el ends here
