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
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index length)
     (let* ((string '(a b c d))
            (string-length (length string))
            (max-index (+ index length))
            (tokens))
       (while (and
               (< index string-length)
               (< index max-index))
         (push (nth index string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))
  (parser-generator-lex-analyzer--reset)
  (should-error
   (parser-generator-lex-analyzer--peek-next-look-ahead))

  (setq parser-generator--look-ahead-number 1)
  (should
   (equal
    '(a)
    (parser-generator-lex-analyzer--peek-next-look-ahead)))

  (setq parser-generator--look-ahead-number 2)
  (should
   (equal
    '(a b)
    (parser-generator-lex-analyzer--peek-next-look-ahead)))

  (setq parser-generator--look-ahead-number 10)
  (should
   (equal
    '(a b c d)
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
     (let* ((string '(a b))
            (string-length (length string))
            (max-index (+ index length))
            (tokens))
       (while (and
               (< index string-length)
               (< index max-index))
         (push (nth index string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
  (should-error
   (parser-generator-lex-analyzer--pop-token))
  (parser-generator-lex-analyzer--reset)

  (should
   (equal
    '(a)
    (parser-generator-lex-analyzer--pop-token)))
  (should
   (equal
    '(b)
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
