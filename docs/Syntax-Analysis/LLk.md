# LL(k) Parser

LL(k) parser is a Left-to-right, Leftmost derivation with look-ahead number k > 1.

This library contains functions to parse, translate, validate grammars.

## Parse

Perform a left-parse of input-stream.

```emacs-lisp
(require 'parser-generator-ll)
(require 'ert)

(parser-generator-set-eof-identifier '$)
(parser-generator-set-e-identifier 'e)
(parser-generator-set-look-ahead-number 2)
(parser-generator-set-grammar
 '(
   (S A)
   (a b)
   (
    (S (a A a a) (b A b a))
    (A b e)
    )
   S
   )
 )
(parser-generator-process-grammar)
(parser-generator-ll-generate-table)
(setq
 parser-generator-lex-analyzer--function
 (lambda (index)
   (let* ((string '((b 1 . 2) (b 2 . 3) (a 3 . 4)))
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

(should
 (equal
  '(1 3) ;; Example is indexed from 1 so that is why they have '(2 4)
  (parser-generator-ll-parse)))
(message "Passed example 5.16 p. 352")
```

## Translate

Each production RHS can optionally contain a lambda-expression that will be called if specified when stack is reduced:

```emacs-lisp
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
   (lambda (index)
     (let* ((string '((b 1 . 2) (b 2 . 3) (a 3 . 4)))
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
  (should
   (equal
    "delta ingrid laval"
    (parser-generator-ll-translate)))
  (message "Passed translation test 1")
```

## Export

```emacs-lisp
(require 'parser-generator-ll)
(require 'ert)

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
 (lambda (index)
   (let* ((string '((b 1 . 2) (b 2 . 3) (a 3 . 4)))
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
(let ((export (parser-generator-ll-export-to-elisp "ba")))
  (with-temp-buffer
    (insert export)
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
```


[Back to syntax analysis](../Syntax-Analysis.md)
