# LL(1) Parser

LL(1) parser is a Left-to-right, Leftmost derivation with look-ahead number k = 1.

This library contains functions to parse, translate, validate grammars.

## Parse

Perform a left-parse of input-stream.

```emacs-lisp
(require 'parser-generator-ll)
(require 'ert)
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
 (lambda (index)
   (let* ((string '((a 1 . 2) (b 2 . 3) (b 3 . 4) (a 4 . 5) (b 5 . 6)))
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
(parser-generator-ll-parse)
(should
 (equal
  '(0 3 1 2 1) ;; Example is indexed from 1 so that is why they have '(1 4 2 3 2)
  (parser-generator-ll-parse)))
(message "Passed example 5.5 p. 340")
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

*WIP*

[Back to syntax analysis](../Syntax-Analysis.md)
