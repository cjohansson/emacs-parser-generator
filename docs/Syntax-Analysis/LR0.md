# LR(0) Parser

LR(k) parser is a Left-to-right, Rightmost derivation in reverse without a look-ahead invented by Donald Knuth.

This library contains functions to parse, translate, validate grammars as well as exporting parser, parser/translators as stand-alone emacs-lisp code. *WIP*

## LR Item

A valid LR-item for a viable prefix has this structure:

``` emacs-lisp
(A B C)
```

Example with grammar with production: S -> SaSb and S is non-terminal and a, b are terminals. Look-ahead number: 0

``` emacs-lisp
((S) nil (S a S b))
```

* A is the production LHS
* B, C is parts of the production RHS, if the dot is at the left B is nil and C is the entire RHS. If the dot is at the right then B is the production RHS and C is nil, otherwise B and C contains parts of the RHS

## Parse

Perform a right-parse of input-stream. Example from [Wikipedia](https://en.wikipedia.org/wiki/LR_parser#Constructing_LR(0)_parsing_tables).

```emacs-lisp
(require 'parser-generator-lr)
(require 'ert)

(let ((buffer (generate-new-buffer "*a*")))
  (switch-to-buffer buffer)
  (kill-region (point-min) (point-max))
  (insert "1+1")

  (parser-generator-set-grammar
   '((S E B) ("*" "+" "0" "1") ((S (E $)) (E (E "*" B) (E "+" B) (B)) (B ("0") ("1"))) S))
  (parser-generator-set-look-ahead-number 0)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  ;; Setup lex-analyzer
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (with-current-buffer buffer
       (when (<= (+ index 1) (point-max))
         (let ((start index)
               (end (+ index 1)))
           (let ((token (buffer-substring-no-properties start end)))
             `(,token ,start . ,end)))))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (with-current-buffer buffer
       (let ((start (car (cdr token)))
             (end (cdr (cdr token))))
         (when (<= end (point-max))
           (buffer-substring-no-properties
            start
            end))))))

  (should
   (equal
    '(5 3 5 2)
    (parser-generator-lr-parse)))
  (message "Passed parse with k = 0 # 1")
```

## Translate

Each production RHS can optionally contain a lambda-expression that will be called if specified when a reduction is made, example:

```emacs-lisp
(let ((buffer (generate-new-buffer "*a*")))
  (switch-to-buffer buffer)
  (kill-region (point-min) (point-max))
  (insert "1+1")

  (parser-generator-set-grammar
   '((S E B) ("*" "+" "0" "1") ((S (E $)) (E (E "*" B (lambda(args) (let ((ret (list (nth 0 args)))) (when (nth 2 args) (setq ret (append ret `(" x " ,(nth 2 args))))) ret))) (E "+" B (lambda(args) (let ((ret (list (nth 0 args)))) (when (nth 2 args) (setq ret (append ret `(" . " ,(nth 2 args))))) ret))) (B)) (B ("0") ("1"))) S))
  (parser-generator-set-look-ahead-number 0)
  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  ;; Setup lex-analyzer
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (with-current-buffer buffer
       (when (<= (+ index 1) (point-max))
         (let ((start index)
               (end (+ index 1)))
           (let ((token (buffer-substring-no-properties start end)))
             `(,token ,start . ,end)))))))
  (setq
   parser-generator-lex-analyzer--get-function
   (lambda (token)
     (with-current-buffer buffer
       (let ((start (car (cdr token)))
             (end (cdr (cdr token))))
         (when (<= end (point-max))
           (buffer-substring-no-properties start end))))))

  (should
   (equal
    '((("1")) " . " ("1"))
    (parser-generator-lr-translate)))
  (message "Passed translation k=0")
  (kill-buffer))
```

[Back to syntax analysis](../Syntax-Analysis.md)
