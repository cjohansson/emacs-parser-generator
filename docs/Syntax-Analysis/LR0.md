# LR(0) Parser

LR(k) parser is a Left-to-right, Rightmost derivation in reverse without a look-ahead invented by Donald Knuth.

This library contains functions to parse, translate, validate grammars as well as exporting parser, parser/translators as stand-alone emacs-lisp code.

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

## Declaring operator precedence

You can set global symbol operator precedence and also context-sensitive precedence, like in GNU Bison. Example

``` emacs-lisp
(require 'parser-generator-lr)

(setq
   parser-generator--global-attributes
   '(%left %precedence %right))
  (setq
   parser-generator-lr--global-precedence-attributes
   '(%left %precedence %right))
  (setq
   parser-generator--context-sensitive-attributes
   '(%prec))
  (setq
   parser-generator-lr--context-sensitive-precedence-attribute
   '%prec)
  (setq
   parser-generator--global-declaration
   '((%left a)
     (%right b)
     (%left c)
     (%precedence FIRST)))
(parser-generator-set-grammar
   '(
     (Sp S A B)
     (a b c)
     (
      (Sp S)
      (S (A c) B)
      (A (a b %prec a))
      (B (a b c %prec FIRST))
      )
     Sp))
```

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

    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "1+1*1")

    (should
     (equal
      '(5 3 5 2 5 1)
      (parser-generator-lr-parse)))
    (message "Passed parse with k = 0 # 2")
    (kill-buffer))
```

## Translate

Each production RHS can optionally contain a lambda-expression that will be called if specified when a reduction is made, example:

```emacs-lisp
(require 'parser-generator-lr)
(require 'ert)

(let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (insert "1+1")

    (parser-generator-set-grammar
     '(
       (S E B)
       ("*" "+" "0" "1")
       (
        (S (E $))
        (E
         (E "*" B (lambda(args) (let ((ret (list (nth 0 args)))) (when (nth 2 args) (setq ret (append ret `(" x " ,(nth 2 args))))) ret)))
         (E "+" B (lambda(args) (let ((ret (list (nth 0 args)))) (when (nth 2 args) (setq ret (append ret `(" . " ,(nth 2 args))))) ret)))
         (B)
         )
        (B
         ("0")
         ("1"))
        )
       S))
    (parser-generator-set-look-ahead-number 0)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    ;; Setup lex-analyzer
    (setq
     parser-generator-lex-analyzer--function
     (lambda (index)
       (with-current-buffer buffer
         (when (< index (point-max))
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
      '("1" " . " "1")
      (parser-generator-lr-translate)))
    (message "Passed translation k=0")
    (kill-buffer))
```

## Export

The export should be executed after a parser has been generated, example:

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
          '(5 3 5 2)
          (e---parse))))
      (message "Passed parse for exported parser"))))
```

[Back to syntax analysis](../Syntax-Analysis.md)
