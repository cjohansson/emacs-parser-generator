# LR(k) Parser

LR(k) parser is a Left-to-right, Rightmost derivation in reverse with look-ahead number k invented by Donald Knuth.

This library contains functions to parse, translate, validate grammars as well as exporting parser, parser/translators as stand-alone emacs-lisp code.

## LR Item

A valid LR-item for a viable prefix has this structure:

``` emacs-lisp
(A B C L)
```

Example with grammar with production: S -> SaSb and S is non-terminal and a, b are terminals. Look-ahead number: 1

``` emacs-lisp
((S) nil (S a S b) (a))
```

* A is the production LHS
* B, C is parts of the production RHS, if the dot is at the left B is nil and C is the entire RHS. If the dot is at the right then B is the production RHS and C is nil, otherwise B and C contains parts of the RHS
* L is the item look-ahead

## Declare default conflict resolution

If two items A and B both lack precedence values you can allow a default shift action with:

``` emacs-lisp
(setq parser-generator-lr--allow-default-conflict-resolution t)
```

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

## LR items for prefix (S)

Calculate the set of LR items valid for any viable prefix S.

``` emacs-lisp
(require 'parser-generator-lr)
(require 'ert)

(parser-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
(parser-set-look-ahead-number 1)
(parser-process-grammar)

(should
  (equal
   '(((S) nil (S a S b) ($))
     ((S) nil (S a S b) (a))
     ((S) nil nil ($))
     ((S) nil nil (a))
     ((Sp) nil (S) ($)))
   (parser-generator-lr--items-for-prefix 'e)))
```

``` emacs-lisp
(require 'parser-generator-lr)
(require 'ert)

(parser-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
(parser-set-look-ahead-number 1)
(parser-process-grammar)

(should
  (equal
   '(((S) (S) (a S b) ($))
     ((S) (S) (a S b) (a))
     ((Sp) (S) nil ($)))
   (parser-generator-lr--items-for-prefix 'S)))
```

## Parse

Perform a right-parse of input-stream.

```emacs-lisp
(require 'parser-generator-lr)
(require 'ert)

(parser-generator-set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
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

(should
 (equal
  '(2 2 2 1 1)
  (parser-generator-lr-parse)))
```

## Translate

Each production RHS can optionally contain a lambda-expression that will be called if specified when a reduction is made, example:

```emacs-lisp
(require 'parser-generator-lr)
(require 'ert)

(let ((buffer (generate-new-buffer "*a*")))
    (switch-to-buffer buffer)
    (insert "if (a) { b; }")

    (parser-generator-set-grammar 
      '((Sp S) (";" OPEN_ROUND_BRACKET CLOSE_ROUND_BRACKET ECHO IF OPEN_CURLY_BRACKET CLOSE_CURLY_BRACKET VARIABLE) 
        ((Sp S) (S (IF OPEN_ROUND_BRACKET VARIABLE CLOSE_ROUND_BRACKET OPEN_CURLY_BRACKET VARIABLE ";" 
          CLOSE_CURLY_BRACKET (lambda(args) (format "(when %s %s)" (nth 2 args) (nth 5 args)))))) Sp))
    (parser-generator-set-look-ahead-number 1)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    (setq
     parser-generator-lex-analyzer--function
     (lambda (index)
       (with-current-buffer buffer
         (unless (>= index (point-max))
           (goto-char index)
           (unless (looking-at "[^ \n\t]")
             (search-forward-regexp "[^ \n\t]" nil t nil)
             (forward-char -1))
           (let ((token))
             (cond
              ((looking-at "if")
               (setq token `(IF ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "echo")
               (setq token `(ECHO ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "(")
               (setq token `(OPEN_ROUND_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at ")")
               (setq token `(CLOSE_ROUND_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "{")
               (setq token `(OPEN_CURLY_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "}")
               (setq token `(CLOSE_CURLY_BRACKET ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at ";")
               (setq token `(";" ,(match-beginning 0) . ,(match-end 0))))
              ((looking-at "[a-zA-Z]+")
               (setq token `(VARIABLE ,(match-beginning 0) . ,(match-end 0))))
              (t (error "Invalid syntax! Could not lex-analyze at %s!" (point))))
             token)))))

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
      "(when a b)"
      (parser-generator-lr-translate)))
    (message "Passed test with non-nested translation")

    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))

    (parser-generator-set-grammar 
      '((Sp S T) (";" OPEN_ROUND_BRACKET CLOSE_ROUND_BRACKET ECHO IF OPEN_CURLY_BRACKET CLOSE_CURLY_BRACKET VARIABLE) 
        ((Sp S) (S (IF OPEN_ROUND_BRACKET VARIABLE CLOSE_ROUND_BRACKET OPEN_CURLY_BRACKET T 
          CLOSE_CURLY_BRACKET (lambda(args) (format "(when %s %s)" (nth 2 args) (nth 5 args))))) 
            (T (ECHO VARIABLE ";" (lambda(args) (format "(message %s)" (nth 1 args)))) 
              (VARIABLE ";" (lambda(args) (format "%s" (nth 0 args)))))) Sp))
    (parser-generator-set-look-ahead-number 1)
    (parser-generator-process-grammar)
    (parser-generator-lr-generate-parser-tables)

    (insert "if (a) { echo b; }")

    (should
     (equal
      "(when a (message b))"
      (parser-generator-lr-translate)))

    (message "Passed test with nested-translation with depth 2")

    (switch-to-buffer buffer)
    (kill-region (point-min) (point-max))
    (goto-char 1)
    (insert "if (a) { echo b }")

    (should-error
     (parser-generator-lr-parse))

    (kill-buffer buffer))
```

## Export

The export should be executed after a parser has been generated, example:

```emacs-lisp
(require 'parser-generator-lr)
(require 'ert)

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
```

[Example LR(k) Infix Calculator](LRk-Infix-Calculator.md)

[Back to syntax analysis](../Syntax-Analysis.md)
