# Lexical Analysis

Set lexical analysis function by setting variable `parser-generator-lex-analyzer--function`. Optionally set reset function by setting variable `parser-generator-lex-analyzer--reset-function`. 

The lexical analysis is internally indexed on a local variable `parser-generator-lex-analyzer--index` and has it optional state in the local variable `parser-generation-lex-analyzer--state`. The initial values for the index and state can be set in variables `parser-generation-lex-analyzer--index-init` and `parser-generator-lex-analyzer--state-init`.

All parsers expect a list as response from lexical-analysis, the first item in the list should be a list of one or more tokens. The second is "move index"-flag, if it is non-nil it is expected to be a integer representing the index to temporarily move the index to and perform a new lex. Third item is not used. The fourth item is the new state after the lex.

To enable exporting, the functions need to be specified in a way that the entire body is within the same block, do that using `(let)` or `(progn)` for example.

```emacs-lisp
  (setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("a" 2 . 3) ("b" 3 . 4) ("b" 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list tokens nil nil nil))))
```

## Token

A token is defined as a list with 3 elements, first is a string or symbol, second is the start index (including) of the token in the stream and third is the end index (excluding) of token in stream, second and third element have a dot between them, this structure is to be compatible with Emacs Semantic system. Example token:

``` emacs-lisp
'("a" 1 . 2)
```

or

``` emacs-lisp
'(a 1 . 2)
```

Which is a token that starts before position 1 and ends after position 2.

## Peek next look-ahead

Returns the look-ahead number of next terminals in stream, if end of stream is reached a EOF-identifier is returned. Result is expected to be a list with each token in it.

``` emacs-lisp
(require 'ert)
(setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5)))
            (string-length (length string))
            (max-index index)
            (tokens)
            (next-token)
            (new-index))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (setq next-token (nth (1- index) string))
         (setq new-index (cdr (cdr (nth (1- index) string))))
         (push next-token tokens)
         (setq index (1+ index)))
       (list (nreverse tokens) nil nil nil))))
(parser-generator-lex-analyzer--reset)

(setq parser-generator--look-ahead-number 1)
(should
 (equal
  '(("a" 1 . 2))
  (parser-generator-lex-analyzer--peek-next-look-ahead)))

(setq parser-generator--look-ahead-number 2)
(should
 (equal
  '(("a" 1 . 2) ("b" 2 . 3))
  (parser-generator-lex-analyzer--peek-next-look-ahead)))

(setq parser-generator--look-ahead-number 10)
(should
 (equal
  '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5) ($) ($) ($) ($) ($) ($))
  (parser-generator-lex-analyzer--peek-next-look-ahead)))
```

## Pop token

Returns the next token in stream and moves the lexical analyzer index one point forward. If end of stream is reached return nil. The result is expected to be a list containing each token popped.

``` emacs-lisp
(require 'ert)
(setq
   parser-generator-lex-analyzer--function
   (lambda (index _state)
     (let* ((string '(("a" 1 . 2) ("b" 2 . 3)))
            (string-length (length string))
            (max-index index)
            (tokens)
            (new-index))
       (while (and
               (< (1- index) string-length)
               (< (1- index) max-index))
         (setq new-index (cdr (cdr (nth (1- index) string))))
         (push (nth (1- index) string) tokens)
         (setq index (1+ index)))
       (list (nreverse tokens) nil nil nil))))
(parser-generator-lex-analyzer--reset)

(setq parser-generator--look-ahead-number 1)
(should
 (equal
  '(("a" 1 . 2))
  (parser-generator-lex-analyzer--pop-token)))
(should
 (equal
  '(("b" 2 . 3))
  (parser-generator-lex-analyzer--pop-token)))
(should
 (equal
  nil
  (parser-generator-lex-analyzer--pop-token)))
```

[Back to start](../../../)
