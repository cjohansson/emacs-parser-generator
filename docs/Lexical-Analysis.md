# Lexical Analysis

Set lexical analysis function by setting variable `parser-generator-lex-analyzer--function`. Optionally set reset function by setting variable `parser-generator-lex-analyzer--reset-function`. The lexical analysis is indexed on variable `parser-generator-lex-analyzer--index`. All parsers expect a list of tokens as response from lexical-analysis.

### Peek next look-ahead

Returns the look-ahead number of next terminals in stream.

``` emacs-lisp
(require 'ert)
(setq
   parser-generator-lex-analyzer--function
   (lambda (index length)
     (let* ((string '(a a b b b))
            (string-length (length string))
            (max-index (+ index length))
            (tokens))
       (while (and
               (< index string-length)
               (< index max-index))
         (push (nth index string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
(parser-generator-lex-analyzer--reset)

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

```

### Pop token

Returns the next token in stream and moves the index one point forward.

``` emacs-lisp
(require 'ert)
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
```

[Back to start](../../../)
