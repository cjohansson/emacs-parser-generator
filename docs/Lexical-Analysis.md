# Lexical Analysis

Set lexical analysis function by setting variable `parser-generator-lex-analyzer--function`. Optionally set reset function by setting variable `parser-generator-lex-analyzer--reset-function`. The lexical analysis is indexed on variable `parser-generator-lex-analyzer--index`. All parsers expect a list of tokens as response from lexical-analysis.

## Example

``` emacs-lisp
(setq
   parser-generator-lex-analyzer--function
   (lambda (index length)
     (let* ((string '(("a" 1 . 2) ("b" 2 . 3) ("c" 3 . 4) ("d" 4 . 5)))
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
    '(("a" 1 . 2))
    (parser-generator-lex-analyzer--peek-next-look-ahead)))
```

[Back to start](../../../)
