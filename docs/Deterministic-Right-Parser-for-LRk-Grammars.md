# Deterministic Right Parser for LR(k) Grammars

A valid LR-item for a viable prefix has this structure:

``` emacs-lisp
(A B C L)
```

Example with grammar with production: S -> SaSb and S is non-terminal and a, b are terminals. Look-ahead number: 1

``` emacs-lisp
(S nil (S a S b) (a))
```

* A is the production LHS
* B, C is parts of the production RHS, if the dot is the left B is nil and C is the entire RHS. If the dot is at the right then B is the production RHS and C is nil, otherwise B and C contains parts of the RHS
* L is the terminal look-ahead

## LR items for prefix (S)

Calculate the set of LR items valid for any viable prefix S.

``` emacs-lisp
(require 'ert)

(parser--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
(parser--set-look-ahead-number 1)
(parser--process-grammar)

(should
  (equal
    '((S nil (S a S b) (a))
      (S nil (S a S b) (e))
      (S nil nil (a))
      (S nil nil (e))
      (Sp nil (S) (e)))
    (parser--lr-items-for-prefix 'e)))
```

``` emacs-lisp
(require 'ert)

(parser--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
(parser--set-look-ahead-number 1)
(parser--process-grammar)

(should
  (equal
    '((S (S) (a S b) (a))
      (S (S) (a S b) (e))
      (Sp (S) nil (e)))
    (parser--lr-items-for-prefix 'S)))
```

## Parse

Perform a right-parse of input-stream.

```emacs-lisp
(require 'ert)

(parser-generator--set-grammar '((Sp S) (a b) ((Sp S) (S (S a S b)) (S e)) Sp))
(parser-generator--set-look-ahead-number 1)
(parser-generator--process-grammar)

(setq
   parser-generator-lex-analyzer--function
   (lambda (index length)
     (let* ((string '((a 1 . 2) (a 2 . 3) (b 3 . 4) (b 4 . 5) (b 5 . 6)))
            (string-length (length string))
            (max-index (+ index length))
            (tokens))
       (while (and
               (< index string-length)
               (< index max-index))
         (push (nth index string) tokens)
         (setq index (1+ index)))
       (nreverse tokens))))
(should
   (equal
    '(2 2 2 1 1)
    (parser-generator-lr--parse)))
    

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
(should-error
    (parser-generator-lr--parse))
```

[Back to start](../../../)
