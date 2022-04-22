# Syntax Analysis / Parsing

We use push down transducer (PDT) based algorithms.

## With Backtracking

* The Bottom-Up Parsing Algorithm *WIP*
* The Top-Down Parsing Algorithm *WIP*
* The Cocke-Younger-Kasami Algorithm *WIP*
* The Parsing Method of Earley *WIP*

## Without Backtracking

* LL(k) *WIP*
* [LR(k)](Syntax-Analysis/LRk.md)
* [LR(0)](Syntax-Analysis/LR0.md)
* Formal Shift-Reduce Parsing Algorithms *WIP*
* Simple Precedence Grammars *WIP*
* Extended Precedence Grammars *WIP*
* Weak Precedence Grammars *WIP*
* Bounded-Right-Context Grammars *WIP*
* Mixed Strategy Precedence Grammars *WIP*
* Operator Precedence Grammars *WIP*
* Floyd-Evans Production Language *WIP*

## With Limited Backtracking

* GTDPL *WIP*
* Noncanonical Pasing *WIP*
* Two-stack Parsers *WIP*

## Grammar

Grammar consists of `N`, `T`, `P` and `S`, where `N` is non-terminals, `T` is terminals, `P` is productions and `S` is start-production. Example:

* N = `'(S A B C)`
* T = `'(a b c)`
* P = `'((S (A B)) (A (B a) e) (B (C b) C) (C c e))`
* S = `'S`

Example:

``` emacs-lisp
(require 'parser-generator)

(parser-generator-set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
```

Productions can include context-sensitive attributes like this:

``` emacs-lisp
((S (A B %prec first)) (A (B a %weight c) e) (B (C b) C) (C c e))
```

### Global attributes

A list of valid attributes can be set in the variable `parser-generator--global-attributes`.

### Context-sensitive attributes

A list of valid attributes can be set in the variable `parser-generator--context-sensitive-attributes`.

### Global declaration

Can be set in variable `parser-generator--global-declaration`. This may be used differently in different parsing algorithms.

### e-identifier

The symbol defined in variable `parser-generator--e-identifier`, with default-value: `'e`, symbolizes the e symbol. The symbol is allowed in some grammars and not in others and can be used to make parts of grammar optional.

### End-of-file identifier

The symbol defined in variable `parser-generator--eof-identifier`, with default-value: `'$`, symbolizes the end-of-file symbol.

### Non-terminals

A non-terminal is either a symbol or a string so `"A"` and `A` are equally valid.

### Terminals

A terminal is either a symbol or a string so `"{"` and `A` are equally valid.

### Sentential-form

A list of one or more non-terminals and terminals, example `'(A "A" c ":")`, the e-symbol is allowed depending on grammar.

### Productions

A production consists of a list of at least two elements. The first element is the left-hand-side (LHS) and should contain at least one element. The right-hand-side (RHS) consists of the rest of the elements, if there is more than one list in RHS then each list will be treated as a alternative production RHS.

Example, production `S -> A | B` is defined as:

``` emacs-lisp
'(S A B)
```

Another example, production `S -> IF "{" EXPRESSION "}" | EXIT` is declared as:

``` emacs-lisp
'(S (IF "{" EXPRESSION "}") EXIT)
```

### Start

The start symbol is the entry-point of the grammar and should be either a string or a symbol and should exists in the list of productions as the LHS.

### Look-ahead number

Is a simple integer above zero. You set it like this: `(parser-generator-set-look-ahead-number 1)` for a `1` number look-ahead.

### Syntax-directed-translation (SDT)

A optional translation is defined as a lambda function as the last element of a production right-hand-side, example:

```emacs-lisp
(require 'parser-generator)

(parser-generator-set-grammar 
  '((Sp S) ("a" "b") ((Sp S) (S (S "a" S "b" (lambda(args _terminals) (nreverse args)))) (S e)) Sp))
```

You cannot have a SDT + SA on the same production right-hand side, just one or the other.

### Semantic actions (SA)

A optional semantic action is defined as a lambda function as the last element of a production right-hand-side, two arguments were first is the value of the symbols in the rule and the second is the terminal values for the same symbols, example:

```emacs-lisp
(require 'parser-generator)

(parser-generator-set-grammar 
  '((Sp S) ("a" "b") ((Sp S) (S (S "a" S "b" (lambda(args _terminals) (nreverse args)))) (S e)) Sp))
```

You cannot have a SDT + SA on the same production right-hand side, just one or the other.

## Functions

### FIRST(S)

Calculate the first look-ahead number of terminals of the sentential-form `S`, example:

``` emacs-lisp
(require 'parser-generator)
(require 'ert)

(parser-generator-set-grammar 
  '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
(parser-generator-set-look-ahead-number 2)
(parser-generator-process-grammar)
(should
  (equal
    '((a) (a c) (a b) (c a) (b a) (e) (c) (b) (c b))
    (parser-generator--first 'S)))
```

### E-FREE-FIRST(S)

Calculate the e-free-first look-ahead number of terminals of sentential-form `S`, if you have multiple symbols the e-free-first will only affect the first symbol, the rest will be treated via the first-function (above). Example:

``` emacs-lisp
(require 'parser-generator)
(require 'ert)

(parser-generator-set-grammar 
  '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
(parser-generator-set-look-ahead-number 2)
(parser-generator-process-grammar)

(should
  (equal
    '((c b) (c a))
    (parser-generator--e-free-first 'S)))
```

### FOLLOW(S)

Calculate the look-ahead number of terminals possibly following S.

``` emacs-lisp
(require 'parser-generator)
(require 'ert)

(parser-generator-set-grammar 
  '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S))
(parser-generator-set-look-ahead-number 2)
(parser-generator-process-grammar)

(should
  (equal
   '((a))
   (parser-generator--follow 'A)))
```

[Back to start](../../../)
