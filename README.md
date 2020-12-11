# Emacs Parser

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/cjohansson/emacs-parser.svg?branch=master)](https://travis-ci.org/cjohansson/emacs-parser)

The idea of this plugin is to provide functions for various kinds of context-free grammar parsing with support for syntax-directed-translations (SDT) and semantic-actions. This project is about implementing algorithms described in the book `The Theory of Parsing, Translation and Compiling (Volume 1)` by `Alfred V. Aho and Jeffrey D. Ullman` (1972). Also this project is about me learning how to parse languages.

This is just started, so most stuff are WIP.

## Lexical Analysis

We use a regular-language based lexical analyzer that can be implemented by a finite-state-machine (FSM).

## Syntax Analysis / Parsing

We use push down transducer (PDT) based algorithms:

### With Backtracking
#### The Bottom-Up Parsing Algorithm
#### The Top-Down Parsing Algorithm
#### The Cocke-Younger-Kasami Algorithm
#### The Parsing Method of Earley
### Without Backtracking
#### LL(k)
#### Deterministic Shift-Reduce Parsing
#### LR(k)
#### Deterministic Right Parser for LR(k) Grammars

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

### LR items for prefix (S)

Calculate the set of LR items valid for any viable prefix S.

### Functions

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

#### Formal Shift-Reduce Parsing Algorithms
#### Simple Precedence Grammars
#### Extended Precedence Grammars
#### Weak Precedence Grammars
#### Bounded-Right-Context Grammars
#### Mixed Strategy Precedence Grammars
#### Operator Precedence Grammars
#### Floyd-Evans Production Language

## Grammar

Grammar consists of `N`, `T`, `P` and `S`, where `N` is non-terminals, `T` is terminals, `P` is productions and `S` is start-production. Example:

* N = `'(S A B C)`
* T = `'(a b c)`
* P = `'((S (A B)) (A (B a) e) (B (C b) C) (C c e))`
* S = `'S`

``` emacs-lisp
(parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
```

### e

The symbol defined in variable `parser--e-identifier`, with default-value: 'e`, symbolizes the e symbol. The symbol is allowed in some grammars and not in others.

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

Is a simple integer above zero. You set it like this: `(parser--set-look-ahead-number 1)` for `1` number look-ahead.

### Syntax-directed-translation (SDT)

*WIP* Where should this be defined?

### Semantic-actions (SA)

*WIP* Where should this be defined?

## Functions

### FIRST(S)

Calculate the first look-ahead number of terminals of the sentential-form `S`, example:

``` emacs-lisp
(require 'ert)

(parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
(parser--set-look-ahead-number 2)
(parser--process-grammar)

(should
  (equal
    '((a) (a c) (a b) (c a) (b a) (e) (c) (b) (c b))
    (parser--first 'S)))
```

### E-FREE-FIRST(S)

Calculate the e-free-first look-ahead number of terminals of sentential-form `S`, example:

``` emacs-lisp
(require 'ert)

(parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S))
(parser--set-look-ahead-number 2)
(parser--process-grammar)

(should
  (equal
    '((c b) (c a))
    (parser--e-free-first 'S)))
```

### FOLLOW(S)

Calculate the look-ahead number of terminals possibly following S.

``` emacs-lisp
(require 'ert)

(parser--set-grammar '((S A B) (a c d f) ((S (A a)) (A B) (B (c f) d)) S))
(parser--set-look-ahead-number 2)
(parser--process-grammar)

(should
  (equal
   '((a))
   (parser--follow 'A)))
```

## Test

Run in terminal `make clean && make tests && make compile`
