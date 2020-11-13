# Emacs Parser

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/cjohansson/emacs-parser.svg?branch=master)](https://travis-ci.org/cjohansson/emacs-parser)

The idea of this plugin is to provide functions for various kinds of context-free grammar parsing with support for syntax-directed-translations (SDT) and semantic-actions. This project is about implementing algorithms described in the book `The Theory of Parsing, Translation and Compiling (Volume 1)` by `Alfred V. Aho and Jeffrey D. Ullman` (1972). Also this project is about me learning how to parse languages.

This is just started, so most stuff are WIP.

## Lexical Analysis

We use a regular-language based lexical analyzer that can be implemented by a finite-state-machine (FSM).

## Syntax Analysis / Parsing

We use push down transducer (PDT) based algorithms:

# With Backtracking
## The Bottom-Up Parsing Algorithm
## The Top-Down Parsing Algorithm
## The Cocke-Younger-Kasami Algorithm
## The Parsing Method of Earley
# Without Backtracking
## LL(k)
## Deterministic Shift-Reduce Parsing
## LR(k)
## Deterministic Right Parser for LR(k) Grammars
## Formal Shift-Reduce Parsing Algorithms
## Simple Precedence Grammars
## Extended Precedence Grammars
## Weak Precedence Grammars
## Bounded-Right-Context Grammars
## Mixed Strategy Precedence Grammars
## Operator Precedence Grammars
## Floyd-Evans Production Language

## Grammar

Grammar consists of `N`, `T`, `P` and `S`, where `N` is non-terminals, `T` is terminals, `P` is productions and `S` is start-production. N, T, P consists of lists of one or more strings and symbols. When initializing grammar you also set the number of look-ahead to use, like this:

* N = (S A B C)
* T = (a b c)
* P = ((S (A B)) (A (B a) e) (B (C b) C) (C c e))
* S = S

```elisp
(parser--set-grammar '((S A B C) (a b c) ((S (A B)) (A (B a) e) (B (C b) C) (C c e)) S) 2)
```

### Non-terminals

A non-terminal is either a symbol or a string so `"A"` and `A` are equally valid.

### Terminals

A terminal is either a symbol or a string so `"{"` and `A` are equally valid.

### Productions

A production consists of a list of at least two elements. The first element is the left-hand-side (LHS) and should contain at least one element. The right-hand-side (RHS) consists of the rest of the elements, if there is more than one list in RHS then each list will be treated as a alternative production RHS.

Example, production `S -> A | B` is defined as:

``` elisp
(S A B)
```

Another example, production `S -> IF "{" EXPRESSION "}" | EXIT` is declared as:

```elisp
(S (IF "{" EXPRESSION "}") EXIT)
```

### Look-ahead number

Is a simple integer above zero.

### Start

The start symbol is either a string or a symbol and should exists in the list of productions as the LHS.

## Test

Run in terminal `make clean && make tests && make compile`
