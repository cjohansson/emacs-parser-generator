# Emacs Parser

The idea of this plugin is to provide functions for various kinds of context-free grammar parsing with support for syntax-directed-translations (SDT) and semantic-actions. This project is about implementing algorithms described in the book `The Theory of Parsing, Translation and Compiling (Volume 1)` by `Alfred V. Aho and Jeffrey D. Ullman` (1972). Also this project is about me learning how to parse languages.

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

## Test

Run in terminal `make clean && make test`
