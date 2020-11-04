# Emacs Parser

The idea of this plugin is to provide functions for various kinds of context-free grammar parsing. This project is about implementing algorithms described in the book `The Theory of Parsing, Translation and Compiling (Volume 1)`.

## Lexical Analysis

We use a regular-language based lexical analyzer that can be implemented by a finite-state-machine (FSM).

WIP

## Syntax Analysis / Parsing

We use a deterministic push down transducer (DPDT) based algorithms.

### Top-down
#### With backtracking
#### Without backtracking
### Bottom-up
#### With backtracking
#### Without backtracking
##### LL(k)
##### LR(k)
##### LALR(k)

## Test

Run in terminal `make clean && make test`
