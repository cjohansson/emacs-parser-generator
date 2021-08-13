# Emacs Parser Generator

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/cjohansson/emacs-parser-generator.svg?branch=master)](https://travis-ci.org/cjohansson/emacs-parser-generator)

The idea of this plugin is to provide functions for various kinds of context-free grammar parser generations with support for syntax-directed-translations (SDT) and semantic actions (SA) and the possibility of exporting parsers and translators (as generated elisp code) to enable Emacs plugin-agnostic usage. This project is also about implementing algorithms described in the book `The Theory of Parsing, Translation and Compiling (Volume 1)` by `Alfred V. Aho and Jeffrey D. Ullman` (1972). Additional this project is about me learning how to parse programming languages.

## Lexical Analysis

We use a regular-language based lexical analyzer that can be implemented by a finite-state-machine (FSM). Read more [here](docs/Lexical-Analysis.md).

## Syntax Analysis / Parser

We use deterministic push down transducer (PDT) based algorithms. Read more [here](docs/Syntax-Analysis.md).

## Test

Run in terminal `make clean && make tests && make compile`
