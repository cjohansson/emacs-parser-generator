# TODO / Roadmap / Issues

## Main

Functions (with validations) to set global variables:

* parser-generator--context-sensitive-attributes
* parser-generator--global-attributes
* parser-generator--global-declaration

## Lex-analyzer

* State-based lex-analyzer
* Verify that parser-generator-lex-analyzer--index is used in exported lex-analyzers
* Verify that parser-generator-lex-analyzer--state-init is used in exported lex-analyzers
* Use buffer when lexing in case more tokens are return than what are needed
* Use buffer when lexing in exported parsers as well

## LR-Parser

Functions (with validations) to set global variables:

* parser-generator-lr--context-sensitive-precedence-attribute
* parser-generator-lr--global-precedence-attributes

[Back to start](../../)
