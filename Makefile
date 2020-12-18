EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif
EMACS_CMD := $(EMACS) -Q -batch -L . -L test/

EL  := parser-generator.el parser-generator-lex-analyzer.el parser-generator-lr.el test/parser-generator-test.el test/parser-generator-lex-analyzer-test.el test/parser-generator-lr-test.el
ELC := $(EL:.el=.elc)

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONY: compile
compile:
	$(EMACS_CMD) -f batch-byte-compile $(EL)

.PHONY: test
test:
	$(EMACS_CMD) -l test/parser-generator-test.el -f "parser-generator-test"

.PHONY: test-lex-analyzer
test-lex-analyzer:
	$(EMACS_CMD) -l test/parser-generator-lex-analyzer-test.el -f "parser-generator-lex-analyzer-test"

.PHONY: test-lr
test-lr:
	$(EMACS_CMD) -l test/parser-generator-lr-test.el -f "parser-generator-lr-test"

.PHONY: tests
tests: test test-lex-analyzer test-lr
