EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif
EMACS_CMD := $(EMACS) -Q -batch -L . -L test/

EL  := parser.el test/parser-test.el
ELC := $(EL:.el=.elc)

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONY: compile
compile:
	$(EMACS_CMD) -f batch-byte-compile $(EL)

.PHONY: test
test:
	$(EMACS_CMD) -l test/parser-test.el -f "parser-test"

.PHONY: test-lr
test-lr:
	$(EMACS_CMD) -l test/parser-lr-test.el -f "parser-lr-test"

.PHONY: tests
tests: test test-lr
