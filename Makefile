# See LICENSE file for copyright and license details.

EMACS ?= emacs
LOAD = -l architect.el -l test/architect-test.el

all: compile test checkdoc

test:
	$(EMACS) -batch $(LOAD) -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -batch -l make/compile.el

checkdoc:
	$(EMACS) -batch -l make/checkdoc.el

clean:
	rm -f *.elc

.PHONY: all test clean
