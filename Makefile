# See LICENSE file for copyright and license details.

EMACS ?= emacs

all: compile checkdoc

compile:
	$(EMACS) -batch -l make/compile.el

checkdoc:
	$(EMACS) -batch -l make/checkdoc.el

clean:
	rm -f *.elc

.PHONY: all clean
