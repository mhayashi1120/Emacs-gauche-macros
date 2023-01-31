-include env.mk

LOAD_PATH = -L .
LOAD_PATH += $(POPUP_EL_PATH)

EMACS ?= emacs
BATCH := $(EMACS) -Q -batch $(LOAD_PATH)

EL = gauche-macros.el
ELC := $(EL:%.el=%.elc)

all: compile

check: compile
	$(BATCH) -l gauche-macros-unbound.el
	$(BATCH) $(EL:%=-l %) -l gauche-macros-test.el \
		-f ert-run-tests-batch-and-exit
	$(BATCH) $(ELC:%=-l %) -l gauche-macros-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile $(EL)

clean:
	rm -f $(ELC)
