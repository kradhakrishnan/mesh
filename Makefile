SUBDIR	= algo

include ${SUBDIR:%=%/SOURCE}

BIN= $(shell pwd)/bin
ERLC=/usr/bin/erlc
EFLAGS=-W1
DIALYZER=/usr/bin/dialyzer
DIALYZER_OPT=--quiet --plt $(BIN)/mesh.plt

TARGETS=$(SOURCE:%.erl=$(BIN)/%.beam)

setup:
ifeq ($(wildcard $(BIN)/mesh.plt),)
	@echo 'SETUP dialyzer (may take a few mins)'
	@mkdir -p $(BIN)
	@touch $(BIN)/mesh.plt
	@dialyzer $(DIALYZER_OPT) --output_plt $(BIN)/mesh.plt --build_plt --apps erts kernel stdlib crypto mnesia sasl
endif

all: setup $(TARGETS)

clean:
	@echo 'CLEAN'
	@rm -r -f $(BIN)

$(BIN)/%.beam: %.erl
	@echo 'DIALYZER      ' $@
	@mkdir -p $(shell dirname $@)
	@$(DIALYZER) $(DIALYZER_OPT) --src $<
	@echo 'ERLC          ' $@
	@$(ERLC) -W -b beam -o $(shell dirname $@) $(EFLAGS) $(WAIT) $< 
