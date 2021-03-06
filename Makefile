SUBDIR	= core

include ${SUBDIR:%=%/SOURCE}

BIN= $(shell pwd)/bin
ERLC=/usr/bin/erlc
EFLAGS=-W1
DIALYZER=/usr/bin/dialyzer
DIALYZER_OPT=--quiet --plt $(BIN)/mesh.plt

# Elixir
ELIXIRC=/usr/bin/elixirc

TARGETS=$(SOURCE:%.erl=%.beam) $(SOURCE:%.ex=%.beam)

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

.SUFFIXES: .erl .ex .beam

.erl.beam:
	@mkdir -p $(shell dirname $(BIN)/$@)
	@echo 'ERLC       ' $<
	@$(ERLC) -W -b beam -o $(shell dirname $(BIN)/$@) $(EFLAGS) $(WAIT) $< 
	@echo 'DIALYZER   ' $<
	@$(DIALYZER) $(DIALYZER_OPT) --src $<
.ex.beam:
		@mkdir -p $(shell dirname $(BIN)/$@)
		@echo 'ELIXIRC		' $<
		@$(ELIXIRC) -o $(shell dirname $(BIN)/$@) $(WAIT) $<
		@echo 'DIALYZER      ' $@
		@$(DIALYZER) $(DIALYZER_OPT) $(shell dirname $(BIN)/$@)/*.beam

