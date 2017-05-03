REBAR=rebar

all: compile-all

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

compile-all:
	$(REBAR) compile --recursive

check: test itest
test: eunit
itest: ct

eunit: compile
	mkdir -p logs
	env ERL_LIBS=deps ERL_AFLAGS='-config test/sys -s lager' $(REBAR) eunit skip_deps=true verbose=1

ct: compile
	mkdir -p logs
	env ERL_LIBS=deps ERL_AFLAGS='-config test/sys -s lager' $(REBAR) ct skip_deps=true $(CT_ARGS) || grep Testing logs/raw.log

doc:
	$(REBAR) doc

clean: clean-itest
	$(REBAR) clean

clean-all: clean-itest
	$(REBAR) clean --recursive

.PHONY: all deps compile compile-all check test itest eunit ct doc clean clean-all clean-itest


