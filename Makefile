REBAR = ./rebar3

.PHONY: cv
.PHONY: all compile test clean upgrade
.PHONY: test eunit xref dialyze
.PHONY: release release_minor release_major release_patch

all: compile

cv:
	pandoc -V geometry:margin=3cm -o priv/static/cv.pdf priv/static/cv.md 

compile:
	@$(REBAR) compile

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

test: compile xref eunit

eunit: all
	ERL_FLAGS="-sname eunit" $(REBAR) eunit
	@$(REBAR) cover

xref: all
	@$(REBAR) xref

dialyze:
	@$(REBAR) dialyzer

upgrade:
	@$(REBAR) upgrade

release_major: test
	./bin/release major

release_minor: test
	./bin/release minor

release_patch: test
	./bin/release patch

release: relase_patch

