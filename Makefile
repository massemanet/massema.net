all: deps
	./rebar compile

deps: deps/egeoip

deps/%:
	./rebar get-deps

clean:
	./rebar clean
