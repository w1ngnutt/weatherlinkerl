all: compile

compile:
	rebar co

clean:
	rebar clean

test:
	rebar compile eunit
