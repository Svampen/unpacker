all:
	./rebar3 get-deps
	./rebar3 compile
	./rebar3 escriptize
