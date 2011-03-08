.PHONY: deps test clean doc rel shell

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

test:
	@./rebar eunit skip_deps=true

analyze:
	@./rebar analyze skip_deps=true

doc:
	@./rebar doc skip_deps=true

rel:
	@./rebar generate

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

shell:
	erl -pa ebin deps/*/ebin -boot start_sasl -s chef_otto start0
