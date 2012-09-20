all: get-deps compile xref eunit

compile:
	@./rebar compile

get-deps:
	@./rebar get-deps

xref:
	@./rebar xref

clean:
	@./rebar clean

distclean:
	@./rebar delete-deps
	clean

eunit:
	@./rebar eunit skip_deps=true

edoc:
	@./rebar doc

