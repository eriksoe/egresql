main: getdeps compile

getdeps:
	@if ! [ -d deps/hanoidb ] ; then ./rebar get-deps compile ; fi

compile:
	@./rebar compile skip_deps=true

test: compile
	erl -pa ebin -boot start_sasl -eval 'application:start(egresql).'



