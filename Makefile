main: getdeps compile

.PHONY: main getdeps compile test clean run

getdeps:
	@if ! [ -d deps/hanoidb ] ; then ./rebar get-deps compile ; fi

compile:
	@./rebar compile skip_deps=true

test: compile
	@./rebar eunit skip_deps=true

clean:
	@./rebar clean skip_deps=true

run: compile
	erl \
	-pa ebin \
	-pa deps/hanoidb/ebin \
	-pa deps/snappy/ebin  \
	-pa deps/plain_fsm/ebin \
	-pa deps/sext/ebin \
	-pa deps/lager/ebin \
	-boot start_sasl -eval 'application:start(egresql).'



