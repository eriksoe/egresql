main: getdeps compile

getdeps:
	@if ! [ -d deps/hanoidb ] ; then ./rebar get-deps compile ; fi

compile:
	@./rebar compile skip_deps=true

test: compile
	erl \
	-pa ebin \
	-pa deps/hanoidb/ebin \
	-pa deps/snappy/ebin  \
	-pa deps/plain_fsm/ebin \
	-pa deps/sext/ebin \
	-pa deps/lager/ebin \
	-boot start_sasl -eval 'application:start(egresql).'



