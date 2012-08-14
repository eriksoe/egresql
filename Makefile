main: compile

test: compile
	erl -pa ebin -boot start_sasl -eval 'application:start(egresql).'

compile:
	./rebar compile skip_deps=true


