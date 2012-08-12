test: compile
	erl -pa ebin -boot start_sasl -eval 'egresql_server_listener:start_link(7878).'

compile:
	./rebar compile


