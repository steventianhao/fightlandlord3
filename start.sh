#!/bin/sh
erl -pa ebin deps/*/ebin -s fll3 \
	-eval "io:format(\"Run: telnet localhost 7070~n\")."