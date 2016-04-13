#!/bin/bash
echo "In erlang console please write the following:"
echo "Please run application:start(wall)."

erl -pa /ebin /deps/*/ebin
