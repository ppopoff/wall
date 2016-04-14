#!/bin/bash
erl -pa ebin deps/ranch/ebin deps/goldrush/ebin deps/lager/ebin

# not working
# erl -pa ebin deps/*/ebin
