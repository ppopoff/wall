#!/bin/bash

# clean compile before run
./rebar clean compile eu

# -s runs function: -s $MODULE $RUN
# -pa CLASSPATH
erl -pa ebin deps/ranch/ebin deps/goldrush/ebin deps/lager/ebin -s wall run

