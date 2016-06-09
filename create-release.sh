#!/usr/bin/bash

# Creating a node condig
cd rel
../rebar create-node nodeid=wallnode

cd ..

# Generate the release
./rebar generate
