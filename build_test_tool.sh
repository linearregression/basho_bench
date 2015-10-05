#!/bin/bash
echo "Basho Bench does not support rebar3. This is a hack"
echo "It will first build the tool then gen_rpc which built with rebar3"
make && (cd ./deps/gen_rpc && make all && cd ../..)
