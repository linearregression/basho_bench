#!/bin/bash
echo "Start a target server"
TARGET_NODE_NAME=gen_rpc_target@127.0.0.1
TEST_SYS_COOKIE=genrpc

#erl -name ${TARGET_NODE_NAME} -setcookie ${TEST_SYS_COOKIE} -pa ./deps/*/ebin -pa ./deps/gen_rpc/_build/dev/lib/*/ebin --eval 'application:start(gen_rpc)'
# Start Beehive
echo "Starting gen_rpc Bench Test"
eval "erl \
    -name \"${TARGET_NODE_NAME}\" \
    -setcookie \"${TEST_SYS_COOKIE}\" \
    -pa deps/*/ebin -pa .gen_rpc/_build/dev/lib/*/ebin  \
    -eval \"application:start(crypto)\" \
    -eval \"application:start(asn1)\" \
    -eval \"application:start(public_key)\" \
    -eval \"application:start(ssl)\" \
    -eval \"application:start(lager)\" 
    -eval \"application:start(gen_rpc)\""
