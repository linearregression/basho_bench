#!/bin/bash
echo "Start a target server"
TARGET_NODE_NAME=gen_rpc_target@127.0.0.1
TEST_SYS_COOKIE=genrpc
MY_APP=gen_rpc_app
erl -name ${TARGET_NODE_NAME} -setcookie ${TEST_SYS_COOKIE} -pa ./deps/*/ebin -pa ./deps/gen_rpc/_build/dev/lib/*/ebin -s ${MY_APP}
