#!/bin/bash
echo on
echo "Start a target server"
TARGET_NODE_NAME=gen_rpc_benchmark@127.0.0.1
TEST_SYS_COOKIE=genrpc
rm -rf ./tests/*
./basho_bench -N ${TARGET_NODE_NAME} -C ${TEST_SYS_COOKIE} ./examples/gen_rpc.config
