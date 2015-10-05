#!/bin/bash
echo on
rm -rf ./tests/*
./basho_bench -N gen_rpc_benchmark@127.0.0.1 -C gen_rpc ./examples/gen_rpc.config
