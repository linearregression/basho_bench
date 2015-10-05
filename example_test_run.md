## Info
This is a example setup fo using basho bench.
Gen_rpc is the project under test.

##Build the test tool:
This build script is a hack. Basho bench is built under old version of rebar and assumes the old folder structure
call build_test_tool.sh
   -- this calls basho bench make script & call gen_rpc build script
 
##The example setup:
1) one target node: gen_rpc_target@127.0.0.1 for taking all the requests. It must exist first.
   run_test
2) one test node gen_rpc_bench@127.0.0.1 to send out requests

There are other setups configurations. Consule basho tech documentation.

##Get your test results:
make all_results <-- this runs all tests. consult Makefile for other options
make results-browser  <-- spin out browser for pretty plot of test logs

Files of insterest:
./examples -- gen_rpc.config 
./example_script
    --
    --
./tests <-- all the test logs/crash logs etc. Results are in csv file.
Sample test run for a one minute test:
example_script/gen_rpc_sample_run.pdf  <-- jsut a sample plot.

Notes:
Basho bench from this repo differs from that released by Basho Tech of the following:
* basho_bench official release does not support OTP 18 at the time of writing.
  - Several projects that only apply to testing riak are hardcoded (riakc, riak_pb)
  - basho/erlang_protobuffs fails compilation under otp 18. 
    Dependecies like katja,which is ony useful if you want to send result to a reiman server, are now excluded
  - All non core dependencies that relies on basho/erlang_protobuffs are excluded.
  - Added more print outs for better truobleshooting
  - missing a test driver will now alert and halt the tool 
  - product specific test drivers are no longer under src but under plugin.
    They are excluded from default compilation. Just put your test driver under src.



References:
