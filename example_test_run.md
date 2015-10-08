## Info
This is a example setup fo using basho bench.
Gen_rpc is the project under test.

##Build the test tool:
* build_test_tool.sh (calls basho bench make script & call gen_rpc build script).
* This build script is a hack. Basho bench is built under old version of rebar doesn't fully recognise dependency built from rebar3 folder structure (e.g. gen_rpc).
* specifiy your test driver in basho_bench.app (check basho_bench.app.src for example).

##The example setup:
* one target node: gen_rpc_target@127.0.0.1 for taking all the requests. It must exist first.
call run_target.sh 
* one test node gen_rpc_bench@127.0.0.1 to send out requests.
call run_test.sh 

There are other setups configurations. Consult basho tech documentation.

##Get your test results:
* make all_results  (this runs all tests. consult Makefile for other options)
* make results-browser  (spin out browser for pretty plot of test logs)
** Other options availble in Makefile

## Other Files/Folders of insterest:
* examples/gen_rpc.config (your test configuration)
* example_script
  - gen_rpc_sample_run.pdf
  - run_target.sh
  - run_test.sh
  - gen_rpc_sample_run.pdf (sample test result plot for one minute run)
* tests ( test logs/crash logs etc. Results are in csv file.)

## Notes:
* Basho bench from this repo differs from that released by Basho Tech of the following:
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
