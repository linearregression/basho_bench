%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%

%% * Brief: 
%%   Bench test gen_rpc server using one or many gen_rpc clients from multiple erlang nodes.
%%   Each Erlang nodes can run one or more test processes.
%% * More Info:

-module(basho_bench_driver_gen_rpc).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(basho_bench_driver_behaviour).
-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-define(APP, 'gen_rpc').

%%% Test State
-record(state, { client,
                 bucket,
                 replies }).

%%% ===================================================
%%% Library interface
%%% ===================================================
new(Id) ->



%%% ===================================================
%%% Test Helpers
%%% ===================================================
ensure_deps_present(App)->
    %% Make sure the path is setup
    case code:which(App) of
        non_existing -> ?FAIL_MSG("function=ensure_deps_present event=module_not_found module=\"~p\"", [App]);
        _ -> {ok, {App, loaded}
    end.

ensure_target_nodes()-> 
    Nodes   = basho_bench_config:get(gen_rpc_client_nodes),
    Cookie  = basho_bench_config:get(gen_rpc_client_cookie, 'gen_rpc'),
    MyNode  = basho_bench_config:get(gen_rpc_client_mynode, [basho_bench, longnames]),
    TestTargets = choose_test_target(Hosts, Port, WorkerId)
    %% Initialize cookie for each of the nodes
    [begin 
        N0 = check_target_node(N),
        true = erlang:set_cookie(N0, Cookie),   
        {ok, _SlaveApps} = rpc:call(N0, application, ensure_all_started, [?APP]),     
     end|| N <- Nodes],
    %% Try to ping each of the nodes
    ping_each(Nodes).

check_target_node(Node) when Node =:= node() ->
    ?FAIL_MSG("function=check_target_node event=test_driver_and_target_same module=\"~p\"", [Node]).
    exit(test_target_is_localnode);
check_target_node(Node) -> Node.

ping_each([]) ->
    ok;
ping_each([Node | Rest]) ->
    case net_adm:ping(Node) of
        pong ->
            ping_each(Rest);
        pang ->
            ?FAIL_MSG("function=ping_each event=ping_fail node=\"~p\"", [Node])
    end.

%% Choose a test target to hit to test with different traffic pattern.
choose_test_target(Hosts, Port, WorkerId)->
    %% Choose the node using our ID as a modulus
    TargetHost = lists:nth((WorkerId rem length(Hosts)+1), Hosts),
    ?INFO("function=choose_test_target s:~p for worker ~p\n", [TargetHost, Port, WorkerId]).
    TargetHost.

