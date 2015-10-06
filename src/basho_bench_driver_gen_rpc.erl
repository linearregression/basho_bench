%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 <Your Name>. All Rights Reserved.
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%

%% * Brief: 
%%   Bench test gen_rpc server using one or many gen_rpc clients from multiple erlang nodes.
%%   Each Erlang nodes can run one or more test processes.
%% * More Info:

-module(basho_bench_driver_gen_rpc).
-author("<Your Name> <Your Email>").

%%% Behaviour
-behaviour(basho_bench_driver_behaviour).
-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-define(APP, 'gen_rpc').

%%% Test State
-record(state, { target, %% Id of current test client
                 port, %% connection port
                 workerId, %% Id of test worker same as test Id
                 replies %% Number of Successful Rpc call
               }).

%%% ===================================================
%%% Library interface
%%% ===================================================
new(Id) ->
    ?DEBUG("module=\"~p\" function=new event=test_target_started", [?MODULE]),
    {ok, {?APP, loaded}} = ensure_deps_present(?APP),
    {ok, _Ign} = application:ensure_all_started(?APP),
    {ok, all_process_alive} = ensure_core_process_alive(),
    {ok, {Targets, connected}} = ensure_target_nodes(),
    Port = basho_bench_config:get('gen_rpc_port', undefined),
    Target = choose_test_target(Targets, Port, Id),
    ?DEBUG("module=\"~p\" function=new event=test_target_started target_nide=\"~p\"", [?MODULE, Target]),
    {ok, #state{target=Target, port=Port, workerId=Id, replies=0}}.

run(call, _KeyGen, _ValueGen, State) ->
    TargetNode = State#state.target,
    ?DEBUG("Testing [call] target_node=\"~p\"", [TargetNode]),
    {ok, all_process_alive} = ensure_core_process_alive(),
    case gen_rpc:call(TargetNode , erlang, node) of
        {'error', Reason} -> {error, Reason, State}; 
        {'EXIT', Reason} -> {error, Reason, State};
        {'badrpc', Reason} -> {error, Reason, State};
        {'badtcp', Reason} -> {error, Reason, State};
        _Ign -> Replies = State#state.replies,
                {ok, State#state{replies=Replies+1}}
    end;

run(call_lamda, _KeyGen, _ValueGen, State) ->
    TargetNode = State#state.target,
    ?DEBUG("Testing [call_anonymous] target_node=\"~p\"", [TargetNode]),
    {ok, all_process_alive} = ensure_core_process_alive(),
    case gen_rpc:call(TargetNode , erlang, apply, [fun() -> os:timestamp() end], []) of
        {'error', Reason} -> {error, Reason, State}; 
        {'EXIT', Reason} -> {error, Reason, State};
        {'badrpc', Reason} -> {error, Reason, State};
        {'badtcp', Reason} -> {error, Reason, State};
        _Ign -> Replies = State#state.replies,
                {ok, State#state{replies=Replies+1}}
    end;

 
run(cast, _KeyGen, _ValueGen, State) ->
    TargetNode = State#state.target,
    ?DEBUG("Testing [cast] target_node=\"~p\"", [TargetNode]),
    {ok, all_process_alive} = ensure_core_process_alive(),
    case gen_rpc:cast(TargetNode, erlang, node) of
        {'error', Reason} -> {error, Reason, State}; 
        {'EXIT', Reason} -> {error, Reason, State};
        {'badrpc', Reason} -> {error, Reason, State};
        {'badtcp', Reason} -> {error, Reason, State};
        true -> Replies = State#state.replies,
                {ok, State#state{replies=Replies+1}}
    end;

run(safe_cast, _KeyGen, _ValueGen, State) ->
    TargetNode = State#state.target,
    ?DEBUG("Testing [safe_cast] target_node=\"~p\"", [TargetNode]),
    {ok, all_process_alive} = ensure_core_process_alive(),
    case gen_rpc:safe_cast(TargetNode, erlang, node) of
        {'error', Reason} -> {error, Reason, State}; 
        {'EXIT', Reason} -> {error, Reason, State};
        {'badrpc', Reason} -> {error, Reason, State};
        {'badtcp', Reason} -> {error, Reason, State};
        _Ign -> Replies = State#state.replies,
                {ok, State#state{replies=Replies+1}}
    end.


%%% ===================================================
%%% Test Helpers
%%% ===================================================
ensure_deps_present(App)->
    %% Make sure the path is setup
    case code:which(App) of
        non_existing -> ?FAIL_MSG("module=\"~p\" function=ensure_deps_present event=module_not_found module=\"~p\"", [?MODULE, App]);
        _ -> {ok, {App, loaded}}
    end.

ensure_target_nodes()-> 
    TargetPool   = basho_bench_config:get('gen_rpc_client_target_nodes'),
    Cookie  = basho_bench_config:get('gen_rpc_client_cookie', 'genrpc'),
    BenchmarkNode  = basho_bench_config:get('gen_rpc_client_benchmark', ['genrpc', longnames]),
    {ok, {BenchmarkNode, _}} = spin_up_initiator(BenchmarkNode),
    %% Initialize cookie for each of the nodes
    [begin 
        N0= check_target_node(N),
        true = erlang:set_cookie(N0, Cookie), 
        ok = rpc:call(N0, code, add_pathsz, [code:get_path()]),
        {ok, _SlaveApps} = rpc:call(N0, application, ensure_all_started, [?APP])
     end|| N <- TargetPool],
    %% Try to ping each of the nodes
    ping_each(TargetPool),
    ?INFO("module=\"~p\" function=ensure_target_nodes event=test_target_started cookie=\"~p\"", [?MODULE, Cookie]),
    {ok, {TargetPool, connected}}.

spin_up_initiator(Node)->
    %% Try to spin up net_kernel
    case net_kernel:start([Node, longnames]) of
        {ok, _} ->
            ?INFO("module=\"~p\" function=spin_up_initiator event=benchmark_initiator_up Node=\"~p\"", [?MODULE, node()]),
            {ok, {Node, started}};
        {error,{already_started, _Pid}} ->
            {ok, {Node, already_started}};
        {error, Reason} ->
            ?FAIL_MSG("module=\"~p\" function=spin_up_initiator event=fail_start_benchmark_initiator Reason=\"~p\"", [?MODULE, Reason])
    end.

check_target_node(Node) when Node =:= node() ->
    ?FAIL_MSG("module=\"~p\" function=check_target_node event=test_driver_and_target_same module=\"~p\"", [?MODULE, Node]),
    exit(test_target_is_localnode);
check_target_node(Node) -> Node.

ping_each([]) ->
    ok;
ping_each([Node | Rest]) ->
    case net_adm:ping(Node) of
        pong ->
            ping_each(Rest);
        pang ->
            ?FAIL_MSG("module=\"~p\" function=ping_each event=ping_fail node=\"~p\"", [?MODULE, Node])
    end.

%% Choose a test target to hit to test with different traffic pattern.
choose_test_target(Hosts, Port, WorkerId)->
    %% Choose the node using our ID as a modulus
    TargetHost = lists:nth((WorkerId rem length(Hosts)+1), Hosts),
    ?INFO("module=\"~p\" function=choose_test_target Target=\"~p\" Port=\"~p\" WorkerId=\"~p\"", [?MODULE, TargetHost, Port, WorkerId]),
    TargetHost.

ensure_core_process_alive()->
    true = erlang:is_process_alive(whereis(gen_rpc_server_sup)),
    true = erlang:is_process_alive(whereis(gen_rpc_acceptor_sup)),
    true = erlang:is_process_alive(whereis(gen_rpc_client_sup)),
    true = erlang:is_process_alive(whereis(gen_rpc_dispatcher)),
    {ok, all_process_alive}. 

