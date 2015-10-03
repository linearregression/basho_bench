%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%

%% * Set ups one or many gen_rpc clients to hit on one or many target gen_rpc server.
%% * Result collect back to gen_rpc_master for logs

-module(basho_bench_driver_gen_rpc).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(basho_bench_driver_behaviour).
-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-define(APP, 'gen_rpc').

%%% Library interface
%%-export([call/3, call/4, call/5, call/6, cast/3, cast/4, cast/5]).

%%% ===================================================
%%% Library interface
%%% ===================================================
ensure_deps_present(App)->
    %% Make sure the path is setup
    case code:which(App) of
        non_existing -> ?FAIL_MSG("Module=\"~p\" function=ensure_deps_present event=module_not_found module=\"~p\"", [?MODULE, App]);
        _ -> {ok, {App, loaded}
    end.

ensure_target_nodes()-> 
    Nodes   = basho_bench_config:get(gen_rpc_client_nodes),
    Cookie  = basho_bench_config:get(gen_rpc_client_cookie, 'gen_rpc'),
    MyNode  = basho_bench_config:get(gen_rpc_client_mynode, [basho_bench, longnames]),

start_slave() ->
    %% Starting a slave node with Distributed Erlang
    {ok, _Slave} = slave:start(?SLAVE_IP, ?SLAVE_NAME, "+K true"),
    ok = rpc:call(?SLAVE, code, add_pathsz, [code:get_path()]),
    %% Start the application remotely
    {ok, _SlaveApps} = rpc:call(?SLAVE, application, ensure_all_started, [gen_rpc]),
    ok.

