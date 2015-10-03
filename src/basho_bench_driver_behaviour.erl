%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%

-module(basho_bench_driver_behaviour).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%% R15 and later
-callback new(any()) -> {ok, any()}. 
-callback run(atom, any(), any(), any()) -> {ok, any()} | {error, term(), term()}.




