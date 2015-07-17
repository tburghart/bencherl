%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 

%%
%%  Revised to (hopefully) start and run a bit more efficiently, with the
%%  goal of measuring less overhead and more behavior.
%%
%%  Author: Ted Burghart
%%  Date:   2015-07-16
%%
%%  File:   timer_wheel.erl
%%  Author: Björn-Egil Dahlberg
%%  Date:   2010-09-10
%%

-module(timer_wheel).
-behavior(benchmark).

-export([bench_args/2, run/3]).

% this presumably gets called externally somewhere ...
-export([wheel/1, no_wheel/1]).

% exported so erlang:spawn_link/3 can find it
-export([run_test/3]).

-define(MODES, [wheel, no_wheel]).

% 2^30 milliseconds, a *very* long time
-define(TIMEOUT, 1073741824).

bench_args(Version, Config) ->
    NCores  = benchmark:config_value(Config, number_of_cores),
    BeConf  = benchmark:bench_config(Config),
    Modes   = benchmark:config_value(BeConf, modes, ?MODES),
    Count   = NCores * case Version of
        short ->
            16;
        intermediate ->
            40;
        long ->
            125
    end,
    [[Mode, Count] || Mode <- Modes].

run([wheel, Count | _], Slaves, Config) ->
    test(Count, Slaves, Config, fun recv_loop_after/3);
run([no_wheel, Count | _], Slaves, Config) ->
    test(Count, Slaves, Config, fun recv_loop/3).

wheel(Count) ->
    test(Count, [], [], fun recv_loop_after/3).

no_wheel(Count) ->
    test(Count, [], [], fun recv_loop/3).

test(Count, _Slaves, _Config, RecvLoopFun)
         when erlang:is_integer(Count) andalso Count > 0 ->
    Started = os:timestamp(),
    Manager = self(),
    RunArgs = [Count - 1, RecvLoopFun, Manager],
    Workers = [erlang:spawn_link(?MODULE, run_test, RunArgs)
                || _ <- lists:seq(1, Count)],
    InitMsg = {init, Workers},
    [Worker ! InitMsg || Worker <- Workers],
    [receive {Worker, ready} -> ok end || Worker <- Workers],
    Inited  = os:timestamp(),
    [Worker ! start || Worker <- Workers],
    [receive {Worker, done} -> ok end || Worker <- Workers],
    Elapsed = timer:now_diff(os:timestamp(), Inited),
    [{work_time, Elapsed}, {exec_time,  timer:now_diff(os:timestamp(), Started)}].

run_test(NLoops, RecvLoopFun, Manager) ->
    MyPid = self(),
    Peers = receive
        {init, Workers} ->
            Workers -- [MyPid]
    end,
    Manager ! {MyPid, ready},
    receive
        start ->
            ok
    end,
    loop({MyPid, ping}, {MyPid, pong}, Peers, NLoops, RecvLoopFun, Manager).

loop({MyPid, _}, _, [], 0, _, Manager) ->
    Manager ! {MyPid, done};
loop(Ping, Pong, [], NLoops, RecvLoopFun, Manager) ->
    loop(Ping, Pong, [],
         RecvLoopFun(Pong, undefined, NLoops), RecvLoopFun, Manager);
loop(Ping, Pong, [Peer|Peers], NLoops, RecvLoopFun, Manager) ->
    Peer ! Ping,
    loop(Ping, Pong, Peers,
        RecvLoopFun(Pong, Peer, NLoops), RecvLoopFun, Manager).

recv_loop_after(_, _, 0) ->
    0;
recv_loop_after({MyPid, _} = Pong, Peer, NLoops) ->
    receive
        {Peer, pong} ->
            NLoops;
        {Other, ping} ->
            Other ! Pong,
            recv_loop_after(Pong, Peer, NLoops - 1)
        after ?TIMEOUT ->
            exit(MyPid, kill)
    end.

recv_loop(_, _, 0) ->
    0;
recv_loop(Pong, Peer, NLoops) ->
    receive
        {Peer, pong} ->
            NLoops;
        {Other, ping} ->
            Other ! Pong,
            recv_loop(Pong, Peer, NLoops - 1)
    end.

