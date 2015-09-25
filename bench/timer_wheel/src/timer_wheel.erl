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

% use a range of timeouts to scatter them around the wheel
% min should be enought to make sure timeouts don't actually occur
% max is 2^30 milliseconds, a *very* long time
-define(MAX_TIMEOUT, 1073741824).
-define(MIN_TIMEOUT, 9876).
-define(TIMEOUT_RANGE, (?MAX_TIMEOUT - ?MIN_TIMEOUT)).
-define(TIMEOUT_EXIT_THRESHHOLD, 59876).

bench_args(Version, Config) ->
    NCores  = benchmark:config_value(Config, number_of_cores),
    BeConf  = benchmark:bench_config(Config),
    Modes   = benchmark:config_value(BeConf, modes, ?MODES),
    Count   = NCores * case Version of
        short ->
            16;
        intermediate ->
            67;
        long ->
            321
    end,
    [[Mode, Count] || Mode <- Modes].

run([wheel, Count | _], Slaves, Config) ->
    test(Count, Slaves, Config, true);
run([no_wheel, Count | _], Slaves, Config) ->
    test(Count, Slaves, Config, false).

wheel(Count) ->
    test(Count, [], [], true).

no_wheel(Count) ->
    test(Count, [], [], false).

test(Count, _Slaves, _Config, WithTimeouts)
         when erlang:is_integer(Count) andalso Count > 0 ->
    Started = os:timestamp(),
    Manager = self(),
    RunArgs = [Count, Manager, WithTimeouts],
    Workers = start_workers(Count, RunArgs, []),
    InitMsg = {Manager, init, Workers},
    RunMsg  = {Manager, start},
    [Worker ! InitMsg || Worker <- Workers],
    [receive {Worker, ready} -> ok end || Worker <- Workers],
    Inited  = os:timestamp(),
    [Worker ! RunMsg || Worker <- Workers],
    [receive {Worker, done} -> ok end || Worker <- Workers],
    Elapsed = timer:now_diff(os:timestamp(), Inited),
    [{work_time, Elapsed}, {exec_time,  timer:now_diff(os:timestamp(), Started)}].

start_workers(0, _, Workers) ->
    Workers;
start_workers(N, RunArgs, Workers) ->
    start_workers((N - 1), RunArgs,
        [erlang:spawn_link(?MODULE, run_test, RunArgs) | Workers]).

run_test(Count, Manager, WithTimeouts) ->
    MyPid = self(),
    % pre-allocate the timeouts so the RNG time isn't included in measurement
    % this can run while peers are being started ...
    State = case WithTimeouts of
        true ->
            {Rs1, Rs2, Rs3} = os:timestamp(),
            random:seed((Rs1 + erlang:phash2(MyPid)), Rs2, Rs3),
            % shouldn't get *any* timeouts, but add a few spares
            timeout_list((Count + (Count div 9) + 1), []);
        _ ->
            % erlang:length(Peers) *should* be (Count - 1)
            (Count - 1)
    end,
    % ... by now the list of peers should be just about ready
    Peers = receive
        {Manager, init, Workers} ->
            Workers -- [MyPid]
    end,
    Ping = {MyPid, ping},
    Pong = {MyPid, pong},
    Manager ! {MyPid, ready},
    receive
        {Manager, start} ->
            [Peer ! Ping || Peer <- Peers]
    end,
    R = loop(Peers, State, Manager, Pong),
    Manager ! {MyPid, done},
    R.

loop([], _, _, _) ->
    ok;
loop([Peer | RemPeers] = Peers, [TO | RemTOs] = TOs, Manager, {MyPid, _} = Pong) ->
    receive
        {Peer, pong} ->
            loop(RemPeers, RemTOs, Manager, Pong);
        {Other, ping} ->
            Other ! Pong,
            loop(Peers, TOs, Manager, Pong)
    after TO ->
        case continue_after_timeout(TO) of
            true ->
                loop(Peers, RemTOs, Manager, Pong);
            _ ->
                exit(MyPid, kill)
        end
    end;
loop([Peer | RemPeers] = Peers, NLoops, Manager, Pong) ->
    receive
        {Peer, pong} ->
            loop(RemPeers, (NLoops - 1), Manager, Pong);
        {Other, ping} ->
            Other ! Pong,
            loop(Peers, NLoops, Manager, Pong)
    end.

timeout_list(0, Timeouts) ->
    Timeouts;
timeout_list(Count, Timeouts) ->
    timeout_list((Count - 1),
        [(?MIN_TIMEOUT + random:uniform(?TIMEOUT_RANGE)) | Timeouts]).

continue_after_timeout(Timeout) ->
    Timeout < ?TIMEOUT_EXIT_THRESHHOLD.

