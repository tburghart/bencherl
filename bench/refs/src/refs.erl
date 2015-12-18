%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(refs).
-behavior(benchmark).

-export([bench_args/2, run/3]).
% spawned proc
-export([worker/5]).

-define(MODES, [make_ref]).

-define(MRC_ATOM, milrefs_per_core).
-define(MRC_DFLT, 10).

version_multiplier(short) -> 1;
version_multiplier(long) -> 7;
version_multiplier(_) -> 3.

%% -------------------------------------------------------------------
%% Config & Dispatch
%% -------------------------------------------------------------------

-spec bench_args(
    Version :: benchmark:bench_vers(),
    Config  :: benchmark:bench_conf())
        -> [benchmark:named_args()].

bench_args(Version, Config) ->
    NCores  = benchmark:config_value(Config, number_of_cores),
    BeConf  = benchmark:bench_config(Config),
    Modes   = benchmark:config_value(BeConf, modes, ?MODES),
    MRpC    = benchmark:config_value(BeConf, ?MRC_ATOM, ?MRC_DFLT),
    MRefs   = NCores * MRpC * version_multiplier(Version),
    Args    = [[Mode, MRefs] || Mode <- Modes],
    [{bench_label(A), A} || A <- Args].

bench_label([Mode, MRefs]) ->
    {text, lists:flatten(io_lib:format(
        "[~s,~bM]", [benchmark:clean_label(Mode), MRefs]))}.

-spec run(
    Args    :: benchmark:plain_args(),
    Slaves  :: benchmark:slaves(),
    Config  :: benchmark:bench_conf())
        -> [benchmark:result_time()].

run([Mode, MRefs], Slaves, Config) ->
    NScheds = erlang:system_info(schedulers),
    Started = os:timestamp(),
    WorkTm  = run_test(Mode, NScheds, (MRefs * 1000000), Slaves, Config),
    ExecTm  = timer:now_diff(os:timestamp(), Started),
    [{work_time, WorkTm}, {exec_time, ExecTm}].

%% -------------------------------------------------------------------
%% make_ref mode
%% -------------------------------------------------------------------

run_test(make_ref = Mode, NWorkers, TotRefs, _Slaves, _Config) ->
    RpP     = (TotRefs div NWorkers),
    Rem     = (TotRefs rem NWorkers),
    MgrPid  = self(),
    MgrRef  = make_ref(),
    Workers = spawn_workers(NWorkers, Mode, MgrRef, MgrPid, RpP, []),
    Start   = {MgrRef, start},
    ready   = recv_message(ready, Workers),
    Begin   = os:timestamp(),
    Start   = send_message(Start, Workers),
    done    = make_refs(Rem),
    Results = recv_results(Workers),
    Done    = os:timestamp(),
    true    = lists:all(fun(Elem) -> Elem =:= done end, Results),
    timer:now_diff(Done, Begin).

worker(make_ref, MyRef, MgrRef, MgrPid, Count) ->
    MgrPid ! {MyRef, ready},
    receive {MgrRef, start} -> ok end,
    MgrPid ! {MyRef, make_refs(Count)},
    ok.

%% -------------------------------------------------------------------
%% Test operations
%% -------------------------------------------------------------------

%
% structured so the compiler won't just optimize it all away - need to do
% *something* with the returned refs
%

make_refs(0) ->
    done;
make_refs(Count) ->
    make_refs(Count, Count, 0).

make_refs(_, Ref, Ref) ->
    duplicate_ref;
make_refs(0, _, _) ->
    done;
make_refs(Count, _, Last) ->
    make_refs((Count - 1), Last, make_ref()).

%% -------------------------------------------------------------------
%% Worker processes
%% -------------------------------------------------------------------

send_message(Message, []) ->
    Message;
send_message(Message, [{_, Pid} | Workers]) ->
    Pid ! Message,
    send_message(Message, Workers).

recv_results(Workers) ->
    recv_results(Workers, []).

recv_results([], Results) ->
    Results;
recv_results([{Ref, _} | Workers], Results) ->
    Result = receive
        {Ref, Token} ->
            Token
    end,
    recv_results(Workers, [Result | Results]).

recv_message(Token, []) ->
    Token;
recv_message(Token, [{Ref, _} | Workers]) ->
    receive
        {Ref, Token} ->
            ok
    end,
    recv_message(Token, Workers).

spawn_workers(0, _, _, _, _, Workers) ->
    Workers;
spawn_workers(N, Mode, MgrRef, MgrPid, Count, Workers) ->
    Ref = make_ref(),
    Pid = spawn_link(?MODULE, worker, [Mode, Ref, MgrRef, MgrPid, Count]),
    spawn_workers((N - 1),
        Mode, MgrRef, MgrPid, Count, [{Ref, Pid} | Workers]).

