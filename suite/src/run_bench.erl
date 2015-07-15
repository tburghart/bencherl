%% The benchmark executor.
%% The executor is responsible for starting the necessary slave nodes, for
%% running a benchmark and for stopping the slaves, after the execution of the
%% benchmark is over.

-module(run_bench).

-export([main/0, main/1]).

-include_lib("kernel/include/inet.hrl").

-spec main() -> ok | no_return().

main() ->
    ConfigFile = case os:getenv("BENCHERL_BECONFIG") of
        false ->
            case os:getenv("BENCHERL_ROOT") of
                false ->
                    error_exit('BadConfig',
                        "No suitabe environment variable present");
                Dir ->
                    Dir ++ "/scratch/run_bench.conf"
            end;
        File ->
            File
    end,
    main(ConfigFile).

-spec main(ConfigFile :: file:filename()) -> ok | no_return().

main(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Config} ->
            run_bench(Config);
        {error, {_, _, _} = Reason} ->
            error_exit('ConfigFileFormat', Reason);
        {error, IO} ->
            error_exit(IO, ConfigFile)
    end.

-spec error_exit(
    Error   :: atom() | string(),
    Reason  :: term())
        -> no_return().

error_exit(Error, Reason) ->
    io:format(standard_error, "Error '~s': ~p~n", [Error, Reason]),
    erlang:halt(1).

-spec error_exit(
    Error   :: atom() | string(),
    Reason  :: term(),
    Trace   :: [tuple()])
        -> no_return().

error_exit(Error, Reason, Trace) ->
    io:format(standard_error,
        "Exception '~s':~n~p~n~p~n", [Error, Reason, Trace]),
    erlang:halt(1).

-spec run_bench(
    Config  :: [benchmark:config_rec()])
        -> ok | no_return().

run_bench(Config) ->
    try
        Bench   = benchmark:config_value(Config, bench),
        Version = benchmark:config_value(Config, version, short),
        OutForm = benchmark:config_value(Config, output_format, 'avg_min_max'),
        NSlaves = benchmark:config_value(Config, number_of_slaves, 0),
        NScheds = benchmark:config_value(Config, number_of_schedulers),
        NIters  = benchmark:config_value(Config, iterations, 1),
        NCores  = benchmark:config_value(Config, number_of_cores),

        MeasureCount = case benchmark:config_value(Config, what) of
            node  ->
                NSlaves;
            sched ->
                NScheds
        end,

        % the constant Config parameter to benchmark:run/3
        % set it up before we start doing any heavy lifting in case any
        % required values are missing from the input configuration
        RunBenchConf = [
            {datadir, benchmark:config_value(Config, datadir)},
            {master, benchmark:config_value(Config, master)},
            {schedulers, NScheds}
        ],

	% open the output files
        % again, fail early if config values are missing or invalid
        {ok, Measurements} = file:open(
            benchmark:config_value(Config, measfile), [append]),
        {ok, OutputSink} = file:open(
            benchmark:config_value(Config, outfile), [write]),

        % start the slaves if necessary, and store the function to stop them
        % this is the last consumer of values from Config
        {Slaves, StopSlaves} = init_slaves(NSlaves, Config),

        %
	% function to run the benchmark once and send back results
        %
        RunBenchFunc = fun(Coordinator, CurArgs) ->
            group_leader(OutputSink, self()),
            Started = os:timestamp(),
            Result  = Bench:run(CurArgs, Slaves, RunBenchConf),
            Micros  = timer:now_diff(os:timestamp(), Started),
            Coordinator ! {done, {Result, Micros}}
        end,

        %
        % function called once per configuration to run the benchmark
        %
        RunBench = fun(BenchArgs) ->
            %
            % function called once per iteration, possibly recursively -
            % recursive behavior occurs via benchmark results that aren't
            % otherwise documented
            %
            RunAndAggregate = fun(RecursiveRAA, CurrentState, Results) ->
                Coordinator = erlang:self(),
                % In a new process, please.
                SpawnedBenchFunc = case CurrentState of
                    initial ->
                        fun() ->
                            RunBenchFunc(Coordinator, BenchArgs)
                        end;
                    {state, SBState} ->
                        fun() ->
                            RunBenchFunc(Coordinator, [SBState | BenchArgs])
                        end
                end,
                erlang:spawn(erlang:node(), SpawnedBenchFunc),
                % handle {done, {Result, Duration}} messages from RunBenchFunc
                % this handles a bunch of undocumented results, too
                receive
                    {done, {{{continue, ignore}, BenchState}, _}} ->
                        RecursiveRAA(
                            RecursiveRAA, {state, BenchState}, Results);
                    {done, {{{continue, Name}, BenchState}, Duration}} ->
                        RecursiveRAA(
                            RecursiveRAA, {state, BenchState},
                            orddict:append(Name, Duration, Results));
                    {done, {{{done, ignore}, _}, _}} ->
                        Results;
                    {done, {{{done, Name}, _}, Duration}} ->
                        orddict:append(Name, Duration, Results);
                    % this is the documented result from benchmark:run/3
                    {done, {_, Duration}} ->
                        orddict:append(standard, Duration, Results)
                end
            end,

            % run the benchmark Iterations times ...
            RunTimes = lists:foldl(
                fun(_Iteration, Results) ->
                    RunAndAggregate(RunAndAggregate, initial, Results)
                end, orddict:new(), lists:seq(1, NIters)),

            % ... and write out the results
            case orddict:to_list(RunTimes) of
                [{standard, Times}] ->
                    record_times(
                        Measurements, OutForm, MeasureCount,
                        BenchArgs, Times);
                TimeRecs ->
                    lists:foreach(
                        fun({Name, Times}) ->
                            record_times(
                                Measurements, OutForm, MeasureCount,
                                {Name, BenchArgs}, Times)
                        end, TimeRecs)
            end
        end,

        % run the benchmark with each returned configuration
        lists:foreach(
            RunBench,
            Bench:bench_args(
                Version, [{number_of_cores, NCores}, {slaves, Slaves}])),

	% close the measurements file
        file:close(Measurements),

	% stop the slaves, as appropriate.
        StopSlaves(),

        % close the output sink AFTER the slaves are cleaned up, in case any
        % of them got redirected to it
        file:close(OutputSink)

    catch
        Error:Reason ->
            error_exit(Error, Reason, erlang:get_stacktrace())
    end.

-spec init_slaves(
    NSlaves :: non_neg_integer(),
    Config  :: [benchmark:config_rec()])
        -> {[benchmark:slave_node()], fun()} | no_return().

init_slaves(NumSlaves, Config) ->
    case NumSlaves > 0 of
        true ->
            SlavesIn = benchmark:config_value(Config, slaves),
            SlavesCount = length(SlavesIn),
            Slaves = if
                SlavesCount < NumSlaves ->
                    error({'NotEnoughSlaves', NumSlaves, SlavesCount});
                SlavesCount == NumSlaves ->
                    SlavesIn;
                true ->
                    lists:sublist(SlavesIn, NumSlaves)
            end,
            case benchmark:config_value(Config, setup_slaves) of
                true ->
                    Nodes = slaves_start(Config, Slaves),
                    StopFunc = fun() ->
                        lists:foreach(fun slave:stop/1, Nodes)
                    end,
                    {Nodes, StopFunc};
                _ ->
                    {Slaves, fun() -> ok end}
            end;
        _ ->
            % nothing to do
            {[], fun() -> ok end}
    end.

-spec slaves_start(
    Config  :: [benchmark:config_rec()],
    Slaves  :: [benchmark:slave_name()])
        -> [benchmark:slave_node()] | no_return().

slaves_start(Config, Slaves) ->
    {ok, HostName} = inet:gethostname(),
    DefaultHost = case benchmark:config_value(Config, use_long_names) of
        true ->
            {ok, HE} = inet:gethostbyname(HostName),
            HE#hostent.h_name;
        _ ->
            HostName
    end,
    ErlExec = benchmark:config_value(Config, erl_exec, "erl"),
    ErlArgs = benchmark:config_value(Config, erl_args),
    [begin
        [Name|Rest] = string:tokens(atom_to_list(Sin), "@"),
        Host = case Rest of
            [] ->
                DefaultHost;
            [First|_] ->
                First
        end,
        %
        % This is an undocumented arity of slave:start!
        %
        % It's equivalent to slave:start_link/3 with the addition of
        % an explicit "erl" program.
        %
        {ok, Slave} = slave:start(
            list_to_atom(Host), list_to_atom(Name), ErlArgs, self(), ErlExec),
        Slave
    end || Sin <- Slaves].


-spec record_times(
    IoDev   :: io:device(),
    OutForm :: atom(),
    Count   :: non_neg_integer(),
    Label   :: {string(), benchmark:bench_args()} | benchmark:bench_args(),
    TList   :: [non_neg_integer()] ) ->
    ok | {error, Reason :: term()}.

record_times(IoDev, OutForm, Count, Label, TList) ->
    % times are already microseconds, keep them as integers
    Times = case OutForm of
        avg_min_max ->
            [ (lists:sum(TList) div length(TList)),
                lists:min(TList), lists:max(TList) ];
        avg ->
            [lists:sum(TList) div length(TList)];
        min ->
            [lists:min(TList)];
        max ->
            [lists:max(TList)];
        plain ->
            TList
    end,
    case Label of
        {Name, BenchArgs} ->
            Formatted = format_label(false, Name),
            io:format(IoDev, "~s\t~w\t~p\t", [Formatted, BenchArgs, Count]);
        BenchArgs ->
            Formatted = format_label(
                lists:all(fun erlang:is_integer/1, BenchArgs), BenchArgs),
            io:format(IoDev, "\t~s\t~p\t", [Formatted, Count])
    end,
    case Times of
        [T] ->
            io:put_chars(IoDev, format_time(T));
        _ ->
            io:put_chars(IoDev,
                string:join([format_time(T) || T <- Times], " "))
    end,
    io:nl(IoDev).

-spec format_time(Microsecs :: non_neg_integer()) -> string().

format_time(Microsecs) ->
    io_lib:format("~.3f", [(Microsecs / 1000)]).

-spec format_label(AsType :: atom(), Label :: term()) -> string().

format_label(true, Label) ->
    cleanup_label(io_lib:format("~w", [Label]));
format_label(text, Label) ->
    cleanup_label(io_lib:format("~s", [Label]));
format_label(_, Label) ->
    cleanup_label(io_lib:format("~p", [Label])).

-spec cleanup_label(Text :: string()) -> string().

cleanup_label(Text) ->
    cleanup_label(lists:flatten(Text), []).

cleanup_label([], Result) ->
    lists:reverse(Result);
cleanup_label([$\s | Rest], Result) ->
    cleanup_label(Rest, [$= | Result]);
cleanup_label([$\t | Rest], Result) ->
    cleanup_label(Rest, Result);
cleanup_label([$\n | Rest], Result) ->
    cleanup_label(Rest, Result);
cleanup_label([$\r | Rest], Result) ->
    cleanup_label(Rest, Result);
cleanup_label([Char | Rest], Result) ->
    cleanup_label(Rest, [Char | Result]).

