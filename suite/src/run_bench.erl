%% The benchmark executor.
%% The executor is responsible for starting the necessary slave nodes, for
%% running a benchmark and for stopping the slaves, after the execution of the
%% benchmark is over.

-module(run_bench).

% entry functions
-export([main/0, main/1]).

% functions spawned by MFA
- export([run_bench/6]).

-include_lib("kernel/include/inet.hrl").

% time avaeraging threshhold, resolves to boolean
-define(drop_avg_hilo(NTimes), (NTimes > 4)).

-spec main() -> ok | no_return().
%%
%%  @doc    Locates configuration file from OS environment and invokes
%%          {@link main/1}.
%%
main() ->
    ConfigFile = case os:getenv("BENCHERL_BECONFIG") of
        false ->
            case os:getenv("BENCHERL_ROOT") of
                false ->
                    error_exit('BadConfig',
                        "No suitabe environment variable present");
                Dir ->
                    filename:join([Dir, "scratch", "run_bench.conf"])
            end;
        File ->
            File
    end,
    main(ConfigFile).

-spec main(ConfigFile :: file:filename()) -> ok | no_return().
%%
%%  @doc    Obtains configuration from a file and runs a benchmark.
%%
main(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Config} ->
            run_bench(Config);
        {error, {_, _, _} = Reason} ->
            error_exit('ConfigFileFormat', Reason);
        {error, IO} ->
            error_exit(IO, ConfigFile)
    end.

-spec error_exit(Error :: atom() | string(), Reason :: term()) -> no_return().
%%
%%  @doc    Displays error message and exits.
%%
error_exit(Error, Reason) ->
    error_exit(Error, Reason, []).

-spec error_exit(Error :: atom() | string(), Reason :: term(),
    Trace :: [tuple()]) -> no_return().
%%
%%  @doc    Displays error message and exits.
%%
error_exit(Error, Reason, Trace) ->
    benchmark:print_error(standard_error, Error, Reason, Trace),
    erlang:halt(1).

-spec run_bench(Config :: benchmark:bench_conf()) -> ok | no_return().
%%
%%  @doc    Main implementation.
%%
run_bench(Config) ->
    try
        Bench   = benchmark:config_value(Config, bench),
        Version = benchmark:config_value(Config, version, short),
        OutForm = benchmark:config_value(Config, output_format, 'avg_min_max'),
        NSlaves = benchmark:config_value(Config, number_of_slaves, 0),
        NScheds = benchmark:config_value(Config, number_of_schedulers),
        NIters  = benchmark:config_value(Config, iterations, 1),

        MeasureCount = case benchmark:config_value(Config, what) of
            node  ->
                NSlaves;
            sched ->
                NScheds
        end,

        % open the output files
        % fail early if config values are missing or invalid
        Measurements = benchmark:open_file(
            benchmark:config_value(Config, measfile), [append]),
        OutputSink = benchmark:open_file(
            benchmark:config_value(Config, outfile), [write]),

        % start the slaves if necessary, and store the function to stop them
        {Slaves, StopSlaves} = init_slaves(NSlaves, Config),

        % The constant Config parameter to benchmark:bench_args/2 and
        % benchmark:run/3. In the first case the `version' key is redundant,
        % but we want the `run' function to have all the information the
        % `bench_args' function had so it can apply the same decision logic,
        % in whatever form that takes.
        BenchConf = [
            {bench, Bench},
            {bench_conf, [{
                Bench, benchmark:config_value(
                    benchmark:config_value(Config, bench_conf, []), Bench, [])
            }]},
            {schedulers, NScheds},
            {slaves, Slaves},
            {version, Version}
        ] ++ [
            {K, benchmark:config_value(Config, K)}
                || K <- [datadir, master, number_of_cores, workdir]],

        % At this point any required values missing from the input
        % configuration should have triggerred an error, so beyond here
        % we should have smooth sailing ...

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
                RunBenchArgs = [
                    erlang:self(), OutputSink, Bench,
                    case CurrentState of
                        initial ->
                            BenchArgs;
                        {state, SBState} ->
                            [SBState | BenchArgs]
                    end,
                    Slaves, BenchConf
                ],
                % in a new process, please
                erlang:spawn(erlang:node(), ?MODULE, run_bench, RunBenchArgs),
                % handle the {done, {ExecMicros, WorkMicros, BenchReturn}}
                % message from run_bench/6
                {WorkTime, BenchResult} = receive
                    {done, {_, WorkMicros, BenchReturn}} ->
                        {WorkMicros, BenchReturn}
                end,
                % there are a bunch of undocumented result patterns from
                % benchmark:run/3 that get special handling
                case BenchResult of
                    {{{continue, ignore}, BenchState}, _} ->
                        RecursiveRAA(
                            RecursiveRAA, {state, BenchState}, Results);
                    {{{continue, Name}, BenchState}, Duration} ->
                        RecursiveRAA(
                            RecursiveRAA, {state, BenchState},
                            orddict:append(Name, Duration, Results));
                    {{{done, ignore}, _}, _} ->
                        Results;
                    {{{done, Name}, _}, Duration} ->
                        orddict:append(Name, Duration, Results);
                    % nothing special, just record it
                    _ ->
                        orddict:append(standard, WorkTime, Results)
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
        lists:foreach(RunBench, Bench:bench_args(Version, BenchConf)),

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

-spec run_bench(
    Coordinator :: pid() | atom() | {atom(), node()},
    GroupLeader :: pid(),
    Benchmark   :: module(),
    BenchArgs   :: benchmark:bench_args(),
    Slaves      :: benchmark:slaves(),
    BenchConf   :: benchmark:bench_conf())
        -> ok.
%%
%%  @doc    Runs the benchmark and measures its execution time.
%%
%%  On completion, a `{done, {ExecMicros, WorkMicros, BenchReturn}}' message
%%  is sent to the `Coordinator`, where:
%%
%%  `ExecMicros' is the time, in microseconds, that the benchmark took to run.
%%
%%  `WorkMicros' is either the working time, in microseconds, that the
%%  benchmark reported in its results, or `ExecMicros' if the benchmark did
%%  not explicitly report its working time. This feature allows benchmarks to
%%  report working time separately from startup and shutdown time in order
%%  to provide more directly comparable results.
%%
%%  `BenchReturn' is the term returned by the benchmark.
%%
%%  Because this function is always spawned, it returns only `ok'.
%%
run_bench(Coordinator, GroupLeader, Benchmark, BenchArgs, Slaves, BenchConf) ->
    erlang:group_leader(GroupLeader, self()),
    Start   = os:timestamp(),
    Result  = Benchmark:run(BenchArgs, Slaves, BenchConf),
    ExecTm  = timer:now_diff(os:timestamp(), Start),
    WorkTm  = case Result of
        {work_time, WorkTime} ->
            WorkTime;
        [{work_time, WorkTime} | _] ->
            WorkTime;
        _ ->
            ExecTm
    end,
    Coordinator ! {done, {ExecTm, WorkTm, Result}},
    ok.

-spec init_slaves(
    NSlaves :: non_neg_integer(), Config :: benchmark:bench_conf())
        -> {Slaves :: benchmark:slaves(), StopSlaves :: fun()} | no_return().
%%
%%  @doc    Initializes slaves as appropriate.
%%
%%  Returns a list of active slaves and the function to stop them when the
%%  time comes to do so. If the slaves were not started by this function,
%%  the returned function will not stop them.
%%
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
    Config :: benchmark:bench_conf(), Slaves :: [benchmark:slave_name()])
        -> [benchmark:slave_node()] | no_return().
%%
%%  @doc    Starts specified slaves and returns them as nodes.
%%
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
    TList   :: [non_neg_integer()])
         -> ok | {error, Reason :: term()}.
%%
%%  @doc    Formats and writes one line to a measurements file.
%%
record_times(IoDev, OutForm, Count, Label, TList) ->
    % times are already microseconds, keep them as integers
    Times = case OutForm of
        avg_min_max ->
            [average_time(TList), lists:min(TList), lists:max(TList)];
        avg ->
            [average_time(TList)];
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

-spec average_time(TList :: [non_neg_integer()]) -> non_neg_integer().
%%
%%  @doc    Returns the average of the specified list of times.
%%
%%  If the number of times specified is over a predefined threshhold, the
%%  highest and lowest values are discarded from the calculation.
%%
average_time(TList) ->
    L = erlang:length(TList),
    T = case ?drop_avg_hilo(L) of
        true ->
            lists:sublist(lists:sort(TList), 2, (L - 2));
        _ ->
            TList
    end,
    lists:sum(T) div erlang:length(T).

-spec format_time(Microsecs :: non_neg_integer()) -> string().
%%
%%  @doc    Formats a microsecond value for output in a measurements file.
%%
%%  The current representation is as milliseconds.
%%
format_time(Microsecs) ->
    io_lib:format("~.3f", [(Microsecs / 1000)]).

-spec format_label(AsType :: atom(), Label :: term()) -> string().
%%
%%  @doc    Formats a measurements file record label.
%%
format_label(true, Label) ->
    cleanup_label(io_lib:format("~w", [Label]));
format_label(text, Label) ->
    cleanup_label(io_lib:format("~s", [Label]));
format_label(_, Label) ->
    cleanup_label(io_lib:format("~p", [Label])).

-spec cleanup_label(Text :: string()) -> string().
%%
%%  @doc    Sanitizes a measurements file record label.
%%
cleanup_label(Text) ->
    cleanup_label(lists:flatten(Text), []).

-spec cleanup_label(Text :: string(), Result :: string()) -> string().
%%
%%  @doc    Recursively sanitizes a measurements file record label.
%%
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

