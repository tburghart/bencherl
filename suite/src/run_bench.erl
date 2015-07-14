%% The benchmark executor.
%% The executor is responsible for starting the necessary slave nodes, for
%% running a benchmark and for stopping the slaves, after the execution of the
%% benchmark is over.

-module(run_bench).

-export([main/0, main/1]).

-include_lib("kernel/include/inet.hrl").

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

main(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Config} ->
            run_bench(Config);
        {error, {_, _, _} = Reason} ->
            error_exit('ConfigFileFormat', Reason);
        {error, IO} ->
            error_exit(IO, ConfigFile)
    end.

error_exit(Error, Reason) ->
    io:format(standard_error, "Error '~s': ~p~n", [Error, Reason]),
    erlang:halt(1).

error_exit(Error, Reason, Trace) ->
    io:format(standard_error,
        "Exception '~s':~n~p~n~p~n", [Error, Reason, Trace]),
    erlang:halt(1).

config_value(Config, Key) ->
    case lists:keyfind(Key, 1, Config) of
        false ->
            error({'ConfigKeyMissing', Key});
        {Key, Value} ->
            Value
    end.

config_value(Config, Key, Default) ->
    case lists:keyfind(Key, 1, Config) of
        false ->
            Default;
        {Key, Value} ->
            Value
    end.

run_bench(Config) ->
    try
        BenchMod = config_value(Config, bench),
        Version = config_value(Config, version, short),
        OutputFormat = config_value(Config, output_format, 'avg_min_max'),
        ErlExec = config_value(Config, erl_exec, "erl"),
        ErlArgs = config_value(Config, erl_args),
        Master = config_value(Config, master),
        Snames = config_value(Config, slaves, []),
        N = config_value(Config, number_of_slaves, 0),
        S = config_value(Config, number_of_schedulers),
        Iterations = config_value(Config, iterations, 1),
        OutFile = config_value(Config, outfile),
        MeasFile = config_value(Config, measfile),
        DataDir = config_value(Config, datadir),
        What = config_value(Config, what),
        UseLongNames = config_value(Config, use_long_names),
        Cores = config_value(Config, number_of_cores),
        SkipSlaveSetup = (config_value(Config, setup_slaves) == false),

        NS = case What of
                 node  -> N;
                 sched -> S
             end,

	%% Start the slaves (if necessary).
        Slaves =
           case SkipSlaveSetup of
              false -> lists:map(fun(Sn) ->
                          [Name|Rest] = string:tokens(atom_to_list(Sn), "@"),
                          {ok, Host} = case Rest of
                                          [] ->
                                             {ok, Hname} = inet:gethostname(),
                                             case UseLongNames of
                                                true  -> {ok, #hostent{h_name=H}} =
                                                             inet:gethostbyname(Hname),
                                                         {ok, H};
                                                false -> {ok, Hname}
                                             end;
                                          _ -> {ok, hd(Rest)}
                                       end,
                          {ok, Slave} = slave:start(list_to_atom(Host),
                                           list_to_atom(Name), ErlArgs, self(),
                              ErlExec),
                          Slave
                       end, lists:sublist(Snames, N));
              true -> lists:sublist(Snames, N)
           end,

	%% Open the measurements file.
        {ok, MF} = file:open(MeasFile, [append]),
        {ok, OF} = file:open(OutFile, [write]),

	%% Run the benchmark for all argument sets.
        RunFun = fun(Coordinator, CurArgs) -> group_leader(OF, self()),
                    T0 = os:timestamp(),
                    Return = apply(BenchMod, run, [CurArgs, Slaves, [{datadir, DataDir}, {master, Master}, {schedulers, S}]]),
                    Dur = timer:now_diff(os:timestamp(), T0)/1000,
                    Coordinator ! {done, {Return, Dur}}
        end,
        RecordFun = fun(Text, Times) ->
                LabelFormat =
                    begin
                        case Text of
                            {_, _} -> "~p";
                            _ ->
                                IsIntList =
                                    lists:all(fun(Elem)->is_integer(Elem) end, Text),
                                case IsIntList of
                                    true ->
                                        "~w";
                                    false ->
                                        "~p"
                                end
                        end
                    end,
		PTimes = case OutputFormat of
			min -> [lists:min(Times)];
			max -> [lists:max(Times)];
			avg -> [lists:sum(Times) / length(Times)];
			avg_min_max ->
				[ lists:sum(Times) / length(Times),
				  lists:min(Times),
				  lists:max(Times)
			        ];
			plain -> Times
		end,
                case Text of
                    {Str, L} ->
                        FStr = remove_whitespace_and_new_lines(lists:flatten(io_lib:format(LabelFormat,[Str]))),
                        io:format(MF, "~s\t~w\t~p\t", [FStr, L, NS]);
                    _ ->
                        FText = remove_whitespace_and_new_lines(lists:flatten(io_lib:format(LabelFormat,[Text]))),
                        io:format(MF, "\t~s\t~p\t", [FText, NS])
                end,
		io:format(MF, string:join([io_lib:format("~w", [A]) || A <- PTimes], " "), []),
		io:nl(MF)
        end,
        Fun =
            fun(Bargs) ->
                    RunAndAggregate = fun(RecursiveRAA, CurrentState, AggregateResults) ->
                            Coordinator = self(),
                            %% In a new process, please.
                            spawn(node(), fun() -> RunFun(Coordinator, case CurrentState of initial -> Bargs; {state, C} -> [C | Bargs] end) end),
                            receive
                                {done, {R, T}} ->
                                    case R of
                                        {{continue, ignore}, State} -> RecursiveRAA(RecursiveRAA, {state, State}, AggregateResults);
                                        {{continue, Name}, State} -> RecursiveRAA(RecursiveRAA, {state, State}, orddict:append(Name, T, AggregateResults));
                                        {{done, ignore}, _} -> AggregateResults;
                                        {{done, Name}, _} -> orddict:append(Name, T, AggregateResults);
                                        _ -> orddict:append(standard, T, AggregateResults)
                                    end
                            end
                    end,
                    Aggregated = orddict:new(),
                    Times = lists:foldl(fun(_, A) -> RunAndAggregate(RunAndAggregate, initial, A) end, Aggregated, lists:seq(1,Iterations)),
                    case orddict:to_list(Times) of
                        [{standard, T}] -> RecordFun(Bargs, T);
                        T -> lists:foreach(fun({Name, SubTimes}) -> RecordFun({Name, Bargs}, SubTimes) end, T)
                    end
            end,
        lists:foreach(Fun, BenchMod:bench_args(Version, [{number_of_cores, Cores}, {slaves, Slaves}])),
        file:close(OF),

	%% Close the measurements file.
        file:close(MF),

	%% Stop the slaves.
        case SkipSlaveSetup of
            false -> lists:foreach(fun(Slave)-> slave:stop(Slave) end, Slaves);
            true  -> ok
        end

    catch
        Error:Reason ->
            error_exit(Error, Reason, erlang:get_stacktrace())
    end.


remove_whitespace_and_new_lines(Str) ->
    lists:reverse(remove_whitespace_and_new_lines([], Str)).

remove_whitespace_and_new_lines(Compl, []) ->
    Compl;
remove_whitespace_and_new_lines(Compl, [C | Str]) ->
    case C of
        $\s -> remove_whitespace_and_new_lines([$=|Compl], Str);
        $\n -> remove_whitespace_and_new_lines(Compl, Str);
        $\t -> remove_whitespace_and_new_lines(Compl, Str);
        _   -> remove_whitespace_and_new_lines([C|Compl], Str)
    end.
