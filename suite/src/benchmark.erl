%
% benchmark test behavior and helpers
%
-module(benchmark).

-export([
    bench_config/1,
    bench_config/2,
    clean_label/1,
    config_value/2,
    config_value/3,
    open_file/2,
    print_error/3,
    print_error/4,
    temp_dir/0,
    temp_dir/1,
    temp_file/0,
    temp_file/1,
    tmp_dir/0
]).

-export_type([
    bench_arg/0,
    bench_args/0,
    bench_conf/0,
    bench_label/0,
    bench_result/0,
    bench_vers/0,
    config_key/0,
    config_rec/0,
    config_val/0,
    error_reason/0,
    error_result/0,
    microsecs/0,
    named_args/0,
    plain_args/0,
    result_time/0,
    slave_name/0,
    slave_node/0,
    slaves/0,
    time_key/0
]).

-type bench_arg()   :: term().
-type bench_args()  :: plain_args() | named_args().
-type bench_conf()  :: [config_rec()].
-type bench_label() :: term().
-type bench_result():: ok | [result_time()] | error_result().
-type bench_vers()  :: short | intermediate | long.
-type config_key()  :: atom().
-type config_rec()  :: {config_key(), config_val()}.
-type config_val()  :: term().
-type error_reason():: term().
-type error_result():: {error, error_reason()}.
-type microsecs()   :: non_neg_integer().
-type named_args()  :: {bench_label(), plain_args()}.
-type plain_args()  :: [bench_arg()].
-type result_time() :: {time_key(), microsecs()}.
-type slave_name()  :: atom().
-type slave_node()  :: node().
-type slaves()      :: [slave_name()] | [slave_node()].
-type time_key()    :: work_time | exec_time.

%%======================================================================
%%  Test API
%%======================================================================

%%
%%  @doc    Returns the arguments to use for running the benchmark.
%%
%%  The plain_args() component of each result list element is provided to one
%%  invocation of {@link run/3} as its `Args` parameter.
%%
%%  If an element is a tuple
%%
-callback bench_args(Version :: bench_vers(), Config :: bench_conf())
        -> [bench_args()].

%%
%%  @doc    Runs the benchmark using the specified arguments.
%%
%%
-callback run(Args :: plain_args(), Slaves :: slaves(),
    Config :: bench_conf()) -> bench_result().

%%======================================================================
%%  Helper API
%%======================================================================

-spec bench_config(Config :: bench_conf(), Bench :: module())
        -> [config_rec()].
%%
%%  @doc    Returns the configuration terms for the specified benchmark.
%%
%%  If the `BENCHERL_BENCHCONF' configuration variable was set in the active
%%  configuration file, and contains the key (benchmark name) `Bench', then
%%  the value associated with that key (which should be a list of K/V pairs)
%%  is returned.
%%
%%  If `BENCHERL_BENCHCONF' was not set, or if it does not contain an entry
%%  for `Bench', then an empty list is returned.
%%
bench_config(Config, Bench) ->
    case lists:keyfind(bench_conf, 1, Config) of
        false ->
            [];
        {bench_conf, SubConf} ->
            case lists:keyfind(Bench, 1, SubConf) of
                false ->
                    [];
                {Bench, BeConf} ->
                    BeConf
            end
    end.

-spec bench_config(Config :: bench_conf()) -> [config_rec()] | no_return().
%%
%%  @doc    Returns the configuration terms for the invoked benchmark.
%%
%%  This is equivalent to invoking {@link bench_config/2} with `Bench' equal
%%  to the name of the benchmark that `bencherl' (or a sub-script) is
%%  currently running. This is one of the values from the effective benchmark
%%  list (see the handling of `BENCHERL_INCLUDE' and `BENCHERL_EXCLUDE').
%%
%%  If `BENCHERL_BENCHCONF' was not set, or if it does not contain an entry
%%  for the current benchmark, then an empty list is returned.
%%
%%  Note that some benchmarks may invoke operations in other benchmarks, so
%%  the current benchmark MAY NOT match the currently executing code. Keep
%%  this in mind when considering using this function!
%%
%%  Note also that unlike {@link bench_config/2}, this function MAY raise a
%%  `ConfigKeyMissing' error if the benchmark is run with an older version
%%  of the `bencherl' script.
%%
bench_config(Config) ->
    bench_config(Config, config_value(Config, bench)).

-spec clean_label(Term :: atom() | string()) -> string().
%%
%%  @doc    Returns a string transformed not to include gnuplot escapes.
%%
clean_label(Label) when is_atom(Label) ->
    clean_label(atom_to_list(Label));
clean_label(Label) when is_list(Label) ->
    [gnuplot_safe_char(Ch) || Ch <- Label].

-spec config_value(Config :: bench_conf(), Key :: config_key())
        -> config_val() | no_return().
%%
%%  @doc    Returns the value of the specified `Key' from `Config'.
%%
%%  If `Key' is not present, `error:{ConfigKeyMissing, Key}' is raised.
%%
config_value(Config, Key) ->
    case lists:keyfind(Key, 1, Config) of
        false ->
            error({'ConfigKeyMissing', Key});
        {Key, Value} ->
            Value
    end.

-spec config_value(Config :: bench_conf(), Key :: config_key(),
    Default :: config_val()) -> config_val().
%%
%%  @doc    Returns the value of the specified `Key' from `Config'.
%%
%%  If `Key' is not present, `Default' is returned.
%%
config_value(Config, Key, Default) ->
    case lists:keyfind(Key, 1, Config) of
        false ->
            Default;
        {Key, Value} ->
            Value
    end.

-spec open_file(File :: file:filename(), Modes :: [file:mode()])
        -> file:io_device() | no_return().
%%
%%  @doc    Behavior is identical to {@link file:open/2}, but raises an
%%          exception on error.
%%
%%  Where `file:open' returns `{error, Reason}' that result is mapped to
%%  an `error:{Reason, File}' exception.
%%
open_file(File, Modes) ->
    case file:open(File, Modes) of
        {ok, IoDev} ->
            IoDev;
        {error, Reason} ->
            error({Reason, File})
    end.

-spec print_error(Error :: atom() | string(), Reason :: term(),
    Trace :: [tuple()]) -> ok.
%%
%%  @doc    Formats and prints the specified error to the group leader.
%%
print_error(Error, Reason, Trace) ->
    io:format("Error '~s':\n\t~p\n", [Error, Reason]),
    [io:format("\t~p\n", [T]) || T <- Trace],
    ok.

-spec print_error(IoDev :: atom() | file:io_device(),
    Error :: atom() | string(), Reason :: term(), Trace :: [tuple()])
         -> ok.
%%
%%  @doc    Formats and prints the specified error to the specified device.
%%
print_error(IoDev, Error, Reason, Trace) ->
    io:format(IoDev, "Error '~s':\n\t~p\n", [Error, Reason]),
    [io:format(IoDev, "\t~p\n", [T]) || T <- Trace],
    ok.

-spec temp_dir() -> file:filename().
%%
%%  @doc    Creates and returns the path to a newly created temporary
%%          directory.
%%
temp_dir() ->
    temp_dir(tmp_dir()).

-spec temp_dir(BaseDir :: file:filename()) -> file:filename().
%%
%%  @doc    Creates and returns the path to a temporary directory under
%%          the specified directory.
%%
temp_dir(BaseDir) ->
    FsPath = filename:join(BaseDir, candidate_name()),
    case file:make_dir(FsPath) of
        ok ->
            FsPath;
        {error, eexist} ->
            temp_dir(BaseDir);
        {error, Reason} ->
            error({Reason, FsPath})
    end.

-spec temp_file() -> file:filename().
%%
%%  @doc    Creates and returns the path to a newly created temporary file.
%%
temp_file() ->
    temp_file(tmp_dir()).

-spec temp_file(BaseDir :: file:filename()) -> file:filename().
%%
%%  @doc    Creates and returns the path to a newly created temporary file
%%          under the specified directory.
%%
temp_file(BaseDir) ->
    FsPath = filename:join(BaseDir, candidate_name()),
    case file:open(FsPath, [raw, exclusive]) of
        {ok, IoDev} ->
            file:close(IoDev),
            FsPath;
        {error, eexist} ->
            temp_file(BaseDir);
        {error, Reason} ->
            error({Reason, FsPath})
    end.

-spec tmp_dir() -> file:filename().
%%
%%  @doc    Returns the path to the default temporary directory.
%%
tmp_dir() ->
    tmp_dir([
        "BENCHERL_WORK", "TMPDIR", "TMP", "TEMP",
        {"BENCHERL_ROOT", ["scratch"]}, {"HOME", ["tmp"]},
        {"HOME", ["temp"]}, {fs, "/tmp"}, {fs, "/temp"},
        "HOME"
    ]).

%%======================================================================
%%  Internal functions
%%======================================================================

-spec candidate_name() -> string().
% Creates a filesystem item name that might be unique.
candidate_name() ->
    {T1, T2, T3} = os:timestamp(),
    Num = (T1 + T2 + T3) * random:uniform(1024 * 1024 * 1024),
    io_lib:format("~.36b", [Num]).

-spec gnuplot_safe_char(Ch :: byte()) -> byte().
% Returns a character that won't be treated as an escape by gnuplot.
gnuplot_safe_char($_) ->
    $-;
gnuplot_safe_char(Ch) ->
    Ch.

-spec tmp_dir(EnvVars :: [string()]) -> file:filename() | false.
% Returns the path to the default temporary directory.
tmp_dir([]) ->
    file:get_cwd();
tmp_dir([{fs, FsPath} | EnvVars]) ->
    case file:is_dir(FsPath) of
        true ->
            FsPath;
        _ ->
            tmp_dir(EnvVars)
    end;
tmp_dir([{EnvVar, SubDirs} | EnvVars]) ->
    case os:getenv(EnvVar) of
        false ->
            tmp_dir(EnvVars);
        Dir ->
            DirPath = filename:join([Dir | SubDirs]),
            case file:is_dir(DirPath) of
                true ->
                    DirPath;
                _ ->
                    tmp_dir(EnvVars)
            end
    end;
tmp_dir([EnvVar | EnvVars]) ->
    case os:getenv(EnvVar) of
        false ->
            tmp_dir(EnvVars);
        Dir ->
            case file:is_dir(Dir) of
                true ->
                    Dir;
                _ ->
                    tmp_dir(EnvVars)
            end
    end.
