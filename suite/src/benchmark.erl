%
% benchmark test behavior and helpers
%
-module(benchmark).

-export([
    config_value/2, config_value/3,
    bench_config/1, bench_config/2,
    open_file/2
]).

-export_type([
    bench_args/0, bench_conf/0, bench_vers/0,
    config_key/0, config_rec/0, config_val/0,
    slave_name/0, slave_node/0, slaves/0
]).

-type bench_args()  :: [term()].
-type bench_conf()  :: [config_rec()].
-type bench_vers()  :: short | intermediate | long.
-type config_key()  :: atom().
-type config_rec()  :: {config_key(), config_val()}.
-type config_val()  :: term().
-type slave_name()  :: atom().
-type slave_node()  :: node().
-type slaves()      :: [slave_name()] | [slave_node()].

%%
%%  @doc    Returns the arguments to use for running the benchmark.
%%
%%  The returned list is provided to {@link run/3} in its `Args` parameter.
%%
-callback bench_args(Version :: bench_vers(), Config :: bench_conf())
        -> bench_args().

%%
%%  @doc    Runs the benchmark using the specified arguments.
%%
%%
-callback run(Args :: bench_args(), Slaves :: slaves(),
    Config :: bench_conf()) -> ok | {error, Reason :: term()}.

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

