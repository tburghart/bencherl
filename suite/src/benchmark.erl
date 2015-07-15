%
% benchmark test behavior and helpers
%
-module(benchmark).

-export([
    config_value/2, config_value/3
]).

-export_type([
    config_key/0, config_val/0, config_rec/0,
    bench_args/0, bench_vers/0,
    slave_name/0, slave_node/0
]).

-type config_key() :: atom().
-type config_val() :: term().
-type config_rec() :: {config_key(), config_val()}.
-type bench_vers() :: short | intermediate | long.
-type bench_args() :: [term()].
-type slave_name() :: atom().
-type slave_node() :: node().

%%
%%  @doc    Returns the arguments to use for running the benchmark.
%%
%%  The returned list is provided to {@link run/3} in its `Args` parameter.
%%
-callback bench_args(
    Version :: bench_vers(),
    Config  :: [config_rec()])
        -> bench_args().

%%
%%  @doc    Runs the benchmark using the specified arguments.
%%
%%
-callback run(
    Args    :: bench_args(),
    Slaves  :: [slave_node()],
    Config  :: [config_rec()] )
        -> ok | {error, Reason :: term()}.

-spec config_value(
    Config  :: [config_rec()],
    Key     :: config_key())
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

-spec config_value(
    Config  :: [config_rec()],
    Key     :: config_key(),
    Default :: config_val())
        -> config_val().
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

