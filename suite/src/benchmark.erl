%
% benchmark test behavior
%
-module(benchmark).

%%
%%  @doc    Returns the arguments to use for running the benchmark.
%%
%%  The returned list is provided to {@link run/3} in its `Args` parameter.
%%
-callback bench_args(
    Version :: short | intermediate | long,
    Config  :: [{Key :: atom(), Val :: term()}] )
        -> [term()].

%%
%%  @doc    Runs the benchmark using the specified arguments.
%%
%%
-callback run(
    Args    :: [term()],
    Slaves  :: [node()],
    Config  :: [{Key :: atom(), Val :: term()}] )
        -> ok | {error, Reason :: term()}.

