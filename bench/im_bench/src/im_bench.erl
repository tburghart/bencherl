-module(im_bench).

-export([bench_args/2, run/3]).

bench_args(Vsn, Conf) ->
  { _, Slaves } = lists:keyfind(slaves, 1, Conf),   
  ClientNodes = filter_nodes(Slaves, "client"),
  Ratio = case Vsn of
            short        -> 2000;
            intermediate -> 4000;
            long         -> 8000
          end,
  [[ Ratio * length(ClientNodes) ]].

run([Clients], Slaves, Conf) ->
  % Setup a coordinator to know when the benchmark finished. This is done by
  % counting the number of loggers that have finished.
  global:register_name(coordinator, self()),
  % Get the data dir in order to store the .csv output files there.
  { _, DataDir } = lists:keyfind(datadir, 1, Conf),
  % Get the benchmark arguments from the configuration.
  ClientNodes = filter_nodes(Slaves, "client"),
  ServerNodes = filter_nodes(Slaves, "server"),
  RouterNodes = filter_nodes(Slaves, "router"),
  % Start the benchmark on the different client domains.
  launcher:start_bencherl(length(ServerNodes) div length(RouterNodes), 1,
    length(ServerNodes), length(ClientNodes), Slaves),
  logger:launch_latency("Bencherl_test", length(RouterNodes), length(ServerNodes),
    Clients, 1, ClientNodes, DataDir ++ "/"),
  timer:sleep(60000), %XXX: Just to make sure that all clients have logged in.
  toxic_client:launch(Clients, ClientNodes),
  timer:sleep(60000),
  toxic_client:launch_traffic(Clients, ClientNodes),
  {ok, Fd} = file:open(DataDir ++ "/statistics.txt", [append]),

  io:fwrite(Fd, "# Conf: ~w scheduler(s), ~w server(s), ~w router(s), "
        "~w client(s), ~w client processes~n",
    [element(2, lists:keyfind(schedulers, 1, Conf)), length(ServerNodes),
     length(RouterNodes), length(ClientNodes), Clients]),
  loop(length(ClientNodes), Fd),
  ok = file:close(Fd).

%% filter_nodes/2 returns the nodes in the given list whose name starts with
%% the given prefix.
filter_nodes(Nodes, Prefix) ->
  lists:filter(fun(N) ->
      string:sub_string(atom_to_list(N), 1, string:len(Prefix)) == Prefix
    end, Nodes).

%% loop/1 is a helper function that "prevents" run/3 from finishing until all
%% loggers have halted. Without this function the benchmark would finish after
%% launching the traffic generators and, thus, bencherl would kill all spawned
%% nodes, i.e. routers, servers, etc.
loop(0, _) -> ok;
loop(N_Loggers, Fd) ->
  receive
    logger_stopped -> loop(N_Loggers - 1, Fd);
    {stats, Node, L, Avg, Mean} ->
        io:fwrite(Fd, "~p ~w ~w ~w~n", [Node, L, Avg, Mean]),
        loop(N_Loggers, Fd)
  end.
