-module(tryingP).
-compile(export_all).
-export([main/1]).
-import(lists, [append/2, reverse/1]).

tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | T], Acc) -> tail_len(T, Acc + 1).

generateActors(N,M,MID) ->
  generateActors(N,M, [], MID).

generateActors(0, _, L, _) ->
  reverse(L);
generateActors(N, M, L, MID) ->
  generateActors(
    N - 1, M,[spawn(fun() -> actor_process((math:pow(2, M)/M * N -1), MID, #{},counters:new(1, [atomics])) end) | L], MID
  ).

main(NumNodes) ->

  MID = spawn(fun() -> master_process() end),
  L = generateActors(NumNodes, NumNodes, MID),
  MID ! {actorList, {L}},

  X = map_attempt(),
  io:fwrite("Map after put(): ~p\n", [X]),
  intToList(),
  sha_attempt().

master_process() ->
  io:fwrite("Master\n").

actor_process(NodeId, MID, FingerTable, RequestCounter)->
  io:fwrite("NodeID: ~p\n", [NodeId]),
  io:fwrite("ActorMap: ~p\n", [FingerTable]).

map_attempt() ->
  Map1 = #{key1 => 1, val1 => 'Actor_PID'},
  io:fwrite("Map before put(): ~p\n", [Map1]),
  maps:put(key2, "Actor2_PID", Map1).

intToList() ->
  H = integer_to_list(30, 16),
  io:fwrite("Int to hex: ~p\n", [H]),
  M = list_to_integer(H, 16),
  io:fwrite("List to integer: ~p\n", [M]).

sha_attempt() ->
  <<Mac:160/integer>> = crypto:hash(sha, "Test String"),
  Y = lists:flatten(io_lib:format("~40.16.0b", [Mac])),
  io:fwrite("Sha string: ~p\n", [Y]),
  M = list_to_integer(Y, 16),
  io:fwrite("List to integer sha: ~p\n", [M]),
  Modulo = M rem 127,
  io:fwrite("Modulo: ~p\n", [Modulo]).