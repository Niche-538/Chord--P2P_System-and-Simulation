-module(tryingP).
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
    N - 1, M,[spawn(fun() -> actor_process(trunc(math:pow(2, M)/M * (M-N+1)-1), M, self(), MID, #{},counters:new(1, [atomics])) end) | L], MID
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
  receive
  {actorList, {L}} ->
    commandFingerTableCreation(L,tail_len(L)),
  master_process()
  end.

commandFingerTableCreation(L,N)->
  case N > 0 of
    true ->
      lists:nth(N, L) ! {createFingerTable, {L}},
      commandFingerTableCreation(L,N-1);
    false ->
      done
  end.

actor_process(NodeId, NumNodes, AID, MID, FingerTable, RequestCounter)->
  receive
    {createFingerTable, {L}} ->
      io:fwrite("NodeID: ~p ~p ~p \n", [NodeId,AID,L]),
      createFingerTable(NodeId,NumNodes, AID, L, FingerTable);
%%      io:fwrite("FingerTable Update: ~p\n", [FT]);
    {updateFT, {NodeId, AID, FingerTable}} ->
      io:fwrite("Updated FT: ~p\n", [FingerTable])
  end.

createFingerTable(NodeId, N, AID, L, FingerTable)->
  case N > 0 of
    true ->
      maps:put(key2, "Actor2_PID", FingerTable);
    false ->
      AID ! {updateFT, {NodeId, AID, FingerTable}}
  end,
  createFingerTable(NodeId, N-1, AID, L, FingerTable).

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
