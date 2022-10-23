-module(tryingAP).
-export([main/1]).
-import(lists, [append/2, reverse/1]).

tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | T], Acc) -> tail_len(T, Acc + 1).

generateActors(N, MID) ->
    generateActors(N, N, [], MID).

generateActors(0, _, L, _) ->
    reverse(L);
generateActors(N, M, L, MID) ->
    NodeID = trunc(math:pow(2, M) / M * (M - N + 1) - 1),
    generateActors(
        N - 1,
        M,
        [
            spawn(fun() ->
                actor_process(
                    NodeID,
                    M,
                    self(),
                    MID,
                    #{},
                    counters:new(1, [atomics])
                )
            end)
            | L
        ],
        MID
    ).

main(NumNodes) ->
    MID = spawn(fun() -> master_process() end),
    L = generateActors(NumNodes, MID),
    MID ! {actorList, {L}}.

master_process() ->
    receive
        {actorList, {L}} ->
            commandFingerTableCreation(L, tail_len(L)),
            master_process()
    end.

commandFingerTableCreation(L, N) ->
    case N > 0 of
        true ->
            lists:nth(N, L) ! {createFingerTable, {L}},
            commandFingerTableCreation(L, N - 1);
        false ->
            done
    end.

actor_process(NodeIdentity, NumNodes, AID, MID, FingerTable, RequestCounter) ->
    io:fwrite("Just removing Warnings: ~p ~p ~p\n", [NodeIdentity, MID, RequestCounter]),
    receive
        {createFingerTable, {L}} ->
            io:fwrite("NodeID: ~p ~p ~p \n", [NodeIdentity, AID, L]),
            createFingerTable(NodeIdentity, NumNodes, AID, L, FingerTable);
        {updatedFT, {NodeIdentity, AID, FingerTable}} ->
            io:fwrite("Updated FT: ~p\n", [FingerTable])
    end.

createFingerTable(NodeIdentity, N, AID, L, FingerTable) ->
    case N > 0 of
        true ->
            S = trunc(NodeIdentity + math:pow(2, N)),
            % io:fwrite("Key in  of ~p is ~p\n", [AID, S]),
            FT = maps:put(S, "Actor2_PID", FingerTable),
            createFingerTable(NodeIdentity, N - 1, AID, L, FT);
        false ->
            io:fwrite("Updated FT before Send: ~p \t AID:~p\n", [FingerTable, AID]),
            AID ! {updatedFT, {NodeIdentity, AID, FingerTable}}
    end.
