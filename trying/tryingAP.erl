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
    generateActors(
        N - 1,
        M,
        [
            spawn(fun() ->
                actor_process(
                    trunc(math:pow(2, M) / M) * N,
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
            actorFingerTableCreation(L, tail_len(L)),
            master_process()
    end.

actorFingerTableCreation(L, N) ->
    case N > 0 of
        true ->
            lists:nth(N, L) ! {createFingerTable, {L}},
            actorFingerTableCreation(L, N - 1);
        false ->
            done
    end.

actor_process(NodeIdentity, NumNodes, AID, MID, FingerTable, RequestCounter) ->
    io:fwrite("Just removing Warnings: ~p ~p ~p\n", [NodeIdentity, MID, RequestCounter]),
    receive
        {createFingerTable, {L}} ->
            io:fwrite("NodeID: ~p ~p ~p \n", [NodeIdentity, AID, L]),
            FTable = createFingerTable(NodeIdentity, NumNodes - 1, AID, L, FingerTable),
            io:fwrite("Finger Table For ~p: ~p\n", [NodeIdentity, FTable])
    end.

createFingerTable(NodeIdentity, N, AID, L, FingerTable) ->
    case N >= 0 of
        true ->
            S = trunc(NodeIdentity + math:pow(2, N)),
            X = 18 * (1 + trunc(S / 18)),
            case ((X > 0) and (X < 18)) or (X > 126) of
                true ->
                    FT = maps:put(S, 18, FingerTable),
                    createFingerTable(NodeIdentity, N - 1, AID, L, FT);
                false ->
                    FT = maps:put(S, X, FingerTable),
                    createFingerTable(NodeIdentity, N - 1, AID, L, FT)
            end;
        false ->
            FingerTable
    end.
