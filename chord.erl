-module(chord).
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
            FTable = createFingerTable(
                NodeIdentity,
                NumNodes - 1,
                AID,
                tail_len(L),
                FingerTable
            ),
            io:fwrite("Finger Table For ~p: ~p\n", [NodeIdentity, FTable])
    end.

createFingerTable(NodeIdentity, N, AID, LLen, FingerTable) ->
    % multiplicant
    Mlt = trunc(math:pow(2, LLen) / LLen),
    MaxNode = Mlt * LLen,
    case N >= 0 of
        true ->
            FTKey = trunc(NodeIdentity + math:pow(2, N)),
            % Immediate Successor
            ImS = Mlt * (1 + trunc(FTKey / Mlt)),
            case ((ImS > 0) and (ImS < Mlt)) or (ImS > MaxNode) of
                true ->
                    FT = maps:put(FTKey, Mlt, FingerTable),
                    createFingerTable(NodeIdentity, N - 1, AID, LLen, FT);
                false ->
                    FT = maps:put(FTKey, ImS, FingerTable),
                    createFingerTable(NodeIdentity, N - 1, AID, LLen, FT)
            end;
        false ->
            FingerTable
    end.
