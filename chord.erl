-module(chord).
-export([main/2]).
-import(lists, [append/2, reverse/1]).

tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | T], Acc) -> tail_len(T, Acc + 1).

generateActors(N, MID) ->
    generateActors(N, N, [], MID).

generateActors(0, _, L, _) ->
    reverse(L);
generateActors(N, M, L, MID) ->
    Multiplicant = trunc(math:pow(2, M) / M),
    Cal = Multiplicant * (M - N + 1),
    generateActors(
        N - 1,
        M,
        [
            spawn(fun() ->
                actor_process(
                    Cal,
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

main(NumNodes, NumRequests) ->
    MID = spawn(fun() -> master_process(counters:new(1, [atomics]), NumNodes, NumRequests) end),
    L = generateActors(NumNodes, MID),
    MID ! {actorList, {L}}.

master_process(Counter, NumNodes, NumRequests) ->
    receive
        {actorList, {L}} ->
            actorFingerTableCreation(L, tail_len(L)),
            master_process(Counter, NumNodes, NumRequests);
        {actorFingerComplete, {L}} ->
            counters:add(Counter, 1, 1),
            case counters:get(Counter, 1) == NumNodes of
                true ->
                    commandStartChord(L, 1, NumRequests);
                false ->
                    done
            end,
            master_process(Counter, NumNodes, NumRequests)
    end.
commandStartChord(L, N, NumRequests) ->
    case N == tail_len(L) + 1 of
        true ->
            done;
        false ->
            lists:nth(N, L) ! {startChord, {NumRequests}},
            commandStartChord(L, N + 1, NumRequests)
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
    receive
        {createFingerTable, {L}} ->
            %%      io:fwrite("NodeID: ~p  PID: ~p \n", [NodeIdentity, AID]),
            FTable = createFingerTable(
                NodeIdentity,
                NumNodes - 1,
                AID,
                L,
                FingerTable
            ),
            MID ! {actorFingerComplete, {L}},
            actor_process(NodeIdentity, NumNodes, AID, MID, FTable, RequestCounter);
        {startChord, {NumRequests}} ->
            communicateNumRequestTimes(
                NumRequests, NodeIdentity, NumNodes, AID, MID, FingerTable, RequestCounter
            ),
            actor_process(NodeIdentity, NumNodes, AID, MID, FingerTable, RequestCounter);
        {notMine, {Modulo, NumRequests}} ->
            case Modulo =< (NodeIdentity + 1) of
                true ->
                    V = (counters:get(RequestCounter, 1)/(NumNodes*NumRequests)),
                              io:fwrite("Average Hops: ~p\n\n", [V]),
                    done;
                false ->
                    checkIsYoursOrSend(
                        Modulo, FingerTable, NodeIdentity, NumNodes, NumRequests, RequestCounter
                    )
            end,
            actor_process(NodeIdentity, NumNodes, AID, MID, FingerTable, RequestCounter)
    end.

communicateNumRequestTimes(
    NumRequests, NodeIdentity, NumNodes, AID, MID, FingerTable, RequestCounter
) ->
    case NumRequests > 0 of
        true ->
            <<Mac:160/integer>> = crypto:hash(sha, crypto:strong_rand_bytes(16)),
            Y = lists:flatten(io_lib:format("~40.16.0b", [Mac])),
            M16 = list_to_integer(Y, 16),
            Range = trunc(math:pow(2, NumNodes)) - 1,
            Modulo = M16 rem Range,
                  io:fwrite("Random String Hashed: ~p FingerTable: ~p \n", [Modulo, FingerTable]),
            checkIsYoursOrSend(
                Modulo, FingerTable, NodeIdentity, NumNodes, NumRequests, RequestCounter
            ),
            communicateNumRequestTimes(
                NumRequests - 1, NodeIdentity, NumNodes, AID, MID, FingerTable, RequestCounter
            );
        false ->
            done
    end.

checkIsYoursOrSend(Modulo, FingerTable, NodeIdentity, NumNodes, NumRequests, RequestCounter) ->
    case NumNodes > 0 of
        true ->
            FTK = trunc(NodeIdentity + math:pow(2, NumNodes)),

            case Modulo =< (NodeIdentity + 1) of
                true ->
                    counters:add(RequestCounter, 1, 1),
                    maps:get(NodeIdentity + 1, FingerTable) ! {notMine, {Modulo}};
                false ->
                    done
            end,

            case Modulo >= FTK of
                true ->
                    maps:get(FTK, FingerTable) ! {notMine, {Modulo, NumRequests}};
                false ->
                    continue
            end,
            checkIsYoursOrSend(
                Modulo, FingerTable, NodeIdentity, NumNodes - 1, NumRequests, RequestCounter
            );
        false ->
            done
    end.

createFingerTable(NodeIdentity, N, AID, L, FingerTable) ->
    LLen = tail_len(L),
    Mlt = trunc(math:pow(2, LLen) / LLen),
    MaxNode = Mlt * LLen,
    case N >= 0 of
        true ->
            FTKey = trunc(NodeIdentity + math:pow(2, N)),
            % Immediate Successor
            ImS = Mlt * (1 + trunc(FTKey / Mlt)),
            case ((ImS > 0) and (ImS < Mlt)) or (ImS > MaxNode) of
                true ->
                    FT = maps:put(FTKey, lists:nth(1, L), FingerTable),
                    createFingerTable(NodeIdentity, N - 1, AID, L, FT);
                false ->
                    FT = maps:put(FTKey, lists:nth(trunc(ImS / Mlt), L), FingerTable),
                    createFingerTable(NodeIdentity, N - 1, AID, L, FT)
            end;
        false ->
            FingerTable
    end.
