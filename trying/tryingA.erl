-module(tryingA).
-export([main/0]).

main() ->
    X = map_attempt(),
    io:fwrite("Map after put(): ~p\n", [X]),
    intToList().

map_attempt() ->
    Map1 = #{key1 => 1, val1 => 'Actor_PID'},
    io:fwrite("Map before put(): ~p\n", [Map1]),
    maps:put(key2, "Actor2_PID", Map1).

intToList() ->
    H = integer_to_list(30, 16),
    io:fwrite("int to hex: ~p\n", [H]),
    M = list_to_integer(H, 16),
    io:fwrite("list to integer: ~p\n", [M]).
