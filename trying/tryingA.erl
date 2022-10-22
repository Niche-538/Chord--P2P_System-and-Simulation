-module(tryingA).
% -import(crypto, []).
-export([main/0]).

main() ->
    X = map_attempt(),
    io:fwrite("Map after put(): ~p\n", [X]),
    intToList(),
    sha_attempt().

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
