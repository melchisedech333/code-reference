
-module(list_funcs).
-export([main/0]).

main() ->
    F1 = fun(X) -> 
        io:format("element: ~w~n", [ X ])    
    end,

    F2 = fun(X) -> X * 2 end,

    A = foreach(F1, [1, 2, 3]),
    B = map(F2, [1, 2, 3]),

    io:format("F1: ~w, F2: ~w~n", [ A, B ]).

foreach(Fun, [First|Rest]) ->
    Fun(First),
    foreach(Fun, Rest);
foreach(Fun, []) ->
    ok.

map(Fun, [First|Rest]) -> 
    [Fun(First)|map(Fun,Rest)];
map(Fun, []) -> 
    [].


