
-module(funcs).
-export([main/0]).

main() ->
    Fx = fun(X) -> X * 2 end,
    Fn = fun(X) -> round(X) end,

    A = Fx(3),
    B = Fn(3.3),

    io:format("Fx: ~w, Fn: ~w~n", [ A, B ]).


