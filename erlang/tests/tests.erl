
-module(tests).
-export([tests/0]).

tests() ->
    A = round(3.3),
    io:format("value: ~w~n", [ A ]).


