
-module(iotest).
-export([mio/0]).

mio() ->
    io:format("Iesus Hominum Salvator!~n"),
    io:format("Sanctus, Sanctus, Sanctus, ~w!~n", [forever]).


