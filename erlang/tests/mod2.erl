
-module(mod2).
-export([main/0]).

main() ->
    mod1:func(),
    io:format("Sanctus, Sanctus, Sanctus!~n").


