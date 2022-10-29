-module(fact).
-export([fc/1]).

fc(1) ->
    1;

fc(X) ->
    X * fc(X - 1).


