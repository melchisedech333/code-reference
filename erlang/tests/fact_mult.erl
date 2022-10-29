
-module(fact_mult).

% Define duas funções como acessíveis externamente.
-export([fact/1, mult/2]).

%% Função 1 (fatorial).
fact(1) ->
    1;

fact(N) ->
    N * fact(N - 1).

%% Função 2 (multiplicação).
mult(X, Y) ->
    X * Y.


