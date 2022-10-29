
% Define um nome para o módulo.
-module(fact).

% Especifica que a função 'fc' pode ser utilizada por outros módulos.
-export([fc/1]).

%% Define uma função, especificando que para o argumento 1, o 
%% retorno será sempre 1.
fc(1) -> 
    1; % O ponto e virgula significa que a função possui mais código
       % para ser adicionado a ela.

%% Continua código da função, especificando o que ocorre para outros valores.
fc(X) ->
    X * fc(X - 1). % Conclui função com o ponto.


