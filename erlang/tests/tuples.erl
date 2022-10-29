
-module(tuples).
-export([convert/1]).

%% A tupla é definida utilizando chaves {}.
%% Ela pode ter várias tuplas concatenadas dentro delas.
%% Pode-se utilizar elas em vários locais, como parâmetro e no retorno das funções.
convert({centimeter, X}) ->
    {inch, X / 2.54};

convert({inch, X}) ->
    {centimeter, X * 2.54}.

% 18> tuples:convert({centimeter, 10}).
% {inch,3.937007874015748}
% 19> tuples:convert({inch, 10}).      
% {centimeter,25.4}


