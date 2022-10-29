
-module(atoms).
-export([convert/2]).

%% Todas as variáveis são definidas começando com letra maiúscula.
%% Os átomos são definidos com letras minúsculas (inch é o átomo).
convert(M, inch) ->
    M / 2.54;

convert(M, centimeter) ->
    M * 2.54.

% Utilização:
%   > atoms:convert(10, centimeter).
%    25.4
%   > atoms:convert(10, inch).      
%    3.937007874015748


