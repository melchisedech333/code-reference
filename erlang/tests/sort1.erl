
-module(sort1).
-export([convert_list_to_c/1]).

convert_to_c({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.

convert_list_to_c(List) ->
    New_list = lists:map(fun convert_to_c/1, List),

    % Realiza o ordenamento da lista com base no valor da temperatura.
    lists:sort(fun({_, {c, Temp1}}, {_, {c, Temp2}}) ->
                        
                        % Se o primeiro argumento for menor que o segundo,
                        % retorna true, caso contrário retorna false.
                        Temp1 < Temp2 end, New_list).

%
% Saída:
%
%   94> tut13:convert_list_to_c([{moscow, {c, -10}}, {cape_town, {f, 70}},
%   {stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
%   [{moscow,{c,-10}},
%    {stockholm,{c,-4}},
%    {paris,{c,-2}},
%    {london,{c,2}},
%    {cape_town,{c,21}}]


