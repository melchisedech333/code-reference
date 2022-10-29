
-module(list_max).
-export([list_max/1]).

list_max([Head|Rest]) ->
    io:format("1 called -> ~w~n", [Head]),
    list_max(Rest, Head).

list_max([], Res) ->
    io:format("4 called -> res: ~w~n", [Res]),
    Res;

%% É verificado se o Head é maior que o Result_so_far.
%% Com esse comportamento é possível encontrar o maior valor na lista.
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    io:format("2 called -> head:~w, result:~w~n", [Head, Result_so_far]),
    list_max(Rest, Head);

list_max([Head|Rest], Result_so_far)  ->
    io:format("3 called -> head:~w, result:~w~n", [Head, Result_so_far]),
    list_max(Rest, Result_so_far).


