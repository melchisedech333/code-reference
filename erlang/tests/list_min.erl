
-module(list_min).
-export([list_min/1]).

list_min([Head|Rest]) ->
    io:format("1 called -> ~w~n", [Head]),
    list_min(Rest, Head).

list_min([], Res) ->
    io:format("4 called -> res: ~w~n", [Res]),
    Res;

%% Foi mudada a regra do when para <.
list_min([Head|Rest], Result_so_far) when Head < Result_so_far ->
    io:format("2 called -> head:~w, result:~w~n", [Head, Result_so_far]),
    list_min(Rest, Head);

list_min([Head|Rest], Result_so_far)  ->
    io:format("3 called -> head:~w, result:~w~n", [Head, Result_so_far]),
    list_min(Rest, Result_so_far).


