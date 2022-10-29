
-module(list1).
-export([len/1]).

len([]) ->
    0;

len([First | Rest]) ->
    1 + len(Rest).

% Test:
%   41> list1:len([1,2,3,4,5,6,7,8,9]).
%   9


