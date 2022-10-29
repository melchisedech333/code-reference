
-module(case1).
-export([convert/1]).

convert(Length) ->
    case Length of
        {centimeter, X} ->
            {inch, X / 2.54};
        {inch, Y} ->
            {centimeter, Y * 2.54}
    end.

% O mesmo programa pode ser escrito assim:
%
% convert_length({centimeter, X}) ->
%     {inch, X / 2.54};
% convert_length({inch, Y}) ->
%     {centimeter, Y * 2.54}.


