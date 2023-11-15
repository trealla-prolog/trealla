str --> "e".
str --> "a", str.

run :-
  length(S,66000), % + '\n' in format/2 = 65544.
  phrase(str,S),
  format("~s~n",[S]).

:- initialization(run).
