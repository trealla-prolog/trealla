str --> "e".
str --> "a", str.

run :-
  length(S,66000),
  phrase(str,S),
  format("~s~n",[S]).

:- initialization(run).
