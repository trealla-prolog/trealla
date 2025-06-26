:- use_module(library(format)).
:- use_module(library(lists)).

str --> "e".
str --> "a", str.

run :-
  length(S,66000),
  phrase(str,S),
  format("~s~n",[S]).

:- initialization(run).
