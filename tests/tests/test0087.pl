f(N) :- !, N > 0, N1 is N - 1, f(N1).
f(_) :- write(here), nl.

main :- f(1000000); write(ok), nl.

:- initialization(main).
