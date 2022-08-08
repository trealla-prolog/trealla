:- initialization(main).

as --> [].
as --> [a], as.

main :- phrase(as, Ls, []), write(Ls), nl, length(Ls, 5).
main.

