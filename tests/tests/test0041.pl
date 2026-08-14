:- initialization(main).

list(0,L) :- L = [].
list(N,L) :- N1 is N - 1, list(N1,L1), L = [c|L1].

main :- list(5000,L), atom_chars(A,L), write(A), nl.
