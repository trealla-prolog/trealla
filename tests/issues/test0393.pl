:- initialization(main).
:- use_module(library(dif)).

main :-
	dif(A,B), A=C*B, C = c, B = b,
	!,
	write(ok), nl.
main :-
	write(nok), nl.
