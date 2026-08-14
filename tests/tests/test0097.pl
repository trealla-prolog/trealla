:- initialization(main).
:- use_module(library(dif)).

main :-
	dif([],A),L=A*L,L=L*A,A=a,
	!,
	write(nok), nl.
main :-
	write(ok), nl.

