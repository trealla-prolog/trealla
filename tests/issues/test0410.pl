:- use_module(library(dif)).
:- initialization(main).

main :-
	A=A*[],B=C*[],C=C*a,dif(A,B),
	write(ok), nl.
