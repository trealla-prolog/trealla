:- use_module(library(dif)).
:- initialization(main).

main :-
	\+ (A=[]*B,B=C*D,D=C*C,A=B),
	\+ (A=[]*B,B=C*C*C,A=B),
	write(ok), nl.
