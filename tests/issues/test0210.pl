:- initialization(main).
:- use_module(library(dif)).

main :-
	\+ (dif(A,B),A=[A|A],B=[B|B]).
