:-initialization(main).
:- use_module(library(when)).

main :-
	freeze(X,(integer(X),X=Y)),
	freeze(Z,(when(nonvar(Y),((freeze(_,(I,_)))=Z)))),
	frozen(X,Z),
	member(X,[1]),
	!.
