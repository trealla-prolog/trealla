:- initialization(main).
:- use_module(library(clpz)).

impossibilitas(X, Y) :- #X #> #Y, #Y #> #X.

main :-
	call_residue_vars(impossibilitas(X,Y),Vs),
	write_term(Vs, [variable_names(['X'=X, 'Y'=Y])]), nl.
