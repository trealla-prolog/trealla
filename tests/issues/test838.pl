:- initialization(main).
:- use_module(library(clpz)).

impossibilitas :- #X #> #Y, #Y #> #X.

main :-
	call_residue_vars(impossibilitas,Vs),
	write(Vs), nl.
