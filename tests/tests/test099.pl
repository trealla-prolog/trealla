:- initialization(run).
:- use_module(library(when)).

run :-
	when(nonvar(A), Run1 = true),
	when(ground(A), Run2 = true),
	var(Run1), var(Run2),
	A = a(B),
	Run1 == true, var(Run2),
	B = 1,
	Run2 == true,
	write(ok), nl.

