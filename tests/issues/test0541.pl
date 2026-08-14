:- initialization(main).
:- op(0,yfx,-).
:- op(0,fy,--).

main :-
	X = -(---(_A),-_B),
	Y = -(- --(_A),-_B),
	write_term([X,Y], [variable_names(['A'=_A, 'B'=_B])]), nl.
