:- initialization(main).

% Issue #1095: length/2 must call resource_error/1, not resource_error/2.

main :-
	catch(length(L, L), E1, true),
	E1 = error(resource_error(finite_memory), _),
	write(length_L_L-ok), nl,
	catch((L2 = [a|X], length(L2, X)), E2, true),
	E2 = error(resource_error(finite_memory), _),
	write(length_partial-ok), nl,
	halt.
