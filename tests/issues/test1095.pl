:- initialization(main).

% Issue #1095: length/2 must throw with context length/2 (as Scryer does),
% via resource_error/2 — not existence_error for a missing resource_error/2.

main :-
	catch(length(L, L), E1, true),
	E1 = error(resource_error(finite_memory), length/2),
	write(length_L_L-ok), nl,
	catch((L2 = [a|X], length(L2, X)), E2, true),
	E2 = error(resource_error(finite_memory), length/2),
	write(length_partial-ok), nl,
	halt.
