:- initialization(main).

% Issue #1002: copy_term/2 invents an extra variable for a cyclic term,
% which then breaks variant/2.

test(X) :- X = f(g(X, _), _).

main :-
	test(X),
	term_variables(X, L1),
	length(L1, 2),
	copy_term(X, Y),
	term_variables(Y, L2),
	length(L2, N),
	(	N =:= 2
	->	write(ok), nl
	;	write(extra_vars), write(' '), write(N), nl
	),
	(	variant(X, Y)
	->	write(variant_ok), nl
	;	write(variant_fail), nl
	).
