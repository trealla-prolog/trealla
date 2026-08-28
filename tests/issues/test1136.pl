% Issue #1136: copying a variable to an atomic term bound the source
% variable instead of unifying a fresh copy with the destination.

:- initialization(main).

main :-
	check(copy_term_variable, (copy_term(X, 3), var(X))),
	check(copy_term_nat_variable, (copy_term_nat(X, 3), var(X))),
	check(duplicate_term_variable, (duplicate_term(X, 3), var(X))),
	check(compound_source, (copy_term(f(X), f(3)), var(X))),
	check(atomic_source, (copy_term(3, X), X == 3)),
	check(compound_atomic_mismatch, \+ copy_term(f(_), 3)),
	halt.

check(Name, Goal) :-
	(   call(Goal)
	->  write(Name), write('_ok'), nl
	;   write('FAIL: '), write(Name), nl
	).
