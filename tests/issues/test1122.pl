% Issue #1122: printing the toplevel answer bindings for
%     C=[[]|C],A=[C|D],D=[D|A],B=[C|A].
% looped forever on B specifically. print_iso_list()'s spine walk only
% detected a cycle back to *this* iteration's start, or to a node still
% on the C call stack; a spine needing 2+ hops to return to an earlier
% node (B->A->D->A->D->...) was never caught, so it just cycled A,D,A,D
% forever. Fixed with the tortoise-and-hare walk (term_next/3) already
% used by skip_max_list/6.

:- initialization(main).

writes_ok(Term, Label) :-
	(   with_output_to(string(_), write(Term))
	->  write(Label), nl
	;   format("FAIL ~w: write failed~n", [Label])
	).

main :-
	C=[[]|C], A=[C|D], D=[D|A], B=[C|A],
	writes_ok(C, c_ok),
	writes_ok(A, a_ok),
	writes_ok(D, d_ok),
	writes_ok(B, b_ok),
	halt.
