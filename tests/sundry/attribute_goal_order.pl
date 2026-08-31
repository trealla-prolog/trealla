:- initialization(main).

% Goals returned by verify_attributes/3 must run in the order they were
% returned. modularize/4 in library/builtins.pl built its result with an
% accumulator it prepended to, so a module's goals ran back to front.
% Found while chasing issue #1127; the reversal was observable to any
% constraint library whose goals are order-sensitive.

:- use_module(library(atts)).
:- attribute ord/1.

verify_attributes(Var, _, Goals) :-
	(	get_atts(Var, +ord(N))
	->	goals_for(N, Goals)
	;	Goals = []
	).

goals_for(order, [w(1), w(2), w(3)]).
goals_for(veto, [w(a), w_fails(b), w(c)]).

w(X) :- write(X).

% A goal that fails vetoes the unification, so the goals after it must
% not run - the order matters for that too.
w_fails(X) :- write(X), fail.

main :-
	write('order: '),
	(	( put_atts(X, +ord(order)), X = bound )
	->	true
	;	write('*** unification unexpectedly failed')
	),
	nl,
	write('veto:  '),
	(	( put_atts(Y, +ord(veto)), Y = bound )
	->	write('*** unification unexpectedly succeeded')
	;	write(' vetoed')
	),
	nl.
