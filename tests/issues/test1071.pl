:- initialization(main).

% A quad may be labelled with a ground term identifying the query, so
% '?-' is an infix operator as well as a prefix one. The label is not a
% clause head: nothing is added to the database, and the answer
% description that follows is not loaded as a clause either.

member_1 ?- member(X, [1,2]).
   X = 1
;  X = 2.

member_2 ?- member(X, [1,2]).
   X = 1
;  X = 99.

main :-
	forall(current_op(P, T, ?-), (write(op(P,T)), nl)),
	write('(?-)/2: '),
	(	catch(predicate_property('?-'(_,_), _), _, fail)
	->	write(defined)
	;	write(undefined)
	),
	nl,
	use_module(library(quads)),
	run_quads.
