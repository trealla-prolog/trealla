% Printing a cyclic term under a max_depth cutoff reaches for the name
% of the variable it elides, in the parser's variable table. Only the
% query that parsed the goal has one: an engine (or a thread) has a NULL
% q->top, and the lookup crashed. It is reached from throw_error() too,
% which clamps depth to 10, so any error whose culprit was cyclic took
% the process down.

:- initialization(main).

check(Name, Goal) :-
	(	catch(call(Goal), E, (write(Name), write(' THREW '), writeq(E), nl, fail))
	->	write(Name), write(' ok'), nl
	;	write(Name), write(' FAILED'), nl
	).

% write_term/2 with an explicit cutoff, inside an engine

in_engine(Goal) :-
	engine_create(done, Goal, E),
	engine_next(E, done),
	engine_destroy(E).

written :-
	in_engine((X = f(X), write_term(X, [max_depth(5)]), nl)).

% and the same term as an error culprit, where the cutoff comes from
% throw_error() rather than from write options

thrown :-
	in_engine((X = f(X), catch(atom_length(X, _), error(type_error(atom, _), _), true))).

% a cyclic list takes a different path through the printer

list :-
	in_engine((L = [a|L], write_term(L, [max_depth(5)]), nl)).

main :-
	check(written, written),
	check(thrown, thrown),
	check(list, list).
