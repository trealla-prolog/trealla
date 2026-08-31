% An error raised inside an engine's goal unwinds the engine query to
% its bottom barrier, and that barrier was pushed before the goal was
% installed - so it restores a NULL instruction pointer. start() then
% ran proceed() on it and segfaulted. Any builtin-thrown error did it;
% an explicit throw/1 happened to survive.
%
% The error is reported and engine_next/2 just fails, which is what it
% did for throw/1 before. What matters here is that execution carries
% on at all - a pass needs both the output and a clean exit.

:- initialization(main).

check(Name, Goal) :-
	(	catch(call(Goal), E, (write(Name), write(' THREW '), writeq(E), nl, fail))
	->	write(Name), write(' ok'), nl
	;	write(Name), write(' FAILED'), nl
	).

% engine_next/2 fails rather than crashing, and the engine is still
% well enough formed to destroy

erring(Goal) :-
	engine_create(x, Goal, E),
	\+ engine_next(E, _),
	engine_destroy(E).

evaluable :- erring(_ is foo + 1).

type :- erring(atom_length(1, _)).

no_data :- erring(engine_fetch(_)).

explicit :- erring(throw(boom)).

% an answer already yielded is still delivered, and the error on the
% way to the next one does not take the process down

after_yield :-
	engine_create(x, (engine_yield(one), _ is bar + 1), E),
	engine_next(E, A),
	A == one,
	\+ engine_next(E, _),
	engine_destroy(E).

main :-
	check(evaluable, evaluable),
	check(type, type),
	check(no_data, no_data),
	check(explicit, explicit),
	check(after_yield, after_yield).
