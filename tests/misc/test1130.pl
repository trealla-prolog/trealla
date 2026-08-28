% Issue #1130: advancing an engine past its final answer retried the plain
% bottom barrier. Its saved instruction pointer predated engine execution
% and was NULL, so start() dereferenced it instead of reporting exhaustion.
% Needs real threads, so this test belongs in tests/misc.

:- use_module(library(tabling)).
:- initialization(main).

:- table tabled/1.

tabled(X) :- between(1, 20, X).

exhaust(E) :-
	( engine_next(E, _) -> exhaust(E) ; true ).

main :-
	engine_create(x, true, E1),
	engine_next(E1, x),
	\+ engine_next(E1, _),
	engine_destroy(E1),
	write(engine_exhaustion_ok), nl,

	engine_create(Y, tabled(Y), E3),
	thread_create(exhaust(E3), T, []),
	thread_join(T, true),
	engine_destroy(E3),
	write(threaded_tabled_engine_ok), nl,
	halt.
