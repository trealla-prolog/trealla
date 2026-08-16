:- initialization(main).

main :-
	catch(length(_, 1_000_000_000), E1, true),
	E1 = error(resource_error(memory), _),
	catch(call_cleanup(length(_, 1_000_000_000), true), E2, true),
	E2 = error(resource_error(memory), _),
	catch(length(_, 1_000_000_000), E3, true),
	E3 = error(resource_error(memory), _),
	write(caught-memory-rearmed), nl,
	halt.
