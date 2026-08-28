% Issue #1133: optimized meta-calls with a variable argument must validate the
% complete dereferenced goal before executing any part of it. The empty output
% assertions ensure the invalid conjunction is rejected before write/1 runs.

call_var(A) :-
	Goal = (write(A), A),
	call(Goal).

once_var(A) :-
	Goal = (write(A), A),
	once(Goal).

ignore_var(A) :-
	Goal = (write(A), A),
	ignore(Goal).

call ?- call_var(33).
   outputs(""), type_error(callable,(write(33),33)).

once ?- once_var(33).
   outputs(""), type_error(callable,(write(33),33)).

ignore ?- ignore_var(33).
   outputs(""), type_error(callable,(write(33),33)).

:- initialization(main).

main :-
	use_module(library(quads)),
	run_quads.
