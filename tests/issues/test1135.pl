% Issue #1135: copy_term/2 stamped every variable it produced as "local",
% and unify_var()'s occurs-check gate skips the cyclic-term scan for local
% variables. Since run_quads calls copy_term on each query before running
% it, unify_with_occurs_check/2 silently stopped occurs-checking anything
% run through quads (or findall/3, call_nth, etc.), letting a query that
% should fail on the occurs check succeed instead.

:- initialization(main).

direct ?- unify_with_occurs_check(X, a(X)).
   false.

nested ?- unify_with_occurs_check(X, f(a, g(X))).
   false.

self ?- unify_with_occurs_check(X, X).
   true.

acyclic ?- unify_with_occurs_check(X, a(Y)).
   X = a(Y).

main :-
	use_module(library(quads)),
	run_quads.
