:- initialization(main).

% copy_term/2 of a cyclic term is a cyclic term with as many variables
% as the original, not an acyclic unrolling of it with an invented
% variable standing for the back-reference (issue #1002). The cycle of
% cyclic/1 below goes through the callee's variable, which is the same
% variable as the caller's without being the same cell.

cyclic(X) :- X = f(g(X,_),_).
cyclic_ground_list(L) :- L = [a|L].
cyclic_partial_list(L) :- L = [_|L].

wrap(X, Y) :- copy_term(X, Y).

% Sharing is not a cycle, however much it looks like one from inside the
% copier: every activation of dag/2 builds its term out of the same cells
% of the same clause, and only the context tells the depths apart.

dag(0, x) :- !.
dag(N, f(T,T)) :- N1 is N-1, dag(N1, T).

same_vars_cyclic(X, Y) :-
	term_variables(X, XVs), length(XVs, N),
	term_variables(Y, YVs), length(YVs, N),
	\+ acyclic_term(Y).

main :-
	cyclic(A), wrap(A, A2),
	( same_vars_cyclic(A, A2), variant(A, A2) -> write(wrap-ok) ; write(wrap-fail) ), nl,
	cyclic_ground_list(G), copy_term(G, G2),
	( same_vars_cyclic(G, G2) -> write(glist-ok) ; write(glist-fail) ), nl,
	cyclic_partial_list(P), copy_term(P, P2),
	( same_vars_cyclic(P, P2) -> write(plist-ok) ; write(plist-fail) ), nl,
	dag(3, D), copy_term(D, D2),
	( acyclic_term(D2), variant(D, D2) -> write(dag-ok) ; write(dag-fail) ), nl,
	X = f(X), copy_term(X, Y),
	( \+ acyclic_term(Y) -> write(fx-ok) ; write(fx-fail) ), nl,
	halt.
