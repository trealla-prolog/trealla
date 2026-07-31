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

report(Label, X, Y) :-
	term_variables(X, XVs), length(XVs, NX),
	term_variables(Y, YVs), length(YVs, NY),
	( acyclic_term(Y) -> Kind = acyclic ; Kind = cyclic ),
	write(Label), write(': '), write(NX-NY), write(' '), write(Kind), nl.
