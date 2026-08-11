% Issue #1106: a recursive predicate with any goal after the recursive
% call was quadratic, where the same logic with the call genuinely last
% was linear.
%
% push_frame() already collapses the return chain when a call is last
% ("Avoid long chains of useless returns"), but tested only whether the
% very next cell was the clause end. compile_term() plants no-op landings
% - a bare `true`, or a forward `$jump` to one - after the last goal of a
% disjunction branch, so the test never fired there and every frame kept
% a distinct continuation: O(depth) per return, O(n^2) over O(n) returns.
%
% is_last_call() already knew how to walk past those landings for TCO.
% push_frame() now shares that walk.
%
% Timing is not asserted here - only that the four equivalent forms agree
% on their solutions, which is what the frame-chaining change could break.
% For the timings run disj_quadratic.pl in the repo root.
%
% https://github.com/trealla-prolog/trealla/issues/1106

:- initialization(main).

% Same relation, four shapes. Only dots_clauses/dots_head put the
% recursive call genuinely last.

dots_disj(A, B) :- ( A = B ; A = [_|C], dots_disj(C, B) ).

dots_clauses(A, B) :- A = B.
dots_clauses(A, B) :- A = [_|C], dots_clauses(C, B).

dots_head(A, A).
dots_head([_|C], B) :- dots_head(C, B).

dots_trail(A, B) :- A = B.
dots_trail(A, B) :- A = [_|C], dots_trail(C, B), true.

% Landings in other control constructs.

ite(A, B)  :- ( A = [_|C] -> ite(C, B) ; A = B ).
soft(A, B) :- ( A = [_|C] *-> soft(C, B) ; A = B ).
nest(A, B) :- ( ( A = B ; fail ) ; ( A = [_|C], nest(C, B) ; fail ) ).

% A real goal after the recursive call must still run.
after(A, B, N) :- ( A = B, N = 0 ; A = [_|C], after(C, B, M), N is M+1 ).

mk(0, []) :- !.
mk(N, [x|T]) :- M is N-1, mk(M, T).

t(N, G) :-
	(  catch(G, E, (format("~w: ERROR ~w~n", [N,E]), fail))
	-> format("~w: ok~n", [N])
	;  format("~w: FAILED~n", [N])
	).

main :-
	mk(50, L),
	findall(X, dots_disj(L,X), D1),
	findall(X, dots_clauses(L,X), D2),
	findall(X, dots_head(L,X), D3),
	findall(X, dots_trail(L,X), D4),
	findall(X, nest(L,X), D5),

	% all four forms: same solutions, same order
	t(disj_len,      length(D1, 51)),
	t(clauses_agree, D1 == D2),
	t(head_agree,    D1 == D3),
	t(trail_agree,   D1 == D4),
	t(nested_agree,  D1 == D5),

	% committed forms stay committed
	t(ite_det,       (findall(X, ite(L,X), I), I == [[]])),
	t(soft_det,      (findall(X, soft(L,X), S), S == [[]])),

	% a goal after the recursive call still runs
	t(after_goal,    (after(L, [], K), K =:= 50)),

	% deep, and on backtracking into every intermediate choicepoint
	t(deep_backtrack,(mk(2000, LL), dots_trail(LL, R), R == [])),
	t(deep_disj,     (mk(2000, LM), dots_disj(LM, R2), R2 == [])),

	% unwinding an exception through chained frames
	t(throw_unwind,  catch((dots_trail(L,_), throw(boom)), boom, true)),

	% The actual regression guard. Absolute times are machine dependent,
	% so compare the trailing-goal form against the genuinely-last-call
	% form at the same n: that ratio is ~1 when the return chain is
	% collapsed and was ~200-400x when it was not. The bound is loose on
	% purpose - it only has to separate O(n) from O(n^2).
	t(not_quadratic, ratio_ok(20000, 20)),

	true.

% Drive the full search: the second argument is left unbound and tested
% afterwards, so the caller backtracks into every intermediate choice
% point. Calling P(L,[]) directly prunes the search and hides the cost.

drive(P, L) :- G =.. [P,L,R], call(G), R == [].

ms(P, L, Ms) :-
	statistics(runtime, [T0,_]),
	( drive(P, L) -> true ; true ),
	statistics(runtime, [T1,_]),
	Ms is max(1, T1-T0).

ratio_ok(N, Bound) :-
	mk(N, L),
	ms(dots_clauses, L, Fast),
	ms(dots_trail, L, Slow),
	Ratio is Slow / Fast,
	(  Ratio =< Bound
	-> true
	;  format("  (dots_trail ~w ms vs dots_clauses ~w ms, ratio ~w)~n",
	          [Slow, Fast, Ratio]), fail
	).
