:- initialization(main).

% The THEN branch of an if-then-else is a tail position. In
% (C -> T ; E) the last goal of T is followed only by the jump past E to
% the landing that ends the clause, so it can reuse the frame - but the
% compile-time recursive-call flag was set by asking whether a goal's
% cells end where the clause's cells end, and in the term T is followed
% by the whole of E. Only E was ever marked, so every if-then-else
% recursed through its THEN branch by growing the frame stack.
%
% Half of this test walks that branch deeply and checks the frame count
% stays flat. The other half is what enabling it exposed:
% commit_frame() has to see the choicepoints an earlier goal of the same
% clause left behind, or the tail call recycles the frame they need and
% solutions go missing.

depth(300000).

then_loop(N)   :- ( N > 0 -> M is N-1, then_loop(M) ; frames(then_loop) ).
soft_loop(N)   :- ( N > 0 *-> M is N-1, soft_loop(M) ; frames(soft_loop) ).
nested_loop(N) :- ( N > 0 -> ( N > 5 -> M is N-1, nested_loop(M)
                             ; M is N-1, nested_loop(M) )
                  ; frames(nested_loop) ).

frames(Name) :-
	statistics(frames, F),
	(  F < 100
	-> format("~w-constant~n", [Name])
	;  format("~w-grew~n", [Name])
	).

% An accumulator carried down the THEN branch must still arrive.

sum_then(N, A, S) :- ( N > 0 -> A1 is A+N, M is N-1, sum_then(M, A1, S) ; S = A ).

% Nondeterminism left behind by an earlier goal, in a plain body and in
% a THEN branch, hard and soft.

p(0) :- !.
p(N) :- between(1,2,_), M is N-1, p(M).

q(0) :- !.
q(N) :- ( N > 0 -> between(1,2,_), M is N-1, q(M) ; true ).

r(N, []) :- N =< 0, !.
r(N, [X|L]) :- ( N > 0 -> member(X, [a,b]), M is N-1, r(M, L) ; L = [] ).

s(0) :- !.
s(N) :- ( between(1,2,_) *-> M is N-1, s(M) ; true ).

solutions(Name, G, Expected) :-
	findall(x, call(G), L),
	length(L, Got),
	(  Got =:= Expected
	-> format("~w-~d~n", [Name,Got])
	;  format("~w-~d-expected-~d~n", [Name,Got,Expected])
	).

main :-
	depth(N),
	then_loop(N),
	soft_loop(N),
	nested_loop(N),
	sum_then(N, 0, S),
	Expected is N*(N+1)//2,
	(  S =:= Expected
	-> format("sum_then-ok~n")
	;  format("sum_then-~d-expected-~d~n", [S,Expected])
	),
	solutions(between_plain, p(3), 8),
	solutions(between_then, q(3), 8),
	solutions(member_then, r(3,_), 8),
	solutions(softcut_then, s(3), 8).
