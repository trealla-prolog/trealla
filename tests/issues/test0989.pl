% Issue #989: a cyclic term built inside findall's goal came back with
% the cycle broken - the back-edge replaced by a fresh variable - so
% bagof/3 reported answers that were quietly wrong.
%
% The terms are never printed: write/1 has no cycle detection and would
% unroll one of these until it hit the depth cap.

:- initialization(main).

cyclic(T) :- \+ acyclic_term(T).

main :-
	% reduced: the cycle is created inside the goal
	findall(X, X=p(X), [C1]),
	(  cyclic(C1) -> writeln(findall_keeps_cycle) ; writeln('FAIL findall') ),

	% and when it already existed beforehand
	Y = p(Y),
	findall(V, member(V,[Y]), [C2]),
	(  cyclic(C2) -> writeln(findall_keeps_existing) ; writeln('FAIL existing') ),

	% two independent cycles in one solution
	findall(f(A,B), (A=p(A), B=q(B)), [f(C3,C4)]),
	(  cyclic(C3), cyclic(C4) -> writeln(two_cycles_ok) ; writeln('FAIL two cycles') ),

	% a cyclic list, whose spine is walked by a different path
	findall(L, L=[a|L], [C5]),
	(  cyclic(C5) -> writeln(cyclic_list_ok) ; writeln('FAIL list') ),

	% the issue's own cases, via bagof
	bagof(X1, Y1^member(Y1-X1,[X1-p(Y1)]), Z1),
	Z1 = [p(p(p(ZZ1)))],
	(  cyclic(ZZ1) -> writeln(bagof_case1_ok) ; writeln('FAIL bagof 1') ),

	bagof(X2, Y2^(Y2-X2=X2-p(Y2)), Z2),
	Z2 = [p(p(p(ZZ2)))],
	ZZ2 =.. [F2|_],
	(  F2 == p -> writeln(bagof_case2_ok) ; writeln('FAIL bagof 2') ),

	bagof(X3, Y3^(Y3-X3=X3-p(q(Y3))), Z3),
	Z3 = [p(ZZ3)],
	(  cyclic(ZZ3) -> writeln(bagof_case3_ok) ; writeln('FAIL bagof 3') ),

	% the chain from the issue's follow-up comment must stay consistent
	bagof(X4, Y4^(Y4-X4=X4-p(q(Y4))), Z4),
	Z4 = [p(ZZ4)], ZZ4 =.. L4, [_,L2] = L4, [L2] = Z4, Z4 = [p(q(L2))],
	writeln(comment_chain_ok),

	% acyclic findall is untouched
	findall(N, member(N,[a,b,c]), Ns),
	(  Ns == [a,b,c] -> writeln(acyclic_unaffected) ; writeln('FAIL acyclic') ).
