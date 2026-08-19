% Issue #989: findall/3 could not represent a cyclic term in its queue -
% the clone has no frame slots to hang a cycle from, so it emitted a
% fresh variable where the back-edge was - and the collected term quietly
% stopped being cyclic. bagof/3, which is built on findall/3, then
% reported answers that looked plausible and were wrong.
%
% Collecting a cyclic term is now refused, which is the position
% library(builtins) already takes for bagof/3 and setof/3 via their
% acyclic_term(G) check.
%
% The culprit term is never printed: write/1 has no cycle detection.

:- initialization(main).

throws(Goal, Label) :-
	(  catch(Goal, error(type_error(acyclic_term, _), _), (writeln(Label), fail))
	-> format("FAIL ~w: no error~n", [Label])
	;  true
	).

main :-
	% the cycle is created inside the goal
	throws(findall(X1, X1=p(X1), _), findall_inside),

	% and when it already existed beforehand. This one used to come back
	% intact, by accident of the reference outliving the copy; it is
	% refused too rather than have findall/3 depend on which frame the
	% cycle happens to sit in.
	Y = p(Y),
	throws(findall(V, member(V,[Y]), _), findall_existing),

	% a cyclic list spine
	throws(findall(X2, X2=[a|X2], _), findall_list),

	% findnsols/4 collects through the same queue
	throws(findnsols(5, X3, X3=p(X3), _), findnsols),

	% the issue's own cases, via bagof/3
	throws(bagof(X4, Y4^member(Y4-X4,[X4-p(Y4)]), _), bagof_case1),
	throws(bagof(X5, Y5^(Y5-X5=X5-p(Y5)), _), bagof_case2),
	throws(bagof(X6, Y6^(Y6-X6=X6-p(q(Y6))), _), bagof_case3),

	% acyclic collecting is untouched
	findall(N, member(N,[a,b,c]), Ns),
	(  Ns == [a,b,c] -> writeln(acyclic_findall) ; writeln('FAIL acyclic findall') ),
	findall(A-B, (member(A,[1,2]), B=f(A)), Ps),
	(  Ps == [1-f(1), 2-f(2)] -> writeln(acyclic_compound) ; writeln('FAIL acyclic compound') ),
	bagof(C, member(C,[x,y]), Cs),
	(  Cs == [x,y] -> writeln(acyclic_bagof) ; writeln('FAIL acyclic bagof') ),
	findnsols(5, D, member(D,[p,q]), Ds),
	(  Ds == [p,q] -> writeln(acyclic_findnsols) ; writeln('FAIL acyclic findnsols') ).
