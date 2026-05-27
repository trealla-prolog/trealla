:- initialization(main).
:- use_module(library(clpb)).

% Issue #338: sat(X*Y + X*Z), labeling([X,Y,Z]) must report all three
% solutions (previously the first was missing).
main :-
	forall((sat(X*Y + X*Z), labeling([X,Y,Z])),
	       format("~w~n", [[X,Y,Z]])).
