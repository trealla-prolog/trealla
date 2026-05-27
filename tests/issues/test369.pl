:- initialization(main).
:- use_module(library(clpb)).

% Issue #369: unifying two CLP(B) variables must succeed (previously
% sat(A+B), A=B failed). Also exercise equivalence and its labelings.
main :-
	( sat(A+B), A=B -> format("A=~w B=~w~n", [A,B]) ; write('unexpected failure'), nl ),
	findall(X-Y, (sat(X=:=Y), labeling([X,Y])), Sols),
	format("eqv: ~w~n", [Sols]).
