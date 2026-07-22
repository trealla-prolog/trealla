:- use_module(library(lists)).
:- use_module(library(iso_ext)).
:- use_module(library(dcgs)).
:- use_module(library(clpb)).
:- initialization(main).

% Issue #360: reduced harness from Triska's CLP(B) consistency test.
% The original threw domain_error(clpb_variable, ...) due to broken
% variable aliasing. Full sizes are intractable (same in Scryer), so we
% check the tractable sizes N=0 and N=1: every generated pair of
% formulas must yield identical solution sets via both solving orders.

perm([], []).
perm(List, [First|Perm]) :- select(First, List, Rest), perm(Rest, Perm).

f(_)  --> [].
f(X*Y) --> [_], f(X), f(Y).
f(X+Y) --> [_], f(X), f(Y).
f(X#Y) --> [_], f(X), f(Y).
f(card([0,1],[X,Y])) --> [_], f(X), f(Y).

vs_eqs(Vs, Eqs) :- phrase(vs_eqs(Vs), Eqs).
vs_eqs([]) --> [].
vs_eqs([V|Vs]) --> vs_eqs_(Vs, V), vs_eqs(Vs).
vs_eqs_([], _) --> [].
vs_eqs_([V|Vs], X) --> vs_eqs_(Vs, X), ( [X=V] ; [] ).

consistent(N) :-
	forall(( length(Ls, N),
	         phrase(f(S1), Ls), phrase(f(S2), Ls),
	         term_variables(S1-S2, Vs0), perm(Vs0, Vs), vs_eqs(Vs, Eqs) ),
	       ( findall(Vs, (sat(S1), sat(S2), maplist(call, Eqs), labeling(Vs)), A),
	         findall(Vs, (labeling(Vs), maplist(call, Eqs), sat(S1*S2)), B),
	         sort(A, S), sort(B, S) )).

main :-
	consistent(0), write('N=0 consistent'), nl,
	consistent(1), write('N=1 consistent'), nl.
