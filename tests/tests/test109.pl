:- initialization(main).

% Clauses with a var in an indexed arg must still be found once the
% predicate crosses the dynamic index threshold (500). The pre-existing
% clauses are indexed by the bulk-build loop in assert_commit(), which
% used to skip the is_var_in_first_arg check, and idx2 had no such check
% on either path.

:- dynamic(p/2).
:- dynamic(q/2).
:- dynamic(r/2).
:- dynamic(s/2).

% var Arg1, asserted before the threshold
setup_p :- assertz(p(_, varclause)), fail.
setup_p :- between(2,600,I), assertz(p(I,ground)), fail.
setup_p.

% var Arg1, asserted after the threshold
setup_q :- between(2,600,I), assertz(q(I,ground)), fail.
setup_q :- assertz(q(_, varclause)), fail.
setup_q.

% var Arg2, asserted before the threshold
setup_r :- assertz(r(vc, _)), fail.
setup_r :- between(2,600,I), assertz(r(I,I)), fail.
setup_r.

% var Arg2, asserted after the threshold
setup_s :- between(2,600,I), assertz(s(I,I)), fail.
setup_s :- assertz(s(vc, _)), fail.
setup_s.

main :-
	setup_p, setup_q, setup_r, setup_s,
	findall(Y, p(7,Y), LP), write(LP), nl,
	findall(Y, q(7,Y), LQ), write(LQ), nl,
	findall(X, r(X,7), LR), write(LR), nl,
	findall(X, s(X,7), LS), write(LS), nl.
