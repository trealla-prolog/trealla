% call_task/N must pass its arguments to the task by value.
%
% The task is a query of its own, with its own frames. Arguments used to
% be copied with append_to_tmp(), which copies variable cells by
% reference - and those references point into the *spawning* query's
% frames, which the task cannot resolve. A bound variable sitting inside
% a compound therefore arrived unbound:
%
%     N = 7, call_task(p, '$future'(N))   ==>  task sees '$future'(_)
%
% A term written out with its values already in place survived, which is
% why this went unnoticed - only a variable *reference* was lost, not a
% value. library(concurrent) hit it squarely, since future/3 builds
% '$future'(N) with N bound, and left a NOTE about "a bug to do with
% passing variables in call_task/1" plus a write/read round trip through
% an atom to work around it. send/1 crosses the same query boundary and
% has always cloned.
%
% The task has to judge the term itself and report only a verdict. Send
% the term back instead and the test cannot fail: send/1 clones, and
% while the spawning query is still sitting in wait/0 its frames are
% intact, so a dangling reference quietly resolves on the way out.

:- initialization(main).

% Report a verdict, not the term: send/1 would clone Got on the way out
% and a lost binding would look intact by the time it is printed.

check(Id, Expect, Got) :-
	(	Got == Expect -> send(Id-ok)
	;	\+ ground(Got) -> send(Id-'binding lost in transit')
	;	send(Id-'wrong value')
	).

drain(L) :- drain_([], L0), msort(L0, L).
drain_(Acc, L) :- ( recv(X) -> drain_([X|Acc], L) ; L = Acc ).

report(Id-ok) :- !, format("~w: ok~n", [Id]).
report(Id-Bad) :- format("~w: FAILED ~q~n", [Id,Bad]).

main :-
	N = 7,
	A = f(42),
	Inner = g(N),
	call_task(check, int,       42,           42),
	call_task(check, atm,       foo,          foo),
	call_task(check, lit,       f(42),        f(42)),
	call_task(check, via_var,   f(42),        A),
	call_task(check, bound_var, '$future'(7), '$future'(N)),
	call_task(check, nested,    h(g(7),7),    h(Inner,N)),
	call_task(check, in_list,   [7,f(42),x],  [N,A,x]),
	wait,
	drain(L),
	maplist(report, L),
	vars.

% Cloning has to happen once over the whole goal. Doing it per argument
% restarts clone_term_to_tmp()'s variable generation each time, which
% lets unrelated variables collide - distinct ones alias, shared ones
% come apart - and that corruption reaches the point of a segfault.

alias(X, Y) :- X = 1, (var(Y) -> send(distinct-ok) ; send(distinct-aliased)).
shared(X, Y) :- X = 1, (Y == 1 -> send(shared-ok) ; send(shared-'came apart')).
ingoal(G, X) :- call(G), (X == 7 -> send(goal_var-ok) ; send(goal_var-'came apart')).

vars :-
	call_task(alias, _, _), wait, drain(L1), maplist(report, L1),
	call_task(shared, Z, Z), wait, drain(L2), maplist(report, L2),
	G = (W = 7), call_task(ingoal, G, W), wait, drain(L3), maplist(report, L3).
