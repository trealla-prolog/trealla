:- initialization(main).

% A cut executed by a self-recursive activation must not escape the
% barrier of the control construct that called it.
%
% The last-call optimisation used to fire on the strength of the
% compile-time recursive-call flag alone. That flag marks any cell that
% ends at the clause end, which includes the argument of a trailing
% \+/1, once/1 or ignore/1 - goals those constructs do run, but with a
% continuation of their own planted after them. Reusing the frame threw
% that continuation away: \+ G lost its `!, $drop_barrier, fail' and so
% succeeded for a provable G, and ignore/1 lost its cut and left
% choice points behind.

fx(x).

igoal(fx(_)).
slv(G) :- igoal(G), !, call(G).

% A textbook meta-interpreter: conjunction, disjunction via cut plus
% if-then-else, and negation as failure. sb(yk_not(D)) runs \+ sb(D)
% from inside a clause of sb/1 itself, and the inner sb/1 activation
% commits with a cut in one of sb/1's own clauses.

sb((A , B)) :- !, sb(A), sb(B).
sb((A ; B)) :- !, (sb(A) -> true ; sb(B)).
sb(yk_not(A)) :- !, \+ sb(A).
sb(A) :- slv(A).

% The same thing stripped down to the bone.

mn(d(A,_)) :- !, mn(A).
mn(n(A)) :- !, \+ mn(A).
mn(A) :- fx(A).

% once/1 and ignore/1 in the same position must stay determinate.

nn(1).
nn(2).

on(0) :- nn(_).
on(N) :- M is N-1, once(on(M)).

ig(0) :- nn(_).
ig(N) :- M is N-1, ignore(ig(M)).

% Deep tail recursion in each shape whose last call must keep being
% optimised away, so a lost optimisation shows up as a crash here.

t1(0) :- !.
t1(N) :- M is N-1, t1(M).

t2(0) :- !.
t2(N) :- ( N < 0 -> true ; M is N-1, t2(M) ).

t3(0) :- !.
t3(N) :- ( N < 0, true ; M is N-1, t3(M) ).

t4(0) :- !.
t4(N) :- M is N-1, call(t4(M)).

t5(0) :- !.
t5(N) :- ( true -> M is N-1, t5(M) ).

% A self-recursive call/1 in NON-tail position must keep the rest of
% the clause, and a self-recursive goal under catch/3 must keep the
% handler reachable.

nt(0) :- !.
nt(N) :- N > 0, M is N-1, call(nt(M)), write(after), nl.

rc(0) :- throw(error_foo).
rc(N) :- N > 0, M is N-1, catch(rc(M), error_foo, (write(caught), nl)).

report(Label, Goal) :-
	( call(Goal) -> R = succeeded ; R = failed ),
	write(Label-R), nl.

main :-
	report(disj, sb((fx(x) ; fx(y)))),
	report(neg_of_true_disj, sb(yk_not((fx(x) ; fx(y))))),
	report(neg_toplevel, \+ sb((fx(x) ; fx(y)))),
	report(neg_of_true_atom, sb(yk_not(fx(x)))),
	report(neg_of_false, sb(yk_not(fx(y)))),
	report(minimal_neg_of_true, mn(n(d(x,y)))),
	report(minimal_neg_of_false, mn(n(d(y,y)))),
	findall(x, on(2), L1), write(once_solutions-L1), nl,
	findall(x, ig(2), L2), write(ignore_solutions-L2), nl,
	report(deep_plain, t1(1000000)),
	report(deep_if_then_else, t2(1000000)),
	report(deep_disjunction, t3(1000000)),
	report(deep_call, t4(1000000)),
	report(deep_if_then, t5(1000000)),
	report(nontail_call, nt(2)),
	report(catch_recursive, catch(rc(1), E, (write(escaped(E)), nl))).
