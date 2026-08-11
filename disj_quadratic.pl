% Minimal repro: a recursive predicate with ANY goal after the recursive
% call is quadratic here, where the same logic with the call genuinely
% last is linear.
%
%   tpl -g halt disj_quadratic.pl
%
% All four predicates below compute the same thing - walk a list to its
% end, nondeterministically - and are driven identically. Only the shape
% of the definition differs:
%
%   dots_disj    one clause, body disjunction   (A=B ; A=[_|C], ...)
%   dots_clauses two clauses, body unification
%   dots_head    two clauses, head unification
%   dots_trail   two clauses, plus a trailing `true` after the recursion
%
% Expected: dots_clauses and dots_head flat in n; dots_disj and
% dots_trail roughly quadrupling when n doubles.
%
% dots_trail is the one that matters. It contains no disjunction at all
% and is just as slow, so the disjunction is not the cause - `;` merely
% compiles to `$succeed_on_retry, LHS, $jump, RHS, true`, and that
% landing `true` is a goal after the recursive call. Hand dots_trail to
% other systems rather than dots_disj.
%
% Not universal: Scryer runs dots_disj at 1.06x its two-clause form and
% SWI shows no difference. Trealla is 300-700x.
%
% Nothing here involves DCGs, strings or library code. It was found via
% ...//0 in library(dcgs), which was defined as `[] | [_], ...` and so
% compiled to exactly that shape; that has since been rewritten as two
% rules, which is a workaround and not a fix.
%
% Full diagnosis - including what the cost is NOT, and why the two
% obvious fixes fail - is in docs/tco-then-branch-report.md.

:- initialization(main).

dots_disj(A, B) :-
    (   A = B
    ;   A = [_|C],
        dots_disj(C, B)
    ).

dots_clauses(A, B) :- A = B.
dots_clauses(A, B) :- A = [_|C], dots_clauses(C, B).

dots_head(A, A).
dots_head([_|C], B) :- dots_head(C, B).

% The disjunction turns out NOT to be special. Two clauses with a single
% no-op goal AFTER the recursive call reproduce it exactly - and that is
% what `;` compiles to: `$succeed_on_retry, LHS, $jump, RHS, true`, where
% the landing `true` is the jump target and sits after the RHS. So the
% recursive call is never last, every frame keeps leftover work, and each
% return executes one goal per frame all the way out.

dots_trail(A, B) :- A = B.
dots_trail(A, B) :- A = [_|C], dots_trail(C, B), true.

% Force the full search. The second argument is left UNBOUND and tested
% afterwards, so the caller backtracks into every intermediate choice
% point before reaching the one where the remainder is [].
%
% This matters: calling P(L, []) directly instead prunes the search and
% all three forms then look linear. The cost only appears on re-entry.

drive(P, L) :-
    G =.. [P, L, R],
    call(G),
    R == [].

mklist(0, []) :- !.
mklist(N, [x|T]) :- M is N - 1, mklist(M, T).

bench(P, N) :-
    mklist(N, L),
    statistics(runtime, [T0,_]),
    (   drive(P, L) -> true ; true ),
    statistics(runtime, [T1,_]),
    Ms is T1 - T0,
    format("  ~w~t~14| n=~w~t~24| ~w ms~n", [P, N, Ms]).

sizes([5000, 10000, 20000, 40000]).

run(P) :-
    sizes(Ns),
    forall(member(N, Ns), bench(P, N)),
    nl.

main :-
    format("in-body disjunction vs equivalent clause forms~n~n"),
    run(dots_disj),
    run(dots_clauses),
    run(dots_head),
    run(dots_trail).
