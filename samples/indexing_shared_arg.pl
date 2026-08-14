% Dynamic-index regression probe.
%
% All clauses have the same first argument and differ only in later,
% ground arguments. A whole-head index performs an exact lookup; a
% first-argument-only index must retrieve every clause in the shared
% Arg1 bucket. This is deliberately a benchmark, not a pass/fail test.
%
% Run with:
%   time ./tpl -q -f -g main samples/indexing_shared_arg.pl

:- dynamic(record/3).

main :-
	populate(1, 10000),
	lookup(100000),
	write(ok), nl,
	!.

populate(I, Limit) :-
	I =< Limit,
	assertz(record(shared, I, value(I))),
	Next is I + 1,
	populate(Next, Limit).
populate(_, _).

lookup(0).
lookup(N) :-
	record(shared, 777, value(777)),
	Next is N - 1,
	lookup(Next).
