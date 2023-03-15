:- module(concurrent, [
	future/3,
	future_all/2,
	future_any/2,
	future_done/1,
	await/2
	]).

:- use_module(library(apply)).
:- dynamic('$concurrent_count'/1).

'$concurrent_count'(0).

future(X, Goal, F) :-
	retract('$concurrent_count'(N)),
	N1 is N + 1,
	assertz('$concurrent_count'(N1)),
	F = N,
	Task0 = (Goal, send([F-X])),
	copy_term(Task0, Task),
	task(Task).

future_all(Fs, all(Fs)).
future_any(Fs, any(Fs)).

await(all(Fs), Vars) :-
	!,
	findall([F-X], (member(F, Fs), wait, recv([F-X])), Msgs),
	msort(Msgs, Msgs1),
	strip_prefix_(Msgs1, [], Xs),
	Vars = Xs.

await(any(Fs), Var) :-
	!,
	wait, recv([F-X]),
	member(F, Fs),
	!,
	Var = X.

await(F, Var) :-
	repeat,
		wait, recv([F-Var]),
		!.

strip_prefix_([], L0, L) :- reverse(L0, L).
strip_prefix_([[_-V]|Rest], Init, L) :-
	strip_prefix_(Rest, [V|Init], L).
