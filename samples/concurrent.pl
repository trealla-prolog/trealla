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
	Goal2 = [F,X,Goal],
	Task0 = (Goal2=[F,X2,GoalX], call(GoalX), send([F-X2])),
	copy_term(Task0, Task),
	task(Task).

future_all(Fs, all(Fs)).
future_any(Fs, any(Fs)).

await(all(Fs), Vars) :-
	!,
	wait,
	findall([F-Msg], (member(F, Fs), recv([F-Msg])), L0),
	msort(L0, Msgs),
	strip_prefix_(Msgs, [], L),
	L = Vars.

await(any(Fs), Var) :-
	!,
	wait,
	recv([F-Msg]),
	member(F, Fs),
	!,
	Var = Msg.

await(F, Var) :-
	repeat,
		wait,
		recv([F-Var]),
		!.

strip_prefix_([], L0, L) :- reverse(L0, L).
strip_prefix_([[_-V]|Rest], Init, L) :-
	strip_prefix_(Rest, [V|Init], L).
