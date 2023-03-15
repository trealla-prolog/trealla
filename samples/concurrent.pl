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

future(Template, Goal, F) :-
	retract('$concurrent_count'(N)),
	N1 is N + 1,
	assertz('$concurrent_count'(N1)),
	F = N,
	Task0 = (Goal, send([F-Template])),
	copy_term(Task0, Task),
	write_term_to_atom(A, Task, []),
	task(callgoal(A)).

future_all(Fs, all(Fs)).
future_any(Fs, any(Fs)).

await(all(Fs), Templates) :-
	!,
	findall([F-Template], (member(F, Fs), wait, recv([F-Template])), Msgs),
	msort(Msgs, Msgs1),
	strip_prefix_(Msgs1, [], Templates0),
	Templates = Templates0.

await(any(Fs), Template) :-
	!,
	wait, recv([F-Template0]),
	member(F, Fs),
	!,
	Template = Template0.

await(F, Template) :-
	repeat,
		wait, recv([F-Template]),
		!.

strip_prefix_([], L0, L) :- reverse(L0, L).
strip_prefix_([[_-V]|Rest], Init, L) :-
	strip_prefix_(Rest, [V|Init], L).

callgoal(A) :-
	read_term_from_atom(A, T, []),
	T.
