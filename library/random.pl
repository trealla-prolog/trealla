:- module(random, [maybe/0, maybe/1, maybe/2, set_random/1]).

set_random(seed(random)) :-
	wall_time(Seed),
	set_seed(Seed),
	!.
set_random(seed(Seed)) :-
	set_seed(Seed).

maybe(K, N) :-
	P is K / N,
	random(F),
	F < P.

maybe(P) :-
	random(F),
	F < P.

maybe :-
	random(F),
	F < 0.5.

