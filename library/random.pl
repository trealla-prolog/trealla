:- module(random, [
	maybe/0, maybe/1, maybe/2,
	random_integer/2,
	set_random/1]).

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

%% random_integer(+Lower, +Upper, -R).
%
% Generates a random integer number between Lower (inclusive) and Upper (exclusive).
%
% Throws `instantiation_error` if Lower or Upper are variables.
%
% Throws `type_error` if Lower or Upper aren't integers.
random_integer(Lower, Upper, R) :-
    var(R),
    (   (var(Lower) ; var(Upper)) ->
        instantiation_error(random_integer/3)
    ;   \+ integer(Lower) ->
        type_error(integer, Lower, random_integer/3)
    ;   \+ integer(Upper) ->
        type_error(integer, Upper, random_integer/3)
    ;   Upper > Lower,
        random(R0),
        R is floor((Upper - Lower) * R0 + Lower)
    ).

rnd(N, R) :-
    rnd_(N, 0, R).

rnd_(0, R, R) :- !.
rnd_(N, R0, R) :-
    maybe,
    !,
    N1 is N - 1,
    rnd_(N1, R0, R).
rnd_(N, R0, R) :-
    N1 is N - 1,
    R1 is R0 + 1.0 / 2.0 ^ N,
    rnd_(N1, R1, R).
