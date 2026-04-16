:- module(arithmetic, [
		lsb/2, msb/2, popcount/2,
		number_to_rational/2,
		rational_numerator_denominator/3
	]).

rational_numerator_denominator(R, N, D) :-
	N is numerator(R),
	D is denominator(R).

:- help(rational_numerator_denominator(+rational,-integer,-integer), [iso(false)]).

lsb(I, N) :-
	must_be(I, integer, lsb/2, _),
    N is lsb(I).

:- help(lsb(+integer,?integer), [iso(false)]).

msb(I, N) :-
	must_be(I, integer, msb/2, _),
    N is msb(I).

:- help(msb(+integer,?integer), [iso(false)]).

popcount(I, N) :-
	must_be(I, integer, popcount/2, _),
    N is popcount(I).

:- help(popcount(+integer,?integer), [iso(false)]).

number_to_rational(F, F) :-
	integer(F), !.
number_to_rational(F, R) :- F < 0, !,
   H is -F,
   number_to_rational(H, R0),
   rational_numerator_denominator(R0, A, B),
   C is -A,
   R is C rdiv B.
number_to_rational(F, R) :-
   rat_start_(F, V, W),
   divmod(V, W, D, U),
   rat_iter_(W rdiv U, D rdiv 1, 1 rdiv 0, F, R0),
   R is R0.

rat_start_(F, V, W) :-
   parts_(F, M, E),
   (E < 0 ->
       V = M, W is 2^(-E);
       V is M*E^2, W = 1).

rat_iter_(_, X, _, Y, X) :- X =:= Y, !.
rat_iter_(_ rdiv 0, X, _, _, X) :- !.
rat_iter_(V rdiv W, M rdiv N, P rdiv Q, Y, X) :-
   divmod(V, W, D, U),
   R is D*M+P,
   S is D*N+Q,
   rat_iter_(W rdiv U, R rdiv S, M rdiv N, Y, X).

parts_(F, M, E) :-
   logb_(F, G),
   E is G-52,
   U is -E,
   scalb_(F, U, N),
   M is truncate(N).

scalb_(M, E, R) :-
   R is M*2**E.

logb_(M, E) :-
   E is floor(log(M)/log(2)).

:- help(number_to_rational(+number,-rational), [iso(false)]).
