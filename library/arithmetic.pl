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

/**
 * number_to_rational(X):
 * If X is a number then the function returns an approximate rational number.
 */
% number_to_rational(+Number, -Rational)
number_to_rational(F, C rdiv B) :- F < 0, !,
   H is -F,
   number_to_rational(H, A rdiv B),
   C is -A.
number_to_rational(F, R) :-
   rat_start2(F, V, W),
   divmod(V, W, D, U),
   rat_iter2(W rdiv U, D rdiv 1, 1 rdiv 0, F, R).

% rat_start2(+Number, -Integer, -Integer)
rat_start2(F, V, W) :-
   parts(F, M, E),
   (E < 0 ->
       V = M, W is 2^(-E);
       V is M*E^2, W = 1).

% rat_iter(+Rational, +Rational, +Rational, +Number, -Rational)
rat_iter2(_, X, _, Y, X) :- X =:= Y, !.
rat_iter2(_ rdiv 0, X, _, _, X) :- !.
rat_iter2(V rdiv W, M rdiv N, P rdiv Q, Y, X) :-
   divmod(V, W, D, U),
   R is D*M+P,
   S is D*N+Q,
   rat_iter2(W rdiv U, R rdiv S, M rdiv N, Y, X).

:- help(number_to_rational(+number,-rational), [iso(false)]).
