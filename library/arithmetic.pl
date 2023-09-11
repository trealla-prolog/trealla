:- module(arithmetic, [
		lsb/2, msb/2, popcount/2,
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

