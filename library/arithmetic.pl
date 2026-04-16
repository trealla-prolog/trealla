:- module(arithmetic, [
		lsb/2, msb/2, popcount/2,
		number_to_rational/2,
		number_to_rational/3,
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

%% number_to_rational(+Real, -Fraction).
%
% True iff given a number Real, Fraction is the same number represented as a fraction.
number_to_rational(Real, Fraction) :-
    (   var(Real) -> instantiation_error(number_to_rational/2)
    ;   integer(Real) -> Fraction is Real rdiv 1
    ;   (rational(Real) ; float(Real)) ->
            number_to_rational(1.0e-6, Real, Fraction)
    ;   type_error(number, Real, number_to_rational/2)
    ).

% If 0 <= Eps0 <= 1e-16 then the search is for "infinite" precision.
number_to_rational(Eps0, Real0, Fraction) :-
    (   var(Eps0) -> instantiation_error(number_to_rational/3)
    ;   \+ number(Eps0) -> type_error(number, Eps0, number_to_rational/3)
    ;   Eps0 < 0 -> domain_error(not_less_than_zero, Eps0, number_to_rational/3)
    ;   Eps_ is Eps0 rdiv 1,
        rational_numerator_denominator(Eps_, EpsN, EpsD),
        Eps = EpsN/EpsD
    ),
    (   var(Real0) -> instantiation_error(number_to_rational/3)
    ;   \+ number(Real0) -> type_error(number, Eps0, number_to_rational/3)
    ;   Real_ is Real0 rdiv 1,
        rational_numerator_denominator(Real_, RealN, RealD),
        Real = RealN/RealD
    ),
    E0/E1 = Eps,
    P0/Q0 = Real,
    (   P0 < 0 -> I1 is -1 + P0 // Q0
    ;   I1 is P0 // Q0
    ),
    P1 is P0 mod Q0,
    Q1 = Q0,
    (   P1 =:= 0 -> Fraction is I1 + 0 rdiv 1
    ;   Qn1n is max(P1 * E1 - Q1 * E0, 0),
        Qn1d is Q1 * E1,
        Qn1 = Qn1n/Qn1d,
        Qp1n is P1 * E1 + Q1 * E0,
        Qp1d = Qn1d,
        Qp1 = Qp1n/Qp1d,
        stern_brocot_(Qn1, Qp1, 0/1, 1/0, P2/Q2),
        Fraction is I1 + P2 rdiv Q2
    ),
    !.

stern_brocot_(Qnn/Qnd, Qpn/Qpd, A/B, C/D, Fraction) :-
    Fn1 is A + C,
    Fd1 is B + D,
    simplify_fraction(Fn1/Fd1, Fn/Fd),
    S1 is sign(Fn * Qnd - Fd * Qnn),
    S2 is sign(Fn * Qpd - Fd * Qpn),
    (   S1 < 0 -> stern_brocot_(Qnn/Qnd, Qpn/Qpd, Fn/Fd, C/D, Fraction)
    ;   S2 > 0 -> stern_brocot_(Qnn/Qnd, Qpn/Qpd, A/B, Fn/Fd, Fraction)
    ;   Fraction = Fn/Fd
    ).

simplify_fraction(A0/B0, A/B) :-
    G is gcd(A0, B0),
    A is A0 // G,
    B is B0 // G.

:- help(number_to_rational(+number,-rational), [iso(false)]).
