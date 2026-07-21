:- use_module(library(clpz)).
:- initialization(main).

% Goal expansion must not merge two distinct clause variables. Here the
% reification booleans of (I2 #= 2) and (R2 #\= 5) were given the same
% slot, so asserting one asserted the other and R2 = 1 was rejected.
% R2 = 1 is satisfiable: I2 = 0, so (I2 #= 2) is false, (R2 #\= 5) true.

with_extra :-
	_A #= 1 #<==> _B,
	_C #= 1 #<==> _D,
	I2 + 1 #= R2,
	I2 #= 2 #<==> _U3,
	R2 in 1..6,
	R2 #\= 5 #<==> _M4,
	R2 = 1.

without_extra :-
	I2 + 1 #= R2,
	I2 #= 2 #<==> _U3,
	R2 in 1..6,
	R2 #\= 5 #<==> _M4,
	R2 = 1.

distinct_booleans :-
	_A #= 1 #<==> _B,
	_C #= 1 #<==> _D,
	I2 + 1 #= R2,
	I2 #= 2 #<==> U3,
	R2 in 1..6,
	R2 #\= 5 #<==> M4,
	I2 = I2,
	U3 \== M4.

report(Name, Goal) :- ( call(Goal) -> write(Name-ok) ; write(Name-wrong) ), nl.

main :-
	report(with_extra, with_extra),
	report(without_extra, without_extra),
	report(distinct_booleans, distinct_booleans).
