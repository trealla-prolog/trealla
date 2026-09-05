:- initialization(main).

% float_integer_part/1 and float_fractional_part/1 truncated through a
% cast to int64, which is undefined behaviour once the value is outside
% that range - and 1.0e30 is. Every float at or above 2^53 is already a
% whole number, so the integer part is the value itself and the
% fractional part is 0.0.

t(X) :-
	I is float_integer_part(X),
	F is float_fractional_part(X),
	write(X-I-F), nl.

main :-
	t(1.0e30),
	t(-1.0e30),
	t(1.0e300),
	t(4.5),
	t(-4.5),
	t(0.0).
