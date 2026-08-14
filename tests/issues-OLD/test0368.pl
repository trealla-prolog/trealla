main :-
	writeq((1*2)/(3*4)), nl,
	writeq(-(-a)), nl,
	writeq(-(1)), nl,
	writeq(-(-1)), nl,
	writeq(-(-(-a))), nl,
	writeq(-(-(1))), nl,
	writeq(-(-1- -1)), nl,
	writeq(-(-1- +1)), nl,
	writeq(-(-1- 1)), nl,
	writeq(-(-(-a))), nl,
	writeq(+(+1)), nl,
	writeq(+(+)), nl,
	writeq(-(-)), nl,
	writeq(-(-1- +a)), nl,
	writeq(-(-1- -a)), nl,
	true.

:- initialization(main).
