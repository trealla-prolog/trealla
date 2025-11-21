:- initialization(main).

main :-
	L1=("a"||.),
	writeq(L1), nl,
	L2=("a"||...),
	writeq(L2), nl,
	L3= ""||'.' ,
	writeq(L3), nl,
	L4= "a"||'b c' ,
	writeq(L4), nl,
	L5= "a"||"b c" ,
	write_term(stdout,L5,[double_quotes(true)]), nl.

