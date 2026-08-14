:- initialization(main).

main :-
	catch((number_chars(_,"0_%0"),fail), _, write(ok1)), nl,
	catch((number_chars(_,"0x0_/*b"),fail), _, write(ok2)), nl,
	number_chars(N3,"0_\n3"), write(N3), nl,
	number_chars(N4,"0x0_\n4"), write(N4), nl,
	catch((number_chars(_,"0x0_\n%1"),fail), _, write(ok5)), nl,
	number_chars(N6,"0_%\n6"), write(N6), nl,
	number_chars(N7,"0_/**/\n7"), write(N7), nl.

