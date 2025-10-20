:- initialization(main).

main :-
	catch((number_chars(N,"0_%0"),fail), _, write(ok1)), nl,
	catch((number_chars(N,"0x0_/*b"),fail), _, write(ok2)), nl.
