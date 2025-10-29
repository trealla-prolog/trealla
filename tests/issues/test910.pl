:- initialization(main).

main :-
	catch((read_from_chars("[1\n(2)].",_),fail),_,true),
	%catch((read_from_chars("{1\"\"||_}.",_),fail),_,true),
	catch((read_from_chars("{! (1)}.",_),fail),_,true),
	write(ok), nl.
