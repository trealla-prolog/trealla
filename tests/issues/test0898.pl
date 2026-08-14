:- initialization(main).

main :-
	catch(call((true->false,1)), E, true), !, write(E), nl.
main :-
	write(nok), nl.
