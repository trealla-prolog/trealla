:- initialization(main).
:- use_module(library(lists)).

main :-
	catch(length(_,1000000),E,(write(hello),nl)), write(E), nl.
