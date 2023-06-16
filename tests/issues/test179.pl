:-initialization(main).

main :-
	A = [A|A],
	B = [B|B],
	A = B,
	write(here1), nl,
	A=[A].				% expected to fail
main :-
	write(here2), nl.
