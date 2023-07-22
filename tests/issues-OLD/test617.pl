:- op(300,xfx,\\).

main :-
	read_from_chars("arg(1,(\\) \\\\ '', Y).", X),
	read_from_chars("arg(1,\\ \\\\ '', Y).", X).

:- initialization(main).
