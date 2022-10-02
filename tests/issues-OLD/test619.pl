:- op(300,xfx,\\).

main :-
	read_term_from_chars("[,@].", T).

:- initialization(main).
