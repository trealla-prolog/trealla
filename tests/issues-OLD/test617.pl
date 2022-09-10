:- op(300,xfx,\\).

main :-
	read_term_from_chars(X, "arg(1,(\\) \\\\ '', Y)."),
	read_term_from_chars(X, "arg(1,\\ \\\\ '', Y).").

:- initialization(main).
