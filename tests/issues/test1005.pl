:- initialization(main).

:- dynamic(foo/0).
foo.
foo:-bar.

main :-
	clause(foo, _, R),
	clause(H, B, R),
	write([H,B]), nl,
	fail.
main.

