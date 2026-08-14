:- initialization(main).

:- multifile(foo/1).
:- multifile('$bar'/1).

main :-
	foo(abc).
main :-
	'$bar'(xyz).
main :-
	write(ok), nl.
