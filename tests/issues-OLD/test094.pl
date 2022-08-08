:- initialization(main).

'$l_foo' :-
	'$l_bar',
	'$l_baz'.

'$l_bar'.

'$l_baz'.

main :-
	'$l_bar',
	'$l_baz',
	'$l_foo',
	writeq(ok), nl.
