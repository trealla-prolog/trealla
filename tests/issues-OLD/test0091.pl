:- initialization(main).

foo :-
	bar,
	fail.

foo.

bar.


'$l_foo' :-
	'$l_bar',
	fail.

'$l_foo'.

'$l_bar'.

main :-
	'$l_foo',
	writeq(ok), nl.
