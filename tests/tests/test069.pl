:- initialization(main).
:- use_module(library(freeze)).

task70(X,Y) :-
	write('Frozen X='),
	write(X), Y=456,
	write(', set Y='),
	write(Y), nl.

test70 :-
	freeze(X, task70(X,Y)),
	X=123, write('Y='),
	write(Y), nl,
	write('OK done'), nl.

task71(X) :-
	write('Frozen X='),
	write(X), nl, fail.

test71 :-
	freeze(X, task71(X)),
	X=123,
	write('Ooops'), nl.
test71 :-
	write('OK done'), nl.

task72(X) :-
	write('Frozen X='),
	write(X), nl.

test72 :-
	X=123,
	freeze(X, task72(X)),
	write('OK done'), nl.

main :- test70, test71, test72.
