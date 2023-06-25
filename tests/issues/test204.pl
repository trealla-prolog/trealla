:-initialization(main).

f(g(a)).
f(g(b)).

main :-
	f(g(X)), write(X), nl, fail.
main.

