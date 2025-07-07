:- initialization(main).

main :-
	 X = "abc"||(1*(1+2)), write(X), nl, fail.
main :-
	 X = "abc"||-123, write(X), nl, fail.
main :-
	 X = "abc"||_, write(X), nl, fail.
main :-
	 X = "abc"||"def", write(X), nl, fail.
main :-
	 X = "abc"||f(_), write(X), nl, fail.
main :-
	 X = "abc"||[1,2,3], write(X), nl, fail.
main :-
	 X = "abc"||{1,2,3}, write(X), nl, fail.
main.
