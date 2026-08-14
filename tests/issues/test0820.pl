:- initialization(main).

main :-
	 X = "abc"||(1*(1+2)),
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "abc"||-123,
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "abc"||_,
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "abc"||"def",
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "abc"||f(_),
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "abc"||[1,2,3],
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "abc"||{1,2,3},
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "ab'c"||[],
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "ab`c"||[],
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "ab\`c"||_,
	 write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "ab\'c"||_,
	  write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "ab,c"||_,
	  write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "ab|c"||_,
	  write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "ab]c"||_,
	  write_term(X,[double_quotes(true)]), nl, fail.
main :-
	 X = "ab[c"||_,
	  write_term(X,[double_quotes(true)]), nl, fail.
main.
