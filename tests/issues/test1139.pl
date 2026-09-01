% Issue #1139: writing a list whose chars-list tail follows a
% non-char element (a variable, an integer, ...) spliced the string
% suffix one element too late - eg. [a,b,c,D,e,f,g] printed as
% [a,b,c,D,e|"fg"] instead of [a,b,c,D|"efg"]. The tail-to-string
% check was gated on the *current* element being a char, when it
% should only depend on whether the remaining tail is a full chars
% list.

:- initialization(main).

show(T) :- write_term(T, [double_quotes(true)]), nl.

main :-
	show([a,b,c,D,e,f,g]),
	show([D2,e,f,g]),
	show([D3,D4,e,f]),
	show([a,b,c]),
	show([a,b,c,D5,e]),
	show([1,2,3,a,b,c]).
