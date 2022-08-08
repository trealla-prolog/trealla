:- initialization(main).

:- op(600, xfy, ::).
:- op(600,  fy, ::).

a::b.

X::Y :-
	X = c,
	Y = d.

main :- X::Y, writeq(['X=',X,'Y=',Y]), nl, fail.
main.
