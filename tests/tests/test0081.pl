foo([
]).

bar([
1,2,3]).

baz([ /* test */ ]).

houses([
	house(_,_,_,_,_),
	house(_,_,_,_,_),
	house(_,_,_,_,_),
	house(_,_,_,_,_),
	house(_,_,_,_,_)]).

main :-
	true.

:- initialization(main).
