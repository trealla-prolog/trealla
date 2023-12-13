:- use_module(library(dif)).
:- initialization(main).

main :-
	portray_clause(c:t((dif(A,B),A=[]*C,B=[[]|D]),(A=[]*C,B=[[]|D],dif(A,B)))).
