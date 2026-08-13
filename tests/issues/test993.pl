:- initialization(main).

main :-
	between(0,5,D),Y=f(X),X=f(Z),write_term(D:Y,[max_depth(D),variable_names(['Z'=Z])]),nl,false.
main :-
	true.
