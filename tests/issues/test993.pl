:- initialization(main).

main :-
	between(0,5,D),Y=f(X),X=f(_),write_term(D:Y,[max_depth(D),variable_names(['NV'=X])]),nl,false.
main :-
	true.
