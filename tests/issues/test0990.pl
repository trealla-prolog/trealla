:- use_module(library(freeze)).
:- initialization(main).

main :-
	X1=f(X1), write_term(X1,[max_depth(2),variable_names([x=X1])]), nl,
	X2=f(X2), XXX2=xxx, write_term(X2,[max_depth(3),variable_names([XXX2=X2])]), nl,
	freeze(XXX3,write_term(X3,[max_depth(3),variable_names([XXX3=X3])])),X3=f(X3),XXX3=xxx, nl,
	true.
