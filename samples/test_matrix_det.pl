:- use_module(library(gsl)).

main :-
	open('samples/test_matrix_det.mat',read,S),
	mat_read(M,S,Rows,Cols),
	Rows =:= Cols,
	mat_lup_det(M,Rows,Det),
	gsl_matrix_free(M),
	close(S),
	write(Det), nl.
