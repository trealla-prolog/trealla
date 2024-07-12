:- use_module(library(gsl)).

% Calculate the determinant of a matrix...

main :-
	open('samples/test_matrix_det.mat',read,S),
	mat_read(M,S,Rows,Cols),
	mat_lup_det(M,Det),
	gsl_matrix_free(M),
	close(S),
	write(Det), nl.
