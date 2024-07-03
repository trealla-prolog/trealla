:- use_module(library(gsl)).

% M is mutable here, should probably work on a copy

mat_lup_det(M,Size,Det) :-
	gsl_permutation_alloc(Size,P),
	gsl_linalg_LU_decomp(M,P,Signum,_),
	gsl_vector_alloc(Size,B),
	(
		between(1,Size,I),
			I2 is I - 1,
			V is float(I),
			gsl_vector_set(B,I2,V),
			fail; true
	),
	gsl_vector_alloc(Size,X),
	gsl_linalg_LU_solve(M,P,B,X,_),
	gsl_permutation_free(P),
	gsl_vector_free(B),
	gsl_vector_free(X),
	gsl_linalg_LU_det(M,Signum,Det).

main :-
	open('samples/test_matrix_det.mat',read,S),
	mat_read(M,S,Rows,Cols),
	Rows =:= Cols,
	mat_lup_det(M,Rows,Det),
	gsl_matrix_free(M),
	close(S),
	write(Det), nl.
