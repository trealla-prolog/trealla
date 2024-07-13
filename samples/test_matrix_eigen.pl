:- use_module(library(gsl)).

% Calculate the eigenvalues/eigenvectors of a matrix...

main :-
	mat_from_list(M,[
		[1/1,1/2,1/3,1/4],
		[1/2,1/3,1/4,1/5],
		[1/3,1/4,1/5,1/6],
		[1/4,1/5,1/6,1/7]
		]),

	mat_eigen(M, Vals, Vecs),
	format("eigenvalues = ~w~neigenvectors = ~w~n", [Vals,Vecs]),
	gsl_matrix_free(M).

