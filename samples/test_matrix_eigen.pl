:- use_module(library(gsl)).

% Calculate the eigenvalues/eigenvectors of a matrix...

main :-
	mat_from_list(M,[
		[1/1,1/2,1/3,1/4],
		[1/2,1/3,1/4,1/5],
		[1/3,1/4,1/5,1/6],
		[1/4,1/5,1/6,1/7]
		]),

	gsl_vector_calloc(4,Eval),
	gsl_matrix_calloc(4,4,Evec),
	gsl_eigen_symmv_alloc(4,W),
	gsl_eigen_symmv(M,Eval,Evec,W),
	gsl_eigen_symmv_free(W),
	gslConst(gslGSL_EIGEN_SORT_ABS_ASC,Val),
	gsl_eigen_symmv_sort(Eval,Evec,Val),

	between(0,3,I),
		gsl_vector_get(Eval,I,Eval_i),
		gsl_matrix_column(Evec,I,Evec_i),

		% TODO: just dump the vector view meta-data for now
		format("[~d] eigenvalue=~g, eigenvector=~w~n", [I,Eval_i,Evec_i]),
		%'$ffi_make_pointer'(Evec_i,V),
		%vec_to_list(V,L),
		%write(L), nl,

		fail;
	gsl_vector_free(Eval),
	gsl_matrix_free(Evec),
	gsl_matrix_free(M).

