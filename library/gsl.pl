:- module(gsl, [

	gsl_vector_alloc/2,
	gsl_vector_calloc/2,
	gsl_vector_free/1,
	gsl_vector_ptr/3,
	gsl_vector_const_ptr/3,
	gsl_vector_memcpy/3,
	gsl_vector_swap/3,
	gsl_vector_set/3,
	gsl_vector_get/3,
	gsl_vector_set_all/2,
	gsl_vector_set_zero/1,
	gsl_vector_set_basis/3,

	gsl_matrix_alloc/3,
	gsl_matrix_calloc/3,
	gsl_matrix_free/1,
	gsl_matrix_ptr/4,
	gsl_matrix_const_ptr/4,
	gsl_matrix_memcpy/3,
	gsl_matrix_swap/3,
	gsl_matrix_set/4,
	gsl_matrix_get/4,
	gsl_matrix_set_all/2,
	gsl_matrix_set_zero/1,
	gsl_matrix_set_identity/1,
	gsl_matrix_minmax/3,
	gsl_matrix_max/2,
	gsl_matrix_min/2,

	gsl_linalg_LU_solve/5,
	gsl_linalg_LU_det/3,

	mat_write/2
	]).

% GNU Scientific Library (GSL) v2.8
%
% UNDER DEVELOPMENT, EXPERIMENTAL
%
% UBUNTU: sudo apt install libgsl-dev
%
% REF: https://www.gnu.org/software/gsl/doc/html/index.html
%
% Contributions to add new definitions are most welcome!

/*
	$ tpl
	?- use_module(library(gsl)).
		true.
	?- gsl_matrix_calloc(10,10,M),
		gsl_matrix_set_identity(M),
		gsl_matrix_minmax(M,Min,Max),
		gsl_matrix_free(M).
	M = 109675795343088, Min = 0.0, Max = 1.0.

	Or, working with aliases...

	?- gsl_matrix_calloc(10,10,M), alias(M,foo).
		M = 95297409861920.
	?- alias(M,foo),
		gsl_matrix_set_identity(M),
		gsl_matrix_minmax(M,Min,Max),
		gsl_matrix_free(M).
		M = 95297409861920, Min = 0.0, Max = 1.0..
*/

:- foreign_struct(gsl_vector, [ulong,ulong,ptr,ptr,sint]).
:- foreign_struct(gsl_matrix, [ulong,ulong,ulong,ptr,ptr,sint]).

:- use_foreign_module('libgsl.so', [
	gsl_vector_alloc([ulong], ptr),
	gsl_vector_calloc([ulong], ptr),
	gsl_vector_free([ptr], void),
	gsl_vector_memcpy([ptr,ptr], sint),
	gsl_vector_swap([ptr,ptr], sint),
	gsl_vector_ptr([ptr,ulong], ptr),
	gsl_vector_const_ptr([ptr,ulong], ptr),
	gsl_vector_set([ptr,ulong,double], void),
	gsl_vector_get([ptr,ulong], double),
	gsl_vector_set_all([ptr,double], void),
	gsl_vector_set_zero([ptr], void),
	gsl_vector_set_basis([ptr,ulong], sint),

	gsl_matrix_alloc([ulong,ulong], ptr),
	gsl_matrix_calloc([ulong,ulong], ptr),
	gsl_matrix_free([ptr], void),
	gsl_matrix_memcpy([ptr,ptr], sint),
	gsl_matrix_swap([ptr,ptr], sint),
	gsl_matrix_ptr([ptr,ulong,ulong], ptr),
	gsl_matrix_const_ptr([ptr,ulong,ulong], ptr),
	gsl_matrix_set([ptr,ulong,ulong,double], void),
	gsl_matrix_get([ptr,ulong,ulong], double),
	gsl_matrix_set_all([ptr,double], void),
	gsl_matrix_set_zero([ptr], void),
	gsl_matrix_set_identity([ptr], void),
	gsl_matrix_minmax([ptr,-double,-double], void),
	gsl_matrix_max([ptr], double),
	gsl_matrix_min([ptr], double),

	gsl_linalg_LU_solve([ptr,ptr,ptr,ptr], sint),
	gsl_linalg_LU_det([ptr,sint], double)
	]).

