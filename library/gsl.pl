:- module(gsl, [
	gsl_matrix_calloc/3,
	gsl_matrix_free/1,
	gsl_matrix_memcpy/3,
	gsl_matrix_set/4,
	gsl_matrix_get/4,
	gsl_matrix_set_all/2,
	gsl_matrix_set_zero/1,
	gsl_matrix_set_identity/1,
	gsl_linalg_LU_solve/5,
	gsl_linalg_LU_det/3
	]).

% GNU Scientific Library
%
% UNDER DEVELOPENT, EXPERIMENTAL
%
% UBUNTU: sudo apt install libgsl-dev
%
% REF: https://www.gnu.org/software/gsl/doc/html/index.html

/*
	$ tpl
	?- use_module(library(gsl)).
	   true.
	?- gsl_matrix_calloc(10,10,M), gsl_matrix_free(M).
	   M = 109200253012240.
*/

:- use_foreign_module('libgsl.so', [
	gsl_matrix_calloc([uint64,uint64], ptr),
	gsl_matrix_free([ptr], void),
	gsl_matrix_memcpy([ptr,ptr], sint),

	gsl_matrix_set([ptr,uint64,uint64,fp64], void),
	gsl_matrix_get([ptr,uint64,uint64], fp64),
	gsl_matrix_set_all([ptr,fp64], void),
	gsl_matrix_set_zero([ptr], void),
	gsl_matrix_set_identity([ptr], void),

	gsl_linalg_LU_solve([ptr,ptr,ptr,ptr], sint),
	gsl_linalg_LU_det([ptr,sint], fp64)
	]).
