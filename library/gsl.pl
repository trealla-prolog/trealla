:- module(gsl, [
	gsl_matrix_calloc/3,
	gsl_matrix_free/1
	]).

% GNU Scientific Library
%
% USAGE UNDER DEVELOPENT, EXPERIMENTAL
%
% UBUNTU: sudo apt install libgsl-dev

/*
	$ tpl
	?- use_module(library(gsl)).
	   true.
	?- gsl_matrix_calloc(10,10,M), gsl_matrix_free(M).
	   M = 109200253012240.
*/

:- use_foreign_module('libgsl.so', [
	gsl_matrix_calloc([uint64,uint64], ptr),
	gsl_matrix_free([ptr], void)
	]).
