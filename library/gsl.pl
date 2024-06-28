:- module(gsl, []).

% GNU Scientific Library
%
% USAGE UNDER DEVELOPENT, EXPERIMENTAL
%
% UBUNTU: sudo apt install libgsl-dev

:- use_foreign_module('libgsl.so', [
	gsl_matrix_calloc([uint64,uint64], ptr),
	gsl_matrix_free([ptr], void)
	]).
