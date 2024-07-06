:- module(gsl, [

	gsl_set_error_handler_off/1,

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
	gsl_vector_fwrite/3,
	gsl_vector_fread/3,
	gsl_vector_add/3,
	gsl_vector_sub/3,
	gsl_vector_mul/3,
	gsl_vector_div/3,
	gsl_vector_scale/3,
	gsl_vector_add_constant/3,
	gsl_vector_sum/2,

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
	gsl_matrix_transpose_memcpy/3,
	gsl_matrix_transpose/2,
	gsl_matrix_fwrite/3,
	gsl_matrix_fread/3,
	gsl_matrix_get_row/4,
	gsl_matrix_get_col/4,
	gsl_matrix_set_row/4,
	gsl_matrix_set_col/4,
	gsl_matrix_swap_rows/4,
	gsl_matrix_swap_columns/4,
	gsl_matrix_swap_rowcol/4,
	gsl_matrix_add/3,
	gsl_matrix_sub/3,
	gsl_matrix_mul_elements/3,
	gsl_matrix_div_elements/3,
	gsl_matrix_scale/3,
	gsl_matrix_scale_columns/3,
	gsl_matrix_scale_rows/3,
	gsl_matrix_add_constant/3,

	gsl_permutation_alloc/2,
	gsl_permutation_free/1,

	gsl_linalg_LU_decomp/4,
	gsl_linalg_LU_solve/5,
	gsl_linalg_LU_det/3,

	new_vec/2,
	vec_list/2,
	vec_read/3,
	vec_write/2,
	vec_random/2,

	new_mat/2,
	mat_list/2,
	mat_read/4,
	mat_write/2,
	mat_lup_det/2,
	mat_random/3
	]).

% GNU Scientific Library (GSL) v2.8
%
% UNDER DEVELOPMENT, EXPERIMENTAL
%
% UBUNTU: sudo apt install libgsl-dev libgslcblas0
%
% REF: https://www.gnu.org/software/gsl/doc/html/index.html
%
% Contributions to add new definitions are most welcome!

:- foreign_struct(gsl_vector, [ulong,ulong,ptr,ptr,sint]).
:- foreign_struct(gsl_matrix, [ulong,ulong,ulong,ptr,ptr,sint]).

:- use_foreign_module('libgslcblas.so', []).

:- use_foreign_module('libgsl.so', [
	gsl_set_error_handler_off([], ptr),

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
	gsl_vector_fwrite([ptr,ptr], sint),
	gsl_vector_fread([ptr,ptr], sint),
	gsl_vector_add([ptr,ptr], sint),
	gsl_vector_sub([ptr,ptr], sint),
	gsl_vector_mul([ptr,ptr], sint),
	gsl_vector_div([ptr,ptr], sint),
	gsl_vector_scale([ptr,double], sint),
	gsl_vector_add_constant([ptr,double], sint),
	gsl_vector_sum([ptr], double),

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
	gsl_matrix_transpose_memcpy([ptr,ptr], sint),
	gsl_matrix_transpose([ptr], sint),
	gsl_matrix_fwrite([ptr,ptr], sint),
	gsl_matrix_fread([ptr,ptr], sint),
	gsl_matrix_get_row([ptr,ptr,ulong], sint),
	gsl_matrix_get_col([ptr,ptr,ulong], sint),
	gsl_matrix_set_row([ptr,ulong,ptr], sint),
	gsl_matrix_set_col([ptr,ulong,ptr], sint),
	gsl_matrix_swap_rows([ptr,ulong,ulong], sint),
	gsl_matrix_swap_cols([ptr,ulong,ulong], sint),
	gsl_matrix_swap_rowcol([ptr,ulong,ulong], sint),
	gsl_matrix_swap_add([ptr,ptr], sint),
	gsl_matrix_swap_sub([ptr,ptr], sint),
	gsl_matrix_swap_mul_elements([ptr,ptr], sint),
	gsl_matrix_swap_div_elements([ptr,ptr], sint),
	gsl_matrix_scale([ptr,double], sint),
	gsl_matrix_scale_columns([ptr,ptr], sint),
	gsl_matrix_scale_rows([ptr,ptr], sint),
	gsl_matrix_add_constant([ptr,double], sint),

	gsl_permutation_alloc([sint], ptr),
	gsl_permutation_free([ptr], void),

	gsl_linalg_LU_decomp([ptr,ptr,-sint], sint),
	gsl_linalg_LU_solve([ptr,ptr,ptr,ptr], sint),
	gsl_linalg_LU_det([ptr,sint], double)
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vec_random(V, Cols) :-
	gsl_vector_alloc(Cols, V),
	( cfor(0, Cols-1, J),
		random(Val),
		gsl_vector_set(V, J, Val),
		fail; true
	).

mat_random(M, Rows, Cols) :-
	gsl_matrix_alloc(Rows, Cols, M),
	( cfor(0, Rows-1, I),
		( cfor(0, Cols-1, J),
			random(Val),
			gsl_matrix_set(M, I, J, Val),
			fail; true
		),
		fail; true
	).

check_error_(Goal, Check, Action) :-
	Goal, ( Check; (Action, fail)).

mat_lup_det(M0, Det0) :-
	'$gsl_matrix_size'(M0, Rows, Cols),
	(Rows =:= Cols -> true; throw(error(domain_error(matrix_not_square, (Rows * Cols)), mat_lup_det/2))),
	Size is Rows,
	(Size > 0 -> true; throw(error(domain_error(matrix_empty, Size), mat_lup_det/2))),
	gsl_matrix_alloc(Size, Size, M),
	gsl_matrix_memcpy(M, M0, _),
	gsl_permutation_alloc(Size, P),
	check_error_(
		gsl_linalg_LU_decomp(M, P, Signum, Status1),
		Status1 =:= 0,
		gsl_matrix_free(M)
		),
	gsl_vector_alloc(Size, B),
	( cfor(0, Size-1, I),
		V is float(I+1),
		gsl_vector_set(B, I, V),
		fail; true
	),
	gsl_vector_alloc(Size, X),
	check_error_(
		gsl_linalg_LU_solve(M, P, B, X, Status2),
		Status2 =:= 0,
		(gsl_vector_free(X),gsl_vector_free(B),gsl_permutation_free(P))
		),
	gsl_vector_free(X),
	gsl_vector_free(B),
	gsl_permutation_free(P),
	gsl_linalg_LU_det(M, Signum, Det),
	gsl_matrix_free(M),
	Det0 is Det.			% checks for NAN


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(lists)).

% new_vec(V, [1,2,3])

new_vec(V, L) :-
	length(L, Size),
	Size > 0,
	gsl_vector_alloc(Size, V),
	new_vec_(V, 0, L).

new_vec_(_, _, []) :- !.
new_vec_(V, I, [H|T]) :-
	H2 is float(H),
	gsl_vector_set(V, I, H2),
	I2 is I + 1,
	new_vec_(V, I2, T).

vec_list(V, L) :-
	'$gsl_vector_size'(V, Size),
	vec_list_(V, Size, [], L).

vec_list_(_, 0, L, L) :- !.
vec_list_(V, Col, L0, L) :-
	NewCol is Col - 1,
	gsl_vector_get(V, NewCol, Val),
	vec_list_(V, NewCol, [Val|L0], L).

vec_write(V, S) :-
	'$gsl_vector_write'(V, S).

vec_read(V, S, Size1) :-
	'$gsl_vector_alloc'(S, Size1),
	gsl_vector_alloc(Size1, V),
	'$gsl_vector_read'(V, S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% new_mat(M, [[1,2,3],[4,5,6],[7,8,9]])

new_mat(M, L) :-
	length(L, Rows),
	Rows > 0,
	L = [H|T],
	length(H, Cols),
	Cols > 0,
	gsl_matrix_calloc(Rows, Cols, M),
	new_row_(M, 0, L).

new_row_(_, _, []) :- !.
new_row_(M, Row, [H|T]) :-
	new_col_(M, Row, 0, H),
	Row2 is Row + 1,
	new_row_(M, Row2, T).

new_col_(_, _, _, []).
new_col_(M, Row, Col, [H|T]) :-
	H2 is float(H),
	gsl_matrix_set(M, Row, Col, H2),
	Col2 is Col + 1,
	new_col_(M, Row, Col2, T).

mat_list(M, L) :-
	'$gsl_matrix_size'(M, Size1, Size2),
	mat_list_row_(M, Size1, Size2, [], L).

mat_list_row_(_, 0, _, L, L) :- !.
mat_list_row_(M, Row, Size2, L0, L) :-
	NewRow is Row - 1,
	mat_list_col_(M, NewRow, Size2, [], NewL),
	mat_list_row_(M, NewRow, Size2, [NewL|L0], L).

mat_list_col_(_, _, 0, L, L) :- !.
mat_list_col_(M, Row, Col, L0, L) :-
	NewCol is Col - 1,
	gsl_matrix_get(M, Row, NewCol, Val),
	mat_list_col_(M, Row, NewCol, [Val|L0], L).

mat_write(M, S) :-
	'$gsl_matrix_write'(M, S).

mat_read(M, S, Size1, Size2) :-
	'$gsl_matrix_alloc'(S, Size1, Size2),
	gsl_matrix_alloc(Size1, Size2, M),
	'$gsl_matrix_read'(M, S).
