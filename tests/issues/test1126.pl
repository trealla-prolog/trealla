% Issue #1126: cyclic comparison used independent "seen on the left/right"
% flags. Two unrelated cycles could therefore stop the walk early and make
% \==/2 report equal terms, causing a valid dif/2 constraint to fail.

:- use_module(library(dif)).
:- initialization(main).

nest(0, X, X).
nest(N, X, f(T)) :-
	N > 0,
	N2 is N - 1,
	nest(N2, X, T).

main :-
	(   dif(A, B), C=[[]|C], A=[C|B], B=[C|D], D=[D|D]
	->  write(dif_first_ok)
	;   write('FAIL: dif-first rational terms reported equal')
	), nl,

	(   C2=[[]|C2], A2=[C2|B2], B2=[C2|D2], D2=[D2|D2], dif(A2, B2)
	->  write(dif_last_ok)
	;   write('FAIL: dif-last rational terms reported equal')
	), nl,

	(   X=f(X), Y=f(f(Y)), X == Y
	->  write(equivalent_cycles_ok)
	;   write('FAIL: equivalent rational terms reported different')
	), nl,

	(   X2=f(X2), Y2=f(g(Y2)), X2 \== Y2
	->  write(distinct_cycles_ok)
	;   write('FAIL: distinct rational terms reported equal')
	), nl,

	(   nest(64, a, NA), nest(64, b, NB), NA \== NB
	->  write(deep_terms_ok)
	;   write('FAIL: deep terms reported equal')
	), nl,

	halt.
