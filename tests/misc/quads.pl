:- initialization(main).

% Quads: queries using answer descriptions (issue #1063).
% These are recorded at load time and interpreted by library(quads).

foo(bar).
loop :- loop.

?- member(X, [1,2,3]).
   X = 1
;  X = 2
;  X = 3.

?- foo(X).
   X = bar.

?- foo(baz).
   false.

?- X = 1, Y = 2.
   X = 1, Y = 2.

?- atom_length(A, L).
   error(instantiation_error, _).

?- atom_length(abc, L).
   L = 3.

?- between(1, 100, X).
   X = 1
;  X = 2
;  ... .

?- repeat.
   true
;  true
;  ..., ad_infinitum.

?- catch(throw(ball), E, true).
   E = ball
|  error(system_error, _).

?- length(L, 2).
   L = [_,_].

?- loop.
   loops.

% the 'unexpected' annotation: the described answer must not occur

?- X = 1.
   X = 2, unexpected.

?- member(X, [1,2,3]).
   X = 4, unexpected.

?- X = 1.
   X = 2, unexpected
|  X = 3, unexpected.

% a deliberately failing quad, to test reporting

?- member(X, [1,2]).
   X = 1
;  X = 99.

% a deliberately failing 'unexpected' quad: this answer does occur

?- X = 1.
   X = 1, unexpected.

main :-
	use_module(library(quads)),
	run_quads.
