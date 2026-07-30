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
   instantiation_error.

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
|  error(system_error, ...).

?- length(L, 2).
   L = [_,_].

?- loop.
   loops.

% the 'unexpected' annotation: the described answer must not occur

?- X = 1.
   X = 2, unexpected.

?- member(X, [1,2,3]).
   X = 4, unexpected
;  ... .

?- X = 1.
   X = 2, unexpected
|  X = 3, unexpected.

% short forms of the ISO errors (issue #1066)

?- undefined_pred_xyz(1).
   existence_error(procedure, undefined_pred_xyz/1).

?- assertz(atom_length(a,b)).
   permission_error(modify, static_procedure, atom_length/2).

?- X is 1//0.
   evaluation_error(zero_divisor).

?- atom_length(1, L).
   type_error(atom, 1).

% an answer description must describe the answer completely (issue #1067)

?- X = f(Y,Z), Y = Z.
   X = f(Y,Y), Z = Y.

?- X = 1, Y = 2.
   X = 1, Y = 2.

% a quad may carry several answer descriptions; all must hold

?- X = 1.
   X = 2, unexpected.
   X = 1.

% 'inattendue' is a synonym for 'unexpected'

?- X = 1.
   X = 2, inattendue.

% a deliberately failing quad, to test reporting

?- member(X, [1,2]).
   X = 1
;  X = 99.

% a deliberately failing 'unexpected' quad: this answer does occur

?- X = 1.
   X = 1, unexpected.

% a deliberately failing quad: too general, says nothing about Z

?- X = f(Y,Z), Y = Z.
   X = f(Y,Y).

% a quad may be labelled with a ground term (issue #1071)

member_1 ?- member(X, [1,2,3]).
   X = 1
;  X = 2
;  X = 3.

'a quoted label' ?- foo(X).
   X = bar.

label(with, args) ?- X = 1.
   X = 1.

% a labelled quad that fails, to test reporting

member_2 ?- member(X, [1,2]).
   X = 1
;  X = 99.

% A described ball must be a variant of the one actually thrown, so the
% correspondence between their variables is one-to-one (issue #1080)

?- throw(error(_,_)).
   error(X,Y).

?- throw(f(A,A)).
   throw(f(X,X)).

% throw/1 copies the ball, so an anonymous description variable
% describes the copy

?- throw(f(X)).
   throw(f(_)).

% a deliberately failing quad: one description variable cannot describe
% two distinct variables of the ball

ball_1 ?- throw(error(_A,_B)).
   error(X,X).

% a deliberately failing quad: nor two description variables one
% variable of the ball

ball_2 ?- throw(f(A,A)).
   throw(f(X,Y)).

% deliberately failing quads: a variable shared with the query is that
% variable of the query, which a copied ball never contains

ball_3 ?- throw(f(X)).
   throw(f(X)).

ball_4 ?- throw(error(type_error(atom,[X,Y]),[])).
   type_error(atom,[X,X]).

% An answer description must describe an answer *substitution*, so each
% equation binds a variable and no variable is bound twice within one
% answer (issue #1074). The parser rejects a malformed description when
% the file is consulted, which aborts the load, so that case is
% tests/issues/test1074.pl; a '$quad' fact can also be asserted by hand,
% and library(quads) makes the same check on those.

main :-
	use_module(library(quads)),
	assertz('$quad'(hand_1, (X=1), ['X'=X], (1 = X), 'hand-written.pl', 1)),
	assertz('$quad'(hand_2, (Y=1), ['Y'=Y], (Y = 1, Y = 2), 'hand-written.pl', 2)),
	assertz('$quad'(hand_3, (Z=1), ['Z'=Z], (Z = 1), 'hand-written.pl', 3)),
	run_quads.
