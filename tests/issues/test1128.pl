:- initialization(main).

% Issue #1128: 'maybe' marks an answer that leaves some variable
% attributed - a pending constraint of any attribute module, not
% resolved into an ordinary binding. It names no particular variable
% or module, only that one exists - not even one of the query's own,
% as #3 shows: freeze/2's pending goal sits on a variable local to it,
% one the query never names.
%
% Since an answer describes an answer completely (issue #1067), the
% absence of maybe is itself an assertion - nothing is left pending -
% so #3 also checks that a bare 'true' does not equally describe it.

:- use_module(library(dif)).
:- use_module(library(freeze)).

1 ?- dif(X, Y), X = a.
   X = a, maybe.

2 ?- dif(X, Y).
   maybe.

ffalse :- freeze(_, false).

3 ?- ffalse.
   maybe.
   true, unexpected.

main :-
	use_module(library(quads)),
	run_quads.
