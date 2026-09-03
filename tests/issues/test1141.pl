:- initialization(main).

% Issue #1141: 'unexpected' marks one leaf answer as not the one the
% query gives at that point - it does not say the answer is wrong in
% general. So the description ends there and says nothing about the
% answers that follow it, not even that there are none: #2 below holds
% because member/2 answers X = c third, not second.

1 ?- member(X, "abc").
   X = a
;  X = b
;  X = c.

2 ?- member(X, "abc").
   X = a
;  X = c, unexpected.

% the leaf is still checked where it stands, so an 'unexpected' answer
% the query does give there is a failure

3 ?- member(X, "abc").
   X = a
;  X = b, unexpected.

% run_quads names the file as it was consulted, so the report would
% otherwise depend on the path this was invoked with. Keep the base
% name only, the way tests/issues/test1099.pl does.

strip_dirs(Cs, Out) :- strip_dirs(Cs, [], Out).

strip_dirs([], W, Out) :- reverse(W, Out).
strip_dirs([C|Cs], W, Out) :-
	(	C == (/)
	->	strip_dirs(Cs, [], Out)
	;	C == ' '
	->	reverse([C|W], Pre), append(Pre, Out0, Out), strip_dirs(Cs, [], Out0)
	;	C == '\n'
	->	reverse([C|W], Pre), append(Pre, Out0, Out), strip_dirs(Cs, [], Out0)
	;	strip_dirs(Cs, [C|W], Out)
	).

main :-
	use_module(library(quads)),
	with_output_to(chars(Cs), run_quads),
	strip_dirs(Cs, Out),
	atom_chars(A, Out),
	write(A).
