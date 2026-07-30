:- initialization(main).

% Issue #1082: outputs/1 in answer descriptions. The argument is matched
% against characters written to current output; a character list (or
% double-quoted string) is enough for now.

15 ?- write(abc), nl.
   outputs("abc\n"),
   true.

16 ?- call((write(3), X)).
   outputs("3"),
   instantiation_error.

% Mismatched output must fail the quad.
wrong ?- write(abc), nl.
   outputs("nope"),
   true.

main :-
	use_module(library(quads)),
	run_quads.
