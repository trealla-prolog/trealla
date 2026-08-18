% Issue #1114: consulting an extensionless name must try `f.pl` before
% the bare `f`. Quintus and SICStus both specified that order and every
% system since has followed it; the confusing case is a directory
% holding an executable `f` beside its source `f.pl`.
%
% The fixtures are written here rather than shipped, so the test cannot
% be broken by a checkout that drops an extensionless file. They are
% written into tests/issues/ because consult/1 resolves a relative name
% against the consulting file's directory, while open/3 resolves against
% the working directory - the suite runs from the repo root.

:- initialization(main).

write_file(Name, Text) :-
	open(Name, write, S),
	write(S, Text),
	nl(S),
	close(S).

main :-
	write_file('tests/issues/t1114_a', 'bare_a.'),
	write_file('tests/issues/t1114_a.pl', 'dotpl_a.'),
	write_file('tests/issues/t1114_b', 'bare_b.'),
	consult(t1114_a),
	(  catch(dotpl_a, _, fail) -> write(picked_dotpl) ; write(picked_bare) ),
	nl,
	% with no .pl beside it the bare name is still found
	consult(t1114_b),
	(  catch(bare_b, _, fail) -> write(fallback_ok) ; write(fallback_broken) ),
	nl,
	delete_file('tests/issues/t1114_a'),
	delete_file('tests/issues/t1114_a.pl'),
	delete_file('tests/issues/t1114_b').
