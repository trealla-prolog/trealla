% Phase 0 acceptance: the build wiring, libpython discovery, a
% zero-argument registration, and the shutdown hook.
%
% Not run by tests/run.sh - it needs a `make janus` binary. See
% tests/janus/run.sh.

:- use_module(library(janus)).

% A second atexit clause, to prove janus's own ends in fail and does not
% cut the chain short for anything else that registers.
:- assertz((atexit :- write('second hook ran'), nl, fail)).

main :-
	py_lib(Lib),
	( atom(Lib) -> write('found libpython'), nl ; write('NO LIBRARY'), nl ),

	% Py_GetVersion takes no arguments, which is what needed the
	% '$register_predicate'/4 fix, and returns a borrowed string.
	py_version_matches_runtime,

	% Register a Python-side atexit handler. If Py_FinalizeEx really
	% runs, its output appears below - after Prolog's, because the two
	% runtimes buffer independently.
	janus:py_run_('import atexit\natexit.register(lambda: print("python atexit ran"))'),

	write('prolog done'), nl,
	halt(3).

py_version_matches_runtime :-
	janus:'Py_GetVersion'(V),
	(   sub_atom(V, 0, 2, _, '3.')
	->  write('version starts 3.'), nl
	;   format("UNEXPECTED VERSION ~w~n", [V])
	).
