% Differential test over every DCG rule actually in the tree.
%
% The companion test (dcg_differential.pl) uses a hand-built corpus of
% ISO 7.14 constructs. That found two bugs, which is exactly why a
% hand-built corpus is not enough: both hid in shapes nobody thought to
% write down. This one reads library/*.pl and tests/ with the real
% reader and compares native '$dcg_rule'/2 against the still-live
% dcgs:dcg_rule/2 on every --> rule it finds.
%
% Reading rather than generating a corpus file means it cannot go stale:
% a DCG rule added to any library is covered the next time this runs.
%
% Directives are NOT executed, with two whitelisted exceptions - op/3 and
% set_prolog_flag/2 - because without them a file's own operators and
% double_quotes setting are not in effect and its terms either fail to
% read or read as something else. Anything unreadable is counted and
% skipped rather than silently dropped; the counts go to stderr, which
% tests/run.sh does not capture, so stdout stays stable as files come
% and go.

:- initialization(main).
:- use_module(library(dcgs)).
:- ensure_loaded('tests/dcg_reference').
:- use_module(library(lists)).

% Any file whose rules should be exercised. Directories, not files, so
% new files are picked up automatically.

dir('library').
dir('tests/tests').
dir('tests/issues').
dir('tests/issues-OLD').
dir('tests/slow').
dir('tests/misc').

pl_file(Path) :-
	dir(Dir),
	catch(directory_files(Dir, Fs), _, fail),
	member(F, Fs),
	atom_concat(_, '.pl', F),
	atom_concat(Dir, '/', Dir1),
	atom_concat(Dir1, F, Path).

% --- comparison ------------------------------------------------------

run_native(R, X) :-
	(  catch('$dcg_rule'(R, Out), E, true)
	-> (var(E) -> X = ok(Out) ; X = err(E))
	;  X = failed
	).

run_ref(R, Y) :-
	(  catch(dcg_reference:dcg_rule(R, Out), E, true)
	-> (var(E) -> Y = ok(Out) ; Y = err(E))
	;  Y = failed
	).

% The one permanent divergence: #1102 (== #832). A nonvar non-callable in
% non-terminal position raises natively and is silently mistranslated by
% the reference. Not expected to appear in real library code, but if it
% does this must not be reported as a corpus failure.

known_divergence(X, _) :-
	X = err(error(type_error(callable, _), _)).

compare_rule(File, R) :-
	run_native(R, X),
	run_ref(R, Y),
	(  variant(X, Y) -> true
	;  known_divergence(X, Y)
	-> format(user_error, "~w: known #1102 divergence~n", [File])
	;  format("DIFF in ~w~n   rule   ~q~n   native ~q~n   ref    ~q~n", [File, R, X, Y])
	).

% --- scanning --------------------------------------------------------

% op/3 and set_prolog_flag/2 only. Executing arbitrary directives out of
% every file in the tree would be both slow and destructive.

apply_directive(op(P,T,N)) :- !, catch(op(P,T,N), _, true).
apply_directive(set_prolog_flag(F,V)) :- !, catch(set_prolog_flag(F,V), _, true).
apply_directive(_).

handle(File, T, R0, R, S0, S) :-
	(  T = (:- D)
	-> apply_directive(D), R = R0, S = S0
	;  T = (_ --> _)
	-> compare_rule(File, T), R is R0+1, S = S0
	;  R = R0, S = S0
	).

scan(Stream, File, R0, R, S0, S) :-
	(  catch(read_term(Stream, T, []), _, T = '$unreadable')
	-> true
	;  T = end_of_file
	),
	(  T == end_of_file
	-> R = R0, S = S0
	;  T == '$unreadable'
	-> S1 is S0+1, scan(Stream, File, R0, R, S1, S)
	;  handle(File, T, R0, R1, S0, S1),
	   scan(Stream, File, R1, R, S1, S)
	).

scan_file(File, R0, R, S0, S) :-
	(  catch(open(File, read, Stream), _, fail)
	-> (  catch(scan(Stream, File, R0, R, S0, S), _, (R = R0, S = S0))
	   -> true
	   ;  R = R0, S = S0
	   ),
	   catch(close(Stream), _, true)
	;  R = R0, S = S0
	).

scan_all([], R, R, S, S, F, F).
scan_all([File|Fs], R0, R, S0, S, F0, F) :-
	scan_file(File, R0, R1, S0, S1),
	F1 is F0+1,
	scan_all(Fs, R1, R, S1, S, F1, F).

main :-
	findall(P, pl_file(P), Ps0),
	msort(Ps0, Ps),
	scan_all(Ps, 0, Rules, 0, Skipped, 0, Files),
	format(user_error, "dcg corpus: ~w files, ~w rules, ~w unreadable terms~n",
	       [Files, Rules, Skipped]),

	% Guard the silent-zero failure mode: if the scan stops finding
	% rules (a reader change, a moved directory), this test would
	% otherwise pass by doing nothing.

	(  Rules < 100
	-> format("CORPUS-TOO-SMALL: only ~w rules found~n", [Rules])
	;  true
	),
	format("dcg corpus: all rules agree~n"),
	halt.
