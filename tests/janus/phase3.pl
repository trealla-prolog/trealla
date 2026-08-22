% Phase 3 acceptance: iteration, dict access, library paths.

:- set_prolog_flag(empty_args, true).
:- use_module(library(janus)).

chk(Label, Goal, Got, Want) :-
	(   catch(Goal, E, (format("  ~w~t~32| RAISED ~p~n", [Label, E]), fail))
	->  (   Got == Want
	    ->  format("  ~w~t~32| ok~n", [Label])
	    ;   format("  ~w~t~32| GOT ~p WANT ~p~n", [Label, Got, Want])
	    )
	;   format("  ~w~t~32| FAILED~n", [Label])
	).

err(Label, Goal) :-
	(   catch(Goal, error(E, _), (format("  ~w~t~32| ~p~n", [Label, E]), true))
	->  true
	;   format("  ~w~t~32| NO ERROR RAISED~n", [Label])
	).

main :-
	write('py_iter as a generator'), nl,
	chk('range(4)', findall(X, py_iter(builtins:range(4), X), L1), L1, [0,1,2,3]),
	chk('empty range', findall(X, py_iter(builtins:range(0), X), L2), L2, []),
	chk('list of mixed', findall(X, py_iter(builtins:list([1,a,2.5]), X), L3), L3, [1,a,2.5]),
	chk('filters on a bound value',
	    findall(x, py_iter(builtins:range(5), 3), L4), L4, [x]),
	chk('a generator, not a list',
	    ( py_run_gen, findall(X, py_iter(builtins:'_janus_gen', X), L5) ), L5, [0,1,4,9]),
	chk('nested iteration',
	    findall(A-B, (py_iter(builtins:range(2), A), py_iter(builtins:range(2), B)), L6),
	    L6, [0-0, 0-1, 1-0, 1-1]),
	chk('once/1 does not leak or crash',
	    once(py_iter(builtins:range(1000000), V1)), V1, 0),
	chk('iteration survives an exception',
	    catch(( py_iter(builtins:range(10), _), throw(stop) ), stop, true), yes, yes),

	nl, write('dict access, no Python involved'), nl,
	D = {a:1, b:2, c:3},
	chk('keys', keys(D, K1), K1, [a,b,c]),
	chk('keys of empty dict', keys({}, K2), K2, []),
	chk('items', items(D, I1), I1, [a:1, b:2, c:3]),
	chk('items of empty dict', items({}, I2), I2, []),
	chk('single-entry keys', keys({z:9}, K3), K3, [z]),
	chk('key enumerates', findall(K, key(D, K), K4), K4, [a,b,c]),
	chk('key of empty dict', findall(K, key({}, K), K5), K5, []),
	chk('values by key', values(D, b, V2), V2, 2),
	chk('values, missing key', ( values(D, zz, _) -> R1 = found ; R1 = absent ), R1, absent),
	chk('values enumerates', findall(K-V, values(D, K, V), L7), L7, [a-1, b-2, c-3]),
	chk('values by path',
	    values({x:{y:{z:42}}}, [x,y,z], V3), V3, 42),
	chk('values, empty path', values(D, [], V4), V4, D),
	err('keys of a non-dict', keys(notadict, _)),

	nl, write('round trip through Python'), nl,
	chk('dict from Python then keys',
	    ( py_call(builtins:dict(a=1, b=2), PD), keys(PD, K6) ), K6, [a,b]),

	nl, write('library paths'), nl,
	chk('py_lib_dirs is a list',
	    ( py_lib_dirs(Ds), is_list(Ds) -> R2 = yes ; R2 = no ), R2, yes),
	chk('add last',
	    ( py_add_lib_dir('/tmp/janus_last'),
	      py_lib_dirs(D2), last(D2, Last) ), Last, '/tmp/janus_last'),
	chk('add first',
	    ( py_add_lib_dir('/tmp/janus_first', first),
	      py_lib_dirs([First|_]) ), First, '/tmp/janus_first'),
	err('bad position', py_add_lib_dir('/tmp/x', sideways)),
	err('non-atom dir', py_add_lib_dir(123)).

py_run_gen :-
	janus:py_run_('import builtins\nbuiltins._janus_gen = (i*i for i in range(4))').
