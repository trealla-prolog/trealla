% Phase 5a acceptance: the compatibility surface.
%
% The six XSB spellings and the two SWI conveniences kept in scope by
% section 2 of docs/janus-design.md. Phase 7 runs suites that call these
% names, so they have to exist and mean the right thing.

:- set_prolog_flag(empty_args, true).
:- use_module(library(janus)).
:- use_module(library(lists)).

chk(Label, Goal, Got, Want) :-
	(   catch(Goal, E, (format("  ~w~t~30| RAISED ~p~n", [Label, E]), fail))
	->  (   Got == Want
	    ->  format("  ~w~t~30| ok~n", [Label])
	    ;   format("  ~w~t~30| GOT ~p WANT ~p~n", [Label, Got, Want])
	    )
	;   format("  ~w~t~30| FAILED~n", [Label])
	).

err(Label, Goal) :-
	(   catch(Goal, error(E, _), (format("  ~w~t~30| ~p~n", [Label, E]), true))
	->  true
	;   format("  ~w~t~30| NO ERROR RAISED~n", [Label])
	).

main :-
	write('py_type'), nl,
	chk('an integer', py_type(42, T1), T1, int),
	chk('a bignum', ( B is 2^70, py_type(B, T2) ), T2, int),
	chk('a float', py_type(1.5, T3), T3, float),
	chk('an atom', py_type(hello, T4), T4, str),
	chk('a list', py_type([1,2], T5), T5, list),
	chk('a tuple', py_type(-(1,2), T6), T6, tuple),
	chk('a dict', py_type({a:1}, T7), T7, dict),
	chk('the empty dict', py_type({}, T8), T8, dict),
	chk('a set', py_type(py_set([1]), T9), T9, set),
	chk('a rational', ( R is 1 rdiv 3, py_type(R, T10) ), T10, 'Fraction'),
	chk('@true', py_type(@true, T11), T11, bool),
	chk('@none', py_type(@none, T12), T12, 'NoneType'),
	chk('a handle',
	    ( py_call(builtins:list([1]), H, [py_object(true)]), py_type(H, T13) ),
	    T13, list),

	nl, write('py_pp'), nl,
	chk('a dict prints as Python',
	    with_output_to(atom(P1), py_pp({a:1, b:2})), P1, '{\'a\': 1, \'b\': 2}\n'),
	chk('a list prints as Python',
	    with_output_to(atom(P2), py_pp([1, hello, 2.5])), P2, '[1, \'hello\', 2.5]\n'),

	nl, write('XSB spellings'), nl,
	chk('add_py_lib_dir',
	    ( add_py_lib_dir('/tmp/janus_xsb'), py_lib_dirs(Ds), last(Ds, L) ),
	    L, '/tmp/janus_xsb'),
	chk('janus_python_version',
	    ( janus_python_version(V), sub_atom(V, 0, 2, _, Pre) ), Pre, '3.'),
	chk('value/3 commits',
	    findall(X, value({a:1, b:2}, K, X), Xs), Xs, [1]),
	chk('values/3 still enumerates',
	    findall(K-X, values({a:1, b:2}, K, X), Ys), Ys, [a-1, b-2]),
	chk('value/3 by key', value({a:1, b:2}, b, V2), V2, 2),

	nl, write('obj_dir and obj_dict'), nl,
	% not a str: with py_object(true) an exact scalar still translates,
	% so builtins:str(hi) comes back as the atom hi and not a handle.
	chk('obj_dir of a list lists methods',
	    ( py_call(builtins:list([1]), S, [py_object(true)]),
	      obj_dir(S, Dir), memberchk(append, Dir) -> D1 = yes ; D1 = no ),
	    D1, yes),
	chk('obj_dict of an instance',
	    ( janus:py_run_('import builtins\nclass _P:\n    def __init__(self): self.x = 7\nbuiltins._p = _P()'),
	      py_call(builtins:'_p', Obj, [py_object(true)]),
	      obj_dict(Obj, Dict) ), Dict, {x:7}),

	nl, write('py_next'), nl,
	chk('steps an iterator',
	    ( py_call(builtins:iter([10,20,30]), It, [py_object(true)]),
	      py_next(It, N1), py_next(It, N2), py_next(It, N3) ),
	    [N1,N2,N3], [10,20,30]),
	chk('fails when exhausted',
	    ( py_call(builtins:iter([1]), It2, [py_object(true)]),
	      py_next(It2, _),
	      ( py_next(It2, _) -> R1 = again ; R1 = exhausted ) ),
	    R1, exhausted),
	err('py_next of a non-object', py_next(hello, _)),
	err('py_next of an unbound var', py_next(_, _)).
