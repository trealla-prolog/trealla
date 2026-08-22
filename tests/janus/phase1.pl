% Phase 1 acceptance: the bi-translation table of docs/janus-design.md
% section 3, both directions, recursive.
%
% Almost every marshalling bug is catchable by a round trip - build a
% term, send it, read it back, compare - so that is what this does. The
% awkward cases are deliberate: they are what a conformance suite
% written against another system will not think to probe.

:- use_module(library(janus)).

rt(Label, Term) :-
	(   catch(janus:py_round_trip(Term, Back), E,
	          (format("  ~w~t~26| RAISED ~p~n", [Label, E]), fail))
	->  (   Back == Term
	    ->  format("  ~w~t~26| ok~n", [Label])
	    ;   format("  ~w~t~26| MISMATCH ~p -> ~p~n", [Label, Term, Back])
	    )
	;   format("  ~w~t~26| FAILED~n", [Label])
	).

% Numbers compare by value, not by term identity
rt_num(Label, Term) :-
	(   catch(janus:py_round_trip(Term, Back), E,
	          (format("  ~w~t~26| RAISED ~p~n", [Label, E]), fail))
	->  (   Back =:= Term
	    ->  format("  ~w~t~26| ok~n", [Label])
	    ;   format("  ~w~t~26| MISMATCH ~p -> ~p~n", [Label, Term, Back])
	    )
	;   format("  ~w~t~26| FAILED~n", [Label])
	).

main :-
	write('scalars'), nl,
	rt('@none', @none),
	rt('@true', @true),
	rt('@false', @false),
	rt_num('small int', 42),
	rt_num('negative int', -42),
	rt_num('zero', 0),
	rt_num('float', 1.5),
	rt('atom', hello),
	rt('atom, UTF-8', 'naïve — ünïcodé ☺'),
	rt('empty atom', ''),

	nl, write('integers at and past the int64 edge'), nl,
	Max is 2^63-1,      rt_num('2^63-1', Max),
	Min is -(2^63),     rt_num('-2^63', Min),
	Over is 2^63,       rt_num('2^63', Over),
	Big is 2^70,        rt_num('2^70', Big),
	NegBig is -(2^70),  rt_num('-2^70', NegBig),
	Huge is 2^40000,    rt_num('2^40000 (past the', Huge),
	Vast is 2^400000,   rt_num('2^400000  digit cap)', Vast),

	nl, write('rationals'), nl,
	Third is 1 rdiv 3,        rt_num('1 rdiv 3', Third),
	NegSev is -2 rdiv 7,      rt_num('-2 rdiv 7', NegSev),
	BigRat is (2^70) rdiv 3,  rt_num('(2^70) rdiv 3', BigRat),
	VastRat is (2^40000) rdiv 3, rt_num('(2^40000) rdiv 3', VastRat),

	nl, write('containers, and the awkward ones'), nl,
	rt('empty list', []),
	rt('list', [1,2,3]),
	rt('mixed list', [1, a, 1.5, @none]),
	rt('empty dict is an atom', {}),
	rt('dict', {a:1, b:2}),
	rt('single-entry dict', {a:1}),
	rt('1-tuple', -(1)),
	rt('2-tuple', -(1,2)),
	rt('3-tuple', -(1,2,3)),
	rt('nested dict-in-list-in-tuple', -([{a:1}, {b:[2,3]}], x)),
	rt('set', py_set([1,2,3])),
	rt('empty set', py_set([])),

	nl, write('opaque fallback'), nl,
	opaque_check,

	nl, write('errors'), nl,
	err('unbound', _),
	err('unknown functor', foo(1)),
	err('bad @ constant', @maybe).

% bytes is a sequence but not in the table, so it must stay opaque and
% not quietly become a list of integers.
% Anything reaching the C API directly has to hold the GIL: since phase
% 2 nobody holds it between calls, and a bare py_to_pl/2 here segfaults
% the process on the way out with every test already reported as passed.
opaque_check :-
	janus:py_run_('import builtins\nbuiltins._probe = b"xy"'),
	janus:py_gil(( janus:py_import_attr(builtins, '_probe', Obj),
	               janus:py_to_pl(Obj, Term) )),
	(   Term = '$py_obj'(_)
	->  format("  bytes stays opaque~t~26| ok~n")
	;   format("  bytes~t~26| LEAKED AS ~p~n", [Term])
	).

err(Label, Term) :-
	(   catch(janus:py_round_trip(Term, _), error(E, _),
	          (format("  ~w~t~26| ~p~n", [Label, E]), true))
	->  true
	;   format("  ~w~t~26| NO ERROR RAISED~n", [Label])
	).
