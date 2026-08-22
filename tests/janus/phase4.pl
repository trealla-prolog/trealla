% Phase 4 acceptance: lifetime.
%
% The ownership rule of docs/janus-design.md section 3, checked against
% CPython's own reference counts rather than inferred from the code:
%
%   - marshalling BORROWS, so translating an object leaves its count
%     untouched;
%   - a handle owns exactly one reference;
%   - py_free/1 releases exactly that one.

:- set_prolog_flag(empty_args, true).
:- use_module(library(janus)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

chk(Label, Goal, Got, Want) :-
	(   catch(Goal, E, (format("  ~w~t~34| RAISED ~p~n", [Label, E]), fail))
	->  (   Got == Want
	    ->  format("  ~w~t~34| ok~n", [Label])
	    ;   format("  ~w~t~34| GOT ~p WANT ~p~n", [Label, Got, Want])
	    )
	;   format("  ~w~t~34| FAILED~n", [Label])
	).

err(Label, Goal) :-
	(   catch(Goal, error(E, _), (format("  ~w~t~34| ~p~n", [Label, E]), true))
	->  true
	;   format("  ~w~t~34| NO ERROR RAISED~n", [Label])
	).

% A stable object on the Python side, plus a way to read its count that
% does not itself go through the marshaller.
setup_probe :-
	janus:py_run_('import sys, builtins\nbuiltins._obj = [1, 2, 3]\nbuiltins._rc = lambda: sys.getrefcount(builtins._obj)').

rc(N) :- py_call(builtins:'_rc'(), N).

translate_many(0) :- !.
translate_many(N) :- py_call(builtins:'_obj', _), M is N-1, translate_many(M).

handles(0, []) :- !.
handles(N, [H|T]) :-
	py_call(builtins:'_obj', H, [py_object(true)]),
	M is N-1, handles(M, T).

main :-
	setup_probe,

	write('py_is_object'), nl,
	py_call(builtins:'_obj', Handle, [py_object(true)]),
	chk('a handle is an object', ( py_is_object(Handle) -> R1 = yes ; R1 = no ), R1, yes),
	chk('a list is not', ( py_is_object([1,2,3]) -> R2 = yes ; R2 = no ), R2, no),
	chk('an atom is not', ( py_is_object(foo) -> R3 = yes ; R3 = no ), R3, no),
	chk('an unbound var is not', ( py_is_object(_) -> R4 = yes ; R4 = no ), R4, no),

	nl, write('marshalling borrows'), nl,
	rc(Before),
	translate_many(2000),
	rc(After),
	Drift is After - Before,
	chk('2000 translations, count drift', true, Drift, 0),

	nl, write('a handle owns exactly one reference'), nl,
	rc(B2),
	handles(50, Hs),
	rc(A2),
	Gained is A2 - B2,
	chk('50 handles, count rises by', true, Gained, 50),
	maplist(py_free, Hs),
	rc(A3),
	Net is A3 - B2,
	chk('after py_free of all 50', true, Net, 0),

	nl, write('py_free of the first handle'), nl,
	rc(B3),
	py_free(Handle),
	rc(A4),
	Freed is B3 - A4,
	chk('releases one reference', true, Freed, 1),

	nl, write('errors'), nl,
	err('py_free of an unbound var', py_free(_)),
	err('py_free of a non-object', py_free(foo)),
	err('py_free of a list', py_free([1,2])),

	nl, write('the declared reference discipline'), nl,
	discipline.

% The classification is data, not a comment, so it can be checked: every
% pointer-valued entry point has to say which kind of reference it hands
% back, and nothing else may claim to hand one back at all.
discipline :-
	findall(N, (janus:py_sig(N, _, ptr, D),
	            \+ member(D, [new, borrowed, foreign])), Bad1),
	chk('every ptr return is classified', true, Bad1, []),
	findall(N, (janus:py_sig(N, _, R, none), member(R, [ptr, ccstr])), Bad0),
	chk('no pointer return says none', true, Bad0, []),
	findall(N, (janus:py_sig(N, _, ccstr, D), D \== borrowed), Bad2),
	chk('every ccstr return is borrowed', true, Bad2, []),
	findall(N, (janus:py_sig(N, _, R, D), member(D, [new, borrowed]),
	            \+ member(R, [ptr, ccstr])), Bad3),
	chk('nothing else claims a reference', true, Bad3, []),
	findall(N, (janus:py_steals(N, _), \+ janus:py_sig(N, _, _, _)), Bad4),
	chk('every steals names a real entry', true, Bad4, []),
	aggregate_all(count, janus:py_sig(_,_,_,_), Total),
	format("  ~w entry points declared~n", [Total]).
