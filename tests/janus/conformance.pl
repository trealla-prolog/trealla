% Phase 7: conformance against the shared Janus test suite.
%
% A port of test_xsb_janus.pl from SWI's swipy package, which is itself a
% port of XSB's testSuite.P - the compatibility suite for the common core.
% It drives the SAME Python fixtures (xsb_tests/*.py), so what is being
% checked is this implementation against theirs, not against itself.
%
% It could not be run unmodified, and not only for the f() syntax:
%
%   - it is written for plunit;
%   - its expected dicts are SWI's native py{k:v} rather than the {k:v}
%     the agreed interface specifies, which is what this side produces;
%   - it reads error text through SWI's message_to_string/2;
%   - min_tagged_integer / max_tagged_integer are SWI flags.
%
% The test CONTENT is otherwise unchanged. Where an expectation differs
% it is because the two systems genuinely differ, and that is noted at
% the test.
%
% The fixtures are third-party and not vendored here. Point
% JANUS_XSB_TESTS at them, or leave it and the default is used; the
% suite reports that it skipped rather than failing when they are absent.

:- set_prolog_flag(empty_args, true).

:- use_module(library(janus)).
:- use_module(library(lists)).

:- dynamic(failed/1).

fixtures(Dir) :-
	(   getenv('JANUS_XSB_TESTS', Dir)
	->  true
	;   expand_home('~/swipl-devel/packages/swipy/xsb_tests', Dir)
	).

expand_home(Path, Full) :-
	(   sub_atom(Path, 0, 2, _, '~/')
	->  getenv('HOME', Home),
	    sub_atom(Path, 2, _, 0, Rest),
	    atomic_list_concat([Home, '/', Rest], Full)
	;   Full = Path
	).

t(Label, Goal) :-
	(   catch(Goal, E, (format("  ~w~t~34| RAISED ~p~n", [Label, E]),
	                    assertz(failed(Label)), fail))
	->  format("  ~w~t~34| ok~n", [Label])
	;   format("  ~w~t~34| FAILED~n", [Label]),
	    ( failed(Label) -> true ; assertz(failed(Label)) )
	).

main :-
	fixtures(Dir),
	atomic_list_concat([Dir, '/returnVal.py'], Probe),
	(   exists_file(Probe)
	->  run(Dir)
	;   format("skipped: no fixtures at ~w~n", [Dir]),
	    format("set JANUS_XSB_TESTS to the swipy xsb_tests directory~n")
	).

run(Dir) :-
	py_add_lib_dir(Dir, first),

	write('calling'), nl,
	t(sumlist, ( py_func(sumlist3, sumlist3(5, [1,2,3]), [6,7,8]),
	             \+ py_func(sumlist3, sumlist3(5, [1,2,3]), [4,5,6]),
	             \+ py_func(sumlist3, sumlist3(5, [1,2,3]), [1,2,3]) )),

	write('data conversion'), nl,
	t(integers, int_conv),
	t(floats, float_conv),
	t(strings, string_conv),
	t(lists, list_conv),
	t(sets, set_conv),
	t(tuples, tuple_conv),
	t(dicts, dict_conv),
	t('@none', py_func(returnVal, return_None(), @none)),
	t('@true', py_func(returnVal, return_True(), @true)),
	t('@false', py_func(returnVal, return_False(), @false)),

	write('json'), nl,
	t(json_string, json_1),
	t(json_file, json_2(Dir)),

	write('keyword arguments'), nl,
	t(kwargs, ( py_func(kwargs, kwargs_append(foo, bar=1, baz=2), R),
	            R == [foo, -(bar,1), -(baz,2)] )),

	write('errors'), nl,
	t(no_such_module, error_has(no_module, foo(1), ['ModuleNotFoundError'])),
	t(no_such_function, error_has(kwargs, foo(1), ['AttributeError', foo])),
	t(raised_in_python, error_has(test_err, raise_err_1(), ['Exception', spam])),

	write('methods and variadics'), nl,
	t(methods, meth_tests),
	t(variadic, variadic_tests),

	write('gc'), nl,
	t(gc, ( py_func(gc, collect(), C), integer(C) )),

	nl,
	aggregate_failures.

aggregate_failures :-
	findall(L, failed(L), Ls),
	(   Ls == []
	->  write('all conformance tests passed'), nl
	;   length(Ls, N),
	    format("~w FAILED: ~w~n", [N, Ls])
	).

% Trealla has no min/max_tagged_integer: its integers are unbounded and
% the FFI's own limit is what matters, so the boundary tested here is
% that one - see the smallint asymmetry in docs/janus-design.md section 3.
int_conv :-
	Max is 2^63-1, Min is -(2^63)+1, Big is 2^70,
	py_func(returnVal, returnVal(Max), Max),
	py_func(returnVal, returnVal(Min), Min),
	py_func(returnVal, returnVal(Big), Big).

float_conv :-
	py_func(returnVal, returnVal(3.54), 3.54),
	py_func(returnVal, returnVal(3.5535252352), 3.5535252352).

string_conv :-
	py_func(returnVal, returnVal(helloworld), helloworld),
	py_func(returnVal, returnVal('Санкт-Петербург'), R),
	R == 'Санкт-Петербург'.

list_conv :-
	py_func(returnVal, returnVal([a,b,c]), [a,b,c]),
	py_func(returnVal, returnVal([]), R2), R2 == [],
	py_func(returnVal, returnVal([1,[2,3,4],[hello,155]]), R3),
	R3 == [1, [2,3,4], [hello,155]],
	py_func(tupInList, func(), R4),
	R4 == [1, 2, 3, -(5,6), hello, [11,17]].

set_conv :-
	py_func(returnVal, returnSet(), F),
	F = ['"foo"', '''bar''', py_set(S)],
	length(S, 3),
	py_func(returnVal, returnVal(py_set([a,b,c])), R1),
	arg(1, R1, A),
	length(A, 3).

% The empty tuple is the atom '-', there being no zero-arity compound to
% be. SWI writes the same expectation as -().
tuple_conv :-
	py_func(returnVal, returnVal(-(a,b,c)), -(a,b,c)),
	py_func(tupletest, func(), R2),
	R2 == -(5, -, hello, -(5,6,7)).

% SWI expects its native py{...}; the agreed interface says {k:v}, which
% is what this returns.
dict_conv :-
	py_func(returnVal, return_dictionary(), Ret),
	Ret == {'Name':'Geeks', 1:[1,2,3,4]}.

json_1 :-
	J = '{"name": "Bob", "languages": ["English", "Fench","GERMAN"]}',
	py_func(jintf, prolog_loads(J), F),
	F == {name:'Bob', languages:['English','Fench','GERMAN']}.

json_2(Dir) :-
	atomic_list_concat([Dir, '/sample.json'], File),
	py_func(jintf, prolog_load(File), F),
	values(F, [glossary, title], T),
	T == 'example glossary'.

error_has(Mod, Goal, Parts) :-
	catch(py_func(Mod, Goal, _), error(python_error(Type, Msg), _), true),
	nonvar(Type),
	atomic_list_concat([Type, ' ', Msg], Text),
	forall(member(P, Parts), sub_atom(Text, _, _, _, P)).

meth_tests :-
	py_func('Person', 'Person'(john, 35), Obj),
	py_dot(Obj, func0(), R1), R1 == 'Hello my name is john',
	py_dot(Obj, func1(doofus), R2),
	R2 == 'Hello my name is john and I\'m a doofus',
	py_dot(Obj, favorite_ice_cream, R3), R3 == chocolate,
	py_dot(Obj, func2(real, doofus), R4),
	R4 == 'Hello my name is john and I\'m a real doofus',
	py_dot(Obj, func3(real, big, doofus), R5),
	R5 == 'Hello my name is john and I\'m a real big doofus',
	py_free(Obj).

variadic_tests :-
	py_func(variadic, variadic_print(a,b,c), A), A == 'a|b|c|',
	py_func(variadic, variadic_print(a,b,c,d), B), B == 'a|b|c|d|',
	py_func(variadic, opt_print(a), C), C == 'a|1',
	py_func(variadic, opt_print(b,c), D), D == 'b|c'.
