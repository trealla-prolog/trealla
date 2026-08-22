% Phase 5 acceptance: errors.
%
% The split both reference systems agree on: a Python exception becomes
% a Prolog exception; instantiation, type and domain faults stay Prolog
% errors raised BEFORE the call is made.

:- set_prolog_flag(empty_args, true).
:- use_module(library(janus)).
:- use_module(library(lists)).

pyerr(Label, Goal, WantType) :-
	(   catch(Goal, error(E, _), true)
	->  (   nonvar(E), E = python_error(Type, Msg)
	    ->  (   Type == WantType
		->  format("  ~w~t~30| ~w: ~w~n", [Label, Type, Msg])
		;   format("  ~w~t~30| GOT ~w WANT ~w~n", [Label, Type, WantType])
		)
	    ;   format("  ~w~t~30| NOT A PYTHON ERROR: ~p~n", [Label, E])
	    )
	;   format("  ~w~t~30| NO ERROR RAISED~n", [Label])
	).

plerr(Label, Goal) :-
	(   catch(Goal, error(E, _), true)
	->  (   nonvar(E), E = python_error(_, _)
	    ->  format("  ~w~t~30| WENT TO PYTHON: ~p~n", [Label, E])
	    ;   format("  ~w~t~30| ~p~n", [Label, E])
	    )
	;   format("  ~w~t~30| NO ERROR RAISED~n", [Label])
	).

chk(Label, Goal, Got, Want) :-
	(   catch(Goal, E, (format("  ~w~t~30| RAISED ~p~n", [Label, E]), fail))
	->  (   Got == Want
	    ->  format("  ~w~t~30| ok~n", [Label])
	    ;   format("  ~w~t~30| GOT ~p WANT ~p~n", [Label, Got, Want])
	    )
	;   format("  ~w~t~30| FAILED~n", [Label])
	).

main :-
	write('Python exceptions become Prolog exceptions'), nl,
	pyerr('divide by zero', py_call(operator:truediv(1, 0), _), 'ZeroDivisionError'),
	pyerr('bad int literal', py_call(builtins:int(notanumber), _), 'ValueError'),
	pyerr('missing attribute', py_call(math:no_such_attr, _), 'AttributeError'),
	pyerr('missing dict key',
	      ( py_call(builtins:dict(), D, [py_object(true)]),
	        py_dot(D, pop(missing), _) ), 'KeyError'),
	pyerr('wrong argument type', py_call(math:sqrt(hello), _), 'TypeError'),
	pyerr('too many arguments', py_call(math:sqrt(1.0, 2.0), _), 'TypeError'),
	pyerr('raised from Python code',
	      ( janus:py_run_('import builtins\ndef _boom(): raise RuntimeError("deliberate")\nbuiltins._boom = _boom'),
	        py_call(builtins:'_boom'(), _) ), 'RuntimeError'),

	nl, write('Prolog faults stay on this side'), nl,
	plerr('unbound goal', py_call(_, _)),
	plerr('unbound argument', py_call(builtins:str(_), _)),
	plerr('untranslatable argument', py_call(builtins:str(foo(bar)), _)),
	plerr('bad option', py_call(builtins:str(1), _, [nope(x)])),
	plerr('options not a list', py_call(builtins:str(1), _, nope)),
	plerr('bad @ constant', py_call(builtins:str(@maybe), _)),
	plerr('py_free of a non-object', py_free(hello)),
	plerr('unknown module', py_call(no_such_module_qq:f(), _)),

	nl, write('the interpreter is still usable afterwards'), nl,
	chk('a call after an error', py_call(builtins:str(42), R1), R1, '42'),
	chk('no error left pending',
	    ( catch(py_call(math:sqrt(hello), _), _, true),
	      py_call(builtins:len([1,2,3]), R2) ), R2, 3),
	chk('errors in a loop',
	    ( forall(between(1, 200, _),
	             catch(py_call(math:sqrt(hello), _), _, true)),
	      py_call(builtins:len([a]), R3) ), R3, 1),

	nl, write('the pre-3.12 fetch path gives the same answer'), nl,
	legacy_path.

% CPython 3.12 replaced PyErr_Fetch with PyErr_GetRaisedException. Both
% resolve on this interpreter, so the older path can be exercised by
% hiding the newer one - which is the only way to test it without an
% older Python to hand.
legacy_path :-
	(   janus:py_have_('PyErr_Fetch')
	->  janus:py_use_error_api_(fetch),
	    catch(py_call(builtins:int(notanumber), _), error(E, _), true),
	    janus:py_use_error_api_(default),
	    (   nonvar(E), E = python_error(T, M)
	    ->  format("  via PyErr_Fetch~t~30| ~w: ~w~n", [T, M])
	    ;   format("  via PyErr_Fetch~t~30| UNEXPECTED ~p~n", [E])
	    )
	;   format("  via PyErr_Fetch~t~30| not available on this build~n")
	),
	% and the default selection is back
	janus:py_error_api(Api),
	format("  api in use~t~30| ~w~n", [Api]),
	catch(py_call(builtins:int(notanumber), _), error(E2, _), true),
	(   nonvar(E2), E2 = python_error(T2, _)
	->  format("  back to GetRaisedException~t~30| ~w~n", [T2])
	;   format("  back to GetRaisedException~t~30| UNEXPECTED ~p~n", [E2])
	).
