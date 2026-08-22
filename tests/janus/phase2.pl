% Phase 2 acceptance: calling. py_func/3,4, py_dot/3,4, py_call/2,3,
% py_setattr/3, keyword arguments, `:` chains, and the Options argument.

:- use_module(library(janus)).

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
	write('py_func'), nl,
	chk('math.factorial(20)', py_func(math, factorial(20), R1), R1, 2432902008176640000),
	chk('math.factorial(30) bignum', py_func(math, factorial(30), R2), R2, 265252859812191058636308480000000),
	chk('math.sqrt -> float', py_func(math, sqrt(16.0), R3), R3, 4.0),
	chk('builtins.len', py_func(builtins, len([a,b,c]), R4), R4, 3),
	chk('module attribute', py_func(math, pi, R5), R5, 3.141592653589793),

	nl, write('py_call and : chains'), nl,
	chk('builtins:str(42)', py_call(builtins:str(42), R6), R6, '42'),
	chk('chain os:path:join', py_call(os:path:join(a,b), R7), R7, 'a/b'),
	chk('attribute chain sys:maxsize', py_call(sys:maxsize, R8), R8, 9223372036854775807),
	chk('dict round trip through Python',
	    py_call(builtins:dict(a=1, b=2), R9), R9, {a:1, b:2}),

	nl, write('py_dot on an object'), nl,
	% An atom on the left of : is a MODULE name, in this port as in SWI,
	% where py_dot(hello, upper(), X) raises ModuleNotFoundError too.
	err('bare atom is a module', py_dot(hello, '()'(upper), _)),
	chk('method on a handle',
	    ( py_call(builtins:list([3,1,2]), L, [py_object(true)]),
	      py_dot(L, '()'(sort), _),
	      py_dot(L, '()'(copy), R11) ), R11, [1,2,3]),

	nl, write('keyword arguments'), nl,
	chk('positional + keyword',
	    py_call(builtins:round(3.14159, ndigits=2), R12), R12, 3.14),
	chk('keyword only',
	    py_call(builtins:int('ff', base=16), R13), R13, 255),
	chk('two keywords',
	    py_func(builtins, sorted([3,1,2], reverse = @true), R14), R14, [3,2,1]),
	err('positional after keyword',
	    py_call(builtins:round(ndigits=2, 3.14159), _)),

	nl, write('py_setattr'), nl,
	chk('setattr then read back',
	    ( py_setattr(builtins, '_janus_probe', hello),
	      py_call(builtins:'_janus_probe', R15) ), R15, hello),

	nl, write('the Options argument'), nl,
	chk('int, default', py_call(builtins:int(42), R16), R16, 42),
	chk('int, py_object(true)',
	    py_call(builtins:int(42), R17, [py_object(true)]), R17, 42),
	chk('list, default', py_call(builtins:list([1,2]), R18), R18, [1,2]),
	handle('list, py_object(true)',
	    py_call(builtins:list([1,2]), R19, [py_object(true)]), R19),
	handle('dict, py_object(true)',
	    py_call(builtins:'()'(dict), R20, [py_object(true)]), R20),
	chk('tuple, py_object(true)',
	    py_call(builtins:tuple([1,2]), R21, [py_object(true)]), R21, -(1,2)),
	chk('XSB sizecheck accepted',
	    py_call(builtins:int(1), R22, [sizecheck(true)]), R22, 1),
	err('unknown option', py_call(builtins:int(1), _, [no_such_option(x)])),
	err('options not a list', py_call(builtins:int(1), _, no_such_option)),

	nl, write('errors'), nl,
	err('no such module', py_call(no_such_module_xyz:'()'(f), _)),
	err('no such attribute', py_call(math:no_such_attr, _)),
	err('unbound call', py_call(_, _)),

	nl, write('the GIL: four threads into CPython at once'), nl,
	threads.

% Without PyGILState_Ensure this is a crash rather than a race, since
% Trealla's threads are real pthreads and the FFI is reentrant across
% them.
worker(N) :-
	between(1, 250, _),
	py_func(math, factorial(N), _),
	py_call(builtins:str(N), _),
	fail.
worker(_).

threads :-
	findall(T, (member(N, [20, 30, 40, 50]),
	            thread_create(worker(N), T, [])), Ts),
	forall(member(T, Ts), thread_join(T, _)),
	format("  1000 calls across 4 threads~t~30| ok~n").

handle(Label, Goal, Got) :-
	(   catch(Goal, E, (format("  ~w~t~30| RAISED ~p~n", [Label, E]), fail))
	->  (   Got = '$py_obj'(_)
	    ->  format("  ~w~t~30| ok, is a handle~n", [Label])
	    ;   format("  ~w~t~30| TRANSLATED TO ~p~n", [Label, Got])
	    )
	;   format("  ~w~t~30| FAILED~n", [Label])
	).
