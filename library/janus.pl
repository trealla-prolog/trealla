% Janus Prolog-Python interface - phases 0 and 1.
%
% See docs/janus-design.md. Phase 0 is the build wiring, finding
% libpython, the shutdown hook and py_version/0. Phase 1 is the
% marshaller: the bi-translation table of the design's section 3, both
% directions, recursive. Calling and iteration arrive in phases 2-3.
%
% REFERENCE OWNERSHIP, which is the rule everything here obeys:
%
%   - pl_to_py/2 returns a NEW reference. The caller owns it and must
%     Py_DecRef it.
%   - py_to_pl/2 BORROWS its object. It never releases what it was
%     given; the caller keeps ownership. The one exception is the
%     fallback to '$py_obj'/1, which increfs so the handle owns a
%     reference of its own, released by py_free/1 in phase 4.
%   - Whether an entry point returns a new or a borrowed reference is a
%     property of the function, not of the value, so it is recorded in
%     py_sig/3 below and never decided at the call site.
%   - PyList_SetItem and PyTuple_SetItem STEAL their reference; the
%     other containers do not, so what goes into a dict or a set is
%     released afterwards and what goes into a list or tuple is not.
%
% Nothing here is reachable from a default build: the module is only
% embedded when built with `make janus`, so a stock `make` carries no
% reference to Python of any kind.

:- module(janus, [
	py_version/0,
	py_lib/1
	]).

:- use_module(library(lists)).

% The library is found by dlopen at run time, never at build time -
% configure-time detection would put a Python dependency back into the
% build. Three platforms, three shapes:
%
%   macOS    a framework path carrying no .so suffix at all
%   Linux    libpython3.X.so, with an 'm' ABI suffix before 3.8
%   Windows  python3XX.dll - no 'lib' prefix, version in the filename
%
% Newest first, so a machine with several installs gets the newest.
% PROLOG_PYTHON_LIB overrides the search entirely, which is the escape
% hatch for an install in none of these places.

py_minor_version(V) :- between(9, 20, N), V is 29 - N.

py_candidate(Lib) :-
	getenv('PROLOG_PYTHON_LIB', Lib).
py_candidate(Lib) :-
	py_minor_version(V),
	(   % macOS: Homebrew, python.org, and a MacPorts-style prefix
	    member(Dir, ['/opt/homebrew/Frameworks', '/usr/local/Frameworks',
	                 '/Library/Frameworks']),
	    format(atom(Lib), "~w/Python.framework/Versions/3.~w/Python", [Dir, V])
	;   % Linux, and anything else with a conventional soname
	    format(atom(Lib), "libpython3.~w.so", [V])
	;   format(atom(Lib), "libpython3.~w.so.1.0", [V])
	;   % Windows: neither the prefix nor the suffix mapping reaches this
	    format(atom(Lib), "python3~w.dll", [V])
	).

:- dynamic(py_lib_/1).

%   py_lib(-Path) is semidet.
%
%   The libpython this session is bound to.

py_lib(Lib) :- py_lib_(Lib).

py_open(Handle, Lib) :-
	py_candidate(Lib),
	catch('$dlopen'(Lib, 0, Handle), _, fail),
	!.

% Every entry point phase 0 needs. Two of them take no arguments at all,
% which is why this phase also carries the '$register_predicate'/4 fix.

% The third column of the comment is the reference the call returns:
% (new) must be released, (borrowed) must not, (steals) consumes the one
% handed to it. A borrowed char* must be ccstr and never cstr, or
% Trealla's allocator is handed CPython's own buffer.

py_sig('Py_InitializeEx',   [sint32], void).
py_sig('Py_FinalizeEx',     [],       sint32).
py_sig('Py_IsInitialized',  [],       sint32).
py_sig('Py_GetVersion',     [],       ccstr).           % borrowed
py_sig('PyRun_SimpleString', [cstr],  sint32).
py_sig('Py_IncRef',         [ptr],    void).
py_sig('Py_DecRef',         [ptr],    void).

% Errors. Phase 5 turns these into Prolog exceptions properly; phase 1
% only needs to notice a NULL and not marshal it.
py_sig('PyErr_Occurred',    [],       ptr).             % borrowed
py_sig('PyErr_Clear',       [],       void).

% Prolog -> Python
py_sig('PyLong_FromLongLong', [sint64],           ptr). % new
py_sig('PyLong_FromString',   [cstr, ptr, sint32], ptr).% new
py_sig('PyFloat_FromDouble',  [double],           ptr). % new
py_sig('PyUnicode_FromString',[cstr],             ptr). % new
py_sig('PyBool_FromLong',     [sint32],           ptr). % new
py_sig('PyList_New',          [sint64],           ptr). % new
py_sig('PyList_SetItem',      [ptr, sint64, ptr], sint32). % steals
py_sig('PyTuple_New',         [sint64],           ptr). % new
py_sig('PyTuple_SetItem',     [ptr, sint64, ptr], sint32). % steals
py_sig('PyDict_New',          [],                 ptr). % new
py_sig('PyDict_SetItem',      [ptr, ptr, ptr],    sint32).
py_sig('PySet_New',           [ptr],              ptr). % new
py_sig('PySet_Add',           [ptr, ptr],         sint32).

% Python -> Prolog
py_sig('PyObject_Type',       [ptr],              ptr). % new
py_sig('PyObject_IsInstance', [ptr, ptr],         sint32).
py_sig('PyObject_IsTrue',     [ptr],              sint32).
py_sig('PyLong_AsLongLongAndOverflow', [ptr, -sint32], sint64).
py_sig('PyNumber_ToBase',     [ptr, sint32],      ptr). % new
py_sig('PyFloat_AsDouble',    [ptr],              double).
py_sig('PyUnicode_AsUTF8',    [ptr],              ccstr).% borrowed
py_sig('PySequence_Size',     [ptr],              sint64).
py_sig('PySequence_GetItem',  [ptr, sint64],      ptr). % new
py_sig('PyDict_Keys',         [ptr],              ptr). % new
py_sig('PyObject_GetItem',    [ptr, ptr],         ptr). % new
py_sig('PyObject_GetIter',    [ptr],              ptr). % new
py_sig('PyIter_Next',         [ptr],              ptr). % new, 0 at end

% Building the type-object cache, and reaching fractions.Fraction
py_sig('PyImport_ImportModule',  [cstr],          ptr). % new
py_sig('PyObject_GetAttrString', [ptr, cstr],     ptr). % new
py_sig('PyObject_CallObject',    [ptr, ptr],      ptr). % new

% A registered foreign predicate is reachable only from an unqualified
% call inside the module that registered it: '$register_predicate'/4 puts
% it on the prolog instance rather than in the module's own table, and
% neither janus:'Py_GetVersion'(V) from outside nor a bare call from user
% finds it. Every C-API call therefore has to live in this file, which is
% the design in any case - but it also means tests cannot reach the API
% directly, and have to go through a Prolog wrapper such as py_run_/1.

% This has to be a plain directive: it runs *during* load, so the clauses
% below compile against predicates that already exist. initialization/1
% runs after the file is loaded, by which time the callers have been
% compiled without them and raise existence_error at run time.

:- (   py_open(H, Lib)
   ->  assertz(py_lib_(Lib)),
       forall(py_sig(N, A, R), '$register_predicate'(H, N, A, R))
   ;   throw(error(existence_error(foreign_library, libpython),
                   'library(janus)':py_open/2))
   ).

% Start the interpreter, once. Py_InitializeEx(0) rather than
% Py_Initialize() so Python does not install its own signal handlers over
% Trealla's - SIGINT has to keep reaching the Prolog toplevel.

% Py_IsInitialized returns *nonzero* when the interpreter is up, not
% specifically 1, so the result has to be compared rather than matched.
% Matching on 1 makes both guards below fail silently, and the failure is
% invisible: py_init just initialises twice (harmless), while py_finalize
% quietly does nothing and Python's buffered output is lost at exit.

py_initialized :-
	'Py_IsInitialized'(R),
	R =\= 0.

py_init :-
	py_types_ready, !.
py_init :-
	(   py_initialized -> true ;   'Py_InitializeEx'(0)   ),
	py_learn_types.

% Shutdown. Registered by asserting a clause for atexit/0, which halt/0,1
% run through ignore/1 before '$halt'.
%
% Two properties this clause has to have, both of them easy to get wrong:
% it ends in fail, because ignore/1 takes the first solution and a clause
% that succeeds would stop any other library's hook from running; and it
% never throws, because an exception here aborts the goal before '$halt'
% is reached and the exit status is lost.

py_finalize :-
	py_initialized,
	!,
	py_forget_types,
	'Py_FinalizeEx'(_).
py_finalize.

% Qualified to user: halt/0,1 call ignore(atexit) against the global
% atexit/0, and a bare assertz/1 inside a module creates janus:atexit/0
% instead, which nothing ever calls. The failure is silent - the hook
% simply never runs, and the first symptom is Python's buffered output
% vanishing at exit.

:- assertz(user:(atexit :- catch(janus:py_finalize, _, true), fail)).

% Not part of the Janus interface. Phase 0 needs some way to reach the
% interpreter before phase 1's marshalling exists, and the shutdown test
% uses it to register a Python-side atexit handler - which is how we know
% Py_FinalizeEx really ran rather than merely being called.

py_run_(Source) :-
	py_init,
	'PyRun_SimpleString'(Source, 0).

%   py_version is det.
%
%   Print the Python version this session is bound to, and the library it
%   came from. The phase 0 smoke test: it exercises dlopen, a zero-argument
%   registration, a borrowed string return, and startup.

py_version :-
	py_init,
	'Py_GetVersion'(Version),
	py_lib_(Lib),
	format("Python ~w~nlibrary ~w~n", [Version, Lib]).


		 /*******************************
		 *   PHASE 1: THE TYPE CACHE    *
		 *******************************/

% PyLong_Check and its friends are C macros, so there is no symbol to
% dlsym, and the type objects they test against - PyLong_Type and the
% rest - are exported as data, which '$register_predicate'/4 cannot use
% either because it registers a symbol as something to CALL.
%
% PyObject_Type of any exemplar is the type object, though, and that is
% an ordinary exported function. So build one value of each type at
% startup and keep its type. Type objects are immortal, so the reference
% kept here is deliberate and never released.

:- dynamic(py_type_/2).           % Name, type object
:- dynamic(py_const_/2).          % Name, singleton object

py_types_ready :-
	py_type_(int, _).

py_forget_types :-
	retractall(py_type_(_, _)),
	retractall(py_const_(_, _)).

py_learn(Name, Exemplar) :-
	py_check(Exemplar),
	'PyObject_Type'(Exemplar, Type),
	py_check(Type),
	assertz(py_type_(Name, Type)),
	'Py_DecRef'(Exemplar).

py_import_attr(Module, Name, Obj) :-
	'PyImport_ImportModule'(Module, M),
	py_check(M),
	'PyObject_GetAttrString'(M, Name, Obj),
	'Py_DecRef'(M),
	py_check(Obj).

py_learn_types :-
	% None is a singleton, compared by identity rather than by type
	py_import_attr(builtins, 'None', NoneObj),
	assertz(py_const_(none, NoneObj)),

	'PyLong_FromLongLong'(1, I),    py_learn(int, I),
	'PyBool_FromLong'(1, B),        py_learn(bool, B),
	'PyFloat_FromDouble'(1.0, F),   py_learn(float, F),
	'PyUnicode_FromString'(x, U),   py_learn(str, U),
	'PyTuple_New'(0, T),            py_learn(tuple, T),
	'PyDict_New'(D),                py_learn(dict, D),
	'PyList_New'(0, L),             py_learn(list, L),
	'PySet_New'(0, S),              py_learn(set, S),

	% fractions.Fraction is a class, so it is already the "type"
	py_import_attr(fractions, 'Fraction', FC),
	assertz(py_type_(fraction, FC)).

%   py_check(+Obj) is det.
%
%   A failed CPython call returns NULL, and the FFI will happily marshal
%   that onwards - a NULL ccstr becomes a nonsense atom and the error
%   surfaces several steps later, naming the wrong predicate. So every
%   fallible result is tested where it is produced. Phase 5 replaces this
%   with the real Python exception.

py_check(Obj) :-
	Obj =:= 0,
	!,
	'PyErr_Clear',
	throw(error(system_error(python_call_failed), janus)).
py_check(_).

py_isa(Obj, Name) :-
	py_type_(Name, Type),
	'PyObject_IsInstance'(Obj, Type, 1).


		 /*******************************
		 *      PYTHON -> PROLOG        *
		 *******************************/

%   py_to_pl(+Obj, -Term) is det.
%
%   Borrows Obj. The order below is load-bearing, not a lookup table:
%   bool is a subclass of int in Python, so an int-first test turns True
%   into 1 and never reaches @true. bytes is a sequence, so list is
%   tested by type rather than by PySequence_Check, which keeps bytes on
%   the opaque path where the spec leaves it.

py_to_pl(Obj, Term) :-
	py_check(Obj),
	(   py_is_none(Obj)       ->  Term = @none
	;   py_isa(Obj, bool)     ->  py_bool_to_pl(Obj, Term)
	;   py_isa(Obj, int)      ->  py_int_to_pl(Obj, Term)
	;   py_isa(Obj, float)    ->  'PyFloat_AsDouble'(Obj, Term)
	;   py_isa(Obj, str)      ->  'PyUnicode_AsUTF8'(Obj, Term)
	;   py_isa(Obj, tuple)    ->  py_tuple_to_pl(Obj, Term)
	;   py_isa(Obj, dict)     ->  py_dict_to_pl(Obj, Term)
	;   py_isa(Obj, fraction) ->  py_fraction_to_pl(Obj, Term)
	;   py_isa(Obj, list)     ->  py_seq_to_pl(Obj, Term)
	;   py_isa(Obj, set)      ->  py_set_to_pl(Obj, Term)
	;   py_opaque(Obj, Term)
	).

py_is_none(Obj) :-
	py_const_(none, None),
	Obj =:= None.

py_bool_to_pl(Obj, Term) :-
	'PyObject_IsTrue'(Obj, R),
	(   R =:= 0 -> Term = @false ;   Term = @true   ).

% The int64 fast path, with the base-16 slow path behind it. Base 16 and
% not base 10: since 3.11 CPython caps DECIMAL integer conversion at
% sys.get_int_max_str_digits(), 4300 digits by default, and raises above
% it. Hex is exempt from that limit, and is faster besides.

py_int_to_pl(Obj, N) :-
	'PyLong_AsLongLongAndOverflow'(Obj, Overflow, V),
	(   Overflow =:= 0
	->  N = V
	;   'PyNumber_ToBase'(Obj, 16, S),
	    py_check(S),
	    'PyUnicode_AsUTF8'(S, Text),
	    atom_number(Text, N),
	    'Py_DecRef'(S)
	).

py_fraction_to_pl(Obj, Rational) :-
	py_attr_int(Obj, numerator, N),
	py_attr_int(Obj, denominator, D),
	Rational is N rdiv D.

py_attr_int(Obj, Name, N) :-
	'PyObject_GetAttrString'(Obj, Name, A),
	py_check(A),
	py_int_to_pl(A, N),
	'Py_DecRef'(A).

py_seq_items(Obj, I, N, []) :-
	I >= N,
	!.
py_seq_items(Obj, I, N, [H|T]) :-
	'PySequence_GetItem'(Obj, I, Item),
	py_check(Item),
	py_to_pl(Item, H),
	'Py_DecRef'(Item),
	J is I + 1,
	py_seq_items(Obj, J, N, T).

py_seq_to_pl(Obj, List) :-
	'PySequence_Size'(Obj, N),
	py_seq_items(Obj, 0, N, List).

% A tuple of arity N is a compound -/N. The empty tuple has nowhere else
% to go than the atom '-', since a compound of arity zero is not a term.

py_tuple_to_pl(Obj, Term) :-
	py_seq_to_pl(Obj, Items),
	Term =.. [-|Items].

% A dict is a curly term wrapping a comma-list of :/2 - except the empty
% one, which is the ATOM {} and not a compound at all.

py_dict_to_pl(Obj, Term) :-
	'PyDict_Keys'(Obj, Keys),
	py_check(Keys),
	'PySequence_Size'(Keys, N),
	(   N =:= 0
	->  Term = {}
	;   py_dict_pairs(Obj, Keys, 0, N, Pairs),
	    py_comma_list(Pairs, Inner),
	    Term = {Inner}
	),
	'Py_DecRef'(Keys).

py_dict_pairs(_, _, I, N, []) :-
	I >= N,
	!.
py_dict_pairs(Obj, Keys, I, N, [K:V|T]) :-
	'PySequence_GetItem'(Keys, I, KeyObj),
	py_check(KeyObj),
	py_to_pl(KeyObj, K),
	'PyObject_GetItem'(Obj, KeyObj, ValObj),
	'Py_DecRef'(KeyObj),
	py_check(ValObj),
	py_to_pl(ValObj, V),
	'Py_DecRef'(ValObj),
	J is I + 1,
	py_dict_pairs(Obj, Keys, J, N, T).

py_comma_list([X], X) :-
	!.
py_comma_list([X|Xs], (X,Rest)) :-
	py_comma_list(Xs, Rest).

% And the same structure taken apart. This cannot be py_comma_list/2 run
% backwards: its first clause cuts, so (a:1,b:2) comes back as the single
% element [(a:1,b:2)] and every dict with more than one key is rejected.

py_uncomma((A,B), [A|T]) :-
	!,
	py_uncomma(B, T).
py_uncomma(X, [X]).

% A set is not a sequence, so it goes through the iterator protocol.
% Element order is not preserved, which the spec says explicitly.

py_set_to_pl(Obj, py_set(List)) :-
	'PyObject_GetIter'(Obj, Iter),
	py_check(Iter),
	py_iter_items(Iter, List),
	'Py_DecRef'(Iter).

py_iter_items(Iter, List) :-
	'PyIter_Next'(Iter, Item),
	(   Item =:= 0
	->  List = []                  % exhausted, or an error phase 5 owns
	;   py_to_pl(Item, H),
	    'Py_DecRef'(Item),
	    List = [H|T],
	    py_iter_items(Iter, T)
	).

% Everything the table does not name - bytes, complex, a module, a class,
% an arbitrary instance - becomes an opaque handle. It increfs, so the
% term owns a reference of its own; py_free/1 in phase 4 releases it.

py_opaque(Obj, '$py_obj'(Obj)) :-
	'Py_IncRef'(Obj).


		 /*******************************
		 *      PROLOG -> PYTHON        *
		 *******************************/

%   pl_to_py(+Term, -Obj) is det.
%
%   Returns a new reference; the caller releases it. The clause order
%   matters here too, for the mirror-image reason: every integer is a
%   rational in Trealla, so integer/1 has to be tested before rational/1
%   or every integer leaves as a Fraction.

pl_to_py(Term, _) :-
	var(Term),
	!,
	throw(error(instantiation_error, janus)).
pl_to_py(@(Const), Obj) :-
	!,
	py_const_to_py(Const, Obj).
pl_to_py('$py_obj'(Ptr), Obj) :-
	!,
	Obj = Ptr,
	'Py_IncRef'(Ptr).
pl_to_py(Term, Obj) :-
	integer(Term),
	!,
	py_int_to_py(Term, Obj).
pl_to_py(Term, Obj) :-
	rational(Term),
	!,
	py_rational_to_py(Term, Obj).
pl_to_py(Term, Obj) :-
	float(Term),
	!,
	'PyFloat_FromDouble'(Term, Obj),
	py_check(Obj).
pl_to_py([], Obj) :-
	!,
	'PyList_New'(0, Obj),
	py_check(Obj).
pl_to_py({}, Obj) :-
	!,
	'PyDict_New'(Obj),
	py_check(Obj).
pl_to_py(Term, Obj) :-
	atom(Term),
	!,
	'PyUnicode_FromString'(Term, Obj),
	py_check(Obj).
pl_to_py(Term, Obj) :-
	string(Term),
	!,
	atom_string(A, Term),
	'PyUnicode_FromString'(A, Obj),
	py_check(Obj).
pl_to_py({Inner}, Obj) :-
	!,
	py_dict_to_py(Inner, Obj).
pl_to_py(py_set(List), Obj) :-
	is_list(List),
	!,
	py_set_to_py(List, Obj).
pl_to_py(Term, Obj) :-
	is_list(Term),
	!,
	py_list_to_py(Term, Obj).
pl_to_py(Term, Obj) :-
	compound(Term),
	functor(Term, -, _),
	!,
	py_tuple_to_py(Term, Obj).
pl_to_py(Term, _) :-
	throw(error(type_error(python_term, Term), janus)).

py_const_to_py(none, Obj) :-
	!,
	py_const_(none, Obj),
	'Py_IncRef'(Obj).
py_const_to_py(true, Obj) :-
	!,
	'PyBool_FromLong'(1, Obj),
	py_check(Obj).
py_const_to_py(false, Obj) :-
	!,
	'PyBool_FromLong'(0, Obj),
	py_check(Obj).
py_const_to_py(Other, _) :-
	throw(error(domain_error(py_constant, Other), janus)).

% The mirror of py_int_to_pl: int64 where it fits, base-16 text where it
% does not. A bignum cannot cross the FFI at all - every integer argument
% is guarded by is_smallint - so this is not an optimisation.

% The lower bound is INT64_MIN + 1, not INT64_MIN. Trealla's smallint
% range is asymmetric - is_smallint rejects -9223372036854775808 exactly,
% while -9223372036854775807 and 9223372036854775807 both pass - so the
% one value C would call representable is the one the FFI will not carry.
% It takes the hex path instead, which handles it correctly.

py_int_to_py(N, Obj) :-
	N >= -9223372036854775807,
	N =< 9223372036854775807,
	!,
	'PyLong_FromLongLong'(N, Obj),
	py_check(Obj).
py_int_to_py(N, Obj) :-
	format(atom(Hex), "~16r", [N]),
	'PyLong_FromString'(Hex, 0, 16, Obj),
	py_check(Obj).

% Fraction(Numerator, Denominator), built from two integer OBJECTS and
% never from text: Fraction("<huge>/3") goes through int(str) and hits
% the same decimal digit cap, which is a live limitation in SWI's Janus.

py_rational_to_py(R, Obj) :-
	N is numerator(R),
	D is denominator(R),
	py_int_to_py(N, NO),
	py_int_to_py(D, DO),
	'PyTuple_New'(2, Args),
	py_check(Args),
	'PyTuple_SetItem'(Args, 0, NO, _),        % steals NO
	'PyTuple_SetItem'(Args, 1, DO, _),        % steals DO
	py_type_(fraction, Class),
	'PyObject_CallObject'(Class, Args, Obj),
	'Py_DecRef'(Args),
	py_check(Obj).

py_list_to_py(List, Obj) :-
	length(List, N),
	'PyList_New'(N, Obj),
	py_check(Obj),
	py_fill_seq(List, 0, Obj, list).

py_tuple_to_py(Term, Obj) :-
	Term =.. [-|Items],
	length(Items, N),
	'PyTuple_New'(N, Obj),
	py_check(Obj),
	py_fill_seq(Items, 0, Obj, tuple).

% Both setters STEAL the reference, so nothing is released here.

py_fill_seq([], _, _, _).
py_fill_seq([H|T], I, Obj, Kind) :-
	pl_to_py(H, Item),
	(   Kind == list
	->  'PyList_SetItem'(Obj, I, Item, _)
	;   'PyTuple_SetItem'(Obj, I, Item, _)
	),
	J is I + 1,
	py_fill_seq(T, J, Obj, Kind).

% PyDict_SetItem and PySet_Add do NOT steal, so both sides are released
% after they are stored.

py_dict_to_py(Inner, Obj) :-
	'PyDict_New'(Obj),
	py_check(Obj),
	py_uncomma(Inner, Pairs),
	py_fill_dict(Pairs, Obj).

py_fill_dict([], _).
py_fill_dict([K:V|T], Obj) :-
	!,
	pl_to_py(K, KO),
	pl_to_py(V, VO),
	'PyDict_SetItem'(Obj, KO, VO, _),
	'Py_DecRef'(KO),
	'Py_DecRef'(VO),
	py_fill_dict(T, Obj).
py_fill_dict([Bad|_], _) :-
	throw(error(type_error(py_dict_entry, Bad), janus)).

py_set_to_py(List, Obj) :-
	'PySet_New'(0, Obj),
	py_check(Obj),
	py_fill_set(List, Obj).

py_fill_set([], _).
py_fill_set([H|T], Obj) :-
	pl_to_py(H, Item),
	'PySet_Add'(Obj, Item, _),
	'Py_DecRef'(Item),
	py_fill_set(T, Obj).

%   py_round_trip(+Term, -Back) is det.
%
%   Not part of the interface: the marshaller has no caller until phase
%   2, and this is what phase 1's tests exercise it through.

py_round_trip(Term, Back) :-
	py_init,
	pl_to_py(Term, Obj),
	py_to_pl(Obj, Back),
	'Py_DecRef'(Obj).
