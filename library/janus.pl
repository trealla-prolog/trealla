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
	py_initialized, !.
py_init :-
	'Py_InitializeEx'(0).

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
