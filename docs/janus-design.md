# Janus Prolog–Python interface for Trealla — design

Status: the library build of §4.3 is done and in the tree. Everything else here
is design only.

Goal: a bidirectional Prolog–Python bridge presenting the *Janus* interface that
SWI-Prolog and XSB have agreed on (established as a PIP, a Prolog Improvement
Proposal). Both directions are in scope: `library(janus)` embedding Python into
Trealla, and a Python package embedding Trealla into CPython.

**Hard constraint: Trealla carries no Python dependency by default.** A stock
`make` must not reference Python in any form — not a header, not a library, not
an embedded `library/janus.pl`. Everything here is reachable only through an
opt-in makefile target. See §5.

**Source basis.** Read: `~/swipl-devel/packages/swipy` (`janus/janus.pl`,
`janus/janus.c`, `janus/janus.py`, `README.md`, `test_xsb_janus.pl`) and
`~/xsb-code/XSB/packages/janus` (`../janus.P`, `janusm.c`,
`XSB/docs/userman/janus-plg.tex` §"Bi-translation", `janus-py.tex`). On the
Trealla side: `src/bif_ffi.c`, `src/trealla.h`, `src/internal.h`, `GNUmakefile`.
Facts marked **[checked]** were verified by running code during this pass, on
Trealla v3.5.11-3 against CPython 3.14.7, macOS/arm64.

---

## 1. The central finding

The two directions need different mechanisms, and keeping them apart is what
makes this tractable.

**Prolog → Python needs no C.** Every CPython C-API entry point Janus requires is
exported by `libpython` and is non-variadic, so Trealla's existing FFI reaches
all of it. **[checked]** — this ran before any of this document was written:

```
  import math      -> ptr 4387867760
  math.factorial(20) = 2432902008176640000
  str round-trip   -> 'round-trip'
  iterating range(4): 0 1 2 3
  [python] hello from CPython
```

That is `PyImport_ImportModule` → `PyObject_GetAttrString` → `PyTuple_New` /
`PyTuple_SetItem` → `PyObject_CallObject` → `PyLong_AsLongLong`, plus
`PyUnicode_FromString`/`AsUTF8` and the `PyObject_GetIter`/`PyIter_Next`
protocol. That is the whole spine of `py_func/3`, `py_dot/3` and `py_iter/2`.

A second run confirmed `PyGILState_Ensure`/`PyGILState_Release` and error
detection via `PyErr_Occurred`, and that `do_dlopen` copes with a macOS framework
path carrying no `.so` extension. **[checked]**

**Python → Prolog needs C.** The FFI cannot build a closure that calls back into
Prolog, so this direction cannot ride on it. It needs a small extension module,
as SWI ships `mod_swipl.c`.

---

## 2. The interface to target

SWI and XSB each extend the agreed core. The intersection of their exports
defines compatibility. Computed from `janus.pl`'s module declaration and
`janus.P`'s `:- export` lines: **[checked]**

| Group | Predicates |
|---|---|
| **Common core** (18) | `py_call/2,3`, `py_func/3,4`, `py_dot/3,4`, `py_iter/2,3`, `py_setattr/3`, `py_free/1`, `py_is_object/1`, `py_add_lib_dir/1,2`, `py_lib_dirs/1`, `keys/2`, `key/2`, `values/3`, `items/2` |
| XSB spellings (6) | `add_py_lib_dir/1`, `obj_dir/2`, `obj_dict/2`, `value/3`, `janus_python_version/1`, `py_next/2` |
| SWI extras (20) | `py_call/1`, `py_version/0`, `py_shell/0`, `py_pp/1,2,3`, `py_type/2`, `py_isinstance/2`, `py_import/2`, `py_module/2`, `py_module_exists/1`, `py_hasattr/2`, `py_with_gil/1`, `py_gil_owner/1`, `py_object_dir/2`, `py_object_dict/2`, `py_obj_dir/2`, `py_obj_dict/2`, `py_is_dict/1`, `py_initialize/3` |

Scope: the common core, plus the XSB spellings as thin wrappers, plus
`py_version/0`, `py_type/2` and `py_pp/1` — those three cost almost nothing and
are what makes the thing usable at a toplevel.

Out of scope: `py_with_gil/1` and `py_gil_owner/1` expose SWI's thread
synchronisation model; `py_is_dict/1` and SWI's dict support depend on SWI's
native dict type, which Trealla does not have. Both are SWI extensions, not part
of the agreed interface.

---

## 3. Bi-translation

Fully specified by the agreed interface (`janus-plg.tex` §"Bi-translation"), so
this is implementation, not design. Translation is recursive on both sides.

| Python | Prolog | Note |
|---|---|---|
| `int` | integer | Trealla is unbounded, so no clamping — XSB has to range-check here, we do not |
| `float` | float | |
| `str` | atom | UTF-8 both sides |
| `True` / `False` | `@true` / `@false` | needs `op(200, fy, @)` |
| `None` | `@none` | |
| `list` | list | syntactically identical |
| `tuple` of arity N | `-/N` compound | functor is a hyphen |
| `dict` | `{K:V, K:V}` | curly term wrapping a `:/2` comma-list |
| `set` | `py_set(List)` | element order not preserved |
| anything else | opaque object reference | representation is explicitly system-dependent |
| `bytes`, `complex` | — | not in the spec; leave unmapped |

Every term form needed already exists in Trealla. **[checked]**

```prolog
?- X = {a:1,b:2}, X =.. L.
   L = [{},(a:1,b:2)]
?- T = -(1,2,3), T =.. TL.
   TL = [-,1,2,3]
```

So the dictionary form comes for free from the curly-brace comma-list Trealla
already parses for DCGs, and tuples are ordinary compounds.

**Object references.** The spec states the representation is system-dependent
(XSB uses `pyObj/1`, SWI a blob with a GC finalizer). Trealla has no blob type
with finalizers, so: `'$py_obj'(Ptr)` holding the raw `PyObject*`. This is
sound because *both* reference systems already export `py_free/1` — explicit
release is part of the agreed interface, not a workaround for Trealla.

---

## 4. Architecture

### 4.1 Prolog → Python: `library(janus)`, pure Prolog over the FFI

Everything above the marshalling layer is ordinary Prolog. Consequences:

- `py_iter/2` becomes a recursive Prolog generator over `PyIter_Next`, giving
  real backtracking with no non-deterministic foreign predicate required.
- `keys/2`, `key/2`, `values/3`, `items/2` are pure term manipulation on the
  curly form — no Python involvement at all.
- If marshalling later proves hot, it can move into C without changing the
  interface.

The FFI signatures needed are all plain pointer and integer types. No struct
passes by value anywhere in the CPython API we touch, so none of the
`foreign_struct` machinery is involved.

### 4.2 Python → Prolog: an extension module

Trealla already has the query engine this needs — `src/trealla.h` exposes
`pl_query`, `pl_redo`, `pl_done`, which is exactly the backtracking that
`janus.query()` presents to Python as an iterator. **[checked]**

**A library build — done.** `make` now also produces `libtrealla.a`; see §4.3.
This was taken as a precursor rather than left to phase 6.

Three things are still missing, all on the critical path for this direction:

**Structured answer extraction.** `trealla.h` has no term inspection at all —
answers reach the caller by being *printed* (`dump_vars`), not returned.
**[checked]** This is the one genuine piece of new API design in the project.

**`pl_query` frees the goal's strings before the query finishes.** It calls
`parser_destroy` before returning, so a string literal in the goal is freed while
the running query still refers to it. The first solution is fine; later ones read
freed memory. **[checked]** — `member(X,[a,b,c]), format("~w",[X])` yields 2
solutions and a bogus `type_error(atom, mmy)` instead of 3, and suppressing the
`parser_destroy` call makes it 3. Janus goals routinely carry strings, so this
blocks phase 6 outright.

**`get_status` is not meaningful after `pl_query`.** It reads false even for a
goal that just succeeded, so a goal with no solutions cannot be told from one
with a single solution. **[checked]** — `pl_eval` sets it correctly, only the
`pl_query` path does not. `janus.query_once()` has to report truth, so this needs
fixing too.

A sketch of the minimum surface, following the existing header's style:

```c
// Inspecting a term
int         pl_term_type(pl_term*);        // var/integer/float/atom/string/compound
const char *pl_atom_text(pl_term*);
bool        pl_get_int64(pl_term*, int64_t*);
bool        pl_get_float(pl_term*, double*);
const char *pl_functor(pl_term*);
unsigned    pl_arity(pl_term*);
pl_term    *pl_arg(pl_term*, unsigned n);

// Reaching the bindings of the current answer
bool        pl_binding(pl_sub_query*, const char *var_name, pl_term**);
bool        pl_bindings(pl_sub_query*, unsigned n, const char **name, pl_term**);
```

Plus a matching construction side so Python values can be passed *in* as
arguments rather than formatted into the goal string. Big integers need a text
accessor, since Trealla's are unbounded and will not always fit an `int64_t`.

Note this API is not Janus-specific — it is what any embedder has wanted, and it
would serve the WASM and Go hosts too.

### 4.3 The library build (done)

`make` produces `libtrealla.a` alongside `tpl`, from every object except `tpl.o`,
which carries `main()`. `-fPIC` is on by default so the archive can be linked
into a shared object — a Python extension module is one — with `NOPIC=1` opting
out for WASI, Windows and cosmocc. `make install` also installs the archive and
`src/trealla.h`.

Two things had to be fixed to make the archive stand on its own, both found by
linking a C program against it rather than by reading:

- `g_sigfn` and `g_envp` were *declared* inside the engine (`src/prolog.h`,
  `src/bif_os.c`) but *defined* in `tpl.c`, so the engine could not link without
  the front end. Moved to `src/prolog.c` and `src/bif_os.c`. **[checked]**
- `trealla.h` declared `typedef struct {} pl_sub_query;`. An empty struct is a
  GNU extension, not valid ISO C, and this header is now installed for embedders
  to include — MSVC would reject it. Now an opaque incomplete type. **[checked]**

`samples/embed.c` is the demo and smoke test: it consults a file, runs
deterministic and non-deterministic goals, runs two engines side by side, and
reports the two gaps above. It is built by `make` and run by `make misc` via
`tests/misc/embed.sh`, so a `libtrealla.a` that stops standing on its own fails
the suite rather than going unnoticed. It links exactly the way an embedder
would, which is what gives it that property.

Both gap lines are part of the compared output. If either defect is fixed the
line changes to "looks fixed" and the test fails — deliberately, so the fix
arrives with an updated `embed.expected`.

**The measured API contract**, since none of this is written down and two of the
three are not what the names suggest:

| Call | Returns | Success/failure |
|---|---|---|
| `pl_eval` | `!error` | `get_status()` — correct |
| `pl_query` | `!error` | `get_status()` — **not meaningful**, count with `pl_redo` |
| `pl_redo` | another solution exists | destroys the query itself on false |
| `pl_done` | released | only on a query `pl_redo` has not exhausted |

---

## 5. Keeping Python out of the default build

The constraint is satisfiable cheaply, because nothing about the Prolog → Python
half needs Python *at build time*: `library/janus.pl` is pure Prolog and the FFI
resolves `libpython` by `dlopen` when `use_module(library(janus))` runs. So the
only thing to suppress by default is embedding the library itself.

The existing `USE_MAIN` handling in `src/library.c` is the precedent — an extern
pair and a table entry, both behind `#ifdef`, driven by a `-D` from the
makefile. **[checked]** Janus follows it exactly:

```make
# GNUmakefile — off unless asked for
ifdef JANUS
CFLAGS     += -DUSE_JANUS=1
LIBOBJECTS += library/janus.o
endif

janus:
	$(MAKE) JANUS=1
```

```c
/* src/library.c */
#ifdef USE_JANUS
extern unsigned char library_janus_pl[];
extern unsigned int library_janus_pl_len;
#endif
...
#ifdef USE_JANUS
	 {"janus", library_janus_pl, &library_janus_pl_len},
#endif
```

Note the library list is already inside `ifeq ($(EMBED), 1)`, so the `ifdef
JANUS` nests within it. **[checked]**

Consequences worth stating:

- A default build has no `janus` module at all; `use_module(library(janus))`
  fails with an existence error, which is the honest outcome.
- `tests/run.sh` must stay green on a default build, so no test may reference
  Janus. Its tests belong behind the same target.
- Phase 6 — the extension module and `libtrealla` — is a separate target again,
  and the only part that needs Python *headers*. It should not be reachable from
  `make janus` either, since that target is for the Prolog → Python half.
- Adding a `-DUSE_JANUS` object to `LIBOBJECTS` changes the link line, so
  `make janus` and `make` produce different binaries. That is intended, but it
  means the two should not share an object directory without a clean.

---

## 6. What Trealla has and lacks

| Need | State | Work |
|---|---|---|
| CPython C-API reachable from Prolog | present **[checked]** | none |
| Dict / tuple / `@` term forms | present **[checked]** | none |
| Unbounded integers for Python ints | present **[checked]** | none |
| Backtracking query engine | present **[checked]** | `pl_query`/`pl_redo`/`pl_done` |
| GIL calls reachable | present **[checked]** | none |
| Build Trealla as a library | **done** **[checked]** | `libtrealla.a`, part of `make`, §4.3 |
| Structured term inspection | missing **[checked]** | new `trealla.h` API, §4.2 |
| `pl_query` keeps goal strings alive | broken **[checked]** | §4.2 — blocks phase 6 |
| Success/failure after `pl_query` | broken **[checked]** | §4.2 — blocks `query_once` |
| Opaque handle with finalizer | missing | use `'$py_obj'/1` + explicit `py_free/1` |
| C callbacks from the FFI | absent by design | why §4.2 needs C |

---

## 7. Phasing

Ordered so each phase leaves something usable. Phases 1–5 have no dependency on
phase 6, which is where all the C lives — so the Prolog → Python half can ship
on its own.

**Phase 0 — build wiring and finding libpython.**
The `JANUS` makefile target and `#ifdef USE_JANUS` guards of §5, so the feature
is opt-in from the first commit rather than retrofitted. Then locate the
interpreter at run time across platforms: Homebrew framework (no `.so` suffix),
Debian `libpython3.x.so`, Windows DLL. That search belongs in Prolog, not
configure — configure-time detection would put a Python dependency back into the
build. Ship `py_version/0` as the smoke test.
*No dependencies.*

**Phase 1 — marshalling.**
The §3 table, both directions, recursive. The bulk of the Prolog, and everything
later sits on it. Testable standalone by round-tripping nested structures before
any API exists. Fix the reference-ownership rule here and write it down.
*Largest single piece.*

**Phase 2 — calling.**
`py_func/3,4`, `py_dot/3,4`, `py_call/2,3`, `py_setattr/3`. Includes parsing
keyword arguments out of the goal term (`f(a, kw=v)`, positional before keyword)
and following `:` chains.
*Needs 1.*

**Phase 3 — iteration and dict access.**
`py_iter/2,3` as a Prolog generator over `PyIter_Next`; then `keys/2`, `key/2`,
`values/3`, `items/2`, which are term manipulation only.
*Needs 1.*

**Phase 4 — lifetime and the GIL.**
Reference counting across the boundary, `py_free/1`, `py_is_object/1`. Wrap every
entry point in `PyGILState_Ensure`/`Release` so Trealla threads cannot race the
interpreter.
*Needs 2.*

**Phase 5 — errors.**
Python exceptions become Prolog exceptions; instantiation, type and domain faults
stay Prolog errors raised before the call. Both systems agree on this split, so
it is specified rather than invented.
*Needs 2.*

**Phase 6 — Python → Prolog.**
The `libtrealla` target is already done (§4.3). What remains: fix the two
`pl_query` defects of §4.2, add the term-inspection API, then the extension
module exposing `query`, `query_once`, `consult`, `apply`. Independent of
everything above.
*All the C lives here.*

**Phase 7 — conformance.**
Run SWI's `test_xsb_janus.pl`, which is precisely the compatibility suite for the
common core, and XSB's `xsbtests/janus_tests`. Both are already on this machine.
*The real acceptance test.*

---

## 8. Traps

**Borrowed pointers must never be typed `cstr`.** `PyUnicode_AsUTF8` returns a
pointer *into* the Python object. Trealla's `cstr` return type calls `TPL_free`
on whatever it gets, so typing it `cstr` hands CPython's own buffer to Trealla's
allocator. Use `ccstr` for every borrowed string. This is the same hazard the
raylib bindings had to route around, and it is silent until it corrupts the heap.

**Reference counting has no safety net.** There is no finalizer to hang
`Py_DecRef` on, so every object crossing the boundary is manual. `py_free/1`
makes that legitimate, but a long-running program that forgets it leaks. Decide
the ownership rule in phase 1, not later.

**Two runtimes, two stdio buffers.** In the feasibility run, CPython's `print`
output landed *after* Trealla's, because each buffers independently.
**[checked]** Anything interleaving output from both sides needs an explicit
flush discipline; `py_shell/0` would need it badly.

**The GIL versus Trealla threads.** Trealla builds with `USE_THREADS=1`. A thread
entering Python without holding the GIL is a crash, not a race that will be
forgiven. Cheap in phase 4, expensive to retrofit.

**Python version skew.** The C-API is stable across 3.x for what we use, but the
*library name and location* are not. Phase 0 owns this; do not let it leak into
the marshalling code.
