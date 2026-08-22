# Janus Prolog–Python interface for Trealla — design

Status: the library build of §4.3 is done and in the tree. **Phases 0 and 1 are
done**, and **phase 2 with them**, on branch `janus` — `library/janus.pl`, the
`make janus` wiring of §5, the `'$register_predicate'/4` fix of §4.4, and
`tests/janus/` behind `make janus-test`. Phase 1 is the marshaller of §3; phase 2
is `py_call/2,3`, `py_func/3,4`, `py_dot/3,4`, `py_setattr/3`, keyword arguments,
the `Options` argument, and the GIL; **phase 3** is `py_iter/2,3`, the dict
accessors, and `sys.path`; **phase 4** is reference counting, `py_free/1` and
`py_is_object/1`. Phases 5 onwards are still design only.

§2a covers the one place the interface could not be spelled here, and how that
was resolved: the `empty_args` flag.

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
Trealla side: `src/bif_ffi.c`, `src/trealla.h`, `src/internal.h`,
`library/builtins.pl` (for `halt/0,1` and `atexit/0`), `GNUmakefile`.
Facts marked **[checked]** were verified by running code during this pass, on
Trealla v3.5.11-3 against CPython 3.14.7, macOS/arm64. Both reference systems
were built so their behaviour could be measured rather than inferred: SWI-Prolog
10.1.13 with the `swipy` package, in `~/swipl-devel/build-janus` (a separate
build directory — the existing `build/` has `SWIPL_PACKAGES=OFF` and was left
alone), and XSB 5.0.0 in `~/xsb-code/XSB`. Facts still marked **[read]** come
from reading source that was not exercised — mostly claims about *how* an
implementation is written rather than what it does. Three claims in an earlier
draft of this document were inferences that turned out to be wrong when run; they
are corrected below rather than quietly deleted, because the same reasoning would
otherwise be repeated.

---

## 1. The central finding

The two directions need different mechanisms, and keeping them apart is what
makes this tractable.

**Prolog → Python needs no C.** Almost every CPython C-API entry point Janus
requires is exported by `libpython` and is non-variadic, so Trealla's existing
FFI reaches it. The exception is type checking, which is macros over exported
*data* — §3 says what to do instead, and it needs no C either. **[checked]** — this ran before any of this document was written:

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
protocol. That is the spine of `py_func/3`, `py_dot/3` and `py_iter/2`.

Two things that run did not touch, both still non-variadic and so still
reachable the same way: keyword arguments need `PyObject_Call` against a dict
built with `PyDict_New`/`PyDict_SetItemString`, not `PyObject_CallObject`; and
integers outside `int64` need the text path of §3, because the FFI will not
carry them.

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
`janus.P`'s `:- export` lines: **[read]**

| Group | Predicates |
|---|---|
| **Common core** (18) | `py_call/2,3`, `py_func/3,4`, `py_dot/3,4`, `py_iter/2,3`, `py_setattr/3`, `py_free/1`, `py_is_object/1`, `py_add_lib_dir/1,2`, `py_lib_dirs/1`, `keys/2`, `key/2`, `values/3`, `items/2` |
| XSB spellings (6) | `add_py_lib_dir/1`, `obj_dir/2`, `obj_dict/2`, `value/3`, `janus_python_version/1`, `py_next/2` |
| SWI extras (20) | `py_call/1`, `py_version/0`, `py_shell/0`, `py_pp/1,2,3`, `py_type/2`, `py_isinstance/2`, `py_import/2`, `py_module/2`, `py_module_exists/1`, `py_hasattr/2`, `py_with_gil/1`, `py_gil_owner/1`, `py_object_dir/2`, `py_object_dict/2`, `py_obj_dir/2`, `py_obj_dict/2`, `py_is_dict/1`, `py_initialize/3` |

Scope: the common core, plus the XSB spellings as thin wrappers, plus
`py_version/0`, `py_type/2` and `py_pp/1` — those three cost almost nothing and
are what makes the thing usable at a toplevel. Plus one bi-translation
extension SWI also carries, `fractions.Fraction` ↔ rational, which §3 shows is a
dozen lines given that Trealla has rationals already.

Out of scope: `py_with_gil/1` and `py_gil_owner/1` expose SWI's thread
synchronisation model; `py_is_dict/1` and SWI's dict support depend on SWI's
native dict type, which Trealla does not have. Both are SWI extensions, not part
of the agreed interface.

**The `Options` argument is part of the core, not decoration.** Four of the
eighteen take one — `py_call/3`, `py_func/4`, `py_dot/4`, `py_iter/3` — and one
option is implemented by both systems, which puts it inside the agreed
intersection rather than in either extension set: **[read]**

| Option | Where | Verdict |
|---|---|---|
| `py_object(true)` | SWI `janus.pl`, XSB `janus_opts_1/3` | **implement** — narrows what gets translated, see below |
| `sizecheck(Bool)` | XSB only | accept and ignore; it guards XSB's saturating integers (§3), which Trealla does not have |
| `iter(Bool)` | XSB only, commented "keeping around for test suite" | accept and ignore |
| `py_string_as/1`, `py_dict_as/1` | SWI only, documented there as extensions to the PIP | out of scope |

**`py_object(true)` does not mean "give me a handle instead of a value",** which
is what this document assumed until SWI was built and asked. It leaves the exact
scalar types translating and turns everything else into a handle: **[checked]**

| call | default | with `py_object(true)` |
|---|---|---|
| `int(42)` | `42` | `42` — unchanged |
| `tuple([1,2])` | `1-2` | `1-2` — unchanged |
| `list([1,2])` | `[1,2]` | `<py_list>(0x…)` |
| `dict()` | `py{}` | `<py_dict>(0x…)` |

That matches the source exactly — the option swaps SWI's subclass-aware
`PyLong_Check` chain for a short `PyLong_CheckExact` one, so int, float, str and
tuple still translate and containers no longer do. It is a *narrowing of
translation*, not a request for a reference, and §3's ownership rule has to say
that rather than the simpler thing.

**Unknown options are silently accepted by SWI**, which is also not what this
document assumed. Both `sizecheck(true)` and an invented `no_such_option(x)` are
taken without complaint. **[checked]** XSB's `janus_opts_1/3` ends in a
`domain_error` clause instead **[read]**, so the two disagree and the PIP does
not settle it. Following XSB here is the better behaviour and costs nothing —
but it means a goal that is quietly ignored on SWI raises here, which is worth a
line in the module documentation.

---

## 2a. The one thing that cannot be spelled here

Every zero-argument method call in the Janus interface is written `f()` —
`py_dot(Stream, close(), Return)` is from XSB's own manual, and `.upper()`,
`.keys()`, `.sort()`, `.copy()` are the shape of ordinary Python. **Both
reference systems parse it and Trealla cannot.** **[checked]**

Both agree on what matters and disagree on almost everything else, which is
itself useful: **[checked]**

| probe | SWI 10.1.13 | XSB 5.0.0 | Trealla 3.5.25 |
|---|---|---|---|
| `X = close()` | parses | parses | **syntax error** |
| `compound(close())` | true | true | — |
| `close() == close` | **false** | **false** | — |
| `functor(close(), N, A)` | raises `domain_error(compound_non_zero_arity, close())` | `close/0` | — |
| `close() =.. L` | raises | `[close]` — lossy, the atom gives the same | — |
| `functor(Y, close, 0)` | — | the **atom** `close` | the **atom** `close` |

Nor is this only the reader. Trealla has no zero-arity compound to read *into*:
`functor(X, close, 0)` yields the atom. So supporting the spec's spelling is a
change to the term representation, not to the parser.

The disagreement in the lower half of that table is the encouraging part. Both
systems treat `f()` as a genuine term distinct from the atom, but the feature is
*shallow* in both: SWI refuses to decompose it at all, XSB decomposes it
inconsistently — `close() =.. [close]` while `Y =.. [close]` gives back the atom
— and **neither can construct one programmatically.** Nothing in the Janus
interface needs to: a zero-argument call is written by a human and read once.

A bare atom cannot stand in, because `Obj:name` has to keep meaning attribute
access — that is exactly what `math:pi` and `sys:maxsize` are, and both are in
the tests. The distinction `()` encodes is real.

**Resolved without an ISO violation: the `empty_args` flag.** A zero-arity
compound *is* the non-conformance, so Trealla does not gain one. Instead the
reader gains a rule, off by default, that turns the spec's syntax into an
ordinary ISO term: **[checked]**

```prolog
:- set_prolog_flag(empty_args, true).

?- X = close(), write_canonical(X).
'()'(close)
```

`'()'(close)` is a plain compound of arity 1. It satisfies everything the
interface needs — it parses, it is a compound, and it is distinct from the atom
`close` — while `functor/3`, `=..` and the writer keep working on it exactly as
they do on any other term, with no new shape to teach them. The functor is not a
possible Python identifier, so nothing can collide with it. Written by hand,
`'()'(sort)` and `sort()` are the same term, so both spellings work.

With the flag off, which is the default in every build, `close()` is the syntax
error it has always been. Bare `()` stays an error either way, and `[]` and `{}`
are untouched. The full suite is unchanged: 342 pass, with the same three
failures a `main` build has. **[checked]**

Three notes on where it lives:

- The flag is module-scoped like `double_quotes`, so a file that wants the syntax
  says so itself. `library(janus)` deliberately does not turn it on globally.
  Phase 7's conformance suites need one directive at the top rather than a
  rewrite of every `f()` in them.
- It is registered in **two** places, which is easy to miss: the runtime
  `set_prolog_flag/2` in `src/bif_predicates.c`, and the parser's own directive
  handler in `src/parser.c`, which recognises a fixed list of flags at read time
  and silently ignores anything else. Adding it to only the first leaves the
  directive with no effect and no diagnostic.
- The flag bit is **not** behind `#ifdef USE_JANUS`. Only `src/library.o` is
  compiled with that define (§5), so an `#ifdef` inside `prolog_flags_` would
  give that one object a different struct layout from the rest of the engine.
  The few lines of parser support are unconditional too; with the flag off they
  do nothing. Hard-gating them would mean compiling `src/parser.o` twice as
  well, which doubles the fragile surface of §5's build for no behavioural gain.

The writer completes the round trip under the same flag: `'()'(close)` prints as
`close()`, so a term read that way writes back as itself. **[checked]** All four
of `write/1`, `print/1`, `writeq/1` and `write_canonical/1` agree, quoted
functors survive (`'hello world'()` reads back), and the special case is narrow —
it applies only when the single argument is an atom, so `'()'(1)` still prints as
`'()'(1)` rather than the unreadable `1()`. With the flag off, `'()'(close)`
prints as itself, which is the only form that reads back there.

`tests/misc/empty_args.pl` covers the flag on its own account, since none of this
is Janus-specific: the default, the syntax error without it, the term identity
with it, the round trip through every write form, and the things that must not
change — ordinary compounds, atoms, lists, curly terms, and bare `()`.

---

## 3. Bi-translation

Fully specified by the agreed interface (`janus-plg.tex` §"Bi-translation"), so
this is implementation, not design. Translation is recursive on both sides.

| Python | Prolog | Note |
|---|---|---|
| `int` | integer | both languages' integers are unbounded; the FFI that carries them is not — see below |
| `float` | float | |
| `str` | atom | UTF-8 both sides |
| `True` / `False` | `@true` / `@false` | no `op/3` needed — see below |
| `None` | `@none` | |
| `list` | list | syntactically identical |
| `tuple` of arity N | `-/N` compound | functor is a hyphen |
| `dict`, non-empty | `{K:V, K:V}` | curly term wrapping a `:/2` comma-list |
| `dict`, empty | `{}` — an **atom** | not a compound; special case both ways |
| `set` | `py_set(List)` | element order not preserved |
| `fractions.Fraction` | rational (`1 rdiv 3`) | **not in the spec** — an SWI-compatible extension, see below |
| anything else | opaque object reference | representation is explicitly system-dependent |
| `bytes`, `complex` | `'$py_obj'/1` | not in the spec; they fall through to the opaque case |

Every term form needed already exists in Trealla, and none of them needs an
operator declaration. **[checked]**

```prolog
?- X = {a:1,b:2}, X =.. L.
   L = [{},(a:1,b:2)]
?- X = {a:1}, X =.. L.
   L = [{},a:1]
?- X = {}, compound(X).
   false.                  % the empty dict is an ATOM
?- T = -(1,2,3), T =.. TL.
   TL = [-,1,2,3]
?- T = -(1), compound(T).
   true.                   % so a 1-tuple is expressible
```

So the dictionary form comes for free from the curly-brace comma-list Trealla
already parses for DCGs, and tuples are ordinary compounds — including the
1-tuple, which is a compound `-(1)` and not the integer `-1`.

Two cases the table above is easy to read past. The **empty dict** is the atom
`{}`, not a compound, so marshalling needs an explicit branch in both directions
and a dict test cannot simply check for `{}/1`. And `@` **needs no `op/3`
declaration**: Trealla already defines it as `100 fy`, so `@true`, `@false` and
`@none` parse as they stand. SWI declares it at 200; re-declaring it here would
change an existing operator to no purpose. SWI's module also exports
`op(50, fx, #)`, which Trealla has no equivalent for **[checked]** — but `#Value`
appears nowhere in the common core, so it leaves scope with the rest of the SWI
extensions.

**The table is a dispatch order, not a mapping.** Read as a set of independent
rules it gets `True` wrong on the first try, because in Python `bool` is a
subclass of `int`: **[checked]**

```
isinstance(True, int)  = true      <- bool IS an int
isinstance(True, bool) = true
isinstance(1, bool)    = false
```

An int-first marshaller therefore translates `True` to `1` and never reaches the
`@true` row. SWI gets this right, which is the standard to match:
`py_call(builtins:bool(1), X)` gives `@true` while `py_call(builtins:int(1), X)`
gives `1`. **[checked]** SWI guards this by testing `Py_None` by identity and `PyBool_Check`
*before* opening its dispatch chain at all (`py_unify()`, `janus.c`). The rest of
the order is load-bearing for the same reason — `str`, `tuple` and `list` all
answer to `PySequence_Check`. SWI's order, worth copying wholesale:

```
None (identity) -> bool -> int -> float -> str -> tuple -> dict
                -> iterator -> sequence -> set
```

**And the `Check` macros are not reachable from the FFI.** This is the one place
§1's "exported and non-variadic" does not hold. `PyLong_Check`, `PyBool_Check`,
`PyFloat_Check`, `PyUnicode_Check`, `PyDict_Check`, `PyList_Check`,
`PyTuple_Check` and `PySet_Check` are C macros, so there is no symbol to
`dlsym`. Nor do the type objects they test against help directly: `PyLong_Type`
and friends are exported as *data* — `nm` reports `D`, not `T` — and
`'$register_predicate'/4` registers a symbol as something to *call*.
`PyIter_Check` is the lone exception, a real function. **[checked]**

The way round uses exported functions only, because `PyObject_Type` of any
exemplar *is* the type object. Build one of each at load time and cache the
pointers: **[checked]**

```prolog
:- 'PyLong_FromLongLong'(1, I), 'PyObject_Type'(I, T), assertz(ty(int, T)).
```

With those in hand, exact-type dispatch — SWI's `CheckExact`, which is what the
`py_object(true)` path uses — is a pointer comparison against
`PyObject_Type(Obj)`; subclass-aware dispatch, SWI's `Check`, is
`PyObject_IsInstance(Obj, TypeObj)`. Both verified across int, bool, float, str,
tuple, dict and list, `isinstance(1, bool)` correctly false included.
`PyObject_Type` returns a **new** reference, which is one more case for the
ownership rule below.

`py_type/2` then costs nothing extra: `PyType_GetName(PyObject_Type(Obj))`
returns `int`, `bool`, `float`, `str`, `tuple`, `dict`, `list` as atoms.
**[checked]**

**Integers wider than 64 bits do not cross the FFI, in either direction.** This
is the one place this document was flatly wrong, and correcting it adds work to
phase 1. Both languages have unbounded integers; the bridge between them does
not. Every integer argument in `src/bif_ffi.c` is guarded by `is_smallint`, so a
bignum is rejected before the call is even made — here against libc's `llabs`
registered through `'$register_predicate'/4` as `[sint64] -> sint64`, the
narrowest possible probe: **[checked]**

```prolog
?- X is -(2^70), llabs(X, R).
   error(type_error(integer,-1180591620717411303424),llabs/2)
```

Inbound is the same story from the other end — `PyLong_AsLongLong` sets
`OverflowError` and returns -1 for anything that does not fit.

One boundary the implementation found that reading would not: **Trealla's
smallint range is asymmetric.** `is_smallint` rejects exactly
`-9223372036854775808` while accepting `-9223372036854775807` and
`9223372036854775807`, so the one value C would call representable in an
`int64_t` is the one the FFI will not carry. **[checked]** The fast-path guard is
therefore `INT64_MIN + 1`, and `INT64_MIN` itself takes the hex path, which
handles it correctly.

Nor is this reachable only by trying. §4.1's own benchmark calls
`math.factorial(20)` = 2.4e18, which fits `int64` with one factor to spare;
`factorial(21)` = 5.1e19 does not, and neither does an `int` out of `hashlib`,
`secrets`, or any ordinary use of `**`.

**It is addressable, in Prolog, with no C change.** The way through is text, on
the slow path only — **in base 16, and that is a correctness requirement rather
than a preference.** Since 3.11 CPython caps *decimal* conversion of integers at
`sys.get_int_max_str_digits()`, 4300 digits by default, in both directions. Hex
is exempt. **[checked]**

```python
>>> str(2**40000)
ValueError: Exceeds the limit (4300 digits) for integer string conversion
>>> len(hex(2**40000))
10003
```

A decimal exchange therefore works to about 2^14270 and then raises — the kind
of ceiling that surfaces in somebody else's program rather than in our tests.
Round-tripped through the FFI, at four sizes and with every negative
counterpart: **[checked]**

| | 2^70 | 2^4000 | 2^40000 | 2^400000 |
|---|---|---|---|---|
| `str()`, base 10 | ok | ok | **fails** | **fails** |
| `PyNumber_ToBase`, base 16 | ok | ok | ok | ok |

**SWI arrived at the same place**, which is worth knowing before rediscovering
it: `py_unify_long()` in `janus.c` branches on `PyLong_AsLongLongAndOverflow` and
takes a hex path when it overflows, with `#define BASE 16` outbound and a base-10
fallback only for SWI-Prolog older than 9.1.16. Two details we can improve on.
SWI reaches Python's `hex()` through `PyEval_GetBuiltins` +
`PyDict_GetItemString`, cached in a static — `PyNumber_ToBase(obj, 16)` is public
C-API, does it in one non-variadic call, and holds no borrowed reference to a
builtin. And where SWI hands the resulting `0x...` to `PL_put_term_from_chars`,
Trealla's `atom_number/2` parses `'0x...'` and `'-0x...'` as they stand.
**[checked]** So both directions are two lines:

```prolog
out(X, Obj) :- format(atom(A), "~16r", [X]), 'PyLong_FromString'(A, 0, 16, Obj).
in(Obj, X)  :- 'PyNumber_ToBase'(Obj, 16, S),
               'PyUnicode_AsUTF8'(S, T), atom_number(T, X), 'Py_DecRef'(S).
```

Confirmed to be a real Python integer rather than text being shuffled about:
2^40000 sent out, squared with `PyNumber_Multiply`, read back equal to `X*X`
computed in Prolog. **[checked]** Two ownership obligations come with it —
`PyUnicode_AsUTF8` must be `ccstr` (§8), and the string object it borrows from is
a new reference to release.

**Detection is exact, and free inbound.** `PyLong_AsLongLongAndOverflow` reports
overflow through an out-parameter — Trealla's FFI spells that `-sint32` in the
argument list — and, unlike `PyLong_AsLongLong`, raises nothing, so there is no
error state to clear and no dependence on the zero-argument registration fix of
§4.4. It is right at both ends of the range, including the asymmetric one:
**[checked]**

| value | overflow reported |
|---|---|
| 2^63-1 | 0 |
| 2^63 | 1 |
| -(2^63) | 0 |
| -(2^70) | -1 |

Outbound, the same test is a Prolog comparison against those bounds and costs
nothing.

**Keep the fast path anyway.** Text is not cheap enough to use unconditionally.
µs per conversion, 4,000 to 50,000 iterations by size, every figure repeated to
check for drift: **[checked]**

| | int64 fast | 2^64 | 2^1024 | 2^4000 |
|---|---|---|---|---|
| out | 0.31 | 0.75 hex / 0.91 dec | 5.97 / 8.78 | 33.7 / 47.2 |
| in | 0.25 | 1.07 hex / 1.12 dec | 5.82 / 8.42 | 20.2 / 40.9 |

Hex leads decimal by 1.2× to 2× wherever decimal still works at all, and the
`int64` path leads both by 2–4× at the small sizes that dominate ordinary code —
against the 0.7µs §4.1 measures for a complete `math.factorial(20)` call. So the
branch stays, and phase 1 owns both sides of it.

**SWI's integer half works, measured against a built copy:** `2^70`, `2^40000`
and `-(2^400000)` all round-trip through `py_call(builtins:abs(X), Y)` intact.
**[checked]** That is the hex path doing its job, and it is the standard the
implementation here should meet.

§4.2 reaches the same conclusion for the C API — "big integers need a text
accessor" — for the same reason, so the two halves of the project can share the
convention if not the code.

**Rationals — not in the spec because XSB has no rationals.** That is the whole
reason the PIP table has no rational row: the agreed core is an *intersection*,
so it records what both systems could do, not what either thought worth doing.
XSB has no rational type — it has no `rdiv` operator at all, so `X is 1 rdiv 3`
does not even parse there **[checked]** — and its `janus.P` has no `Fraction`
handling. SWI has rationals and duly added the mapping. Trealla is on SWI's side
of that line, so the omission carries no argument against implementing it.

The same intersection artifact explains `sizecheck/1` above, and XSB's integers
turn out to be worth seeing directly: **[checked]**

```prolog
| ?- Y is 2^70.
Y = 9223372036854775807
```

Not an error — a silent saturation at `INT64_MAX`. So `sizecheck/1` guards a
real hazard on XSB, and the spec's numeric model is XSB's numeric model. Both
places where Trealla's arithmetic is richer than the PIP — unbounded integers,
rationals — are places to follow SWI rather than the spec.

Mapping `fractions.Fraction` onto Trealla's rationals is nearly free.
Trealla's are *atomic* — `1 rdiv 3` prints as an operator term but `compound/1`
is false and it does not unify with `A rdiv B` — so decomposition goes through
the evaluable functions `numerator/1` and `denominator/1`, and construction
through `X is N rdiv D`. All three carry bignums. **[checked]**

**Do not port SWI's text trick, though.** SWI converts through the printed form:
outbound it hands `Fraction()` an `"N/D"` string, inbound it takes `str(obj)`,
swaps `/` for `r`, and reads `1r3` back as a term. Neither half survives the trip
here. Trealla does not read `1r3`, and `atom_number/2` rejects `'1 rdiv 3'`
**[checked]**. More seriously, both halves route through the same decimal digit
cap as the integers above. An earlier draft called this an inference and warned
against repeating it as fact; with SWI built, it is now simply a fact.
**[checked]**

```prolog
?- X is (2^40000) rdiv 3, py_call(builtins:abs(X), Y).
ERROR: python_error('ValueError', ...)
```

The same goal with `2^40000` as a plain integer succeeds — so it is specifically
the rational path, which converts through `str`, and not SWI's integer handling,
which converts through hex. The underlying refusal is CPython's:

```python
>>> Fraction(str(2**40000) + '/3')
ValueError: Exceeds the limit (4300 digits) for integer string conversion
>>> str(Fraction(2**40000, 3))
ValueError: Exceeds the limit (4300 digits) for integer string conversion
```

So SWI's Janus translates rationals correctly only up to roughly 2^14270, and
fails with a Python `ValueError` above it. Going through `.numerator` and
`.denominator` as below has no such ceiling — the design here is better than the
system it is modelled on, on this one point, and phase 7 should not treat SWI's
behaviour as the reference for it.

Going through the parts avoids text at the rational level entirely:
`Fraction(NumObj, DenObj)` from two already-marshalled int objects outbound, and
the `.numerator` / `.denominator` attributes inbound. Round-tripped equal over
`1 rdiv 3`, `-2 rdiv 7`, `2^70 rdiv 3` and `2^40000 rdiv 3`. **[checked]** The
`Fraction` class itself is cached at load exactly like §3's type objects, and
`PyObject_IsInstance` against it is the inbound test. Note `PyTuple_SetItem`
*steals* its reference, which is a third reference convention beside new and
borrowed.

**The outbound dispatch has the same subtype trap as `bool`.** `rational(3)` is
true in Trealla **[checked]**, and SWI's `py_from_prolog` tests `PL_is_integer`
inside its rational branch for the same reason **[read]** — every integer is a
rational. So
`integer/1` must be tested before `rational/1`, or every integer leaves as a
`Fraction`. It is the mirror image of `bool` before `int` coming the other way,
and the same mistake.

Normalisation needs no special handling at either end: `Fraction(6, 3)` is
`Fraction(2, 1)`, and `X is 2 rdiv 1` is the integer 2. **[checked]**

**Object references, and who owns them.** The spec states the representation is
system-dependent (XSB uses `pyObj/1`, SWI a blob with a GC finalizer). Trealla
has no blob type with finalizers, so: `'$py_obj'(Ptr)` holding the raw
`PyObject*`.

This is the weakest point in the design and worth being blunt about. A Prolog
term is copied and backtracked freely, so `'$py_obj'(Ptr)` has no unique owner:
two copies of the same term are two references to one `PyObject`, and nothing
tells them apart. Freeing through one leaves the other dangling; freeing through
neither leaks. `py_free/1` being part of the agreed interface makes *explicit*
release legitimate — it does not solve aliasing, and SWI only escapes this
because its blobs are garbage collected.

The rule has to be fixed in phase 1 and written into the module header:

- **Release exactly what we own.** A `PyObject` reaching Prolog as a translated
  term (int, atom, list, dict) does not outlive the call — but whether ending it
  means calling `Py_DecRef` depends on the entry point that produced it, and the
  FFI cannot tell. `PyObject_CallObject`, `PyObject_GetAttrString` and
  `PyIter_Next` return **new** references, which must be released;
  `PyTuple_GetItem`, `PyList_GetItem`, `PyDict_GetItemString` and `PyDict_Next`
  return **borrowed** ones, and releasing those is a double-free. That second
  list is precisely how a recursive marshaller walks a container, so this sits on
  the main path rather than in a corner. Each declared entry point carries its
  new-or-borrowed classification in the shim, next to its type signature.
- An *untranslatable* object becomes a `'$py_obj'/1`, and under
  `py_object(true)` so does everything outside the exact scalar types — see the
  measured table in §2, which is narrower than "whatever the caller asked for".
  That term owns exactly one reference, released by `py_free/1`.
- Freeing a handle twice, or using one after freeing, is a program error the
  same way `free()` twice is. Do not attempt to detect it by scanning the store.

That keeps the count exact for everything the marshaller understands, and
confines manual bookkeeping to handles the user asked for by name. Leaked
handles are still leaked, but they no longer outlive an orderly exit: the
`atexit` hook of phase 0 finalizes the interpreter, so Python's own cleanup
runs even when Prolog forgot a `py_free/1`.

---

## 4. Architecture

### 4.1 Prolog → Python: `library(janus)`, pure Prolog over the FFI

Everything above the marshalling layer is ordinary Prolog. Consequences:

- `py_iter/2` becomes a recursive Prolog generator over `PyIter_Next`, giving
  real backtracking with no non-deterministic foreign predicate required.
- `keys/2`, `key/2`, `values/3`, `items/2` are pure term manipulation on the
  curly form — no Python involvement at all.
- `py_call/2,3` is *derived*, not primitive: XSB implements it in Prolog over
  `py_func`/`py_dot`, walking `:` chains. SWI factors it the other way round —
  there, `py_func/4` and `py_dot/4` are one-line calls to `py_call/3`
  **[read]** — so this is a choice rather than a given. XSB's direction is the
  one to copy, because it leaves the two primitives where the FFI already is.
  One consequence: the dispatcher pattern-matches on the object representation
  (XSB on `pyObj(O)`, us on `'$py_obj'/1`), so the term shape chosen in §3 leaks
  into `py_call`'s clause heads.

The FFI signatures needed are all plain pointer and integer types. No struct
passes by value anywhere in the CPython API we touch, so none of the
`foreign_struct` machinery is involved.

**This is fast enough, measured rather than assumed.** **[checked]**

| | µs per iteration |
|---|---|
| bare Prolog call, as a baseline | 0.17 |
| one FFI call (`PyLong_FromLongLong`) | 0.21 |
| integer round trip, 3 FFI calls | 0.37 |
| `math.factorial(20)`, 7 FFI calls | 0.71 |

So the FFI costs about 35ns over a plain Prolog call, and a complete Python
call with argument marshalling lands near 0.7µs — roughly 1.4M calls a second.
Moving the marshalling into C remains possible without changing the interface,
but nothing in these numbers asks for it.

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

### 4.4 Naming the shared library, and Windows

Not implemented — recorded here so the decision is made before phase 0 starts.

`do_dlopen` now maps `.so` onto `.dylib` and `.dll`, which is enough on macOS
and Linux but only solves a third of the problem on Windows. Three things vary
there, and only the first is a suffix:

| | example |
|---|---|
| suffix | `.so` → `.dll` — **handled** |
| version infix | `libffi-8.dll`, `libssl-3-x64.dll`, `libcurl-4.dll` |
| absent `lib` prefix | `python313.dll` |

The version cannot be derived by any mapping, because it belongs to the
*installation* and not the platform. Globbing `libfoo-*.dll` off `PATH` would
load whatever ABI happened to be present, which is worse than failing honestly.
So two mechanisms, for two different situations.

**Fixed, known names — a candidate list on `use_foreign_module/2`:**

```prolog
:- use_foreign_module(['libcurl.so', 'libcurl-4.dll'], [ ... ]).
```

Tried in order, first that opens wins, and the error names all of them. A bare
atom stays a one-element list, so nothing existing changes. The whole change is
local to `do_use_foreign_module` in `src/module.c`. This puts the ABI version in
the shim that knows which ABI it binds, rather than in a guess somewhere central
— worth stressing, because the versioned names for curl, gsl, sqlite3 and raylib
on MSYS2 are exactly the sort of thing that should not be invented from a
machine that cannot test them.

**Names known only at run time — `'$dlopen'/3` plus `'$register_predicate'/4`.**
This is what libpython needs, since 3.9 through 3.14 are all plausible on one
machine and no fixed list covers that. It already works. **[checked]**

```prolog
:- ( pylib(L), catch('$dlopen'(L, 0, H), _, fail)
   ->  forall(sig(N, A, R), '$register_predicate'(H, N, A, R))
   ;   throw(error(existence_error(foreign_library, libpython), _)) ).
```

Two constraints, both measured:

- It has to be a plain `:- Goal` directive, which runs *during* load, so clauses
  read after it compile against predicates that already exist. `initialization/1`
  is too late — it runs after the file is loaded, and the calling clauses were
  compiled without the predicate, giving `existence_error` at run time.
- `'$register_predicate'/4` rejects `[]` for the argument list, because `[]` is
  an atom rather than an `iso_list`, so a zero-argument function such as
  `Py_Initialize` cannot go through it. `use_foreign_module/2` accepts `[]`
  happily — see the `'CloseWindow'([], void)` entries in `library/raylib.pl`.
  Changing that `GET_NEXT_ARG(p3, iso_list)` to `iso_list_or_nil` is the whole
  fix, and phase 0 needs it.

Neither mechanism should bake guessed DLL names into the shipped shims. The
point of both is to give whoever has a Windows box a place to put the accurate
name.

---

## 5. Keeping Python out of the default build

The constraint is satisfiable cheaply, because nothing about the Prolog → Python
half needs Python *at build time*: `library/janus.pl` is pure Prolog and the FFI
resolves `libpython` by `dlopen` when `use_module(library(janus))` runs. So the
only thing to suppress by default is embedding the library itself.

The existing `USE_MAIN` handling in `src/library.c` is the precedent — an extern
pair and a table entry, both behind `#ifdef`. **[checked]** The C half of Janus
copies it exactly. The makefile half has to copy it more carefully than this
document first proposed, because the obvious spelling fails *silently*.

The obvious spelling is `CFLAGS += -DUSE_JANUS=1` under `ifdef JANUS`. Make does
not track flag changes, so after a plain `make` there is an up-to-date
`src/library.o` compiled *without* the define, and `make janus` will not rebuild
it. The link then succeeds, `library/janus.o` is in it, `g_libs[]` has no `janus`
entry, and `use_module(library(janus))` throws an existence error out of a tree
that just built the target for it. A wrong result, not a build error.

`USE_MAIN` does not have this problem, and the reason is in the recipe rather
than in the `#ifdef`: the `compile:` target deletes `src/library.o`, compiles
that one translation unit with the define, links, and deletes it again, so
neither build can be satisfied by the other's copy. **[checked]** Janus does the
same:

```make
# GNUmakefile — off unless asked for. The define goes on one object and
# never on CFLAGS, so a plain `make` cannot inherit it and `make janus`
# cannot be satisfied by a stale src/library.o.
ifdef JANUS
LIBOBJECTS += library/janus.o
endif

janus:
	$(MAKE) JANUS=1 janus-tpl

janus-tpl: $(OBJECTS)
	rm -f src/library.o
	$(CC) $(CFLAGS) -DUSE_JANUS=1 -o src/library.o -c src/library.c
	$(CC) $(CFLAGS) -o tpl $(OBJECTS) $(OPT) $(LDFLAGS)
	rm -f src/library.o
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
- `make janus` and `make` produce different binaries out of one object
  directory. That is intended, and the recipe above is what makes it safe: the
  only object that differs is `src/library.o`, which exists on disk during
  neither build's resting state.
- Going back is automatic, which was not obvious until it was tried. The recipe
  deletes `src/library.o` *after* linking as well as before, so a following plain
  `make` finds that object missing, rebuilds it without the define, and relinks a
  default `tpl`. **[checked]** No `make clean` needed in either direction: the
  one object that distinguishes the two builds never survives either of them.

---

## 6. What Trealla has and lacks

| Need | State | Work |
|---|---|---|
| CPython C-API reachable from Prolog | present **[checked]** | none |
| Dict / tuple / `@` term forms | present **[checked]** | none |
| Unbounded integers as Prolog terms | present **[checked]** | none |
| Rationals both sides | present **[checked]** | §3 — `numerator/1`, `denominator/1`, `rdiv`; phase 1 |
| Python type dispatch from Prolog | `Check` macros unreachable **[checked]** | §3 — exemplar type objects + `PyObject_IsInstance` |
| Integers past 64 bits across the FFI | **done** **[checked]** | §3 — base-16 slow path, phase 1 |
| Backtracking query engine | present **[checked]** | `pl_query`/`pl_redo`/`pl_done` |
| GIL calls reachable | present **[checked]** | none |
| Shutdown hook for `Py_Finalize` | present **[checked]** | `atexit/0`, asserted; see phase 0 |
| Build Trealla as a library | **done** **[checked]** | `libtrealla.a`, part of `make`, §4.3 |
| Structured term inspection | missing **[checked]** | new `trealla.h` API, §4.2 |
| `pl_query` keeps goal strings alive | broken **[checked]** | §4.2 — blocks phase 6 |
| Success/failure after `pl_query` | broken **[checked]** | §4.2 — blocks `query_once` |
| Runtime library resolution | works **[checked]** | `$dlopen` + `$register_predicate`, §4.4 |
| Registering a zero-arg function | **fixed** **[checked]** | §4.4 — `iso_list_or_nil`, phase 0 |
| Naming a versioned Windows DLL | missing | §4.4 — candidate list on `use_foreign_module/2` |
| Opaque handle with finalizer | missing | `'$py_obj'/1` + explicit `py_free/1`, phase 4 **[checked]** |
| Cleanup for an abandoned iterator | **done** **[checked]** | `setup_call_cleanup/3`, phase 3 |
| `py_object(true)` option | **done** **[checked]** | §2 — exact-type mode, phase 2 |
| Spelling a zero-argument call `f()` | **done** **[checked]** | §2a — `empty_args` flag, reads as `'()'(Name)`; off by default |
| C callbacks from the FFI | absent by design | why §4.2 needs C |

---

## 7. Phasing

Ordered so each phase leaves something usable. Phases 1–5a have no dependency on
phase 6, which is where all the C lives — so the Prolog → Python half can ship
on its own.

**Phase 0 — build wiring, finding libpython, shutdown hook.**
The `JANUS` makefile target and `#ifdef USE_JANUS` guards of §5, so the feature
is opt-in from the first commit rather than retrofitted. Then locate the
interpreter at run time. That search belongs in Prolog, not configure —
configure-time detection would put a Python dependency back into the build.

Three platforms, three shapes:

- macOS/Homebrew: a framework path with no `.so` suffix. `do_dlopen` handles it
  as-is. **[checked]**
- Linux: `libpython3.x.so`, ordinary `dlopen`.
- Windows: `python3X.dll` — no `lib` prefix and the version in the filename, so
  the suffix mapping alone does not reach it. Use the runtime resolution of
  §4.4, which also needs the one-line `iso_list_or_nil` fix described there
  before `Py_Initialize` can be registered. The FFI itself is built on Windows —
  the `WIN` block does not set `NOFFI`, and the CI installs
  `mingw-w64-x86_64-dlfcn`, which resolves `dlopen` through `LoadLibrary`.

Shutdown is now straightforward, which it was not when this was first written.
`halt/0` and `halt/1` run `ignore(atexit)` before `'$halt'`, and `atexit/0` is
dynamic, so a library registers a shutdown action by asserting a clause for it
(`library/builtins.pl`). `library(janus)` should use that to call
`Py_Finalize`, so Python's own `atexit` handlers run and its buffered writes
land.

Three things about the hook, measured rather than assumed: **[checked]**

- The exit status survives it: `halt(3)` with a hook registered still exits 3.
- `ignore/1` takes the *first solution*, so a clause that succeeds ends the
  chain and any other library's registration never runs. **End the clause with
  `fail`** and the next one is tried — verified with two clauses, where the
  failing first ran and then the second did, and `halt(5)` still exited 5. Janus
  must do this; it will not be the only thing registering.
- **Never throw from it.** An exception there aborts the goal before `'$halt'`
  is reached, and the requested status is lost — `halt(2)` with a throwing hook
  exited 0. Wrap the `Py_Finalize` call in `catch/3` and discard anything it
  raises.

So the hook is:

```prolog
:- assertz((atexit :- catch(py_finalize_, _, true), fail)).
```

Ship `py_version/0` as the smoke test.

**Three things this phase found the hard way**, none of them visible from
reading, and all of which fail *silently* rather than loudly: **[checked]**

- **A registered foreign predicate is reachable only from an unqualified call
  inside the module that registered it.** `'$register_predicate'/4` puts it on
  the prolog instance rather than in the module's table, so neither
  `janus:'Py_GetVersion'(V)` from outside nor a bare call from `user` finds it —
  both raise `existence_error`. Every C-API call has to live in
  `library/janus.pl`, which is the design anyway, but it also means tests cannot
  reach the API directly and must go through a Prolog wrapper.
- **`Py_IsInitialized` returns *nonzero*, not 1.** Writing the guard as
  `'Py_IsInitialized'(1)` makes it fail rather than throw, so `py_init`
  initialises twice (harmless) and `py_finalize` quietly does nothing. The first
  symptom is Python's buffered output vanishing at exit, which reads like the
  stdio trap of §8 rather than a bug in the guard. Bind and compare.
- **`assertz/1` inside a module targets that module.** `halt/0,1` call
  `ignore(atexit)` against the global `atexit/0`, so a bare
  `assertz((atexit :- ...))` in `library/janus.pl` creates `janus:atexit/0`,
  which nothing ever calls. It must be `assertz(user:(atexit :- ...))`. Same
  symptom as the previous item, which is what makes the pair confusing: two
  independent bugs presenting as one missing flush.

The acceptance test registers a Python-side `atexit` handler through
`PyRun_SimpleString` and checks its output appears at exit — that is what proves
`Py_FinalizeEx` genuinely ran, rather than merely being called. It also registers
a second Prolog hook, to prove the janus clause's trailing `fail` leaves the
chain intact, and exits 3 to prove the status survives.
*No dependencies.*

**Phase 1 — marshalling.**
The §3 table, both directions, recursive. The bulk of the Prolog, and everything
later sits on it. Fix the reference-ownership rule of §3 here and write it into
the module header, classification of every entry point as new-or-borrowed
included, and the exemplar type-object cache of §3, which every dispatch
decision then reads. Order the dispatch as §3 gives it — `bool` before `int`
inbound, `integer` before `rational` outbound; neither is a detail. The base-16
path of §3 belongs here too, not to a later optimisation pass: without it
`math.factorial(21)` is an error.

Test it here too, not at phase 7. Conformance arrives far too late to be the
first thing that exercises the marshaller, and almost every marshalling bug is
catchable by a round trip: build a term, send it, read it back, compare. Cover
the awkward cases deliberately — empty dict, empty list, 1-tuple, nested
dict-in-list-in-tuple, an atom needing UTF-8, an integer past 64 bits, and
`@true`/`@false`/`@none` — since those are exactly what a conformance suite
written against another system will not think to probe.
**Done.** The shape held: the exemplar type cache, the ordered dispatch, the
base-16 integer path and the `numerator`/`denominator` route for rationals all
worked as designed, and 50,000 round trips of a nested term leak 1 allocated
block, which is noise. **[checked]** Two things the design did not predict, both
found by the tests rather than by reading:

- **The smallint asymmetry above.** The obvious guard, `N >= -(2^63)`, admits one
  value that `PyLong_FromLongLong` then rejects with a `type_error`.
- **A comma-list cannot be taken apart by running its constructor backwards.**
  `py_comma_list([X], X) :- !.` builds `{a:1, b:2}` correctly, but in reverse the
  cut fires on the first clause and yields the single element `[(a:1,b:2)]`, so
  every dict with more than one key was rejected as a malformed entry. It needs a
  separate predicate. Worth naming because the symptom — a `type_error` on a
  perfectly good dict — points at the dict code rather than at the list utility.

One deliberate narrowing: SWI translates an arbitrary *iterator* eagerly, between
`dict` and `sequence` in its chain. Phase 1 does not, so a generator becomes an
opaque handle; `py_iter/2` in phase 3 is what walks it, and XSB gates the eager
behaviour behind `iter/1` anyway.
*Largest single piece.*

**Phase 2 — calling.**
`py_func/3,4`, `py_dot/3,4`, `py_call/2,3`, `py_setattr/3`. Includes parsing
keyword arguments out of the goal term (`f(a, kw=v)`, positional before keyword)
— which is where `PyObject_Call` and a kwargs dict replace `PyObject_CallObject`
— following `:` chains, and the `Options` argument of §2. `py_object(true)` is
the only option with behaviour, and it is the first caller of the handle
machinery phase 4 finishes.

**The GIL belongs here, not in phase 4.** This phase writes the one wrapper every
Python call passes through. `PyGILState_Ensure`/`Release` is two lines inside it
now, and a sweep through finished code later — the retrofit §8 warns about, with
no reason to schedule it deliberately.
**Done.** `py_call/2,3` walks the `:` chain, with `py_func` and `py_dot` over it;
keyword arguments split positional-then-keyword and go through `PyObject_Call`
with a kwargs dict; `py_object(true)` selects the exact-type mode measured in §2.
Confirmed against SWI where behaviour was in doubt: `py_dot(hello, ...)` treats
the atom as a module name and raises there exactly as it does here. **[checked]**

Three things the GIL cost that the design did not anticipate, all of them silent
or misdirected: **[checked]**

- **`Py_InitializeEx` leaves the GIL held by the calling thread, and nothing ever
  drops it.** Every other thread then blocks in `PyGILState_Ensure` forever — not
  a crash, a deadlock, first seen as a test that never returns.
  `PyEval_SaveThread` after startup is what releases it.
- **Trealla ran the global `atexit/0` hook when any *thread* exited, not only at
  process exit.** **[checked]** So the first worker thread to finish called
  `Py_FinalizeEx` and tore CPython down underneath the threads still inside it.
  The symptom was a CPython fatal error — `gilstate_tss_set: failed to set
  current tstate (TSS)`, runtime state `preinitialized` — raised on a thread that
  had done nothing wrong.

  The cause was one token: `bif_thread_create_3` built the thread's goal as
  `(Goal, halt)`, and `halt/0` is `ignore(atexit)` then `'$halt'`. It now appends
  `'$halt'` directly, so a thread finishing no longer runs process-exit hooks —
  which was never specific to Janus: any library registering cleanup that way had
  it run early and repeatedly. `tests/misc/atexit.sh` now covers both the fixed
  case and the one that legitimately remains, an explicit `halt/0` *inside* a
  thread goal.

  `library(janus)` still guards its own hook to the thread that started the
  interpreter, because that last case is real: a user goal may call `halt/0` from
  a thread, and finalizing CPython from there would be just as wrong.
- **`Py_FinalizeEx` pairs with `PyEval_RestoreThread`, not `PyGILState_Ensure`.**
  Taking the GIL through the auto-state machinery leaves a reference held while
  the runtime is torn down, and the process segfaults on the way out *after every
  test has passed* — the worst possible place for it to show up.

A smaller one: `kw=@true` does not parse, because `=@` lexes as a single symbol
atom. It has to be written `kw = @true`.
*Needs 1.*

**Phase 3 — iteration, dict access, library paths.**
`py_iter/2,3` as a Prolog generator over `PyIter_Next`; then `keys/2`, `key/2`,
`values/3`, `items/2`, which are term manipulation only; then
`py_add_lib_dir/1,2` and `py_lib_dirs/1`, which are `sys.path` manipulation and
belong with the first phase that needs to import something the user wrote.

The generator holds an iterator handle across choice points, and a `once/1`, a
cut or a throw abandons it with nothing to catch it — the likeliest leak in
ordinary code, ahead of a forgotten `py_free/1`. Trealla has
`setup_call_cleanup/3` **[checked]**; use it here rather than meeting the leak in
phase 4.

**Done, and it went in as designed** — the only phase so far with nothing to
correct. `py_iter/2,3` is a `repeat` loop over `PyIter_Next` wrapped in
`setup_call_cleanup/3`; the dict accessors are pure term manipulation that never
start the interpreter; `py_add_lib_dir/1,2` and `py_lib_dirs/1` fall out of the
phase 2 chain walker, since `py_call(sys:path:append(Dir), _)` resolves `sys`,
then `path`, then calls `append` on the result.

Two things confirmed rather than assumed: **[checked]**

- Trealla's `setup_call_cleanup/3` runs the cleanup on all five endings that
  matter here — exhaustion, cut, `once/1`, failure and exception — which is what
  makes the iterator handle safe to hold across choice points.
- 9,000 iterations abandoned by `once/1`, by a cut and by a throw move CPython's
  allocated block count by 1. The leak the design warned about is closed rather
  than merely intended to be.

One deviation from SWI, in `values/3`: an unbound key enumerates rather than
failing. SWI's clauses only match an atom key or a list path, so an unbound key
falls through; XSB enumerates, and that is both more useful and the behaviour the
tests here pin.
*Needs 1.*

**Phase 4 — lifetime.**
Reference counting across the boundary, `py_free/1`, `py_is_object/1`, and the
new-or-borrowed rule of §3 applied to every entry point the shim declares. The
GIL has moved to phase 2, where the wrapper it belongs inside gets written.

**Done.** The ownership rule is now checked against CPython's own counts through
`sys.getrefcount`, rather than argued from the code: **[checked]**

| claim | measured |
|---|---|
| marshalling borrows | 2,000 translations of one object, refcount drift **0** |
| a handle owns exactly one reference | 50 handles, refcount **+50** |
| `py_free/1` releases exactly one | after freeing all 50, back to **+0** |

The classification stopped being a comment and became data: `py_sig/4` carries
`new`, `borrowed`, `foreign` or `none` beside each signature, and `py_steals/2`
records the two setters that consume an argument. That is worth doing for its own
sake — the tests can then assert what prose cannot: every `ptr` return is
classified, no pointer return claims `none`, every `ccstr` return is `borrowed`,
nothing that is not a pointer claims a reference, and every `py_steals/2` names a
real entry point. Forty-four entry points, all conforming.

Making it checkable immediately found an imprecision. `PyEval_SaveThread` returns
`ptr`, but a `PyThreadState*` rather than a `PyObject*`, so refcounting does not
apply to it — it had been lumped in with `none`, which also covers "returns no
pointer at all". `foreign` now names that case, which is what keeps "every `ptr`
return is classified" a claim with content.

One place Trealla cannot match SWI, recorded rather than worked around:
`py_is_object/1` is a type test and nothing more. SWI clears its blob in
`py_free/1`, so `py_is_object/1` goes false on a freed handle; a Prolog term
cannot be cleared from under its own copies, so a freed `'$py_obj'/1` still looks
like an object. That is the aliasing weakness §3 called the weakest point in the
design, showing up exactly where it said it would.
*Needs 2.*

**Phase 5 — errors.**
Python exceptions become Prolog exceptions; instantiation, type and domain faults
stay Prolog errors raised before the call. Both systems agree on this split, so
it is specified rather than invented.

This is also where §8's "the C-API is stable across 3.x" is thinnest: 3.12
replaced `PyErr_Fetch`/`PyErr_NormalizeException` with
`PyErr_GetRaisedException`. The older pair is still exported, but this phase
should confirm that against the oldest and newest interpreters it claims to
support instead of assuming it.
*Needs 2.*

**Phase 5a — the compatibility surface.**
The six XSB spellings as thin wrappers (`add_py_lib_dir/1`, `obj_dir/2`,
`obj_dict/2`, `value/3`, `janus_python_version/1`, `py_next/2`), plus the
toplevel conveniences §2 keeps in scope: `py_type/2` and `py_pp/1`
(`py_version/0` already shipped in phase 0). Small, and easy to leave until
phase 7 discovers it — but phase 7 runs XSB's suite, and every one of these is a
name that suite calls.
*Needs 2 and 3.*

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

**Borrowed things must not be released, and there are two kinds.** Strings:
`PyUnicode_AsUTF8` returns a pointer *into* the Python object, and Trealla's
`cstr` return type calls `TPL_free` on whatever it gets, so typing it `cstr`
hands CPython's own buffer to Trealla's allocator. Use `ccstr` for every borrowed
string — the same hazard the raylib bindings had to route around, silent until it
corrupts the heap. Objects: the container accessors return borrowed *references*,
and decref'ing one is the same bug a level up (§3). Both are properties of the
entry point rather than of the value in hand, so both belong in the shim's
declaration table, settled once where the function is declared and never at the
call site.

**A failed CPython call returns NULL, and the FFI will marshal it.** When the
decimal experiment above hit the digit limit, no Python error reached Prolog:
`PyObject_Str` returned NULL, that NULL went on into `PyUnicode_AsUTF8` as a
`ccstr`, and what finally surfaced was `syntax_error(operator_expected)` out of
`atom_number/2` — several steps downstream of the actual fault, and naming the
wrong predicate. **[checked]** Every fallible `PyObject*` has to be tested
against 0 where it is returned, before anything else touches it. Phase 5 owns
turning that into a proper Prolog exception, but the test itself cannot wait for
phase 5: without it, failures surface as nonsense.

**Reference counting has no safety net.** There is no finalizer to hang
`Py_DecRef` on, so every object crossing the boundary is manual. `py_free/1`
makes that legitimate, but a long-running program that forgets it leaks. Decide
the ownership rule in phase 1, not later.

**Two runtimes, two stdio buffers.** In the feasibility run, CPython's `print`
output landed *after* Trealla's, because each buffers independently.
**[checked]** Anything interleaving output from both sides needs an explicit
flush discipline; `py_shell/0` would need it badly.

**The GIL versus Trealla threads.** Trealla builds with `USE_THREADS=1`, its
threads are real pthreads (`src/bif_threads.c` calls `pthread_create`), and the
FFI is reentrant across them — three threads making 15,000 FFI calls
concurrently ran clean. **[checked]** So two threads entering CPython at once is
reachable in ordinary code, not a theoretical hazard, and without the GIL that
is a crash rather than a race that will be forgiven. Cheap in phase 2, where the
call wrapper is written; expensive as a later sweep, which is why phase 4 no
longer owns it.

**Python version skew, in two separate places.** The *library name and location*
vary by platform and installation; phase 0 owns that, and it must not leak into
the marshalling code. The *C-API* is stable across 3.x for most of what we use,
but not all of it, and both exceptions are recent: 3.11 introduced the
`int_max_str_digits` cap that makes decimal integer exchange fail (§3), and 3.12
replaced `PyErr_Fetch` with `PyErr_GetRaisedException` (phase 5). Neither is
visible on the interpreter this document was written against, which is the
reason to state the supported range and test its ends rather than infer
stability from one machine.
