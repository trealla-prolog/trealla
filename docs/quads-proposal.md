# Proposal: Quad support in Trealla (issue #1063)

Quads are `?- Query.` terms embedded in source files, followed by answer-description
terms documenting the expected toplevel answers. Markus proposes two stages:
(1) parse and ignore them correctly, (2) interpret them as embedded tests.

## 1. Current state (HEAD e187304)

`directives()` in `parser.c` recognises `?-`/1 and sets `p->is_quad`:

```c
if (!strcmp(C_STR(p, d), "?-")) {
    p->is_quad = true;
    return true;
}
```

The flag is consumed in the `tokenize()` end-of-term block (~line 3847): when set,
the *next* term is discarded before `process_clause()` runs. Testing this build
shows three problems with the current "ignore" behaviour:

**a. The query term is asserted into the database.** `directives()` returns `true`
for `?-`, but `process_term()` ignores the return value and falls through to
`assertz_to_db()`. Unlike `:-` terms (which are renamed to `$directive` first),
the `?-` term is asserted verbatim, creating a static `?-`/1 predicate:

```
$ tpl consulted-quad-file.pl
?- predicate_property('?-'(_), P).
   P = static.
```

**b. Exactly one following term is skipped, unconditionally.** The skip is not
form-based, so two consecutive queries break — the second query is swallowed as
if it were the first one's answer, and its real answer term is then processed
as a clause:

```
?- true.          % sets is_quad
?- foo(X).        % DISCARDED (consumed by is_quad)
   X = bar.       % processed as a clause:
                  % "Error: permission error modifying user:(=)/2"
```

Conversely a query with a *multi-term* trailing annotation, or a stray query
with no answer at all, mis-aligns everything after it.

**c. Answer terms still get singleton warnings** (e.g. `X = bar.` warns on `X`),
which will be noisy across a large quad suite.

## 2. Stage 1 — ignore quads *correctly*

Model it on `conditionals()`: a dedicated function called first from
`process_term()`, with its state on the **module** (like `if_depth` /
`ifs_blocked`), since nested parsers are created per-term by `expand_term()`
and `p` flags don't survive reliably; the parser-level `is_quad` bit and the
skip block at line 3847 go away.

### 2.1 New module state (`internal.h`)

```c
struct module_ {
    ...
    clause *quad_query;        // copy of pending ?- Query (stage 2)
    unsigned quad_line_num;    // where the query appeared
    bool in_quad:1;            // saw ?- Q., now consuming answer terms
    ...
};
```

### 2.2 Answer-description recogniser

A small recursive shape check over cells, straight from the grammar in the
issue:

```c
static bool is_answer_description(module *m, cell *c)
{
    if (is_var(c)) return false;

    if (!c->arity)
        return !CMP_STRING_TO_CSTR2(m, c, "true")
            || !CMP_STRING_TO_CSTR2(m, c, "false")
            || !CMP_STRING_TO_CSTR2(m, c, "...")
            || !CMP_STRING_TO_CSTR2(m, c, "loops")
            || !CMP_STRING_TO_CSTR2(m, c, "instantiation_error")
            || !CMP_STRING_TO_CSTR2(m, c, "ad_infinitum")
            || !CMP_STRING_TO_CSTR2(m, c, "sto")
            || !CMP_STRING_TO_CSTR2(m, c, "unexpected");

    if (c->arity == 2) {
        if (!CMP_STRING_TO_CSTR2(m, c, "=")) return true;
        if (!CMP_STRING_TO_CSTR2(m, c, ",")
         || !CMP_STRING_TO_CSTR2(m, c, ";")
         || !CMP_STRING_TO_CSTR2(m, c, "|"))
            return is_answer_description(m, FIRST_ARG(c))
                && is_answer_description(m, NEXT_ARG(FIRST_ARG(c)));
        if (!CMP_STRING_TO_CSTR2(m, c, "error")
         || !CMP_STRING_TO_CSTR2(m, c, "type_error")
         || !CMP_STRING_TO_CSTR2(m, c, "domain_error"))
            return true;
    }

    if (c->arity == 1)
        return !CMP_STRING_TO_CSTR2(m, c, "throw")
            || !CMP_STRING_TO_CSTR2(m, c, "syntax_error")
            || !CMP_STRING_TO_CSTR2(m, c, "representation_error")
            || !CMP_STRING_TO_CSTR2(m, c, "resource_error")
            || !CMP_STRING_TO_CSTR2(m, c, "uninstantiation_error");

    return false;
}
```

### 2.3 `quads()` hook, mirroring `conditionals()`

```c
static bool quads(parser *p, cell *d)
{
    // A new query, whether or not one was pending
    if (is_interned(d) && (d->arity == 1)
        && !strcmp(C_STR(p, d), "?-")) {
        p->m->in_quad = true;
        p->m->quad_line_num = p->line_num;
        // stage 2: stash a copy of FIRST_ARG(d) + vartab names here
        return true;                       // consume: do NOT assert
    }

    // Terms following a query: consume while they look like answers
    if (p->m->in_quad) {
        if (is_answer_description(p->m, d)) {
            // stage 2: run the check here
            return true;                   // consume
        }
        p->m->in_quad = false;             // ordinary term resumes normal load
    }

    return false;
}

static bool process_term(parser *p, cell *p1)
{
    if (conditionals(p, p1))
        return true;

    if (p->m->ifs_blocked[p->m->if_depth])
        return true;

    if (quads(p, p1))                      // NEW
        return true;
    ...
}
```

Then delete the `?-` branch from `directives()`, the `is_quad` bit from
`struct parser_`, and the skip block at ~3847.

This fixes (a) — nothing asserted; (b) — skipping is form-based, so
consecutive queries, missing answers, and multiple answer terms all work; the
`ignore_vars`/singleton noise (c) can be handled by suppressing the singleton
check when `m->in_quad` was active for the term (small extra flag, optional).

One subtlety: the answer term still passes through `check_body_callable()` and
`term_expansion()` before `process_term()` is reached. `,`/`;`/`=` terms are
callable so the check is harmless, but user `term_expansion/2` could rewrite
an answer term. Acceptable for stage 1; stage 2 could move quad detection
earlier if it ever bites.

## 3. Stage 2 — interpret quads as tests

The parser stays purely syntactic and executes **nothing** at load time
(directives in Trealla are not run as goals, and quads shouldn't be either).
Instead the parser *records* each quad as data, and a library predicate runs
the recorded quads on demand, after the file is fully loaded — ordinary Prolog
at run time, not parse time. This also fixes forward references: a quad may
test a predicate defined further down the file.

### 3.1 Recording

In `quads()`, instead of just consuming the terms, pair the query with its
answer description and assert (via the same path `$directive` terms take):

```prolog
'$quad'(Query, QueryVarNames, AnswerDesc, AnswerVarNames, 'file.pl':Line).
```

The var-name lists (`['X'=V1, ...]`) are built from `p->vartab`
(`vartab.pool` / `vartab.off`, the same data the singleton warning uses).
Because the query and the answer description are *separate terms*, their
variables can only be related **by name** — the two name lists are what ties
`X` in `?- member(X,...)` to `X` in `X = 1 ; ...`.

The C-side cost over stage 1 is only: keep a copy of the pending query clause
in `m->quad_query` (as `expand_term()` already does with `dup_cells`), and on
seeing the answer term, assert the combined `'$quad'/5` fact.

### 3.2 Running: `library/quads.pl`

A new embedded library (added to `library.c` like the others) exporting:

```prolog
quads:run_all/0        % run every recorded '$quad'/5, report, fail count
quads:run/1            % run quads whose query calls the given PI, e.g. member/2
```

`run_all/0` iterates the `'$quad'/5` facts and for each:

1. Unifies same-named variables across the two name lists.
2. Normalises the answer description: split on `'|'` into acceptable
   alternatives; split each on `;` into the expected solution sequence;
   expand error shorthands (`instantiation_error` →
   `error(instantiation_error,_)`, etc.); note trailing `...` /
   `ad_infinitum`, `sto`, `unexpected` annotations.
3. Enumerates solutions of `Query` with a cap (e.g. 64, like Flowlog's
   `quad_default_solution_cap`) using `catch/3` to capture thrown errors as
   `error(...)` outcomes, rendering each solution as its `Name = Value`
   bindings (`false` when no solution, `true` when no visible bindings).
   `loops` is approximated with `call_with_time_limit/2` (already in
   `library/iso_ext.pl`); an inference-limit builtin can replace it later.
4. Compares actual vs expected; on mismatch prints
   `file:line: quad failed: ?- Query.  expected: ...  got: ...`.
5. Finally prints `N quads, P passed, F failed`.

Matching semantics can start strict (`==` on rendered bindings modulo variable
renaming) and grow toward the full Flowlog semantics incrementally — the value
is that all of this lives in Prolog and never touches the parser again.

### 3.3 Invoking

- Interactively, any time after loading (the `'$quad'/5` facts persist):
  `?- quads:run_all.`
- Batch: `tpl --quads file.pl` — consult, then call `quads:run_all/0`
  instead of entering the toplevel, and exit nonzero (via `pl->halt_code`)
  if any quad failed, so `make quad-tpl`-style runners need no wrapper.
- Whether recording is always on or gated behind a flag
  (`:- set_prolog_flag(quads, record).`) is a space/tidiness call; a `'$quad'/5`
  fact per test is cheap, and always-on means the REPL can run any consulted
  file's quads without reloading.

### 3.4 Suggested split into PRs

1. **PR 1 (stage 1):** `quads()` + `is_answer_description()`, remove
   `p->is_quad`, module state, tests for the mis-alignment cases above.
   Small, no behaviour change for non-quad files, unblocks writing quads today.
2. **PR 2:** record `'$quad'/5` facts (query copy + var-name lists).
3. **PR 3:** `library/quads.pl` runner/matcher, `--quads` exit-code, then run
   Flowlog's `tests/ulrich/*_quad.pl` and the Prologue quads as the
   acceptance suite (`make quad-tpl` from the Flowlog repo already exists as
   a cross-check).

## 4. Test cases for stage 1

```prolog
% consecutive queries — second must not be swallowed
?- true.
?- foo(X).
   X = bar.
foo(bar).                 % must load; '?-'/1 must NOT exist

% multi-solution answer, one term
?- member(X,[1,2,3]).
   X = 1
;  X = 2
;  X = 3.

% error description
?- atom_length(A, L).
   error(instantiation_error, _).

% quad followed by directive — directive must run
?- foo(bar).
   true.
:- initialization(main).
```

## 5. Open questions

- Should a pending `in_quad` at `end_of_file` warn (query with no answer
  description)? Probably yes, once checking is on.
- `X = 1, Y = 2` answers use `,`/2 which is also a valid clause body — the
  form-based skip therefore only treats it as an answer when directly
  following a query, which the design above already does; a bare `true.`
  fact after a quad would also be eaten. In practice quad files don't do
  this, and the issue's spec explicitly accepts it.
- Whether quad queries under `:- if(false)` should be skipped: yes for free,
  since `process_term()` checks `ifs_blocked` before `quads()` — but note the
  answer terms too must be skipped while blocked (they are, same check).
- `sto`/`unexpected`/`ad_infinitum` handling can be stubbed (treated as
  "any outcome accepted") in the first matcher version.
