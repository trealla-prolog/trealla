# abolish_table/1 and per-instance tables

Both done, in `native-tabling-full.patch` (now cumulative: tabling +
the two leak fixes). The standalone `leak-fix-*.patch` files are the
same changes for separate upstream submission — apply the cumulative
patch **or** the pieces, not both.

## Per-instance tables

Everything in tabling.c was a file static, so two `prolog` instances in
one process shared tables. Demonstrated with the embedding API — two
instances each computing a tabled fib(22), counting workers via a
dynamic counter:

    before:  A: workers=21   B: workers=0    <- B inherited A's tables
    after:   A: workers=21   B: workers=21

State now lives in a `tbl_state` hanging off `prolog` as an opaque
`void *tabling_state`, allocated on first tabled call and freed by
`tabling_destroy()` from `pl_destroy()`. The thread-ownership guard
moved in too, so it is now per-instance rather than process-wide.

This is also the groundwork for locking: the state a mutex would have
to protect is now one struct with one owner, instead of fifteen
statics.

## abolish_table/1

`abolish_table(+Spec)`, exported from `library(tabling)`. Spec takes
the same shapes as the `(:- table)` directive: `Name/Arity`,
`Name//Arity` for a DCG non-terminal, or a comma-conjunction.

Needed because a completed table does not notice `assert/retract` on
the predicates it derived from — the answers just stay as they were:

    ?- findall(X-Y, path(X,Y), L).       L = [a-b].
    ?- retract(edge(a,b)), findall(X-Y, path(X,Y), L).
       L = [a-b].                        % still

Until incremental tabling exists, invalidating by hand is the route,
and `abolish_all_tables/0` was too blunt for it. Selectivity is proven
by counter, not by inspection: with two tabled predicates computed,
`abolish_table(drop_t/1)` leaves `keep_t/1`'s worker count unchanged
and bumps only `drop_t/1`'s.

Errors: `instantiation_error` for a var, `type_error(predicate_indicator, S)`
for a bad shape, and `existence_error(table, Name/Arity)` if the
predicate is not tabled — silence there would hide a typo while the
caller believes stale answers were dropped.

Implementation: each table now records the functor/arity it answers and
the trie leaf pointing at it, so abolish walks the table list, destroys
matching variants and resets `leaf->value` to NULL. The trie node stays
(a NULL value just reads as "fresh"). It refuses while a leader is
running, same rule as `abolish_all_tables/0`, and only bumps the
generation if something actually went.

## Validation

- `tests/misc/tabling.pl`: **17/17**, new `abolish_table: ok` case
  covering selectivity, assert-visibility and all three error shapes.
- `make test`: 313, identical to baseline.
- ASAN: tabling suite clean apart from the known 110 B / 4 residual
  (`atom_concat` + imath); repeated abolish/recompute loop clean;
  full 252-file corruption sweep clean.
- Logtalk under ASAN: iso8601 113 tests / 106 passed, pddl_parser 8 / 7,
  no ASAN errors.
- Fresh clone of `271703b` + cumulative patch: builds, 17/17.

## Still open, in the order I would do them

1. **Restraints** — an infinite answer set still gets OOM-killed rather
   than raising `resource_error`. `'$tbl_add_answer'` already walks the
   answer, so a count/size limit is nearly free.
2. **Table introspection** — no `statistics(tables, _)`; you cannot see
   how many tables exist or what they cost, which makes (1) hard to
   diagnose.
3. **Locking** — now a much smaller job than it was.
4. Attvars rejected; suspension inside an if-then-else *condition*;
   cross-module tabling not module-keyed.
