# Native tabling for Trealla — patch & macOS instructions

One patch, `native-tabling-full.patch`, against trealla-prolog/trealla
main (verified at commit `a2c4c67`). It adds:

- `src/tabling.c` — native tabling engine: variant/answer tries
  (hash-indexed children above 16 fanout, so flat key spaces like
  fib(N,_) stay O(1) per step), tables, worklists, `'$tbl_*'`
  builtins (~880 lines).
- `library/tabling.pl` — embedded `library(tabling)`: the
  `(:- table)/1` directive, clause renaming, and the driver.
- `src/bif_control.c` — shift/1 continuation-capture hardening:
  follows compiled if-then-else `'$jump'` cells instead of capturing
  them as goals; empty continuations are executable `true`.
- Prolog flag `tabling` (`true` default | `false`): read at call
  time; when `false`, tabled predicates run as PLAIN calls — no
  memoization, no termination guarantees (left recursion loops
  again). One-liner A/B comparison of a program with/without tabling.
- Wiring (`Makefile`, `library.c`, `internal.h`, `bif_predicates.c`,
  `prolog.h`, `prolog.c`).

## Apply & build (macOS)

    cd ~/trealla                      # your checkout, on main
    git status                        # start clean
    git apply native-tabling-full.patch
    make clean && make                # library/tabling.pl is embedded:
                                      # a full make is required

Applies with no warnings. `git apply --check` first for a dry run;
if main has drifted: `git apply --3way`.

## Use

    $ tpl samples/fib_tabled.pl       # no --library path needed
    ?- test.
    fib(10)=89 PASSED

    ?- time(fib(100,F)), !.
    % CPU elapsed 0.001s, 8_579 inferences
    F = 573147844013817084101.

    ?- time(fib(10000,F)), !.
    % CPU elapsed 0.053s     (SWI-Prolog: 0.053s)
    ?- time(fib(100000,F)), !.
    % CPU elapsed 1.365s     (SWI: 0.510s - the residual gap is
    %                         imath-vs-GMP bignum addition on ~21K-digit
    %                         numbers, not tabling; with small answers
    %                         scaling is exactly linear)

    ?- current_prolog_flag(tabling, V).
       V = true.
    ?- set_prolog_flag(tabling, false),   % plain SLD, no memoization
       time(fib(25,F)).
    % CPU elapsed 0.124s, 3_884_559 inferences

    ?- set_prolog_flag(tabling, true).    % back on, same session

`abolish_all_tables/0` clears all tables. Programs use
`:- use_module(library(tabling)).` and `:- table f/N.` as usual.

## Quick sanity suite

    # left recursion on a cyclic graph (the real tabling test —
    # only terminates with the flag on):
    $ cat > /tmp/p.pl <<'EOF'
    :- use_module(library(tabling)).
    :- table path/2.
    path(X,Y) :- path(X,Z), edge(Z,Y).
    path(X,Y) :- edge(X,Y).
    edge(a,b). edge(b,c). edge(c,a).
    EOF
    $ tpl -g "consult('/tmp/p.pl'), findall(X-Y,path(X,Y),L), length(L,N), write(N), nl, halt"
    9

    $ make test                       # regression suite

## Phase-1 limits (documented, not bugs)

Variant tabling with least-model semantics only — no tnot/WFS, no
incremental or answer-subsumption tabling. Attvars in tabled calls
error. A worker suspending inside an if-then-else *condition* is
unsupported (then/else branches are fine).

### Tabling is single-threaded per instance

Tables are **per-`prolog`-instance** — they hang off `prolog` as an
opaque `void *tabling_state`, allocated on first tabled call and freed
by `tabling_destroy()` from `pl_destroy()` — so two embedded
interpreters in one process do not share tables, and each can have a
different owning thread.

Within one instance the state is unlocked, so tabling belongs to the
first thread in that instance to use it. A tabled call from any other
thread *in the same instance* raises

    error(resource_error(tabling_not_thread_safe), '$tbl_variant_table'/3)

rather than racing the tries (which, before the guard, hung). It is a
normal catchable error:

    thread_create(( catch(my_tabled_goal, E, handle(E)) ), T, [])

`abolish_all_tables/0` clears the tables and releases ownership, so one
thread can hand tabling to the next. Per-thread tables are Phase-2.

### Tabled predicates need a finite answer set

A table is completed before its answers are returned (local
scheduling, as in Scryer and SWI by default), so a tabled call whose
answer set is infinite does not terminate — and because every answer is
stored, memory grows until the process is killed rather than spinning
in place. Scryer issue #573 is the canonical example:

    :- table as//0.
    as --> [].
    as --> [a], as.

    ?- phrase(as, Ls).      % Ls unbound: as([],[]), as([a],[]),
                            % as([a,a],[]) ... infinitely many answers.
                            % Diverges (killed on memory).

Untabled, the same non-terminal enumerates those solutions lazily and
the caller can stop after the ones it wants; tabling wants the whole
table first. Call tabled DCGs with a bound input and the table is
finite:

    ?- phrase(as, [a,a,a]).      % true
    ?- phrase(as, [a,b]).        % false

The general rule: table predicates whose answers are bounded. Guarding
this properly needs *restraints* (SWI's max_answer_size / XSB's bounded
rationality — turn a runaway table into a resource_error) or batched
scheduling (post answers before completion); both are Phase-2 work.
The same caveat applies to an unbounded chain of distinct call
variants, which grows the SCC nesting rather than the answer set.
