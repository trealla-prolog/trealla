# Memory-mapped string slices were never unmapped

**Fixed** — see *Fixed* below. Kept because the diagnosis corrects a
long-standing and wrong description of this bug, and because the
underlying representation issue is still open.

`open/4` with the `mmap(Ls)` option mapped the whole file and nothing
ever unmapped it — not `close/1`, not backtracking, not scope exit.
Every such open retained the full file size for the life of the
process.

This supersedes the earlier description of a "string-slice-on-backtrack
leak of ~53 KB". Three parts of that framing were wrong, see *What the
earlier framing got wrong* below.

## The mechanism

`FLAG_CSTR_SLICE` is set in exactly one place in the tree —
`src/bif_streams.c`, in `open/4`'s mmap option:

    tmp.flags = FLAG_CSTR_BLOB | FLAG_CSTR_STRING | FLAG_CSTR_SLICE;
    tmp.val_str = addr;          // from mmap()
    tmp.str_len = len;

So a "slice" is not a view into a packed string; it is a mapped *file*.
Two facts finish the story:

- **`munmap` appeared nowhere** in `src/`, `library/` or `tests/`. The
  mapping was never released by anything.
- **`unshare_cell_` (`src/internal.h`) has no slice branch.** It
  handles `is_strbuf`, `is_bigint`, `is_rational` and `is_blob`. A
  slice carries no refcount struct at all — just `val_str` and
  `str_len` — so refcounting cannot drive a release even in principle.

`USE_MMAP=0` appears only in the freestanding configuration, so this
was live in ordinary builds.

## Measurement

256 KB file, whole mapping touched, stream closed each iteration:

| opens | peak RSS |
|-------|----------|
| 1     | 9.1 MB   |
| 20    | 13.9 MB  |
| 100   | 33.9 MB  |
| 200   | 58.9 MB  |

+49.8 MB across 199 further opens against an expected 199 x 256 KB =
51 MB — one whole mapping retained per open, linear in file size. A
2 MB file touched at only one page grows RSS by one page per open,
confirming it is the mapping that is retained rather than anything
proportional to what is read.

Reproduce with:

```prolog
walk([]).
walk([_|T]) :- walk(T).
once_open(F) :- open(F, read, S, [mmap(Ls)]), walk(Ls), close(S).
run(N) :- ( between(1,N,_), once_open('some-256k-file'), fail ; true ).
```

## Blast radius

Wider than a diagnostic curiosity. `phrase_from_file/[2,3]` uses this
option (`library/pio.pl`), as do three paths in `library/builtins.pl`.
Any program that DCG-parses files in a loop retains every file it has
ever read.

## Why no leak checker finds it

The memory comes from `mmap`, not `malloc`, so neither LeakSanitizer
nor macOS `leaks` tracks it — both were clean on a run that was plainly
retaining memory. Growth in RSS is the only thing that shows it.

Separately, and worth knowing before trusting any ASAN result on this
machine: **LeakSanitizer is not supported on macOS arm64.** A
deliberate unfreed `malloc` under `ASAN_OPTIONS=detect_leaks=1` reports

    AddressSanitizer: detect_leaks is not supported on this platform.

So an ASAN run here checks memory *errors* only. Leak claims need
Linux, and would still miss this one for the mmap reason above.

## What the earlier framing got wrong

- **"~53 KB"** — far too small. That is incidental allocation; the
  mappings are file-sized and unbounded.
- **"on backtrack"** — it is not backtracking-specific. Every mmap'd
  open leaks, including a straight-line one that closes its stream.
- **"leaks that are not ours" (in the tabling context)** — correct, and
  still the right advice for that document: this has nothing to do with
  tabling and should not be chased there.

## Fixed

`stream_close()` now unmaps: the stream records `mmap_addr`/`mmap_len`
at open and `munmap`s them at close. Measured on the reproduction
above, 256 KB file, 200 opens: **58.9 MB -> 9.0 MB**, flat.

An earlier draft of this note claimed the mapped list was *designed* to
outlive its stream, so unmapping at close would be a use-after-free.
That was wrong, and inferred from the README's "zero-overhead file as a
list" phrasing rather than from the call sites. All four in-tree uses
are

    setup_call_cleanup(open(F, read, S, [mmap(Ls)]), <consume Ls>, close(S))

— `library/pio.pl` (`phrase_from_file/[2,3]`) and three in
`library/builtins.pl`. The list is consumed inside the Goal and closed
after, so the mapping's lifetime already *was* the stream's lifetime
everywhere. "Do not use `Ls` after `close/1`" is a constraint the
codebase already honours universally, not a new restriction.

## Residual hazard: escaping slices

Nothing deep-copies a slice — `is_slice` appears nowhere in `heap.c`,
`terms.c` or `unify.c`, and `share_cell_` has no slice branch — so a
copy of a slice cell aliases the raw address with no refcount. That was
harmless while nothing ever unmapped. Now that close unmaps, a slice
copied somewhere that outlives the stream dangles:

- `assertz(foo(Ls))` inside the goal, read after close;
- a slice reaching a **tabled** answer, since `tbl_image` does
  `copy_term_to_tmp` + `dup_cells` + `share_cell`, none of which
  materialise it.

Neither happens in-tree, and both previously leaked rather than
dangled. It is a real edge worth a test if the mmap option gets wider
use.

## The better fix: delete the raw representation

The raw slice variant (`val_str` + `str_len`, `FLAG_CSTR_SLICE`) exists
solely to support this one mmap site. It has **11 references in the
whole tree**, and the refcounted variant already does everything it
does — including cheap tails, which is the property it was presumably
introduced for. The two tail-advance paths are the same cost:

    // slice                             // strbuf  (parser.c)
    tmp->val_str  = l->val_str  + n;     tmp->strb_off = l->strb_off + n;
    tmp->str_len  = l->str_len  - n;     tmp->strb_len = l->strb_len - n;

Both are a cell copy plus two field adjustments — O(1), no allocation.
So reading the file into a refcounted `strbuf` instead of mapping it
would remove the leak, the lifetime constraint and the dangling hazard
together, and delete a whole cell representation with its special cases.

Two things to settle first:

- `make_slice()` in `query.c` has a slice fast path but **no strbuf
  fast path** — it falls through to `make_stringn()`, which copies.
  That needs the same two-line branch, which is also a latent win for
  ordinary packed strings.
- `mmap` pages lazily; a `strbuf` is read eagerly. Irrelevant for
  ordinary files, a regression for multi-gigabyte ones. That trade is
  the only real argument for keeping the mapping.
