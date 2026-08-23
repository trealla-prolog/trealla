# GUSTTO v2

A Grand Unified System of Tasks and Thread Objects.

**The idea in one line:** a thread object holds a *task* rather than a
dedicated query, and a pool of worker threads picks up ready tasks, runs
them until they suspend, and puts them back. Threads and tasks stop
being two mechanisms and become one schedulable thing.

v1 tried to get there by reimplementing threads on top of tasks, and ran
aground on the obvious objection: a blocking thread would stall every
other thread sharing the scheduler. v2 turns that over. With a pool of N
workers a blocked worker costs 1/N rather than everything, and the pool
can grow when workers block. That is a real answer, not a hope.

**This is exploratory.** Each phase is a checkpoint we can back out of,
and phase 2 is deliberately a place to stop and see how it behaves
before committing to the pool. Nothing here has to be finished to be
worth having.


## Why this fits Trealla specifically

**A task is already a resumable continuation.** `q->yielded`, a pushed
choice point, and three places to park in — the ready FIFO, the timer
min-heap, the io poll list. A thread today is a pthread stack, which
nothing can resume. So putting a task inside a thread object is not a
reorganisation, it is the thing that gives threads suspend/resume at
all, and suspend/resume is the whole precondition for pooling.

**The interpreter is flat.** `findall`/`bagof` run through choice points
in the main loop rather than re-entering the solver, so a task can
suspend at nearly any instruction boundary. Only four places nest
`start()`, and they become no-suspend zones — which they already are for
tasks today:

| Nested `start()` | Where |
|---|---|
| `format/3` `~@` | `src/bif_format.c:743` |
| `with_output_to` and friends | `src/bif_streams.c:2492`, `:2574` |
| engines | `src/bif_maps.c:454` |
| thread signal delivery | `src/bif_threads.c:440` |

**The cell layer is already thread-ready.** Refcounts and `dbgen` are
`_Atomic int64_t` (`src/internal.h:41`).

**What is left of threads is worth keeping.** After the v1 rip-out the
surviving API is `thread_create/3`, `thread_send_message` /
`thread_get_message` / `thread_peek_message`, queues and mutexes — the
SWI-shaped surface. That is what phase 2 re-implements, not replaces.


## Phases

Ordered so that each one is independently testable and the risky part
comes second, not last.

### Phase 0 — hoist the scheduler from `query` to `prolog` — done

The single enabling change. It turned out **not** to be a pure refactor,
in exactly one place, and that place was a bug:

- the scheduler is now one per `prolog`, reached through `sched_get()`,
  and freed by `pl_destroy()` rather than `query_destroy()`
- ownership is tracked separately: a task still sits on its spawner's
  `q->tasks` registry, and every query above it counts it in a new
  `q->num_subtasks`. The scheduler answers *what can run*, the registry
  answers *whose is it*. That split is what lets `wait/0` still mean
  "until my own work is done" over shared queues.
- `wait/0` therefore returns when the caller's whole **subtree** is
  done, at any depth, rather than when its direct children are
- `query_destroy()` now calls `sched_release()` to unlink its tasks
  before tearing them down. With a per-query scheduler this was free -
  the queues died with the query - but they outlive any one query now,
  and anything left in them would dangle.

**The semantic change:** a task that spawned another and did not wait
used to have that child silently discarded, because the child went into
a scheduler nothing would ever drain. It now runs. Two cases in
`tests/sundry/task_ownership.pl` changed to say so, and both are marked
with what they used to report. Nothing else moved: nesting order, spawn
FIFO, and the two-phase `end_wait/0` behaviour are unchanged.

Verified with the full suite under `-fsanitize=address` as well as
optimised - the unlink-before-destroy path is where a dangling task
would show up, and it is clean. `library(concurrent)` is unaffected,
`future_any/2` early exit included.

Today `scheduler *sched` lives on `query` (`src/internal.h:827`),
allocated lazily by `push_task()`, and only turns over when a parent
calls `wait/0`. `q->tasks` is that parent's registry. A thread object
has no parent to drive it, so none of this can schedule one.

- move `sched` and the task registry to `prolog`
- `wait/0` becomes "run until *my* children are done" rather than "run
  the only scheduler there is"; tasks already carry `->parent`, so the
  ownership test exists
- audit `end_wait/0` and `q->end_wait` against the new ownership — the
  flag currently belongs to whoever called `wait/0`

Still strictly single-threaded and one worker: nothing runs in
parallel, and the queues are untouched by any other thread.

### Phase 1 — make blocking primitives suspend

The bulk of the work, and the part that decides whether v2 is possible.

`thread_get_message/2` blocks in a C loop on a condvar —
`suspend_thread()` at `src/bif_threads.c:457`, inside
`do_match_message` — with the interpreter state sitting mid-builtin.
A task cannot suspend there, because the C stack cannot unwind. It has
to be restructured so the task parks on the queue's wait-list and
`thread_send_message/2` wakes it. Same for `thread_join/2` and mutex
acquisition.

**This is the actor mailbox.** A queue holding parked tasks, woken by a
send, is exactly what the actor model needs — so it gets built once and
serves both. Which is also why v1's "actors as a capstone" was the wrong
shape: the mailbox is not the reward for finishing, it is the mechanism
that makes the rest work.

**`send/1`, `recv/1` and `await/0` go rather than move.** Two reasons,
and the second is the stronger:

- `library(concurrent)` can work around them. That has been tried:
  porting `future/3` and `await/2` onto the shared database keeps the
  whole public API, and `future_any/2` keeps its early exit because
  `end_wait/0` releases a `wait/0` with tasks still queued. All four
  cases in `samples/test_concurrent.pl` behaved as before.
- `recv/1` is the *worse* of the two selective receives we have. Given a
  queue of 1,2,3,4 and a receive of 3, `recv/1` leaves `[4,1,2]` — the
  skipped messages rotate to the back — where
  `thread_get_message/2` leaves `[1,2,4]`, scanning without disturbing
  the queue. The thread mailbox is already Erlang-correct; `recv/1`'s
  rotation is not a design worth preserving.

So the thread mailbox becomes *the* mailbox, and phase 4's actor
addressing is added to it rather than to a second mechanism. What
`send/1` had that it lacks is only reach: it can address `q->parent`,
where a thread queue is addressed by id.

### Phase 2 — thread objects hold tasks

- `thread.q` becomes the task
- `thread_create/3` creates a task, not a pthread
- **`get_self()` breaks and must be rewritten.** It finds the current
  thread by scanning for `t->id == pthread_self()`
  (`src/bif_threads.c:410`). Under a pool `pthread_self()` is the
  *worker*, not the thread object, so identity has to come from "what is
  this worker running". Eight call sites depend on it, including mutex
  ownership and `do_unlock_all`.
- thread identity becomes id + mailbox + task, with no pthread in it

**Drop the fixed table here too.** `thread threads[MAX_THREADS]`
(`src/internal.h:1034`) is a 2048-entry inline array in the `prolog`
struct, and a thread's channel *is* its array index — that index is what
gets boxed into the Prolog term with `FLAG_INT_THREAD`. Replacing it
with a skiplist, the way the rest of the system stores things, is the
right move once identity is being reworked anyway:

- it removes the cap that v1 wrongly blamed on the O/S
- `new_thread()`'s linear scan for a free slot goes away entirely;
  allocation becomes malloc plus insert
- ids stop being *reused*, which kills a real class of bug: today slot
  `n` can be freed and reissued while a message in flight still names it

It is probably also a performance *win*, not a cost. The ~26
`MAX_THREADS` sites are scans that walk all 2048 entries
unconditionally, however few threads are live — `get_self()` among them,
which sits on the mutex path, and `thread_cancel_all()`,
`do_unlock_all()` and the tabling cleanup. Iterating a skiplist
(`sl_first` / `sl_next`) is O(live), so those all get cheaper. Only
keyed lookup goes the other way, O(1) array index to O(log n), and that
is the smaller effect by some distance.

So the real cost is the ~40 `threads[n]` index sites to convert, and
two things that need care:

- **lifetime.** A `thread *` into a fixed array is stable forever. Once
  entries are allocated, a pointer can dangle — so either refcount them,
  or never free until `pl_destroy()`, or leave tombstones. This is the
  part most likely to bite later, and it should be decided before the
  first entry is malloc'd rather than after.
- `&pl->threads[0]` is used as an implicit "main thread" in `bif_os.c`
  (`:504`, `:619`, `:670`, `:688`). That needs to become an explicit
  object rather than an index that happens to be zero.

**Stop here and look.** Threads on tasks, single worker, no pool: it
should already run, and how it behaves is the most informative thing we
can learn before building phase 3. A compute-bound thread will starve
its worker at this point, because preemption does not land until phase
3 — that is expected, not a bug to chase.

### Phase 3 — the worker pool

- N workers pulling from one shared ready queue. **Not per-CPU queues.**
  Affinity and work stealing fall together: the only reason to steal is
  to rebalance queues that affinity made uneven. A task migrating
  between workers is safe because exactly one worker holds it at a time
  and the queue's own synchronisation supplies the barrier on handoff —
  release/acquire, not affinity. Cache locality was never the argument;
  a task's working set is its own malloc'd heap and frames.
- **A growth valve is not optional.** FFI calls, file I/O and TLS reads
  have no suspension point and will pin a worker —
  `do_yield_on_stream()` only parks plain sockets (`str->is_socket &&
  !str->ssl && !str->is_memory`). With a fixed pool, N workers all
  blocked waiting on a runnable-but-unscheduled thread is a deadlock,
  not a slowdown.
- **Preemption**, or a compute-bound thread hogs a worker forever. Yield
  points are otherwise only sleep, socket EAGAIN and `yield/0`. The hook
  already exists: `YIELD_INTERVAL` in the main loop at
  `src/query.c:2453`, currently gated on `q->yield_at`, which only the
  embedding API sets.
- **Shared-state locking**, which is where the real bug risk lives:
  - clause resolution reads the db with no reader lock; only writers
    take `pl->guard`. True today for `thread_create/3`, but every task
    would now be exposed.
  - streams are per-`prolog` (`src/internal.h:1033`), so two tasks
    writing stdout interleave mid-term.
  - `module_lock()` / `module_unlock()` (`src/module.h:68`) are defined
    and **never called from anywhere**. Either they are the missing
    reader lock or they should go.
- pool width is a setting with a sensible default, not a number derived
  from `cpu_count` — the right width depends on how I/O-bound the load
  is. `cpu_count` and `os_threads` are informational.

### Phase 4 — actors

Mostly falls out of phase 1. What is left is addressing by name, links
and supervision. Each task is already a query with its own heap, so
actors are isolated by construction, and `send/1` already copies terms
rather than sharing them (`clone_term_to_tmp` + `share_cell`).


## Before any of it: tests

Coverage for this whole area is two files —
`tests/sundry/tasks_scheduler.pl` and `tests/sundry/task_args.pl` — and
phase 0 is a refactor with nothing to check itself against. Thicken
first, not after:

- task ownership: nested `call_task`, a task spawning a task, `wait/0`
  from more than one place
- `end_wait/0` across the new ownership rules
- mailbox behaviour before it moves: send/receive ordering, selective
  receive, timeouts, a receiver that never gets its message
- `thread_join/2` and mutex handoff, which phase 1 rewrites

Tests written against the *current* semantics are the specification for
the refactor. Anything not pinned down now is something that can change
silently in phase 1.


## Open questions

**Settled:**

- *Does `thread_create/3` keep preemptive semantics between phases 2 and
  3?* No, and that is fine — phase 2 is a checkpoint to run it and see,
  not a release. Preemption arrives with the pool.
- *Is `MAX_THREADS 2048` still the right shape?* No. The fixed table
  goes, replaced by a skiplist, as part of phase 2. See there.

**Still open, revisit when we reach them:**

- **What happens to the embedding API?** `pl_yield_at()` and
  `q->yield_at` assume the host drives the scheduler. A pool changes who
  is in charge. Not urgent before phase 3.
- **Logical update view under parallel workers.** `dbgen` is atomic, but
  whether a task's snapshot semantics survive two workers asserting
  concurrently has not been checked. Only matters once there is more
  than one worker — so, phase 3.

Both are phase 3 problems, and phase 3 is the one we have explicitly
reserved the right not to reach.
