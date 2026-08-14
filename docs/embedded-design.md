# Trealla Prolog Freestanding Build Implementation Plan

## Objective

Add a low-overhead embedded target to Trealla Prolog that can run in a freestanding environment without libc, libm, a filesystem, processes, networking, signals, dynamic loading, or threads.

The first target should be a memory-only, single-threaded Prolog engine supporting atoms, terms, integers, big integers, rationals, source loaded from memory, queries, and selected embedded Prolog libraries.

Normal hosted builds must retain their current API, behavior, and performance. In particular, hosted hot paths should continue to call libc directly rather than paying for platform callback dispatch.

## Initial Scope

### Included

- Engine creation and destruction
- Parsing Prolog source supplied in memory
- Query creation, iteration, and destruction
- Atoms, variables, structures, and lists
- Integers, big integers, and rational arithmetic
- Selected Prolog libraries embedded as C data
- Host-provided allocation
- Callback or memory-backed output
- Explicit execution interruption
- Structured error reporting

### Initially excluded

- Floating-point literals and arithmetic
- Transcendental mathematics
- Filesystem access
- Standard input, output, and error as `FILE *`
- Locale-dependent behavior
- Wall-clock and calendar predicates
- OS entropy
- Signals and alarm timers
- Threads and synchronization services
- Processes, pipes, and shell commands
- Sockets, DNS, and TLS
- Dynamic loading and libffi
- Interactive history and line editing

These features can later be added as independent capabilities without enlarging the mandatory platform interface.

## Proposed Build Configuration

Add a new build setting distinct from the existing `EMBED=1` option:

```make
FREESTANDING ?= 0
```

For `FREESTANDING=1`:

- Define `TPL_FREESTANDING=1`.
- Force FFI, OpenSSL, and threads off.
- Exclude the CLI, interactive history, networking, OS, POSIX, task, thread, and filesystem modules.
- Do not link libm, libdl, pthreads, editline, readline, OpenSSL, or libffi.
- Build a static `libtpl-freestanding.a` rather than the `tpl` executable.
- Use an explicit, curated source list.
- Embed only the selected Prolog library modules.
- Permit target-specific flags such as `-ffreestanding` without changing hosted defaults.

Capabilities should be represented independently:

```c
TPL_CFG_FLOAT
TPL_CFG_MATH
TPL_CFG_FILESYSTEM
TPL_CFG_TIME
TPL_CFG_RANDOM
TPL_CFG_THREADS
TPL_CFG_NETWORK
TPL_CFG_PROCESS
TPL_CFG_FFI
TPL_CFG_LOCALE
```

Unavailable built-ins should be omitted from the registered built-in tables rather than compiled as functions that fail at runtime.

## Proposed Platform Interface

The mandatory platform interface should remain small:

```c
typedef struct tpl_platform {
    void *context;

    void *(*alloc)(void *context, size_t size);
    void *(*calloc)(void *context, size_t count, size_t size);
    void *(*realloc)(void *context, void *ptr, size_t size);
    void  (*free)(void *context, void *ptr);

    void (*write)(void *context, unsigned channel,
                  const void *data, size_t length);

    bool (*should_interrupt)(void *context);
} tpl_platform;
```

Optional capability extensions may provide:

```c
uint64_t (*monotonic_us)(void *context);
uint64_t (*wall_time_s)(void *context);
bool (*entropy)(void *context, void *dst, size_t length);
```

Filesystem or device access should use an optional stream interface with open, read, write, seek, flush, and close operations.

## Allocator Requirements

The allocator is the only unavoidable external runtime service for a useful general-purpose engine.

It must provide:

- Alignment suitable for every Trealla object, preferably `max_align_t`
- Zero-filled `calloc` behavior
- Multiplication overflow detection in `calloc`
- Content-preserving `realloc`
- Safe `free(NULL)` behavior
- Consistent zero-size allocation behavior
- `NULL` on allocation failure

Trealla performs genuine variable-sized reallocations and individual frees. A pure bump allocator will therefore leak until engine destruction unless it implements size-aware reallocation. A segregated free list, TLSF allocator, or host heap is the preferred general implementation. An arena-per-engine remains useful when reclaiming all memory at engine destruction is acceptable.

## Minimal Embedding API

```c
typedef struct tpl_engine tpl_engine;
typedef struct tpl_query tpl_query;

typedef struct {
    const tpl_platform *platform;
    const void *library_image;
    size_t library_image_size;
    size_t memory_limit;
    uint64_t random_seed;
    unsigned capabilities;
} tpl_config;

tpl_engine *tpl_create(const tpl_config *config);
void tpl_destroy(tpl_engine *engine);

tpl_status tpl_consult_text(
    tpl_engine *engine,
    const char *virtual_name,
    const char *source,
    size_t source_length);

tpl_status tpl_query_begin(
    tpl_engine *engine,
    const char *goal,
    size_t goal_length,
    tpl_query **query);

tpl_status tpl_query_next(tpl_query *query);
void tpl_query_destroy(tpl_query *query);

void tpl_interrupt(tpl_engine *engine);
const tpl_error *tpl_last_error(const tpl_engine *engine);
```

The existing `pl_*` API should remain available as a source-compatible hosted facade.

## Work Plan

### Phase 1: Define the freestanding contract

1. Document the initial supported language and predicate set.
2. Define compile-time capabilities and their dependencies.
3. Specify the behavior of unavailable predicates.
4. Define allocation failure and interruption semantics.
5. Identify compiler runtime helpers permitted on each target.

**Exit criteria:** The initial capability matrix and public platform contract are agreed and documented.

### Phase 2: Introduce platform configuration

1. Add `tpl_platform` and `tpl_config` definitions.
2. Add `tpl_create(const tpl_config *)`.
3. Store configuration and platform state per engine.
4. Implement the existing `pl_create()` as a hosted wrapper.
5. Begin moving ambient global configuration into the engine instance.

**Exit criteria:** Hosted construction works through the new configuration layer with no API breakage.

### Phase 3: Centralize allocation

1. Replace every direct call to `malloc`, `calloc`, `realloc`, `free`, and `strdup` in engine code.
2. Include imath, regex, skip-list, tabling, DCG, base64, parser, modules, and streams.
3. Add `tpl_strdup` and overflow-checked array allocation.
4. Define uniform allocation failure propagation.
5. Remove fatal out-of-memory `abort()` paths.
6. Add a tracking allocator for tests.
7. Add failure-injection tests at allocation boundaries.

For hosted builds, allocation macros should preprocess to direct libc calls. Freestanding builds should call the configured allocator.

**Exit criteria:** The core and selected libraries contain no direct allocator calls, and forced allocation failures do not crash or corrupt the engine.

### Phase 4: Decouple streams and diagnostics from stdio

1. Introduce internal stream operations for read, write, seek, flush, and close.
2. Implement memory-backed input streams.
3. Implement callback-backed output streams.
4. Retain a hosted `FILE *` stream backend.
5. Stop automatically installing stdin, stdout, and stderr in freestanding construction.
6. Route diagnostics through the configured output or error mechanism.
7. Remove `FILE *` from the new embedded public API.
8. Add source consultation directly from byte buffers.

**Exit criteria:** A program can be consulted and queried using only memory and callbacks.

### Phase 5: Create the minimal engine source set

1. Divide sources into core and optional feature groups.
2. Build built-in tables conditionally by capability.
3. Remove references to excluded built-in arrays.
4. Exclude OS, POSIX, networking, filesystem, tasks, threads, FFI, history, and CLI sources.
5. Select the minimal embedded Prolog library set.
6. Verify that excluded modules create no undefined references.

**Exit criteria:** A curated engine archive links without optional subsystem objects.

### Phase 6: Remove remaining libc dependencies

1. Inventory the memory and string operations still required by the minimal source set.
2. Supply small freestanding implementations where the compiler cannot provide them.
3. Replace locale-sensitive `ctype` and `wctype` use with deterministic helpers.
4. Use ASCII rules initially and explicitly document Unicode behavior.
5. Replace basic integer parsing and formatting dependencies.
6. Audit and remove `snprintf`, `sscanf`, `strtod`, `strtoll`, and related calls from the minimal profile.
7. Remove environment, path discovery, and current-directory assumptions.

**Exit criteria:** The minimal engine has no unresolved libc symbols except explicitly approved compiler memory helpers.

### Phase 7: Implement the no-libm numeric profile

1. Preserve integer, big-integer, and rational arithmetic.
2. Conditionally exclude transcendental and floating-point built-ins.
3. Remove floating environment operations from the minimal parser and evaluator.
4. Reject unsupported float literals with a clear structured error.
5. Verify that imath does not introduce libm references in this profile.
6. Document the numeric subset.

**Exit criteria:** The archive links without libm and passes integer, big-integer, and rational arithmetic tests.

### Phase 8: Isolate optional platform services

1. Replace signal-driven interruption with `tpl_interrupt()` and `should_interrupt`.
2. Use an engine-local PRNG with an explicit seed.
3. Add optional monotonic time, wall-clock, and entropy hooks.
4. Eliminate automatic seeding from time, addresses, or process-global random state.
5. Remove `atexit()` reporting and expose explicit diagnostics instead.
6. Convert remaining fatal `abort()` paths to returned errors.
7. Verify that threads, processes, networking, TLS, dynamic loading, and filesystem code are absent from the minimal link.

**Exit criteria:** Construction and normal evaluation require no OS service other than the supplied allocator and optional output callback.

### Phase 9: Add the build target

1. Add `FREESTANDING=1` to the build system.
2. Add a curated freestanding source list.
3. Produce `libtpl-freestanding.a`.
4. Add embedded library-image selection.
5. Avoid all hosted external libraries.
6. Provide documented target flags without forcing them on hosted builds.
7. Add a command that audits undefined symbols in the resulting archive.

**Exit criteria:** One command reproducibly creates the freestanding archive and embedded library image.

### Phase 10: Build a reference host shim and example

1. Provide a small allocator-backed platform implementation.
2. Construct an engine without a C runtime startup path.
3. Consult an in-memory Prolog program.
4. Run a query and iterate over its solutions.
5. Capture output through a callback.
6. Demonstrate interruption and error retrieval.
7. Destroy the engine and verify full memory reclamation.

**Exit criteria:** The example runs on a minimal test harness and demonstrates the complete embedding lifecycle.

### Phase 11: Verification and compatibility

1. Run the full existing hosted test suite after every structural phase.
2. Benchmark representative hosted workloads before and after the changes.
3. Inspect generated hosted code where necessary to confirm direct libc allocation remains.
4. Audit every undefined symbol in the freestanding archive.
5. Link against an environment that deliberately lacks libc and libm.
6. Test allocator exhaustion at many allocation points.
7. Test malformed input, interruption, repeated construction, and repeated destruction.
8. Test multiple independent engine instances.
9. Run sanitizers on the hosted reference configuration.
10. Test at least one constrained target or freestanding emulator.

**Exit criteria:** Hosted compatibility is maintained, no material hosted performance regression is observed, and the freestanding link contains only approved platform/compiler dependencies.

### Phase 12: Documentation and later capabilities

1. Document supported predicates and omitted capabilities.
2. Document allocator semantics and memory sizing.
3. Document permitted compiler runtime helpers.
4. Provide a platform porting checklist.
5. Document embedded Prolog library selection.
6. Add optional virtual filesystem support independently.
7. Add optional time and entropy capabilities independently.
8. Later add floating-point parsing and formatting using self-contained implementations.
9. Later add a selectable math provider for transcendental functions.

## Milestones

### Milestone 1: Configuration and allocation

- Platform/configuration API exists.
- Hosted API is preserved.
- All allocation is centralized.
- Allocation failure tests pass.

### Milestone 2: Memory-only execution

- Streams no longer require `FILE *` in the embedded core.
- Source can be consulted from memory.
- Output is delivered through callbacks.
- Basic queries execute successfully.

### Milestone 3: No-libm freestanding archive

- Minimal built-in and source sets are established.
- Integer, big-integer, and rational arithmetic work.
- The archive has no libm dependency.

### Milestone 4: Strict no-libc demonstration

- Undefined-symbol audit passes.
- A freestanding test harness links without libc and libm.
- The reference embedding example runs successfully.

### Milestone 5: Optional capability layers

- Time, entropy, virtual filesystem, and floating-point support can be added independently.
- None of these capabilities increase the mandatory platform interface.

## Hosted Performance Policy

Hosted builds should use compile-time selection so the existing direct calls remain after preprocessing:

```c
#if TPL_FREESTANDING
#define TPL_MALLOC(pl, size) \
    ((pl)->platform->alloc((pl)->platform->context, (size)))
#else
#define TPL_MALLOC(pl, size) malloc(size)
#endif
```

The same approach should be used for other performance-sensitive primitives. Platform callbacks are acceptable in the freestanding build because allocation and external I/O are already relatively expensive operations. They should not be introduced into hosted evaluator hot paths.

## Principal Risks

### Stdio entanglement

The stream system, diagnostics, printing, and public API currently rely heavily on `FILE *`. Stream abstraction is likely to be the largest structural change.

### Numeric parsing and formatting

Complete floating-point parsing and formatting without libc is substantial. Deferring floating-point support keeps the first target bounded.

### Incomplete allocation centralization

Several bundled components call allocators directly. The undefined-symbol and source audits must cover all selected third-party and utility code.

### Hidden compiler runtime dependencies

Wide integer division, atomics, stack protection, and other compiler transformations may introduce runtime helper symbols. These must be audited per target rather than assumed absent.

### Global process state

Argument vectors, library paths, interrupt flags, engine counts, and other globals may impede independent engine instances. Configuration should move into the engine incrementally while preserving hosted compatibility.

## Completion Criteria

The initial project is complete when:

- `libtpl-freestanding.a` builds through a documented command.
- It links in a test environment without libc and libm.
- The only required host service is a conforming allocator; output and interruption may be optional.
- It consults Prolog source from memory and executes queries.
- Integer, big-integer, and rational arithmetic pass their tests.
- Unsupported facilities are absent or return structured capability errors.
- Allocation exhaustion is handled without crashes or corruption.
- Engine destruction reclaims all owned memory.
- Existing hosted tests pass.
- Hosted benchmarks show no material regression.
- The embedding API and porting requirements are documented.
