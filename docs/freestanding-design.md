# Freestanding Trealla — design

Status: phases 1-4 implemented on the `freestanding` branch; phase 5 remains
proposed.

## 1. Goal

Make an operating-system-free Trealla port a supported configuration rather
than a downstream fork. A porter should provide a small platform adapter and a
linker/startup environment, select the required Prolog facilities, embed an
application, and build the current Trealla sources without inventing POSIX
functions that cannot work on the target.

The first reference target will be a small QEMU machine. Physical boards and
board SDKs remain downstream concerns.

The design must preserve these properties:

- a normal `make` has the same features and behaviour as today;
- Cosmopolitan remains one binary that can run hosted or boot without a host
  OS;
- freestanding support is capability-driven, not a growing list of operating
  system names;
- unavailable facilities are removed or report a defined error; they do not
  reach panic stubs masquerading as POSIX implementations;
- cross-build tools run on the build host, while Trealla objects are compiled
  by the target compiler; and
- the supported surface is continuously built and boot-tested upstream.

## 2. Terminology

**Hosted** means Trealla runs as a process under an operating system and uses
the host's files, sockets, processes, clocks and terminal facilities.

**Freestanding** in this document means Trealla does not require a host
operating system. It does not initially mean `-ffreestanding` with no C
library. The first implementation may rely on a small target C runtime such as
Picolibc for allocation, strings, arithmetic and basic `stdio` support.

**Bare metal** describes a deployment which boots directly on hardware or a
machine emulator. It is one consumer of the freestanding configuration.

**Platform service** is a narrow operation supplied by the environment, such
as writing to a console or reading a monotonic clock.

**Trealla feature** is interpreter code selected at build time, such as
networking, processes or threads. A feature may require one or more platform
services.

Cosmopolitan's metal mode is a bare-metal deployment, but Cosmopolitan itself
supplies a broad runtime. A Cortex-M/Picolibc port has a much smaller runtime
and is the stronger test of the platform boundary.

## 3. Evidence from the existing embedded project

The Prolog4Embedded project successfully ran a reduced Trealla on two ESP32
targets. Its port had to:

- replace the normal front end;
- special-case history and networking;
- provide implementations or stubs for POSIX calls;
- add board-specific timekeeping;
- integrate Picolibc, startup objects and linker scripts; and
- convert the application Prolog file to a C array at build time.

Those are recurring porting concerns and should not require an interpreter
fork. The project also measured a 53% binary-size reduction and a 71% reduction
in peak heap use after removing facilities that its application did not need.
Its reduced interpreter still peaked at roughly 1.48 MB of heap, making
feature selection and allocator visibility functional requirements rather
than cosmetic build options.

Sources:

- <https://github.com/Prolog4Embedded/trealla>
- <https://github.com/Prolog4Embedded/T3100/blob/main/main.pdf>
- <https://github.com/trealla-prolog/trealla/pull/1011#issuecomment-4300653985>

The downstream source is a useful requirements document and source of test
ideas. It is based on an older Trealla and contains deliberately provisional
code, so it should not be merged wholesale.

## 4. Current upstream building blocks

Trealla already has several pieces needed by this design:

- `NOFFI=1`, `NOSSL=1` and `NOTHREADS=1` remove major hosted dependencies;
- `EMBED=1` converts Prolog libraries into target objects;
- `util/bin2c` is correctly built with `HOST_CC`, independently of the target
  compiler;
- `libtrealla.a` separates the engine from `tpl.c` and its `main()`;
- the embedding API can create an engine and inspect query answers without
  scraping terminal output; and
- the Cosmopolitan build detects metal mode and fails network operations
  before entering an unavailable syscall path.

The remaining gaps are structural:

- `tpl.c` assumes signals, processes, files, an environment and an interactive
  terminal;
- terminal code chooses readline, editline or isocline unless a platform is
  specially excluded;
- networking is compiled even when the target cannot provide sockets;
- filesystem, process, clock and memory-stream operations share large source
  modules, so whole-file exclusion is not always possible;
- `pl_create()` falls back to loading `library/builtins.pl` from a filesystem
  when that library is not embedded;
- `EMBED=1` embeds every library rather than an application-selected set;
- `NOLIB=1` means “do not produce `libtrealla.a`”, not “omit Prolog
  libraries”; and
- there is no supported public API for consulting a length-delimited Prolog
  source buffer.

## 5. Architectural boundary

The intended dependency direction is:

```
application / reference firmware
             |
       public embedding API
             |
     Trealla engine and selected features
             |
       internal platform services
             |
  board runtime / Picolibc / Cosmopolitan / POSIX
```

Trealla core must not include board headers or know board names. Platform
adapters may include board or runtime headers. A build selects features and
one adapter; it must not scatter checks such as `__MY_BOARD__` through the
engine.

### 5.1 Features versus services

Feature selection determines which predicates and implementation modules are
present. Platform services satisfy the remaining implementation's external
needs.

The build will generate or define numeric feature macros. They are positive
and always resolve to `0` or `1`, for example:

```
TPL_FEATURE_FILESYSTEM
TPL_FEATURE_MMAP
TPL_FEATURE_NETWORK
TPL_FEATURE_PROCESS
TPL_FEATURE_THREADS
TPL_FEATURE_TTY
TPL_FEATURE_FFI
TPL_FEATURE_TLS
TPL_FEATURE_REALTIME_CLOCK
```

`TPL_FREESTANDING` selects defaults; it is not itself used as a substitute for
individual capabilities throughout the source. Existing `USE_THREADS`,
`USE_FFI` and `USE_OPENSSL` definitions remain during migration and are derived
from the new feature values where practical.

Feature dependencies are validated by the build. Examples:

- TLS requires networking;
- mmap requires a filesystem and mmap service;
- an interactive TTY requires console input and output;
- threads require a scheduler/thread backend; and
- filesystem-backed library loading requires a filesystem.

An invalid combination fails during configuration rather than much later at
link time.

### 5.2 Default freestanding profile

The initial `FREESTANDING=1` profile will use these defaults:

| Facility | Default | Notes |
|---|---:|---|
| C allocation and string/math support | on | supplied by the target C runtime initially |
| embedded `library/builtins` | on | mandatory; no filesystem fallback |
| embedded application | selected by `PROGRAM` | optional for an engine library, required by the reference firmware |
| console streams | on | simple byte input/output, no line editor |
| monotonic clock | on | supplied by the platform adapter |
| realtime clock | off | optional |
| filesystem and mmap | off | memory and console streams remain available |
| networking and TLS | off | network predicates are not registered |
| processes and syslog | off | predicates are not registered |
| threads | off | cooperative engine facilities may be considered separately |
| FFI | off | target-native foreign predicates are a later design |
| interactive TTY/history | off | a downstream application may provide its own REPL |

The word “off” means the code is not linked and its predicates are normally not
registered. `current_predicate/1` therefore reports the actual image's
capabilities. Where a useful module mixes available and unavailable operations,
the module is split or the unavailable operation raises a defined Prolog error.
Silent dummy success and aborting POSIX stubs are not acceptable fallbacks.

### 5.3 Platform services

The first implementation uses an internal link-time interface rather than
committing immediately to a public ABI. A reference adapter will establish the
minimum useful set:

```
read console bytes
write console bytes
read monotonic time
halt the application
report a fatal platform failure
```

The exact signatures belong to the first implementation patch and will be
reviewed against both the QEMU runtime and hosted test adapter. Optional
services return a typed “unsupported” result; mandatory services must be
present at link time.

Once the interface has survived at least two materially different adapters,
it can become a versioned public structure. A future public structure must
contain a size/version field and an opaque caller context so fields can be
added without breaking source compatibility.

### 5.4 Allocation

Allocation is deliberately not hidden behind a superficial callback in phase
one. Trealla has process-global state, most engine allocations use
`TPL_malloc`/`TPL_calloc`/`TPL_realloc`/`TPL_free`, and a few bundled components
still call the C allocator directly. A per-`prolog` allocator would therefore
be misleading.

All engine-owned allocations now pass through one internal family. The public
installation rule is deliberately runtime-wide: `pl_set_allocator()` must be
called before the first `pl_create()` or any other Trealla allocation. The
first allocation locks the choice for the lifetime of the process. This
matches the current ownership of global atoms without pretending allocation is
per-engine.

The allocator layer records current bytes, peak bytes, successful allocation
operations and failed allocation operations. Returned strings from
`pl_term_text()` and `pl_int_text()` belong to this family and must be released
with `pl_free()`.

The runtime-wide option is the smaller compatible change. An explicit runtime
object which owns global atoms and the allocator remains cleaner, but is a much
larger future API and ownership change.

## 6. Build interface

GNUmakefile remains the canonical upstream build. The embedded project may
continue to use CMake downstream, but upstream should not require two feature
configuration systems to stay in sync.

The intended user-facing shape is:

```
make freestanding \
    CC=arm-none-eabi-gcc \
    AR=arm-none-eabi-ar \
    HOST_CC=cc \
    PROGRAM=app/fdir.pl \
    EMBED_LIBS='builtins lists error' \
    PLATFORM_OBJ=board/platform.o
```

Names may be refined during implementation, but their roles are distinct:

- `FREESTANDING=1` selects the OS-free feature defaults;
- `PROGRAM` identifies application source to embed;
- `EMBED_LIBS` replaces the all-or-nothing library set and always includes
  `builtins` for a freestanding image;
- `HOST_CC` builds source-generation utilities;
- `CC` and `AR` build target objects;
- `PLATFORM_OBJ` supplies the internal platform service contract; and
- ordinary `CFLAGS`/`LDFLAGS` remain available for the target runtime.

The primary artifact is `libtrealla.a` plus generated embedded-content
objects. Upstream does not attempt to own every board's reset vector, flash
layout or SDK. A reference firmware demonstrates how to link those artifacts.

The existing `compile` target will eventually share the same content generator
rather than maintaining a second embedding path.

## 7. Embedded source API

Freestanding applications cannot depend on a temporary file merely to load
their rules. Add a public, length-delimited operation with semantics equivalent
to consulting a named source:

```
bool pl_consult_text(prolog *pl,
                     const char *source,
                     size_t source_len,
                     const char *source_name);
```

The function owns no caller buffer and completes parsing before returning.
`source_name` is used in diagnostics and relative-source bookkeeping; it does
not imply filesystem access.

Generated program objects expose a byte array and its length. They do not need
to append a NUL byte or reach into `module` internals. The reference firmware
creates a `prolog`, calls `pl_consult_text()`, issues a query through the public
embedding API and inspects the answer through `pl_term`.

Freestanding `pl_create()` must never attempt a filesystem fallback for
`builtins`. Configuration fails if the required embedded library is absent.

Runtime rule updates over a serial link or reserved flash region are then an
application concern using the same API, rather than a new interpreter port.

## 8. Source organisation

Prefer separate implementation files over large conditional regions:

```
src/platform/platform.h
src/platform/hosted.c
src/platform/freestanding.c        # contract checks/common helpers
src/network_posix.c
src/network_none.c
src/history_tty.c
src/history_none.c
```

Names are illustrative. Splitting is justified where a source file otherwise
includes unavailable headers or leaves unavailable symbols in the link. Small
portable operations can remain in their existing modules behind feature
registration gates.

`bif_streams.c` and `bif_os.c` need special care because they mix useful core
operations with hosted services. They should be divided by capability rather
than removed wholesale. Memory streams, term I/O and monotonic timing remain
useful without a filesystem or processes.

## 9. Error and predicate policy

There are three cases:

1. **Feature omitted:** its predicates are not registered and calls receive the
   normal `existence_error(procedure, ...)`.
2. **Feature present, optional service unavailable at runtime:** the predicate
   raises a specific Prolog error appropriate to that operation.
3. **Mandatory platform service missing:** configuration or linking fails.

Returning `-1` without a Prolog error, silently succeeding, or trapping in a
function named like a working POSIX service does not meet the contract.

Cosmopolitan may continue to make a runtime decision because one APE runs in
both hosted and metal environments. The generic feature boundary must still be
usable at compile time by smaller targets.

## 10. Verification

### 10.1 Hosted freestanding smoke build

Before adding a cross-toolchain dependency, CI builds the freestanding profile
with the host compiler and a tiny hosted platform adapter. It embeds a program,
creates an engine through the public API, runs a deterministic query and checks
the structured answer.

The resulting link is inspected for a denylist of accidental hosted symbols,
including process creation, sockets, mmap, dynamic loading, pthreads and line
editing. This is not a proof of freestanding correctness, but it catches a
large class of dependency regressions cheaply.

### 10.2 QEMU boot test

A second CI job cross-builds one documented machine and boots it under QEMU.
The firmware must:

- print a fixed boot marker;
- load an embedded Prolog program;
- execute at least one successful and one failing query;
- validate a returned binding without parsing toplevel text; and
- print a fixed completion marker before halting.

The job has a strict timeout. The target and toolchain version are pinned.

### 10.3 Regression and footprint reporting

Normal hosted tests remain mandatory. Freestanding tests additionally cover:

- missing embedded `builtins` as a configuration failure;
- feature/predicate registration;
- text consultation with embedded NUL-safe length handling;
- unsupported-service error paths;
- exhausted-heap behaviour once allocation accounting exists; and
- selected library closure.

Binary size and peak heap are recorded from the first reference build. They are
reported before becoming hard limits, because toolchain and debug-format
changes can otherwise produce noisy failures. Once stable, CI enforces reviewed
budgets.

## 11. Implementation phases

### Phase 0 — design and dependency inventory

- agree this document;
- inventory headers, symbols and builtin registration by capability;
- record hosted and Cosmopolitan binary/undefined-symbol baselines; and
- identify the smallest query/library set needed for the smoke image.

Exit criterion: every undefined service in the proposed smoke image has an
owner or is explicitly excluded.

### Phase 1 — feature model and hosted freestanding smoke build

- add `FREESTANDING=1` defaults and numeric feature definitions;
- exclude complete hosted modules where possible;
- split or gate mixed modules;
- add no-TTY and no-network implementations;
- make missing feature dependencies configuration errors; and
- link and run the hosted smoke image.

Exit criterion: the smoke image passes and the hosted-symbol denylist is clean,
with no board-specific code in the engine.

### Phase 2 — selected libraries and embedded application

- add `EMBED_LIBS` with `builtins` mandatory for freestanding builds;
- add the public length-delimited `pl_consult_text()` API;
- generate a `PROGRAM` object with `HOST_CC` tooling;
- share content generation with the existing `compile` target; and
- exercise structured result inspection in the smoke test.

Exit criterion: an application containing no filesystem access is built and
run entirely from embedded bytes.

### Phase 3 — platform service contract and QEMU reference target

- introduce console, monotonic-clock, halt and panic services;
- provide hosted-test and QEMU adapters;
- add a reference startup/linker configuration outside the engine; and
- boot and query under QEMU in CI.

Exit criterion: the pinned QEMU target reaches the completion marker without
POSIX compatibility stubs.

### Phase 4 — allocator coverage and footprint

- route direct engine and bundled-library allocations through one family;
- add current and peak allocation accounting;
- define the runtime-wide allocator installation rule;
- test deterministic allocation failure; and
- establish size and heap baselines.

Exit criterion: all owned allocations are attributable and an intentionally
small heap fails with a controlled Prolog/resource error rather than corruption.

### Phase 5 — porting guide and second adapter

- document the required C runtime subset and service contract;
- add a generic board checklist and minimal example;
- validate the interface against a second, materially different runtime; and
- only then consider making the platform structure public ABI.

Exit criterion: a new adapter can be written without modifying Trealla engine
sources.

## 12. Patch sequence

Keep patches reviewable and independently useful:

1. feature definitions, dependency validation and a hosted smoke configuration;
2. terminal and network backend separation;
3. filesystem/process registration gates and mixed-module splits;
4. selected embedded libraries;
5. `pl_consult_text()` and generated application object;
6. platform service interface plus hosted adapter;
7. QEMU reference firmware and CI;
8. allocation unification and accounting; and
9. documentation, footprint budgets and second-adapter validation.

No patch should combine a build-system replacement with interpreter behaviour
changes. Normal hosted builds and tests remain green after every patch.

## 13. Explicit non-goals

The initial work does not:

- adopt the downstream CMake build as Trealla's canonical build;
- ship or maintain Picolibc, a board SDK or every linker script;
- promise strict no-libc ISO C freestanding support;
- make every Trealla predicate available without an operating system;
- optimise the 24-byte cell representation for 32-bit targets;
- provide hard real-time guarantees or spaceflight qualification; or
- treat the Prolog4Embedded fork as a patch series against current `main`.

Those may become separate projects after the platform boundary is stable.

## 14. Recommended decisions

- Call the profile `FREESTANDING`, not `BAREMETAL`: the former describes the
  software contract, while the latter is one deployment.
- Keep GNUmakefile canonical.
- Make `libtrealla.a` the primary cross-build artifact and keep board startup
  outside the engine.
- Use positive feature macros and validate their dependencies.
- Omit predicates for omitted features rather than registering traps.
- Require embedded `builtins` and prohibit filesystem fallback in a
  freestanding build.
- Start with an internal link-time platform contract; publish an ABI only after
  a second adapter validates it.
- Treat allocation as a dedicated phase because Trealla's global state makes a
  casual per-instance callback incorrect.
