# Porting freestanding Trealla

This guide is the contract for embedding Trealla in a runtime without an
operating system. A port should add target files under `ports/` and select them
through the make variables below. It must not add board conditionals to the
engine.

## What Trealla supplies

`FREESTANDING=1` removes filesystem, process, network, terminal, FFI, TLS and
thread dependencies. It embeds the selected Prolog libraries and application
source, and exposes a small link-time platform contract in
`src/platform/platform.h`.

The contract is internal rather than stable public ABI. Its five services are:

| Service | Requirement |
| --- | --- |
| console read | Return the number of bytes placed in the caller's buffer. It may block. |
| console write | Write up to `len` bytes; output and error may share a device. |
| monotonic clock | Return nondecreasing microseconds. It need not be wall time. |
| halt | Stop or reset the application and never return. |
| panic | Report a fatal platform failure and never return. |

The reusable adapter in `ports/template/platform.c` supplies these symbols and
delegates the hardware work to the five board functions in
`ports/template/board.h`. A board may use that adapter or implement the Trealla
contract directly, as `ports/qemu-riscv32/platform.c` does.

## C runtime and toolchain

The target needs a C99 compiler, linker and archiver. A compiler without the
GCC/Clang `noreturn` attribute must provide the C11 `_Noreturn` spelling. The
build also needs a native `HOST_CC`: the native tools convert libraries and
`PROGRAM` into length-delimited C byte arrays, while the target compiler builds
Trealla.

Trealla still expects the ordinary non-I/O portion of a small C runtime:

- fixed-width integer and size types;
- `memcpy`, `memmove`, `memset` and `memcmp`;
- basic string and numeric conversion routines;
- the floating-point and integer math used by Prolog arithmetic; and
- an allocator, either the runtime default or one installed with
  `pl_set_allocator()` before `pl_create()`.

The exact undefined-symbol list depends on compiler builtins, selected Prolog
libraries and arithmetic configuration. Inspect the final image rather than
copying another target's list. Picolibc is used by the RV32 reference, but it is
not part of the platform contract.

The allocator callbacks and their context are runtime-wide and must outlive all
Trealla allocations. Use `pl_get_allocator_stats()` to establish a realistic
heap baseline, then inject a deterministic failure before trusting the port.
Strings returned by `pl_term_text()` and `pl_int_text()` must be released with
`pl_free()`.

## Exposing board hardware to Prolog

A port that wants board services callable from Prolog supplies a builtin table
rather than editing the engine. Define `g_port_bifs` in a port object and
select it with `PORT_BIFS_OBJECT`; a build that does not gets the empty table
in `src/port_bifs_none.c`. The engine walks one more table and stays free of
board conditionals, the same way `BIF_OS_OBJECT` and `NETWORK_OBJECT` are
chosen. `ports/rpi4/bif_gpio.c` is the worked example.

One trap when writing those builtins: `throw_error()` returns **true** when a
`catch/3` handler accepted the ball, so its result is the value the builtin
owes the engine, not a "did the check pass" flag. A helper that reports errors
must hand that value back to its caller unchanged; treating it as a boolean
success silently continues past a caught error.

## Port checklist

1. Choose the target ABI, startup code, memory map, stack and C runtime.
2. Copy `ports/template/`, replace `hosted-board.c`, and keep hardware addresses
   and interrupt details inside the new port directory.
3. Make console writes binary-safe. Do not rely on a terminating NUL, and
   either distinguish output/error or deliberately merge them.
4. Extend a narrow hardware timer into 64 bits if necessary. Reads must never
   go backwards, including across counter rollover.
5. Make halt and panic visibly terminal. Firmware should report failure to its
   supervisor, watchdog, test finisher or reset path.
6. Select only the required embedded libraries with `EMBED_LIBS`; `builtins` is
   mandatory and added automatically.
7. Build with the target compiler but retain a native `HOST_CC`.
8. Boot a deterministic smoke program and validate structured bindings through
   the embedding API, not by parsing toplevel text.
9. Check the final undefined symbols for accidental OS services and inspect the
   linked memory map for overlaps.
10. Record text, data, bss, stack and peak Trealla-owned heap before setting
    budgets.
11. Force allocation failure and confirm `resource_error(memory)` is caught and
    subsequent queries still work.
12. Run the normal hosted regression suite before submitting the port.

## Build shape

The essential invocation is:

```
make FREESTANDING=1 NOPIC=1 \
  CC=<target-cc> AR=<target-ar> HOST_CC=<native-cc> \
  PLATFORM_OBJ='<adapter objects>' \
  TARGET_CFLAGS='<ABI and target flags>' \
  LDFLAGS='<linker script, runtime and math library>' \
  PROGRAM=<application.pl> EMBED_LIBS='<libraries>' \
  samples/freestanding
```

`PROGRAM` is converted on the build host and consulted from bytes at runtime.
Applications can load additional flash or serial-delivered source with
`pl_consult_text()`; no temporary filesystem is involved.

`PROGRAM` says what to embed; `FREESTANDING_MAIN` says what runs it.
`samples/freestanding.c` is the acceptance harness and drives fixed queries,
while `samples/freestanding_app.c` simply consults the program and lets its
`initialization/1` goal run, which is what an application image wants.

## Acceptance test

The reference smoke application demonstrates the minimum useful test: boot,
consult embedded rules, run successful and failing queries, inspect a binding,
exercise the clock, force a controlled allocation failure, report peak heap and
halt with a machine-visible status. `util/qemu_smoke.py` enforces its marker
order and timeout for the RV32 image.

A new port is ready when it can perform the same sequence without modifying
`src/`, and its normal and failure paths are repeatable after a clean build.

The first physical-board application of this checklist is under
`ports/arduino-nano-esp32/`. It deliberately places both Trealla's static BSS
and owned heap in the board's directly mapped PSRAM, leaving internal SRAM for
the ESP-IDF runtime and task stack.

`ports/rpi4/` applies the same checklist to a 64-bit application processor with
nothing underneath it. Where the two earlier targets inherit startup from
Picolibc's crt0 and from ESP-IDF, this one has to supply it: parking the spare
cores, dropping to EL1, enabling FP/SIMD, and turning on the MMU before any
library code runs. That last step is a correctness requirement, not a
performance one. Until an AArch64 target has page tables, every access is
Device memory, where unaligned accesses fault whatever the alignment-check bit
says. A port to a similar core should expect startup, not the platform
contract, to be the bulk of the work.
