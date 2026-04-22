# CMake Build Reference

## Presets

| Preset | Target | Libc | Build type |
|---|---|---|---|
| `linux-debug` | x86-64 Linux | system glibc | Debug |
| `linux-release` | x86-64 Linux | system glibc | Release |
| `linux-minsizerel` | x86-64 Linux | system glibc | MinSizeRel |
| `arm-none-eabi-nolibc-generic` | ARM Cortex-M | none | Debug |
| `arm-none-eabi-picolibc-generic` | ARM Cortex-M | picolibc | Debug |
| `qemu-mps3-an524-picolibc-debug` | ARM Cortex-M33 (QEMU) | picolibc | Debug |
| `qemu-mps3-an524-picolibc-release` | ARM Cortex-M33 (QEMU) | picolibc | Release |

---

## CMake Variables

### Runtime variables (auto-set, rarely overridden)

| Variable | Values | Set by | Description |
|---|---|---|---|
| `TPL_BACKEND` | `linux` \| `baremetal` | auto (from `CMAKE_SYSTEM_NAME`) | Selects platform source files and compile definitions. `linux` when `CMAKE_SYSTEM_NAME=Linux`, `baremetal` when `Generic`. |
| `TPL_LIBC_BACKEND` | `system` \| `none` \| `picolibc` | toolchain file (FORCE) | Selects the C standard library. `system` = host glibc, `none` = no libc (compat stubs only), `picolibc` = built from submodule. |

### Board / hardware variables (ARM targets only)

| Variable | Required | Example values | Description |
|---|---|---|---|
| `TPL_BAREMETAL_CPU` | yes | `cortex-m0`, `cortex-m3`, `cortex-m4`, `cortex-m7` | Passed as `-mcpu=`. |
| `TPL_BAREMETAL_FPU` | no | `fpv4-sp-d16`, `fpv5-d16` | Passed as `-mfpu=`. Requires `TPL_BAREMETAL_FLOAT_ABI`. |
| `TPL_BAREMETAL_FLOAT_ABI` | no | `soft`, `softfp`, `hard` | Passed as `-mfloat-abi=`. |
| `TPL_LINKER_SCRIPT` | for linking | path to `.ld` file | Passed as `-T<path>`. Omitting it gives a warning and produces a compile-only build. |
| `TPL_ENTRY` | for linking | path to `.S` or `.c` | Startup source added to the build. For picolibc targets must call `__libc_init_array()` before `main`. |

### Feature flags

| Variable | Default | Description |
|---|---|---|
| `TPL_MEMORY_LOGGING` | `OFF` | Enable memory allocation counters and stats output. Adds `-DTPL_MEMORY_LOGGING`. |
| `TPL_LTO` | `OFF` | Enable link-time optimization (IPO). Checked via `check_ipo_supported`. |
| `TPL_EXTRA_CFLAGS` | `""` | Extra flags appended to `target_compile_options`. Use for `-I/path/to/cmsis` etc. |
| `TPL_EXTRA_LDFLAGS` | `""` | Extra flags appended to `target_link_options`. |

### Developer variables

| Variable | Default | Description |
|---|---|---|
| `COMPILE_DIAGNOSTICS_FORMAT` | `text` | Passed as `-fdiagnostics-format=`. Use `json-file` when running `just report`. |
| `CMAKE_BUILD_TYPE` | `Release` (warned if unset) | Standard CMake build type. |
| `CMAKE_EXPORT_COMPILE_COMMANDS` | `ON` (set by all presets) | Generates `compile_commands.json` for IDE/clangd. |

---

## Flags Reference

### Common compile flags (all targets)

```
-Wall -Wextra
-Wno-unused-but-set-variable -Wno-unused-parameter -Wno-unused-variable
-fmacro-prefix-map=<source_root>/=          # strip absolute paths from __FILE__
-fdiagnostics-format=<text|json-file>
-fdiagnostics-color=auto
```

### Common compile definitions (all targets)

```
-D_GNU_SOURCE
-DVERSION="<git-describe>"
-DBACKEND="<linux|baremetal>"
```

---

### Linux x86-64 (`TPL_BACKEND=linux`, `TPL_LIBC_BACKEND=system`)

**Additional compile flags:** *(none beyond the arch defaults)*

**Additional definitions:**
```
-DTPL_BACKEND_LINUX
```

**Include paths:**
```
src/
src/platform/
src/platform/api/
build/<preset>/generated/       # generated library .c files (xxd from .pl)
```

**Link libraries:**
```
-lm
-lreadline
```

**Linker flags:** *(none beyond defaults)*

---

### arm-none-eabi no-libc (`TPL_BACKEND=baremetal`, `TPL_LIBC_BACKEND=none`)

**Compiler (from toolchain):** `arm-none-eabi-gcc`

**Arch flags (from toolchain `CMAKE_C_FLAGS_INIT`):**
```
-mcpu=<TPL_BAREMETAL_CPU>  -mthumb  -ffreestanding
[-mfpu=<TPL_BAREMETAL_FPU>  -mfloat-abi=<TPL_BAREMETAL_FLOAT_ABI>]
```

**Additional definitions:**
```
-DTPL_BACKEND_BAREMETAL
```

**Include paths:**
```
src/
src/platform/
src/platform/api/
compat/                         # stub headers (stdint.h, stdio.h, etc.)
```

**Linker flags (from toolchain `CMAKE_EXE_LINKER_FLAGS_INIT`):**
```
--specs=nosys.specs  -nostartfiles  -nostdlib
```

**Additional linker flags (from CMakeLists.txt):**
```
-Wl,-Map=tpl.map
-T<TPL_LINKER_SCRIPT>           # if set
```

**Link libraries:** *(none)*

---

### arm-none-eabi picolibc (`TPL_BACKEND=baremetal`, `TPL_LIBC_BACKEND=picolibc`)

**Compiler (from toolchain):** `arm-none-eabi-gcc`

**Arch flags (from toolchain `CMAKE_C_FLAGS_INIT`):**
```
-mcpu=<TPL_BAREMETAL_CPU>  -mthumb
[-mfpu=<TPL_BAREMETAL_FPU>  -mfloat-abi=<TPL_BAREMETAL_FLOAT_ABI>]
```
*(no `-ffreestanding` — picolibc provides hosted headers)*

**Additional definitions:**
```
-DTPL_BACKEND_BAREMETAL
```

**Include paths:**
```
src/
src/platform/
src/platform/api/
build/<preset>/_picolibc/install/include/   # picolibc headers (built at compile time)
```

**Linker flags (from toolchain `CMAKE_EXE_LINKER_FLAGS_INIT`):**
```
-nostartfiles
```
*(no `-nostdlib` — picolibc is linked explicitly)*

**Additional linker flags (from CMakeLists.txt):**
```
-Wl,-Map=tpl.map
-T<TPL_LINKER_SCRIPT>           # if set
```

**Link libraries:**
```
build/<preset>/_picolibc/install/lib/libc.a     # built by ExternalProject
```

---

## Inspecting the Build Dynamically

These are most useful when debugging unexpected flags or missing includes.

### Show all configured cache variables

```sh
just vars linux-debug
# or directly:
cmake -LH -N build/linux-debug
```

Prints every cache variable with its help text. Quick way to confirm `TPL_LIBC_BACKEND`, `TPL_BAREMETAL_CPU`, etc. were picked up correctly.

### Show exact compiler and linker invocations

```sh
just flags linux-debug
# or:
cmake --build --preset linux-debug --parallel -- VERBOSE=1
```

Prints the full `arm-none-eabi-gcc` command line for every translation unit, including all `-D`, `-I`, and `-f` flags that actually reached the compiler. Most reliable way to verify the flags table above.

### Browse compile commands in your editor / clangd

All presets set `CMAKE_EXPORT_COMPILE_COMMANDS=ON`, so `compile_commands.json` is always generated after configure. Point clangd at it with:

A symlink is created automatically at `compile_commands.json` in the repo root when you run `cmake --preset` (or `just configure`). Point clangd at it with `"compilationDatabasePath": "."` in your editor config.

### Interactive cache editor

```sh
ccmake build/linux-debug
```

Full-screen TUI that lets you toggle options and reconfigure. Useful for exploring what options exist without reading the source.
