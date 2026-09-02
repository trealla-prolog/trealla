# Raspberry Pi 4 adapter

This is the third freestanding target and the first that boots a 64-bit
application processor with no operating system underneath it. It targets the
Pi 4's BCM2711 (four Cortex-A72 cores, low-peripheral mode) and uses the Arm
GNU bare-metal toolchain and newlib for the small C-runtime layer.

Unlike the RV32 and ESP32-S3 targets, nothing here is memory constrained: the
smallest Pi 4 has 1 GB and Trealla's live-heap peak is under 6 MB. What the
target does need, and what the two earlier ports got from someone else, is
AArch64 startup — so `boot.S` and `mmu.c` are the substance of this adapter and
`platform.c` is the small part.

## Building

Install the Arm GNU toolchain for `aarch64-none-elf` (on macOS,
`brew install --cask gcc-aarch64-embedded`, or unpack the tarball from
developer.arm.com), put its `bin` on `PATH`, then:

```
make rpi4
```

That produces `ports/rpi4/trealla.elf` and the flashable
`ports/rpi4/kernel8.img`. Override `RPI4_CC`, `RPI4_AR`, `RPI4_OBJCOPY` and
`RPI4_SIZE` if the toolchain is not on `PATH` under its usual names.

To boot the same image under QEMU and check it against the acceptance markers:

```
make rpi4-smoke
```

## Running on hardware

Copy `kernel8.img` to the boot partition of an otherwise ordinary Raspberry Pi
OS card, alongside a `config.txt` containing:

```
arm_64bit=1
kernel=kernel8.img
enable_uart=1
device_tree_address=0x20000000
```

The console is PL011 UART0 on GPIO14/15 at 115200 8N1, which is where a USB
serial adapter on the 40-pin header lands. `device_tree_address` matters: the
port ignores the device tree, but the linker script hands everything from the
end of BSS up to 0x20000000 to the Trealla heap, and firmware left to itself
may place the tree inside that range.

## What the port owns

| File | Role |
| --- | --- |
| `boot.S` | Parks cores 1-3, drops EL3/EL2 to EL1, enables FP/SIMD, sets the stack, zeroes BSS |
| `mmu.c` | Identity-maps RAM as Normal write-back and the peripheral window as Device, then enables the MMU and caches |
| `platform.c` | The five platform services: PL011 console, generic-timer clock, halt, panic |
| `syscalls.c` | Newlib's bottom half — console `_read`/`_write` and a bump `_sbrk` over the linker-defined heap |
| `rpi4.ld` | Image at 0x80000, 8 MiB stack, heap up to 0x20000000 |

The MMU is not an optimisation. With it off, every access is Device-nGnRnE:
unaligned accesses fault whatever `SCTLR_EL1.A` says, and nothing is cached.

`halt` parks the core in `wfe` after draining the UART. The smoke build adds
`-DRPI4_SEMIHOSTING=1`, which exits through a semihosting call first so QEMU
can report the status the way the RV32 target's test finisher does; on hardware
with no debugger attached that call is not serviced, which is why it is not in
the image you flash.

A Pi 4 has no battery-backed clock. `_gettimeofday` therefore reports time
since boot, taken from the same monotonic counter as the platform clock — it is
honestly monotonic and honestly not wall time.

## Acceptance

The smoke runner requires these markers, in order, under a timeout:

```
TREALLA FREESTANDING BOOT
TREALLA PROLOG OK
TREALLA ALLOCATION FAILURE CONTROLLED
TREALLA HEAP PEAK <bytes>
TREALLA FREESTANDING COMPLETE
```

The first measured AArch64 baseline (Arm GNU Toolchain 15.2.rel1, newlib) is:

| Metric | Baseline bytes | CI limit |
| --- | ---: | ---: |
| ELF text | 1,412,584 | 1,750,000 |
| ELF data | 400,824 | 500,000 |
| ELF bss | 697,048 | 900,000 |
| Peak Trealla-owned heap | 5,795,037 | 7,000,000 |

Text and data run larger than the RV32 baseline and BSS runs smaller, which is
what 64-bit pointers and a different libc do to the same engine. As with the
other targets, a change that intentionally exceeds a limit must move the
baseline and the limit together, with a reason.

The image has been built and booted under QEMU's `raspi4b` machine, which is
what CI repeats. It has not yet been run on a physical Raspberry Pi 4; the
peripheral base, PL011 wiring and generic-timer frequency are the parts QEMU
models faithfully enough to be worth trusting, and the GPIO alt-function
setup, baud divisors and `config.txt` contract above are the parts that only
hardware can confirm.
