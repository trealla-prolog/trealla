# QEMU RV32 reference adapter

This is the first non-hosted consumer of Trealla's internal platform-service
contract. It targets QEMU's `virt` machine with a 32-bit RISC-V CPU and uses
Picolibc for the small C-runtime layer which remains in scope for the initial
freestanding work.

On Ubuntu 24.04, install the reference tools with:

```
sudo apt-get install gcc-riscv64-unknown-elf \
    picolibc-riscv64-unknown-elf qemu-system-misc
```

Then build and boot the deterministic smoke image:

```
make qemu-riscv32-smoke
```

The build uses Picolibc's startup object and generic linker layout. The local
linker configuration places that runtime and the Trealla image in QEMU RAM.
`platform.c` owns the only target-specific engine services: UART input/output,
the monotonic counter, panic and the test-finisher halt operation.

The smoke runner requires these markers, in order, and applies a strict
timeout:

```
TREALLA FREESTANDING BOOT
TREALLA PROLOG OK
TREALLA FREESTANDING COMPLETE
```
