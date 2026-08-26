#!/usr/bin/env python3

import subprocess
import sys


MARKERS = (
    "TREALLA FREESTANDING BOOT",
    "TREALLA PROLOG OK",
    "TREALLA ALLOCATION FAILURE CONTROLLED",
    "TREALLA HEAP PEAK ",
    "TREALLA FREESTANDING COMPLETE",
)


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: qemu_smoke.py <qemu-system-riscv32> <firmware.elf>", file=sys.stderr)
        return 2

    command = [
        sys.argv[1],
        "-machine", "virt,accel=tcg",
        "-cpu", "rv32",
        "-m", "128M",
        "-bios", "none",
        "-semihosting-config", "enable=on",
        "-nographic",
        "-monitor", "none",
        "-kernel", sys.argv[2],
    ]

    try:
        result = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            timeout=20,
            check=False,
        )
    except subprocess.TimeoutExpired as error:
        output = error.stdout or ""
        print(output, end="")
        print("QEMU smoke timed out", file=sys.stderr)
        return 1

    print(result.stdout, end="")
    position = 0

    for marker in MARKERS:
        found = result.stdout.find(marker, position)

        if found < 0:
            print(f"QEMU smoke missing marker: {marker}", file=sys.stderr)
            return 1

        position = found + len(marker)

    if result.returncode != 0:
        print(f"QEMU exited with status {result.returncode}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
