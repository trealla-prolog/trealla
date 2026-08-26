#!/usr/bin/env python3

import re
import subprocess
import sys


MARKERS = (
    "TREALLA FREESTANDING BOOT",
    "TREALLA PROLOG OK",
    "TREALLA ALLOCATION FAILURE CONTROLLED",
    "TREALLA HEAP PEAK ",
    "TREALLA FREESTANDING COMPLETE",
)

MAX_TEXT_BYTES = 1_250_000
MAX_DATA_BYTES = 300_000
MAX_BSS_BYTES = 1_600_000
MAX_HEAP_BYTES = 6_500_000


def image_sizes(size_output: str):
    for line in size_output.splitlines():
        fields = line.split()

        if len(fields) >= 6 and all(field.isdigit() for field in fields[:4]):
            return tuple(int(field) for field in fields[:3])

    return None


def heap_peak(qemu_output: str):
    match = re.search(r"TREALLA HEAP PEAK ([0-9]+)", qemu_output)
    return int(match.group(1)) if match else None


def main() -> int:
    if len(sys.argv) != 4:
        print(
            "usage: qemu_smoke.py <qemu-system-riscv32> <firmware.elf> <size-tool>",
            file=sys.stderr,
        )
        return 2

    size_result = subprocess.run(
        [sys.argv[3], sys.argv[2]],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        check=False,
    )
    print(size_result.stdout, end="")
    sizes = image_sizes(size_result.stdout)

    if size_result.returncode != 0 or sizes is None:
        print("could not read QEMU firmware sizes", file=sys.stderr)
        return 1

    text_size, data_size, bss_size = sizes
    limits = (
        ("text", text_size, MAX_TEXT_BYTES),
        ("data", data_size, MAX_DATA_BYTES),
        ("bss", bss_size, MAX_BSS_BYTES),
    )

    for name, actual, maximum in limits:
        if actual > maximum:
            print(
                f"QEMU firmware {name} budget exceeded: {actual:,} > {maximum:,} bytes",
                file=sys.stderr,
            )
            return 1

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

    heap_size = heap_peak(result.stdout)

    if heap_size is None:
        print("QEMU smoke did not report a numeric heap peak", file=sys.stderr)
        return 1

    if heap_size > MAX_HEAP_BYTES:
        print(
            f"QEMU heap budget exceeded: {heap_size:,} > {MAX_HEAP_BYTES:,} bytes",
            file=sys.stderr,
        )
        return 1

    if result.returncode != 0:
        print(f"QEMU exited with status {result.returncode}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
