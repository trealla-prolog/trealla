#!/usr/bin/env python3
"""Check the ESP32-S3 freestanding image's stable layout invariants."""

import argparse
import re
import sys
from pathlib import Path


MAX_FIRMWARE_BYTES = 1_500_000
MAX_INTERNAL_DRAM_BYTES = 270_000

SECTION_RE = re.compile(
    r"^\.(?P<name>\S+)\s+0x[0-9a-fA-F]+\s+0x(?P<size>[0-9a-fA-F]+)\s*$",
    re.MULTILINE,
)


def sections(map_text):
    matches = list(SECTION_RE.finditer(map_text))
    result = {}

    for idx, match in enumerate(matches):
        end = matches[idx + 1].start() if idx + 1 < len(matches) else len(map_text)
        result[match.group("name")] = (
            int(match.group("size"), 16),
            map_text[match.end():end],
        )

    return result


def fail(message):
    print(f"ESP32-S3 layout check failed: {message}", file=sys.stderr)
    return 1


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("map", type=Path)
    parser.add_argument("firmware", type=Path)
    args = parser.parse_args()

    try:
        map_text = args.map.read_text(encoding="utf-8", errors="replace")
        firmware_size = args.firmware.stat().st_size
    except OSError as exc:
        return fail(str(exc))

    parsed = sections(map_text)
    required = ("ext_ram.bss", "dram0.data", "dram0.bss")
    missing = [name for name in required if name not in parsed]

    if missing:
        return fail("missing map section(s): " + ", ".join(missing))

    psram_size, psram_body = parsed["ext_ram.bss"]
    dram_data_size, _ = parsed["dram0.data"]
    dram_bss_size, dram_bss_body = parsed["dram0.bss"]
    internal_dram_size = dram_data_size + dram_bss_size

    if "libtrealla.a(" not in psram_body:
        return fail("Trealla BSS is not present in .ext_ram.bss")

    if "*libtrealla.a:(.bss .bss.*)" not in psram_body:
        return fail("the linker no longer selects all Trealla BSS for PSRAM")

    if "libtrealla.a(" in dram_bss_body:
        return fail("Trealla BSS leaked into internal DRAM")

    if internal_dram_size > MAX_INTERNAL_DRAM_BYTES:
        return fail(
            f"static internal DRAM budget exceeded: {internal_dram_size:,} > "
            f"{MAX_INTERNAL_DRAM_BYTES:,} bytes"
        )

    if firmware_size > MAX_FIRMWARE_BYTES:
        return fail(
            f"firmware budget exceeded: {firmware_size:,} > "
            f"{MAX_FIRMWARE_BYTES:,} bytes"
        )

    print(f"firmware:             {firmware_size:>10,} / {MAX_FIRMWARE_BYTES:,} bytes")
    print(f"Trealla PSRAM BSS:    {psram_size:>10,} bytes")
    print(f"static internal RAM: {internal_dram_size:>10,} / {MAX_INTERNAL_DRAM_BYTES:,} bytes")
    print("Trealla BSS placement: PSRAM only")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
