# Arduino Nano ESP32 adapter

This is the second freestanding target and the first physical-board adapter.
It uses the Nano ESP32's ESP32-S3, 16 MB flash, 8 MB octal PSRAM and native USB
serial/JTAG interface. ESP-IDF supplies startup, the Xtensa toolchain and the
small runtime layer; filesystem, network, FFI, TLS and Trealla threads remain
disabled.

Trealla's measured RV32 live-heap peak is about 5.45 MB and its static BSS is
about 1.4 MB. The adapter therefore:

- routes every Trealla-owned dynamic allocation explicitly to PSRAM;
- maps `libtrealla.a` BSS to PSRAM with ESP-IDF's `extram_bss` scheme;
- reserves internal RAM for the ESP-IDF runtime and the 32 KB main-task stack;
- embeds the Prolog smoke program in flash; and
- repeats the structured-query and controlled-allocation-failure checks used
  by the QEMU firmware.

Install and activate ESP-IDF 6.0.2 or newer, then build with:

```
source ~/.espressif/tools/activate_idf_v6.0.2.sh
make arduino-nano-esp32
```

The reference build produces a 1,286,609-byte image (1,286,720-byte padded
binary). It places 375,792 bytes of static Trealla BSS in external RAM and
leaves 53,751 bytes of linker-visible internal DIRAM free. Dynamic Trealla
allocations are not included in that static figure; the adapter routes them to
the capability-managed PSRAM heap at run time.

Flash and monitor the connected board with:

```
cd ports/arduino-nano-esp32
idf.py -p /dev/cu.<board-port> flash monitor
```

The serial output should finish with `TREALLA NANO ESP32 COMPLETE` and report
the initial/final PSRAM availability, controlled allocation failure and peak
Trealla-owned heap. Press Ctrl-] to leave the ESP-IDF monitor.

The original ESP32-WROVER-E also has 8 MB PSRAM in current R8 variants, but the
original ESP32 can expose only 4 MB through its ordinary addressable heap. Its
upper PSRAM requires the windowed `himem` API, so it is not a drop-in target for
Trealla's present allocator and live-heap profile.

The firmware has been cross-built and its linker map checked. Board boot,
serial output and the run-time heap figures still require validation on a
connected Nano ESP32.
