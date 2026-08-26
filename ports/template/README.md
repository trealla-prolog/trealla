# Generic board adapter template

`platform.c` is a reusable bridge from Trealla's freestanding service contract
to the five operations declared in `board.h`. Copy this directory for a new
target and replace `hosted-board.c` with the board's console, monotonic timer
and halt implementation. Trealla engine sources do not need to change.

The hosted implementation exists only to keep the template executable and
covered by tests:

```
make port-template-smoke
```

For a real target, pass both adapter objects when building the freestanding
application:

```
make FREESTANDING=1 NOPIC=1 \
  CC=<target-cc> AR=<target-ar> HOST_CC=<host-cc> \
  PLATFORM_OBJ='ports/my-board/platform.o ports/my-board/board.o' \
  TARGET_CFLAGS='<target compile flags>' \
  LDFLAGS='<target link flags and runtime libraries>' \
  samples/freestanding
```

See `docs/freestanding-porting.md` for the complete contract and checklist.
