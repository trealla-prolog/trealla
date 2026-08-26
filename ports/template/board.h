#pragma once

#include <stddef.h>
#include <stdint.h>

// Small board-facing interface used by the reusable template adapter. A port
// implements these functions without changing Trealla engine sources.

enum trealla_board_channel {
	TREALLA_BOARD_OUTPUT,
	TREALLA_BOARD_ERROR
};

size_t trealla_board_console_read(void *buf, size_t len);
size_t trealla_board_console_write(enum trealla_board_channel channel,
	const void *buf, size_t len);
uint64_t trealla_board_monotonic_usec(void);

#if defined(__GNUC__) || defined(__clang__)
#define TREALLA_BOARD_NORETURN __attribute__((noreturn))
#else
#define TREALLA_BOARD_NORETURN _Noreturn
#endif

TREALLA_BOARD_NORETURN void trealla_board_halt(int status);
