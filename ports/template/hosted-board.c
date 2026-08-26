// Executable validation harness for the port template. Replace this file with
// board-specific UART, timer and halt operations in a real port.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "board.h"

size_t trealla_board_console_read(void *buf, size_t len)
{
	return fread(buf, 1, len, stdin);
}

size_t trealla_board_console_write(enum trealla_board_channel channel,
	const void *buf, size_t len)
{
	FILE *stream = channel == TREALLA_BOARD_ERROR ? stderr : stdout;
	size_t written = fwrite(buf, 1, len, stream);
	fflush(stream);
	return written;
}

uint64_t trealla_board_monotonic_usec(void)
{
	struct timespec now;

	if (clock_gettime(CLOCK_MONOTONIC, &now))
		abort();

	return (uint64_t)now.tv_sec * 1000000u + (uint64_t)now.tv_nsec / 1000u;
}

void trealla_board_halt(int status)
{
	exit(status);
}
