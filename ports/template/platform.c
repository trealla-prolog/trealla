#include <stddef.h>
#include <stdint.h>

#include "board.h"
#include "platform/platform.h"

size_t tpl_platform_console_read(void *buf, size_t len)
{
	return trealla_board_console_read(buf, len);
}

size_t tpl_platform_console_write(enum tpl_console_channel channel,
	const void *buf, size_t len)
{
	enum trealla_board_channel board_channel = channel == TPL_CONSOLE_ERROR
		? TREALLA_BOARD_ERROR : TREALLA_BOARD_OUTPUT;
	return trealla_board_console_write(board_channel, buf, len);
}

uint64_t tpl_platform_monotonic_usec(void)
{
	return trealla_board_monotonic_usec();
}

void tpl_platform_halt(int status)
{
	trealla_board_halt(status);
}

void tpl_platform_panic(const char *message)
{
	static const char prefix[] = "TREALLA PLATFORM PANIC: ";
	const char *end = message;

	while (*end)
		end++;

	trealla_board_console_write(TREALLA_BOARD_ERROR,
		prefix, sizeof(prefix) - 1);
	trealla_board_console_write(TREALLA_BOARD_ERROR,
		message, (size_t)(end - message));
	trealla_board_console_write(TREALLA_BOARD_ERROR, "\n", 1);
	trealla_board_halt(1);
}
