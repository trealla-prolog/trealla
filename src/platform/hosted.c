#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "platform.h"

size_t tpl_platform_console_read(void *buf, size_t len)
{
	return fread(buf, 1, len, stdin);
}

size_t tpl_platform_console_write(enum tpl_console_channel channel,
	const void *buf, size_t len)
{
	FILE *fp = channel == TPL_CONSOLE_ERROR ? stderr : stdout;
	size_t written = fwrite(buf, 1, len, fp);
	fflush(fp);
	return written;
}

uint64_t tpl_platform_monotonic_usec(void)
{
	struct timespec now;

	if (clock_gettime(CLOCK_MONOTONIC, &now))
		tpl_platform_panic("monotonic clock failed");

	return (uint64_t)now.tv_sec * 1000000u + (uint64_t)now.tv_nsec / 1000u;
}

void tpl_platform_halt(int status)
{
	exit(status);
}

void tpl_platform_panic(const char *message)
{
	static const char prefix[] = "TREALLA PLATFORM PANIC: ";
	const char *end = message;

	while (*end)
		end++;

	tpl_platform_console_write(TPL_CONSOLE_ERROR, prefix, sizeof(prefix) - 1);
	tpl_platform_console_write(TPL_CONSOLE_ERROR, message, (size_t)(end - message));
	tpl_platform_console_write(TPL_CONSOLE_ERROR, "\n", 1);
	abort();
}
