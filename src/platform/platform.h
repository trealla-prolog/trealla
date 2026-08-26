#pragma once

#include <stddef.h>
#include <stdint.h>

// Internal link-time service contract for an OS-free Trealla image. A port
// supplies exactly one adapter object. This is intentionally not public ABI:
// it will remain internal until a second materially different target has
// validated the shape of the interface.

enum tpl_console_channel {
	TPL_CONSOLE_OUTPUT,
	TPL_CONSOLE_ERROR
};

size_t tpl_platform_console_read(void *buf, size_t len);
size_t tpl_platform_console_write(enum tpl_console_channel channel,
	const void *buf, size_t len);
uint64_t tpl_platform_monotonic_usec(void);

#if defined(__GNUC__) || defined(__clang__)
#define TPL_NORETURN __attribute__((noreturn))
#else
#define TPL_NORETURN _Noreturn
#endif

TPL_NORETURN void tpl_platform_halt(int status);
TPL_NORETURN void tpl_platform_panic(const char *message);
