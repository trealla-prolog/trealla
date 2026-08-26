#include <stddef.h>
#include <stdint.h>

#include "platform/platform.h"

// QEMU's RISC-V virt machine exposes an ns16550-compatible UART, an mtime
// counter and a SiFive test-finisher device at these fixed physical addresses.

#define UART_BASE 0x10000000u
#define UART_RX_TX (*(volatile uint8_t*)(uintptr_t)(UART_BASE + 0))
#define UART_LSR (*(volatile uint8_t*)(uintptr_t)(UART_BASE + 5))
#define UART_DATA_READY 0x01u
#define UART_TX_IDLE 0x20u

#define MTIME_LOW (*(volatile uint32_t*)(uintptr_t)0x0200bff8u)
#define MTIME_HIGH (*(volatile uint32_t*)(uintptr_t)0x0200bffcu)
#define TEST_FINISHER (*(volatile uint32_t*)(uintptr_t)0x00100000u)

static void uart_putc(uint8_t ch)
{
	while (!(UART_LSR & UART_TX_IDLE))
		;

	UART_RX_TX = ch;
}

size_t tpl_platform_console_read(void *buf, size_t len)
{
	uint8_t *dst = buf;

	for (size_t i = 0; i < len; i++) {
		while (!(UART_LSR & UART_DATA_READY))
			;

		dst[i] = UART_RX_TX;
	}

	return len;
}

size_t tpl_platform_console_write(enum tpl_console_channel channel,
	const void *buf, size_t len)
{
	(void)channel;
	const uint8_t *src = buf;

	for (size_t i = 0; i < len; i++)
		uart_putc(src[i]);

	return len;
}

uint64_t tpl_platform_monotonic_usec(void)
{
	uint32_t high1, low, high2;

	do {
		high1 = MTIME_HIGH;
		low = MTIME_LOW;
		high2 = MTIME_HIGH;
	} while (high1 != high2);

	// The virt machine's timebase is 10 MHz.
	return (((uint64_t)high1 << 32) | low) / 10u;
}

void tpl_platform_halt(int status)
{
	TEST_FINISHER = status ? 0x3333u : 0x5555u;

	for (;;)
		__asm__ volatile("wfi");
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
	tpl_platform_halt(1);
}
