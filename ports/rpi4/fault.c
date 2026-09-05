#include <stdint.h>

#include "platform/platform.h"

// What happens when something goes wrong on a board with no debugger.
//
// Until this existed, any fault - a stray pointer, an alignment error, a read
// of a device that is not fitted - left VBAR_EL1 unset, so the CPU jumped to
// an undefined vector and the board simply stopped. No output, no clue. That
// is the worst failure mode a bare-metal image can have, and it cost an
// afternoon before it was noticed.

static void put(const char *s)
{
	const char *e = s;

	while (*e)
		e++;

	tpl_platform_console_write(TPL_CONSOLE_ERROR, s, (size_t)(e - s));
}

static void put_hex(uint64_t v)
{
	static const char digits[] = "0123456789abcdef";
	char text[19] = "0x";

	for (unsigned i = 0; i < 16; i++)
		text[2 + i] = digits[(v >> ((15 - i) * 4)) & 0xf];

	text[18] = '\0';
	put(text);
}

// The exception class is the first thing to look at: 0x25 is a data abort
// from the same EL, which on this port usually means a device that is not
// there or a pointer that is not mapped.

static const char *class_name(uint64_t ec)
{
	switch (ec) {
	case 0x15: return " (SVC)";
	case 0x21: return " (instruction abort)";
	case 0x22: return " (PC alignment)";
	case 0x25: return " (data abort)";
	case 0x26: return " (SP alignment)";
	case 0x2c: return " (floating point)";
	default: return "";
	}
}

void rpi4_fault(uint64_t esr, uint64_t far, uint64_t elr, uint64_t entry)
{
	uint64_t ec = (esr >> 26) & 0x3f;

	put("\nTREALLA EXCEPTION entry=");
	put_hex(entry);
	put(" esr=");
	put_hex(esr);
	put(class_name(ec));
	put(" far=");
	put_hex(far);
	put(" elr=");
	put_hex(elr);
	put("\n");

	tpl_platform_halt(1);
}
