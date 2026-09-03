#include <stdint.h>
#include <stdlib.h>

#include "trealla.h"
#include "platform/platform.h"

// Generic entry point for a freestanding image whose job is to run one
// embedded Prolog program - the bare-metal equivalent of `make compile
// main=...`. samples/freestanding.c is the acceptance harness instead, and
// drives fixed queries; this one just boots the program and gets out of the
// way.
//
// PROGRAM is converted to bytes on the build host and consulted from memory,
// so any `:- initialization(main).` in it runs at the end of the load, the
// same way it does in a hosted standalone build.

extern unsigned char program_pl[];
extern unsigned int program_pl_len;

int main(void)
{
	prolog *pl = pl_create();

	if (!pl)
		tpl_platform_panic("could not create the Prolog engine");

	set_quiet(pl);
	set_dump_vars(pl, 0);

	bool ok = pl_consult_text(pl, (const char*)program_pl, program_pl_len,
		"main");

	// halt/1 in the program decides the status; otherwise a clean load is
	// success and a load error or uncaught exception is failure.
	int status = 0;

	if (get_halt(pl))
		status = get_halt_code(pl);
	else if (!ok || get_error(pl))
		status = 1;

	pl_destroy(pl);
	tpl_platform_halt(status);
}
