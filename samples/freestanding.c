#include "trealla.h"
#include "platform/platform.h"

extern unsigned char program_pl[];
extern unsigned int program_pl_len;

static void marker(const char *s)
{
	const char *end = s;

	while (*end)
		end++;

	tpl_platform_console_write(TPL_CONSOLE_OUTPUT, s, (size_t)(end - s));
}

int main(void)
{
	marker("TREALLA FREESTANDING BOOT\n");
	uint64_t started = tpl_platform_monotonic_usec();
	prolog *pl = pl_create();
	pl_sub_query *q = NULL;

	if (!pl)
		return 1;

	set_quiet(pl);
	set_dump_vars(pl, 0);

	if (!pl_consult_text(pl, (const char*)program_pl, program_pl_len, "freestanding-program")) {
		pl_destroy(pl);
		return 1;
	}

	bool parsed = pl_query(pl, "freestanding_answer(X)", &q, 0);
	pl_term *x = parsed ? pl_binding(q, "X") : NULL;
	int64_t value = 0;
	bool solved = parsed && get_status(pl) && !get_error(pl)
		&& x && pl_get_int64(x, &value) && (value == 42);

	if (q)
		pl_done(q);

	pl_eval(pl, "freestanding_failure", false);
	solved = solved && !get_status(pl) && !get_error(pl);

	pl_eval(pl, "freestanding_platform_probe", false);
	solved = solved && get_status(pl) && !get_error(pl);

	const char invalid_source[] = "valid_fact.\0hidden_fact.";
	solved = solved && !pl_consult_text(pl, invalid_source,
		sizeof(invalid_source) - 1, "embedded-nul");

	pl_eval(pl, "\\+current_predicate('$server'/3),\\+current_predicate(shell/1),"
		"\\+current_predicate(open/4),\\+current_predicate(directory_files/2)", false);
	solved = solved && get_status(pl) && !get_error(pl);
	solved = solved && (tpl_platform_monotonic_usec() >= started);

	pl_destroy(pl);
	marker(solved ? "TREALLA FREESTANDING COMPLETE\n" : "TREALLA FREESTANDING FAILED\n");
	tpl_platform_halt(solved ? 0 : 1);
}
