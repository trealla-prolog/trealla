#include "trealla.h"

extern unsigned char program_pl[];
extern unsigned int program_pl_len;

int main(void)
{
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

	const char invalid_source[] = "valid_fact.\0hidden_fact.";
	solved = solved && !pl_consult_text(pl, invalid_source,
		sizeof(invalid_source) - 1, "embedded-nul");

	pl_eval(pl, "\\+current_predicate('$server'/3),\\+current_predicate(shell/1)", false);
	solved = solved && get_status(pl) && !get_error(pl);

	pl_destroy(pl);
	return solved ? 0 : 1;
}
