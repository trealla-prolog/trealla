#include "trealla.h"

int main(void)
{
	prolog *pl = pl_create();
	pl_sub_query *q = NULL;

	if (!pl)
		return 1;

	set_quiet(pl);
	bool parsed = pl_query(pl, "X is 6*7", &q, 0);
	pl_term *x = parsed ? pl_binding(q, "X") : NULL;
	int64_t value = 0;
	bool solved = parsed && get_status(pl) && !get_error(pl)
		&& x && pl_get_int64(x, &value) && (value == 42);

	if (q)
		pl_done(q);

	pl_eval(pl, "\\+current_predicate('$server'/3),\\+current_predicate(shell/1)", false);
	solved = solved && get_status(pl) && !get_error(pl);

	pl_destroy(pl);
	return solved ? 0 : 1;
}
