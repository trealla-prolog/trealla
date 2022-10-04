#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <errno.h>

#include "trealla.h"
#include "internal.h"
#include "heap.h"
#include "prolog.h"
#include "query.h"

static bool fn_posix_strftime_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,compound);
	GET_NEXT_ARG(p3,var);

	if ((p2->val_off != g_tm_s) || (p2->arity != 9)) {
		return false;
	}

	const char *format = C_STR(q, p1);
	size_t length = C_STRLEN(q, p1);

	// XXX: Is this check reasonable? May strftime() return non-empty
	// result for empty format?

	if (length == 0) {
		cell tmp;
		make_atom(&tmp, g_empty_s);
		return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	}

	struct tm tm = {0};
	cell *arg;

	arg = deref(q, p2+1, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_sec = get_smallint(arg);
	arg = deref(q, p2+2, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_min = get_smallint(arg);
	arg = deref(q, p2+3, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_hour = get_smallint(arg);
	arg = deref(q, p2+4, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_mday = get_smallint(arg);
	arg = deref(q, p2+5, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_mon = get_smallint(arg);
	arg = deref(q, p2+6, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_year = get_smallint(arg);
	arg = deref(q, p2+7, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_wday = get_smallint(arg);
	arg = deref(q, p2+8, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_yday = get_smallint(arg);
	arg = deref(q, p2+9, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_isdst = get_smallint(arg);

	char *buffer = NULL;
	int tries = 0;
	const int max_tries = 5;

	while (++tries <= max_tries) {
		// make enough space for some long formats, e.g. `%c'
		length = 128 + length * 2;
		buffer = realloc(buffer, length);

		// FIXME: `0' returned by strftime() does not always indicate
		// an error, seems there is no easy way to check that.

		if (strftime(buffer, length, format, &tm) > 0) {
			cell tmp;
			make_cstring(&tmp, buffer);
			free(buffer);
			return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		}
	}

	free(buffer);
	return false;
}

static bool fn_posix_gmtime_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);

	time_t t = get_smallint(p1);
	struct tm tm = {0};

	if (gmtime_r(&t, &tm) == NULL)
		return 0;

	cell *tmp = alloc_on_heap(q, 10);
	make_struct(tmp, g_tm_s, NULL, 9, 0);
	pl_idx_t nbr_cells = 1;
	make_int(tmp+nbr_cells++, tm.tm_sec);
	make_int(tmp+nbr_cells++, tm.tm_min);
	make_int(tmp+nbr_cells++, tm.tm_hour);
	make_int(tmp+nbr_cells++, tm.tm_mday);
	make_int(tmp+nbr_cells++, tm.tm_mon);
	make_int(tmp+nbr_cells++, tm.tm_year);
	make_int(tmp+nbr_cells++, tm.tm_wday);
	make_int(tmp+nbr_cells++, tm.tm_yday);
	make_int(tmp+nbr_cells++, tm.tm_isdst);

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool fn_posix_localtime_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);

	time_t t = get_smallint(p1);
	struct tm tm = {0};

	if (localtime_r(&t, &tm) == NULL)
		return 0;

	cell *tmp = alloc_on_heap(q, 10);
	make_struct(tmp, g_tm_s, NULL, 9, 0);
	pl_idx_t nbr_cells = 1;
	make_int(tmp+nbr_cells++, tm.tm_sec);
	make_int(tmp+nbr_cells++, tm.tm_min);
	make_int(tmp+nbr_cells++, tm.tm_hour);
	make_int(tmp+nbr_cells++, tm.tm_mday);
	make_int(tmp+nbr_cells++, tm.tm_mon);
	make_int(tmp+nbr_cells++, tm.tm_year);
	make_int(tmp+nbr_cells++, tm.tm_wday);
	make_int(tmp+nbr_cells++, tm.tm_yday);
	make_int(tmp+nbr_cells++, tm.tm_isdst);

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool fn_posix_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, time(NULL));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

builtins g_posix_bifs[] =
{
    {"posix_strftime", 3, fn_posix_strftime_3, "+atom,+compound,-atom", false, false, BLAH},
	{"posix_gmtime", 2, fn_posix_gmtime_2, "+integer,-compound", false, false, BLAH},
	{"posix_localtime", 2, fn_posix_localtime_2, "+integer,-compound", false, false, BLAH},
	{"posix_time", 1, fn_posix_time_1, "-integer", false, false, BLAH},
	{0}
};

