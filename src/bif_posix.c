#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <errno.h>
#include <signal.h>

#include "trealla.h"
#include "internal.h"
#include "heap.h"
#include "prolog.h"
#include "query.h"

#ifdef _WIN32
#define ctime_r(p1,p2) ctime(p1)
#define gmtime_r(p1,p2) gmtime(p1)
#define localtime_r(p1,p2) localtime(p1)
#endif

static bool bif_posix_strftime_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p2,compound);

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
			bool ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
			unshare_cell(&tmp);
			return ok;
		}
	}

	free(buffer);
	return false;
}

#ifndef _WIN32
static bool bif_posix_strptime_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,var);

    struct tm tm = {0};

	if (strptime(C_STR(q, p2), C_STR(q, p1), &tm) == NULL)
		return false;

	cell *tmp = alloc_on_heap(q, 10);
	make_struct(tmp, g_tm_s, NULL, 9, 0);
	pl_idx nbr_cells = 1;
	make_int(tmp+nbr_cells++, tm.tm_sec);
	make_int(tmp+nbr_cells++, tm.tm_min);
	make_int(tmp+nbr_cells++, tm.tm_hour);
	make_int(tmp+nbr_cells++, tm.tm_mday);
	make_int(tmp+nbr_cells++, tm.tm_mon);
	make_int(tmp+nbr_cells++, tm.tm_year);
	make_int(tmp+nbr_cells++, tm.tm_wday);
	make_int(tmp+nbr_cells++, tm.tm_yday);
	make_int(tmp+nbr_cells++, tm.tm_isdst);

	return unify(q, p3, p3_ctx, tmp, q->st.curr_frame);
}
#endif

static bool bif_posix_mktime_2(query *q)
{
	GET_FIRST_ARG(p1,compound);
	GET_NEXT_ARG(p2,var);

	if ((p1->val_off != g_tm_s) || (p1->arity != 9)) {
		return false;
	}

	struct tm tm = {0};
	cell *arg;

	arg = deref(q, p1+1, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_sec = get_smallint(arg);
	arg = deref(q, p1+2, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_min = get_smallint(arg);
	arg = deref(q, p1+3, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_hour = get_smallint(arg);
	arg = deref(q, p1+4, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_mday = get_smallint(arg);
	arg = deref(q, p1+5, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_mon = get_smallint(arg);
	arg = deref(q, p1+6, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_year = get_smallint(arg);
	arg = deref(q, p1+7, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_wday = get_smallint(arg);
	arg = deref(q, p1+8, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_yday = get_smallint(arg);
	arg = deref(q, p1+9, p2_ctx); if (!is_smallint(arg)) return throw_error(q, arg, p2_ctx, "type_error", "integer"); tm.tm_isdst = get_smallint(arg);

	time_t now = mktime(&tm);
	cell tmp;
	make_int(&tmp, now);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool bif_posix_gmtime_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);

	time_t t = get_smallint(p1);
	struct tm tm = {0};

	if (gmtime_r(&t, &tm) == NULL)
		return 0;

	cell *tmp = alloc_on_heap(q, 10);
	make_struct(tmp, g_tm_s, NULL, 9, 0);
	pl_idx nbr_cells = 1;
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

static bool bif_posix_localtime_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);

	time_t t = get_smallint(p1);
	struct tm tm = {0};

	if (localtime_r(&t, &tm) == NULL)
		return 0;

	cell *tmp = alloc_on_heap(q, 10);
	make_struct(tmp, g_tm_s, NULL, 9, 0);
	pl_idx nbr_cells = 1;
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

static bool bif_posix_ctime_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);
	time_t when = get_smallint(p1);
	char tmpbuf[256];
	cell tmp;
	make_cstring(&tmp, ctime_r(&when, tmpbuf));
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_posix_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, time(NULL));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_posix_gettid_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
#ifndef __wasi__
	make_int(&tmp, gettid());
#else
	make_int(&tmp, 42);
#endif
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_posix_getpid_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
#ifndef __wasi__
	make_int(&tmp, getpid());
#else
	make_int(&tmp, 42);
#endif
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_posix_getppid_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
#ifndef __wasi__
	make_int(&tmp, getppid());
#else
	make_int(&tmp, 42);
#endif
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_posix_fork_1(query *q)
{
	GET_FIRST_ARG(p1,var);
#ifndef __wasi__
	signal(SIGCHLD, SIG_IGN);
	int pid = fork();
	cell tmp;
	make_int(&tmp, pid);
#else
	make_int(&tmp, -1);
#endif
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

builtins g_posix_bifs[] =
{
    {"posix_strftime", 3, bif_posix_strftime_3, "+atom,-atom,+compound", false, false, BLAH},
#ifndef _WIN32
    {"posix_strptime", 3, bif_posix_strptime_3, "+atom,+atom,-compound", false, false, BLAH},
#endif
	{"posix_gmtime", 2, bif_posix_gmtime_2, "+integer,-compound", false, false, BLAH},
	{"posix_localtime", 2, bif_posix_localtime_2, "+integer,-compound", false, false, BLAH},
	{"posix_mktime", 2, bif_posix_mktime_2, "+compound,-integer", false, false, BLAH},
	{"posix_ctime", 2, bif_posix_ctime_2, "+integer,-atom", false, false, BLAH},
	{"posix_time", 1, bif_posix_time_1, "-integer", false, false, BLAH},

	{"posix_gettid", 1, bif_posix_gettid_1, "-integer", false, false, BLAH},
	{"posix_getpid", 1, bif_posix_getpid_1, "-integer", false, false, BLAH},
	{"posix_getppid", 1, bif_posix_getppid_1, "-integer", false, false, BLAH},
	{"posix_fork", 1, bif_posix_fork_1, "-integer", false, false, BLAH},

	{0}
};

