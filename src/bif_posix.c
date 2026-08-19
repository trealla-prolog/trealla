#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include "prolog.h"
#include "query.h"

#ifndef _WIN32
#include <signal.h>
#include <unistd.h>
#endif

#if !defined(_WIN32) && !defined(__wasi__)
#define USE_SYSLOG 1
#include <syslog.h>
#endif

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
		return unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx);
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
		buffer = TPL_realloc(buffer, length);

		// FIXME: `0' returned by strftime() does not always indicate
		// an error, seems there is no easy way to check that.

		if (strftime(buffer, length, format, &tm) > 0) {
			cell tmp;
			make_string(&tmp, buffer);
			TPL_free(buffer);
			bool ok = unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx);
			unshare_cell(&tmp);
			return ok;
		}
	}

	TPL_free(buffer);
	return false;
}

#ifndef _WIN32
static bool bif_posix_strptime_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,string);
	GET_NEXT_ARG(p3,var);

    struct tm tm = {0};

	if (strptime(C_STR(q, p2), C_STR(q, p1), &tm) == NULL)
		return false;

	cell *tmp = alloc_heap(q, 10);
	make_instr(tmp, g_tm_s, NULL, 9, 0);
	pl_idx num_cells = 1;
	make_int(tmp+num_cells++, tm.tm_sec);
	make_int(tmp+num_cells++, tm.tm_min);
	make_int(tmp+num_cells++, tm.tm_hour);
	make_int(tmp+num_cells++, tm.tm_mday);
	make_int(tmp+num_cells++, tm.tm_mon);
	make_int(tmp+num_cells++, tm.tm_year);
	make_int(tmp+num_cells++, tm.tm_wday);
	make_int(tmp+num_cells++, tm.tm_yday);
	make_int(tmp+num_cells++, tm.tm_isdst);

	return unify(q, p3, p3_ctx, tmp, q->st.cur_ctx);
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
	return unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_posix_gmtime_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);

	time_t t = get_smallint(p1);
	struct tm tm = {0};

	if (gmtime_r(&t, &tm) == NULL)
		return 0;

	cell *tmp = alloc_heap(q, 10);
	make_instr(tmp, g_tm_s, NULL, 9, 0);
	pl_idx num_cells = 1;
	make_int(tmp+num_cells++, tm.tm_sec);
	make_int(tmp+num_cells++, tm.tm_min);
	make_int(tmp+num_cells++, tm.tm_hour);
	make_int(tmp+num_cells++, tm.tm_mday);
	make_int(tmp+num_cells++, tm.tm_mon);
	make_int(tmp+num_cells++, tm.tm_year);
	make_int(tmp+num_cells++, tm.tm_wday);
	make_int(tmp+num_cells++, tm.tm_yday);
	make_int(tmp+num_cells++, tm.tm_isdst);

	return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
}

static bool bif_posix_localtime_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);

	time_t t = get_smallint(p1);
	struct tm tm = {0};

	if (localtime_r(&t, &tm) == NULL)
		return 0;

	cell *tmp = alloc_heap(q, 10);
	make_instr(tmp, g_tm_s, NULL, 9, 0);
	pl_idx num_cells = 1;
	make_int(tmp+num_cells++, tm.tm_sec);
	make_int(tmp+num_cells++, tm.tm_min);
	make_int(tmp+num_cells++, tm.tm_hour);
	make_int(tmp+num_cells++, tm.tm_mday);
	make_int(tmp+num_cells++, tm.tm_mon);
	make_int(tmp+num_cells++, tm.tm_year);
	make_int(tmp+num_cells++, tm.tm_wday);
	make_int(tmp+num_cells++, tm.tm_yday);
	make_int(tmp+num_cells++, tm.tm_isdst);

	return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
}

static bool bif_posix_ctime_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);
	time_t when = get_smallint(p1);
	char tmpbuf[256];
	cell tmp;
	make_cstring(&tmp, ctime_r(&when, tmpbuf));
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_posix_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, time(NULL));
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_posix_getpid_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
#ifndef __wasi__
	make_int(&tmp, getpid());
#else
	make_int(&tmp, -1);
#endif
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_posix_getppid_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
#if !defined(_WIN32) && !defined(__wasi__)
	make_int(&tmp, getppid());
#else
	make_int(&tmp, -1);
#endif
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_posix_fork_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
#if !defined(_WIN32) && !defined(__wasi__)
	signal(SIGCHLD, SIG_IGN);
	int pid = fork();
	make_int(&tmp, pid);
#else
	make_int(&tmp, -1);
#endif
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}


// --- syslog ------------------------------------------------------------
//
// The LOG_* values are platform-specific, so the symbolic names SWI's
// library(syslog) uses are mapped here rather than in Prolog. Anything
// the platform does not define is simply absent from the table and is
// reported as a domain_error.

#if USE_SYSLOG

typedef struct { const char *name; int val; } syslog_name;

static const syslog_name s_syslog_facilities[] = {
	{"auth", LOG_AUTH},
#ifdef LOG_AUTHPRIV
	{"authpriv", LOG_AUTHPRIV},
#endif
	{"cron", LOG_CRON},
	{"daemon", LOG_DAEMON},
#ifdef LOG_FTP
	{"ftp", LOG_FTP},
#endif
	{"kern", LOG_KERN},
	{"local0", LOG_LOCAL0}, {"local1", LOG_LOCAL1},
	{"local2", LOG_LOCAL2}, {"local3", LOG_LOCAL3},
	{"local4", LOG_LOCAL4}, {"local5", LOG_LOCAL5},
	{"local6", LOG_LOCAL6}, {"local7", LOG_LOCAL7},
	{"lpr", LOG_LPR}, {"mail", LOG_MAIL}, {"news", LOG_NEWS},
	{"syslog", LOG_SYSLOG}, {"user", LOG_USER}, {"uucp", LOG_UUCP},
	{NULL, 0}
};

static const syslog_name s_syslog_priorities[] = {
	{"emerg", LOG_EMERG}, {"alert", LOG_ALERT}, {"crit", LOG_CRIT},
	{"err", LOG_ERR}, {"warning", LOG_WARNING}, {"notice", LOG_NOTICE},
	{"info", LOG_INFO}, {"debug", LOG_DEBUG},
	{NULL, 0}
};

static const syslog_name s_syslog_options[] = {
	{"cons", LOG_CONS}, {"ndelay", LOG_NDELAY}, {"nowait", LOG_NOWAIT},
	{"odelay", LOG_ODELAY},
#ifdef LOG_PERROR
	{"perror", LOG_PERROR},
#endif
	{"pid", LOG_PID},
	{NULL, 0}
};

static bool syslog_lookup(const syslog_name *tbl, const char *name, int *val)
{
	for (const syslog_name *p = tbl; p->name; p++) {
		if (!strcmp(p->name, name)) {
			*val = p->val;
			return true;
		}
	}

	return false;
}

// openlog(3) stores the pointer it is given, it does not copy the
// string, so the ident has to outlive the call. Keeping our own copy is
// not optional: the Prolog atom's storage can move or be reclaimed, and
// syslog would then read freed memory on every message.

static char *g_syslog_ident = NULL;

#endif

static bool bif_sys_openlog_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);
	GET_NEXT_ARG(p3,atom);
#if USE_SYSLOG
	int facility = 0;

	if (!syslog_lookup(s_syslog_facilities, C_STR(q, p3), &facility))
		return throw_error(q, p3, p3_ctx, "domain_error", "syslog_facility");

	int mask = 0;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (!is_atom(c))
			return throw_error(q, c, c_ctx, "type_error", "atom");

		int opt = 0;

		if (!syslog_lookup(s_syslog_options, C_STR(q, c), &opt))
			return throw_error(q, c, c_ctx, "domain_error", "syslog_option");

		mask |= opt;
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (!is_nil(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	char *ident = strdup(C_STR(q, p1));
	CHECKED(ident);
	openlog(ident, mask, facility);

	// Freed only after the new one is in place, since the old pointer is
	// live until openlog() replaces it.

	free(g_syslog_ident);
	g_syslog_ident = ident;
	return true;
#else
	return throw_error(q, p1, p1_ctx, "resource_error", "syslog_unavailable");
#endif
}

static bool bif_sys_syslog_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
#if USE_SYSLOG
	int pri = 0;

	if (!syslog_lookup(s_syslog_priorities, C_STR(q, p1), &pri))
		return throw_error(q, p1, p1_ctx, "domain_error", "syslog_priority");

	const char *src;
	char *tofree = NULL;

	if (is_atom(p2) || is_string(p2)) {
		src = C_STR(q, p2);
	} else if (is_nil(p2)) {
		src = "";
	} else if (is_iso_list(p2)) {
		if (!scan_is_chars_list(q, p2, p2_ctx, true))
			return throw_error(q, p2, p2_ctx, "type_error", "text");

		tofree = chars_list_to_string(q, p2, p2_ctx);
		CHECKED(tofree);
		src = tofree;
	} else
		return throw_error(q, p2, p2_ctx, "type_error", "text");

	// "%s" rather than passing the message as the format itself - a
	// message carrying %s or %n would otherwise read arbitrary memory.

	syslog(pri, "%s", src);

	if (tofree)
		free(tofree);

	return true;
#else
	return throw_error(q, p1, p1_ctx, "resource_error", "syslog_unavailable");
#endif
}

static bool bif_sys_closelog_0(query *q)
{
#if USE_SYSLOG
	closelog();
	free(g_syslog_ident);
	g_syslog_ident = NULL;
	return true;
#else
	return throw_error(q, q->st.instr, q->st.cur_ctx, "resource_error", "syslog_unavailable");
#endif
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

	{"posix_getppid", 1, bif_posix_getppid_1, "-integer", false, false, BLAH},
	{"posix_getpid", 1, bif_posix_getpid_1, "-integer", false, false, BLAH},
	{"posix_fork", 1, bif_posix_fork_1, "-integer", false, false, BLAH},

	{"$openlog", 3, bif_sys_openlog_3, "+atom,+list,+atom", false, false, BLAH},
	{"$syslog", 2, bif_sys_syslog_2, "+atom,+term", false, false, BLAH},
	{"$closelog", 0, bif_sys_closelog_0, NULL, false, false, BLAH},

	// For Logtalk...

	{"pid", 1, bif_posix_getpid_1, "-integer", false, false, BLAH},

	{0}
};

