#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>

#if !defined(_WIN32) && !defined(__wasi__) && !defined(__ANDROID__)
#include <spawn.h>
#include <sys/wait.h>
#endif

#include "history.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

#ifdef _WIN32
#include <windows.h>
#define unsetenv(p1)
#define setenv(p1,p2,p3) _putenv_s(p1,p2)
#define msleep Sleep
#define localtime_r(p1,p2) localtime(p1)
#else
#include <unistd.h>
static void msleep(int ms)
{
	struct timespec tv = {0};
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

#define MAX_ARGS 128

#ifdef _WIN32

#define MS_PER_SEC      1000ULL     // MS = milliseconds
#define US_PER_MS       1000ULL     // US = microseconds
#define HNS_PER_US      10ULL       // HNS = hundred-nanoseconds (e.g., 1 hns = 100 ns)
#define NS_PER_US       1000ULL

#define HNS_PER_SEC     (MS_PER_SEC * US_PER_MS * HNS_PER_US)
#define NS_PER_HNS      (100ULL)    // NS = nanoseconds
#define NS_PER_SEC      (MS_PER_SEC * US_PER_MS * NS_PER_US)

static int clock_gettime_monotonic(struct timespec *tv)
{
	static LARGE_INTEGER ticksPerSec = {0};
	LARGE_INTEGER ticks;
	double seconds;

	if (!ticksPerSec.QuadPart) {
		QueryPerformanceFrequency(&ticksPerSec);
		if (!ticksPerSec.QuadPart) {
			errno = ENOTSUP;
			return -1;
		}
	}

	QueryPerformanceCounter(&ticks);
	seconds = (double) ticks.QuadPart / (double) ticksPerSec.QuadPart;
	tv->tv_sec = (time_t)seconds;
	tv->tv_nsec = (long)((ULONGLONG)(seconds * NS_PER_SEC) % NS_PER_SEC);
	return 0;
}

static int clock_gettime_realtime(struct timespec *tv)
{
	FILETIME ft;
	ULARGE_INTEGER hnsTime;
	GetSystemTimeAsFileTime(&ft);
	hnsTime.LowPart = ft.dwLowDateTime;
	hnsTime.HighPart = ft.dwHighDateTime;

	// To get POSIX Epoch as baseline, subtract the number of hns intervals from Jan 1, 1601 to Jan 1, 1970.
	hnsTime.QuadPart -= (11644473600ULL * HNS_PER_SEC);

	// modulus by hns intervals per second first, then convert to ns, as not to lose resolution
	tv->tv_nsec = (long) ((hnsTime.QuadPart % HNS_PER_SEC) * NS_PER_HNS);
	tv->tv_sec = (long) (hnsTime.QuadPart / HNS_PER_SEC);
	return 0;
}

static int my_clock_gettime(clockid_t type, struct timespec *tp)
{
	if (type == CLOCK_MONOTONIC)
		return clock_gettime_monotonic(tp);
	else if (type == CLOCK_REALTIME)
		return clock_gettime_realtime(tp);

    errno = ENOTSUP;
    return -1;
}
#else
#define my_clock_gettime clock_gettime
#endif

uint64_t cpu_time_in_usec(void)
{
	struct timespec now = {0};
#ifdef CLOCK_PROCESS_CPUTIME_ID
	my_clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &now);
#else
	my_clock_gettime(CLOCK_MONOTONIC, &now);
#endif
	return (uint64_t)(now.tv_sec * 1000 * 1000) + (now.tv_nsec / 1000);
}

uint64_t wall_time_in_usec(void)
{
	struct timespec now = {0};
	my_clock_gettime(CLOCK_REALTIME, &now);
	return (uint64_t)(now.tv_sec * 1000 * 1000) + (now.tv_nsec / 1000);
}

#ifndef __wasi__
static bool bif_shell_1(query *q)
{
	GET_FIRST_ARG(p1,source_sink);
	char *filename;
	GET_SOURCE_SINK(p1, p1_ctx, filename);
	int status = system(filename);
	free(filename);

	if (status == 0)
		return true;
	else
		return false;
}

static bool bif_shell_2(query *q)
{
	GET_FIRST_ARG(p1,source_sink);
	GET_NEXT_ARG(p2,var);
	char *filename;
	GET_SOURCE_SINK(p1, p1_ctx, filename);
	int status = system(filename);
	free(filename);
	cell tmp;
	make_int(&tmp, status);
	return unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
}
#else
static bool bif_shell_1(query *q)
{
	return false;
}

static bool bif_shell_2(query *q)
{
	return false;
}
#endif

static bool bif_getenv_2(query *q)
{
	GET_FIRST_ARG(p1,source_sink);
	GET_NEXT_ARG(p2,var);
	char *filename;
	GET_SOURCE_SINK(p1, p1_ctx, filename);
	const char *value = getenv(filename);
	free(filename);

	if (!value)
		return false;

	cell tmp;

	if (is_string(p1))
		make_string(&tmp, value);
	else
		make_cstring(&tmp, value);

	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_setenv_2(query *q)
{
	GET_FIRST_ARG(p1,source_sink);
	GET_NEXT_ARG(p2,source_sink);
	char *filename, *filename2;
	GET_SOURCE_SINK(p1, p1_ctx, filename);
	GET_SOURCE_SINK(p2, p2_ctx, filename2);
	setenv(filename, filename2, 1);
	free(filename2);
	free(filename);
	return true;
}

static bool bif_unsetenv_1(query *q)
{
	GET_FIRST_ARG(p1,source_sink);
	char *filename;
	GET_SOURCE_SINK(p1, p1_ctx, filename);
	unsetenv(filename);
	free(filename);
	return true;
}

static bool bif_sleep_1(query *q)
{
	if (q->retry)
		return true;

	GET_FIRST_ARG(p1,number);

	if (is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (q->is_task)
		return do_yield(q, get_smallint(p1)*1000);

	int ms = (is_float(p1) ? (double)get_float(p1) : (double)get_smallint(p1)) * 1000;

	while ((ms > 0) && !q->halt) {
		CHECK_INTERRUPT();
		msleep(100);
		ms -= 100;
	}

	return true;
}

static bool bif_now_0(query *q)
{
	pl_int secs = wall_time_in_usec() / 1000 / 1000;
	q->accum.tag = TAG_INT;
	set_smallint(&q->accum, secs);
	return true;
}

static bool bif_now_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	pl_int secs = wall_time_in_usec() / 1000 / 1000;
	cell tmp;
	make_int(&tmp, secs);
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_get_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	pl_int us = wall_time_in_usec();
	double secs = us / 1000 / 1000;
	double v = us - (secs * 1000 * 1000);
	double frac = v / 1000 / 1000;
	cell tmp;
	make_float(&tmp, secs + frac);
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_wall_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	pl_int us = wall_time_in_usec() - q->get_started;
	double secs = us / 1000 / 1000;
	double v = us - (secs * 1000 * 1000);
	double frac = v / 1000 / 1000;
	cell tmp;
	make_float(&tmp, secs + frac);
	return unify (q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_cpu_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	double v = ((double)cpu_time_in_usec() - q->cpu_time) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (pl_flt)v);
	return unify (q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_date_time_7(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,var);
	GET_NEXT_ARG(p5,var);
	GET_NEXT_ARG(p6,var);
	GET_NEXT_ARG(p7,var);
	struct timeval cur_time;
	gettimeofday(&cur_time, NULL);
	struct tm tm = {0};
	localtime_r((const time_t*)&cur_time.tv_sec, &tm);
	cell tmp;
	make_int(&tmp, tm.tm_year+1900);
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_mon+1);
	unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_mday);
	unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_hour);
	unify(q, p4, p4_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_min);
	unify(q, p5, p5_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_sec);
	unify(q, p6, p6_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, cur_time.tv_usec/1000);
	unify(q, p7, p7_ctx, &tmp, q->st.cur_ctx);
	return true;
}

static bool bif_date_time_6(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,var);
	GET_NEXT_ARG(p5,var);
	GET_NEXT_ARG(p6,var);
	struct tm tm = {0};
	time_t now = time(NULL);
	localtime_r(&now, &tm);
	cell tmp;
	make_int(&tmp, tm.tm_year+1900);
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_mon+1);
	unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_mday);
	unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_hour);
	unify(q, p4, p4_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_min);
	unify(q, p5, p5_ctx, &tmp, q->st.cur_ctx);
	make_int(&tmp, tm.tm_sec);
	unify(q, p6, p6_ctx, &tmp, q->st.cur_ctx);
	return true;
}

static bool bif_sys_alarm_1(query *q)
{
#if defined(_WIN32) || !defined(ITIMER_REAL)
	return false;
#else
	GET_FIRST_ARG(p1,number);
	int time0 = 0;

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "positive_integer");

	if (is_float(p1))
		time0 = get_float(p1) * 1000;
	else
		time0 = get_smallint(p1);

	if (time0 < 0)
		return throw_error(q, p1, p1_ctx, "domain_error", "positive_integer");

	struct itimerval it = {0};

	if (time0 == 0) {
		setitimer(ITIMER_REAL, &it, NULL);
		return true;
	}

	int ms = time0;
	int secs = ms / 1000;
	ms -= secs * 1000;

	it.it_value.tv_sec = secs;
	it.it_value.tv_usec = ms * 1000;
	setitimer(ITIMER_REAL, &it, NULL);
	return true;
#endif
}

static bool bif_busy_1(query *q)
{
	GET_FIRST_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	pl_int elapse = get_smallint(p1);

	if (elapse < 0)
		return true;

	// Limit to 60 seconds...

	if (elapse > (60 * 1000))
		return true;

	pl_uint started = wall_time_in_usec() / 1000;
	pl_uint end = started + elapse;

	while ((wall_time_in_usec() / 1000)  < end) {
		CHECK_INTERRUPT();
	}

	return true;
}

static bool bif_sys_timer_0(query *q)
{
	q->st.cpu_time = cpu_time_in_usec();
	q->total_inferences = 0;
	return true;
}

static bool bif_sys_elapsed_0(query *q)
{
	q->total_inferences--;
	uint64_t cpu_now = cpu_time_in_usec();
	uint64_t cpu_elapsed = cpu_now - q->st.cpu_time;
	double lips = (1.0 / ((double)cpu_elapsed/1000/1000)) * q->total_inferences;
	cell tmp;
	make_int(&tmp, q->total_inferences);
	char tmpbuf[80];
	format_integer(tmpbuf, &tmp, 3, '_', 0, 10);
	fprintf(stderr, "%% CPU elapsed %.3fs, %s inferences, %.3f MLips\n", (double)cpu_elapsed/1000/1000, tmpbuf, lips/1000/1000);
	if (q->is_redo) fprintf(stdout, "  ");
	q->total_inferences = 0;
	q->st.cpu_time = cpu_now;
	return true;
}

static bool bif_time_1(query *q)
{
	if (q->retry) {
		bif_sys_elapsed_0(q);
		return false;
	}

	bif_sys_timer_0(q);
	GET_FIRST_ARG(p1,callable);
	cell *tmp = prepare_call(q, CALL_NOSKIP, p1, p1_ctx, 4);
	pl_idx num_cells = p1->num_cells;
	make_instr(tmp+num_cells++, g_sys_elapsed_s, bif_sys_elapsed_0, 0, 0);
	make_instr(tmp+num_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+num_cells++, q->cp);
	make_call(q, tmp+num_cells);
	CHECKED(push_barrier(q));
	q->st.instr = tmp;
	return true;
}

static bool bif_get_unbuffered_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		return throw_error(q, &tmp, q->st.cur_ctx, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		return throw_error(q, &tmp, q->st.cur_ctx, "permission_error", "input,past_end_of_stream");
	}

	int ch = history_getch_fd(fileno(str->fp));

	if (ch == 4)
		ch = -1;

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	}

	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool bif_get_unbuffered_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		return throw_error(q, &tmp, q->st.cur_ctx, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		return throw_error(q, &tmp, q->st.cur_ctx, "permission_error", "input,past_end_of_stream");
	}

	int ch = history_getch_fd(fileno(str->fp));

	if (ch == 4)
		ch = -1;

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	}

	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	if (ch == -1) {
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	}

	char tmpbuf[MAX_BYTES_PER_CODEPOINT+1];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

#if !defined(_WIN32) && !defined(__wasi__)
static bool bif_popen_4(query *q)
{
	GET_FIRST_ARG(p1,source_sink);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,list_or_nil);
	int n = new_stream(q->pl);
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	char *filename;

	if (is_atom(p1))
		filename = src = DUP_STRING(q, p1);
	else if (!is_iso_list(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx);
		filename = src;
	}

	stream *str = &q->pl->streams[n];
	str->is_pipe = true;
	CHECKED(str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL));
	CHECKED(str->filename = strdup(filename));
	CHECKED(str->mode = DUP_STRING(q, p2));
	bool binary = false;
	uint8_t eof_action = eof_action_eof_code;
	bool is_alias = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_compound(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);


			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, q->latest_ctx, "permission_error", "open,source_sink");

			if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
				if (!CMP_STRING_TO_CSTR(q, name, "current_input")) {
					q->pl->current_input = n;
				} else if (!CMP_STRING_TO_CSTR(q, name, "current_output")) {
					q->pl->current_output = n;
				} else if (!CMP_STRING_TO_CSTR(q, name, "current_error")) {
					q->pl->current_error = n;
				} else {
					sl_app(str->alias, DUP_STRING(q, name), NULL);
#if 0
					cell tmp;
					make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

					if (!unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx))
						return false;

					is_alias = true;
#endif
				}
			} else if (!CMP_STRING_TO_CSTR(q, c, "type")) {
				if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "binary")) {
					binary = true;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "text"))
					binary = false;
			} else if (!CMP_STRING_TO_CSTR(q, c, "eof_action")) {
				if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "error")) {
					eof_action = eof_action_error;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "eof_code")) {
					eof_action = eof_action_eof_code;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "reset")) {
					eof_action = eof_action_reset;
				}
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_var(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	str->binary = binary;
	str->eof_action = eof_action;

	if (!strcmp(str->mode, "read"))
		str->fp = popen(filename, binary?"rb":"r");
	else if (!strcmp(str->mode, "write"))
		str->fp = popen(filename, binary?"wb":"w");
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");

	free(src);

	if (!str->fp) {
		if ((errno == EACCES) || (strcmp(str->mode, "read") && (errno == EROFS)))
			return throw_error(q, p1, p1_ctx, "permission_error", "open,source_sink");
		else
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	if (!is_alias) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_STREAM;

		if (!unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx))
			return false;
	}

	return true;
}
#endif

extern char **g_envp;

#if !defined(_WIN32) && !defined(__wasi__) && !defined(__ANDROID__)
static bool bif_process_create_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);
	GET_NEXT_ARG(p3,list_or_nil);
	char *src = NULL;
	char *filename;

	if (is_atom(p1))
		filename = src = DUP_STRING(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx);
		filename = src;
	}

    int args = 0, envs = 0;
    char *arguments[MAX_ARGS] = {NULL};
    char *environments[MAX_ARGS] = {NULL};
	arguments[args++] = strdup(filename);

	for (int i = 0; g_envp[i] != NULL; i++)
		environments[envs++] = strdup(g_envp[i]);

	LIST_HANDLER(p2);

	while (is_iso_list(p2)) {
		assert(args < MAX_ARGS);
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (!is_atom(c))
			return throw_error(q, c, c_ctx, "domain_error", "args");

		arguments[args++] = DUP_STRING(q, c);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	arguments[args] = NULL;
    posix_spawn_file_actions_t file_actions;
    posix_spawn_file_actions_init(&file_actions);
    posix_spawnattr_t attrp;
    posix_spawnattr_init(&attrp);
    cell *ppid = NULL;
    pl_ctx ppid_ctx = 0;
	LIST_HANDLER(p3);

	while (is_iso_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_compound(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, c_ctx);
			pl_ctx name_ctx = q->latest_ctx;

			if (!CMP_STRING_TO_CSTR(q, c, "process") || !CMP_STRING_TO_CSTR(q, c, "pid")) {
				ppid = name;
				ppid_ctx = name_ctx;
			} else if (!CMP_STRING_TO_CSTR(q, c, "detached")) {
#if (defined(__GLIBC__) && (__GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 26))) || !defined(POSIX_SPAWN_SETSID)
				return throw_error(q, c, c_ctx, "not available", "posix_spawnattr_setflags");
#else
				posix_spawnattr_setflags(&attrp, POSIX_SPAWN_SETSID);
#endif
			} else if (!CMP_STRING_TO_CSTR(q, c, "cwd")) {
#ifndef posix_spawn_file_actions_addchdir_np
				return throw_error(q, c, c_ctx, "not available", "posix_spawn_file_actions_addchdir_np");
#else
				cwd = C_STR(q, name);
				posix_spawn_file_actions_addchdir_np(&file_actions, cwd);
#endif
			} else if (!CMP_STRING_TO_CSTR(q, c, "env") && is_list_or_nil(name)) {
				LIST_HANDLER(name);
				memset(environments, 0, sizeof(environments));
				envs = 0;

				while (is_iso_list(name)) {
					cell *h = LIST_HEAD(name);
					cell *c = deref(q, h, name_ctx);

					if (is_compound(c) && (c->arity == 2) && (c->val_off == g_eq_s)) {
						cell *p1 = c + 1, *p2 = c + 2;
						SB(pr);

						if (is_atom(p1) && is_atom(p2)) {
							SB_sprintf(pr, "%s=%s", C_STR(q, p1), C_STR(q, p2));
						} else if (is_atom(p1) && is_smallint(p2)) {
							SB_sprintf(pr, "%s=%d", C_STR(q, p1), (int)get_smallint(p2));
						}

						environments[envs++] = SB_cstr(pr);
					}

					name = LIST_TAIL(name);
					name = deref(q, name, name_ctx);
					name_ctx = q->latest_ctx;
				}

			} else if (!CMP_STRING_TO_CSTR(q, c, "environment") && is_list_or_nil(name)) {
				LIST_HANDLER(name);

				while (is_iso_list(name)) {
					cell *h = LIST_HEAD(name);
					cell *c = deref(q, h, name_ctx);

					if (is_compound(c) && (c->arity == 2) && (c->val_off == g_eq_s)) {
						cell *p1 = c + 1, *p2 = c + 2;
						SB(pr);

						if (is_atom(p1) && is_atom(p2)) {
							SB_sprintf(pr, "%s=%s", C_STR(q, p1), C_STR(q, p2));
						} else if (is_atom(p1) && is_smallint(p2)) {
							SB_sprintf(pr, "%s=%d", C_STR(q, p1), (int)get_smallint(p2));
						}

						environments[envs++] = SB_cstr(pr);
					}

					name = LIST_TAIL(name);
					name = deref(q, name, name_ctx);
					name_ctx = q->latest_ctx;
				}

			} else if (!CMP_STRING_TO_CSTR(q, c, "stdin") && !CMP_STRING_TO_CSTR(q, name, "std")) {
				posix_spawn_file_actions_adddup2(&file_actions, q->pl->current_input, 0);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stdin") && !CMP_STRING_TO_CSTR(q, name, "null")) {
				posix_spawn_file_actions_addopen(&file_actions, 0, "/dev/null", O_RDONLY, 0);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stdin") && !CMP_STRING_TO_CSTR(q, name, "pipe")
				&& is_compound(name) && (name->arity == 1) && is_var(name+1)) {
				cell *ns = deref(q, name+1, name_ctx);
				pl_ctx ns_ctx = q->latest_ctx;
				int n = new_stream(q->pl);
				int fds[2];
				if (pipe(fds)) return false;
				posix_spawn_file_actions_adddup2(&file_actions, fds[0], 0);
				q->pl->streams[n].fp = fdopen(fds[1], "w");
				q->pl->streams[n].is_pipe = true;
				CHECKED(q->pl->streams[n].mode = strdup("write"));
				cell tmp;
				make_int(&tmp, n);
				tmp.flags |= FLAG_INT_STREAM;
				unify(q, ns, ns_ctx, &tmp, q->st.cur_ctx);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stdin") && !CMP_STRING_TO_CSTR(q, name, "stream")) {
				cell *ns = deref(q, name, name_ctx);
				int n = get_stream(q, ns);
				posix_spawn_file_actions_adddup2(&file_actions, fileno(q->pl->streams[n].fp), 0);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stdout") && !CMP_STRING_TO_CSTR(q, name, "std")) {
				posix_spawn_file_actions_adddup2(&file_actions, q->pl->current_output, 1);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stdout") && !CMP_STRING_TO_CSTR(q, name, "null")) {
				posix_spawn_file_actions_addopen(&file_actions, 1, "/dev/null", O_WRONLY, 0);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stdout") && !CMP_STRING_TO_CSTR(q, name, "pipe")
				&& is_compound(name) && (name->arity == 1) && is_var(name+1)) {
				cell *ns = deref(q, name+1, name_ctx);
				pl_ctx ns_ctx = q->latest_ctx;
				int n = new_stream(q->pl);
				int fds[2];
				if (pipe(fds)) return false;
				posix_spawn_file_actions_adddup2(&file_actions, fds[1], 1);
				q->pl->streams[n].fp = fdopen(fds[0], "r");
				q->pl->streams[n].is_pipe = true;
				CHECKED(q->pl->streams[n].mode = strdup("read"));
				cell tmp;
				make_int(&tmp, n);
				tmp.flags |= FLAG_INT_STREAM;
				unify(q, ns, ns_ctx, &tmp, q->st.cur_ctx);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stdout") && !CMP_STRING_TO_CSTR(q, name, "stream")) {
				cell *ns = deref(q, name, name_ctx);
				int n = get_stream(q, ns);
				posix_spawn_file_actions_adddup2(&file_actions, fileno(q->pl->streams[n].fp), 1);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stderr") && !CMP_STRING_TO_CSTR(q, name, "std")) {
				posix_spawn_file_actions_adddup2(&file_actions, q->pl->current_error, 2);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stderr") && !CMP_STRING_TO_CSTR(q, name, "null")) {
				posix_spawn_file_actions_addopen(&file_actions, 2, "/dev/null", O_WRONLY, 0);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stderr") && !CMP_STRING_TO_CSTR(q, name, "pipe")
				&& is_compound(name) && (name->arity == 1) && is_var(name+1)) {
				cell *ns = deref(q, name+1, name_ctx);
				pl_ctx ns_ctx = q->latest_ctx;
				int n = new_stream(q->pl);
				int fds[2];
				if (pipe(fds)) return false;
				posix_spawn_file_actions_adddup2(&file_actions, fds[1], 2);
				q->pl->streams[n].fp = fdopen(fds[0], "r");
				q->pl->streams[n].is_pipe = true;
				CHECKED(q->pl->streams[n].mode = strdup("read"));
				cell tmp;
				make_int(&tmp, n);
				tmp.flags |= FLAG_INT_STREAM;
				unify(q, ns, ns_ctx, &tmp, q->st.cur_ctx);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stderr") && !CMP_STRING_TO_CSTR(q, name, "stream")) {
				cell *ns = deref(q, name, name_ctx);
				int n = get_stream(q, ns);
				posix_spawn_file_actions_adddup2(&file_actions, fileno(q->pl->streams[n].fp), 2);
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "process_create_option");

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	pid_t pid;
	int ok = posix_spawnp(&pid, C_STR(q, p1), &file_actions, &attrp, (char * const*)arguments, (char * const*)environments);
	posix_spawn_file_actions_destroy(&file_actions);
	posix_spawnattr_destroy(&attrp);
	free(src);

	for (int i = 0; i < args; i++)
		free(arguments[i]);

	for (int i = 0; i < envs; i++)
		free(environments[i]);

	if (ok != 0)
		return throw_error(q, p1, p1_ctx, "system_error", "posix_spawnp");

	if (ppid) {
		cell tmp;
		make_uint(&tmp, pid);
		return unify(q, ppid, ppid_ctx, &tmp, q->st.cur_ctx);
	} else {
		waitpid(pid, NULL, 0);
	}

	return true;
}

static bool bif_process_wait_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,list_or_nil);
	LIST_HANDLER(p2);
	int secs = -1;

	while (is_iso_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);

		if (is_compound(c) && (c->arity == 1) && !CMP_STRING_TO_CSTR(q, c, "timeout")) {
			if (is_integer(FIRST_ARG(c)))
				secs = get_smallint(FIRST_ARG(c));
			else if (is_atom(FIRST_ARG(c)) && !CMP_STRING_TO_CSTR(q, FIRST_ARG(c), "infinite"))
				secs = -1;
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "process_wait_option");

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	int status = 0, pid = get_smalluint(p1);
	int ok = waitpid(pid, &status, secs != -1 ? WNOHANG : 0);
	return ok == pid;
}

static bool bif_process_wait_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int pid = get_smalluint(p1);
	waitpid(pid, NULL, 0);
	return true;
}

static bool bif_process_kill_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	int pid = get_smalluint(p1), sig = get_smallint(p2);
	kill(pid, sig);
	return true;
}

static bool bif_process_kill_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int pid = get_smalluint(p1);
	kill(pid, SIGKILL);
	return true;
}
#endif

builtins g_os_bifs[] =
{
	{"shell", 1, bif_shell_1, "+atom", false, false, BLAH},
	{"shell", 2, bif_shell_2, "+atom,-integer", false, false, BLAH},
	{"getenv", 2, bif_getenv_2, "+atom,-atom", false, false, BLAH},
	{"setenv", 2, bif_setenv_2, "+atom,+atom", false, false, BLAH},
	{"unsetenv", 1, bif_unsetenv_1, "+atom", false, false, BLAH},
	{"sleep", 1, bif_sleep_1, "+number", false, false, BLAH},
	{"now", 0, bif_now_0, NULL, false, false, BLAH},
	{"now", 1, bif_now_1, "-integer", false, false, BLAH},
	{"time", 1, bif_time_1, ":callable", false, false, BLAH},
	{"get_time", 1, bif_get_time_1, "-float", false, false, BLAH},
	{"cpu_time", 1, bif_cpu_time_1, "-integer", false, false, BLAH},
	{"wall_time", 1, bif_wall_time_1, "-integer", false, false, BLAH},
	{"date_time", 6, bif_date_time_6, "-integer,-integer,-integer,-integer,-integer,-integer", false, false, BLAH},
	{"date_time", 7, bif_date_time_7, "-integer,-integer,-integer,-integer,-integer,-integer,-integer", false, false, BLAH},
	{"busy", 1, bif_busy_1, "+integer", false, false, BLAH},
	{"get_unbuffered_code", 1, bif_get_unbuffered_code_1, "?integer", false, false, BLAH},
	{"get_unbuffered_char", 1, bif_get_unbuffered_char_1, "?character", false, false, BLAH},

#if !defined(_WIN32) && !defined(__wasi__) && !defined(__ANDROID__)
	{"process_create", 3, bif_process_create_3, "+atom,+list,+list", false, false, BLAH},
	{"process_wait", 2, bif_process_wait_2, "+integer,-integer", false, false, BLAH},
	{"process_wait", 1, bif_process_wait_1, "+integer", false, false, BLAH},
	{"process_kill", 2, bif_process_kill_2, "+integer,+integer", false, false, BLAH},
	{"process_kill", 1, bif_process_kill_1, "+integer", false, false, BLAH},
#endif

#if !defined(_WIN32) && !defined(__wasi__)
	{"popen", 4, bif_popen_4, "+source_sink,+atom,--stream,+list", false, false, BLAH},
#endif

	{"$alarm", 1, bif_sys_alarm_1, "+integer", false, false, BLAH},
	{"$timer", 0, bif_sys_timer_0, NULL, false, false, BLAH},
	{"$elapsed", 0, bif_sys_elapsed_0, NULL, false, false, BLAH},

	{0}
};
