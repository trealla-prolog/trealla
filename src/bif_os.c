#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>

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

#ifndef __wasi__
static bool bif_shell_1(query *q)
{
	GET_FIRST_ARG(p1,source_sink);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

		filename = chars_list_to_string(q, p1, p1_ctx);
	} else
		filename = DUP_STRING(q, p1);

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

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "string");

		filename = chars_list_to_string(q, p1, p1_ctx);
	} else
		filename = DUP_STRING(q, p1);

	int status = system(filename);
	free(filename);
	cell tmp;
	make_int(&tmp, status);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
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
	GET_NEXT_ARG(p2,atom_or_var);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

		filename = chars_list_to_string(q, p1, p1_ctx);
	} else
		filename = DUP_STRING(q, p1);

	const char *value = getenv(filename);
	free(filename);

	if (!value)
		return false;

	cell tmp;

	if (is_string(p1))
		make_string(&tmp, value);
	else
		make_cstring(&tmp, value);

	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_setenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_integer);

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	if (is_atom(p2)) {
		setenv(C_STR(q, p1), C_STR(q, p2), 1);
	} else if (is_integer(p2)) {
		char tmpbuf[256];
		sprint_int(tmpbuf, sizeof(tmpbuf), get_smallint(p2), 10);
		setenv(C_STR(q, p1), tmpbuf, 1);
	} else
		return false;

	return true;
}

static bool bif_unsetenv_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	unsetenv(C_STR(q, p1));
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
		msleep(ms > 10 ? 10 : ms);
		ms -= 10;
	}

	return true;
}

static bool bif_now_0(query *q)
{
	pl_int secs = get_time_in_usec() / 1000 / 1000;
	q->accum.tag = TAG_INTEGER;
	set_smallint(&q->accum, secs);
	return true;
}

static bool bif_now_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	pl_int secs = get_time_in_usec() / 1000 / 1000;
	cell tmp;
	make_int(&tmp, secs);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_get_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	double v = ((double)get_time_in_usec()-q->get_started) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (double)v);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_cpu_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	double v = ((double)cpu_time_in_usec()-q->cpu_started) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (pl_flt)v);
	return unify (q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_wall_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, time(NULL));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
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
	unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mon+1);
	unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mday);
	unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_hour);
	unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_min);
	unify(q, p5, p5_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_sec);
	unify(q, p6, p6_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, cur_time.tv_usec/1000);
	unify(q, p7, p7_ctx, &tmp, q->st.curr_frame);
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
	unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mon+1);
	unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mday);
	unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_hour);
	unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_min);
	unify(q, p5, p5_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_sec);
	unify(q, p6, p6_ctx, &tmp, q->st.curr_frame);
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

	pl_uint started = get_time_in_usec() / 1000;
	pl_uint end = started + elapse;

	while ((get_time_in_usec() / 1000)  < end) {
		CHECK_INTERRUPT();
	}

	return true;
}

static bool bif_sys_timer_0(query *q)
{
	q->st.timer_started = get_time_in_usec();
	q->tot_inferences = 0;
	return true;
}

static bool bif_sys_elapsed_0(query *q)
{
	q->tot_inferences--;
	uint64_t elapsed = get_time_in_usec();
	elapsed -= q->st.timer_started;
	double lips = (1.0 / ((double)elapsed/1000/1000)) * q->tot_inferences;
	fprintf(stderr, "%% Time elapsed %.3fs, %llu Inferences, %.3f MLips\n", (double)elapsed/1000/1000, (unsigned long long)q->tot_inferences, lips/1000/1000);
	if (q->is_redo) fprintf(stdout, "  ");
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
	check_heap_error(push_barrier(q));
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
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
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
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
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
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
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
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	if (ch == -1) {
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

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
	{"get_time", 1, bif_get_time_1, "-integer", false, false, BLAH},
	{"cpu_time", 1, bif_cpu_time_1, "-integer", false, false, BLAH},
	{"wall_time", 1, bif_wall_time_1, "-integer", false, false, BLAH},
	{"date_time", 6, bif_date_time_6, "-integer,-integer,-integer,-integer,-integer,-integer", false, false, BLAH},
	{"date_time", 7, bif_date_time_7, "-integer,-integer,-integer,-integer,-integer,-integer,-integer", false, false, BLAH},
	{"busy", 1, bif_busy_1, "+integer", false, false, BLAH},
	{"get_unbuffered_code", 1, bif_get_unbuffered_code_1, "?integer", false, false, BLAH},
	{"get_unbuffered_char", 1, bif_get_unbuffered_char_1, "?character", false, false, BLAH},

	{"$alarm", 1, bif_sys_alarm_1, "+integer", false, false, BLAH},
	{"$timer", 0, bif_sys_timer_0, NULL, false, false, BLAH},
	{"$elapsed", 0, bif_sys_elapsed_0, NULL, false, false, BLAH},

	{0}
};
