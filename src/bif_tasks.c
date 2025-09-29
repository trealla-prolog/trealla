#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>

#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

#ifdef _WIN32
#include <windows.h>
#define msleep Sleep
#define localtime_r(p1,p2) localtime(p1)
#else
static void msleep(int ms)
{
	struct timespec tv = {0};
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

bool do_yield(query *q, int msecs)
{
#ifdef __wasi__
	if (!q->is_task && !q->pl->is_query)
#else
	if (!q->is_task)
#endif
		return true;

	q->yield_at = 0;
	q->yielded = true;
	q->tmo_msecs = get_time_in_usec() / 1000;
	q->tmo_msecs += msecs > 0 ? msecs : 1;
	checked(push_choice(q));
	return false;
}

bool do_yield_then(query *q, bool status)
{
#ifdef __wasi__
	if (!q->is_task && !q->pl->is_query)
#else
	if (!q->is_task)
#endif
		return true;

	q->yield_at = 0;
	q->yielded = true;
	q->tmo_msecs = get_time_in_usec() / 1000 + 1;
	// Push a choice point with the same result as the goal we hijacked
	// With that we can continue as if the yield didn't happen
	checked(push_choice(q));
	choice *ch = GET_CURR_CHOICE();

	if (status)
		ch->succeed_on_retry = true;
	else
		ch->fail_on_retry = true;

	return false;
}

void do_yield_at(query *q, unsigned int time_in_ms)
{
	q->yield_at = get_time_in_usec() / 1000;
	q->yield_at += time_in_ms > 0 ? time_in_ms : 1;
}

static cell *pop_queue(query *q)
{
	if (!q->qp[0])
		return NULL;

	cell *c = q->queue[0] + q->popp;
	q->popp += c->num_cells;

	if (q->popp == q->qp[0])
		q->popp = q->qp[0] = 0;

	return c;
}

static void push_task(query *q, query *task)
{
	task->next = q->tasks;

	if (q->tasks)
		q->tasks->prev = task;

	q->tasks = task;
}

static query *pop_task(query *q, query *task)
{
	if (task->prev)
		task->prev->next = task->next;

	if (task->next)
		task->next->prev = task->prev;

	if (task == q->tasks)
		q->tasks = task->next;

	return task->next;
}

static bool bif_end_wait_0(query *q)
{
	if (q->parent)
		q->parent->end_wait = true;

	return true;
}

static bool bif_wait_0(query *q)
{
	while (q->tasks && !q->end_wait) {
		CHECK_INTERRUPT();
		uint64_t now = get_time_in_usec() / 1000;
		query *task = q->tasks;
		unsigned spawn_cnt = 0;
		bool did_something = false;

		while (task) {
			CHECK_INTERRUPT();

			if (task->spawned) {
				spawn_cnt++;

				if (spawn_cnt >= /*g_cpu_count*/64)
					break;
			}

			if (task->tmo_msecs && !task->error) {
				if (now <= task->tmo_msecs) {
					task = task->next;
					continue;
				}

				task->tmo_msecs = 0;
			}

			if (!task->yielded || !task->st.instr || task->error) {
				query *save = task;
				task = pop_task(q, task);
				query_destroy(save);
				continue;
			}

			start(task);
			task = task->next;
			did_something = true;
		}

		if (!did_something)
			msleep(1);
	}

	q->end_wait = false;
	return true;
}

static bool bif_await_0(query *q)
{
	while (q->tasks) {
		CHECK_INTERRUPT();
		pl_uint now = get_time_in_usec() / 1000;
		query *task = q->tasks;
		unsigned spawn_cnt = 0;
		bool did_something = false;

		while (task) {
			CHECK_INTERRUPT();

			if (task->spawned) {
				spawn_cnt++;

				if (spawn_cnt >= /*g_cpu_count*/64)
					break;
			}

			if (task->tmo_msecs && !task->error) {
				if (now <= task->tmo_msecs) {
					task = task->next;
					continue;
				}

				task->tmo_msecs = 0;
			}

			if (!task->yielded || !task->st.instr || task->error) {
				query *save = task;
				task = pop_task(q, task);
				query_destroy(save);
				continue;
			}

			start(task);

			if (!task->tmo_msecs && task->yielded) {
				did_something = true;
				break;
			}
		}

		if (!did_something)
			msleep(1);
		else
			break;
	}

	if (!q->tasks)
		return false;

	checked(push_choice(q));
	return true;
}

static bool bif_yield_0(query *q)
{
	if (q->retry)
		return true;

	return do_yield(q, 0);
}

static bool bif_call_task_n(query *q)
{
	pl_idx save_hp = q->st.hp;
	cell *p0 = clone_term_to_heap(q, q->st.instr, q->st.cur_frame);
	GET_FIRST_RAW_ARG0(p1,callable,p0);
	checked(init_tmp_heap(q));
	checked(clone_term_to_tmp(q, p1, p1_ctx));
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args++ < q->st.instr->arity) {
		GET_NEXT_RAW_ARG(p2,any);
		checked(append_to_tmp(q, p2, p2_ctx));
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->num_cells = tmp_heap_used(q);
	tmp2->arity = arity;
	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2, NULL)) != NULL) {
		tmp2->flags &= ~FLAG_INTERNED_BUILTIN;
	} else if ((tmp2->bif_ptr = get_builtin_term(q->st.m, tmp2, &found, NULL)), found) {
		tmp2->flags |= FLAG_INTERNED_BUILTIN;
	}

	q->st.hp = save_hp;
	cell *tmp = prepare_call(q, CALL_SKIP, tmp2, q->st.cur_frame, 0);
	query *task = query_create_task(q, tmp);
	task->yielded = task->spawned = true;
	push_task(q, task);
	return true;
}

static bool bif_fork_0(query *q)
{
	cell *instr = q->st.instr + q->st.instr->num_cells;
	query *task = query_create_task(q, instr);
	task->yielded = true;
	push_task(q, task);
	return false;
}

static bool bif_sys_cancel_future_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	uint64_t future = get_smalluint(p1);

	for (query *task = q->tasks; task; task = task->next) {
		if (task->future == future) {
			task->error = true;
			break;
		}
	}

	return true;
}

static bool bif_sys_set_future_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	q->future = get_smalluint(p1);
	return true;
}

static bool bif_send_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	query *dstq = q->parent && !q->parent->done ? q->parent : q;
	checked(init_tmp_heap(q));
	cell *c = clone_term_to_tmp(q, p1, p1_ctx);
	checked(c);

	for (pl_idx i = 0; i < c->num_cells; i++) {
		cell *c2 = c + i;
		share_cell(c2);
	}

	checked(alloc_queuen(dstq, 0, c));
	q->yielded = true;
	return true;
}

static bool bif_recv_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	while (true) {
		CHECK_INTERRUPT();
		cell *c = pop_queue(q);
		if (!c) break;

		if (unify(q, p1, p1_ctx, c, q->st.cur_frame))
			return true;

		checked(alloc_queuen(q, 0, c));
	}

	return false;
}

builtins g_tasks_bifs[] =
{
	{"call_task", 1, bif_call_task_n, ":callable", false, false, BLAH},
	{"call_task", 2, bif_call_task_n, ":callable,?term", false, false, BLAH},
	{"call_task", 3, bif_call_task_n, ":callable,?term,?term", false, false, BLAH},
	{"call_task", 4, bif_call_task_n, ":callable,?term,?term,?term", false, false, BLAH},
	{"call_task", 5, bif_call_task_n, ":callable,?term,?term,?term,?term", false, false, BLAH},
	{"call_task", 6, bif_call_task_n, ":callable,?term,?term,?term,?term,?term", false, false, BLAH},
	{"call_task", 7, bif_call_task_n, ":callable,?term,?term,?term,?term,?term,?term", false, false, BLAH},
	{"call_task", 8, bif_call_task_n, ":callable,?term,?term,?term,?term,?term,?term,?term", false, false, BLAH},

	{"end_wait", 0, bif_end_wait_0, NULL, false, false, BLAH},
	{"wait", 0, bif_wait_0, NULL, false, false, BLAH},
	{"await", 0, bif_await_0, NULL, false, false, BLAH},
	{"yield", 0, bif_yield_0, NULL, false, false, BLAH},
	{"fork", 0, bif_fork_0, NULL, false, false, BLAH},
	{"send", 1, bif_send_1, "+term", false, false, BLAH},
	{"recv", 1, bif_recv_1, "?term", false, false, BLAH},

	{"$cancel_future", 1, bif_sys_cancel_future_1, "+integer", false, false, BLAH},
	{"$set_future", 1, bif_sys_set_future_1, "+integer", false, false, BLAH},

	{0}
};
