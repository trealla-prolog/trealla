#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>

#include "heap.h"
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
	if (!q->is_task)
		return true;

	q->yield_at = 0;
	q->yielded = true;
	q->tmo_msecs = get_time_in_usec() / 1000;
	q->tmo_msecs += msecs > 0 ? msecs : 1;
	check_heap_error(push_choice(q));
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
	q->popp += c->nbr_cells;

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

			if (!task->yielded || !task->st.curr_instr || task->error) {
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

			if (!task->yielded || !task->st.curr_instr || task->error) {
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

	check_heap_error(push_choice(q));
	return true;
}

static bool bif_yield_0(query *q)
{
	if (q->retry)
		return true;

	return do_yield(q, 0);
}

static bool bif_task_n(query *q)
{
	pl_idx save_hp = q->st.hp;
	cell *p0 = deep_clone_to_heap(q, q->st.curr_instr, q->st.curr_frame);
	GET_FIRST_RAW_ARG0(p1,callable,p0);
	check_heap_error(init_tmp_heap(q));
	check_heap_error(clone_to_tmp(q, p1, p1_ctx));
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args++ < q->st.curr_instr->arity) {
		GET_NEXT_RAW_ARG(p2,any);
		check_heap_error(append_to_tmp(q, p2, p2_ctx));
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;
	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2, NULL)) != NULL) {
		tmp2->flags &= ~FLAG_BUILTIN;
	} else if ((tmp2->bif_ptr = get_builtin_term(q->st.m, tmp2, &found, NULL)), found) {
		tmp2->flags |= FLAG_BUILTIN;
	}

	q->st.hp = save_hp;
	cell *tmp = prepare_call(q, NOPREFIX_LEN, tmp2, q->st.curr_frame, 0);
	query *task = query_create_task(q, tmp);
	task->yielded = task->spawned = true;
	push_task(q, task);
	return true;
}

static bool bif_fork_0(query *q)
{
	cell *curr_instr = q->st.curr_instr + q->st.curr_instr->nbr_cells;
	query *task = query_create_task(q, curr_instr);
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
	check_heap_error(init_tmp_heap(q));
	cell *c = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(c);

	for (pl_idx i = 0; i < c->nbr_cells; i++) {
		cell *c2 = c + i;
		share_cell(c2);
	}

	check_heap_error(alloc_on_queuen(dstq, 0, c));
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

		if (unify(q, p1, p1_ctx, c, q->st.curr_frame))
			return true;

		check_heap_error(alloc_on_queuen(q, 0, c));
	}

	return false;
}

builtins g_tasks_bifs[] =
{
	{"task", 1, bif_task_n, ":callable", false, false, BLAH},
	{"task", 2, bif_task_n, ":callable,?term", false, false, BLAH},
	{"task", 3, bif_task_n, ":callable,?term,?term", false, false, BLAH},
	{"task", 4, bif_task_n, ":callable,?term,?term,?term", false, false, BLAH},
	{"task", 5, bif_task_n, ":callable,?term,?term,?term,?term", false, false, BLAH},
	{"task", 6, bif_task_n, ":callable,?term,?term,?term,?term,?term", false, false, BLAH},
	{"task", 7, bif_task_n, ":callable,?term,?term,?term,?term,?term,?term", false, false, BLAH},
	{"task", 8, bif_task_n, ":callable,?term,?term,?term,?term,?term,?term,?term", false, false, BLAH},

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
