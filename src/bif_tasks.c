#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>

#include "module.h"
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

// Park a task on a descriptor only where tpl_set_nonblocking() is real,
// otherwise the read would have blocked rather than yielded and we'd
// never get here anyway. Keep this condition identical to that one.

#if !defined(_WIN32) && !defined(__wasi__)
#define USE_POLL 1
#include <poll.h>
#endif

// Longest we sleep in one go.
//
// Where a signal breaks the sleep this only bounds how long an interrupt
// can go unnoticed, and can afford to be generous: SIGINT and the
// SIGALRM behind an expiring alarm both cut poll() and nanosleep() short
// with EINTR, so neither waits this out.
//
// Timeouts are polled rather than signalled, so nothing cuts this sleep
// short on their behalf. That used to force the cap down to 5ms, which
// made every idle scheduler wake two hundred times a second just in
// case. sched_wait() now asks next_alarm_delay() and sleeps exactly
// until the nearest deadline instead, so the cap is back to bounding
// interrupt latency and nothing else.

#define SCHED_MAX_SLEEP_MS 250

// A task parked on a descriptor also carries a deadline, so that a
// wakeup we somehow miss costs latency rather than a hang.

#define SCHED_IO_BACKSTOP_MS 1000

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
	q->tmo_msecs = wall_time_in_usec() / 1000;
	q->tmo_msecs += msecs > 0 ? msecs : 1;
	CHECKED(push_choice(q));
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
	q->tmo_msecs = wall_time_in_usec() / 1000 + 1;
	// Push a choice point with the same result as the goal we hijacked
	// With that we can continue as if the yield didn't happen
	CHECKED(push_choice(q));
	choice *ch = GET_CURR_CHOICE();

	if (status)
		ch->succeed_on_retry = true;
	else
		ch->fail_on_retry = true;

	return false;
}

// yield/0 means "let the others have a turn", not "sleep for a bit", so
// it asks for no deadline at all and the scheduler puts the task back on
// the ready queue as it stands.
//
// That zero deadline is the whole trick: sched_park() only reaches the
// timer heap for a deadline still in the future, so a zero goes
// straight back on the ready queue.

bool do_yield_now(query *q)
{
#ifdef __wasi__
	if (!q->is_task && !q->pl->is_query)
#else
	if (!q->is_task)
#endif
		return true;

	q->yield_at = 0;
	q->yielded = true;
	q->tmo_msecs = 0;
	CHECKED(push_choice(q));
	return false;
}

void do_yield_at(query *q, unsigned int time_in_ms)
{
	q->yield_at = wall_time_in_usec() / 1000;
	q->yield_at += time_in_ms > 0 ? time_in_ms : 1;
}

// Yield knowing what we are actually waiting for, so the scheduler can
// poll() the descriptor instead of retrying us on a timer.
//
// Two conditions have to hold for the descriptor to mean anything. The
// kernel must have said EAGAIN, which tells us the read really did
// reach it - so the stdio buffer above it is empty and poll() is not
// answering about the wrong buffer. And the stream must not be TLS,
// which keeps decrypted bytes of its own that poll() cannot see. Any
// other reason for stopping - a reset peer says ECONNRESET, and poll()
// then reports nothing readable at all - goes back on the retry timer,
// where a doomed read costs a millisecond rather than the backstop.

bool do_yield_on_stream(query *q, stream *str, bool is_write)
{
#if USE_POLL
	bool would_block = (errno == EAGAIN) || (errno == EWOULDBLOCK);

	if (would_block && q->is_task && str->is_socket && !str->ssl && !str->is_memory) {
		FILE *fp = is_write ? str->fp_out : str->fp_in;
		int fd = fp ? fileno(fp) : -1;

		if (fd >= 0) {
			q->wait_fd = fd;
			q->wait_events = is_write ? POLLOUT : POLLIN;
			q->waiting_io = true;
			return do_yield(q, SCHED_IO_BACKSTOP_MS);
		}
	}
#else
	(void)str;
	(void)is_write;
#endif

	return do_yield(q, 1);
}

// The scheduler holds every task in exactly one of three places: the
// ready FIFO (runnable now), the timer heap (waiting on a deadline), or
// the io list (parked on a descriptor). q->tasks stays the registry of
// all of them, and remains what query_destroy() tears down.

enum {
	SCHED_READY = 0,					// a calloc'd query starts out here
	SCHED_TIMER,
	SCHED_IO
};

struct scheduler_ {
	query *ready_head, *ready_tail;
	query *io_head;
	query **timers;						// min-heap keyed on tmo_msecs
	unsigned timers_used, timers_size;
	uint64_t last_poll;					// when we last looked at the descriptors
#if USE_POLL
	struct pollfd *pfds;				// scratch for one poll() call
	query **pfd_owners;					// ... and who each entry belongs to
	unsigned pfds_size;
#endif
};

static void pop_task(query *q, query *task);

// There is one scheduler per prolog instance, not one per query that
// happened to spawn something. Ownership is tracked separately: a task
// stays on its spawner's q->tasks registry, and every query on the
// chain above it counts it in num_subtasks. So the scheduler answers
// "what can run", and the registry answers "whose is it" - which is
// what lets wait/0 mean "until my own work is done" while the queues
// themselves are shared.

// Which scheduler a query's tasks belong to: the one on its thread
// object. Tasks inherit thread_ptr from whoever spawned them, so a task
// of a thread's query lands on that thread's queues rather than the
// main thread's.

static scheduler *sched_get(query *q)
{
	thread *t = get_self_query(q);

	if (!t->sched)
		t->sched = TPL_calloc(1, sizeof(scheduler));

	return t->sched;
}

void sched_destroy(thread *t)
{
	if (!t->sched)
		return;

	TPL_free(t->sched->timers);
#if USE_POLL
	TPL_free(t->sched->pfds);
	TPL_free(t->sched->pfd_owners);
#endif
	TPL_free(t->sched);
	t->sched = NULL;
}

static void heap_swap(scheduler *s, unsigned a, unsigned b)
{
	query *tmp = s->timers[a];
	s->timers[a] = s->timers[b];
	s->timers[b] = tmp;
	s->timers[a]->heap_idx = a;
	s->timers[b]->heap_idx = b;
}

static void heap_up(scheduler *s, unsigned i)
{
	while (i && (s->timers[(i-1)/2]->tmo_msecs > s->timers[i]->tmo_msecs)) {
		heap_swap(s, i, (i-1)/2);
		i = (i-1) / 2;
	}
}

static void heap_down(scheduler *s, unsigned i)
{
	while (true) {
		unsigned l = (2*i) + 1, r = l + 1, min = i;

		if ((l < s->timers_used) && (s->timers[l]->tmo_msecs < s->timers[min]->tmo_msecs))
			min = l;

		if ((r < s->timers_used) && (s->timers[r]->tmo_msecs < s->timers[min]->tmo_msecs))
			min = r;

		if (min == i)
			break;

		heap_swap(s, i, min);
		i = min;
	}
}

static bool heap_push(scheduler *s, query *task)
{
	if (s->timers_used == s->timers_size) {
		unsigned size = s->timers_size ? s->timers_size * 2 : 8;
		query **timers = TPL_realloc(s->timers, size * sizeof(query*));

		if (!timers)
			return false;

		s->timers = timers;
		s->timers_size = size;
	}

	s->timers[s->timers_used] = task;
	task->heap_idx = s->timers_used++;
	task->sched_where = SCHED_TIMER;
	heap_up(s, task->heap_idx);
	return true;
}

static void heap_remove(scheduler *s, unsigned i)
{
	s->timers_used--;

	if (i != s->timers_used) {
		s->timers[i] = s->timers[s->timers_used];
		s->timers[i]->heap_idx = i;
		heap_down(s, i);
		heap_up(s, i);
	}
}

static void sched_ready_push(scheduler *s, query *task)
{
	task->sched_where = SCHED_READY;
	task->sched_next = NULL;
	task->waiting_io = false;

	if (s->ready_tail)
		s->ready_tail->sched_next = task;
	else
		s->ready_head = task;

	s->ready_tail = task;
}

static query *sched_ready_pop(scheduler *s)
{
	query *task = s->ready_head;

	if (!task)
		return NULL;

	s->ready_head = task->sched_next;

	if (!s->ready_head)
		s->ready_tail = NULL;

	task->sched_next = NULL;
	return task;
}

// Take a task off whichever queue it is currently on. Only needed for
// cancellation, so the linear walks are not on any hot path.

static void sched_unlink(scheduler *s, query *task)
{
	if (task->sched_where == SCHED_TIMER) {
		heap_remove(s, task->heap_idx);
		return;
	}

	query **head = task->sched_where == SCHED_IO ? &s->io_head : &s->ready_head;
	query *prev = NULL;

	for (query *t = *head; t; prev = t, t = t->sched_next) {
		if (t != task)
			continue;

		if (prev)
			prev->sched_next = task->sched_next;
		else
			*head = task->sched_next;

		if ((head == &s->ready_head) && (s->ready_tail == task))
			s->ready_tail = prev;

		break;
	}

	task->sched_next = NULL;
}

// Where a task goes after it has had its turn. A task that errored is
// put straight back on the ready queue so it gets reaped next pass,
// rather than sitting out whatever it asked to wait for.

static void sched_park(scheduler *s, query *task, uint64_t now)
{
	if (!task->error) {
		if (task->waiting_io) {
			task->sched_where = SCHED_IO;
			task->sched_next = s->io_head;
			s->io_head = task;
			return;
		}

		if ((task->tmo_msecs >= now) && heap_push(s, task))
			return;
	}

	sched_ready_push(s, task);
}

static void sched_expire_timers(scheduler *s, uint64_t now)
{
	while (s->timers_used && (s->timers[0]->tmo_msecs < now)) {
		query *task = s->timers[0];
		heap_remove(s, 0);
		sched_ready_push(s, task);
	}
}

// Look at the descriptors tasks are parked on, promoting whatever woke
// or ran out of backstop. With `block` set - nothing else is runnable -
// this sleeps until one of them comes up or until the nearest deadline,
// whichever is sooner, the cap only bounding how long an interrupt can
// go unnoticed. Without it this is a check that returns immediately.

static void sched_wait(query *q, scheduler *s, uint64_t now, bool block)
{
	uint64_t deadline = s->timers_used ? s->timers[0]->tmo_msecs + 1 : 0;
	unsigned n = 0;

#if USE_POLL
	for (query *task = s->io_head; task; task = task->sched_next) {
		if (n == s->pfds_size) {
			unsigned size = s->pfds_size ? s->pfds_size * 2 : 8;
			struct pollfd *pfds = TPL_realloc(s->pfds, size * sizeof(struct pollfd));
			query **owners = pfds ? TPL_realloc(s->pfd_owners, size * sizeof(query*)) : NULL;

			if (!owners) {
				if (pfds) s->pfds = pfds;
				break;
			}

			s->pfds = pfds;
			s->pfd_owners = owners;
			s->pfds_size = size;
		}

		s->pfds[n].fd = task->wait_fd;
		s->pfds[n].events = task->wait_events;
		s->pfds[n].revents = 0;
		s->pfd_owners[n] = task;
		n++;

		if (task->tmo_msecs && (!deadline || (task->tmo_msecs + 1 < deadline)))
			deadline = task->tmo_msecs + 1;
	}
#endif

	// Clamp before narrowing: a long enough sleep/1 would otherwise
	// overflow the int and could turn into poll()'s "block forever".

	uint64_t delta = deadline && (deadline > now) ? deadline - now : 0;
	int tmo = delta > SCHED_MAX_SLEEP_MS ? SCHED_MAX_SLEEP_MS : (int)delta;

	if (!deadline)
		tmo = SCHED_MAX_SLEEP_MS;

	// A pending call_with_time_limit/2 is a deadline too. Without this
	// the sleep would run past it and the timeout would fire late by
	// however much of the cap was left.

	unsigned alarm_ms = 0;

	if (next_alarm_delay(q, &alarm_ms) && ((int)alarm_ms < tmo))
		tmo = (int)alarm_ms;

	if (!block)
		tmo = 0;

	if (!n) {
		if (block)
			msleep(tmo > 0 ? tmo : 1);

		return;
	}

#if USE_POLL
	int ready = poll(s->pfds, n, tmo);
	now = wall_time_in_usec() / 1000;

	// Walk the io list in lockstep with the descriptors we just built
	// from it, promoting anything that woke or whose backstop expired.

	query **pprev = &s->io_head;
	unsigned i = 0;

	while (*pprev) {
		query *task = *pprev;
		bool woken = (ready > 0) && (i < n) && s->pfds[i].revents;
		i++;

		if (!woken && task->tmo_msecs && (task->tmo_msecs >= now)) {
			pprev = &task->sched_next;
			continue;
		}

		*pprev = task->sched_next;
		sched_ready_push(s, task);
	}
#endif
}

// Run until the caller's own subtree is done. There used to be a second
// mode that stopped as soon as one task "signalled" - yielded without
// asking for a deadline - which is how await/0 heard about send/1. All
// three went together, so what is left is the drain.

static void sched_run(query *q)
{
	scheduler *s = get_self_query(q)->sched;

	while (q->num_subtasks && !q->end_wait) {
		CHECK_INTERRUPT();
		uint64_t now = wall_time_in_usec() / 1000;
		sched_expire_timers(s, now);
		query *task = sched_ready_pop(s);

		if (!task) {
			if (!s->timers_used && !s->io_head)
				break;					// nothing left to wake us

			sched_wait(q, s, now, true);
			continue;
		}

		// A yield goes straight back on the ready queue, so a task that
		// yields in a loop would keep this queue occupied and we would
		// never reach the blocking wait above - leaving anyone parked on
		// a descriptor unheard. Look at them anyway, without blocking,
		// and at most once a millisecond so this stays off the hot path.

		if (s->io_head && (now > s->last_poll)) {
			s->last_poll = now;
			sched_wait(q, s, now, false);
		}

		task->tmo_msecs = 0;
		task->waiting_io = false;

		if (!task->yielded || !task->st.instr || task->error) {
			pop_task(task->parent, task);
			query_destroy(task);
			continue;
		}

		start(task);
		sched_park(s, task, now);
	}

	q->end_wait = false;
}

// A task counts against everyone above it, not just its spawner, so
// that a wait/0 high up knows there is still work underneath it even
// when its own direct children have all finished.

static void count_subtask(query *q, int delta)
{
	for (; q; q = q->parent)
		q->num_subtasks += delta;
}

static bool push_task(query *q, query *task)
{
	scheduler *s = sched_get(q);

	if (!s)
		return false;

	task->next = q->tasks;

	if (q->tasks)
		q->tasks->prev = task;

	q->tasks = task;
	count_subtask(q, 1);
	sched_ready_push(s, task);
	return true;
}

static void pop_task(query *q, query *task)
{
	if (task->prev)
		task->prev->next = task->next;

	if (task->next)
		task->next->prev = task->prev;

	if (task == q->tasks)
		q->tasks = task->next;

	count_subtask(q, -1);
}

// Take a query's tasks off the shared queues before it is torn down.
// With a scheduler of its own this was free - the whole thing was
// freed with the query - but the queues outlive any one query now, so
// anything still sitting in them has to be unlinked or it dangles.

void sched_release(query *q)
{
	scheduler *s = get_self_query(q)->sched;

	if (!s)
		return;

	for (query *task = q->tasks; task; task = task->next)
		sched_unlink(s, task);
}

static bool bif_end_wait_0(query *q)
{
	if (q->parent)
		q->parent->end_wait = true;

	return true;
}

static bool bif_wait_0(query *q)
{
	if (get_self_query(q)->sched)
		sched_run(q);

	q->end_wait = false;
	return true;
}

static bool bif_yield_0(query *q)
{
	if (q->retry)
		return true;

	return do_yield_now(q);
}

static bool bif_call_task_n(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if ((p1->val_off == g_colon_s) && (p1->arity == 2)) {
		cell *cm = p1 + 1;
		cm = deref(q, cm, p1_ctx);

		if (!is_atom(cm) && !is_var(cm))
			return throw_error(q, cm, p1_ctx, "type_error", "callable");

		if (!is_var(cm)) {
			module *m = find_module(q->pl, C_STR(q, cm));
			if (m) q->st.m = m;
		}

		p1 += 2;
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;

		if (!is_callable(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "callable");
	}

	int arity = p1->arity, args = 1, xarity = q->st.instr->arity;
	CHECKED(init_tmp_heap(q));
	CHECKED(append_to_tmp(q, p1, p1_ctx));

	while (args++ < xarity) {
		GET_NEXT_ARG(p2,any);
		CHECKED(append_to_tmp(q, p2, p2_ctx));
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->num_cells = tmp_heap_used(q);
	tmp2->arity = arity;

	if (is_cstring(tmp2)) {
		share_cell(tmp2);
		convert_to_literal(q->st.m, tmp2);
	}

	tmp2->match = NULL;
	bool status;

	if (!call_check(q, tmp2, &status, true))
		return status;

	// A task is a query of its own, so the goal has to reach it as a
	// term rather than as references into our frames - a context is a
	// frame index, and means nothing over there. So: clone the assembled
	// goal, which resolves what we have bound; then rebase it into a
	// numbering of its own, which is also what turns the references
	// prepare_call() just made back into plain variables.
	//
	// The clone has to read from somewhere other than the heap it writes
	// to, hence staging the goal first. And it has to happen once, over
	// the whole goal: clone_term_to_tmp() starts a new generation on each
	// call, so cloning argument by argument renumbers each in isolation
	// and lets unrelated variables collide.

	pl_idx num_cells = tmp2->num_cells;
	cell *staged = TPL_malloc(num_cells * sizeof(cell));
	CHECKED(staged);
	copy_cells(staged, tmp2, num_cells);

	if (!init_tmp_heap(q)) {
		TPL_free(staged);
		return throw_error(q, q->st.instr, q->st.cur_ctx, "resource_error", "memory");
	}

	cell *goal = clone_term_to_tmp(q, staged, q->st.cur_ctx);
	TPL_free(staged);
	CHECKED(goal);

	cell *tmp = prepare_call(q, CALL_SKIP, goal, q->st.cur_ctx, 0);
	CHECKED(tmp);
	unsigned num_vars = rebase_term(q, tmp, 0, false);
	query *task = query_create_task_rebased(q, tmp, num_vars);
	CHECKED(task);
	task->yielded = task->spawned = true;
	CHECKED(push_task(q, task));
	return true;
}

static bool bif_fork_0(query *q)
{
	cell *instr = q->st.instr + q->st.instr->num_cells;
	query *task = query_create_task(q, instr);
	task->yielded = true;
	CHECKED(push_task(q, task));
	return false;
}

static bool bif_sys_cancel_future_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	uint64_t future = get_smalluint(p1);

	for (query *task = q->tasks; task; task = task->next) {
		if (task->future == future) {
			task->error = true;

			// Take it off whatever it was waiting for, so the cancel
			// lands on the next pass rather than whenever its
			// descriptor or deadline happens to come up.

			scheduler *s = get_self_query(q)->sched;

			if (s && (task->sched_where != SCHED_READY)) {
				sched_unlink(s, task);
				sched_ready_push(s, task);
			}

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
	{"yield", 0, bif_yield_0, NULL, false, false, BLAH},
	{"fork", 0, bif_fork_0, NULL, false, false, BLAH},

	{"$cancel_future", 1, bif_sys_cancel_future_1, "+integer", false, false, BLAH},
	{"$set_future", 1, bif_sys_set_future_1, "+integer", false, false, BLAH},

	{0}
};
