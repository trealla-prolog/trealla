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

#if USE_THREADS
#ifdef _WIN32
#include <process.h>
#include <windows.h>
#define msleep Sleep
#else
#include <pthread.h>
#include <unistd.h>
static void msleep(int ms)
{
	struct timespec tv;
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

typedef struct msg_ {
	struct msg_ *prev, *next;
	unsigned from_chan;
	cell c[];
} msg;

#define is_thread(t) (!(t)->is_queue_only && !(t)->is_mutex_only)

typedef struct pl_thread_ pl_thread;

struct pl_thread_ {
	const char *filename;
	query *q;
	cell *goal, *exit_code, *at_exit;
	msg *queue_head, *queue_tail;
	msg *signal_head, *signal_tail;
	void *locked_by;
	unsigned chan;
	bool init;
	bool is_queue_only, is_mutex_only, is_detached, is_exception;
	pl_atomic bool active;
	lock guard;
#ifdef _WIN32
    HANDLE id;
#else
    pthread_t id;
    pthread_cond_t cond;
    pthread_mutex_t mutex;
#endif
};

static pl_thread g_pl_threads[MAX_THREADS] = {0};
static unsigned g_pl_cnt = 1;		// 0 is the primaryinstance
static unsigned g_pl_any_threads = 0;

#define THREAD_DEBUG if (1)

#define is_threadid(c) is_thread_or_alias(q, c)
#define is_mutexid(c) is_mutex_or_alias(q, c)
#define is_queueid(c) is_queue_or_alias(q, c)

static bool is_thread_or_alias(query *q, cell *c)
{
	pl_idx c_ctx = 0;

	if (is_var(c))
		return throw_error(q, c, c_ctx, "instantiation_error", "thread_or_alias");

	if (!is_smallint(c))
		return throw_error(q, c, c_ctx, "domain_error", "thread_or_alias");

	unsigned chan = get_smalluint(c);

	if (chan >= MAX_THREADS)
		return throw_error(q, c, c_ctx, "domain_error", "thread_or_alias");

	pl_thread *t = &g_pl_threads[chan];

	if (!t->active)
		return throw_error(q, c, c_ctx, "existence_error", "thread_or_alias");

	return true;
}

static bool is_mutex_or_alias(query *q, cell *c)
{
	pl_idx c_ctx = 0;

	if (is_var(c))
		return throw_error(q, c, c_ctx, "instantiation_error", "mutex_or_alias");

	if (!is_smallint(c))
		return throw_error(q, c, c_ctx, "domain_error", "mutex_or_alias");

	unsigned chan = get_smalluint(c);

	if (chan >= MAX_THREADS)
		return throw_error(q, c, c_ctx, "domain_error", "mutex_or_alias");

	pl_thread *t = &g_pl_threads[chan];

	if (!t->active)
		return throw_error(q, c, c_ctx, "existence_error", "mutex_or_alias");

	return true;
}

static bool is_queue_or_alias(query *q, cell *c)
{
	pl_idx c_ctx = 0;

	if (is_var(c))
		return throw_error(q, c, c_ctx, "instantiation_error", "queue_or_alias");

	if (!is_smallint(c))
		return throw_error(q, c, c_ctx, "domain_error", "queue_or_alias");

	unsigned chan = get_smalluint(c);

	if (chan >= MAX_THREADS)
		return throw_error(q, c, c_ctx, "domain_error", "queue_or_alias");

	pl_thread *t = &g_pl_threads[chan];

	if (!t->active)
		return throw_error(q, c, c_ctx, "existence_error", "queue_or_alias");

	return true;
}

static void suspend_thread(pl_thread *t, int ms)
{
#ifdef _WIN32
	SuspendThread(t->id);
#else
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	ts.tv_nsec += 1000 * 1000 * ms;
	pthread_mutex_lock(&t->mutex);
	pthread_cond_timedwait(&t->cond, &t->mutex, &ts);
	pthread_mutex_unlock(&t->mutex);
#endif
}

static void resume_thread(pl_thread *t)
{
#ifdef _WIN32
    ResumeThread(t->id);
#else
    pthread_mutex_lock(&t->mutex);
    pthread_cond_signal(&t->cond);
    pthread_mutex_unlock(&t->mutex);
#endif
}

static unsigned queue_size(unsigned chan)
{
	pl_thread *t = &g_pl_threads[chan];
	acquire_lock(&t->guard);
	unsigned cnt = 0;
	msg *m = t->queue_head;

	while (m) {
		m = m->next;
		cnt++;
	}

	release_lock(&t->guard);
	return cnt;
}

static cell *queue_to_chan(unsigned chan, const cell *c, unsigned from_chan, bool is_signal)
{
	//printf("*** send to chan=%u, nbr_cells=%u\n", chan, c->nbr_cells);
	pl_thread *t = &g_pl_threads[chan];
	msg *m = calloc(1, sizeof(msg) + (sizeof(cell)*c->nbr_cells));

	if (!m)
		return NULL;

	m->from_chan = from_chan;
	copy_cells(m->c, c, c->nbr_cells);
	acquire_lock(&t->guard);

	if (is_signal) {
		if (!t->signal_head) {
			t->signal_head = t->signal_tail = m;
		} else {
			m->prev = t->signal_tail;
			t->signal_tail->next = m;
			t->signal_tail = m;
		}
	} else {
		if (!t->queue_head) {
			t->queue_head = t->queue_tail = m;
		} else {
			m->prev = t->queue_tail;
			t->queue_tail->next = m;
			t->queue_tail = m;
		}
	}

	release_lock(&t->guard);
	return m->c;
}

static bool do_send_message(query *q, unsigned chan, cell *p1, pl_idx p1_ctx, bool is_signal)
{
	pl_thread *t = &g_pl_threads[chan];

	if (!t->active || t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "domain_error", "no_such_thread_or_queue");

	const cell *c = deep_copy_to_heap(q, p1, p1_ctx, false);
	check_heap_error(c);
	check_heap_error(queue_to_chan(chan, c, q->my_chan, is_signal));
    resume_thread(t);
	return true;
}

static bool bif_pl_send_2(query *q)
{
	GET_FIRST_ARG(p1,threadid);
	GET_NEXT_ARG(p2,any);
	unsigned chan = get_smalluint(p1);
	return do_send_message(q, chan, p2, p2_ctx, false);
}

static pl_thread *get_self()
{
#ifdef _WIN32
	HANDLE tid = (void*)GetCurrentThreadId();
#else
	pthread_t tid = pthread_self();
#endif

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		pl_thread *t = &g_pl_threads[i];

		if (!t->active)
			continue;

		if (t->id == tid)
			return t;
	}

	return &g_pl_threads[0];
}

static bool do_match_message(query *q, unsigned chan, cell *p1, pl_idx p1_ctx, bool is_peek)
{
	if (chan >= MAX_THREADS) {
		fprintf(stderr, "*** match %d\n", chan);
		assert(chan < MAX_THREADS);
	}

	pl_thread *t = &g_pl_threads[chan];

	while (!q->pl->halt) {
		if (is_peek && !t->queue_head)
			return false;

		pl_thread *me = get_self();
		uint64_t cnt = 0;

		while (!t->queue_head && !q->pl->halt) {
			suspend_thread(me, cnt < 1000 ? 0 : cnt < 10000 ? 1 : cnt < 100000 ? 10 : 10);
			cnt++;
		}

		if (q->pl->halt)
			return false;

		//printf("*** recv msg nbr_cells=%u\n", t->queue_head->c->nbr_cells);

		try_me(q, MAX_ARITY);
		acquire_lock(&t->guard);
		msg *m = t->queue_head;

		while (m) {
			cell *c = m->c;

			for (unsigned i = 0; i < c->nbr_cells; i++) {
				if (is_ref(&c[i])) {
					c[i].flags &= ~FLAG_VAR_REF;
				}
			}

			cell *tmp = deep_copy_to_heap(q, c, q->st.fp, false);
			check_heap_error(tmp, release_lock(&t->guard));
			unshare_cells(c, c->nbr_cells);

			if (unify(q, p1, p1_ctx, tmp, q->st.curr_frame)) {
				q->curr_chan = m->from_chan;

				if (!is_peek) {
					if (m->prev)
						m->prev->next = m->next;

					if (m->next)
						m->next->prev = m->prev;

					if (t->queue_head == m)
						t->queue_head = m->next;

					if (t->queue_tail == m)
						t->queue_tail = m->prev;
					}

				release_lock(&t->guard);

				if (!is_peek) {
					unshare_cells(m->c, m->c->nbr_cells);
					free(m);
				}

				return true;
			}

			undo_me(q);
			m = m->next;
		}

		release_lock(&t->guard);

		if (is_peek)
			return false;
	}

	return false;
}

static bool bif_thread_get_message_2(query *q)
{
	GET_FIRST_ARG(p1,queueid);
	GET_NEXT_ARG(p2,any);
	unsigned chan = get_smalluint(p1);

	if (!do_match_message(q, chan, p2, p2_ctx, false))
		return false;

	return true;
}

static bool bif_thread_peek_message_2(query *q)
{
	GET_FIRST_ARG(p1,queueid);
	GET_NEXT_ARG(p2,any);
	unsigned chan = get_smalluint(p1);

	if (!do_match_message(q, chan, p2, p2_ctx, true))
		return false;

	return true;
}

static bool bif_thread_send_message_2(query *q)
{
	GET_FIRST_ARG(p1,queueid);
	GET_NEXT_ARG(p2,any);
	unsigned chan = get_smalluint(p1);
	return do_send_message(q, chan, p2, p2_ctx, false);
}

static bool do_recv_message(query *q, unsigned from_chan, cell *p1, pl_idx p1_ctx, bool is_peek)
{
	pl_thread *t = &g_pl_threads[q->pl->my_chan];
	uint64_t cnt = 0;

	while (!t->queue_head && !q->pl->halt) {
		suspend_thread(t, cnt < 1000 ? 0 : cnt < 10000 ? 1 : cnt < 100000 ? 10 : 100);
		cnt++;
	}

	if (!t->queue_head && is_peek)
		return false;

	//printf("*** recv msg nbr_cells=%u\n", t->queue_head->nbr_cells);

	acquire_lock(&t->guard);
	msg *m = t->queue_head;

	if (!is_peek) {
		if (m->next)
			m->next->prev = NULL;

		if (t->queue_head == t->queue_tail)
			t->queue_tail = NULL;

		t->queue_head = m->next;
	}

	release_lock(&t->guard);

	cell *c = m->c;
	try_me(q, MAX_ARITY);

	for (unsigned i = 0; i < c->nbr_cells; i++) {
		if (is_ref(&c[i])) {
			c[i].flags &= ~FLAG_VAR_REF;
		}
	}

	cell *tmp = deep_copy_to_heap(q, c, q->st.fp, false);
	check_heap_error(tmp, release_lock(&t->guard));
	unshare_cells(c, c->nbr_cells);
	q->curr_chan = m->from_chan;

	if (!is_peek) {
		unshare_cells(m->c, m->c->nbr_cells);
		free(m);
	}

	return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
}

static bool bif_pl_recv_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,any);
	unsigned from_chan = 0;

	if (is_integer(p1)) {
		if (is_negative(p1))
			return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

		from_chan = get_smalluint(p1);

		if (from_chan >= g_pl_cnt)
			return throw_error(q, p1, p1_ctx, "domain_error", "no_such_thread");
	}

	pl_thread *t = &g_pl_threads[q->pl->my_chan];

	if (!do_recv_message(q, from_chan, p2, p2_ctx, false))
		return false;

	cell tmp;
	make_uint(&tmp, q->curr_chan);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static void *start_routine_thread(pl_thread *t)
{
	prolog *pl = pl_create();
	ensure(pl);
	pl->my_chan = t->chan;
	pl_consult(pl, t->filename);
	t->active = false;
    return 0;
}

static bool s_first = true;

static bool bif_pl_thread_2(query *q)
{
	if (s_first) {
		s_first = false;
		unsigned chan = g_pl_cnt++;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->chan = 0;
		t->active = true;
		t->init = true;
#ifdef _WIN32
		t->id = (void*)(size_t)GetCurrentThreadId();
#else
		t->id = pthread_self();
#endif
	}

	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,atom);
	char *filename = DUP_STRING(q, p2);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p2, p2_ctx, "existence_error", "file");
	}

	unsigned chan = g_pl_cnt++;
	pl_thread *t = &g_pl_threads[chan];

	while (t->active) {
		chan = g_pl_cnt++ % MAX_THREADS;
		t = &g_pl_threads[chan];
	}

	if (!t->init) {
		init_lock(&t->guard);
		t->init = true;
	}

	t->is_exception = false;
	t->active = true;
	t->filename = filename;
	t->chan = chan;

#ifdef _WIN32
    SECURITY_ATTRIBUTES sa = {0};
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = 0;
    sa.bInheritHandle = 0;
    t->id = (void*)_beginthreadex(&sa, 0, (void*)start_routine_thread, (void*)t, 0, NULL);
#else
    pthread_attr_t sa;
    pthread_attr_init(&sa);
    pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
    pthread_create((pthread_t*)&t->id, &sa, (void*)start_routine_thread, (void*)t);
#endif

	cell tmp;
	make_uint(&tmp, chan);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static void *start_routine_thread_create(pl_thread *t)
{
	execute(t->q, t->goal, MAX_ARITY);

	if (t->at_exit) {
		execute(t->q, t->at_exit, MAX_ARITY);
		t->at_exit = NULL;
	}

	t->is_exception = t->q->did_unhandled_excpetion;

	if (!t->exit_code) {
		query_destroy(t->q);
		t->q = NULL;
	}

	while (t->queue_head) {
		msg *save = t->queue_head;
		t->queue_head = t->queue_head->next;
		unshare_cells(save->c, save->c->nbr_cells);
		free(save);
	}

	while (t->signal_head) {
		msg *save = t->signal_head;
		t->signal_head = t->signal_head->next;
		unshare_cells(save->c, save->c->nbr_cells);
		free(save);
	}

	t->at_exit = NULL;
	t->signal_head = t->queue_head = NULL;
	t->signal_tail = t->queue_tail = NULL;
    return 0;
}

static bool bif_thread_create_4(query *q)
{
	if (s_first) {
		g_pl_any_threads = 1;
		s_first = false;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->chan = 0;
		t->active = true;
		t->init = true;
#ifdef _WIN32
		t->id = (void*)(size_t)GetCurrentThreadId();
#else
		t->id = pthread_self();
#endif
	}

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,any);
	unsigned chan = g_pl_cnt++;
	pl_thread *t = &g_pl_threads[chan];

	while (t->active) {
		chan = g_pl_cnt++ % MAX_THREADS;
		t = &g_pl_threads[chan];
	}

	if (!t->init) {
		init_lock(&t->guard);
		t->init = true;
	}

	cell *goal = deep_copy_to_heap(q, p1, p1_ctx, false);
	check_heap_error(goal);
	t->q = query_create(q->st.m, false);
	check_heap_error(t->q);
	t->q->thread_ptr = t;
	t->q->trace = q->trace;
	t->goal = deep_clone_to_heap(t->q, goal, 0);
	t->chan = chan;
	t->is_exception = false;
	t->active = true;
	t->q->my_chan = chan;
	t->signal_head = t->queue_head = NULL;
	t->signal_tail = t->queue_tail = NULL;

#ifdef _WIN32
    SECURITY_ATTRIBUTES sa = {0};
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = 0;
    sa.bInheritHandle = 0;
    t->id = (void*)_beginthreadex(&sa, 0, (void*)start_routine_thread_create, (void*)t, 0, NULL);
#else
    pthread_attr_t sa;
    pthread_attr_init(&sa);

    if (is_interned(p3) && (p3->val_off == g_true_s)) {
		pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
		t->is_detached = true;
	} else
		t->is_detached = false;

    pthread_create((pthread_t*)&t->id, &sa, (void*)start_routine_thread_create, (void*)t);
#endif

	cell tmp;
	make_uint(&tmp, chan);

	if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
		return false;

	cell *at_exit = is_nonvar(p4) ? deep_copy_to_heap(q, p4, p4_ctx, false) : NULL;
	if (is_nonvar(p4)) check_heap_error(at_exit);
	t->at_exit = is_nonvar(p4) ? deep_clone_to_heap(t->q, at_exit, 0) : NULL;
	return true;
}

void do_signal(query *q, void *thread_ptr)
{
	pl_thread *t = (pl_thread*)thread_ptr;

	if (!t->signal_head)
		return;

	acquire_lock(&t->guard);
	msg *m = t->signal_head;
	t->signal_head = t->signal_head->next;

	if (!t->signal_head)
		t->signal_tail = NULL;

	release_lock(&t->guard);
	cell *c = m->c;
	pl_idx c_ctx = 0;

	for (unsigned i = 0; i < c->nbr_cells; i++) {
		if (is_ref(&c[i])) {
			c[i].flags &= ~FLAG_VAR_REF;
		}
	}

	cell *tmp = prepare_call(q, true, c, c_ctx, 1);
	ensure(tmp);
	pl_idx nbr_cells = PREFIX_LEN + c->nbr_cells;
	unshare_cells(c, c->nbr_cells);
	free(m);
	make_call_redo(q, tmp+nbr_cells);
	q->st.curr_instr = tmp;
}

static bool bif_thread_signal_2(query *q)
{
	GET_FIRST_ARG(p1,threadid);
	GET_NEXT_ARG(p2,callable);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

	if (!is_thread(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "signal,not_thread");

	if (!do_send_message(q, get_smalluint(p1), p2, p2_ctx, true))
		return false;

	if (t->q)
		t->q->thread_signal = true;

	resume_thread(t);
	return true;
}

static bool bif_thread_join_2(query *q)
{
	GET_FIRST_ARG(p1,threadid);
	GET_NEXT_ARG(p2,integer_or_var);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

	if (!is_thread(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "join,not_thread");

#ifdef _WIN32
	return false;
#else
	void *retval;

	if (pthread_join((pthread_t)t->id, &retval))
		return false;
#endif

	t->active = false;

	if (t->exit_code) {
		cell *tmp = deep_copy_to_heap(q, t->exit_code, 1, false);
		t->exit_code = NULL;
		query_destroy(t->q);
		t->q = NULL;
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	} else {
		cell tmp;
		make_atom(&tmp, g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}
}

static bool bif_thread_cancel_1(query *q)
{
	GET_FIRST_ARG(p1,threadid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

	if (!is_thread(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "cancel,not_thread");

	t->q->halt_code = 0;
	t->q->halt = t->q->error = true;
	resume_thread(t);

	for (int i = 0; i < 1000; i++) {
		if (!t->active)
			break;

		msleep(1);
	}

	if (t->active) {
#ifdef _WIN32
		DWORD exit_code;
		TerminateThread(t->id, &exit_code);
#else
		pthread_cancel(t->id);
#endif
		query_destroy(t->q);
	}

	while (t->queue_head) {
		msg *save = t->queue_head;
		t->queue_head = t->queue_head->next;
		unshare_cells(save->c, save->c->nbr_cells);
		free(save);
	}

	while (t->signal_head) {
		msg *save = t->signal_head;
		t->signal_head = t->signal_head->next;
		unshare_cells(save->c, save->c->nbr_cells);
		free(save);
	}

	t->active = false;
	t->signal_head = t->queue_head = NULL;
	t->signal_tail = t->queue_tail = NULL;
	t->q = NULL;
	return true;
}

static bool bif_thread_detach_1(query *q)
{
	GET_FIRST_ARG(p1,threadid);
	unsigned chan = get_smalluint(p1);

	if (chan == 0)
		return throw_error(q, p1, p1_ctx, "permission_error", "detach,thread,main");

	pl_thread *t = &g_pl_threads[chan];

	if (!is_thread(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "detach,not_thread");

	t->q->halt_code = 0;
	t->q->halt = t->q->error = true;

	for (int i = 0; i < 1000; i++) {
		if (!t->active)
			break;

		msleep(1);
	}

	if (t->active) {
#ifdef _WIN32
		CloseHandle(t->id);
#else
		pthread_detach(t->id);
#endif
	}

	return true;
}

static bool bif_thread_self_1(query *q)
{
	GET_FIRST_ARG(p1,var);

	if (s_first) {
		s_first = false;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->chan = 0;
		t->active = true;
		t->init = true;
#ifdef _WIN32
		t->id = (void*)(size_t)GetCurrentThreadId();
#else
		t->id = pthread_self();
#endif
	}

#ifdef _WIN32
	HANDLE tid = (void*)GetCurrentThreadId();
#else
	pthread_t tid = pthread_self();
#endif

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		pl_thread *t = &g_pl_threads[i];

		if (!t->active)
			continue;

		if (t->id == tid) {
			cell tmp;
			make_uint(&tmp, (unsigned)i);
			return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		}
	}

	return false;
}

static bool bif_thread_sleep_1(query *q)
{
	GET_FIRST_ARG(p1,integer);

	if (s_first) {
		s_first = false;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->chan = 0;
		t->active = true;
		t->init = true;
#ifdef _WIN32
		t->id = (void*)(size_t)GetCurrentThreadId();
#else
		t->id = pthread_self();
#endif
	}

	sleep(get_smallint(p1));
	return true;
}

static bool bif_thread_yield_0(query *q)
{
	if (s_first) {
		s_first = false;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->chan = 0;
		t->active = true;
		t->init = true;
#ifdef _WIN32
		t->id = (void*)(size_t)GetCurrentThreadId();
#else
		t->id = pthread_self();
#endif
	}

#ifdef _WIN32
#elif 0
	pthread_yield();
#else
	sleep(0);
#endif

	return true;
}

static bool bif_thread_exit_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	if (s_first) {
		s_first = false;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->chan = 0;
		t->active = true;
		t->init = true;
#ifdef _WIN32
		t->id = (void*)(size_t)GetCurrentThreadId();
#else
		t->id = pthread_self();
#endif
	}

	check_heap_error(init_tmp_heap(q));
	cell *tmp_p1 = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp_p1);
	cell *tmp = alloc_on_heap(q, 1+tmp_p1->nbr_cells);
	check_heap_error(tmp);
	make_struct(tmp, new_atom(q->pl, "exited"), NULL, 1, tmp_p1->nbr_cells);
	dup_cells_by_ref(tmp+1, tmp_p1, q->st.curr_frame, tmp_p1->nbr_cells);

#ifdef _WIN32
	HANDLE tid = (void*)GetCurrentThreadId();
#else
	pthread_t tid = pthread_self();
#endif

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		pl_thread *t = &g_pl_threads[i];

		if (!t->active)
			continue;

		if (t->id == tid) {
			t->exit_code = tmp;
			q->halt_code = 0;
			q->halt = t->q->error = true;
			return true;
		}
	}

	return false;
}

static bool bif_thread_is_detached_1(query *q)
{
	GET_FIRST_ARG(p1,mutexid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];
	return t->is_detached;
}

static bool bif_thread_is_exception_1(query *q)
{
	GET_FIRST_ARG(p1,mutexid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];
	return t->is_exception;
}

static bool bif_message_queue_create_1(query *q)
{
	if (s_first) {
		s_first = false;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->chan = 0;
		t->active = true;
		t->init = true;
#ifdef _WIN32
		t->id = (void*)(size_t)GetCurrentThreadId();
#else
		t->id = pthread_self();
#endif
	}

	GET_FIRST_ARG(p1,var);
	unsigned chan = g_pl_cnt++;
	pl_thread *t = &g_pl_threads[chan];

	while (t->active) {
		chan = g_pl_cnt++ % MAX_THREADS;
		t = &g_pl_threads[chan];
	}

	if (!t->init) {
		init_lock(&t->guard);
		t->init = true;
	}

	t->chan = chan;
	t->active = true;
	t->is_queue_only = true;
	cell tmp;
	make_uint(&tmp, chan);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_message_queue_destroy_1(query *q)
{
	GET_FIRST_ARG(p1,queueid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

	if (!t->is_queue_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "destroy,not_queue");

	while (t->queue_head) {
		msg *save = t->queue_head;
		t->queue_head = t->queue_head->next;
		unshare_cells(save->c, save->c->nbr_cells);
		free(save);
	}

	t->queue_head = t->queue_tail = NULL;
	t->active = false;
	return true;
}

static bool bif_message_queue_size_2(query *q)
{
	GET_FIRST_ARG(p1,queueid);
	unsigned chan = get_smalluint(p1);
	GET_NEXT_ARG(p2,integer_or_var);
	cell tmp;
	unsigned size = queue_size(chan);;
	make_uint(&tmp, size);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool bif_mutex_create_1(query *q)
{
	if (s_first) {
		s_first = false;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->chan = 0;
		t->active = true;
		t->init = true;
#ifdef _WIN32
		t->id = (void*)(size_t)GetCurrentThreadId();
#else
		t->id = pthread_self();
#endif
	}

	GET_FIRST_ARG(p1,var);
	unsigned chan = g_pl_cnt++;
	pl_thread *t = &g_pl_threads[chan];

	while (t->active) {
		chan = g_pl_cnt++ % MAX_THREADS;
		t = &g_pl_threads[chan];
	}

	if (!t->init) {
		init_lock(&t->guard);
		t->init = true;
	}

	t->chan = chan;
	t->active = true;
	t->is_mutex_only = true;
	cell tmp;
	make_uint(&tmp, chan);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_mutex_destroy_1(query *q)
{
	GET_FIRST_ARG(p1,mutexid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

	if (!t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "destroy,not_mutex");

	t->active = false;
	return true;
}

static bool bif_mutex_trylock_1(query *q)
{
	GET_FIRST_ARG(p1,mutexid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

#ifdef _WIN32
	HANDLE tid = (void*)GetCurrentThreadId();
#else
	pthread_t tid = pthread_self();
#endif

	if (!try_lock(&t->guard))
		return false;

	t->locked_by = (void*)tid;
	return true;
}

static bool bif_mutex_is_locked_1(query *q)
{
	GET_FIRST_ARG(p1,mutexid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

#ifdef _WIN32
	HANDLE tid = (void*)GetCurrentThreadId();
#else
	pthread_t tid = pthread_self();
#endif

	if (!try_lock(&t->guard))
		return false;

	if (t->locked_by == (void*)tid)
		return false;

	return true;
}

static bool bif_mutex_lock_1(query *q)
{
	GET_FIRST_ARG(p1,mutexid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

#ifdef _WIN32
	HANDLE tid = (void*)GetCurrentThreadId();
#else
	pthread_t tid = pthread_self();
#endif

	acquire_lock(&t->guard);
	t->locked_by = (void*)tid;
	return true;
}

static bool bif_mutex_unlock_1(query *q)
{
	GET_FIRST_ARG(p1,mutexid);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];
	t->locked_by = 0;
	release_lock(&t->guard);
	return true;
}

static bool bif_mutex_unlock_all_0(query *q)
{
#ifdef _WIN32
	HANDLE tid = (void*)GetCurrentThreadId();
#else
	pthread_t tid = pthread_self();
#endif

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		pl_thread *t = &g_pl_threads[i];

		if (!t->active)
			continue;

		if (t->locked_by != (void*)tid)
			continue;

		release_lock(&t->guard);
		t->locked_by = 0;
	}

	return true;
}

static bool bif_pl_thread_pin_cpu_2(query *q)
{
	GET_FIRST_ARG(p1,threadid);
	GET_NEXT_ARG(p2,integer);
	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];

	if (t->is_queue_only || t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "pin_cpu,not_thread");

	// Do something here
	return true;
}

static bool bif_pl_thread_set_priority_2(query *q)
{
	GET_FIRST_ARG(p1,threadid);
	GET_NEXT_ARG(p2,integer);
	unsigned chan = get_smalluint(p1);
	int pri = get_smallint(p2);
	pl_thread *t = &g_pl_threads[chan];

	if (t->is_queue_only || t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "set_priority,not_thread");

	// Do something here
	return true;
}
static bool bif_any_threads_0(query *q)
{
	return g_pl_any_threads != 0;
}

#endif

builtins g_threads_bifs[] =
{
#if USE_THREADS
	{"$pl_thread", 2, bif_pl_thread_2, "-thread,+atom", false, false, BLAH},
	{"$pl_thread_pin_cpu", 2, bif_pl_thread_pin_cpu_2, "+thread,+integer", false, false, BLAH},
	{"$pl_thread_set_priority", 2, bif_pl_thread_set_priority_2, "+thread,+integer", false, false, BLAH},
	{"$pl_msg_send", 2, bif_pl_send_2, "+thread,+term", false, false, BLAH},
	{"pl_msg_recv", 2, bif_pl_recv_2, "-thread,?term", false, false, BLAH},

	{"$thread_create", 4, bif_thread_create_4, ":callable,-thread,+boolean,:callable", false, false, BLAH},
	{"$thread_cancel", 1, bif_thread_cancel_1, "+thread", false, false, BLAH},
	{"$thread_detach", 1, bif_thread_detach_1, "+thread", false, false, BLAH},
	{"$thread_signal", 2, bif_thread_signal_2, "+thread,:callable", false, false, BLAH},
	{"$thread_join", 2, bif_thread_join_2, "+thread,-integer", false, false, BLAH},
	{"$thread_is_detached", 1, bif_thread_is_detached_1, "+thread", false, false, BLAH},
	{"$thread_is_exception", 1, bif_thread_is_exception_1, "+thread", false, false, BLAH},

	{"thread_exit", 1, bif_thread_exit_1, "+term", false, false, BLAH},
	{"thread_self", 1, bif_thread_self_1, "-integer", false, false, BLAH},
	{"thread_sleep", 1, bif_thread_sleep_1, "+integer", false, false, BLAH},
	{"thread_yield", 0, bif_thread_yield_0, "", false, false, BLAH},

	{"$thread_send_message", 2, bif_thread_send_message_2, "+thread,+term", false, false, BLAH},
	{"$thread_get_message", 2, bif_thread_get_message_2, "+thread,?term", false, false, BLAH},
	{"$thread_peek_message", 2, bif_thread_peek_message_2, "+thread,?term", false, false, BLAH},

	{"$message_queue_create", 1, bif_message_queue_create_1, "-thread", false, false, BLAH},
	{"$message_queue_destroy", 1, bif_message_queue_destroy_1, "+thread", false, false, BLAH},
	{"$message_queue_size", 2, bif_message_queue_size_2, "+thread,?integer", false, false, BLAH},

	{"$mutex_create", 1, bif_mutex_create_1, "-thread", false, false, BLAH},
	{"$mutex_destroy", 1, bif_mutex_destroy_1, "+thread", false, false, BLAH},
	{"$mutex_trylock", 1, bif_mutex_trylock_1, "+thread", false, false, BLAH},
	{"$mutex_is_locked", 1, bif_mutex_is_locked_1, "+thread", false, false, BLAH},
	{"$mutex_lock", 1, bif_mutex_lock_1, "+thread", false, false, BLAH},
	{"$mutex_unlock", 1, bif_mutex_unlock_1, "+thread", false, false, BLAH},
	{"mutex_unlock_all", 0, bif_mutex_unlock_all_0, "", false, false, BLAH},

	{"$any_threads", 0, bif_any_threads_0, "", false, false, BLAH},
#endif

	{0}
};
