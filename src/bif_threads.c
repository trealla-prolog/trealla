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

typedef struct {
#ifdef _WIN32
    CRITICAL_SECTION mutex;
#else
    pthread_mutex_t mutex;
#endif
} lock;

static void init_lock(lock *l)
{
#ifdef _WIN32
    InitializeCriticalSection(&l->mutex);
#else
    pthread_mutex_init(&l->mutex, NULL);
#endif
}

static void deinit_lock(lock *l)
{
#ifndef _WIN32
    pthread_mutex_destroy(&l->mutex);
#endif
}

void acquire_lock(lock *l)
{
#ifdef _WIN32
    EnterCriticalSection(&l->mutex);
#else
    pthread_mutex_lock(&l->mutex);
#endif
}

void release_lock(lock *l)
{
#ifdef _WIN32
    LeaveCriticalSection(&l->mutex);
#else
    pthread_mutex_unlock(&l->mutex);
#endif
}

typedef struct msg_ {
	struct msg_ *next;
	unsigned from_chan;
	cell c[];
} msg;

typedef struct {
	const char *filename;
	pl_atomic msg *queue;
	unsigned chan;
	bool active;
	lock guard;
#ifdef _WIN32
    HANDLE id;
#else
    pthread_t id;
    pthread_cond_t cond;
    pthread_mutex_t mutex;
#endif
} pl_thread;

#define MAX_PL_THREADS 64
static pl_thread g_pl_threads[MAX_PL_THREADS] = {0};
static unsigned g_pl_cnt = 1;	// 0 is the primaryinstance

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

// NOTE: current implementation allows for queueing 1 item at a time.

static cell *queue_to_chan(unsigned chan, const cell *c, unsigned from_chan)
{
	pl_thread *t = &g_pl_threads[chan];
	//printf("*** send to chan=%u, nbr_cells=%u\n", chan, c->nbr_cells);
	msg *m = malloc(sizeof(msg) + (sizeof(cell)*c->nbr_cells));

	if (!m)
		return NULL;

	m->from_chan = from_chan;
	dup_cells(m->c, c, c->nbr_cells);
	acquire_lock(&t->guard);
	m->next = (msg*)t->queue;
	t->queue = m;
	release_lock(&t->guard);
	return m->c;
}

static bool do_pl_send(query *q, unsigned chan, cell *p1, pl_idx p1_ctx)
{
	if (chan >= g_pl_cnt)
		return throw_error(q, p1, p1_ctx, "domain_error", "no_such_thread");

	pl_thread *t = &g_pl_threads[chan];

	if (!t->active)
		return throw_error(q, p1, p1_ctx, "domain_error", "no_such_thread");

	check_heap_error(init_tmp_heap(q));
	cell *c = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(c);
	check_heap_error(queue_to_chan(chan, c, q->pl->my_chan));
    resume_thread(t);
	return true;
}

static bool bif_pl_send_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);

	if (is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	return do_pl_send(q, get_smalluint(p1), p2, p2_ctx);
}

static bool do_pl_recv(query *q, unsigned from_chan, cell *p1, pl_idx p1_ctx)
{
	pl_thread *t = &g_pl_threads[q->pl->my_chan];
	uint64_t cnt = 0;
	acquire_lock(&t->guard);

	while (!t->queue) {
		release_lock(&t->guard);
		suspend_thread(t, cnt < 1000 ? 0 : cnt < 10000 ? 1 : cnt < 100000 ? 10 : 100);
		cnt++;
	}

	//printf("*** recv msg nbr_cells=%u\n", t->queue->nbr_cells);

	msg *m = (msg*)t->queue;
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
	t->queue = m->next;
	free(m);
	release_lock(&t->guard);
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

	if (!do_pl_recv(q, from_chan, p2, p2_ctx))
		return false;

	cell tmp;
	make_uint(&tmp, q->curr_chan);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static void *start_routine(pl_thread *t)
{
	prolog *pl = pl_create();
	ensure(pl);
	init_lock(&t->guard);
	pl->my_chan = t->chan;
	pl_consult(pl, t->filename);
	t->active = false;
    return 0;
}

static bool bif_pl_thread_2(query *q)
{
	static bool s_first = true;

	if (s_first) {
		s_first = false;
		pl_thread *t = &g_pl_threads[0];
		init_lock(&t->guard);
		t->active = true;
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
		chan = g_pl_cnt++ % MAX_PL_THREADS;
		t = &g_pl_threads[chan];
	}

	t->active = true;
	t->filename = filename;
	t->chan = chan;

#ifdef _WIN32
    SECURITY_ATTRIBUTES sa = {0};
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = 0;
    sa.bInheritHandle = 0;
    t->id = (void*)_beginthreadex(&sa, 0, (void*)start_routine, (void*)t, 0, NULL);
#else
    pthread_attr_t sa;
    pthread_attr_init(&sa);
    pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
    pthread_create((pthread_t*)&t->id, &sa, (void*)start_routine, (void*)t);
#endif

	msleep(100);
	cell tmp;
	make_uint(&tmp, chan);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_pl_thread_pin_cpu_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	if (is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	unsigned chan = get_smalluint(p1);
	pl_thread *t = &g_pl_threads[chan];
	// Do something here
	return true;
}

static bool bif_pl_thread_set_priority_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	if (is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	unsigned chan = get_smalluint(p1);
	int pri = get_smallint(p2);
	pl_thread *t = &g_pl_threads[chan];
	// Do something here
	return true;
}
#endif

builtins g_threads_bifs[] =
{
#if USE_THREADS
	{"$pl_thread", 2, bif_pl_thread_2, "-integer,+atom", false, false, BLAH},
	{"$pl_thread_pin_cpu", 2, bif_pl_thread_pin_cpu_2, "+integer,+integer", false, false, BLAH},
	{"$pl_thread_set_priority", 2, bif_pl_thread_set_priority_2, "+integer,+integer", false, false, BLAH},
	{"$pl_send", 2, bif_pl_send_2, "+integer,+term", false, false, BLAH},
	{"$pl_recv", 2, bif_pl_recv_2, "-integer,?term", false, false, BLAH},
#endif

	{0}
};
