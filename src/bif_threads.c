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
#else
#include <pthread.h>
#include <unistd.h>
#endif
#endif

#if USE_THREADS

typedef enum { QIN=0, QOUT=1, QEND=2 } QUEUE;

typedef struct {
	void *id;
	const char *filename;
	cell *queue[QEND];
	pl_idx queue_size[QEND];
	unsigned chan;
} pl_thread;

#define MAX_PL_THREADS 64
static pl_thread g_pl_threads[MAX_PL_THREADS] = {0};
static unsigned g_pl_cnt = 0;

// NOTE: current implementation allows for queueing 1 item at a time.

static cell *queue_to_chan(unsigned chan, QUEUE inout, const cell *c)
{
	if ((inout != QIN) && (inout != QOUT))
		return NULL;

	pl_thread *t = &g_pl_threads[chan];

	if (!t->queue[inout]) {
		t->queue[inout] = malloc(sizeof(cell)*c->nbr_cells);
		if (!t->queue[inout]) return NULL;
	}

	if (t->queue_size[inout] < c->nbr_cells) {
		t->queue[inout] = realloc(t->queue[inout], sizeof(cell)*c->nbr_cells);
		if (!t->queue[inout]) return NULL;
	}

	safe_copy_cells(t->queue[inout], c, c->nbr_cells);
	t->queue_size[inout] = c->nbr_cells;
	return t->queue[inout];
}

static bool do_pl_recv(query *q, unsigned chan, cell *p1, pl_idx p1_ctx)
{
	pl_thread *t = &g_pl_threads[chan];

	if (!t->queue_size[QOUT])
		return false;

	cell *c = t->queue[QOUT];
	cell *tmp = deep_clone_to_heap(q, c, q->st.curr_frame);
	chk_cells(c, c->nbr_cells);
	t->queue_size[QOUT] = 0;
	return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
}

static bool do_pl_send(query *q, unsigned chan, cell *p1, pl_idx p1_ctx)
{
	check_heap_error(init_tmp_heap(q));
	cell *c = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(c);

	for (pl_idx i = 0; i < c->nbr_cells; i++) {
		cell *c2 = c + i;
		share_cell(c2);
	}

	check_heap_error(queue_to_chan(chan, QIN, c));
	return true;
}

static bool bif_pl_send_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);

	if (has_vars(q, p2, p2_ctx))
		return throw_error(q, p2, p2_ctx, "instantiation_error", "not_sufficiently_instantiated");

	return do_pl_send(q, get_smalluint(p1), p2, p2_ctx);
}

static bool bif_pl_send_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (has_vars(q, p1, p1_ctx))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	return do_pl_send(q, q->pl->chan, p1, p1_ctx);
}

static bool bif_pl_recv_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,any);
	return do_pl_recv(q, get_smalluint(p1), p2, p2_ctx);
}

static bool bif_pl_recv_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return do_pl_recv(q, q->pl->chan, p1, p1_ctx);
}

static void *start_routine(pl_thread *t)
{
	prolog *pl = pl_create();
	ensure(pl);
	pl->chan = t->chan;
	pl_consult(pl, t->filename);
    return 0;
}

static bool bif_pl_consult_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,atom);
	char *filename = DUP_STRING(q, p2);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p2, p2_ctx, "existence_error", "file");
	}

	uint chan = g_pl_cnt++;
	pl_thread *t = &g_pl_threads[chan];
	t->filename = filename;

#ifdef _WIN32
    SECURITY_ATTRIBUTES sa = {0};
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = 0;
    sa.bInheritHandle = 0;
    typedef unsigned(_stdcall * start_routine_t)(void *);
    t->id = _beginthreadex(&sa, 0, (start_routine_t)start_routine, (void*)t, 0, NULL);
#else
    typedef void *(*start_routine_t)(void *);
    pthread_attr_t sa;
    pthread_attr_init(&sa);
    pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
    pthread_create((pthread_t*)&t->id, &sa, (start_routine_t)start_routine, (void*)t);
#endif

	cell tmp;
	make_uint(&tmp, chan);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}
#endif

builtins g_threads_bifs[] =
{
#if USE_THREADS
	{"pl_consult", 2, bif_pl_consult_2, "+integer,+atom", false, false, BLAH},
	{"pl_send", 2, bif_pl_send_2, "+integer,+term", false, false, BLAH},
	{"pl_recv", 2, bif_pl_recv_2, "?integer,?term", false, false, BLAH},
	{"pl_send", 1, bif_pl_send_1, "+term", false, false, BLAH},
	{"pl_recv", 1, bif_pl_recv_1, "?term", false, false, BLAH},
#endif

	{0}
};
