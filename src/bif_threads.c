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
typedef struct {
	void *id;
	const char *filename;
	cell *in_queue, *out_queue;
	pl_idx q_size, chan;
} pl_thread;

#define MAX_PL_THREADS 64
static pl_thread pl_threads[MAX_PL_THREADS] = {0};
static unsigned pl_cnt = 0;

static cell *alloc_on_pl_in_queue(unsigned chan, const cell *c)
{
	pl_thread *t = &pl_threads[chan];

	if (!t->in_queue) {
		t->in_queue = malloc(sizeof(cell)*t->q_size);
		if (!t->in_queue) return NULL;
	}

	safe_copy_cells(t->in_queue, c, c->nbr_cells);
	return t->in_queue;
}

static cell *alloc_on_pl_out_queue(unsigned chan, const cell *c)
{
	pl_thread *t = &pl_threads[chan];

	if (!t->out_queue) {
		t->out_queue = malloc(sizeof(cell)*t->q_size);
		if (!t->in_queue) return NULL;
	}

	safe_copy_cells(t->out_queue, c, c->nbr_cells);
	return t->out_queue;
}

static bool bif_pl_send_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,nonvar);

	if (has_vars(q, p2, p2_ctx))
		return throw_error(q, p2, p2_ctx, "instantiation_error", "not_sufficiently_instantiated");

	unsigned chan = get_smalluint(p1);
	check_heap_error(init_tmp_heap(q));
	cell *c = deep_clone_to_tmp(q, p2, p2_ctx);
	check_heap_error(c);

	for (pl_idx i = 0; i < c->nbr_cells; i++) {
		cell *c2 = c + i;
		share_cell(c2);
	}

	check_heap_error(alloc_on_pl_in_queue(chan, c));
	return true;
}

static bool bif_pl_recv_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,nonvar);
	return false;
}

static void *start_routine(pl_thread *t)
{
	prolog *pl = pl_create();
	ensure(pl);
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

	uint chan = pl_cnt++;
	pl_thread *t = &pl_threads[chan];
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
	//{"pl_send", 2, bif_pl_send_2, "+integer,+term", false, false, BLAH},
	//{"pl_recv", 2, bif_pl_recv_2, "?integer,?term", false, false, BLAH},
#endif

	{0}
};
