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

#define is_thread_only(t) (!(t)->is_queue_only && !(t)->is_mutex_only)

typedef struct msg_ {
	struct msg_ *prev, *next;
	int from_chan;
	cell c[];
} msg;

typedef struct pl_thread_ pl_thread;

struct pl_thread_ {
	const char *filename;
	query *q;
	cell *goal, *exit_code, *at_exit;
	msg *queue_head, *queue_tail;
	msg *signal_head, *signal_tail;
	unsigned nbr_vars, at_exit_nbr_vars, locks;
	int chan, locked_by;
	bool init, finished;
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

#define THREAD_DEBUG if (1)

#define is_thread(c) is_thread_or_alias(q, c)
#define is_mutex(c) is_mutex_or_alias(q, c)
#define is_queue(c) is_queue_or_alias(q, c)

#define check_thread(c) check_thread_or_alias(q, c)
#define check_mutex(c) check_mutex_or_alias(q, c)
#define check_queue(c) check_queue_or_alias(q, c)

void thread_initialize(prolog *pl)
{
	int n = new_stream(pl);
	ensure(n >= 0);
	stream *str = &pl->streams[n];
	ensure(str);
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	sl_set(str->alias, strdup("main"), NULL);
	pl_thread *t = &g_pl_threads[n];
	init_lock(&t->guard);
	t->chan = n;
	t->active = true;
	t->is_queue_only = false;
	t->is_mutex_only = false;
	t->finished = false;
	t->init = true;
	t->locked_by = -1;
	t->locks = 0;
#ifdef _WIN32
	t->id = (void*)(size_t)GetCurrentThreadId();
#else
	t->id = pthread_self();
#endif

	t->is_detached = true;
	t->is_exception = false;
	t->signal_head = t->queue_head = NULL;
	t->signal_tail = t->queue_tail = NULL;
	t->at_exit = NULL;
	t->goal = NULL;

	str->fp = (void*)t;
	str->is_thread = true;
	str->is_mutex = false;
	str->is_queue = false;
	str->ignore = false;
	str->chan = n;
}

static bool is_thread_or_alias(query *q, cell *c)
{
	pl_idx c_ctx = 0;

	if (is_var(c))
		return throw_error(q, c, c_ctx, "instantiation_error", "thread_or_alias");

	if (!is_stream(c))
		return throw_error(q, c, c_ctx, "existence_error", "thread_or_alias");

	int chan = get_stream(q, c);
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

	if (!is_stream(c))
		return throw_error(q, c, c_ctx, "existence_error", "mutex_or_alias");

	int chan = get_stream(q, c);
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

	if (!is_stream(c))
		return throw_error(q, c, c_ctx, "existence_error", "queue_or_alias");

	int chan = get_stream(q, c);
	pl_thread *t = &g_pl_threads[chan];

	if (!t->active)
		return throw_error(q, c, c_ctx, "existence_error", "queue_or_alias");

	return true;
}

static bool check_thread_or_alias(query *q, cell *c)
{
	pl_idx c_ctx = 0;

	if (is_var(c))
		return false;

	if (!is_stream(c))
		return false;

	int chan = get_stream(q, c);
	pl_thread *t = &g_pl_threads[chan];
	return !t->is_mutex_only && !t->is_queue_only;
}

static bool check_mutex_or_alias(query *q, cell *c)
{
	pl_idx c_ctx = 0;

	if (is_var(c))
		return false;

	if (!is_stream(c))
		return false;

	int chan = get_stream(q, c);
	pl_thread *t = &g_pl_threads[chan];
	return t->is_mutex_only;
}

static bool check_queue_or_alias(query *q, cell *c)
{
	pl_idx c_ctx = 0;

	if (is_var(c))
		return false;

	if (!is_stream(c))
		return false;

	int chan = get_stream(q, c);
	pl_thread *t = &g_pl_threads[chan];
	return t->is_queue_only;
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
	dup_cells(m->c, c, c->nbr_cells);
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

	if (t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "domain_error", "no_such_thread_or_queue");

	cell *c = deep_clone_to_heap(q, p1, p1_ctx);
	check_heap_error(c);
	rebase_vars(q, c, 0);
	check_heap_error(queue_to_chan(chan, c, q->my_chan, is_signal));
    resume_thread(t);
	return true;
}

static bool bif_pl_send_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,any);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	return do_send_message(q, chan, p2, p2_ctx, false);
}

static bool bif_thread_send_message_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,queue);
	GET_NEXT_ARG(p2,any);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	bool ok = do_send_message(q, chan, p2, p2_ctx, false);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.curr_instr, q->st.curr_frame, 1);
	return ok;
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

		if (t->is_queue_only || t->is_mutex_only)
			continue;

		if (t->id == tid)
			return t;
	}

	printf("*** OOPS\n");
	return &g_pl_threads[0];
}

static bool do_match_message(query *q, unsigned chan, cell *p1, pl_idx p1_ctx, bool is_peek)
{
	pl_thread *t = &g_pl_threads[chan];
	pl_thread *me = get_self();
	uint64_t cnt = 0;

	while (!q->pl->halt) {
		if (is_peek && !t->queue_head)
			return false;

		while (!t->queue_head && !q->pl->halt) {
			suspend_thread(me, cnt < 100 ? 0 : cnt < 1000 ? 1 : cnt < 10000 ? 1 : 10);
			cnt++;
		}

		if (q->pl->halt)
			return false;

		//printf("*** recv msg nbr_cells=%u\n", t->queue_head->c->nbr_cells);

		check_heap_error(push_choice(q));
		check_slot(q, MAX_ARITY);
		try_me(q, MAX_ARITY);
		acquire_lock(&t->guard);
		msg *m = t->queue_head;

		while (m) {
			cell *c = m->c;
			cell *tmp = deep_copy_to_heap(q, c, q->st.fp, false);
			check_heap_error(tmp, release_lock(&t->guard));

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

				drop_choice(q);
				return true;
			}

			undo_me(q);
			m = m->next;
		}

		release_lock(&t->guard);
		drop_choice(q);

		if (is_peek)
			return false;
	}

	return false;
}

static bool bif_thread_get_message_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,queue);
	GET_NEXT_ARG(p2,any);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	bool ok = do_match_message(q, chan, p2, p2_ctx, false);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.curr_instr, q->st.curr_frame, 1);
	return ok;
}

static bool bif_thread_peek_message_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,queue);
	GET_NEXT_ARG(p2,any);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	bool ok = do_match_message(q, chan, p2, p2_ctx, true);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.curr_instr, q->st.curr_frame, 1);
	return ok;
}

static void *start_routine_thread(pl_thread *t)
{
	prolog *pl = pl_create();
	ensure(pl);
	pl->my_chan = t->chan;
	pl_consult(pl, t->filename);
	t->active = false;
	t->finished = false;
    return 0;
}

static bool bif_pl_thread_3(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,list_or_nil);
	char *filename = DUP_STRING(q, p2);
	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p2, p2_ctx, "existence_error", "file");
	}

	int n = new_stream(q->pl);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			sl_set(str->alias, DUP_STRING(q, name), NULL);
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_var(p3))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	pl_thread *t = &g_pl_threads[n];
	t->chan = n;
	t->active = true;
	t->filename = filename;
	t->finished = false;
	t->is_exception = false;
	t->is_queue_only = false;
	t->is_mutex_only = false;

	if (!t->init) {
		init_lock(&t->guard);
		t->init = true;
	}

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

	str->fp = (void*)t;
	str->is_thread = true;
	str->is_queue = false;
	str->is_mutex = false;
	str->chan = n;
	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static void *start_routine_thread_create(pl_thread *t)
{
	execute(t->q, t->goal, t->nbr_vars);

	if (t->at_exit) {
		execute(t->q, t->at_exit, t->at_exit_nbr_vars);
		t->at_exit = NULL;
	}

	t->is_exception = t->q->did_unhandled_excpetion;
	t->finished = true;

	if (!t->is_detached)
		return 0;

	stream_close(t->q, t->chan);

	if (!t->exit_code) {
		query_destroy(t->q);
		t->q = NULL;
	}

	acquire_lock(&t->guard);

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

	t->signal_head = t->queue_head = NULL;
	t->signal_tail = t->queue_tail = NULL;
	t->active = false;
	release_lock(&t->guard);
    return 0;
}

static bool bif_thread_create_3(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,list_or_nil);
	int n = new_stream(q->pl);

	if (n < 0)
		return throw_error(q, p2, p2_ctx, "resource_error", "too_many_streams");

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	cell *p4 = NULL;	// at_exit option
	pl_idx p4_ctx = 0;
	bool is_detached = false, is_alias = false;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			sl_set(str->alias, DUP_STRING(q, name), NULL);
			cell tmp;
			make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

			if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
				return false;

			is_alias = true;
		} else if (!CMP_STRING_TO_CSTR(q, c, "at_exit")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_callable(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			p4 = name;
			p4_ctx = q->latest_ctx;
		} else if (!CMP_STRING_TO_CSTR(q, c, "detached")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (c->arity != 1)
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_interned(name) && (name->val_off == g_true_s))
				is_detached = true;
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_var(p3))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	pl_thread *t = &g_pl_threads[n];
	t->chan = n;
	t->active = true;
	t->is_queue_only = false;
	t->is_mutex_only = false;
	t->finished = false;
	t->locked_by = -1;
	t->locks = 0;

	if (!t->init) {
		init_lock(&t->guard);
		t->init = true;
	}

	str->fp = (void*)t;
	str->is_thread = true;
	str->is_queue = false;
	str->is_mutex = false;
	str->chan = n;

	if (!is_alias) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

		if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return false;
	}

	THREAD_DEBUG DUMP_TERM(" - ", q->st.curr_instr, q->st.curr_frame, 1);
	cell *goal = deep_clone_to_heap(q, p1, p1_ctx);
	check_heap_error(goal);
	t->nbr_vars = rebase_vars(q, goal, 0);
	cell *tmp2 = alloc_on_heap(q, 1+goal->nbr_cells+2);
	check_heap_error(tmp2);
	pl_idx nbr_cells = 0;
	make_struct(tmp2+nbr_cells, g_conjunction_s, bif_iso_conjunction_2, 2, goal->nbr_cells+2);
	SET_OP(tmp2+nbr_cells, OP_XFY);
	nbr_cells++;
	nbr_cells += dup_cells(tmp2+nbr_cells, goal, goal->nbr_cells);
	make_struct(tmp2+nbr_cells++, new_atom(q->pl, "halt"), bif_iso_halt_0, 0, 0);
	make_call(q, tmp2+nbr_cells);

	t->q = query_create(q->st.m, false);
	check_heap_error(t->q);
	t->q->thread_ptr = t;
	//t->q->trace = q->trace;
	t->goal = deep_clone_to_heap(t->q, tmp2, 0);
	check_heap_error(t->goal);
	t->is_detached = false;
	t->is_exception = false;
	t->q->my_chan = n;
	t->signal_head = t->queue_head = NULL;
	t->signal_tail = t->queue_tail = NULL;
	t->at_exit = NULL;

	if (p4) {
		cell *goal = deep_clone_to_heap(q, p4, p4_ctx);
		check_heap_error(goal);
		t->at_exit_nbr_vars = rebase_vars(q, goal, 0);
		cell *tmp2 = alloc_on_heap(q, 1+goal->nbr_cells+2);
		check_heap_error(tmp2);
		pl_idx nbr_cells = 0;
		make_struct(tmp2+nbr_cells, g_conjunction_s, bif_iso_conjunction_2, 2, goal->nbr_cells+2);
		SET_OP(tmp2+nbr_cells, OP_XFY);
		nbr_cells++;
		nbr_cells += dup_cells(tmp2+nbr_cells, goal, goal->nbr_cells);
		make_struct(tmp2+nbr_cells++, new_atom(q->pl, "halt"), bif_iso_halt_0, 0, 0);
		make_call(q, tmp2+nbr_cells);
		//DUMP_TERM("at_exit", tmp2, q->st.curr_frame, 0);
		t->at_exit = deep_clone_to_heap(t->q, tmp2, 0);
	}

#ifdef _WIN32
    SECURITY_ATTRIBUTES sa = {0};
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = 0;
    sa.bInheritHandle = 0;
    t->id = (void*)_beginthreadex(&sa, 0, (void*)start_routine_thread_create, (void*)t, 0, NULL);
#else
    pthread_attr_t sa;
    pthread_attr_init(&sa);

    if (is_detached) {
		pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
		t->is_detached = true;
	}

    pthread_create((pthread_t*)&t->id, &sa, (void*)start_routine_thread_create, (void*)t);
#endif

	THREAD_DEBUG DUMP_TERM(" - ", q->st.curr_instr, q->st.curr_frame, 1);
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
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,callable);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];

	if (!is_thread_only(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "signal,not_thread");

	if (!do_send_message(q, chan, p2, p2_ctx, true))
		return false;

	if (t->q)
		t->q->thread_signal = true;

	resume_thread(t);
	return true;
}

static bool bif_thread_join_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,any);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];

	if (!is_thread_only(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "join,not_thread");

	acquire_lock(&t->guard);

	while (t->signal_head) {
		msg *save = t->signal_head;
		t->signal_head = t->signal_head->next;
		execute(t->q, save->c, MAX_ARITY);
		unshare_cells(save->c, save->c->nbr_cells);
		free(save);
	}

	release_lock(&t->guard);

#ifdef _WIN32
	return false;
#else
	void *retval;

	if (pthread_join((pthread_t)t->id, &retval))
		return false;
#endif

	t->active = false;

	if (t->exit_code) {
		cell *tmp = deep_copy_to_heap(q, t->exit_code, q->st.fp, false);
		t->exit_code = NULL;
		query_destroy(t->q);
		t->q = NULL;
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	} else {
		cell tmp;
		make_atom(&tmp, g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	stream_close(t->q, t->chan);
	acquire_lock(&t->guard);

	while (t->queue_head) {
		msg *save = t->queue_head;
		t->queue_head = t->queue_head->next;
		unshare_cells(save->c, save->c->nbr_cells);
		free(save);
	}

	t->signal_head = t->queue_head = NULL;
	t->signal_tail = t->queue_tail = NULL;
	t->active = false;
	release_lock(&t->guard);
}

static bool bif_thread_cancel_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,thread);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;

	if (chan == 0)
		return throw_error(q, p1, p1_ctx, "permission_error", "detach,thread,main");

	pl_thread *t = &g_pl_threads[chan];

	if (!is_thread_only(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "cancel,not_thread");

	t->q->halt_code = 0;
	t->q->halt = t->q->error = true;
	msleep(10);

	if (!t->active)
		return true;

#ifdef _WIN32
	DWORD exit_code;
	TerminateThread(t->id, &exit_code);
#else
	pthread_cancel(t->id);
#endif

	stream_close(t->q, t->chan);
	query_destroy(t->q);
	acquire_lock(&t->guard);

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

	t->signal_head = t->queue_head = NULL;
	t->signal_tail = t->queue_tail = NULL;
	t->active = false;
	t->q = NULL;
	release_lock(&t->guard);
	return true;
}

static bool bif_thread_detach_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,thread);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;

	if (chan == 0)
		return throw_error(q, p1, p1_ctx, "permission_error", "detach,thread,main");

	pl_thread *t = &g_pl_threads[chan];

	if (!is_thread_only(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "detach,not_thread");

	t->q->halt_code = 0;
	t->q->halt = t->q->error = true;

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

#ifdef _WIN32
	HANDLE id = (void*)GetCurrentThreadId();
#else
	pthread_t id = pthread_self();
#endif

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		pl_thread *t = &g_pl_threads[i];

		if (t->is_queue_only || t->is_mutex_only)
			continue;

		if (t->id == id) {
			cell tmp;
			make_int(&tmp, (int)i);
			tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
			bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
			return ok;
		}
	}

	return false;
}

static bool bif_thread_sleep_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,number);
	int ms = (int)((is_float(p1) ? get_float(p1) : get_smallint(p1)) * 1000);
	msleep(ms);
	return true;
}

static bool bif_thread_yield_0(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);

#ifdef _WIN32
#elif 0
	pthread_yield();
#else
	msleep(0);
#endif

	return true;
}

static bool bif_thread_exit_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,nonvar);
	check_heap_error(init_tmp_heap(q));
	cell *tmp_p1 = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(tmp_p1);
	rebase_vars(q, tmp_p1, 0);
	cell *tmp = alloc_on_heap(q, 1+tmp_p1->nbr_cells);
	check_heap_error(tmp);
	make_struct(tmp, new_atom(q->pl, "exited"), NULL, 1, tmp_p1->nbr_cells);
	dup_cells(tmp+1, tmp_p1, tmp_p1->nbr_cells);

#ifdef _WIN32
	HANDLE tid = (void*)GetCurrentThreadId();
#else
	pthread_t tid = pthread_self();
#endif

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		pl_thread *t = &g_pl_threads[i];

		if (t->is_queue_only || t->is_mutex_only)
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

static bool do_thread_property_pin_both(query *q)
{
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,nonvar);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];
	stream *str = &q->pl->streams[chan];

	if (p2->arity != 1)
		return throw_error(q, p2, p2_ctx, "domain_error", "thread_property");

	cell *c = deref(q, p2, p2_ctx);
	pl_idx c_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, p2, "alias")) {
		sliter *iter = sl_first(str->alias);

		if (!sl_next(iter, NULL))
			return false;

		const char *alias = sl_key(iter);
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, c, c_ctx, tmp, q->st.curr_frame))
			return false;

		unshare_cell(tmp+1);
		sl_done(iter);
		return true;
	} else if (!CMP_STRING_TO_CSTR(q, p2, "detached")) {
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "detached"), NULL, 1, 1);
		make_atom(tmp+1, t->is_detached?g_true_s:g_false_s);
		return unify(q, c, c_ctx, tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p2, "status")) {
		if (!t->finished) {
			cell *tmp = alloc_on_heap(q, 2);
			make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
			make_atom(tmp+1, new_atom(q->pl, "running"));
			return unify(q, c, c_ctx, tmp, q->st.curr_frame);
		}

		if (t->is_exception) {
			cell *tmp = alloc_on_heap(q, 3);
			make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 2);
			make_struct(tmp+1, new_atom(q->pl, "exception"), NULL, 1, 1);
			make_atom(tmp+2, g_true_s);
			return unify(q, c, c_ctx, tmp, q->st.curr_frame);
		}

		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
		make_atom(tmp+1, t->exit_code?g_false_s:g_true_s);
		return unify(q, c, c_ctx, tmp, q->st.curr_frame);
	} else
		return throw_error(q, p2, p2_ctx, "domain_error", "thread_property");

	return false;
}

static bool do_thread_property_pin_property(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,nonvar);
	unsigned i = 0;

	if (q->retry)
		i = q->st.v1;

	while (++i) {
		if (i == MAX_THREADS)
			return true;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || t->is_mutex_only || t->is_queue_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || t->is_mutex_only || t->is_queue_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		check_heap_error(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

	if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
		return false;

	return do_thread_property_pin_both(q);
}

static bool do_thread_property_pin_id(query *q)
{
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,any);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];
	stream *str = &q->pl->streams[chan];
	unsigned i = 0;

	if (q->retry)
		i = ++q->st.v2;
	else
		q->st.v2 = 0;

	if (i == 0) {
		check_heap_error(push_choice(q));
		sliter *iter = sl_first(str->alias);

		if (!sl_next(iter, NULL))
			return false;

		const char *alias = sl_key(iter);
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		unsigned var_nbr = create_vars(q, 1);
		make_cstring(tmp+1, alias);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	} else if (i == 1) {
		check_heap_error(push_choice(q));
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "detached"), NULL, 1, 1);
		make_atom(tmp+1, t->is_detached?g_true_s:g_false_s);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	} else {
		if (!t->finished) {
			cell *tmp = alloc_on_heap(q, 2);
			make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
			make_atom(tmp+1, new_atom(q->pl, "running"));
			return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
		}

		if (t->is_exception) {
			cell *tmp = alloc_on_heap(q, 3);
			make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 2);
			make_struct(tmp+1, new_atom(q->pl, "exception"), NULL, 1, 1);
			make_atom(tmp+2, g_true_s);
			return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
		}

		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
		make_atom(tmp+1, t->exit_code?g_false_s:g_true_s);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	}
}

static bool do_thread_property_wild(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	unsigned i = 0;

	if (q->retry)
		i = q->st.v1;
	else
		q->st.v2 = -1;

	while (++i) {
		if (i == MAX_THREADS)
			return true;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || t->is_mutex_only || t->is_queue_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || t->is_mutex_only || t->is_queue_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		check_heap_error(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

	if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
		return false;

	return do_thread_property_pin_id(q);
}

static bool bif_thread_property_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_nonvar(p1) && !check_thread(p1))
		return false;

	bool ok = false;

	if (check_thread(p1) && !is_var(p2))
		ok = do_thread_property_pin_both(q);
	else if (check_thread(p1))
		ok = do_thread_property_pin_id(q);
	else if (!is_var(p2))
		ok = do_thread_property_pin_property(q);
	else
		ok = do_thread_property_wild(q);

	THREAD_DEBUG DUMP_TERM(" - ", q->st.curr_instr, q->st.curr_frame, 1);
	return ok;
}

static bool bif_message_queue_create_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = new_stream(q->pl);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	bool is_alias = false;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			sl_set(str->alias, DUP_STRING(q, name), NULL);
			cell tmp;
			make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

			if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
				return false;

			is_alias = true;
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;

		if (is_var(p2))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	pl_thread *t = &g_pl_threads[n];
	t->chan = n;
	t->active = true;
	t->is_queue_only = true;
	t->is_mutex_only = false;
	t->locked_by = -1;
	t->locks = 0;

	if (!t->init) {
		init_lock(&t->guard);
		t->init = true;
	}

	str->fp = (void*)t;
	str->is_thread = false;
	str->is_queue = true;
	str->is_mutex = false;
	str->chan = n;

	if (!is_alias) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

		if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
			return false;
	}

	THREAD_DEBUG DUMP_TERM(" - ", q->st.curr_instr, q->st.curr_frame, 1);
	return true;
}

static bool bif_message_queue_destroy_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,queue);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];

	if (!t->is_queue_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "destroy,not_queue");

	acquire_lock(&t->guard);

	while (t->queue_head) {
		msg *save = t->queue_head;
		t->queue_head = t->queue_head->next;
		unshare_cells(save->c, save->c->nbr_cells);
		free(save);
	}

	t->queue_head = t->queue_tail = NULL;
	t->active = false;
	release_lock(&t->guard);
	bif_iso_close_1(q);
	return true;
}

static bool do_message_queue_property_pin_both(query *q)
{
	GET_FIRST_ARG(p1,queue);
	GET_NEXT_ARG(p2,nonvar);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];
	stream *str = &q->pl->streams[chan];

	if (p2->arity != 1)
		return throw_error(q, p2, p2_ctx, "domain_error", "queue_property");

	cell *c = deref(q, p2, p2_ctx);
	pl_idx c_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, p2, "alias")) {
		sliter *iter = sl_first(str->alias);

		if (!sl_next(iter, NULL))
			return false;

		const char *alias = sl_key(iter);
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, c, c_ctx, tmp, q->st.curr_frame))
			return false;

		unshare_cell(tmp+1);
		sl_done(iter);
		return true;
	} else if (!CMP_STRING_TO_CSTR(q, p2, "size")) {
		sliter *iter = sl_first(str->alias);

		if (!sl_next(iter, NULL))
			return false;

		const char *alias = sl_key(iter);
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "size"), NULL, 1, 1);
		make_int(tmp+1, queue_size(chan));

		if (!unify(q, c, c_ctx, tmp, q->st.curr_frame))
			return false;

		unshare_cell(tmp+1);
		sl_done(iter);
		return true;
	} else
		return throw_error(q, p2, p2_ctx, "domain_error", "queue_property");

	return false;
}

static bool do_message_queue_property_pin_property(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,nonvar);
	unsigned i = 0;

	if (q->retry)
		i = q->st.v1;

	while (++i) {
		if (i == MAX_THREADS)
			return true;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || !t->is_mutex_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || !t->is_mutex_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		check_heap_error(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

	if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
		return false;

	return do_message_queue_property_pin_both(q);
}

static bool do_message_queue_property_pin_id(query *q)
{
	GET_FIRST_ARG(p1,queue);
	GET_NEXT_ARG(p2,any);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];
	stream *str = &q->pl->streams[chan];
	unsigned i = 0;

	if (q->retry)
		i = ++q->st.v2;
	else
		q->st.v2 = 0;

	if (i == 0) {
		check_heap_error(push_choice(q));

		sliter *iter = sl_first(str->alias);

		if (!sl_next(iter, NULL))
			return false;

		const char *alias = sl_key(iter);
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		unsigned var_nbr = create_vars(q, 1);
		make_cstring(tmp+1, alias);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	}

	cell *tmp = alloc_on_heap(q, 2);
	make_struct(tmp, new_atom(q->pl, "size"), NULL, 1, 1);
	make_int(tmp+1, queue_size(chan));
	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool do_message_queue_property_wild(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	unsigned i = 0;

	if (q->retry)
		i = q->st.v1;
	else
		q->st.v2 = -1;

	while (++i) {
		if (i == MAX_THREADS)
			return true;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || !t->is_queue_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || !t->is_queue_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		check_heap_error(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

	if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
		return false;

	return do_message_queue_property_pin_id(q);
}

static bool bif_message_queue_property_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_nonvar(p1) && !check_queue(p1))
		return false;

	if (check_queue(p1) && !is_var(p2))
		return do_message_queue_property_pin_both(q);

	if (check_queue(p1))
		return do_message_queue_property_pin_id(q);

	if (!is_var(p2))
		return do_message_queue_property_pin_property(q);

	return do_message_queue_property_wild(q);
}


static bool bif_mutex_create_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,var);

	if (is_stream(p1))
		return throw_error(q, p1, p1_ctx, "permission_error", "open,source_sink");

	int n = new_stream(q->pl);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	if (is_atom(p1)) {
		stream *str = &q->pl->streams[n];
		if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
		sl_set(str->alias, DUP_STRING(q, p1), NULL);
		pl_thread *t = &g_pl_threads[n];
		t->chan = n;
		t->active = true;
		t->is_mutex_only = true;
		t->is_queue_only = false;
		t->locked_by = -1;
		t->locks = 0;

		if (!t->init) {
			init_lock(&t->guard);
			t->init = true;
		}

		str->fp = (void*)t;
		str->is_thread = false;
		str->is_mutex = true;
		str->is_queue = false;
		str->chan = n;
	}

	GET_NEXT_ARG(p2,list_or_nil);
	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	bool is_alias = false;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			sl_set(str->alias, DUP_STRING(q, name), NULL);
			cell tmp;
			make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

			if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
				return false;

			is_alias = true;
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;

		if (is_var(p2))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	pl_thread *t = &g_pl_threads[n];
	t->chan = n;
	t->active = true;
	t->is_mutex_only = true;
	t->is_queue_only = false;
	t->locked_by = -1;
	t->locks = 0;

	if (!t->init) {
		init_lock(&t->guard);
		t->init = true;
	}

	str->fp = (void*)t;
	str->is_thread = false;
	str->is_mutex = true;
	str->is_queue = false;
	str->chan = n;

	if (is_var(p1) && !is_alias) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

		if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
			return false;
	}

	THREAD_DEBUG DUMP_TERM(" - ", q->st.curr_instr, q->st.curr_frame, 1);
	return true;
}

static bool bif_mutex_destroy_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,mutex);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];

	if (!t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "destroy,not_mutex");

	t->active = false;
	bif_iso_close_1(q);
	return true;
}

static bool bif_mutex_trylock_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,any);

	if (!is_stream(p1) && !is_atom(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "mutex_or_alias");

	if (!is_stream(p1) && is_atom(p1)) {
		int n = new_stream(q->pl);

		if (n < 0)
			return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

		stream *str = &q->pl->streams[n];
		if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
		sl_set(str->alias, DUP_STRING(q, p1), NULL);
		pl_thread *t = &g_pl_threads[n];
		t->chan = n;
		t->active = true;
		t->is_mutex_only = true;
		t->is_queue_only = false;
		t->locked_by = -1;
		t->locks = 0;

		if (!t->init) {
			init_lock(&t->guard);
			t->init = true;
		}

		str->fp = (void*)t;
		str->is_thread = false;
		str->is_mutex = true;
		str->is_queue = false;
		str->chan = n;
	}

	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];

	if (!try_lock(&t->guard))
		return false;

	pl_thread *me = get_self();
	t->locked_by = me->chan;
	t->locks++;
	return true;
}

static bool bif_mutex_lock_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,any);

	if (!is_stream(p1) && !is_atom(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "mutex_or_alias");

	if (!is_stream(p1) && is_atom(p1)) {
		int n = new_stream(q->pl);

		if (n < 0)
			return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

		stream *str = &q->pl->streams[n];
		if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
		sl_set(str->alias, DUP_STRING(q, p1), NULL);
		pl_thread *t = &g_pl_threads[n];
		t->chan = n;
		t->active = true;
		t->is_mutex_only = true;
		t->is_queue_only = false;
		t->locked_by = -1;
		t->locks = 0;

		if (!t->init) {
			init_lock(&t->guard);
			t->init = true;
		}

		str->fp = (void*)t;
		str->is_thread = false;
		str->is_mutex = true;
		str->is_queue = false;
		str->chan = n;
	}

	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];
	pl_thread *me = get_self();
	acquire_lock(&t->guard);
	t->locked_by = me->chan;
	t->locks++;
	return true;
}

static bool bif_mutex_unlock_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,mutex);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];
	pl_thread *me = get_self();

	if (t->locked_by != me->chan)
		return throw_error(q, p1, p1_ctx, "permission_error", "mutex_unlock,not_locked_by_me");

	if (--t->locks == 0)
		t->locked_by = -1;

	release_lock(&t->guard);
	return true;
}

static bool bif_mutex_unlock_all_0(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);

#ifdef _WIN32
	HANDLE id = (void*)GetCurrentThreadId();
#else
	pthread_t id = pthread_self();
#endif

	pl_thread *me = get_self();

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		pl_thread *t = &g_pl_threads[i];

		if (!t->active)
			continue;

		if (t != me)
			continue;

		release_lock(&t->guard);
		t->locked_by = -1;
		t->locks = 0;
	}

	return true;
}

static bool do_mutex_property_pin_both(query *q)
{
	GET_FIRST_ARG(p1,mutex);
	GET_NEXT_ARG(p2,nonvar);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];
	stream *str = &q->pl->streams[chan];

	if (p2->arity != 1)
		return throw_error(q, p2, p2_ctx, "domain_error", "mutex_property");

	cell *c = deref(q, p2, p2_ctx);
	pl_idx c_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, p2, "alias")) {
		sliter *iter = sl_first(str->alias);

		if (!sl_next(iter, NULL))
			return false;

		const char *alias = sl_key(iter);
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, c, c_ctx, tmp, q->st.curr_frame))
			return false;

		unshare_cell(tmp+1);
		sl_done(iter);
		return true;
	} else if (!CMP_STRING_TO_CSTR(q, p2, "status")) {
		if (t->locks == 0) {
			cell *tmp = alloc_on_heap(q, 2);
			make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
			make_atom(tmp+1, new_atom(q->pl, "unlocked"));
			return unify(q, c, c_ctx, tmp, q->st.curr_frame);
		}

		cell *tmp = alloc_on_heap(q, 4);
		make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 3);
		make_struct(tmp+1, new_atom(q->pl, "locked"), NULL, 2, 2);
		make_int(tmp+2, t->locked_by);
		tmp[2].flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		make_int(tmp+3, t->locks);
		return unify(q, c, c_ctx, tmp, q->st.curr_frame);
	} else
		return throw_error(q, p2, p2_ctx, "domain_error", "mutex_property");

	return false;
}

static bool do_mutex_property_pin_property(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,nonvar);
	unsigned i = 0;

	if (q->retry)
		i = q->st.v1;

	while (++i) {
		if (i == MAX_THREADS)
			return true;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || !t->is_mutex_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || !t->is_mutex_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		check_heap_error(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

	if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
		return false;

	return do_mutex_property_pin_both(q);
}

static bool do_mutex_property_pin_id(query *q)
{
	GET_FIRST_ARG(p1,mutex);
	GET_NEXT_ARG(p2,any);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];
	stream *str = &q->pl->streams[chan];
	unsigned i = 0;

	if (q->retry)
		i = ++q->st.v2;
	else
		q->st.v2 = 0;

	if (i == 0) {
		check_heap_error(push_choice(q));

		sliter *iter = sl_first(str->alias);

		if (!sl_next(iter, NULL))
			return false;

		const char *alias = sl_key(iter);
		cell *tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		unsigned var_nbr = create_vars(q, 1);
		make_cstring(tmp+1, alias);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	}

	cell *tmp;

	if (t->locks != 0) {
		tmp = alloc_on_heap(q, 4);
		make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 3);
		make_struct(tmp+1, new_atom(q->pl, "locked"), NULL, 2, 2);
		make_int(tmp+2, t->locked_by);
		tmp[2].flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		make_int(tmp+3, t->locks);
	} else {
		tmp = alloc_on_heap(q, 2);
		make_struct(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
		make_atom(tmp+1, new_atom(q->pl, "unlocked"));
	}

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool do_mutex_property_wild(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	unsigned i = 0;

	if (q->retry)
		i = q->st.v1;
	else
		q->st.v2 = -1;

	while (++i) {
		if (i == MAX_THREADS)
			return true;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || !t->is_mutex_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		pl_thread *t = &g_pl_threads[i];

		if (!t->active || !t->is_mutex_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		check_heap_error(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

	if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
		return false;

	return do_mutex_property_pin_id(q);
}

static bool bif_mutex_property_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_nonvar(p1) && !check_mutex(p1))
		return false;

	if (check_mutex(p1) && !is_var(p2))
		return do_mutex_property_pin_both(q);

	if (check_mutex(p1))
		return do_mutex_property_pin_id(q);

	if (!is_var(p2))
		return do_mutex_property_pin_property(q);

	return do_mutex_property_wild(q);
}

static bool bif_pl_thread_pin_cpu_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,integer);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	pl_thread *t = &g_pl_threads[chan];

	if (t->is_queue_only || t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "pin_cpu,not_thread");

	// Do something here
	return true;
}

static bool bif_pl_thread_set_priority_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,integer);
	int chan = get_stream(q, p1);
	if (chan < 0) return true;
	int pri = get_smallint(p2);
	pl_thread *t = &g_pl_threads[chan];

	if (t->is_queue_only || t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "set_priority,not_thread");

	// Do something here
	return true;
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
	check_heap_error(push_choice(q));
	check_slot(q, MAX_ARITY);
	try_me(q, MAX_ARITY);
	cell *c = m->c;
	cell *tmp = deep_clone_to_heap(q, c, q->st.fp);
	check_heap_error(tmp, release_lock(&t->guard));
	q->curr_chan = m->from_chan;

	if (!is_peek) {
		unshare_cells(m->c, m->c->nbr_cells);
		free(m);
	}

	drop_choice(q);
	return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
}

static bool bif_pl_recv_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.curr_instr, q->st.curr_frame, 1);
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,any);
	int from_chan = 0;

	if (is_integer(p1)) {
		from_chan = get_stream(q, p1);

		if (from_chan < 0)
			return throw_error(q, p1, p1_ctx, "domain_error", "no_such_thread");
	}

	pl_thread *t = &g_pl_threads[q->pl->my_chan];

	if (!do_recv_message(q, from_chan, p2, p2_ctx, false))
		return false;

	cell tmp;
	make_int(&tmp, q->curr_chan);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}
#endif

builtins g_threads_bifs[] =
{
#if USE_THREADS
	{"pl_thread", 3, bif_pl_thread_3, "-thread,+atom,+list", false, false, BLAH},
	{"pl_thread_pin_cpu", 2, bif_pl_thread_pin_cpu_2, "+thread,+integer", false, false, BLAH},
	{"pl_thread_set_priority", 2, bif_pl_thread_set_priority_2, "+thread,+integer", false, false, BLAH},
	{"pl_msg_send", 2, bif_pl_send_2, "+thread,+term", false, false, BLAH},
	{"pl_msg_recv", 2, bif_pl_recv_2, "-thread,?term", false, false, BLAH},

	{"thread_create", 3, bif_thread_create_3, ":callable,-thread,+list", false, false, BLAH},
	{"thread_cancel", 1, bif_thread_cancel_1, "+thread", false, false, BLAH},
	{"thread_detach", 1, bif_thread_detach_1, "+thread", false, false, BLAH},
	{"thread_signal", 2, bif_thread_signal_2, "+thread,:callable", false, false, BLAH},
	{"thread_join", 2, bif_thread_join_2, "+thread,-term", false, false, BLAH},
	{"thread_exit", 1, bif_thread_exit_1, "+term", false, false, BLAH},
	{"thread_self", 1, bif_thread_self_1, "-integer", false, false, BLAH},
	{"thread_sleep", 1, bif_thread_sleep_1, "+integer", false, false, BLAH},
	{"thread_yield", 0, bif_thread_yield_0, "", false, false, BLAH},
	{"thread_send_message", 2, bif_thread_send_message_2, "+queue,+term", false, false, BLAH},
	{"thread_get_message", 2, bif_thread_get_message_2, "+queue,?term", false, false, BLAH},
	{"thread_peek_message", 2, bif_thread_peek_message_2, "+queue,?term", false, false, BLAH},
	{"thread_property", 2, bif_thread_property_2, "?thread,?term", false, false, BLAH},

	{"mutex_create", 2, bif_mutex_create_2, "-mutex,+list", false, false, BLAH},
	{"mutex_destroy", 1, bif_mutex_destroy_1, "+mutex", false, false, BLAH},
	{"mutex_trylock", 1, bif_mutex_trylock_1, "+mutex", false, false, BLAH},
	{"mutex_lock", 1, bif_mutex_lock_1, "+mutex", false, false, BLAH},
	{"mutex_unlock", 1, bif_mutex_unlock_1, "+mutex", false, false, BLAH},
	{"mutex_unlock_all", 0, bif_mutex_unlock_all_0, "", false, false, BLAH},
	{"mutex_property", 2, bif_mutex_property_2, "?mutex,?term", false, false, BLAH},

	{"message_queue_create", 2, bif_message_queue_create_2, "-queue,+list", false, false, BLAH},
	{"message_queue_destroy", 1, bif_message_queue_destroy_1, "+queue", false, false, BLAH},
	{"message_queue_property", 2, bif_message_queue_property_2, "?queue,?term", false, false, BLAH},
#endif

	{0}
};
