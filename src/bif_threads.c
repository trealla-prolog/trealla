#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>

#include <sched.h>

#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

#if USE_THREADS

void init_lock(lock *l)
{
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&l->mutex, &attr);
}

void deinit_lock(lock *l)
{
	pthread_mutex_destroy(&l->mutex);
}

bool try_lock(lock *l)
{
	return pthread_mutex_trylock(&l->mutex) == 0;
}

void acquire_lock(lock *l)
{
	pthread_mutex_lock(&l->mutex);
}

void release_lock(lock *l)
{
    pthread_mutex_unlock(&l->mutex);
}

#else

void init_lock(lock *l) {}
void deinit_lock(lock *l) {}
void acquire_lock(lock *l) {}
void release_lock(lock *l) {}

#endif

#if USE_THREADS
static void msleep(int ms)
{
	struct timespec tv = {0};
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}

#define is_thread_only(t) (!(t)->is_queue_only && !(t)->is_mutex_only)

typedef struct msg_ {
	lnode hdr;						// must be first
	int from_chan;
	cell c[];
} msg;

#if 0
#define THREAD_DEBUG if (1) fprintf(stderr, "*** %lld ", (long long)time(NULL));
#else
#define THREAD_DEBUG if (0)
#endif

#define is_thread(c) is_thread_or_alias(q, c)
#define is_mutex(c) is_mutex_or_alias(q, c)
#define is_queue(c) is_queue_or_alias(q, c)

#define check_thread_object(c) check_thread_or_alias_object(q, c)

#define check_thread(c) check_thread_or_alias(q, c)
#define check_mutex(c) check_mutex_or_alias(q, c)
#define check_queue(c) check_queue_or_alias(q, c)

// FIXME: this is too slow. There should be one overall
// alias map, not one per stream.

static int get_named_thread(prolog *pl, const char *name, size_t len)
{
	prolog_lock(pl);

	for (int i = 0; i < MAX_THREADS; i++) {
		thread *t = &pl->threads[i];

		if (!t->is_active || !t->alias)
			continue;

		if (sl_get(t->alias, name, NULL)) {
			prolog_unlock(pl);
			return i;
		}

		if (t->filename && (strlen(t->filename) == len)
			&& !strncmp(t->filename, name, len)) {
			prolog_unlock(pl);
			return i;
		}
	}

	prolog_unlock(pl);
	return -1;
}

static int get_thread(query *q, cell *p1)
{
	if (is_atom(p1)) {
		int n = get_named_thread(q->pl, C_STR(q, p1), C_STRLEN(q, p1));

		if (n < 0)
			return -1;

		return n;
	}

	if (p1->tag != TAG_INT)
		return -1;

	if (!(p1->flags & FLAG_INT_THREAD))
		return -1;

	int n = get_smallint(p1);

	if (!q->pl->threads[n].is_active)
		return -1;

	return n;
}

static int new_thread(prolog *pl)
{
	prolog_lock(pl);

	for (int i = 0; i < MAX_THREADS; i++) {
		unsigned n = pl->thr_cnt++ % MAX_THREADS;
		thread *t = &pl->threads[n];

		if (!t->is_active) {
			t->is_active = true;
			prolog_unlock(pl);

			if (!t->is_init) {
				pthread_cond_init(&t->cond, NULL);
				init_lock(&t->guard);
				t->is_init = true;
			}

			t->id = pthread_self();
			t->pl = pl;
			t->chan = n;
			t->is_queue_only = false;
			t->is_mutex_only = false;
			t->is_finished = false;
			t->locked_by = -1;
			t->num_locks = 0;
			t->is_exception = false;
			t->at_exit = NULL;
			t->goal = NULL;
			return n;
		}
	}

	prolog_unlock(pl);
	assert(0);
	return -1;
}

void thread_initialize(prolog *pl)
{
	int n = new_thread(pl);
	ENSURE(n >= 0);
	thread *t = &pl->threads[n];
	if (!t->alias) t->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	sl_app(t->alias, strdup("main"), NULL);
	t->is_detached = true;
}

void thread_deinitialize(prolog *pl)
{
	for (int i = 0; i < MAX_THREADS; i++) {
		thread *t = &pl->threads[i];

		if (!t->is_init)
			continue;

		sl_destroy(t->alias);
		t->alias = NULL;
	}
}

static bool is_thread_or_alias(query *q, cell *c)
{
	pl_ctx c_ctx = 0;

	if (is_var(c))
		return throw_error(q, c, c_ctx, "instantiation_error", "thread_or_alias");

	int n = get_thread(q, c);

	if (n < 0)
		return throw_error(q, c, c_ctx, "existence_error", "thread_or_alias");

	thread *t = &q->pl->threads[n];

	if (!t->is_active || t->is_mutex_only || t->is_queue_only)
		return throw_error(q, c, c_ctx, "existence_error", "thread_or_alias");

	return true;
}

static bool is_mutex_or_alias(query *q, cell *c)
{
	pl_ctx c_ctx = 0;

	if (is_var(c))
		return throw_error(q, c, c_ctx, "instantiation_error", "mutex_or_alias");

	int n = get_thread(q, c);

	if (n < 0)
		return throw_error(q, c, c_ctx, "existence_error", "mutex_or_alias");

	thread *t = &q->pl->threads[n];

	if (!t->is_active || t->is_queue_only)
		return throw_error(q, c, c_ctx, "existence_error", "mutex_or_alias");

	return true;
}

static bool is_queue_or_alias(query *q, cell *c)
{
	pl_ctx c_ctx = 0;

	if (is_var(c))
		return throw_error(q, c, c_ctx, "instantiation_error", "queue_or_alias");

	int n = get_thread(q, c);

	if (n < 0)
		return throw_error(q, c, c_ctx, "existence_error", "queue_or_alias");

	thread *t = &q->pl->threads[n];

	if (!t->is_active || t->is_mutex_only)
		return throw_error(q, c, c_ctx, "existence_error", "queue_or_alias");

	return true;
}

#if 0
static bool check_thread_or_alias_object(query *q, cell *c)
{
	if (is_var(c))
		return false;

	int n = get_thread(q, c);

	if (n < 0)
		return false;

	return true;
}
#endif

static bool check_thread_or_alias(query *q, cell *c)
{
	if (is_var(c))
		return false;

	int n = get_thread(q, c);

	if (n < 0)
		return false;

	thread *t = &q->pl->threads[n];
	return !t->is_mutex_only && !t->is_queue_only;
}

static bool check_mutex_or_alias(query *q, cell *c)
{
	if (is_var(c))
		return false;

	int n = get_thread(q, c);

	if (n < 0)
		return false;

	thread *t = &q->pl->threads[n];
	return t->is_mutex_only;
}

static bool check_queue_or_alias(query *q, cell *c)
{
	if (is_var(c))
		return false;

	int n = get_thread(q, c);

	if (n < 0)
		return false;

	thread *t = &q->pl->threads[n];
	return t->is_queue_only;
}


static void suspend_thread(thread *t, int ms)
{
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME, &ts);
	ts.tv_nsec += 1000 * 1000 * ms;
	pthread_mutex_lock(&t->mutex);
	pthread_cond_timedwait(&t->cond, &t->mutex, &ts);
	pthread_mutex_unlock(&t->mutex);
}

static void resume_thread(thread *t)
{
    pthread_mutex_lock(&t->mutex);
    pthread_cond_broadcast(&t->cond);
    pthread_mutex_unlock(&t->mutex);
}

static unsigned queue_size(prolog *pl, unsigned chan)
{
	thread *t = &pl->threads[chan];
	unsigned cnt = list_count(&t->queue);
	return cnt;
}

static cell *queue_to_chan(prolog *pl, unsigned chan, const cell *c, unsigned from_chan, bool is_signal)
{
	//printf("*** send to chan=%u, num_cells=%u\n", chan, c->num_cells);
	thread *t = &pl->threads[chan];
	msg *m = calloc(1, sizeof(msg) + (sizeof(cell)*c->num_cells));

	if (!m)
		return NULL;

	m->from_chan = from_chan;
	dup_cells(m->c, c, c->num_cells);
	acquire_lock(&t->guard);

	if (is_signal) {
		list_push_back(&t->signals, m);
	} else {
		list_push_back(&t->queue, m);
	}

	release_lock(&t->guard);
	return m->c;
}

static bool do_send_message(query *q, unsigned chan, cell *p1, pl_ctx p1_ctx, bool is_signal)
{
	thread *t = &q->pl->threads[chan];

	if (t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "domain_error", "no_such_thread_or_queue");

	CHECKED(init_tmp_heap(q));
	cell *c = clone_term_to_tmp(q, p1, p1_ctx);
	CHECKED(c);
	rebase_term(q, c, 0);
	CHECKED(queue_to_chan(q->pl, chan, c, q->my_chan, is_signal));

	if (is_thread_only(t))
		resume_thread(t);

	return true;
}

static bool bif_pl_send_2(query *q)
{
	check_slot(q, MAX_ARITY);
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,any);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	bool ok = do_send_message(q, n, p2, p2_ctx, false);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return ok;
}

static bool bif_thread_send_message_2(query *q)
{
	check_slot(q, MAX_ARITY);
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,queue);
	GET_NEXT_ARG(p2,any);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	bool ok = do_send_message(q, n, p2, p2_ctx, false);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return ok;
}

static thread *get_self(prolog *pl)
{
	pthread_t tid = pthread_self();

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		thread *t = &pl->threads[i];

		if (!t->is_active || t->is_queue_only || t->is_mutex_only)
			continue;

		if (t->id == tid)
			return t;
	}

	return NULL;
}

static bool do_match_message(query *q, unsigned chan, bool is_peek)
{
	GET_FIRST_ARG(pq,queue);
	thread *t = &q->pl->threads[chan];

	while (!q->halt) {
		acquire_lock(&t->guard);

		if (!list_count(&t->queue)) {
			release_lock(&t->guard);

			if (is_peek)
				return false;

			do {
				suspend_thread(t, 10000);
			}
			 while (!list_count(&t->queue) && !q->halt);

			continue;
		}

		msg *m = list_front(&t->queue);
		assert(m);

		while (m) {
			CHECKED(push_choice(q), release_lock(&t->guard));
			CHECKED(check_frame(q, MAX_ARITY));
			try_me(q, MAX_ARITY);
			cell *tmp = copy_term_to_heap(q, m->c, q->st.new_fp, false);	// Copy into thread
			CHECKED(tmp, release_lock(&t->guard));
			GET_FIRST_ARG(p1,queue);
			GET_NEXT_ARG(p2,any);

			if (unify(q, p2, p2_ctx, tmp, q->st.new_fp)) {
				q->curr_chan = m->from_chan;

				if (!is_peek)
					list_remove(&t->queue, m);

				release_lock(&t->guard);

				if (!is_peek) {
					unshare_cells(m->c, m->c->num_cells);
					free(m);
				}

				drop_choice(q);
				return true;
			}

			retry_choice(q);
			m = list_next(m);
		}

		release_lock(&t->guard);

		if (is_peek)
			break;
	}

	return false;
}

static bool bif_thread_get_message_2(query *q)
{
	check_slot(q, MAX_ARITY);
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,queue);
	int n = get_thread(q, p1);

	if (n < 0) {
		THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
		return true;
	}

	bool ok = do_match_message(q, n, false);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return ok;
}

static bool bif_thread_peek_message_2(query *q)
{
	check_slot(q, MAX_ARITY);
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,queue);
	int n = get_thread(q, p1);

	if (n < 0) {
		THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
		return true;
	}

	bool ok = do_match_message(q, n, true);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return ok;
}

static void do_unlock_all(prolog *pl)
{
	thread *me = get_self(pl);

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		thread *t = &pl->threads[i];

		if (!t->is_active)
			continue;

		if (t != me)
			continue;

		release_lock(&t->guard);
		t->locked_by = -1;
		t->num_locks = 0;
	}
}

static void *start_routine_thread(thread *t)
{
	prolog *pl = pl_create();
	ENSURE(pl);
	pl->my_chan = t->chan;
	pl_consult(pl, t->filename);
	t->is_active = false;
	t->is_finished = false;
	t->q = NULL;
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

	int n = new_thread(q->pl);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_threads");

	thread *t = &q->pl->threads[n];
	if (!t->alias) t->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name)) {
				t->is_active = false;
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");
			}

			if (!is_atom(name)) {
				t->is_active = true;
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
			}

			if (get_named_thread(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");
			}

			sl_app(t->alias, DUP_STRING(q, name), NULL);
		} else {
			t->is_active = false;
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_var(p3)) {
			t->is_active = false;
			return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
		}
	}

	t->filename = filename;

    pthread_attr_t sa;
    pthread_attr_init(&sa);
    pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
    pthread_create((pthread_t*)&t->id, &sa, (void*)start_routine_thread, (void*)t);

	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_THREAD;
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static void *start_routine_thread_create(thread *t)
{
	//printf("*** create %d\n", t->chan);
	execute(t->q, t->goal, t->num_vars);
	//printf("*** ~create %d\n", t->chan);
	t->is_exception = t->q->did_unhandled_exception;

	if (t->is_exception) {
		//printf("*** exception, %u\n", t->chan);
		t->ball = calloc(1, (sizeof(cell)*(t->q->ball->num_cells)));
		dup_cells(t->ball, t->q->ball, t->q->ball->num_cells);
		//query *q = t->q;
		//DUMP_TERM("*** ", t->ball, 0, 0);
	}

	if (t->at_exit) {
		//printf("*** at exit, %u\n", t->chan);
		execute(t->q, t->at_exit, t->at_exit_num_vars);
		t->at_exit = NULL;
	}

	t->is_finished = true;
	do_unlock_all(t->pl);

	if (!t->is_detached)
		return 0;

	sl_destroy(t->alias);
	t->alias = NULL;
	query_destroy(t->q);
	t->q = NULL;
	acquire_lock(&t->guard);
	msg *m;

	while ((m = list_pop_front(&t->queue)) != NULL) {
		unshare_cells(m->c, m->c->num_cells);
		free(m);
	}

	while ((m = list_pop_front(&t->signals)) != NULL) {
		unshare_cells(m->c, m->c->num_cells);
		free(m);
	}

	if (t->ball) {
		unshare_cells(t->ball, t->ball->num_cells);
		free(t->ball);
		t->ball = NULL;
	}

	t->is_active = false;
	release_lock(&t->guard);
    return 0;
}

static bool bif_thread_create_3(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,list_or_nil);
	int n = new_thread(q->pl);

	if (n < 0)
		return throw_error(q, p2, p2_ctx, "resource_error", "too_many_threads");

	thread *t = &q->pl->threads[n];
	if (!t->alias) t->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	cell *p4 = NULL;	// at_exit option
	pl_ctx p4_ctx = 0;
	bool is_detached = false, is_alias = false;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_var(c)) {
			t->is_active = false;
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
		}

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name)) {
				t->is_active = false;
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");
			}

			if (!is_atom(name)) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
			}

			if (get_named_thread(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");
			}

			sl_app(t->alias, DUP_STRING(q, name), NULL);
			cell tmp;
			make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

			if (!unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx)) {
				t->is_active = false;
				return false;
			}

			is_alias = true;
		} else if (!CMP_STRING_TO_CSTR(q, c, "at_exit")) {
			if (is_var(name)) {
				t->is_active = false;
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");
			}

			if (!is_callable(name)) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
			}

			p4 = name;
			p4_ctx = q->latest_ctx;
		} else if (!CMP_STRING_TO_CSTR(q, c, "detached")) {
			if (is_var(name)) {
				t->is_active = false;
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");
			}

			if (c->arity != 1) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
			}

			if (is_interned(name) && (name->val_off == g_true_s))
				is_detached = true;
		} else {
			t->is_active = false;
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_var(p3)) {
			t->is_active = false;
			return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
		}
	}

	if (!is_alias) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_THREAD;

		if (!unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx)) {
			t->is_active = false;
			return false;
		}
	}

	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	CHECKED(init_tmp_heap(q));
	cell *goal = clone_term_to_tmp(q, p1, p1_ctx);
	CHECKED(goal);
	t->num_vars = rebase_term(q, goal, 0);
	t->q = query_create_threaded(q->st.m);
	CHECKED(t->q);
	t->q->thread_ptr = t;
	t->q->my_chan = n;
	cell *tmp2 = alloc_heap(t->q, 1+goal->num_cells+1);
	CHECKED(tmp2);
	pl_idx num_cells = 0;
	make_instr(tmp2+num_cells++, g_conjunction_s, bif_iso_conjunction_2, 2, goal->num_cells+1);
	num_cells += dup_cells(tmp2+num_cells, goal, goal->num_cells);
	make_instr(tmp2+num_cells++, new_atom(q->pl, "halt"), bif_iso_halt_0, 0, 0);
	t->goal = tmp2;

	if (p4) {
		CHECKED(init_tmp_heap(q));
		cell *goal = clone_term_to_tmp(q, p4, p4_ctx);
		CHECKED(goal);
		t->at_exit_num_vars = rebase_term(q, goal, 0);
		cell *tmp2 = alloc_heap(q, 1+goal->num_cells+1);
		CHECKED(tmp2);
		pl_idx num_cells = 0;
		make_instr(tmp2+num_cells++, g_conjunction_s, bif_iso_conjunction_2, 2, goal->num_cells+1);
		num_cells += dup_cells(tmp2+num_cells, goal, goal->num_cells);
		make_instr(tmp2+num_cells++, new_atom(q->pl, "halt"), bif_iso_halt_0, 0, 0);
		THREAD_DEBUG DUMP_TERM("at_exit", tmp2, q->st.cur_ctx, 0);
		t->at_exit = clone_term_to_heap(t->q, tmp2, 0);	// Copy into thread
		CHECKED(t->at_exit);
	}

    pthread_attr_t sa;
    pthread_attr_init(&sa);

    if (is_detached) {
		pthread_attr_setdetachstate(&sa, PTHREAD_CREATE_DETACHED);
		t->is_detached = true;
	}

    pthread_create((pthread_t*)&t->id, &sa, (void*)start_routine_thread_create, (void*)t);
	return true;
}

bool do_signal(query *q, void *thread_ptr)
{
	thread *t = (thread*)thread_ptr;
	acquire_lock(&t->guard);

	if (!list_count(&t->signals)) {
		release_lock(&t->guard);
		return false;
	}

	msg *m = list_pop_front(&t->signals);
	release_lock(&t->guard);
	CHECKED(check_frame(q, MAX_ARITY));
	try_me(q, MAX_ARITY);
	THREAD_DEBUG DUMP_TERM("do_signal", m->c, q->st.new_fp, 0);
	cell *c = copy_term_to_heap(q, m->c, q->st.new_fp, false);	// Copy into thread
	unshare_cells(c, c->num_cells);
	free(m);
	cell *tmp = prepare_call(q, CALL_NOSKIP, c, q->st.cur_ctx, 1);
	ENSURE(tmp);
	pl_idx num_cells = c->num_cells;
	make_call(q, tmp+num_cells);
	q->st.instr = tmp;
	return true;
}

static bool bif_thread_signal_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,callable);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (!is_thread_only(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "signal,not_thread");

	if (!do_send_message(q, n, p2, p2_ctx, true)) {
		THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
		return false;
	}

	if (t->q)
		t->q->thread_signal++;

	resume_thread(t);
	return true;
}

static bool bif_thread_join_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,thread);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (!is_thread_only(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "join,not_thread");

	void *retval;

	if (pthread_join((pthread_t)t->id, &retval))
		return throw_error(q, p1, p1_ctx, "system_error", "join,not_thread");

	if (t->exit_code) {
		CHECKED(check_frame(q, MAX_ARITY));
		try_me(q, MAX_ARITY);
		cell *tmp = copy_term_to_heap(q, t->exit_code, q->st.new_fp, false);
		t->exit_code = NULL;
		GET_FIRST_ARG(p1,thread);
		GET_NEXT_ARG(p2,any);
		unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
	} else {
		GET_FIRST_ARG(p1,thread);
		GET_NEXT_ARG(p2,any);
		cell tmp;
		make_instr(&tmp, g_true_s, bif_iso_true_0, 0, 0);
		unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
	}

	acquire_lock(&t->guard);
	sl_destroy(t->alias);
	t->alias = NULL;
	query_destroy(t->q);
	t->q = NULL;
	msg *m;

	while ((m = list_pop_front(&t->queue)) != NULL) {
		unshare_cells(m->c, m->c->num_cells);
		free(m);
	}

	while ((m = list_pop_front(&t->signals)) != NULL) {
		unshare_cells(m->c, m->c->num_cells);
		free(m);
	}

	if (t->ball) {
		unshare_cells(t->ball, t->ball->num_cells);
		free(t->ball);
		t->ball = NULL;
	}

	t->is_active = false;
	release_lock(&t->guard);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static void do_cancel(thread *t)
{
	acquire_lock(&t->guard);
	sl_destroy(t->alias);
	t->alias = NULL;
	t->is_active = false;
	msg *m;
	query *q = t->q;
	pthread_t id = t->id;

	if (list_count(&t->queue)) printf("*** queue...\n");

	while ((m = list_pop_front(&t->queue)) != NULL) {
		DUMP_TERM("***", m->c, q->st.cur_ctx, 0);
		unshare_cells(m->c, m->c->num_cells);
		free(m);
	}

	if (list_count(&t->signals)) printf("*** signals...\n");

	while ((m = list_pop_front(&t->signals)) != NULL) {
		DUMP_TERM("***", m->c, q->st.cur_ctx, 0);
		unshare_cells(m->c, m->c->num_cells);
		free(m);
	}

	if (t->ball) {
		printf("*** ball...\n");
		DUMP_TERM("***", t->ball, q->st.cur_ctx, 0);
		unshare_cells(t->ball, t->ball->num_cells);
		free(t->ball);
		t->ball = NULL;
	}

	query_destroy(t->q);
	t->q = NULL;
	//t->id = 0;
	release_lock(&t->guard);

#if defined(__ANDROID__)
	pthread_kill(id, 0);
#else
	pthread_cancel(id);
#endif
}

static bool bif_thread_cancel_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,thread);
	int n = get_thread(q, p1);
	if (n < 0) return true;

	if (n == 0)
		return throw_error(q, p1, p1_ctx, "permission_error", "detach,thread,main");

	thread *t = &q->pl->threads[n];

	if (!is_thread_only(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "cancel,not_thread");

	do_cancel(t);
	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_thread_detach_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,thread);
	int n = get_thread(q, p1);
	if (n < 0) return true;

	if (n == 0)
		return throw_error(q, p1, p1_ctx, "permission_error", "detach,thread,main");

	thread *t = &q->pl->threads[n];

	if (!is_thread_only(t))
		return throw_error(q, p1, p1_ctx, "permission_error", "detach,not_thread");

	t->q->halt_code = 0;
	t->q->halt = t->q->error = true;

	if (t->is_active)
		pthread_detach(t->id);

	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_thread_self_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,var);
	thread *t = get_self(q->pl);

	if (t != NULL) {
		cell tmp;
		make_int(&tmp, (int)t->chan);
		tmp.flags |= FLAG_INT_THREAD;
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
		THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
		return true;
	}

	//printf("*** no thead_self\n");
	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return false;
}

static bool bif_thread_sleep_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,number);
	int ms = (int)((is_float(p1) ? get_float(p1) : get_smallint(p1)) * 1000);
	msleep(ms);
	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_thread_yield_0(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	sched_yield();
	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_thread_exit_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,nonvar);
	CHECKED(init_tmp_heap(q));
	cell *tmp_p1 = clone_term_to_tmp(q, p1, p1_ctx);
	CHECKED(tmp_p1);
	rebase_term(q, tmp_p1, 0);
	cell *tmp = alloc_heap(q, 1+tmp_p1->num_cells);
	CHECKED(tmp);
	make_instr(tmp, new_atom(q->pl, "exited"), NULL, 1, tmp_p1->num_cells);
	dup_cells(tmp+1, tmp_p1, tmp_p1->num_cells);
	thread * t = get_self(q->pl);

	if (t != NULL) {
		t->exit_code = tmp;
		q->halt_code = 0;
		q->halt = t->q->error = true;
		THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
		return true;
	}

	printf("*** no thead_exit\n");
	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return false;
}

static bool do_thread_property_pin_both(query *q)
{
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,nonvar);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (p2->arity != 1)
		return throw_error(q, p2, p2_ctx, "domain_error", "thread_property");

	cell *c = deref(q, p2, p2_ctx);
	pl_ctx c_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, p2, "alias")) {
		sliter *iter = sl_first(t->alias);

		if (!sl_next(iter, NULL)) {
			sl_done(iter);
			return false;
		}

		const char *alias = sl_key(iter);
		sl_done(iter);
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, c, c_ctx, tmp, q->st.cur_ctx)) {
			unshare_cell(tmp+1);
			return false;
		}

		return true;
	} else if (!CMP_STRING_TO_CSTR(q, p2, "detached")) {
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "detached"), NULL, 1, 1);
		make_atom(tmp+1, t->is_detached?g_true_s:g_false_s);
		return unify(q, c, c_ctx, tmp, q->st.cur_ctx);
	} else if (!CMP_STRING_TO_CSTR(q, p2, "status")) {
		if (t->is_exception) {
			cell *tmp = alloc_heap(q, 2+t->ball->num_cells);
			make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 1+t->ball->num_cells);
			make_instr(tmp+1, new_atom(q->pl, "exception"), NULL, 1, t->ball->num_cells);
			dup_cells(tmp+2, t->ball, t->ball->num_cells);
			return unify(q, c, c_ctx, tmp, q->st.cur_ctx);
		}

		if (!t->is_finished) {
			cell *tmp = alloc_heap(q, 2);
			make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
			make_atom(tmp+1, new_atom(q->pl, "running"));
			return unify(q, c, c_ctx, tmp, q->st.cur_ctx);
		}

		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
		make_atom(tmp+1, t->exit_code?g_false_s:g_true_s);
		return unify(q, c, c_ctx, tmp, q->st.cur_ctx);
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

		thread *t = &q->pl->threads[i];

		if (!t->is_active || t->is_mutex_only || t->is_queue_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		thread *t = &q->pl->threads[i];

		if (!t->is_active || t->is_mutex_only || t->is_queue_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		CHECKED(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_THREAD;
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	return do_thread_property_pin_both(q);
}

static bool do_thread_property_pin_id(query *q)
{
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,any);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];
	unsigned i = 0;

	if (q->retry)
		i = ++q->st.v2;
	else
		q->st.v2 = 0;

	if (i == 0) {
		CHECKED(push_choice(q));
		sliter *iter = sl_first(t->alias);

		if (!sl_next(iter, NULL)) {
			sl_done(iter);
			drop_choice(q);
			return false;
		}

		const char *alias = sl_key(iter);
		sl_done(iter);
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, p2, p2_ctx, tmp, q->st.cur_ctx)) {
			unshare_cell(tmp+1);
			return false;
		}

		return true;
	} else if (i == 1) {
		CHECKED(push_choice(q));
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "detached"), NULL, 1, 1);
		make_atom(tmp+1, t->is_detached?g_true_s:g_false_s);
		return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
	} else {
		if (t->is_exception) {
			cell *tmp = alloc_heap(q, 2+t->ball->num_cells);
			make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 1+t->ball->num_cells);
			make_instr(tmp+1, new_atom(q->pl, "exception"), NULL, 1, t->ball->num_cells);
			dup_cells(tmp+2, t->ball, t->ball->num_cells);
			return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
		}

		if (!t->is_finished) {
			cell *tmp = alloc_heap(q, 2);
			make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
			make_atom(tmp+1, new_atom(q->pl, "running"));
			return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
		}

		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
		make_atom(tmp+1, t->exit_code?g_false_s:g_true_s);
		return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
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

		thread *t = &q->pl->threads[i];

		if (!t->is_active || t->is_mutex_only || t->is_queue_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		thread *t = &q->pl->threads[i];

		if (!t->is_active || t->is_mutex_only || t->is_queue_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		CHECKED(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_THREAD;
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	return do_thread_property_pin_id(q);
}

static bool bif_thread_property_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
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

	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return ok;
}

static bool bif_message_queue_create_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = new_thread(q->pl);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_threads");

	if (is_atom(p1)) {
		thread *t = &q->pl->threads[n];
		if (!t->alias) t->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
		sl_app(t->alias, DUP_STRING(q, p1), NULL);
	}

	thread *t = &q->pl->threads[n];
	if (!t->alias) t->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	bool is_alias = false;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_var(c)) {
			t->is_active = false;
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
		}

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name)) {
				t->is_active = false;
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");
			}

			if (!is_atom(name)) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
			}

			if (get_named_thread(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");
			}

			sl_app(t->alias, DUP_STRING(q, name), NULL);
			cell tmp;
			make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

			if (!unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx)) {
				t->is_active = false;
				return false;
			}

			is_alias = true;
		} else {
			t->is_active = false;
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;

		if (is_var(p2)) {
			t->is_active = false;
			return throw_error(q, p2, p2_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
		}
	}

	t->is_queue_only = true;

	if (is_var(p1) && !is_alias) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_THREAD;

		if (!unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx)) {
			t->is_active = false;
			return false;
		}
	}

	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_message_queue_destroy_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,queue);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (!t->is_queue_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "destroy,not_queue");

	acquire_lock(&t->guard);
	msg *m;

	while ((m = list_pop_front(&t->queue)) != NULL) {
		unshare_cells(m->c, m->c->num_cells);
		free(m);
	}

	sl_destroy(t->alias);
	t->alias = NULL;
	t->is_active = false;
	release_lock(&t->guard);
	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool do_message_queue_property_pin_both(query *q)
{
	GET_FIRST_ARG(p1,queue);
	GET_NEXT_ARG(p2,nonvar);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (p2->arity != 1)
		return throw_error(q, p2, p2_ctx, "domain_error", "queue_property");

	cell *c = deref(q, p2, p2_ctx);
	pl_ctx c_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, p2, "alias")) {
		sliter *iter = sl_first(t->alias);

		if (!sl_next(iter, NULL)) {
			sl_done(iter);
			return false;
		}

		const char *alias = sl_key(iter);
		sl_done(iter);
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, c, c_ctx, tmp, q->st.cur_ctx)) {
			unshare_cell(tmp+1);
			return false;
		}

		return true;
	} else if (!CMP_STRING_TO_CSTR(q, p2, "size")) {
		sliter *iter = sl_first(t->alias);

		if (!sl_next(iter, NULL)) {
			sl_done(iter);
			return false;
		}

		sl_done(iter);
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "size"), NULL, 1, 1);
		make_int(tmp+1, queue_size(q->pl, n));

		if (!unify(q, c, c_ctx, tmp, q->st.cur_ctx))
			return false;

		unshare_cell(tmp+1);
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

		thread *t = &q->pl->threads[i];

		if (!t->is_active || !t->is_mutex_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		thread *t = &q->pl->threads[i];

		if (!t->is_active || !t->is_mutex_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		CHECKED(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_THREAD;
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	return do_message_queue_property_pin_both(q);
}

static bool do_message_queue_property_pin_id(query *q)
{
	GET_FIRST_ARG(p1,queue);
	GET_NEXT_ARG(p2,any);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];
	unsigned i = 0;

	if (q->retry)
		i = ++q->st.v2;
	else
		q->st.v2 = 0;

	if (i == 0) {
		CHECKED(push_choice(q));
		sliter *iter = sl_first(t->alias);

		if (!sl_next(iter, NULL)) {
			sl_done(iter);
			drop_choice(q);
			return false;
		}

		const char *alias = sl_key(iter);
		sl_done(iter);
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, p2, p2_ctx, tmp, q->st.cur_ctx)) {
			unshare_cell(tmp+1);
			return false;
		}

		return true;
	}

	cell *tmp = alloc_heap(q, 2);
	make_instr(tmp, new_atom(q->pl, "size"), NULL, 1, 1);
	make_int(tmp+1, queue_size(q->pl, n));
	return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
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

		thread *t = &q->pl->threads[i];

		if (!t->is_active || !t->is_queue_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		thread *t = &q->pl->threads[i];

		if (!t->is_active || !t->is_queue_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		CHECKED(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_THREAD;
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	return do_message_queue_property_pin_id(q);
}

static bool bif_message_queue_property_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
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
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = new_thread(q->pl);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_threads");

	if (is_atom(p1)) {
		thread *t = &q->pl->threads[n];
		if (!t->alias) t->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
		sl_app(t->alias, DUP_STRING(q, p1), NULL);
	}

	thread *t = &q->pl->threads[n];
	if (!t->alias) t->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	bool is_alias = false;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_var(c)) {
			t->is_active = false;
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
		}

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name)) {
				t->is_active = false;
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");
			}

			if (!is_atom(name)) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
			}

			if (get_named_thread(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0) {
				t->is_active = false;
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");
			}

			sl_app(t->alias, DUP_STRING(q, name), NULL);
			cell tmp;
			make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

			if (!unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx)) {
				t->is_active = false;
				return false;
			}

			is_alias = true;
		} else {
			t->is_active = false;
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;

		if (is_var(p2)) {
			t->is_active = false;
			return throw_error(q, p2, p2_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
		}
	}

	t->is_mutex_only = true;

	if (is_var(p1) && !is_alias) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_THREAD;

		if (!unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx)) {
			t->is_active = false;
			return false;
		}
	}

	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_mutex_destroy_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,mutex);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (!t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "destroy,not_mutex");

	sl_destroy(t->alias);
	t->alias = NULL;
	t->is_active = false;
	THREAD_DEBUG DUMP_TERM(" - ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_mutex_trylock_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,mutex);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (!try_lock(&t->guard))
		return false;

	thread *me = get_self(q->pl);
	assert(me);
	t->locked_by = me->chan;
	t->num_locks++;
	return true;
}

static bool bif_mutex_lock_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,mutex);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];
	thread *me = get_self(q->pl);
	assert(me);
	acquire_lock(&t->guard);
	t->locked_by = me->chan;
	t->num_locks++;
	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_mutex_unlock_1(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,mutex);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];
	thread *me = get_self(q->pl);
	assert(me);

	if (t->locked_by != me->chan)
		return throw_error(q, p1, p1_ctx, "permission_error", "mutex_unlock,not_locked_by_me");

	if (--t->num_locks == 0)
		t->locked_by = -1;

	release_lock(&t->guard);
	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool bif_mutex_unlock_all_0(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	do_unlock_all(q->pl);
	THREAD_DEBUG DUMP_TERM(" -  ", q->st.instr, q->st.cur_ctx, 1);
	return true;
}

static bool do_mutex_property_pin_both(query *q)
{
	GET_FIRST_ARG(p1,mutex);
	GET_NEXT_ARG(p2,nonvar);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (p2->arity != 1)
		return throw_error(q, p2, p2_ctx, "domain_error", "mutex_property");

	cell *c = deref(q, p2, p2_ctx);
	pl_ctx c_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, p2, "alias")) {
		sliter *iter = sl_first(t->alias);

		if (!sl_next(iter, NULL)) {
			sl_done(iter);
			return false;
		}

		const char *alias = sl_key(iter);
		sl_done(iter);
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, c, c_ctx, tmp, q->st.cur_ctx)) {
			unshare_cell(tmp+1);
			return false;
		}

		return true;
	} else if (!CMP_STRING_TO_CSTR(q, p2, "status")) {
		if (t->num_locks == 0) {
			cell *tmp = alloc_heap(q, 2);
			make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
			make_atom(tmp+1, new_atom(q->pl, "unlocked"));
			return unify(q, c, c_ctx, tmp, q->st.cur_ctx);
		}

		cell *tmp = alloc_heap(q, 4);
		make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 3);
		make_instr(tmp+1, new_atom(q->pl, "locked"), NULL, 2, 2);
		make_int(tmp+2, t->locked_by);
		tmp[2].flags |= FLAG_INT_THREAD;
		make_int(tmp+3, t->num_locks);
		return unify(q, c, c_ctx, tmp, q->st.cur_ctx);
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

		thread *t = &q->pl->threads[i];

		if (!t->is_active || !t->is_mutex_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		thread *t = &q->pl->threads[i];

		if (!t->is_active || !t->is_mutex_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		CHECKED(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_THREAD;
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	return do_mutex_property_pin_both(q);
}

static bool do_mutex_property_pin_id(query *q)
{
	GET_FIRST_ARG(p1,mutex);
	GET_NEXT_ARG(p2,any);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];
	unsigned i = 0;

	if (q->retry)
		i = ++q->st.v2;
	else
		q->st.v2 = 0;

	if (i == 0) {
		CHECKED(push_choice(q));

		sliter *iter = sl_first(t->alias);

		if (!sl_next(iter, NULL)) {
			sl_done(iter);
			drop_choice(q);
			return false;
		}

		const char *alias = sl_key(iter);
		sl_done(iter);
		cell *tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "alias"), NULL, 1, 1);
		make_cstring(tmp+1, alias);

		if (!unify(q, p2, p2_ctx, tmp, q->st.cur_ctx)) {
			unshare_cell(tmp+1);
			return false;
		}

		return true;
	}

	cell *tmp;

	if (t->num_locks != 0) {
		tmp = alloc_heap(q, 4);
		make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 3);
		make_instr(tmp+1, new_atom(q->pl, "locked"), NULL, 2, 2);
		make_int(tmp+2, t->locked_by);
		tmp[2].flags |= FLAG_INT_THREAD;
		make_int(tmp+3, t->num_locks);
	} else {
		tmp = alloc_heap(q, 2);
		make_instr(tmp, new_atom(q->pl, "status"), NULL, 1, 1);
		make_atom(tmp+1, new_atom(q->pl, "unlocked"));
	}

	return unify(q, p2, p2_ctx, tmp, q->st.cur_ctx);
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

		thread *t = &q->pl->threads[i];

		if (!t->is_active || !t->is_mutex_only)
			continue;

		break;
	}

	q->st.v1 = i;

	while (++i) {
		if (i == MAX_THREADS)
			break;

		thread *t = &q->pl->threads[i];

		if (!t->is_active || !t->is_mutex_only)
			continue;

		break;
	}

	if (i != MAX_THREADS)
		CHECKED(push_choice(q));

	cell tmp;
	make_int(&tmp, q->st.v1);
	tmp.flags |= FLAG_INT_THREAD;
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	return do_mutex_property_pin_id(q);
}

static bool bif_mutex_property_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
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
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,integer);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (t->is_queue_only || t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "pin_cpu,not_thread");

	// Do something here
	return true;
}

static bool bif_pl_thread_set_priority_2(query *q)
{
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,thread);
	GET_NEXT_ARG(p2,integer);
	int n = get_thread(q, p1);
	if (n < 0) return true;
	thread *t = &q->pl->threads[n];

	if (t->is_queue_only || t->is_mutex_only)
		return throw_error(q, p1, p1_ctx, "permission_error", "set_priority,not_thread");

	// Do something here
	return true;
}

static bool do_recv_message(query *q, unsigned from_chan, cell *p1, pl_ctx p1_ctx, bool is_peek)
{
	thread *t = &q->pl->threads[q->pl->my_chan];

	while (!q->halt) {
		acquire_lock(&t->guard);

		if (list_count(&t->queue))
			break;

		release_lock(&t->guard);

		if (is_peek)
			return false;

		uint64_t cnt = 0;

		do {
			suspend_thread(t, cnt < 100 ? 0 : cnt < 1000 ? 1 : cnt < 10000 ? 10 : 10);
			cnt++;
		}
		 while (!list_count(&t->queue) && !q->halt);
	}

	msg *m;

	if (is_peek)
		m = list_front(&t->queue);
	else
		m = list_pop_front(&t->queue);

	CHECKED(push_choice(q));
	CHECKED(check_frame(q, MAX_ARITY));
	try_me(q, MAX_ARITY);
	cell *c = m->c;
	cell *tmp = clone_term_to_heap(q, c, q->st.new_fp);
	CHECKED(tmp, release_lock(&t->guard));
	release_lock(&t->guard);
	q->curr_chan = m->from_chan;

	if (!is_peek) {
		unshare_cells(m->c, m->c->num_cells);
		free(m);
	}

	drop_choice(q);
	return unify(q, p1, p1_ctx, tmp, q->st.cur_ctx);
}

static bool bif_pl_recv_2(query *q)
{
	check_slot(q, MAX_ARITY);
	THREAD_DEBUG DUMP_TERM("*** ", q->st.instr, q->st.cur_ctx, 1);
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,any);
	int from_chan = 0;

	if (is_integer(p1)) {
		from_chan = get_thread(q, p1);

		if (from_chan < 0)
			return throw_error(q, p1, p1_ctx, "domain_error", "no_such_thread");
	}

	if (!do_recv_message(q, from_chan, p2, p2_ctx, false))
		return false;

	cell tmp;
	make_int(&tmp, q->curr_chan);
	tmp.flags |= FLAG_INT_THREAD;
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

void thread_cancel_all(prolog *pl)
{
	for (int i = 0; (i < 1000) && pl->q_cnt; i++)
		msleep(1);

	if (!pl->q_cnt)
		return;

	printf("Warning: %d outstanding application threads\n", (int)pl->q_cnt);

	for (unsigned i = 0; i < MAX_THREADS; i++) {
		thread *t = &pl->threads[i];

		if (!is_thread_only(t) || !t->is_active)
			continue;

		do_cancel(t);
	}
}
#endif

builtins g_threads_bifs[] =
{
#if USE_THREADS
	{"thread", 3, bif_pl_thread_3, "-thread,+atom,+list", false, false, BLAH},
	{"pl_thread_pin_cpu", 2, bif_pl_thread_pin_cpu_2, "+thread,+integer", false, false, BLAH},
	{"pl_thread_set_priority", 2, bif_pl_thread_set_priority_2, "+thread,+integer", false, false, BLAH},
	{"pl_msg_send", 2, bif_pl_send_2, "+thread,+term", false, false, BLAH},
	{"pl_msg_recv", 2, bif_pl_recv_2, "-thread,?term", false, false, BLAH},

	{"thread_create", 3, bif_thread_create_3, ":callable,-thread,+list", false, false, BLAH},

#if !defined(__ANDROID__)
	{"thread_cancel", 1, bif_thread_cancel_1, "+thread", false, false, BLAH},
#endif

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
