#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <time.h>

#include "heap.h"
#include "module.h"
#include "network.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"
#include "utf8.h"

#ifdef _WIN32
#include <windows.h>
#define msleep Sleep
#else
static void msleep(int ms)
{
	struct timespec tv;
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

#define Trace if (q->trace /*&& !consulting*/) trace_call

static const unsigned INITIAL_NBR_HEAP_CELLS = 16000;
static const unsigned INITIAL_NBR_QUEUE_CELLS = 1000;
static const unsigned INITIAL_NBR_GOALS = 16000;
static const unsigned INITIAL_NBR_SLOTS = 16000;
static const unsigned INITIAL_NBR_TRAILS = 16000;
static const unsigned INITIAL_NBR_CHOICES = 8000;
static const unsigned INITIAL_NBR_CELLS = 1000;

unsigned g_string_cnt = 0, g_interned_cnt = 0;
int g_tpl_interrupt = 0;

typedef enum { CALL, EXIT, REDO, NEXT, FAIL } box_t;

#define TRACE_MEM 0

// Note: when in commit there is a provisional choice point
// that we should skip over, hence the '1' ...

static bool any_choices(const query *q, const frame *f)
{
	if (q->cp == (unsigned)(q->in_commit ? 1 : 0))
		return false;

	const choice *ch = q->in_commit ? GET_PREV_CHOICE() : GET_CURR_CHOICE();
	return ch->cgen > f->cgen;
}

void dump_term(query *q, const char *s, const cell *c)
{
	unsigned nbr_cells = c->nbr_cells;
	printf("*** %s\n", s);

	for (unsigned i = 0; i < nbr_cells; i++, c++) {
		printf("    ");
		printf("[%u] tag=%u ", i, c->tag);

		if (is_atom(c))
			printf("%s ", C_STR(q, c));
		else if (is_var(c))
			printf("_%u ", c->var_nbr);
		else if (is_structure(c))
			printf("%s/%u ", C_STR(q, c), c->arity);

		printf("\n");
	}
}

static void trace_call(query *q, cell *c, pl_idx_t c_ctx, box_t box)
{
	if (!c || is_empty(c))
		return;

	if (c->fn_ptr && !c->fn_ptr->fn)
		return;

#if 0
	if (is_builtin(c))
		return;
#endif

	if (box == CALL)
		box = q->retry?REDO:CALL;

#if 1
	const char *src = C_STR(q, c);

	if (!strcmp(src, ",") || !strcmp(src, ";") || !strcmp(src, "->") || !strcmp(src, "*->"))
		return;
#endif

	SB(pr);

#ifdef DEBUG
	SB_sprintf(pr, "[%s:%"PRIu64":f%u:fp:%u:cp%u:sp%u:hp%u:tp%u] ",
		q->st.m->name,
		q->step++,
		q->st.curr_frame, q->st.fp, q->cp, q->st.sp, q->st.hp, q->st.tp
		);
#else
	SB_sprintf(pr, "[%s:%"PRIu64"] ",
		q->st.m->name,
		q->step++
		);
#endif

	SB_sprintf(pr, "%s ",
		box == CALL ? "CALL" :
		box == EXIT ? "EXIT" :
		box == REDO ? "REDO" :
		box == NEXT ? "NEXT" :
		box == FAIL ? "FAIL":
		"????");

	int save_depth = q->max_depth;
	q->max_depth = 10;
	q->quoted = true;
	char *dst = print_term_to_strbuf(q, c, c_ctx, -1);
	SB_strcat(pr, dst);
	free(dst);
	q->quoted = false;
	SB_sprintf(pr, "%s", "\n");
	src = SB_cstr(pr);
	size_t srclen = srclen = SB_strlen(pr);
	int n = q->pl->current_error;
	stream *str = &q->pl->streams[n];
	net_write(src, srclen, str);
	SB_free(pr);
	q->max_depth = save_depth;

	if (q->creep) {
		msleep(500);
	}
}

static bool check_trail(query *q)
{
	if (q->st.tp > q->max_trails) {
		if (q->st.tp > q->hw_trails)
			q->hw_trails = q->st.tp;

		if (q->st.tp >= q->trails_size) {
			pl_idx_t new_trailssize = alloc_grow((void**)&q->trails, sizeof(trail), q->st.tp, q->trails_size*4/3);
			if (!new_trailssize) {
				q->is_oom = q->error = true;
				return false;
			}

			q->trails_size = new_trailssize;
		}

		q->max_trails = q->st.tp;
	}

	if ((q->trails_size > INITIAL_NBR_TRAILS) && (q->st.tp < (q->trails_size / 3))) {
#if TRACE_MEM
		printf("*** q->st.tp=%u, q->trails_size=%u\n", (unsigned)q->st.tp, (unsigned)q->trails_size);
#endif
		q->trails_size = alloc_grow((void**)&q->trails, sizeof(trail), q->st.tp, q->trails_size / 2);
		q->max_trails = q->st.tp;
	}

	return true;
}

static bool check_choice(query *q)
{
	if (q->cp > q->max_choices) {
		if (q->cp > q->hw_choices)
			q->hw_choices = q->cp;

		if (q->cp >= q->choices_size) {
			pl_idx_t new_choicessize = alloc_grow((void**)&q->choices, sizeof(choice), q->cp, q->choices_size*4/3);
			if (!new_choicessize) {
				q->is_oom = q->error = true;
				return false;
			}

			q->choices_size = new_choicessize;
		}

		q->max_choices = q->cp;
	}

	if ((q->choices_size > INITIAL_NBR_CHOICES) && (q->cp < (q->choices_size / 3))) {
#if TRACE_MEM
		printf("*** q->st.cp=%u, q->choices_size=%u\n", (unsigned)q->cp, (unsigned)q->choices_size);
#endif
		q->choices_size = alloc_grow((void**)&q->choices, sizeof(choice), q->cp, q->choices_size / 2);
		q->max_choices = q->cp;
	}

	return true;
}

bool check_frame(query *q)
{
	if (q->st.fp > q->max_frames) {
		if (q->st.fp > q->hw_frames)
			q->hw_frames = q->st.fp;

		if (q->st.fp >= q->frames_size) {
			pl_idx_t new_framessize = alloc_grow((void**)&q->frames, sizeof(frame), q->st.fp, q->frames_size*4/3);

			if (!new_framessize) {
				q->is_oom = q->error = true;
				return false;
			}

			q->frames_size = new_framessize;
		}

		q->max_frames = q->st.fp;
	}

	if ((q->frames_size > INITIAL_NBR_GOALS) && (q->st.fp < (q->frames_size / 3))) {
#if TRACE_MEM
		printf("*** q->st.fp=%u, q->frames_size=%u\n", (unsigned)q->st.fp, (unsigned)q->frames_size);
#endif
		q->frames_size = alloc_grow((void**)&q->frames, sizeof(frame), q->st.fp, q->frames_size / 2);
		q->max_frames = q->st.fp;
	}

	return true;
}

bool check_slot(query *q, unsigned cnt)
{
	pl_idx_t nbr = q->st.sp + cnt;

	if (nbr > q->max_slots) {
		if (q->st.sp > q->hw_slots)
			q->hw_slots = q->st.sp;

		while (nbr >= q->slots_size) {
			pl_idx_t new_slotssize = alloc_grow((void**)&q->slots, sizeof(slot), nbr, q->slots_size*4/3);

			if (!new_slotssize) {
				q->is_oom = q->error = true;
				return false;
			}

			q->slots_size = new_slotssize;
		}

		q->max_slots = nbr;
	}

	if ((q->slots_size > INITIAL_NBR_SLOTS) && (q->st.sp < (q->slots_size / 3))) {
#if TRACE_MEM
		printf("*** q->st.sp=%u, q->slots_size=%u\n", (unsigned)q->st.sp, (unsigned)q->slots_size);
#endif
		q->slots_size = alloc_grow((void**)&q->slots, sizeof(slot), q->st.sp, q->slots_size / 2);
		q->max_slots = nbr;
	}

	return true;
}

static void setup_key(query *q)
{
	if (!q->pl->opt)
		return;

	cell *arg1 = q->st.key + 1, *arg2 = NULL, *arg3 = NULL;

	if (q->st.key->arity > 1)
		arg2 = arg1 + arg1->nbr_cells;

	if (q->st.key->arity > 2)
		arg3 = arg2 + arg2->nbr_cells;

	arg1 = deref(q, arg1, q->st.curr_frame);

	if (arg2)
		arg2 = deref(q, arg2, q->st.curr_frame);

	if (arg3)
		arg3 = deref(q, arg3, q->st.curr_frame);

	if (is_atomic(arg1) || is_structure(arg1))
		q->st.arg1_is_ground = true;

	if (arg2 && is_atomic(arg2))
		q->st.arg2_is_ground = true;

	if (arg3 && is_atomic(arg3))
		q->st.arg3_is_ground = true;
}

void next_key(query *q)
{
	if (q->st.iter) {
		if (!map_next(q->st.iter, (void*)&q->st.curr_dbe)) {
			q->st.curr_dbe = NULL;
			map_done(q->st.iter);
			q->st.iter = NULL;
		}
	} else
		q->st.curr_dbe = q->st.curr_dbe->next;
}

bool has_next_key(query *q)
{
	if (q->st.iter) {
		db_entry *dbe;
		const frame *f = GET_CURR_FRAME();

		while (map_is_next(q->st.iter, (void**)&dbe)) {
			if (!can_view(f->ugen, dbe)) {
				db_entry *save_dbe = q->st.curr_dbe;
				next_key(q);
				q->st.curr_dbe = save_dbe;
				continue;
			}

			return true;
		}

		return false;
	}

	clause *cl = &q->st.curr_dbe->cl;

	//printf("*** q->st.arg1_is_ground=%d, cl->arg1_is_unique=%d\n",
	//	q->st.arg1_is_ground, cl->arg1_is_unique);

	if (q->st.arg1_is_ground && cl->arg1_is_unique)
		return false;

	if (q->st.arg2_is_ground && cl->arg2_is_unique)
		return false;

	if (q->st.arg3_is_ground && cl->arg3_is_unique)
		return false;

	db_entry *next = q->st.curr_dbe->next;

	if (!next)
		return false;

	// Attempt look-ahead on 1st arg...

	cl = &next->cl;

	if (q->st.arg1_is_ground && !next->next && q->pl->opt
		&& (q->st.key->arity == 1) && is_atomic(cl->cells+1)) {
		if (compare(q, q->st.key, q->st.curr_frame, cl->cells, q->st.curr_frame)) {
			return false;
		}
	}

	return true;
}

const char *dump_id(const void *k, const void *v, const void *p)
{
	const query *q = (query*)p;
	uint64_t id = (uint64_t)(size_t)k;
	static char tmpbuf[1024];
	sprintf(tmpbuf, "%"PRIu64"", id);
	return tmpbuf;
}

static bool find_key(query *q, predicate *pr, cell *key, pl_idx_t key_ctx)
{
	q->st.iter = NULL;
	q->st.arg1_is_ground = false;
	q->st.arg2_is_ground = false;
	q->st.arg3_is_ground = false;
	q->st.key = key;

	if (!pr->idx) {
		if (!pr->is_processed && !pr->is_multifile)
			just_in_time_rebuild(pr);

		q->st.curr_dbe = pr->head;

		if (!key->arity || pr->is_multifile)
			return true;

		setup_key(q);
		return true;
	}

	//sl_dump(pr->idx, dump_key, q);

	check_heap_error(init_tmp_heap(q));
	q->st.key = key = deep_clone_to_tmp(q, key, key_ctx);

	cell *arg1 = key->arity ? key + 1 : NULL;
	map *idx = pr->idx;

	if (arg1 && (is_var(arg1) || pr->is_var_in_first_arg)) {
		if (!pr->idx2) {
			q->st.curr_dbe = pr->head;
			return true;
		}

		cell *arg2 = arg1 + arg1->nbr_cells;

		if (is_var(arg2)) {
			q->st.curr_dbe = pr->head;
			return true;
		}

		q->st.key = key = arg2;
		idx = pr->idx2;
	}

#define DEBUGIDX 0

#if DEBUGIDX
	DUMP_TERM("search, term = ", key, q->st.curr_frame);
#endif

	q->st.curr_dbe = NULL;
	miter *iter;

	if (!(iter = map_find_key(idx, key)))
		return false;

#define TESTINGIDX 0

#if TESTINGIDX

	// This returns results in index order but not database order
	// as is required for normal Prolog operations. Just used
	// for testing purposes only...

	if (!map_next_key(iter, (void*)&q->st.curr_dbe)) {
		map_done(iter);
		return false;
	}

	q->st.iter = iter;
	return true;
#endif

	// If the index search has found just one (definite) solution
	// then we can use it with no problems. If more than one then
	// results must be returned in database order, so prefetch all
	// the results and return them sorted as an iterator...

	map *tmp_idx = NULL;
	const db_entry *dbe;

	while (map_next_key(iter, (void*)&dbe)) {
		CHECK_INTERRUPT();

#if DEBUGIDX
		DUMP_TERM("   got, key = ", dbe->cl.cells, q->st.curr_frame);
#endif

		if (!tmp_idx) {
			tmp_idx = map_create(NULL, NULL, NULL);
			map_allow_dups(tmp_idx, false);
			map_set_tmp(tmp_idx);
		}

		map_app(tmp_idx, (void*)(size_t)dbe->db_id, (void*)dbe);
	}

	map_done(iter);

	if (!tmp_idx)
		return false;

	//sl_dump(tmp_idx, dump_id, q);

	iter = map_first(tmp_idx);

	if (!map_next(iter, (void*)&q->st.curr_dbe)) {
		map_done(iter);
		return false;
	}

	q->st.iter = iter;
	return true;
}

size_t scan_is_chars_list2(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes, bool *has_var, bool *is_partial)
{
	*is_partial = *has_var = false;
	size_t is_chars_list = 0;
	LIST_HANDLER(l);

	while (is_list(l)
		&& (q->st.m->flags.double_quote_chars || allow_codes)
		) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(l);
		cell *c = deref(q, h, l_ctx);
		q->suspect = c;

		if (is_var(c)) {
			*has_var = true;
			return 0;
		}

		if (!is_integer(c) && !is_iso_atom(c)) {
			is_chars_list = 0;
			return 0;
		}

		if (is_integer(c) && !allow_codes) {
			is_chars_list = 0;
			return 0;
		}

		if (is_integer(c)) {
			int ch = get_smallint(c);
			char tmp[20];
			put_char_utf8(tmp, ch);
			size_t len = len_char_utf8(tmp);
			is_chars_list += len;
		} else {
			const char *src = C_STR(q, c);
			size_t len = len_char_utf8(src);

			if (len != C_STRLEN(q, c)) {
				is_chars_list = 0;
				return 0;
			}

			is_chars_list += len;
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	if (is_var(l)) {
		is_chars_list = 0;
		*has_var = *is_partial = true;
	} else if (is_string(l))
		;
	else if (!is_interned(l) || !is_nil(l))
		is_chars_list = 0;

	return is_chars_list;
}

size_t scan_is_chars_list(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes)
{
	bool has_var, is_partial;
	return scan_is_chars_list2(q, l, l_ctx, allow_codes, &has_var, &is_partial);
}

void add_trail(query *q, pl_idx_t c_ctx, unsigned c_var_nbr, cell *attrs, pl_idx_t attrs_ctx)
{
	if (!check_trail(q)) {
		q->error = false;
		return;
	}

	trail *tr = q->trails + q->st.tp++;
	tr->var_ctx = c_ctx;
	tr->var_nbr = c_var_nbr;
	tr->attrs = attrs;
	tr->attrs_ctx = attrs_ctx;
}

static void unwind_trail(query *q, const choice *ch)
{
	while (q->st.tp > ch->st.tp) {
		const trail *tr = q->trails + --q->st.tp;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		unshare_cell(&e->c);
		init_cell(&e->c);
		e->c.attrs = tr->attrs;
		e->c.attrs_ctx = tr->attrs_ctx;
		e->mark = false;
	}
}

void undo_me(query *q)
{
	q->tot_retries++;
	const choice *ch = GET_CURR_CHOICE();
	unwind_trail(q, ch);
}

bool try_me(query *q, unsigned nbr_vars)
{
	frame *f = GET_FRAME(q->st.fp);
	f->initial_slots = f->actual_slots = nbr_vars;
	f->base = q->st.sp;

	for (unsigned i = 0; i < nbr_vars; i++) {
		slot *e = GET_SLOT(f, i);
		//unshare_cell(&e->c);
		init_cell(&e->c);
		e->mark = false;
	}

	q->run_hook = false;
	q->cycle_error = false;
	q->check_unique = false;
	q->has_vars = false;
	q->no_tco = false;
	q->tot_matches++;
	return true;
}

static void trim_heap(query *q)
{
	// q->pages is a push-down stack and points to the
	// most recent page of heap allocations...

	for (page *a = q->pages; a;) {
		if (a->nbr < q->st.curr_page)
			break;

		for (pl_idx_t i = 0; i < a->max_hp_used; i++) {
			cell *c = a->heap + i;
			unshare_cell(c);
			init_cell(c);
		}

		page *save = a;
		q->pages = a = a->next;
		free(save->heap);
		free(save);
	}

#if 0
	const page *a = q->pages;
	const choice *ch = GET_CURR_CHOICE();

	for (pl_idx_t i = ch->st.hp; a && (i < a->max_hp_used) && (i < q->st.hp); i++) {
		cell *c = a->heap + i;
		unshare_cell(c);
		init_cell(c);
	}
#endif
}

void drop_choice(query *q)
{
	if (!q->cp)
		return;

	choice *ch = GET_CURR_CHOICE();

	if (ch->st.iter) {
		map_done(ch->st.iter);
		ch->st.iter = NULL;
	}

	q->st.pr = NULL;
	--q->cp;
}

bool retry_choice(query *q)
{
	while (q->cp) {
		pl_idx_t curr_choice = --q->cp;
		const choice *ch = GET_CHOICE(curr_choice);
		unwind_trail(q, ch);
		q->st = ch->st;
		q->save_m = NULL;
		trim_heap(q);

		frame *f = GET_CURR_FRAME();
		f->ugen = ch->ugen;
		f->cgen = ch->frame_cgen;
		f->initial_slots = ch->initial_slots;
		f->actual_slots = ch->actual_slots;
		f->overflow = ch->overflow;

		if (ch->catchme_exception || ch->soft_cut || ch->did_cleanup)
			continue;

		return true;
	}

	return false;
}

static frame *push_frame(query *q, clause *cl)
{
	pl_idx_t new_frame = q->st.fp++;
	frame *f = GET_FRAME(new_frame);
	const frame *curr_f = GET_CURR_FRAME();
	const cell *next_cell = q->st.curr_cell + q->st.curr_cell->nbr_cells;

	// Avoid long chains of useless returns...

	if (is_end(next_cell) && !next_cell->val_ret && curr_f->prev_cell) {
		f->prev_offset = (new_frame - q->st.curr_frame) + curr_f->prev_offset;
		f->prev_cell = curr_f->prev_cell;
	} else {
		f->prev_offset = new_frame - q->st.curr_frame;
		f->prev_cell = q->st.curr_cell;
	}

	f->cgen = ++q->cgen;
	f->is_last = false;
	f->overflow = 0;

	q->st.sp += cl->nbr_vars;
	q->st.curr_frame = new_frame;
	return f;
}

static void reuse_frame(query *q, frame* f, const clause *cl)
{
	const frame *newf = GET_FRAME(q->st.fp);
	const choice *ch = GET_CURR_CHOICE();
	q->st.sp = ch->st.sp;

	for (pl_idx_t i = 0; i < cl->nbr_vars; i++) {
		const slot *from = GET_SLOT(newf, i);
		slot *to = GET_SLOT(f, i);
		unshare_cell(&to->c);
		*to = *from;
	}

	f->cgen = ++q->cgen;
	f->initial_slots = f->actual_slots = cl->nbr_vars - cl->nbr_temporaries;
	f->overflow = 0;

	q->st.sp = f->base + f->actual_slots;
	q->tot_tcos++;
}

static void trim_trail(query *q)
{
	if (q->undo_hi_tp)
		return;

	if (!q->cp) {
		q->st.tp = 0;
		return;
	}

	const choice *ch = GET_CURR_CHOICE();
	pl_idx_t tp = ch->st.tp;

	while (q->st.tp > tp) {
		const trail *tr = q->trails + q->st.tp - 1;

		if (tr->var_ctx != q->st.curr_frame)
			break;

		q->st.tp--;
	}
}

static bool are_slots_ok(const query *q, const frame *f)
{
	for (unsigned i = 0; i < f->actual_slots; i++) {
		const slot *e = GET_SLOT(f, i);
		const cell *c = &e->c;

		if (is_var(c) && (c->var_ctx < q->st.curr_frame))	// Why?
			return false;
	}

	return true;
}

void share_predicate(query *q, predicate *pr)
{
	if (!pr->is_dynamic)
		return;

	q->st.pr = pr;
	pr->ref_cnt++;
}

void unshare_predicate(query *q, predicate *pr)
{
	if (!pr)
		return;

	if (!pr->is_dynamic)
		return;

	if (!pr->ref_cnt)
		return;

	--pr->ref_cnt;

	if (pr->ref_cnt != 0)
		return;

	// Predicate is no longer being used

	if (!pr->dirty_list)
		return;

	// Just because this predicate is no longer in use doesn't
	// mean there are no shared references to terms contained
	// within. So move items on the dirty-list to the query
	// dirty-list. They will be freed up at end of the query.

	db_entry *dbe = pr->dirty_list;

	while (dbe) {
		delink(pr, dbe);

		if (pr->cnt) {
			predicate *pr = dbe->owner;
			map_remove(pr->idx2, dbe);
			map_remove(pr->idx, dbe);
		}

		dbe->cl.is_deleted = true;
		db_entry *save = dbe->dirty;
		dbe->dirty = q->dirty_list;
		q->dirty_list = dbe;
		dbe = save;
	}

	pr->dirty_list = NULL;

	if (pr->idx && !pr->cnt) {
		map_destroy(pr->idx2);
		map_destroy(pr->idx);
		pr->idx2 = NULL;

		pr->idx = map_create(index_cmpkey, NULL, pr->m);
		ensure(pr->idx);
		map_allow_dups(pr->idx, true);

		if (pr->key.arity > 1) {
			pr->idx2 = map_create(index_cmpkey, NULL, pr->m);
			ensure(pr->idx2);
			map_allow_dups(pr->idx2, true);
		}
	}
}

static void commit_me(query *q)
{
	q->in_commit = true;
	clause *cl = &q->st.curr_dbe->cl;
	frame *f = GET_CURR_FRAME();
	f->mid = q->st.m->id;

	if (!q->st.curr_dbe->owner->is_prebuilt) {
		if (q->st.m != q->st.curr_dbe->owner->m)
			q->st.prev_m = q->st.m;

		q->st.m = q->st.curr_dbe->owner->m;
	}

	cell *body = get_body(cl->cells);
	bool implied_first_cut = q->check_unique && !q->has_vars && cl->is_unique && !q->st.iter;
	bool last_match = implied_first_cut || cl->is_first_cut || !has_next_key(q);
	bool recursive = is_tail_recursive(q->st.curr_cell);
	bool vars_ok = f->actual_slots == cl->nbr_vars;
	bool choices = any_choices(q, f);
	bool slots_ok = are_slots_ok(q, f);
	bool tco;

	if (q->no_tco && (cl->nbr_vars != cl->nbr_temporaries))
		tco = false;
	else
		tco = last_match && recursive && vars_ok && !choices && slots_ok;

#if 0
	printf("*** retry=%d,tco=%d,q->no_tco=%d,last_match=%d (%d/%d),recursive=%d,choices=%d,slots_ok=%d,vars_ok=%d,cl->nbr_vars=%u,cl->nbr_temps=%u\n",
		q->retry, tco, q->no_tco, last_match, implied_first_cut, cl->is_first_cut, recursive, choices, slots_ok, vars_ok, cl->nbr_vars, cl->nbr_temporaries);
#endif

	if (q->pl->opt && tco)
		reuse_frame(q, f, cl);
	else
		f = push_frame(q, cl);

	if (last_match) {
		f->is_last = true;
		unshare_predicate(q, q->st.pr);
		drop_choice(q);
		trim_trail(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.curr_dbe = q->st.curr_dbe;
		ch->cgen = f->cgen;
	}

	q->st.iter = NULL;
	q->st.curr_cell = body;
	q->in_commit = false;
}

void stash_me(query *q, const clause *cl, bool last_match)
{
	pl_idx_t cgen = ++q->cgen;

	if (last_match) {
		unshare_predicate(q, q->st.pr);
		drop_choice(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.curr_dbe = q->st.curr_dbe;
		ch->cgen = cgen;
	}

	unsigned nbr_vars = cl->nbr_vars;

	if (nbr_vars) {
		pl_idx_t new_frame = q->st.fp++;
		frame *f = GET_FRAME(new_frame);
		f->is_last = last_match;
		f->prev_offset = new_frame - q->st.curr_frame;
		f->prev_cell = NULL;
		f->cgen = cgen;
		f->overflow = 0;
		q->st.sp += nbr_vars;
	}

	q->st.iter = NULL;
}

bool push_choice(query *q)
{
	check_heap_error(check_choice(q));
	const frame *f = GET_CURR_FRAME();
	pl_idx_t curr_choice = q->cp++;
	choice *ch = GET_CHOICE(curr_choice);
	memset(ch, 0, sizeof(choice));
	ch->st = q->st;
	ch->ugen = f->ugen;
	ch->frame_cgen = ch->cgen = f->cgen;
	ch->initial_slots = f->initial_slots;
	ch->actual_slots = f->actual_slots;
	ch->overflow = f->overflow;
	return true;
}

// A barrier is used when making a call, it sets a
// new choice generation so that normal cuts are contained.
// An '$inner_cut' though will also remove the barrier...

bool push_barrier(query *q)
{
	check_error(push_choice(q));
	frame *f = GET_CURR_FRAME();
	choice *ch = GET_CURR_CHOICE();
	ch->cgen = f->cgen = ++q->cgen;
	ch->barrier = true;
	return true;
}

// Note: since there is no separate control stack a call will
// create a choice on the stack. This sets a special flag so
// that '$drop_barrier' knows to also remove the barrier (choice)
// if the call is det.

bool push_call_barrier(query *q)
{
	check_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->call_barrier = true;
	return true;
}

bool push_catcher(query *q, enum q_retry retry)
{
	check_error(push_call_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->catcher = true;

	if (retry == QUERY_RETRY)
		ch->catchme_retry = true;
	else if (retry == QUERY_EXCEPTION)
		ch->catchme_exception = true;

	return true;
}

void cut_me(query *q)
{
	frame *f = GET_CURR_FRAME();

	while (q->cp) {
		choice *ch = GET_CURR_CHOICE();

		// A normal cut can't break out of a barrier...

		if (ch->barrier) {
			if (ch->cgen <= f->cgen)
				break;
		} else {
			if (ch->cgen < f->cgen)
				break;
		}

		if (ch->st.iter) {
			map_done(ch->st.iter);
			ch->st.iter = NULL;
		}

		frame *f = GET_FRAME(ch->st.curr_frame);

		if ((ch->st.fp == (q->st.curr_frame + 1))
			&& (f->actual_slots == 0)
			) {
				q->st.fp = ch->st.fp;
			}

		unshare_predicate(q, ch->st.pr);
		q->cp--;

		if (ch->register_cleanup && !ch->did_cleanup) {
			ch->did_cleanup = true;
			cell *c = ch->st.curr_cell;
			pl_idx_t c_ctx = ch->st.curr_frame;
			c = deref(q, c+1, c_ctx);
			c_ctx = q->latest_ctx;
			do_cleanup(q, c, c_ctx);
			break;
		}
	}

	if (!q->cp && !q->undo_hi_tp)
		q->st.tp = 0;
}

void inner_cut(query *q, bool soft_cut)
{
	frame *f = GET_CURR_FRAME();

	while (q->cp) {
		choice *ch = GET_CURR_CHOICE();
		const choice *save_ch = ch;

		while (soft_cut && (ch >= q->choices)) {
			if (ch->barrier && (ch->cgen == f->cgen)) {
				if (ch == save_ch) {
					f->cgen--;
					q->cp--;
					return;
				}

				ch->soft_cut = true;
				f->cgen--;
				return;
			}

			ch--;
		}

		// An inner cut can break through a barrier...

		if (ch->cgen < f->cgen) {
			//f->cgen = ch->cgen;
			f->cgen--;
			break;
		}

		if (ch->st.iter) {
			map_done(ch->st.iter);
			ch->st.iter = NULL;
		}

		unshare_predicate(q, ch->st.pr);
		q->cp--;

		if (ch->register_cleanup && !ch->did_cleanup) {
			ch->did_cleanup = true;
			cell *c = ch->st.curr_cell;
			pl_idx_t c_ctx = ch->st.curr_frame;
			c = deref(q, c+1, c_ctx);
			c_ctx = q->latest_ctx;
			do_cleanup(q, c, c_ctx);
			break;
		}
	}

	if (!q->cp && !q->undo_hi_tp)
		q->st.tp = 0;
}

// If the call is det then the barrier can be dropped...

bool drop_barrier(query *q)
{
	const frame *f = GET_CURR_FRAME();
	const choice *ch = GET_CURR_CHOICE();

	if (ch->call_barrier && (ch->cgen == f->cgen)) {
		drop_choice(q);
		return true;
	}

	return false;
}

// Proceed to next goal in frame...

static void proceed(query *q)
{
	q->st.curr_cell += q->st.curr_cell->nbr_cells;

	while (is_end(q->st.curr_cell)) {
		if (q->st.curr_cell->val_ret) {
			frame *f = GET_CURR_FRAME();
			f->cgen = q->st.curr_cell->cgen;
			q->st.m = q->pl->modmap[q->st.curr_cell->mid];
		}

		if (!(q->st.curr_cell = q->st.curr_cell->val_ret))
			break;
	}
}

// Prune dead frames from the top down...

static void chop_frames(query *q, const frame *f)
{
	if (q->st.curr_frame == (q->st.fp-1)) {
		const frame *tmpf = f;
		pl_idx_t prev_frame = q->st.curr_frame - f->prev_offset;

		while (q->st.fp > (prev_frame+1)) {
			if (any_choices(q, tmpf))
				break;

			q->tot_srecovs += q->st.sp - tmpf->base;
			q->tot_frecovs++;
			q->st.sp = tmpf->base;
			q->st.fp--;
			tmpf--;
		}
	}
}

// Resume previous frame...

static bool resume_frame(query *q)
{
	if (!q->st.curr_frame)
		return false;

	Trace(q, get_head(q->st.curr_dbe->cl.cells), q->st.curr_frame, EXIT);
	frame *f = GET_CURR_FRAME();

	if (q->pl->opt && 0)
		chop_frames(q, f);

	q->st.curr_cell = f->prev_cell;
	q->st.curr_frame = q->st.curr_frame - f->prev_offset;
	f = GET_CURR_FRAME();
	q->st.m = q->pl->modmap[f->mid];
	return true;
}

void make_indirect(cell *tmp, cell *v, pl_idx_t v_ctx)
{
	tmp->tag = TAG_PTR;
	tmp->nbr_cells = 1;
	tmp->arity = 0;
	tmp->flags = 0;
	tmp->val_ptr = v;
	tmp->var_ctx = v_ctx;
}

#define MAX_LOCAL_VARS (1L<<30)

unsigned create_vars(query *q, unsigned cnt)
{
	frame *f = GET_CURR_FRAME();

	if (!cnt)
		return f->actual_slots;

	if ((q->st.sp + cnt) > MAX_LOCAL_VARS) {
		printf("*** Ooops %s %d\n", __FILE__, __LINE__);
		return 0;
	}

	unsigned var_nbr = f->actual_slots;

	if (!check_slot(q, var_nbr+cnt))
		return 0;

	if ((f->base + f->initial_slots) >= q->st.sp) {
		f->initial_slots += cnt;
		q->st.sp = f->base + f->initial_slots;
	} else if (!f->overflow) {
		f->overflow = q->st.sp;
		q->st.sp += cnt;
	} else if ((f->overflow + (f->actual_slots - f->initial_slots)) == q->st.sp) {
		q->st.sp += cnt;
	} else {
		pl_idx_t save_overflow = f->overflow;
		f->overflow = q->st.sp;
		pl_idx_t cnt2 = f->actual_slots - f->initial_slots;
		memmove(q->slots+f->overflow, q->slots+save_overflow, sizeof(slot)*cnt2);
		q->st.sp += cnt2 + cnt;
	}

	for (unsigned i = 0; i < cnt; i++) {
		slot *e = GET_SLOT(f, f->actual_slots+i);
		init_cell(&e->c);
		e->mark = false;
	}

	f->actual_slots += cnt;
	return var_nbr;
}

cell *get_var(query *q, cell *c, pl_idx_t c_ctx)
{
	if (is_ref(c))
		c_ctx = c->var_ctx;

	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);

	while (is_var(&e->c)) {
		c_ctx = e->c.var_ctx;
		c = &e->c;
		f = GET_FRAME(c_ctx);
		e = GET_SLOT(f, c->var_nbr);
	}

	if (is_empty(&e->c)) {
		q->latest_ctx = c_ctx;
		return c;
	}

	if (is_indirect(&e->c)) {
		q->latest_ctx = e->c.var_ctx;
		return e->c.val_ptr;
	}

	if (is_var(&e->c)) {
		q->latest_ctx = e->c.var_ctx;
		return &e->c;
	}

	q->latest_ctx = c_ctx;
	return &e->c;
}

void set_var(query *q, const cell *c, pl_idx_t c_ctx, cell *v, pl_idx_t v_ctx)
{
	frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);
	cell *c_attrs = is_empty(&e->c) ? e->c.attrs : NULL, *v_attrs = NULL;
	pl_idx_t c_attrs_ctx = c_attrs ? e->c.attrs_ctx : 0;

	if (is_var(v)) {
		frame *vf = GET_FRAME(v_ctx);
		slot *ve = GET_SLOT(vf, v->var_nbr);
		v_attrs = is_empty(&ve->c) ? ve->c.attrs : NULL;
	}

	if (c_attrs || (q->cp && (c_ctx < q->st.fp)))
		add_trail(q, c_ctx, c->var_nbr, c_attrs, c_attrs_ctx);

	// If 'c' is an attvar and either 'v' is an attvar or nonvar then run the hook
	// If 'c' is an attvar and 'v' is a plain var then copy attributes to 'v'
	// If 'c' is a plain var and 'v' is an attvar then copy attributes to 'c'

	if (c_attrs && (v_attrs || is_nonvar(v))) {
		q->run_hook = true;
	} else if (c_attrs && !v_attrs && is_var(v)) {
		frame *vf = GET_FRAME(v_ctx);
		slot *ve = GET_SLOT(vf, v->var_nbr);
		add_trail(q, v_ctx, v->var_nbr, NULL, 0);
		ve->c.attrs = c_attrs;
		ve->c.attrs_ctx = c_attrs_ctx;
	} else if (!c_attrs && v_attrs) {
		// attributes get copied below
	}

	if (is_structure(v)) {
		q->no_tco = true;
		make_indirect(&e->c, v, v_ctx);
	} else if (is_var(v)) {
		e->c = *v;
		e->c.flags |= FLAG_VAR_REF;
		e->c.var_ctx = v_ctx;
	} else {
		share_cell(v);
		e->c = *v;
	}

	if (q->flags.occurs_check != OCCURS_CHECK_FALSE)
		e->mark = true;
}

void reset_var(query *q, const cell *c, pl_idx_t c_ctx, cell *v, pl_idx_t v_ctx)
{
	frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);

	add_trail(q, c_ctx, c->var_nbr, NULL, 0);

	if (is_structure(v)) {
		make_indirect(&e->c, v, v_ctx);
	} else if (is_var(v)) {
		e->c = *v;
		e->c.flags |= FLAG_VAR_REF;
		e->c.var_ctx = v_ctx;
	} else {
		share_cell(v);
		e->c = *v;
	}
}

// Match HEAD :- BODY.

bool match_rule(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type is_retract)
{
	if (!q->retry) {
		cell *head = deref(q, get_head(p1), p1_ctx);
		cell *c = head;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c))
			convert_to_literal(q->st.m, c);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c, NULL);
			c->match = pr;
		}

		if (!pr) {
			bool found = false;

			if (get_builtin_term(q->st.m, head, &found, NULL), found)
				return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");

			q->st.curr_dbe = NULL;
			return false;
		}

		if (pr->alias) {
			c->val_off = pr->alias->key.val_off;
			pr = pr->alias;
		}

		if (!pr->is_dynamic)
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");

		find_key(q, pr, c, p1_ctx);
		share_predicate(q, pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->ugen = q->pl->ugen;
	} else {
		next_key(q);
	}

	if (!q->st.curr_dbe) {
		unshare_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	cell *p1_body = deref(q, get_logical_body(p1), p1_ctx);
	cell *orig_p1 = p1;
	const frame *f = GET_FRAME(q->st.curr_frame);
	check_heap_error(check_slot(q, MAX_ARITY));

	for (; q->st.curr_dbe; q->st.curr_dbe = q->st.curr_dbe->next) {
		CHECK_INTERRUPT();

		if (!can_view(f->ugen, q->st.curr_dbe))
			continue;

		clause *cl = &q->st.curr_dbe->cl;
		cell *c = cl->cells;
		bool needs_true = false;
		p1 = orig_p1;
		cell *c_body = get_logical_body(c);

		if (p1_body && is_var(p1_body) && !c_body) {
			p1 = deref(q, get_head(p1), p1_ctx);
			c = get_head(c);
			needs_true = true;
		}

		check_heap_error(try_me(q, cl->nbr_vars));

		if (unify(q, p1, p1_ctx, c, q->st.fp)) {
			int ok;

			if (needs_true) {
				p1_body = deref(q, p1_body, p1_ctx);
				pl_idx_t p1_body_ctx = q->latest_ctx;
				cell tmp = (cell){0};
				tmp.tag = TAG_INTERNED;
				tmp.nbr_cells = 1;
				tmp.val_off = g_true_s;
				ok = unify(q, p1_body, p1_body_ctx, &tmp, q->st.curr_frame);
			} else
				ok = true;

			return ok;
		}

		undo_me(q);
	}

	drop_choice(q);
	unshare_predicate(q, q->st.pr);
	return false;
}

// Match HEAD.
// Match HEAD :- true.

bool match_clause(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type is_retract)
{
	if (!q->retry) {
		cell *c = p1;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c))
			convert_to_literal(q->st.m, c);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c, NULL);
			c->match = pr;
		}

		if (!pr) {
			bool found = false;

			if (get_builtin_term(q->st.m, p1, &found, NULL), found) {
				if (is_retract != DO_CLAUSE)
					return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");
				else
					return throw_error(q, p1, p1_ctx, "permission_error", "access,private_procedure");
			}

			q->st.curr_dbe = NULL;
			return false;
		}

		if (pr->alias) {
			c->val_off = pr->alias->key.val_off;
			pr = pr->alias;
		}

		if (!pr->is_dynamic) {
			if (is_retract == DO_CLAUSE)
				return throw_error(q, p1, p1_ctx, "permission_error", "access,private_procedure");
			else
				return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");
		}

		find_key(q, pr, c, p1_ctx);
		share_predicate(q, pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->ugen = q->pl->ugen;
	} else {
		next_key(q);
	}

	if (!q->st.curr_dbe) {
		unshare_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);
	check_heap_error(check_slot(q, MAX_ARITY));

	for (; q->st.curr_dbe; q->st.curr_dbe = q->st.curr_dbe->next) {
		CHECK_INTERRUPT();

		if (!can_view(f->ugen, q->st.curr_dbe))
			continue;

		clause *cl = &q->st.curr_dbe->cl;
		cell *head = get_head(cl->cells);
		cell *body = get_logical_body(cl->cells);

		// Retract(HEAD) should ignore rules (and directives)

		if ((is_retract == DO_RETRACT) && body)
			continue;

		check_heap_error(try_me(q, cl->nbr_vars));

		if (unify(q, p1, p1_ctx, head, q->st.fp))
			return true;

		undo_me(q);
	}

	drop_choice(q);
	unshare_predicate(q, q->st.pr);
	return false;
}

static bool match_head(query *q)
{
	if (!q->retry) {
		cell *c = q->st.curr_cell;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c))
			convert_to_literal(q->st.m, c);

		q->save_m = q->st.m;

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c, NULL);

			if (!pr) {
				if (!is_end(c) && !(is_interned(c) && !strcmp(C_STR(q, c), "initialization"))) {
					if (q->st.m->flags.unknown == UNK_ERROR)
						return throw_error(q, c, q->st.curr_frame, "existence_error", "procedure");
					return false;
				} else
					q->error = true;

				return false;
			}

			if (pr->alias) {
				c->val_off = pr->alias->key.val_off;
				pr = pr->alias;
			}

			c->match = pr;
		}

		find_key(q, pr, c, q->st.curr_frame);
		share_predicate(q, pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->ugen = q->pl->ugen;
	} else
		next_key(q);

	if (!q->st.curr_dbe) {
		unshare_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);
	check_heap_error(check_slot(q, MAX_ARITY));

	for (; q->st.curr_dbe; next_key(q)) {
		CHECK_INTERRUPT();

		if (!can_view(f->ugen, q->st.curr_dbe))
			continue;

		clause *cl = &q->st.curr_dbe->cl;
		cell *head = get_head(cl->cells);
		check_heap_error(try_me(q, cl->nbr_vars));

		if (unify(q, q->st.curr_cell, q->st.curr_frame, head, q->st.fp)) {
			if (q->error)
				break;

			commit_me(q);
			return true;
		}

		undo_me(q);
	}

	choice *ch = GET_CURR_CHOICE();
	ch->st.iter = NULL;

	drop_choice(q);
	unshare_predicate(q, q->st.pr);
	return false;
}

static bool any_outstanding_choices(query *q)
{
	if (!q->cp)
		return false;

	const choice *ch = GET_CURR_CHOICE();

	while (ch->barrier) {
		drop_choice(q);
		ch--;
	}

	return q->cp ? true : false;
}

static bool consultall(query *q, cell *l, pl_idx_t l_ctx)
{
	if (is_string(l)) {
		char *s = DUP_STR(q, l);

		if (!load_file(q->p->m, s, false)) {
			free(s);
			return throw_error(q, l, l_ctx, "existence_error", "source_sink");
		}

		free(s);
		return true;
	}

	if (is_cyclic_term(q, l, l_ctx))
		return throw_error(q, l, l_ctx, "type_error", "callable");

	LIST_HANDLER(l);

	while (is_list(l)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (is_iso_list(h)) {
			if (consultall(q, h, h_ctx) != true)
				return false;
		} else {
			char *s = DUP_STR(q, h);

			if (!load_file(q->p->m, s, false)) {
				free(s);
				return throw_error(q, h, q->st.curr_frame, "existence_error", "source_sink");
			}

			free(s);
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	return true;
}

bool start(query *q)
{
	q->yielded = false;
	bool done = false;

	while (!done && !q->error) {
#ifndef _WIN32
		if (g_tpl_interrupt == SIGALRM) {
			g_tpl_interrupt = 0;
			bool ok = throw_error(q, q->st.curr_cell, q->st.curr_frame, "time_limit_exceeded", "timed_out");

			if (ok == false)
				q->retry = true;

			continue;
		}
#endif

		if (g_tpl_interrupt) {
			int ok = check_interrupt(q);

			if (!q->st.curr_cell)
				break;

			switch (ok) {
				case 1:
					return true;
				case -1:
					q->retry = true;
					continue;
				default:
					continue;
			}
		}

		if (q->retry) {
			Trace(q, q->st.curr_cell, q->st.curr_frame, FAIL);

			if (!retry_choice(q))
				break;
		}

		if (is_var(q->st.curr_cell)) {
			if (!fn_call_0(q, q->st.curr_cell))
				continue;
		}

		q->tot_goals++;
		q->did_throw = false;
		Trace(q, q->st.curr_cell, q->st.curr_frame, CALL);
		cell *save_cell = q->st.curr_cell;
		pl_idx_t save_ctx = q->st.curr_frame;
		q->run_hook = q->cycle_error = false;
		q->before_hook_tp = q->st.tp;

		if (is_builtin(q->st.curr_cell)) {
			if (!q->st.curr_cell->fn_ptr) {		// ??
				q->st.curr_cell++;
				continue;
			}

			bool status;

#if USE_FFI
			if (q->st.curr_cell->fn_ptr && q->st.curr_cell->fn_ptr->ffi) {
				if (q->st.curr_cell->fn_ptr->evaluable)
					status = wrapper_for_function(q, q->st.curr_cell->fn_ptr);
				else
					status = wrapper_for_predicate(q, q->st.curr_cell->fn_ptr);
			} else
#endif
				status = q->st.curr_cell->fn_ptr->fn(q);

			if (q->retry == QUERY_SKIP) {
				q->retry = QUERY_OK;
				continue;
			}

			if (!status && !q->is_oom) {
				q->retry = QUERY_RETRY;

				if (q->yielded)
					break;

				q->tot_backtracks++;
				continue;
			}

			if (q->run_hook && !q->in_hook)
				do_post_unification_hook(q, true);

			Trace(q, save_cell, save_ctx, EXIT);
			proceed(q);
		} else if (is_iso_list(q->st.curr_cell)) {
			if (consultall(q, q->st.curr_cell, q->st.curr_frame) != true) {
				q->retry = QUERY_RETRY;
				q->tot_backtracks++;
				continue;
			}

			Trace(q, save_cell, save_ctx, EXIT);
			proceed(q);
		} else {
			if (!is_callable(q->st.curr_cell)) {
				throw_error(q, q->st.curr_cell, q->st.curr_frame, "type_error", "callable");
			} else if ((match_head(q) != true) && !q->is_oom) {
				q->retry = QUERY_RETRY;
				q->tot_backtracks++;
				continue;
			}

			if (q->run_hook && !q->in_hook)
				do_post_unification_hook(q, false);

			//Trace(q, save_cell, save_ctx, EXIT);
		}

		q->run_hook = false;

		if (q->is_oom) {
			q->is_oom = q->error = false;

			if (throw_error(q, q->st.curr_cell, q->st.curr_frame, "resource_error", "memory") != true) {
				q->retry = QUERY_RETRY;
				q->tot_backtracks++;
				continue;
			}
		}

		q->resume = false;
		q->retry = QUERY_OK;

		while (!q->st.curr_cell || is_end(q->st.curr_cell)) {
			if (!resume_frame(q)) {
				while (q->cp) {
					choice *ch = GET_CURR_CHOICE();

					if (!ch->barrier)
						break;

					drop_choice(q);
				}

				if (q->p && !q->run_init && any_outstanding_choices(q)) {
					if (!check_redo(q))
						break;

					return true;
				}

				done = q->status = true;
				break;
			}

			q->resume = true;
			proceed(q);
		}
	}

	if (!q->p)
		return true;

	if (q->halt)
		q->error = false;
	else if (q->do_dump_vars && !q->abort && q->status && !q->error)
		dump_vars(q, false);

	return true;
}

#ifdef _WIN32

#define MS_PER_SEC      1000ULL     // MS = milliseconds
#define US_PER_MS       1000ULL     // US = microseconds
#define HNS_PER_US      10ULL       // HNS = hundred-nanoseconds (e.g., 1 hns = 100 ns)
#define NS_PER_US       1000ULL

#define HNS_PER_SEC     (MS_PER_SEC * US_PER_MS * HNS_PER_US)
#define NS_PER_HNS      (100ULL)    // NS = nanoseconds
#define NS_PER_SEC      (MS_PER_SEC * US_PER_MS * NS_PER_US)

static int clock_gettime_monotonic(struct timespec *tv)
{
	static LARGE_INTEGER ticksPerSec = {0};
	LARGE_INTEGER ticks;
	double seconds;

	if (!ticksPerSec.QuadPart) {
		QueryPerformanceFrequency(&ticksPerSec);
		if (!ticksPerSec.QuadPart) {
			errno = ENOTSUP;
			return -1;
		}
	}

	QueryPerformanceCounter(&ticks);
	seconds = (double) ticks.QuadPart / (double) ticksPerSec.QuadPart;
	tv->tv_sec = (time_t)seconds;
	tv->tv_nsec = (long)((ULONGLONG)(seconds * NS_PER_SEC) % NS_PER_SEC);
	return 0;
}

static int clock_gettime_realtime(struct timespec *tv)
{
	FILETIME ft;
	ULARGE_INTEGER hnsTime;
	GetSystemTimeAsFileTime(&ft);
	hnsTime.LowPart = ft.dwLowDateTime;
	hnsTime.HighPart = ft.dwHighDateTime;

	// To get POSIX Epoch as baseline, subtract the number of hns intervals from Jan 1, 1601 to Jan 1, 1970.
	hnsTime.QuadPart -= (11644473600ULL * HNS_PER_SEC);

	// modulus by hns intervals per second first, then convert to ns, as not to lose resolution
	tv->tv_nsec = (long) ((hnsTime.QuadPart % HNS_PER_SEC) * NS_PER_HNS);
	tv->tv_sec = (long) (hnsTime.QuadPart / HNS_PER_SEC);
	return 0;
}

static int my_clock_gettime(clockid_t type, struct timespec *tp)
{
	if (type == CLOCK_MONOTONIC)
		return clock_gettime_monotonic(tp);
	else if (type == CLOCK_REALTIME)
		return clock_gettime_realtime(tp);

    errno = ENOTSUP;
    return -1;
}
#else
#define my_clock_gettime clock_gettime
#endif

uint64_t cpu_time_in_usec(void)
{
	struct timespec now = {0};
#ifdef CLOCK_PROCESS_CPUTIME_ID
	my_clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &now);
#else
	my_clock_gettime(CLOCK_MONOTONIC, &now);
#endif
	return (uint64_t)(now.tv_sec * 1000 * 1000) + (now.tv_nsec / 1000);
}

uint64_t get_time_in_usec(void)
{
	struct timespec now = {0};
	my_clock_gettime(CLOCK_REALTIME, &now);
	return (uint64_t)(now.tv_sec * 1000 * 1000) + (now.tv_nsec / 1000);
}

bool execute(query *q, cell *cells, unsigned nbr_vars)
{
	q->pl->did_dump_vars = false;
	q->st.curr_cell = cells;
	q->st.sp = nbr_vars;
	q->abort = false;
	q->cycle_error = false;
	q->is_redo = false;

	// There is initially a frame (hence fp=0 is valid), so
	// this points to the next available frame...
	q->st.fp = 1;

	// There may not be a choice-point, so this points to the
	// next available choice-point
	q->cp = 0;

	frame *f = q->frames + q->st.curr_frame;
	f->initial_slots = f->actual_slots = nbr_vars;
	f->ugen = ++q->pl->ugen;
	return start(q);
}

void purge_predicate_dirty_list(query *q, predicate *pr)
{
	if (pr->ref_cnt)
		return;

	db_entry *save = NULL;

	while (q->dirty_list) {
		db_entry *dbe = q->dirty_list;
		q->dirty_list = dbe->dirty;

		if (dbe->owner == pr) {
			clear_rule(&dbe->cl);
			free(dbe);
		} else {
			dbe->dirty = save;
			save = dbe;
		}
	}

	q->dirty_list = save;
}

void purge_dirty_list(query *q)
{
	while (q->dirty_list) {
		db_entry *dbe = q->dirty_list;
		q->dirty_list = dbe->dirty;
		clear_rule(&dbe->cl);
		free(dbe);
	}
}

void destroy_query(query *q)
{
	for (page *a = q->pages; a;) {
		for (pl_idx_t i = 0; i < a->max_hp_used; i++) {
			cell *c = a->heap + i;
			unshare_cell(c);
		}

		page *save = a;
		a = a->next;
		free(save->heap);
		free(save);
	}

	for (int i = 0; i < MAX_QUEUES; i++) {
		for (pl_idx_t j = 0; j < q->qp[i]; j++) {
			cell *c = q->queue[i]+j;
			unshare_cell(c);
		}

		free(q->queue[i]);
	}

#if 0
	for (pl_idx_t i = 0; i < q->st.fp; i++) {
		const frame *f = GET_FRAME(i);

		for (pl_idx_t j = 0; j < f->actual_slots; j++) {
			slot *e = GET_SLOT(f, j);
			unshare_cell(&e->c);
		}
	}
#else
	slot *e = q->slots;

	for (pl_idx_t i = 0; i < q->st.sp; i++, e++)
		unshare_cell(&e->c);
#endif

	mp_int_clear(&q->tmp_ival);
	purge_dirty_list(q);
	free(q->trails);
	free(q->choices);
	free(q->slots);
	free(q->frames);
	free(q->tmp_heap);
	free(q);
}

query *create_query(module *m, bool is_task)
{
	static atomic_t uint64_t g_query_id = 0;

	query *q = calloc(1, sizeof(query));
	ensure(q);
	q->qid = g_query_id++;
	q->pl = m->pl;
	q->st.prev_m = q->st.m = m;
	q->trace = m->pl->trace;
	q->flags = m->flags;
	q->get_started = get_time_in_usec();
	q->time_cpu_last_started = q->time_cpu_started = cpu_time_in_usec();
	q->st.prob = 1.0;
	mp_int_init(&q->tmp_ival);

	// Allocate these now...

	q->frames_size = is_task ? INITIAL_NBR_GOALS/10 : INITIAL_NBR_GOALS;
	q->slots_size = is_task ? INITIAL_NBR_SLOTS/10 : INITIAL_NBR_SLOTS;
	q->choices_size = is_task ? INITIAL_NBR_CHOICES/10 : INITIAL_NBR_CHOICES;
	q->trails_size = is_task ? INITIAL_NBR_TRAILS/10 : INITIAL_NBR_TRAILS;

	bool error = false;
	CHECK_SENTINEL(q->frames = calloc(q->frames_size, sizeof(frame)), NULL);
	CHECK_SENTINEL(q->slots = calloc(q->slots_size, sizeof(slot)), NULL);
	CHECK_SENTINEL(q->choices = calloc(q->choices_size, sizeof(choice)), NULL);
	CHECK_SENTINEL(q->trails = calloc(q->trails_size, sizeof(trail)), NULL);

	// Allocate these later as needed...

	q->h_size = is_task ? INITIAL_NBR_HEAP_CELLS/4 : INITIAL_NBR_HEAP_CELLS;
	q->tmph_size = INITIAL_NBR_CELLS;

	for (int i = 0; i < MAX_QUEUES; i++)
		q->q_size[i] = is_task ? INITIAL_NBR_QUEUE_CELLS/4 : INITIAL_NBR_QUEUE_CELLS;

	if (error) {
		destroy_query (q);
		q = NULL;
	}

	return q;
}

query *create_sub_query(query *q, cell *curr_cell)
{
	query *subq = create_query(q->st.m, true);
	if (!subq) return NULL;
	subq->parent = q;
	subq->st.fp = 1;
	subq->is_task = true;
	subq->p = q->p;

	cell *tmp = clone_to_heap(subq, 0, curr_cell, 1);
	pl_idx_t nbr_cells = tmp->nbr_cells;
	make_end(tmp+nbr_cells);
	subq->st.curr_cell = tmp;

	frame *fsrc = GET_FRAME(q->st.curr_frame);
	frame *fdst = subq->frames;
	fdst->actual_slots = fsrc->actual_slots;

	for (unsigned i = 0; i < fsrc->actual_slots; i++) {
		slot *e = GET_SLOT(fsrc, i);
		cell *c = deref(q, &e->c, e->c.var_ctx);
		cell tmp = (cell){0};
		tmp.tag = TAG_VAR;
		tmp.var_nbr = i;
		tmp.val_off = g_anon_s;
		set_var(subq, &tmp, 0, c, q->latest_ctx);
	}

	subq->st.sp = fsrc->actual_slots;
	return subq;
}

