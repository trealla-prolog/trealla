#include <stdlib.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <time.h>

#include "module.h"
#include "network.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

#ifdef _WIN32
#include <windows.h>
#define msleep Sleep
#else
static void msleep(int ms)
{
	struct timespec tv = {0};
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

#define Trace(p1,p2,p3,p4) { q->step++; if (q->trace /*&& !consulting*/) trace_call(p1,p2,p3,p4); }

static const unsigned INITIAL_NBR_QUEUE_CELLS = 1000;
static const unsigned INITIAL_NBR_HEAP_CELLS = 1000;
static const unsigned INITIAL_NBR_SLOTS = 1000;
static const unsigned INITIAL_NBR_TRAILS = 1000;
static const unsigned INITIAL_NBR_CHOICES = 100;
static const unsigned INITIAL_NBR_FRAMES = 100;
static const unsigned INITIAL_NBR_CELLS = 100;

int g_tpl_interrupt = 0;

typedef enum { CALL, EXIT, REDO, NEXT, FAIL } box_t;

#define YIELD_INTERVAL 10000	// Goal interval between yield checks
#define REDUCE_PRESSURE 1
#define PRESSURE_FACTOR 4
#define TRACE_MEM 0

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
		else if (is_compound(c))
			printf("%s/%u ", C_STR(q, c), c->arity);

		printf("\n");
	}
}

static void trace_call(query *q, cell *c, pl_idx c_ctx, box_t box)
{
	if (!c || is_empty(c))
		return;

	if (is_builtin(c) && c->bif_ptr && !c->bif_ptr->fn)
		return;

#ifndef DEBUG
	if (c->val_off == g_sys_succeed_on_retry_s)
		return;

	if (c->val_off == g_sys_fail_on_retry_s)
		return;

	if (c->val_off == g_sys_jump_s)
		return;

	if (c->val_off == g_sys_drop_barrier_s)
		return;

	if (c->val_off == g_sys_block_catcher_s)
		return;

	if (c->val_off == g_conjunction_s)
		return;

	if (c->val_off == g_disjunction_s)
		return;
#endif

	if (box == CALL)
		box = q->retry?REDO:CALL;

	const char *src = C_STR(q, c);

	q->step++;
	SB(pr);

	SB_sprintf(pr, "[%u:%s:%"PRIu64":f%u:fp%u:cp%u:sp%u:hp%u:tp%u] ",
		q->my_chan,
		q->st.curr_m->name,
		q->step,
		q->st.curr_frame, q->st.fp, q->cp, q->st.sp, q->st.hp, q->st.tp
		);

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
	if (++q->vgen == 0) q->vgen = 1;

	if (q->creep) {
		msleep(250);
	}
}

void check_pressure(query *q)
{
#if REDUCE_PRESSURE
	if (q->tmp_heap && (q->tmph_size > 4000)) {
		free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = 1000;
	}

	if (q->trails_size > (INITIAL_NBR_TRAILS*PRESSURE_FACTOR)) {
#if TRACE_MEM
		printf("*** q->st.tp=%u, q->trails_size=%u\n", (unsigned)q->st.tp, (unsigned)q->trails_size);
#endif
		q->trails_size = alloc_grow(q, (void**)&q->trails, sizeof(trail), q->st.tp, q->st.tp*3/2, false);
	}

	if (q->choices_size > (INITIAL_NBR_CHOICES*PRESSURE_FACTOR)) {
#if TRACE_MEM
		printf("*** q->st.cp=%u, q->choices_size=%u\n", (unsigned)q->cp, (unsigned)q->choices_size);
#endif
		q->choices_size = alloc_grow(q, (void**)&q->choices, sizeof(choice), q->cp, q->cp*3/2, false);
	}

	if (q->frames_size > (INITIAL_NBR_FRAMES*PRESSURE_FACTOR)) {
#if TRACE_MEM
		printf("*** q->st.fp=%u, q->frames_size=%u\n", (unsigned)q->st.fp, (unsigned)q->frames_size);
#endif
		q->frames_size = alloc_grow(q, (void**)&q->frames, sizeof(frame), q->st.fp, q->st.fp*3/2, false);
	}

	if (q->slots_size > (INITIAL_NBR_SLOTS*PRESSURE_FACTOR)) {
#if TRACE_MEM
		printf("*** q->st.sp=%u, q->slots_size=%u\n", (unsigned)q->st.sp, (unsigned)q->slots_size);
#endif
		q->slots_size = alloc_grow(q, (void**)&q->slots, sizeof(slot), q->st.sp, q->st.sp*3/2, false);
	}
#endif
}

static bool check_trail(query *q)
{
	if (q->st.tp > q->hw_trails)
		q->hw_trails = q->st.tp;

	if (q->st.tp < q->trails_size)
		return true;

	pl_idx new_trailssize = alloc_grow(q, (void**)&q->trails, sizeof(trail), q->st.tp, q->trails_size*2, false);
	if (!new_trailssize) {
		q->oom = q->error = true;
		return false;
	}

	q->trails_size = new_trailssize;
	return true;
}

static bool check_choice(query *q)
{
	if (q->cp > q->hw_choices)
		q->hw_choices = q->cp;

	if (q->cp < q->choices_size)
		return true;

	pl_idx new_choicessize = alloc_grow(q, (void**)&q->choices, sizeof(choice), q->cp, q->choices_size*2, false);
	if (!new_choicessize) {
		q->oom = q->error = true;
		return false;
	}

	q->choices_size = new_choicessize;
	return true;
}

static bool check_frame(query *q)
{
	if (q->st.fp > q->hw_frames)
		q->hw_frames = q->st.fp;

	if (q->st.fp < q->frames_size)
		return true;

	pl_idx new_framessize = alloc_grow(q, (void**)&q->frames, sizeof(frame), q->st.fp, q->frames_size*2, false);

	if (!new_framessize) {
		q->oom = q->error = true;
		return false;
	}

	q->frames_size = new_framessize;
	return true;
}

bool check_slot(query *q, unsigned cnt)
{
	cnt += 1024;	// Why??

	pl_idx nbr = q->st.sp + cnt;

	if (q->st.sp > q->hw_slots)
		q->hw_slots = q->st.sp;

	if (nbr < q->slots_size)
		return true;

	pl_idx new_slotssize = alloc_grow(q, (void**)&q->slots, sizeof(slot), nbr, nbr*2, false);

	if (!new_slotssize) {
		q->oom = q->error = true;
		return false;
	}

	q->slots_size = new_slotssize;
	return true;
}

void make_call(query *q, cell *tmp)
{
	make_end(tmp);
	cell *c = q->st.curr_instr;
	tmp->ret_instr = c + c->nbr_cells;	// save next as the return instruction
	const frame *f = GET_CURR_FRAME();
	tmp->chgen = f->chgen;				// ... choice-generation
	tmp->mid = q->st.curr_m->id;		// ... current-module
}

void make_call_redo(query *q, cell *tmp)
{
	make_end(tmp);
	const frame *f = GET_CURR_FRAME();
	tmp->ret_instr = q->st.curr_instr;	// save the return instruction
	tmp->chgen = f->chgen;				// ... choice-generation
	tmp->mid = q->st.curr_m->id;		// ... current-module
}

void add_trail(query *q, pl_idx c_ctx, unsigned c_var_nbr, cell *attrs)
{
	if (!check_trail(q)) {
		q->error = false;
		return;
	}

	trail *tr = q->trails + q->st.tp++;
	tr->var_ctx = c_ctx;
	tr->var_nbr = c_var_nbr;
	tr->attrs = attrs;
}

cell *prepare_call(query *q, bool prefix, cell *p1, pl_idx p1_ctx, unsigned extras)
{
	unsigned nbr_cells = (prefix ? PREFIX_LEN : NOPREFIX_LEN) + p1->nbr_cells + extras;
	cell *tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;

	if (prefix) {
		// Placeholder needed for follow() to work, get's skipped
		make_instr(tmp, g_dummy_s, bif_iso_true_0, 0, 0);
	}

	cell *dst = tmp + (prefix ? PREFIX_LEN : NOPREFIX_LEN);
	dup_cells_by_ref(dst, p1, p1_ctx, p1->nbr_cells);
	return tmp;
}

const char *dump_id(const void *k, const void *v, const void *p)
{
	uint64_t id = (uint64_t)(size_t)k;
	static char tmpbuf[1024];
	sprintf(tmpbuf, "%"PRIu64"", id);
	return tmpbuf;
}

static size_t scan_is_chars_list_internal(query *q, cell *l, pl_idx l_ctx, bool allow_codes, bool *has_var, bool *is_partial)
{
	*is_partial = *has_var = false;
	size_t is_chars_list = 0;
	cell *save_l = l;
	pl_idx save_l_ctx = l_ctx;
	bool any1 = false, any2 = false;
	LIST_HANDLER(l);

	while (is_list(l) && (q->st.curr_m->flags.double_quote_chars || allow_codes)) {
		cell *h = LIST_HEAD(l);
		pl_idx h_ctx = l_ctx;
		slot *e = NULL;
		uint32_t save_vgen = 0;
		int both = 0;
		DEREF_VAR(any1, both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);
		q->suspect = h;

		if (is_var(h)) {
			*has_var = true;
			return 0;
		}

		if (!is_integer(h) && !is_iso_atom(h))
			return 0;

		if (is_integer(h) && !allow_codes)
			return 0;

		if (is_integer(h)) {
			int ch = get_smallint(h);
			char tmp[20];
			put_char_utf8(tmp, ch);
			size_t len = len_char_utf8(tmp);
			is_chars_list += len;
		} else {
			const char *src = C_STR(q, h);
			size_t len = len_char_utf8(src);

			if (len != C_STRLEN(q, h))
				return 0;

			is_chars_list += len;
		}

		if (e) e->vgen = save_vgen;
		l = LIST_TAIL(l);

#if USE_RATIONAL_TREES
		both = 0;
		DEREF_VAR(any2, both, save_vgen, e, e->vgen, l, l_ctx, q->vgen);

		if (both)
			return 0;
#else
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
#endif
	}

#if USE_RATIONAL_TREES
	if (any2) {
		cell *l2 = save_l;
		pl_idx l2_ctx = save_l_ctx;
		LIST_HANDLER(l2);

		while (is_list(l2) && (q->st.curr_m->flags.double_quote_chars || allow_codes)) {
			cell *h = LIST_HEAD(l2);
			l2 = LIST_TAIL(l2);
			RESTORE_VAR(l2, l2_ctx, l2, l2_ctx, q->vgen);
		}
	}
#endif

	if (is_var(l)) {
		is_chars_list = 0;
		*has_var = *is_partial = true;
	} else if (is_string(l))
		;
	else if (!is_interned(l) || !is_nil(l))
		is_chars_list = 0;

	return is_chars_list;
}

size_t scan_is_chars_list2(query *q, cell *l, pl_idx l_ctx, bool allow_codes, bool *has_var, bool *is_partial)
{
	if (++q->vgen == 0) q->vgen = 1;
	return scan_is_chars_list_internal(q, l, l_ctx, allow_codes, has_var, is_partial);
}

size_t scan_is_chars_list(query *q, cell *l, pl_idx l_ctx, bool allow_codes)
{
	bool has_var, is_partial;
	return scan_is_chars_list2(q, l, l_ctx, allow_codes, &has_var, &is_partial);
}

bool make_slice(query *q, cell *d, const cell *orig, size_t off, size_t n)
{
	if (!n) {
		make_atom(d, g_empty_s);
		return true;
	}

	if (is_slice(orig)) {
		*d = *orig;
		d->val_str += off;
		d->str_len = n;
		return true;
	}

	const char *s = C_STR(q, orig);

	if (is_string(orig))
		return make_stringn(d, s+off, n);

	return make_cstringn(d, s+off, n);
}

static void enter_predicate(query *q, predicate *pr)
{
	//printf("*** ENTER %s\n", C_STR(q, &pr->key));
	q->st.pr = pr;

	if (pr->is_dynamic)
		pr->refcnt++;
}

static void leave_predicate(query *q, predicate *pr)
{
	//printf("*** LEAVE %s\n", C_STR(q, &pr->key));

	if (!pr || !pr->is_dynamic || !pr->refcnt)
		return;

	sl_done(q->st.iter);
	q->st.iter = NULL;

	if (--pr->refcnt != 0)
		return;

	// Predicate is no longer being used

	if (!list_count(&pr->dirty))
		return;

	module_lock(pr->m);

	if (pr->is_abolished) {
		rule *r;

		while ((r = list_pop_front(&pr->dirty)) != NULL) {
			predicate_delink(pr, r);
			clear_clause(&r->cl);
			free(r->cl.alt);
			free(r);
		}

		module_unlock(pr->m);
		return;
	}

	// Just because this predicate is no longer in use doesn't
	// mean there are no shared references to terms contained
	// within. So move items on the predicate dirty-list to the
	// query dirty-list. They will be freed up at end of the query.
	// FIXME: this is a memory drain.

	rule *r;

	while ((r = list_pop_front(&pr->dirty)) != NULL) {
		predicate_delink(pr, r);

		if (pr->idx && pr->cnt) {
			cell *c = get_head(r->cl.cells);

			if (pr->key.arity > 1) {
				cell *arg1 = FIRST_ARG(c);
				cell *arg2 = NEXT_ARG(arg1);
				sl_rem(pr->idx2, arg2, r);
			}

			sl_rem(pr->idx, c, r);

			if (q->unify_no_tco) {
				r->cl.is_deleted = true;
				list_push_back(&q->dirty, r);
			} else {
				clear_clause(&r->cl);
				free(r->cl.alt);
				free(r);
			}
		} else {
			r->cl.is_deleted = true;
			list_push_back(&q->dirty, r);
		}
	}

	if (pr->idx && !pr->cnt) {
		sl_destroy(pr->idx2);
		sl_destroy(pr->idx);
		pr->idx = pr->idx2 = NULL;
	}

	module_unlock(pr->m);
}

void undo_me(query *q)
{
	q->tot_retries++;
	const choice *ch = GET_CURR_CHOICE();

	while (q->st.tp > ch->st.tp) {
		const trail *tr = q->trails + --q->st.tp;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		cell *c = &e->c;
		unshare_cell(c);
		c->tag = TAG_EMPTY;
		c->flags = 0;
		c->attrs = tr->attrs;
	}
}

void try_me(query *q, unsigned nbr_vars)
{
	frame *f = GET_NEW_FRAME();
	f->initial_slots = f->actual_slots = nbr_vars;
	f->base = q->st.sp;
	f->unify_no_tco = false;
	slot *e = GET_SLOT(f, 0);
	memset(e, 0, sizeof(slot)*nbr_vars);
	q->tot_matches++;
}

static void trim_trail(query *q)
{
	if (q->undo_hi_tp)
		return;

	pl_idx tp;

	if (q->cp)  {
		const choice *ch = GET_CURR_CHOICE();
		tp = ch->st.tp;
	} else
		tp = 0;

	while (q->st.tp > tp) {
		const trail *tr = q->trails + q->st.tp - 1;

		if (tr->var_ctx != q->st.curr_frame)
			break;

		q->st.tp--;
	}
}

static frame *push_frame(query *q, const clause *cl)
{
	const frame *curr_f = GET_CURR_FRAME();
	pl_idx new_frame = q->st.fp++;
	frame *f = GET_FRAME(new_frame);
	f->prev = q->st.curr_frame;
	f->curr_instr = q->st.curr_instr;
	f->hp = q->st.hp;
	f->heap_nbr = q->st.heap_nbr;
	f->initial_slots = f->actual_slots = cl->nbr_vars;
	f->chgen = ++q->chgen;
	f->has_local_vars = cl->has_local_vars;
	f->unify_no_tco = q->unify_no_tco;
	f->overflow = 0;
	q->st.sp += cl->nbr_vars;
	q->st.curr_frame = new_frame;
	return f;
}

static void reuse_frame(query *q, const clause *cl)
{
	cell *c_next = q->st.curr_instr + q->st.curr_instr->nbr_cells;

	if (c_next->val_off == g_sys_drop_barrier_s)
		drop_choice(q);

	frame *f = GET_CURR_FRAME();
	f->initial_slots = f->actual_slots = cl->nbr_vars;
	f->overflow = 0;

	const frame *newf = GET_FRAME(q->st.fp);
	const slot *from = GET_SLOT(newf, 0);
	slot *to = GET_SLOT(f, 0);

	for (pl_idx i = 0; i < cl->nbr_vars; i++, from++, to++) {
		unshare_cell(&to->c);
		to->c = from->c;
	}

	trim_trail(q);
	q->st.hp = f->hp;
	q->st.heap_nbr = f->heap_nbr;
	trim_heap(q);
	q->st.sp = f->base + cl->nbr_vars;
	q->st.curr_rule->tcos++;
	q->tot_tcos++;
}

static bool commit_any_choices(const query *q, const frame *f)
{
	if (q->cp == 1)							// Skip in-progress choice
		return false;

	const choice *ch = GET_PREV_CHOICE();	// Skip in-progress choice
	return ch->chgen > f->chgen;
}

static void commit_frame(query *q)
{
	clause *cl = &q->st.curr_rule->cl;
	cell *head = get_head(cl->cells);
	cell *body = get_body(cl->cells);
	frame *f = GET_CURR_FRAME();
	f->mid = q->st.curr_m->id;

	bool is_det = !q->has_vars && cl->is_unique;
	bool next_key = has_next_key(q);
	bool last_match = is_det || cl->is_first_cut || !next_key;
	bool tco = false;

	// Use with the help directive [tco(true)]

#if 0
	if (q->st.curr_rule->owner->is_tco)
		q->unify_no_tco = false;
#endif

	if (!q->unify_no_tco
		&& last_match
		&& (q->st.fp == (q->st.curr_frame + 1))	// At top of frame stack
		&& !q->st.curr_m->no_tco						// For CLPZ
		) {
		bool tail_call = is_tail_call(q->st.curr_instr);
		bool tail_recursive = tail_call && is_recursive_call(q->st.curr_instr);
		bool slots_ok = f->initial_slots <= cl->nbr_vars;
		bool choices = !commit_any_choices(q, f);
		tco = slots_ok && tail_recursive && choices;

#if 0
		fprintf(stderr,
			"*** %s/%u tco=%d,q->unify_no_tco=%d,last_match=%d,is_det=%d,"
			"next_key=%d,tail_call=%d/r%d,slots_ok=%d,choices=%d,"
			"cl->nbr_vars=%u,f->initial_slots=%u/%u\n",
			C_STR(q, head), head->arity,
			tco, q->unify_no_tco, last_match, is_det,
			next_key, tail_call, tail_recursive, slots_ok, choices,
			cl->nbr_vars, f->initial_slots, f->actual_slots);
#endif
	}

	if (!q->st.curr_rule->owner->is_builtin)
		q->st.curr_m = q->st.curr_rule->owner->m;

	if (q->pl->opt && tco) {
		reuse_frame(q, cl);
	} else {
		f = push_frame(q, cl);
	}

	if (last_match) {
		leave_predicate(q, q->st.pr);
		drop_choice(q);
		trim_trail(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.curr_rule = q->st.curr_rule;
		ch->chgen = f->chgen;
	}

	Trace(q, head, q->st.curr_frame, EXIT);
	q->st.curr_instr = cl->alt ? cl->alt : body;
	q->st.iter = NULL;
}

void stash_frame(query *q, const clause *cl, bool last_match)
{
	pl_idx chgen = ++q->chgen;
	unsigned nbr_vars = cl->nbr_vars;

	if (last_match) {
		Trace(q, get_head(q->st.curr_rule->cl.cells), q->st.curr_frame, EXIT);
		leave_predicate(q, q->st.pr);
		drop_choice(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.curr_rule = q->st.curr_rule;
		ch->chgen = chgen;
	}

	if (nbr_vars) {
		pl_idx new_frame = q->st.fp++;
		frame *f = GET_FRAME(new_frame);
		f->prev = q->st.curr_frame;
		f->curr_instr = NULL;
		f->chgen = chgen;
		f->overflow = 0;
		q->st.sp += nbr_vars;
	}

	q->st.iter = NULL;
}

int retry_choice(query *q)
{
	while (q->cp) {
		undo_me(q);
		pl_idx curr_choice = --q->cp;
		const choice *ch = GET_CHOICE(curr_choice);
		q->st = ch->st;

		frame *f = GET_CURR_FRAME();
		f->dbgen = ch->dbgen;
		f->chgen = ch->frame_chgen;
		f->initial_slots = ch->initial_slots;
		f->actual_slots = ch->actual_slots;
		f->overflow = ch->overflow;
		f->base = ch->base;

		if (ch->reset)
			continue;

		if (ch->catchme_exception || ch->fail_on_retry) {
			leave_predicate(q, ch->st.pr);
			continue;
		}

		if (!ch->register_cleanup && q->noretry) {
			leave_predicate(q, ch->st.pr);
			continue;
		}

		if (ch->register_cleanup && q->noretry)
			q->noretry = false;

		trim_heap(q);

		if (ch->succeed_on_retry) {
			q->st.curr_instr += ch->skip;
			return ch->skip ? -2 : -1;
		}

		return 1;
	}

	trim_heap(q);
	return 0;
}

bool push_choice(query *q)
{
	check_heap_error(check_choice(q));
	const frame *f = GET_CURR_FRAME();
	choice *ch = GET_CHOICE(q->cp++);
	ch->skip = 0;
	ch->st = q->st;
	ch->dbgen = f->dbgen;
	ch->frame_chgen = ch->chgen = f->chgen;
	ch->initial_slots = f->initial_slots;
	ch->actual_slots = f->actual_slots;
	ch->overflow = f->overflow;
	ch->base = f->base;
	ch->catchme_retry =
		ch->catchme_exception = ch->barrier = ch->register_cleanup =
		ch->block_catcher = ch->catcher = ch->fail_on_retry =
		ch->succeed_on_retry = ch->reset = false;

	return true;
}

bool push_succeed_on_retry_no_barrier(query *q, pl_idx skip)
{
	check_heap_error(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->succeed_on_retry = true;
	ch->skip = skip;
	return true;
}

// A barrier is used when making a call, it sets a new
// choice generation so that normal cuts are contained.

bool push_barrier(query *q)
{
	check_heap_error(push_choice(q));
	frame *f = GET_CURR_FRAME();
	choice *ch = GET_CURR_CHOICE();
	ch->chgen = f->chgen = ++q->chgen;
	ch->barrier = true;
	return true;
}

bool push_succeed_on_retry(query *q, pl_idx skip)
{
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->succeed_on_retry = true;
	ch->skip = skip;
	return true;
}

bool push_fail_on_retry(query *q)
{
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	return true;
}

bool push_reset_handler(query *q)
{
	check_heap_error(push_fail_on_retry(q));
	choice *ch = GET_CURR_CHOICE();
	ch->reset = true;
	return true;
}

bool push_catcher(query *q, enum q_retry retry)
{
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->catcher = true;

	if (retry == QUERY_RETRY)
		ch->catchme_retry = true;
	else if (retry == QUERY_EXCEPTION)
		ch->catchme_exception = true;

	return true;
}

// If the call is det then the barrier can be dropped...

bool drop_barrier(query *q, pl_idx cp)
{
	if ((q->cp-1) != cp)
		return false;

	if (q->cp) {
		const choice *ch = GET_CURR_CHOICE();
		frame *f = GET_CURR_FRAME();
		f->chgen = ch->frame_chgen;
	}

	drop_choice(q);
	return true;
}

void cut(query *q)
{
	const frame *f = GET_CURR_FRAME();

	while (q->cp) {
		choice *ch = GET_CURR_CHOICE();

		// A normal cut can't break out of a barrier...

		if (ch->barrier) {
			if (ch->chgen <= f->chgen)
				break;
		} else {
			if (ch->chgen < f->chgen)
				break;
		}

		leave_predicate(q, ch->st.pr);
		drop_choice(q);

		if (ch->register_cleanup && !ch->fail_on_retry) {
			cell *c = ch->st.curr_instr;
			pl_idx c_ctx = ch->st.curr_frame;
			c = deref(q, FIRST_ARG(c), c_ctx);
			c_ctx = q->latest_ctx;
			do_cleanup(q, c, c_ctx);
			break;
		}
	}
}

static bool resume_any_choices(const query *q, const frame *f)
{
	if (!q->cp)
		return false;

	const choice *ch = GET_CURR_CHOICE();
	return ch->chgen > f->chgen;
}

static void trim_frame(query *q, const frame *f)
{
#if 0
	for (unsigned i = 0; i < f->actual_slots; i++) {
		slot *e = GET_SLOT(f, i);
		cell *c = &e->c;
		unshare_cell(c);
	}
#endif

	q->st.sp -= f->actual_slots;
	q->st.fp--;
}

// Resume at next goal in previous clause...

static bool resume_frame(query *q)
{
	const frame *f = GET_CURR_FRAME();

	if (f->prev == (pl_idx)-1)
		return false;

	if (q->pl->opt
		&& !f->unify_no_tco
		&& !f->has_local_vars	// ????
		&& (q->st.fp == (q->st.curr_frame + 1))
		&& !resume_any_choices(q, f)
		) {
		q->st.hp = f->hp;
		q->st.heap_nbr = f->heap_nbr;
		trim_heap(q);
		trim_frame(q, f);
	}

	q->st.curr_instr = f->curr_instr;
	q->st.curr_frame = f->prev;
	f = GET_CURR_FRAME();
	q->st.curr_m = q->pl->modmap[f->mid];
	return true;
}

// Proceed to next goal in current clause...

static void proceed(query *q)
{
	q->st.curr_instr += q->st.curr_instr->nbr_cells;

	if (!is_end(q->st.curr_instr))
		return;

	cell *tmp = q->st.curr_instr;

	if (tmp->ret_instr) {
		frame *f = GET_CURR_FRAME();
		f->chgen = tmp->chgen;
		q->st.curr_m = q->pl->modmap[tmp->mid];
	}

	q->st.curr_instr = tmp->ret_instr;
}

#define MAX_LOCAL_VARS (1L<<30)

int create_vars(query *q, unsigned cnt)
{
	frame *f = GET_CURR_FRAME();

	if (!cnt)
		return f->actual_slots;

	if ((f->actual_slots + cnt) > MAX_LOCAL_VARS) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->oom = q->error = true;
		return -1;
	}

	unsigned var_nbr = f->actual_slots;

	if ((f->base + f->initial_slots) >= q->st.sp) {
		f->initial_slots += cnt;
	} else if (!f->overflow) {
		f->overflow = q->st.sp;
	} else if ((f->overflow + (f->actual_slots - f->initial_slots)) == q->st.sp) {
	} else {
		pl_idx save_overflow = f->overflow;
		f->overflow = q->st.sp;
		pl_idx cnt2 = f->actual_slots - f->initial_slots;

		if (!check_slot(q, cnt2)) {
			//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
			return -1;
		}

		memmove(q->slots+f->overflow, q->slots+save_overflow, sizeof(slot)*cnt2);
		q->st.sp += cnt2;
	}

	if (!check_slot(q, cnt)) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		return -1;
	}

	for (unsigned i = 0; i < cnt; i++) {
		slot *e = GET_SLOT(f, f->actual_slots + i);
		memset(e, 0, sizeof(slot));
	}

	q->st.sp += cnt;
	f->actual_slots += cnt;
	return var_nbr;
}

static bool can_view(query *q, uint64_t dbgen, const rule *r)
{
	if (r->cl.is_deleted)
		return false;

	if (r->cl.dbgen_created > dbgen)
		return false;

	if (r->cl.dbgen_retracted && (r->cl.dbgen_retracted <= dbgen))
		return false;

	return true;
}

static void setup_key(query *q)
{
	cell *arg1 = deref(q, FIRST_ARG(q->st.key), q->st.key_ctx);
	cell *arg2 = NULL;

	if (q->st.key->arity > 1)
		arg2 = deref(q, NEXT_ARG(FIRST_ARG(q->st.key)), q->st.key_ctx);

	q->st.karg1_is_ground = !is_var(arg1);
	q->st.karg2_is_ground = arg2 && !is_var(arg2);
	q->st.karg1_is_atomic = is_atomic(arg1);
	q->st.karg2_is_atomic = arg2 && is_atomic(arg2);
}

static void next_key(query *q)
{
	if (q->st.iter) {
		if (!sl_next(q->st.iter, (void*)&q->st.curr_rule)) {
			q->st.curr_rule = NULL;
			sl_done(q->st.iter);
			q->st.iter = NULL;
		}

		return;
	}

	q->st.curr_rule = q->st.curr_rule->next;
}

bool has_next_key(query *q)
{
	if (q->st.iter)
		return sl_has_next(q->st.iter, NULL);

	if (!q->st.curr_rule->next)
		return false;

	if (!q->st.key->arity)
		return true;

	if (q->st.curr_rule->cl.is_unique) {
		if ((q->st.key->arity == 1) && q->st.karg1_is_atomic)
			return false;

		if ((q->st.key->arity == 2) && q->st.karg1_is_atomic && q->st.karg2_is_atomic)
			return false;
	}

	const cell *karg1 = NULL, *karg2 = NULL;

	if (q->st.karg1_is_ground)
		karg1 = deref(q, FIRST_ARG(q->st.key), q->st.key_ctx);

	if (q->st.karg2_is_ground)
		karg2 = deref(q, NEXT_ARG(FIRST_ARG(q->st.key)), q->st.key_ctx);

	//DUMP_TERM("key ", q->st.key, q->st.key_ctx, 1);

	for (const rule *next = q->st.curr_rule->next; next; next = next->next) {
		const cell *dkey = next->cl.cells;

		if ((dkey->val_off == g_neck_s) && (dkey->arity == 2))
			dkey++;

		//DUMP_TERM("next", dkey, q->st.curr_frame, 0);

		if (karg1) {
			if (index_cmpkey(karg1, FIRST_ARG(dkey), q->st.curr_m, NULL) != 0)
				continue;
		}

		if (karg2) {
			if (index_cmpkey(karg2, NEXT_ARG(FIRST_ARG(dkey)), q->st.curr_m, NULL) != 0)
				continue;
		}

		//if (index_cmpkey(q->st.key, dkey, q->st.curr_m, NULL) == 0)
			return true;
	}

	return false;
}

static bool expand_meta_predicate(query *q, predicate *pr)
{
	unsigned arity = q->st.key->arity;
	cell *tmp = alloc_on_heap(q, q->st.key->nbr_cells*3);	// alloc max possible
	check_heap_error(tmp);
	cell *save_tmp = tmp;
	tmp += copy_cells(tmp, q->st.key, 1);

	// Expand module-sensitive args...

	for (cell *k = q->st.key+1, *m = pr->meta_args+1; arity--; k += k->nbr_cells, m += m->nbr_cells) {
		if ((k->arity == 2) && (k->val_off == g_colon_s) && is_atom(FIRST_ARG(k)))
			;
		else if (!is_interned(k) || is_iso_list(k))
			;
		else if (is_interned(m) && (m->val_off == g_colon_s)) {
			make_instr(tmp, g_colon_s, bif_iso_qualify_2, 2, 1+k->nbr_cells);
			SET_OP(tmp, OP_XFY); tmp++;
			make_atom(tmp++, new_atom(q->pl, q->st.curr_m->name));
		} else if (is_smallint(m) && is_positive(m) && (get_smallint(m) <= 9)) {
			make_instr(tmp, g_colon_s, bif_iso_qualify_2, 2, 1+k->nbr_cells);
			SET_OP(tmp, OP_XFY); tmp++;
			make_atom(tmp++, new_atom(q->pl, q->st.curr_m->name));
		}

		tmp += dup_cells(tmp, k, k->nbr_cells);
	}

	save_tmp->nbr_cells = tmp - save_tmp;
	q->st.key = save_tmp;
	return true;
}

static bool find_key(query *q, predicate *pr, cell *key, pl_idx key_ctx)
{
	q->st.iter = NULL;

	q->st.karg1_is_ground = q->st.karg2_is_ground = false;
	q->st.karg1_is_atomic = q->st.karg2_is_atomic = false;
	q->st.key = key;
	q->st.key_ctx = key_ctx;

	if (!pr->idx) {
		q->st.curr_rule = pr->head;

		if (key->arity) {
			if (pr->is_multifile || pr->is_meta_predicate) {
				q->st.key = clone_to_heap(q, key, key_ctx);
				check_heap_error(q->st.key);
				q->st.key_ctx = q->st.curr_frame;

				if (pr->is_meta_predicate) {
					if (!expand_meta_predicate(q, pr))
						return false;
				}
			}

			setup_key(q);
		}

		return true;
	}

	check_heap_error(init_tmp_heap(q));
	key = deep_clone_to_tmp(q, key, key_ctx);
	key_ctx = q->st.curr_frame;

	if (pr->is_meta_predicate) {
		if (!expand_meta_predicate(q, pr))
			return false;
	}

	cell *arg1 = key->arity ? FIRST_ARG(key) : NULL;
	skiplist *idx = pr->idx;

	if (arg1 && (is_var(arg1) || pr->is_var_in_first_arg)) {
		if (!pr->idx2) {
			q->st.curr_rule = pr->head;
			return true;
		}

		cell *arg2 = NEXT_ARG(arg1);

		if (is_var(arg2)) {
			q->st.curr_rule = pr->head;
			return true;
		}

		key = arg2;
		idx = pr->idx2;
	}

#define DEBUGIDX 0

#if DEBUGIDX
	DUMP_TERM("search, term = ", key, key_ctx);
#endif

	q->st.curr_rule = NULL;
	sliter *iter;

	if (!(iter = sl_find_key(idx, key)))
		return false;

	// If the index search has found just one (definite) solution
	// then we can use it with no problems. If more than one then
	// results must be returned in database order, so prefetch all
	// the results and return them sorted as an iterator...

	skiplist *tmp_idx = NULL;
	const rule *r;

	while (sl_next_key(iter, (void*)&r)) {
#if DEBUGIDX
		DUMP_TERM("   got, key = ", r->cl.cells, q->st.curr_frame);
#endif

		if (!tmp_idx) {
			tmp_idx = sl_create(NULL, NULL, NULL);
			sl_set_tmp(tmp_idx);
		}

		sl_set(tmp_idx, (void*)(size_t)r->db_id, (void*)r);
	}

	sl_done(iter);

	if (!tmp_idx)
		return false;

	//sl_dump(tmp_idx, dump_id, q);

	iter = sl_first(tmp_idx);

	if (!sl_next(iter, (void*)&q->st.curr_rule)) {
		sl_done(iter);
		return false;
	}

	q->st.iter = iter;
	return true;
}

// Match HEAD :- BODY.

bool match_rule(query *q, cell *p1, pl_idx p1_ctx, enum clause_type is_retract)
{
	if (!q->retry) {
		cell *c = deref(q, get_head(p1), p1_ctx);
		pl_idx c_ctx = q->latest_ctx;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c))
			convert_to_literal(q->st.curr_m, c);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.curr_m, c, NULL);
			c->match = pr;
		}

		if (!pr) {
			bool found = false;

			if (get_builtin_term(q->st.curr_m, c, &found, NULL), found)
				return throw_error(q, c, c_ctx, "permission_error", "modify,static_procedure");

			q->st.curr_rule = NULL;
			return false;
		}

		if (pr->alias) {
			c->val_off = pr->alias->key.val_off;
			pr = pr->alias;
		}

		if (!pr->is_dynamic)
			return throw_error(q, c, c_ctx, "permission_error", "modify,static_procedure");

		find_key(q, pr, c, c_ctx);
		enter_predicate(q, pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->dbgen = q->pl->dbgen;
	} else {
		next_key(q);
	}

	if (!q->st.curr_rule) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_slot(q, MAX_ARITY));
	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);
	cell *p1_body = deref(q, get_logical_body(p1), p1_ctx);
	cell *orig_p1 = p1;

	for (; q->st.curr_rule; q->st.curr_rule = q->st.curr_rule->next) {
		if (!can_view(q, f->dbgen, q->st.curr_rule))
			continue;

		clause *cl = &q->st.curr_rule->cl;
		cell *c = cl->cells;
		bool needs_true = false;
		p1 = orig_p1;
		const cell *c_body = get_logical_body(c);

		if (p1_body && is_var(p1_body) && !c_body) {
			p1 = deref(q, get_head(p1), p1_ctx);
			c = get_head(c);
			needs_true = true;
		}

		try_me(q, cl->nbr_vars);

		if (unify(q, p1, p1_ctx, c, q->st.fp)) {
			int ok;

			if (needs_true) {
				p1_body = deref(q, p1_body, p1_ctx);
				pl_idx p1_body_ctx = q->latest_ctx;
				cell tmp;
				make_instr(&tmp, g_true_s, bif_iso_true_0, 0, 0);
				ok = unify(q, p1_body, p1_body_ctx, &tmp, q->st.curr_frame);
			} else
				ok = true;

			return ok;
		}

		undo_me(q);
	}

	leave_predicate(q, q->st.pr);
	drop_choice(q);
	return false;
}

// Match HEAD.
// Match HEAD :- true.

bool match_clause(query *q, cell *p1, pl_idx p1_ctx, enum clause_type is_retract)
{
	if (!q->retry) {
		cell *c = p1;
		pl_idx c_ctx = p1_ctx;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c))
			convert_to_literal(q->st.curr_m, c);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.curr_m, c, NULL);
			c->match = pr;
		}

		if (!pr) {
			bool found = false;

			if (get_builtin_term(q->st.curr_m, p1, &found, NULL), found) {
				if (is_retract != DO_CLAUSE)
					return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");
				else
					return throw_error(q, p1, p1_ctx, "permission_error", "access,private_procedure");
			}

			q->st.curr_rule = NULL;
			return false;
		}

		if (pr->alias) {
			c->val_off = pr->alias->key.val_off;
			pr = pr->alias;
		}

		if (!pr->is_dynamic) {
			if (is_retract == DO_CLAUSE) {
				if (!q->access_private)
					return throw_error(q, p1, p1_ctx, "permission_error", "access,private_procedure");
			} else
				return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");
		}

		find_key(q, pr, c, c_ctx);
		enter_predicate(q, pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->dbgen = q->pl->dbgen;
	} else {
		next_key(q);
	}

	if (!q->st.curr_rule) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_slot(q, MAX_ARITY));
	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);

	for (; q->st.curr_rule; q->st.curr_rule = q->st.curr_rule->next) {
		if (!can_view(q, f->dbgen, q->st.curr_rule))
			continue;

		clause *cl = &q->st.curr_rule->cl;
		cell *head = get_head(cl->cells);
		const cell *body = get_logical_body(cl->cells);

		// Retract(HEAD) should ignore rules (and directives)

		if ((is_retract == DO_RETRACT) && body)
			continue;

		try_me(q, cl->nbr_vars);

		if (unify(q, p1, p1_ctx, head, q->st.fp))
			return true;

		undo_me(q);
	}

	leave_predicate(q, q->st.pr);
	drop_choice(q);
	return false;
}

static bool match_head(query *q)
{
	if (!q->retry) {
		cell *c = q->st.curr_instr;
		pl_idx c_ctx = q->st.curr_frame;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c)) {
			convert_to_literal(q->st.curr_m, c);
		}

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.curr_m, c, NULL);

			if (!pr) {
				if (!is_end(c) && !(is_interned(c) && !strcmp(C_STR(q, c), "initialization"))) {
					if (q->st.curr_m->flags.unknown == UNK_ERROR)
						return throw_error(q, c, c_ctx, "existence_error", "procedure");
					return false;
				} else
					q->error = true;

				return false;
			}

			c->flags = 0;
			c->match = pr;
		}

		if (pr->alias) {
			c->val_off = pr->alias->key.val_off;
			pr = pr->alias;
		}

		find_key(q, pr, c, c_ctx);
		enter_predicate(q, pr);
		frame *f = GET_CURR_FRAME();
		f->dbgen = q->pl->dbgen;
	} else
		next_key(q);

	if (!q->st.curr_rule) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_slot(q, MAX_ARITY));
	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	const frame *f = GET_CURR_FRAME();

	for (; q->st.curr_rule; next_key(q)) {
		if (!can_view(q, f->dbgen, q->st.curr_rule))
			continue;

		clause *cl = &q->st.curr_rule->cl;
		cell *head = get_head(cl->cells);
		try_me(q, cl->nbr_vars);
		q->st.curr_rule->attempted++;

		if (unify(q, q->st.key, q->st.key_ctx, head, q->st.fp)) {
			q->st.curr_rule->matched++;

			if (q->error)
				break;

			commit_frame(q);
			return true;
		}

		undo_me(q);
	}

	leave_predicate(q, q->st.pr);
	drop_choice(q);
	return false;
}

static bool any_outstanding_choices(query *q)
{
	while (q->cp) {
		const choice *ch = GET_CURR_CHOICE();

		if (!ch->barrier)
			break;

		q->cp--;
	}

	return q->cp > 0;
}

void do_cleanup(query *q, cell *c, pl_idx c_ctx)
{
	cell *tmp = prepare_call(q, PREFIX_LEN, c, c_ctx, 4);
	ensure(tmp);
	pl_idx nbr_cells = PREFIX_LEN + c->nbr_cells;
	make_instr(tmp+nbr_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
	make_instr(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	q->st.curr_instr = tmp;
}

static bool consultall(query *q, cell *l, pl_idx l_ctx)
{
	if (is_string(l)) {
		char *s = DUP_STRING(q, l);
		unload_file(q->p->m, s);

		if (!load_file(q->p->m, s, false, true)) {
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
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (is_iso_list(h)) {
			if (consultall(q, h, h_ctx) != true)
				return false;
		} else {
			char *s = DUP_STRING(q, h);
			unload_file(q->p->m, s);

			if (!load_file(q->p->m, s, false, true)) {
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
		if (g_tpl_interrupt) {
			switch (check_interrupt(q)) {
				case 1: return true;
				case -1: q->retry = true; continue;
				default: continue;
			}
		}

#if USE_THREADS
		if (q->thread_signal) {
			q->thread_signal = false;
			do_signal(q, q->thread_ptr);
		}
#endif

		if (q->retry) {
			int ok = retry_choice(q);

			if (!ok)
				break;

			if (ok < 0) {
				q->retry = false;

				if (ok == -1) {
					proceed(q);
					goto MORE;
				}
			}
		}

		if (!is_callable(q->st.curr_instr)) {
			if (is_var(q->st.curr_instr)) {
				cell *p1 = deref(q, q->st.curr_instr, q->st.curr_frame);
				pl_idx p1_ctx = q->latest_ctx;

				if (!bif_call_0(q, p1, p1_ctx)) {
					if (is_var(p1))
						break;

					continue;
				}
			} else
				return throw_error(q, q->st.curr_instr, q->st.curr_frame, "type_error", "callable");
		}

		Trace(q, q->st.curr_instr, q->st.curr_frame, CALL);
		cell *save_cell = q->st.curr_instr;
		pl_idx save_ctx = q->st.curr_frame;
		q->cycle_error = q->did_throw = false;
		q->tot_goals++;

		if (is_builtin(q->st.curr_instr) && q->st.curr_instr->bif_ptr) {
			q->tot_inferences++;
			bool status;

#if USE_FFI
			if (q->st.curr_instr->bif_ptr->ffi) {
				if (q->st.curr_instr->bif_ptr->evaluable)
					status = wrap_ffi_function(q, q->st.curr_instr->bif_ptr);
				else
					status = wrap_ffi_predicate(q, q->st.curr_instr->bif_ptr);
			} else
#endif
				status = q->st.curr_instr->bif_ptr->fn(q);

			if (q->retry == QUERY_NOOP) {
				q->retry = QUERY_OK;
				continue;
			}

			if (!(q->tot_goals % YIELD_INTERVAL)) {
				q->s_cnt = 0;

				if (!(q->s_cnt++ % 100))
					check_pressure(q);

				if (q->yield_at && !q->run_hook) {
					uint64_t now = get_time_in_usec() / 1000;

					if (now > q->yield_at)  {
						do_yield_then(q, status);
						break;
					}
				}
			}

			if (!status || q->abort) {
				Trace(q, q->st.curr_instr, q->st.curr_frame, FAIL);
				q->retry = QUERY_RETRY;

				if (q->yielded)
					break;

				q->tot_backtracks++;
				continue;
			}

			if (q->run_hook)
				do_post_unify_hook(q, true);

			Trace(q, save_cell, save_ctx, EXIT);
			proceed(q);
		} else if (is_iso_list(q->st.curr_instr)) {
			if (!consultall(q, q->st.curr_instr, q->st.curr_frame)) {
				Trace(q, q->st.curr_instr, q->st.curr_frame, FAIL);
				q->retry = QUERY_RETRY;
				q->tot_backtracks++;
				continue;
			}

			Trace(q, save_cell, save_ctx, EXIT);
			proceed(q);
		} else {
			q->tot_inferences++;

			if (!match_head(q)) {
				Trace(q, q->st.curr_instr, q->st.curr_frame, FAIL);
				q->retry = QUERY_RETRY;
				q->tot_backtracks++;
				continue;
			}

			if (q->run_hook)
				do_post_unify_hook(q, false);
		}

		MORE:

		q->retry = QUERY_OK;

		while (!q->st.curr_instr || is_end(q->st.curr_instr)) {
			if (resume_frame(q)) {
				proceed(q);
				continue;
			}

			if (q->p && !q->run_init && any_outstanding_choices(q)) {
				if (!check_redo(q))
					break;

				return true;
			}

			done = q->status = true;
			break;
		}

		if (q->oom) {
			q->error = true;
			printf("\nresource_error(memory). %%query terminated\n");
			break;
		}
	}

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
	q->retry = q->halt = q->error = q->abort = false;
	q->pl->did_dump_vars = false;
	q->st.curr_instr = cells;
	q->st.sp = nbr_vars;
	q->is_redo = false;

	// There is an initial frame (fp=0), so this
	// to the next available frame...

	q->st.fp = 1;

	// There may not be a choicepoint, so this points to the
	// next available choicepoint

	q->cp = 0;

	frame *f = q->frames;
	f->initial_slots = f->actual_slots = nbr_vars;
	f->dbgen = ++q->pl->dbgen;
	return start(q);
}

static void query_purge_dirty_list(query *q)
{
	unsigned cnt = 0;
	rule *r;

	while ((r = list_pop_front(&q->dirty)) != NULL) {
		clear_clause(&r->cl);
		free(r->cl.alt);
		free(r);
		cnt++;
	}

	if (cnt && 0)
		printf("*** query_purge_dirty_list %u\n", cnt);
}

void query_destroy(query *q)
{
	if (!q)
		return;

	q->done = true;

	for (page *a = q->heap_pages; a;) {
		cell *c = a->cells;

		for (pl_idx i = 0; i < a->max_idx_used; i++, c++)
			unshare_cell(c);

		page *save = a;
		a = a->next;
		free(save->cells);
		free(save);
	}

	slot *e = q->slots;

	for (pl_idx i = 0; i < q->st.sp; i++, e++) {
		cell *c = &e->c;
		unshare_cell(c);
	}

	for (int i = 0; i < MAX_QUEUES; i++) {
		cell *c = q->queue[i];
		for (pl_idx j = 0; j < q->qp[i]; j++, c++)
			unshare_cell(c);

		free(q->queue[i]);
	}

	while (q->tasks) {
		query *task = q->tasks->next;
		query_destroy(q->tasks);
		q->tasks = task;
	}

#if 0
	module *m = find_module(q->pl, "concurrent");

	if (m) {
		module_lock(m);
		predicate *pr = find_functor(m, "$future", 1);

		if (pr) {
			for (rule *r = pr->head; r; r = r->next) {
				retract_from_db(r);
			}
		}

		module_unlock(m);
	}
#endif

	mp_int_clear(&q->tmp_ival);
	mp_rat_clear(&q->tmp_irat);
	query_purge_dirty_list(q);
	free(q->trails);
	free(q->choices);
	free(q->slots);
	free(q->frames);
	free(q->tmp_heap);
	q->pl->q_cnt--;
	free(q);
}

query *query_create(module *m)
{
	static pl_atomic uint64_t g_query_id = 0;
	query *q = calloc(1, sizeof(query));
	ensure(q);
	q->flags.occurs_check = false;
	q->qid = g_query_id++;
	q->pl = m->pl;
	q->pl->q_cnt++;
	q->st.curr_m = m;
	q->trace = m->pl->trace;
	q->flags = m->flags;
	q->get_started = get_time_in_usec();
	q->time_cpu_last_started = q->cpu_started = cpu_time_in_usec();
	q->ops_dirty = true;
	q->double_quotes = false;
	q->max_depth = m->pl->def_max_depth;
	mp_int_init(&q->tmp_ival);
	mp_rat_init(&q->tmp_irat);
	clr_accum(&q->accum);

	// Allocate these now...

	q->frames_size = INITIAL_NBR_FRAMES;
	q->choices_size = INITIAL_NBR_CHOICES;
	q->slots_size = INITIAL_NBR_SLOTS;
	q->trails_size = INITIAL_NBR_TRAILS;

	ensure(q->frames = calloc(q->frames_size, sizeof(frame)), NULL);
	ensure(q->choices = calloc(q->choices_size, sizeof(choice)), NULL);
	ensure(q->slots = calloc(q->slots_size, sizeof(slot)), NULL);
	ensure(q->trails = calloc(q->trails_size, sizeof(trail)), NULL);

	// Allocate these later as needed...

	q->heap_size = INITIAL_NBR_HEAP_CELLS;
	q->tmph_size = INITIAL_NBR_CELLS;

	for (int i = 0; i < MAX_QUEUES; i++)
		q->q_size[i] = INITIAL_NBR_QUEUE_CELLS;

	frame *f = GET_CURR_FRAME();
	f->prev = (pl_idx)-1;

	clear_write_options(q);
	return q;
}

query *query_create_subquery(query *q, cell *curr_instr)
{
	query *task = query_create(q->st.curr_m);
	if (!task) return NULL;
	task->parent = q;
	task->st.fp = 1;
	task->p = q->p;

	cell *tmp = prepare_call(task, false, curr_instr, q->st.curr_frame, 1);
	pl_idx nbr_cells = tmp->nbr_cells;
	make_end(tmp+nbr_cells);
	task->st.curr_instr = tmp;

	frame *fsrc = GET_FRAME(q->st.curr_frame);
	frame *fdst = task->frames;
	fdst->initial_slots = fdst->actual_slots = fsrc->actual_slots;
	fdst->dbgen = ++q->pl->dbgen;
	task->st.sp = fdst->actual_slots;
	return task;
}

query *query_create_task(query *q, cell *curr_instr)
{
	query *task = query_create_subquery(q, curr_instr);
	if (!task) return NULL;
	task->is_task = true;
	return task;
}
