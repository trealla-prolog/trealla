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

#include "bif_atts.h"

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

#define Trace if (!(q->trace /*&& !consulting*/)) q->step++; else  trace_call

static const unsigned INITIAL_NBR_QUEUE_CELLS = 1000;
static const unsigned INITIAL_NBR_HEAP_CELLS = 8000;
static const unsigned INITIAL_NBR_FRAMES = 8000;
static const unsigned INITIAL_NBR_SLOTS = 32000;
static const unsigned INITIAL_NBR_TRAILS = 32000;
static const unsigned INITIAL_NBR_CHOICES = 8000;
static const unsigned INITIAL_NBR_CELLS = 1000;

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

	if (c->val_off == g_sys_drop_barrier_s)
		return;

	if (box == CALL)
		box = q->retry?REDO:CALL;

	const char *src = C_STR(q, c);

	if (!strcmp(src, ","))
		return;

	q->step++;
	SB(pr);

#ifdef DEBUG
	SB_sprintf(pr, "[%u:%s:%"PRIu64":f%u:fp%u:cp%u:sp%u:hp%u:tp%u] ",
		q->my_chan,
		q->st.m->name,
		q->step,
		q->st.curr_frame, q->st.fp, q->cp, q->st.sp, q->st.hp, q->st.tp
		);
#else
	SB_sprintf(pr, "[%u:%s:%"PRIu64":cp%u] ",
		q->my_chan,
		q->st.m->name,
		q->step,
		q->cp
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

	pl_idx new_trailssize = alloc_grow(q, (void**)&q->trails, sizeof(trail), q->st.tp, q->trails_size*4/3, false);
	if (!new_trailssize) {
		q->error = true;
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

	pl_idx new_choicessize = alloc_grow(q, (void**)&q->choices, sizeof(choice), q->cp, q->choices_size*4/3, false);
	if (!new_choicessize) {
		q->error = true;
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

	pl_idx new_framessize = alloc_grow(q, (void**)&q->frames, sizeof(frame), q->st.fp, q->frames_size*4/3, false);

	if (!new_framessize) {
		q->error = true;
		return false;
	}

	q->frames_size = new_framessize;
	return true;
}

bool check_slot(query *q, unsigned cnt)
{
	pl_idx nbr = q->st.sp + cnt;

	if (q->st.sp > q->hw_slots)
		q->hw_slots = q->st.sp;

	if (nbr < q->slots_size)
		return true;

	pl_idx new_slotssize = alloc_grow(q, (void**)&q->slots, sizeof(slot), nbr, nbr*4/3, false);

	if (!new_slotssize) {
		q->error = true;
		return false;
	}

	q->slots_size = new_slotssize;
	return true;
}

void add_trail(query *q, pl_idx c_ctx, unsigned c_var_nbr, cell *attrs, pl_idx attrs_ctx)
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

	while (is_list(l) && (q->st.m->flags.double_quote_chars || allow_codes)) {
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

		while (is_list(l2) && (q->st.m->flags.double_quote_chars || allow_codes)) {
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

static void enter_predicate(query *q, predicate *pr)
{
	q->st.recursive = q->st.pr == pr;
	q->st.pr = pr;

	if (pr->is_dynamic)
		pr->refcnt++;
}

static void leave_predicate(query *q, predicate *pr)
{
	q->st.recursive = false;

	if (!pr->is_dynamic || !pr->refcnt)
		return;

	module_lock(pr->m);

	if (--pr->refcnt != 0) {
		module_unlock(pr->m);
		return;
	}

	// Predicate is no longer being used

	if (!list_count(&pr->dirty)) {
		module_unlock(pr->m);
		return;
	}

	if (pr->is_abolished) {
		rule *r;

		while ((r = (rule*)list_pop_front(&pr->dirty)) != NULL) {
			list_delink(pr, r);
			clear_clause(&r->cl);
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

	while ((r = (rule*)list_pop_front(&pr->dirty)) != NULL) {
		list_delink(pr, r);

		if (pr->idx && pr->cnt) {
			cell *c = get_head(r->cl.cells);
			cell *arg1 = c->arity ? FIRST_ARG(c) : NULL;
			cell *arg2 = c->arity > 1 ? NEXT_ARG(arg1) : NULL;
			sl_rem(pr->idx2, arg2, r);
			sl_rem(pr->idx, c, r);
		}

		r->cl.is_deleted = true;
		list_push_back(&q->dirty, r);
	}

	if (pr->idx && !pr->cnt) {
		sl_destroy(pr->idx2);
		sl_destroy(pr->idx);
		pr->idx = pr->idx2 = NULL;
	}

	module_unlock(pr->m);
}

static void unwind_trail(query *q)
{
	pl_idx tp = 0;

	if (q->cp) {
		const choice *ch = GET_CURR_CHOICE();
		tp = ch->st.tp;
	}

	while (q->st.tp > tp) {
		const trail *tr = q->trails + --q->st.tp;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		cell *c = &e->c;
		unshare_cell(c);
		c->tag = TAG_EMPTY;;
		c->attrs = tr->attrs;
		c->attrs_ctx = tr->attrs_ctx;
	}
}

void undo_me(query *q)
{
	q->tot_retries++;
	unwind_trail(q);
}

void try_me(query *q, unsigned nbr_vars)
{
	frame *f = GET_NEW_FRAME();
	f->initial_slots = f->actual_slots = nbr_vars;
	f->base = q->st.sp;

	for (unsigned i = 0; i < nbr_vars; i++) {
		slot *e = GET_SLOT(f, i);
		memset(e, 0, sizeof(slot));
	}

	q->has_vars = false;
	q->no_tco = false;
	q->tot_matches++;
}

void drop_choice(query *q)
{
	if (!q->cp)
		return;

	choice *ch = GET_CURR_CHOICE();

	if (ch->st.iter) {
		sl_done(ch->st.iter);
		ch->st.iter = NULL;
	}

	//q->st.pr = NULL;
	--q->cp;
}

int retry_choice(query *q)
{
	while (q->cp) {
		unwind_trail(q);
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
			leave_predicate(q, ch->st.pr);
			return -1;
		}

		return 1;
	}

	trim_heap(q);
	return 0;
}

static frame *push_frame(query *q, const clause *cl)
{
	const frame *curr_f = GET_CURR_FRAME();
	const cell *next_cell = q->st.curr_instr + q->st.curr_instr->nbr_cells;
	pl_idx new_frame = q->st.fp++;
	frame *f = GET_FRAME(new_frame);

	// Avoid long chains of useless returns...

	if (is_end(next_cell) && !next_cell->save_ret && curr_f->curr_instr) {
		f->prev_offset = (new_frame - q->st.curr_frame) + curr_f->prev_offset;
		f->curr_instr = curr_f->curr_instr;
	} else {
		f->prev_offset = new_frame - q->st.curr_frame;
		f->curr_instr = q->st.curr_instr;
	}

	f->initial_slots = f->actual_slots = cl->nbr_vars;
	f->chgen = ++q->chgen;
	f->hp = q->st.hp;
	f->overflow = 0;
	f->no_tco = false;

	q->st.sp = f->base + cl->nbr_vars;
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
	f->chgen = ++q->chgen;
	f->overflow = 0;

	const frame *newf = GET_FRAME(q->st.fp);

	for (pl_idx i = 0; i < cl->nbr_vars; i++) {
		const slot *from = GET_SLOT(newf, i);
		slot *to = GET_SLOT(f, i);
		cell *c = &to->c;
		unshare_cell(c);
		to->c = from->c;

		if (is_ref(&to->c)) {
			if (to->c.var_ctx == q->st.fp)
				to->c.var_ctx = q->st.curr_frame;
		}
	}

	q->st.sp = f->base + cl->nbr_vars;
	q->st.hp = f->hp;
	q->st.r->tcos++;
	q->tot_tcos++;
}

static void prune_trail(query *q)
{
	while (q->st.tp) {
		const trail *tr = q->trails + q->st.tp - 1;

		if (tr->var_ctx < q->st.curr_frame)
			break;

		q->st.tp--;
	}
}

static void trim_trail(query *q)
{
	if (q->undo_hi_tp)
		return;

	if (!q->cp)
		return;

	const choice *ch = GET_CURR_CHOICE();
	pl_idx tp = ch->st.tp;

	while (q->st.tp > tp) {
		const trail *tr = q->trails + q->st.tp - 1;

		if (tr->var_ctx != q->st.curr_frame)
			break;

		q->st.tp--;
	}
}

inline static bool any_choices(const query *q, const frame *f)
{
	// Note: when in commit there is a provisional choicepoint
	// that we should ignore, hence the '1' ...

	if (q->cp == 1)
		return false;

	const choice *ch = GET_PREV_CHOICE();
	return ch->chgen > f->chgen;
}

static void commit_frame(query *q, cell *body)
{
	const clause *cl = &q->st.r->cl;
	frame *f = GET_CURR_FRAME();
	f->mid = q->st.m->id;

	if (!q->st.r->owner->is_prebuilt)
		q->st.m = q->st.r->owner->m;

	bool is_det = !q->has_vars && cl->is_unique;
	bool next_key = has_next_key(q);
	bool last_match = is_det || cl->is_first_cut || !next_key;
	bool tco = false;

	if (q->st.r->owner->is_tco)
		q->no_tco = false;

	if (!q->no_tco && !f->no_tco && !q->st.m->no_tco && last_match
			&& (q->st.fp == (q->st.curr_frame + 1))) {
		bool tail_call = is_tail_call(q->st.curr_instr);
		bool tail_recursive = tail_call && q->st.recursive;
		bool vars_ok =
			tail_recursive ? f->initial_slots == cl->nbr_vars :
			false;
		bool choices = any_choices(q, f);
		tco = vars_ok && !choices;

#if 0
		const cell *head = get_head((cell*)cl->cells);
		fprintf(stderr,
			"*** %s/%u tco=%d,q->no_tco=%d,last_match=%d,is_det=%d,"
			"next_key=%d,tail_call=%d/%d,vars_ok=%d,choices=%d,"
			"cl->nbr_vars=%u,f->initial_slots=%u/%u\n",
			C_STR(q, head), head->arity,
			tco, q->no_tco, last_match, is_det,
			next_key, tail_call, tail_recursive, vars_ok, choices,
			cl->nbr_vars, f->initial_slots, f->actual_slots);
#endif
	}

	if (q->pl->opt && tco) {
		reuse_frame(q, cl);
		prune_trail(q);
	} else {
		f = push_frame(q, cl);

		// If matching against a fact then drop new frame...

		if (q->pl->opt && !cl->nbr_vars && !body)
			q->st.fp--;
	}

	if (last_match) {
		leave_predicate(q, q->st.pr);
		drop_choice(q);
		cut(q); // ???
		trim_trail(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.r = q->st.r;
		ch->chgen = f->chgen;
	}

	Trace(q, get_head(q->st.r->cl.cells), q->st.curr_frame, EXIT);

	q->st.curr_instr = body;
	q->st.iter = NULL;
}

void stash_frame(query *q, const clause *cl, bool last_match)
{
	pl_idx chgen = ++q->chgen;

	if (last_match) {
		Trace(q, get_head(q->st.r->cl.cells), q->st.curr_frame, EXIT);
		leave_predicate(q, q->st.pr);
		drop_choice(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.r = q->st.r;
		ch->chgen = chgen;
	}

	unsigned nbr_vars = cl->nbr_vars;

	if (nbr_vars) {
		pl_idx new_frame = q->st.fp++;
		frame *f = GET_FRAME(new_frame);
		f->prev_offset = new_frame - q->st.curr_frame;
		f->curr_instr = NULL;
		f->chgen = chgen;
		f->overflow = 0;
		q->st.sp += nbr_vars;
	}

	q->st.iter = NULL;
}

bool push_choice(query *q)
{
	check_heap_error(check_choice(q));
	const frame *f = GET_CURR_FRAME();
	pl_idx curr_choice = q->cp++;
	choice *ch = GET_CHOICE(curr_choice);
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

// A barrier is used when making a call, it sets a new
// choice generation so that normal cuts are contained.
// This is because there is no separate choice stack.

bool push_barrier(query *q)
{
	check_heap_error(push_choice(q));
	frame *f = GET_CURR_FRAME();
	choice *ch = GET_CURR_CHOICE();
	ch->chgen = f->chgen = ++q->chgen;
	ch->barrier = true;
	return true;
}

// A reset adds the ability to do locate
// delimited continuations

bool push_reset_handler(query *q)
{
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->reset = true;
	ch->fail_on_retry = true;
	return true;
}

// A catcher adds the ability to trap exceptions.

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
	if ((q->cp-1) == cp) {
		drop_choice(q);

		if (q->cp) {
			const choice *ch = GET_CURR_CHOICE();
			frame *f = GET_CURR_FRAME();
			f->chgen = ch->chgen;
		}

		return true;
	}

	return false;
}

void cut(query *q)
{
	const frame *f = GET_CURR_FRAME();
	choice *ch = GET_CURR_CHOICE();

	while (q->cp) {
		// A normal cut can't break out of a barrier...

		if (ch->barrier) {
			if (ch->chgen <= f->chgen)
				break;
		} else {
			if (ch->chgen < f->chgen)
				break;
		}

		const frame *f2 = GET_FRAME(ch->st.curr_frame);

		if ((ch->st.fp == (q->st.curr_frame + 1))
			&& (f2->actual_slots == 0)
			) {
				q->st.fp = ch->st.fp;
		}

		leave_predicate(q, ch->st.pr);
		drop_choice(q);

		if (ch->register_cleanup && !ch->fail_on_retry) {
			ch->fail_on_retry = true;
			cell *c = ch->st.curr_instr;
			pl_idx c_ctx = ch->st.curr_frame;
			c = deref(q, FIRST_ARG(c), c_ctx);
			c_ctx = q->latest_ctx;
			do_cleanup(q, c, c_ctx);
			break;
		}

		ch--;
	}

	//if (!q->cp && !q->undo_hi_tp)
	//	q->st.tp = 0;
}

// Resume next goal in previous clause...

inline static bool resume_frame(query *q)
{
	const frame *f = GET_CURR_FRAME();

	if (!f->prev_offset)
		return false;

	if (q->in_call)
		q->in_call--;

	q->st.curr_instr = f->curr_instr;
	q->st.curr_frame = q->st.curr_frame - f->prev_offset;
	f = GET_CURR_FRAME();
	q->st.m = q->pl->modmap[f->mid];
	return true;
}

// Proceed to next goal in current clause...

inline static void proceed(query *q)
{
	q->st.curr_instr += q->st.curr_instr->nbr_cells;
	frame *f = GET_CURR_FRAME();

	while (is_end(q->st.curr_instr)) {
		if (q->st.curr_instr->save_ret) {
			f->chgen = q->st.curr_instr->chgen;
			//q->st.m = q->pl->modmap[q->st.curr_instr->mid];
		}

		if (!(q->st.curr_instr = q->st.curr_instr->save_ret))
			break;
	}
}

#define MAX_LOCAL_VARS (1L<<30)

int create_vars(query *q, unsigned cnt)
{
	frame *f = GET_CURR_FRAME();

	if (!cnt)
		return f->actual_slots;

	if ((f->actual_slots + cnt) > MAX_LOCAL_VARS) {
		printf("*** Oops %s %d\n", __FILE__, __LINE__);
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
			printf("*** Oops %s %d\n", __FILE__, __LINE__);
			return -1;
		}

		memmove(q->slots+f->overflow, q->slots+save_overflow, sizeof(slot)*cnt2);
		q->st.sp += cnt2;
	}

	if (!check_slot(q, cnt)) {
		printf("*** Oops %s %d\n", __FILE__, __LINE__);
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

	if (!is_var(arg1))
		q->st.karg1_is_ground = true;

	if (arg2 && !is_var(arg2))
		q->st.karg2_is_ground = true;

	if (is_atomic(arg1))
		q->st.karg1_is_atomic = true;

	if (arg2 && is_atomic(arg2))
		q->st.karg2_is_atomic = true;
}

static void next_key(query *q)
{
	if (q->st.iter) {
		if (!sl_next(q->st.iter, (void*)&q->st.r)) {
			q->st.r = NULL;
			sl_done(q->st.iter);
			q->st.iter = NULL;
		}

		return;
	}

	q->st.r = q->st.r->next;
}

bool has_next_key(query *q)
{
	if (q->st.iter)
		return sl_is_next(q->st.iter, NULL);

	if (!q->st.r->next)
		return false;

	if (!q->st.key->arity)
		return true;

	if (q->st.r->cl.is_unique) {
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

	for (const rule *next = q->st.r->next; next; next = next->next) {
		const cell *dkey = next->cl.cells;

		if ((dkey->val_off == g_neck_s) && (dkey->arity == 2))
			dkey++;

		//DUMP_TERM("next", dkey, q->st.curr_frame, 0);

		if (karg1) {
			if (index_cmpkey(karg1, FIRST_ARG(dkey), q->st.m, NULL) != 0)
				continue;
		}

		if (karg2) {
			if (index_cmpkey(karg2, NEXT_ARG(FIRST_ARG(dkey)), q->st.m, NULL) != 0)
				continue;
		}

		//if (index_cmpkey(q->st.key, dkey, q->st.m, NULL) == 0)
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
		else if (!is_interned(k))
			;
		else if (is_interned(m) && (m->val_off == g_colon_s)) {
			make_struct(tmp, g_colon_s, bif_iso_invoke_2, 2, 1+k->nbr_cells);
			SET_OP(tmp, OP_XFY); tmp++;
			make_atom(tmp++, new_atom(q->pl, q->st.m->name));
		} else if (is_positive(m) && (get_smallint(m) <= 9)) {
			make_struct(tmp, g_colon_s, bif_iso_invoke_2, 2, 1+k->nbr_cells);
			SET_OP(tmp, OP_XFY); tmp++;
			make_atom(tmp++, new_atom(q->pl, q->st.m->name));
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

	q->st.karg1_is_ground = false;
	q->st.karg2_is_ground = false;
	q->st.karg1_is_atomic = false;
	q->st.karg2_is_atomic = false;
	q->st.key = key;
	q->st.key_ctx = key_ctx;

	if (!pr->idx) {
		q->st.r = pr->head;

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
			q->st.r = pr->head;
			return true;
		}

		cell *arg2 = NEXT_ARG(arg1);

		if (is_var(arg2)) {
			q->st.r = pr->head;
			return true;
		}

		key = arg2;
		idx = pr->idx2;
	}

#define DEBUGIDX 0

#if DEBUGIDX
	DUMP_TERM("search, term = ", key, key_ctx);
#endif

	q->st.r = NULL;
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

	if (!sl_next(iter, (void*)&q->st.r)) {
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
			convert_to_literal(q->st.m, c);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c, NULL);
			c->match = pr;
		}

		if (!pr) {
			bool found = false;

			if (get_builtin_term(q->st.m, c, &found, NULL), found)
				return throw_error(q, c, c_ctx, "permission_error", "modify,static_procedure");

			q->st.r = NULL;
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

	if (!q->st.r) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_slot(q, MAX_ARITY));
	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);
	cell *p1_body = deref(q, get_logical_body(p1), p1_ctx);
	cell *orig_p1 = p1;

	for (; q->st.r; q->st.r = q->st.r->next) {
		if (!can_view(q, f->dbgen, q->st.r))
			continue;

		clause *cl = &q->st.r->cl;
		cell *c = cl->cells;
		bool needs_true = false;
		p1 = orig_p1;
		cell *c_body = get_logical_body(c);

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
				tmp.tag = TAG_INTERNED;
				tmp.arity = 0;
				tmp.nbr_cells = 1;
				tmp.flags = FLAG_BUILTIN;
				tmp.val_off = g_true_s;
				static builtins *s_fn_ptr = NULL;

				if (!s_fn_ptr)
					s_fn_ptr = get_fn_ptr(bif_iso_true_0);

				tmp.bif_ptr = s_fn_ptr;
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

			q->st.r = NULL;
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

	if (!q->st.r) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_slot(q, MAX_ARITY));
	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);

	for (; q->st.r; q->st.r = q->st.r->next) {
		if (!can_view(q, f->dbgen, q->st.r))
			continue;

		clause *cl = &q->st.r->cl;
		cell *head = get_head(cl->cells);
		cell *body = get_logical_body(cl->cells);

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
		else if (is_cstring(c))
			convert_to_literal(q->st.m, c);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c, NULL);

			if (!pr) {
				if (!is_end(c) && !(is_interned(c) && !strcmp(C_STR(q, c), "initialization"))) {
					if (q->st.m->flags.unknown == UNK_ERROR)
						return throw_error(q, c, c_ctx, "existence_error", "procedure");
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

		find_key(q, pr, c, c_ctx);
		enter_predicate(q, pr);
		frame *f = GET_FRAME(q->st.curr_frame);
		f->dbgen = q->pl->dbgen;
	} else
		next_key(q);

	if (!q->st.r) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	check_heap_error(check_slot(q, MAX_ARITY));
	check_heap_error(check_frame(q));
	check_heap_error(push_choice(q));
	const frame *f = GET_FRAME(q->st.curr_frame);

	for (; q->st.r; next_key(q)) {
		if (!can_view(q, f->dbgen, q->st.r))
			continue;

		clause *cl = &q->st.r->cl;
		cell *head = get_head(cl->cells);
		try_me(q, cl->nbr_vars);
		q->st.r->attempted++;

		if (unify(q, q->st.key, q->st.key_ctx, head, q->st.fp)) {
			q->st.r->matched++;

			if (q->error)
				break;

			commit_frame(q, get_body(cl->cells));
			return true;
		}

		undo_me(q);
	}

	if (q->cp) {
		choice *ch = GET_CURR_CHOICE();
		ch->st.iter = NULL;
		drop_choice(q);
	}

	leave_predicate(q, q->st.pr);
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

static bool consultall(query *q, cell *l, pl_idx l_ctx)
{
	if (is_string(l)) {
		char *s = DUP_STRING(q, l);
		unload_file(q->p->m, s);

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
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (is_iso_list(h)) {
			if (consultall(q, h, h_ctx) != true)
				return false;
		} else {
			char *s = DUP_STRING(q, h);
			unload_file(q->p->m, s);

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
				proceed(q);
				q->retry = false;
				goto MORE;
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
		q->cycle_error = false;
		q->did_throw = false;
		q->max_eval_depth = 0;
		q->tot_goals++;

		if (is_builtin(q->st.curr_instr)) {
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

			if (q->retry == QUERY_NOSKIPARG) {
				q->retry = QUERY_OK;
				continue;
			}

			if (q->tot_goals % YIELD_INTERVAL == 0) {
				q->s_cnt = 0;

				if (!(q->s_cnt++ % 100))
					check_pressure(q);

				if (q->yield_at) {
					uint64_t now = get_time_in_usec() / 1000;

					if (now > q->yield_at)  {
						do_yield(q, 0);
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
				do_post_unification_hook(q, true);

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
				do_post_unification_hook(q, false);
		}

		MORE:

		q->retry = QUERY_OK;

		while (!q->st.curr_instr || is_end(q->st.curr_instr)) {
			if (resume_frame(q)) {
				proceed(q);
				continue;
			}

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

		if (q->oom) {
			if (q->oom++ == 99) {
				q->error = true;
				printf("\n%%terminated\n");
				break;
			}
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

	frame *f = q->frames + q->st.curr_frame;
	f->initial_slots = f->actual_slots = nbr_vars;
	f->dbgen = ++q->pl->dbgen;
	return start(q);
}

static void query_purge_dirty_list(query *q)
{
	unsigned cnt = 0;
	rule *r;

	while ((r = (rule *)list_pop_front(&q->dirty)) != NULL) {
		clear_clause(&r->cl);
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

	for (int i = 0; i < MAX_QUEUES; i++) {
		cell *c = q->queue[i];

		for (pl_idx j = 0; j < q->qp[i]; j++, c++)
			unshare_cell(c);

		free(q->queue[i]);
	}

	slot *e = q->slots;

	for (pl_idx i = 0; i < q->st.sp; i++, e++) {
		cell *c = &e->c;
		unshare_cell(c);
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

	module *m = (module*)list_front(&q->pl->modules);

	while (m) {
		module_lock(m);
		predicate *pr = find_functor(m, "$bb_key", 3);

		if (pr) {
			rule *r = pr->head;

			while (r) {
				cell *c = r->cl.cells;
				cell *arg1 = c + 1;
				cell *arg2 = arg1 + arg1->nbr_cells;
				cell *arg3 = arg2 + arg2->nbr_cells;

				if (!CMP_STRING_TO_CSTR(m, arg3, "b")) {
					pr->cnt--;
					list_delink(pr, r);
					rule *save = r;
					r = r->next;
					clear_clause(&save->cl);
					free(save);
				} else
					r = r->next;
			}
		}

		module_unlock(m);
		m = (module*)list_next(m);
	}

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

query *query_create(module *m, bool is_task)
{
	static pl_atomic uint64_t g_query_id = 0;
	query *q = calloc(1, sizeof(query));
	ensure(q);
	q->flags.occurs_check = false;
	q->qid = g_query_id++;
	q->pl = m->pl;
	q->pl->q_cnt++;
	q->st.m = m;
	q->trace = m->pl->trace;
	q->flags = m->flags;
	q->get_started = get_time_in_usec();
	q->time_cpu_last_started = q->cpu_started = cpu_time_in_usec();
	q->ops_dirty = true;
	q->double_quotes = false;
	q->st.prob = 1.0;
	q->max_depth = m->pl->def_max_depth;
	mp_int_init(&q->tmp_ival);
	mp_rat_init(&q->tmp_irat);
	clr_accum(&q->accum);

	// Allocate these now...

	q->frames_size = is_task ? 100 : INITIAL_NBR_FRAMES;
	q->choices_size = is_task ? 100 : INITIAL_NBR_CHOICES;
	q->slots_size = is_task ? 1000 : INITIAL_NBR_SLOTS;
	q->trails_size = is_task ? 1000 : INITIAL_NBR_TRAILS;

	ensure(q->frames = calloc(q->frames_size, sizeof(frame)), NULL);
	ensure(q->choices = calloc(q->choices_size, sizeof(choice)), NULL);
	ensure(q->slots = calloc(q->slots_size, sizeof(slot)), NULL);
	ensure(q->trails = calloc(q->trails_size, sizeof(trail)), NULL);

	// Allocate these later as needed...

	q->heap_size = INITIAL_NBR_HEAP_CELLS;
	q->tmph_size = INITIAL_NBR_CELLS;

	for (int i = 0; i < MAX_QUEUES; i++)
		q->q_size[i] = INITIAL_NBR_QUEUE_CELLS;

	clear_write_options(q);
	return q;
}

query *query_create_subquery(query *q, cell *curr_instr)
{
	query *task = query_create(q->st.m, true);
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
