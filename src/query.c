#include <stdlib.h>
#include <stdlib.h>
#include <stdio.h>
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

#define Trace(p1,p2,p3,p4) if (q->trace /*&& !consulting*/) trace_call(p1,p2,p3,p4)

#define DEBUG_MATCH if (0)

static const unsigned INITIAL_NBR_QUEUE_CELLS = 1000;
static const unsigned INITIAL_NBR_HEAP_CELLS = 1000;
static const unsigned INITIAL_NBR_SLOTS = 1000;
static const unsigned INITIAL_NBR_TRAILS = 1000;
static const unsigned INITIAL_NBR_CHOICES = 100;
static const unsigned INITIAL_NBR_FRAMES = 100;
static const unsigned INITIAL_NBR_CELLS = 100;

int g_tpl_interrupt = 0;

typedef enum { CALL, EXIT, REDO, NEXT, FAIL } box_t;

#define YIELD_INTERVAL 100000	// Goal interval between yield checks
#define REDUCE_PRESSURE 1
#define PRESSURE_FACTOR 4
#define TRACE_MEM 0

void dump_term(query *q, const char *s, const cell *c)
{
	unsigned num_cells = c->num_cells;
	printf("*** %s\n", s);

	for (unsigned i = 0; i < num_cells; i++, c++) {
		printf("    ");
		printf("[%u] tag=%u ", i, c->tag);

		if (is_atom(c))
			printf("%s ", C_STR(q, c));
		else if (is_var(c))
			printf("_%u ", c->var_num);
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

#if 0
	if (!is_builtin(c)) {
		predicate *pr = find_predicate(q->st.m, c);

		if (pr && !pr->is_public)
			return;
	}
#endif

	if (box == CALL)
		box = q->retry?REDO:CALL;

	const char *src = C_STR(q, c);

	q->step++;
	SB(pr);

	SB_sprintf(pr, "[%u:%s:%"PRIu64":f%u:fp%u:cp%u:sp%u:hp%u:tp%u] ",
		q->my_chan,
		q->st.m->name,
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
		q->trails_size = alloc_grow(q, (void**)&q->trails, sizeof(trail), q->st.tp, q->st.tp*5/4);
	}

	if (q->choices_size > (INITIAL_NBR_CHOICES*PRESSURE_FACTOR)) {
#if TRACE_MEM
		printf("*** q->st.cp=%u, q->choices_size=%u\n", (unsigned)q->cp, (unsigned)q->choices_size);
#endif
		q->choices_size = alloc_grow(q, (void**)&q->choices, sizeof(choice), q->cp, q->cp*5/4);
	}

	if (q->frames_size > (INITIAL_NBR_FRAMES*PRESSURE_FACTOR)) {
#if TRACE_MEM
		printf("*** q->st.fp=%u, q->frames_size=%u\n", (unsigned)q->st.fp, (unsigned)q->frames_size);
#endif
		q->frames_size = alloc_grow(q, (void**)&q->frames, sizeof(frame), q->st.fp, q->st.fp*5/4);
	}

	if (q->slots_size > (INITIAL_NBR_SLOTS*PRESSURE_FACTOR)) {
#if TRACE_MEM
		printf("*** q->st.sp=%u, q->slots_size=%u\n", (unsigned)q->st.sp, (unsigned)q->slots_size);
#endif
		q->slots_size = alloc_grow(q, (void**)&q->slots, sizeof(slot), q->st.sp, q->st.sp*5/4);
	}
#endif
}

static bool check_choice(query *q)
{
	if (q->cp > q->hw_choices)
		q->hw_choices = q->cp;

	if (q->cp < q->choices_size)
		return true;

	q->realloc_choices++;
	pl_idx new_choicessize = alloc_grow(q, (void**)&q->choices, sizeof(choice), q->cp, q->choices_size*5/4);

	if (!new_choicessize) {
		q->oom = q->error = true;
		return false;
	}

	q->choices_size = new_choicessize;
	return true;
}

bool check_frame(query *q, unsigned max_vars)
{
	checked(check_slot(q, max_vars));

	if (q->st.fp > q->hw_frames)
		q->hw_frames = q->st.fp;

	if (q->st.fp < q->frames_size) {
		frame *f = GET_NEW_FRAME();
		f->max_vars = max_vars;
		f->base = q->st.sp;
		return true;
	}

	q->realloc_frames++;
	pl_idx new_framessize = alloc_grow(q, (void**)&q->frames, sizeof(frame), q->st.fp, q->frames_size*5/4);

	if (!new_framessize) {
		q->oom = q->error = true;
		return false;
	}

	q->frames_size = new_framessize;
	frame *f = GET_NEW_FRAME();
	f->max_vars = max_vars;
	f->base = q->st.sp;
	return true;
}

bool check_slot(query *q, unsigned cnt)
{
	cnt += 1024;	// Why??

	pl_idx num = q->st.sp + cnt;

	if (q->st.sp > q->hw_slots)
		q->hw_slots = q->st.sp;

	if (num < q->slots_size)
		return true;

	q->realloc_slots++;
	pl_idx new_slotssize = alloc_grow(q, (void**)&q->slots, sizeof(slot), num, num*5/4);

	if (!new_slotssize) {
		q->oom = q->error = true;
		return false;
	}

	q->slots_size = new_slotssize;
	return true;
}

bool check_trail(query *q)
{
	if (q->st.tp > q->hw_trails)
		q->hw_trails = q->st.tp;

	if (q->st.tp < q->trails_size)
		return true;

	q->realloc_trails++;
	pl_idx new_trailssize = alloc_grow(q, (void**)&q->trails, sizeof(trail), q->st.tp, q->trails_size*5/4);

	if (!new_trailssize) {
		q->oom = q->error = true;
		return false;
	}

	q->trails_size = new_trailssize;
	return true;
}

void make_call(query *q, cell *tmp)
{
	make_end(tmp);
	const frame *f = GET_CURR_FRAME();
	cell *c = q->st.instr;
	tmp->ret_instr = c + c->num_cells;	// save next as the return instruction
	tmp->chgen = f->chgen;				// ... choice-generation
	tmp->mid = q->st.m->id;				// ... current-module
}

void make_call_redo(query *q, cell *tmp)
{
	make_end(tmp);
	const frame *f = GET_CURR_FRAME();
	tmp->ret_instr = q->st.instr;		// save the return instruction
	tmp->chgen = f->chgen;				// ... choice-generation
	tmp->mid = q->st.m->id;				// ... current-module
}

cell *prepare_call(query *q, bool noskip, cell *p1, pl_idx p1_ctx, unsigned extras)
{
	unsigned num_cells = p1->num_cells + extras;
	cell *tmp = alloc_heap(q, num_cells);
	if (!tmp) return NULL;
	q->noskip = noskip;
	dup_cells_by_ref(tmp, p1, p1_ctx, p1->num_cells);
	return tmp;
}

const char *dump_id(const void *k, const void *v, const void *p)
{
	uint64_t id = (uint64_t)(size_t)k;
	static char tmpbuf[1024];
	sprintf(tmpbuf, "%"PRIu64"", id);
	return tmpbuf;
}

static size_t scan_is_chars_list_internal(query *q, cell *l, pl_idx l_ctx, bool allow_codes, bool *has_var, bool *is_partial, cell **cptr)
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
		cell *lsave = l;

		both = 0;
		DEREF_VAR(any2, both, save_vgen, e, e->vgen, l, l_ctx, q->vgen);

		if (both) {
			*is_partial = true;
			save_l = lsave;
			break;
		}
	}

	if (any2 && !*is_partial) {
		cell *l2 = save_l;
		pl_idx l2_ctx = save_l_ctx;
		LIST_HANDLER(l2);

		while (is_list(l2) && (q->st.m->flags.double_quote_chars || allow_codes)) {
			LIST_HEAD(l2);
			l2 = LIST_TAIL(l2);
			RESTORE_VAR(l2, l2_ctx, l2, l2_ctx, q->vgen);
		}
	}

	if (is_var(l)) {
		*has_var = *is_partial = true;
		if (cptr) *cptr = l;
	} else if ((is_interned(l) || is_string(l) || is_number(l)) && !is_nil(l)) {
		*is_partial = true;
		if (cptr) *cptr = save_l;
	} else if (!is_interned(l) || !is_nil(l))
		is_chars_list = 0;

	return is_chars_list;
}

size_t scan_is_chars_list2(query *q, cell *l, pl_idx l_ctx, bool allow_codes, bool *has_var, bool *is_partial, cell **cptr)
{
	if (++q->vgen == 0) q->vgen = 1;
	return scan_is_chars_list_internal(q, l, l_ctx, allow_codes, has_var, is_partial, cptr);
}

size_t scan_is_chars_list(query *q, cell *l, pl_idx l_ctx, bool allow_codes)
{
	bool has_var, is_partial;
	return scan_is_chars_list2(q, l, l_ctx, allow_codes, &has_var, &is_partial, NULL);
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

#define MAX_LOCAL_VARS (1L<<30)

int create_vars(query *q, unsigned cnt)
{
	frame *f = GET_CURR_FRAME();

	if (!cnt)
		return f->actual_slots;

	if ((f->actual_slots + cnt) > MAX_LOCAL_VARS) {
		q->oom = q->error = true;
		return -1;
	}

	if (!check_slot(q, cnt)) {
		q->error = true;
		return -1;
	}

	unsigned var_num = f->actual_slots;

	if (!f->op && ((f->base + f->initial_slots) == q->st.sp)) {
		f->initial_slots += cnt;
	} else if (!f->op) {
		f->op = q->st.sp;
	} else if ((f->op + (f->actual_slots - f->initial_slots)) == q->st.sp) {
	} else {
		pl_idx save_overflow = f->op;
		f->op = q->st.sp;
		pl_idx cnt2 = f->actual_slots - f->initial_slots;

		if (!check_slot(q, cnt2)) {
			q->error = true;
			return -1;
		}

		memmove(q->slots+f->op, q->slots+save_overflow, sizeof(slot)*cnt2);
		q->st.sp += cnt2;
	}

	slot *e = get_slot(q, f, f->actual_slots);
	memset(e, 0, sizeof(slot)*cnt);
	q->st.sp += cnt;
	f->actual_slots += cnt;
	return var_num;
}

static void enter_predicate(query *q, predicate *pr)
{
	frame *f = GET_FRAME(q->st.curr_frame);
	f->dbgen = q->pl->dbgen;
	q->st.pr = pr;

	if (pr->is_dynamic)
		pr->refcnt++;
}

static void leave_predicate(query *q, predicate *pr)
{
	if (!pr)
		return;

	sl_done(q->st.iter);
	q->st.iter = NULL;

	if (!pr->is_dynamic || !pr->refcnt)
		return;

	if (--pr->refcnt != 0)
		return;

	if (!list_count(&pr->dirty))
		return;

	if (pr->is_abolished)
		return;

	// Predicate is no longer being used

	module_lock(pr->m);
	rule *r;

	while ((r = list_pop_front(&pr->dirty)) != NULL) {
		predicate_delink(pr, r);

		if (pr->idx1 && pr->cnt) {
			cell *c = get_head(r->cl.cells);
			sl_rem(pr->idx1, c, r);

			if (pr->idx2 && (pr->key.arity > 1)) {
				cell *arg1 = FIRST_ARG(c);
				cell *arg2 = NEXT_ARG(arg1);
				sl_rem(pr->idx2, arg2, r);
			}
		}

		// Just because this predicate is no longer in use doesn't
		// mean there are no shared references to terms contained
		// within. So move items on the predicate dirty-list to the
		// query dirty-list. They will be freed up at end of the query.
		// FIXME: this is a memory drain, should a retracted term be
		// copied to the heap?.

		if (true) {
			r->cl.is_deleted = true;
			list_push_back(&q->dirty, r);
		} else {
			clear_clause(&r->cl);
			free(r);
		}
	}

	if (pr->idx1 && !pr->cnt) {
		sl_destroy(pr->idx2);
		sl_destroy(pr->idx1);
		pr->idx1 = pr->idx2 = NULL;
	}

	module_unlock(pr->m);
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

		if (tr->val_ctx != q->st.curr_frame)
			break;

		q->st.tp--;
	}
}

static void trim_frame(query *q, const frame *f)
{
	for (unsigned i = 0; i < f->actual_slots; i++) {
		slot *e = get_slot(q, f, i);
		cell *c = &e->c;
		unshare_cell(c);
		c->tag = TAG_EMPTY;
		c->val_attrs = NULL;
	}

	q->st.sp -= f->actual_slots;
	q->st.fp--;
}

void undo_me(query *q)
{
	q->total_retries++;
	const choice *ch = GET_CURR_CHOICE();

	while (q->st.tp > ch->st.tp) {
		const trail *tr = q->trails + --q->st.tp;
		const frame *f = GET_FRAME(tr->val_ctx);
		slot *e = get_slot(q, f, tr->var_num);
		cell *c = &e->c;
		unshare_cell(c);
		c->tag = TAG_EMPTY;
		c->val_attrs = tr->attrs;
	}
}

void try_me(query *q, unsigned num_vars)
{
	frame *f = GET_NEW_FRAME();
	f->initial_slots = f->actual_slots = num_vars;
	q->total_matches++;

	if (num_vars) {
		slot *e = get_slot(q, f, 0);
		memset(e, 0, sizeof(slot)*num_vars);
	}
}

static void push_frame(query *q)
{
	const frame *fold = GET_CURR_FRAME();
	frame *fnew = GET_NEW_FRAME();
	const cell *next_cell = q->st.instr + q->st.instr->num_cells;

	// Avoid long chains of useless returns...

	if (q->pl->opt && is_end(next_cell) && !next_cell->ret_instr
		&& (fold->prev != (pl_idx)-1)
		) {
		fnew->prev = fold->prev;
		fnew->instr = fold->instr;
	} else {
		fnew->prev = q->st.curr_frame;
		fnew->instr = q->st.instr;
	}

	fnew->op = 0;
	fnew->no_recov = q->no_recov;
	fnew->chgen = ++q->chgen;
	fnew->hp = q->st.hp;
	fnew->heap_num = q->st.heap_num;
	q->st.sp += fnew->actual_slots;
	q->st.curr_frame = q->st.fp++;
}

// Note: TCO's clause may not be the caller clause... hence passing
// num_vars. Currently restricted to the same predicate though.

static void reuse_frame(query *q, unsigned num_vars)
{
	cell *c_next = q->st.instr + q->st.instr->num_cells;

	if (c_next->val_off == g_sys_drop_barrier_s)
		drop_choice(q);

	frame *fold = GET_CURR_FRAME();
	const frame *fnew = GET_NEW_FRAME();
	const slot *from = get_slot(q, fnew, 0);
	slot *to = get_slot(q, fold, 0);

	for (pl_idx i = 0; i < num_vars; i++) {
		unshare_cell(&to->c);
		to++->c = from++->c;
	}

	fold->initial_slots = fold->actual_slots = num_vars;
	fold->no_recov = false;
	q->st.sp = fold->base + fold->actual_slots;
	q->st.dbe->tcos++;
	q->total_tcos++;
	q->st.hp = fold->hp;
	q->st.heap_num = fold->heap_num;
	trim_heap(q);
}

static bool commit_any_choices(const query *q, const frame *f)
{
	if (q->cp == 1)							// Skip in-progress choice
		return false;

	const choice *ch = GET_PREV_CHOICE();	// Skip in-progress choice
	return ch->gen > f->chgen;
}

static void commit_frame(query *q)
{
	q->st.dbe->matched++;
	q->total_matched++;

	clause *cl = &q->st.dbe->cl;
	frame *f = GET_CURR_FRAME();
	f->m = q->st.m;

	rule *save_dbe = q->st.dbe;
	bool is_det = !q->has_vars && cl->is_unique;
	bool last_match = is_det || cl->is_first_cut || !has_next_key(q);
	bool tco = false;

#if 0
	if (last_match) {
		fprintf(stderr, "*** q->no_recov=%d, last_match=%d %s/%u, q->st.curr_frame=%u,q->st.fp=%u\n",
			q->no_recov, last_match,
			C_STR(q, q->st.key), q->st.key->arity,
			q->st.curr_frame, q->st.fp
			);
	}
#endif

	if (!q->no_recov
		&& last_match
		&& (q->st.fp == (q->st.curr_frame + 1))		// At top of frame stack
		) {
		bool tail_call = is_tail_call(q->st.instr);
		bool tail_recursive = tail_call && is_recursive_call(q->st.instr);
		bool slots_ok = f->initial_slots <= cl->num_vars;
		bool choices = commit_any_choices(q, f);
		tco = slots_ok && tail_recursive && !choices;

#if 0
		cell *head = get_head(cl->cells);

		fprintf(stderr,
			"*** %s/%u tco=%d,q->no_recov=%d,last_match=%d,is_det=%d,"
			"tail_call=%d/r%d,slots_ok=%d,choices=%d,"
			"cl->num_vars=%u,f->initial_slots=%u/%u\n",
			C_STR(q, head), head->arity,
			tco, q->no_recov, last_match, is_det,
			tail_call, tail_recursive, slots_ok, choices,
			cl->num_vars, f->initial_slots, f->actual_slots);
#endif
	}

	if (!q->st.dbe->owner->is_builtin)
		q->st.m = q->st.dbe->owner->m;

	if (tco && q->pl->opt) {
		Trace(q, get_head(save_dbe->cl.cells), q->st.curr_frame, EXIT);
		reuse_frame(q, cl->num_vars);
	} else {
		push_frame(q);
	}

	if (last_match) {
		leave_predicate(q, q->st.pr);
		drop_choice(q);
		trim_trail(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.dbe = q->st.dbe;
		ch->gen = q->chgen;
	}

	q->st.instr = cl->alt ? cl->alt : get_body(cl->cells);
	if (!q->st.instr) q->st.instr = cl->cells + (cl->cidx-1);
	q->st.iter = NULL;
}

void stash_frame(query *q, const clause *cl, bool last_match)
{
	pl_idx chgen = ++q->chgen;
	unsigned num_vars = cl->num_vars;

	if (last_match) {
		leave_predicate(q, q->st.pr);
		drop_choice(q);
	} else {
		choice *ch = GET_CURR_CHOICE();
		ch->st.dbe = q->st.dbe;
		ch->gen = chgen;
	}

	if (num_vars) {
		frame *f = GET_FRAME(q->st.fp);
		f->prev = q->st.curr_frame;
		f->instr = NULL;
		f->chgen = chgen;
		f->op = 0;
		q->st.sp += num_vars;
		q->st.fp++;
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
		f->chgen = ch->chgen;
		f->initial_slots = ch->initial_slots;
		f->actual_slots = ch->actual_slots;
		f->no_recov = ch->no_recov;
		f->op = ch->op;
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
			q->st.instr += ch->skip;
			return ch->skip ? -2 : -1;
		}

		return 1;
	}

	trim_heap(q);
	return 0;
}

bool push_choice(query *q)
{
	checked(check_choice(q));
	const frame *f = GET_CURR_FRAME();
	choice *ch = GET_CHOICE(q->cp++);
	ch->skip = 0;
	ch->st = q->st;

	// Keep a record of the frame state, we need to restore
	// it on retry. On cut we commit to it.

	ch->dbgen = f->dbgen;
	ch->chgen = ch->gen = f->chgen;
	ch->initial_slots = f->initial_slots;
	ch->actual_slots = f->actual_slots;
	ch->no_recov = f->no_recov;
	ch->op = f->op;
	ch->base = f->base;

	ch->catchme_retry =
		ch->catchme_exception = ch->barrier = ch->register_cleanup =
		ch->block_catcher = ch->fail_on_retry =
		ch->succeed_on_retry = ch->reset = false;

	return true;
}

bool push_succeed_on_retry(query *q, pl_idx skip)
{
	checked(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->succeed_on_retry = true;
	ch->skip = skip;
	return true;
}

// A barrier is used when making a call, it sets a new
// choice generation so that normal cuts are contained.

bool push_barrier(query *q)
{
	checked(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	frame *f = GET_CURR_FRAME();
	ch->gen = f->chgen = ++q->chgen;
	ch->barrier = true;
	return true;
}

bool push_succeed_on_retry_with_barrier(query *q, pl_idx skip)
{
	frame *f = GET_CURR_FRAME();
	f->no_recov = true;				// FIXME: memory waste
	checked(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->succeed_on_retry = true;
	ch->skip = skip;
	return true;
}

bool push_fail_on_retry_with_barrier(query *q)
{
	checked(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	return true;
}

bool push_reset_handler(query *q)
{
	checked(push_fail_on_retry_with_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->reset = true;
	return true;
}

bool push_catcher(query *q, enum q_retry retry)
{
	checked(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();

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

	const choice *ch = GET_CURR_CHOICE();
	frame *f = GET_CURR_FRAME();
	f->chgen = ch->chgen;
	drop_choice(q);
	return true;
}

void cut(query *q)
{
	const frame *f = GET_CURR_FRAME();

	while (q->cp) {
		const choice *ch = GET_CURR_CHOICE();

		// A normal cut can't break out of a barrier...

		if (ch->barrier) {
			if (ch->gen <= f->chgen)
				break;
		} else {
			if (ch->gen < f->chgen)
				break;
		}

		leave_predicate(q, ch->st.pr);
		drop_choice(q);

		if (ch->register_cleanup && !ch->fail_on_retry) {
			cell *c = FIRST_ARG(ch->st.instr);
			pl_idx c_ctx = ch->st.curr_frame;
			c = deref(q, c, c_ctx);
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
	return ch->gen >= f->chgen;
}

// Resume at next goal in previous clause...

static bool resume_frame(query *q)
{
	const frame *f = GET_CURR_FRAME();

	if (f->prev == (pl_idx)-1)
		return false;

#if 0
	printf("*** q->st.curr_frame=%d, f->no_recov=%d, any_choices=%d\n",
		(unsigned)q->st.curr_frame,
		(unsigned)f->no_recov, (unsigned)resume_any_choices(q, f));
#endif
	Trace(q, get_head(f->instr), q->st.curr_frame, EXIT);

	if (q->pl->opt
		&& !f->no_recov
		&& (q->st.fp == (q->st.curr_frame + 1))
		&& !resume_any_choices(q, f)
		) {
		q->total_recovs++;
		q->st.hp = f->hp;
		q->st.heap_num = f->heap_num;
		trim_heap(q);
		trim_frame(q, f);
	}

	q->st.instr = f->instr;
	q->st.curr_frame = f->prev;
	f = GET_CURR_FRAME();
	q->st.m = f->m;
	return true;
}

// Proceed to next goal in current clause...

static void proceed(query *q)
{
	if (!q->noskip)
		q->st.instr += q->st.instr->num_cells;

	q->noskip = false;

	if (!is_end(q->st.instr))
		return;

	if (q->st.instr->ret_instr) {
		frame *f = GET_CURR_FRAME();
		f->chgen = q->st.instr->chgen;
		q->st.m = q->pl->modmap[q->st.instr->mid];
	}

	q->st.instr = q->st.instr->ret_instr;
}

static bool can_view(query *q, uint64_t dbgen, const rule *r)
{
	if (r->cl.is_deleted)
		return false;

	if (r->dbgen_created > dbgen)
		return false;

	if (r->dbgen_retracted && (r->dbgen_retracted <= dbgen))
		return false;

	return true;
}

static void setup_key(query *q)
{
	cell *save_arg1 = FIRST_ARG(q->st.key), *save_arg2 = NULL;
	cell *arg1 = deref(q, save_arg1, q->st.key_ctx);

	q->st.karg1_is_ground = !is_var(arg1);
	q->st.karg1_is_atomic = is_atomic(arg1);

	if (q->st.key->arity > 1) {
		cell *arg2 = deref(q, save_arg2 = NEXT_ARG(save_arg1), q->st.key_ctx);
		q->st.karg2_is_ground = arg2 && !is_var(arg2);
		q->st.karg2_is_atomic = arg2 && is_atomic(arg2);
	}

	if (q->st.key->arity > 2) {
		cell *arg3 = deref(q, NEXT_ARG(save_arg2), q->st.key_ctx);
		q->st.karg3_is_ground = arg3 && !is_var(arg3);
		q->st.karg3_is_atomic = arg3 && is_atomic(arg3);
	}
}

static void next_key(query *q)
{
	if (!q->st.iter) {
		q->st.dbe = q->st.dbe->next;
		return;
	}

	if (!sl_next(q->st.iter, (void*)&q->st.dbe)) {
		q->st.dbe = NULL;
		sl_done(q->st.iter);
		q->st.iter = NULL;
	}
}

bool has_next_key(query *q)
{
	if (q->st.iter)
		return sl_has_next(q->st.iter, NULL);

	if (!q->st.dbe->next)
		return false;

	if (!q->st.key->arity)
		return true;

	if (q->st.dbe->cl.is_unique) {
		if ((q->st.key->arity == 1) && q->st.karg1_is_atomic)
			return false;

		if ((q->st.key->arity == 2) && q->st.karg1_is_atomic && q->st.karg2_is_atomic)
			return false;

		if ((q->st.key->arity == 3) && q->st.karg1_is_atomic && q->st.karg2_is_atomic && q->st.karg3_is_atomic)
			return false;
	}

	cell *karg1 = FIRST_ARG(q->st.key), *karg2 = NULL, *karg3 = NULL;
	cell *save_arg1 = karg1;

	if (q->st.karg1_is_ground)
		karg1 = deref(q, save_arg1, q->st.key_ctx);

	if (q->st.karg2_is_ground)
		karg2 = deref(q, NEXT_ARG(save_arg1), q->st.key_ctx);

	if (q->st.karg3_is_ground)
		karg3 = deref(q, NEXT_ARG(NEXT_ARG(save_arg1)), q->st.key_ctx);

	//DUMP_TERM("key ", q->st.key, q->st.key_ctx, 1);

	for (rule *next = q->st.dbe->next; next; next = next->next) {
		cell *dkey = next->cl.cells;

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

		if (karg3) {
			if (index_cmpkey(karg3, NEXT_ARG(NEXT_ARG(FIRST_ARG(dkey))), q->st.m, NULL) != 0)
				continue;
		}

		if (index_cmpkey(q->st.key, dkey, q->st.m, NULL) == 0)
			return true;
	}

	return false;
}

static bool expand_meta_predicate(query *q, predicate *pr)
{
	unsigned arity = q->st.key->arity;
	cell *tmp = alloc_heap(q, q->st.key->num_cells*3);	// alloc max possible
	checked(tmp);
	cell *save_tmp = tmp;
	tmp += copy_cells(tmp, q->st.key, 1);

	// Expand module-sensitive args...

	for (cell *k = q->st.key+1, *m = pr->meta_args+1; arity--; k += k->num_cells, m += m->num_cells) {
		cell *k0 = deref(q, k, q->st.key_ctx);

		if ((k0->arity == 2) && (k0->val_off == g_colon_s) && is_atom(FIRST_ARG(k0)))
			;
		else if (!is_interned(k0) || is_iso_list(k0))
			;
		else if (is_interned(m) && (m->val_off == g_colon_s)) {
			make_instr(tmp, g_colon_s, bif_iso_qualify_2, 2, 1+k->num_cells);
			SET_OP(tmp, OP_XFY); tmp++;
			make_atom(tmp++, new_atom(q->pl, q->st.m->name));
		} else if (is_smallint(m) && is_positive(m) && (get_smallint(m) <= 9)) {
			make_instr(tmp, g_colon_s, bif_iso_qualify_2, 2, 1+k->num_cells);
			SET_OP(tmp, OP_XFY); tmp++;
			make_atom(tmp++, new_atom(q->pl, q->st.m->name));
		}

		tmp += dup_cells_by_ref(tmp, k, q->st.key_ctx, k->num_cells);
	}

	save_tmp->num_cells = tmp - save_tmp;
	q->st.key = save_tmp;
	return true;
}

static bool find_key(query *q, predicate *pr, cell *key, pl_idx key_ctx)
{
	q->st.iter = NULL;
	q->st.karg1_is_ground = q->st.karg2_is_ground = q->st.karg3_is_ground = false;
	q->st.karg1_is_atomic = q->st.karg2_is_atomic = q->st.karg3_is_atomic = false;
	q->st.key = key;
	q->st.key_ctx = key_ctx;

	if (!pr->idx1) {
		q->st.dbe = pr->head;

		if (key->arity) {
			if (pr->is_meta_predicate) {
				if (!expand_meta_predicate(q, pr))
					return false;
			}

			setup_key(q);
		}

		return true;
	}

	if (pr->is_meta_predicate) {
		if (!expand_meta_predicate(q, pr))
			return false;

		key = q->st.key;
		key_ctx = q->st.curr_frame;
	} else {
		checked(init_tmp_heap(q));
		key = clone_term_to_tmp(q, key, key_ctx);
		key_ctx = q->st.curr_frame;
	}

	cell *arg1 = key->arity ? FIRST_ARG(key) : NULL;
	skiplist *idx = pr->idx1;

	if (arg1 && (is_var(arg1) || pr->is_var_in_first_arg)) {
		if (!pr->idx2) {
			q->st.dbe = pr->head;
			return true;
		}

		cell *arg2 = NEXT_ARG(arg1);

		if (is_var(arg2)) {
			q->st.dbe = pr->head;
			return true;
		}

		key = arg2;
		idx = pr->idx2;
	}

	q->st.dbe = NULL;
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
		if (!tmp_idx) {
			tmp_idx = sl_create(NULL, NULL, NULL);
			sl_set_tmp(tmp_idx);
		}

		sl_app(tmp_idx, (void*)(size_t)r->db_id, (void*)r);
	}

	sl_done(iter);

	if (!tmp_idx)
		return false;

	iter = sl_first(tmp_idx);

	if (!sl_next(iter, (void*)&q->st.dbe)) {
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

		if (pr && pr->is_abolished)
			pr = search_predicate(q->st.m, c, NULL);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c, NULL);

			if (pr)
				c->match = pr;
		}

		if (!pr) {
			bool found = false;

			if (get_builtin_term(q->st.m, c, &found, NULL), found)
				return throw_error(q, c, c_ctx, "permission_error", "modify,static_procedure");

			q->st.dbe = NULL;
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
	} else {
		next_key(q);
	}

	if (!q->st.dbe) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	checked(check_frame(q, q->st.pr->max_vars));
	checked(push_choice(q));
	const frame *f = GET_CURR_FRAME();
	cell *p1_body = deref(q, get_logical_body(p1), p1_ctx);
	cell *orig_p1 = p1;

	for (; q->st.dbe; q->st.dbe = q->st.dbe->next) {
		if (!can_view(q, f->dbgen, q->st.dbe))
			continue;

		clause *cl = &q->st.dbe->cl;
		cell *c = cl->cells;
		bool needs_true = false;
		p1 = orig_p1;
		const cell *c_body = get_logical_body(c);

		if (p1_body && is_var(p1_body) && !c_body) {
			p1 = deref(q, get_head(p1), p1_ctx);
			c = get_head(c);
			needs_true = true;
		}

		try_me(q, cl->num_vars);

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
			convert_to_literal(q->st.m, c);

		if (pr && pr->is_abolished)
			pr = search_predicate(q->st.m, c, NULL);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c, NULL);

			if (pr)
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

			q->st.dbe = NULL;
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
	} else {
		next_key(q);
	}

	if (!q->st.dbe) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	checked(check_frame(q, q->st.pr->max_vars));
	checked(push_choice(q));
	const frame *f = GET_CURR_FRAME();

	for (; q->st.dbe; q->st.dbe = q->st.dbe->next) {
		if (!can_view(q, f->dbgen, q->st.dbe))
			continue;

		clause *cl = &q->st.dbe->cl;
		cell *head = get_head(cl->cells);
		const cell *body = get_logical_body(cl->cells);

		// Retract(HEAD) should ignore rules (and directives)

		if ((is_retract == DO_RETRACT) && body)
			continue;

		try_me(q, cl->num_vars);

		if (unify(q, p1, p1_ctx, head, q->st.fp))
			return true;

		undo_me(q);
	}

	leave_predicate(q, q->st.pr);
	drop_choice(q);
	return false;
}

bool match_head(query *q)
{
	if (!q->retry) {
		cell *c = q->st.instr;
		pl_idx c_ctx = q->st.curr_frame;
		predicate *pr = NULL;

		if (is_interned(c))
			pr = c->match;
		else if (is_cstring(c)) {
			convert_to_literal(q->st.m, c);
		}

		if (pr && pr->is_abolished)
			pr = search_predicate(q->st.m, c, NULL);

		if (!pr || is_evaluable(c) || is_builtin(c)) {
			pr = search_predicate(q->st.m, c, NULL);

			if (pr) {
				c->match = pr;
				c->flags = 0;
			}
		}

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

		find_key(q, pr, c, c_ctx);
		enter_predicate(q, pr);
	} else
		next_key(q);

	if (!q->st.dbe) {
		leave_predicate(q, q->st.pr);
		return false;
	}

	checked(check_frame(q, q->st.pr->max_vars));
	checked(push_choice(q));
	const frame *f = GET_CURR_FRAME();

	for (; q->st.dbe; next_key(q)) {
		if (!can_view(q, f->dbgen, q->st.dbe))
			continue;

		clause *cl = &q->st.dbe->cl;
		cell *head = get_head(cl->cells);
		try_me(q, cl->num_vars);
		q->st.dbe->attempted++;

		if (unify(q, q->st.key, q->st.key_ctx, head, q->st.fp)) {
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
	cell *tmp = prepare_call(q, CALL_NOSKIP, c, c_ctx, 4);
	ensure(tmp);
	pl_idx num_cells = c->num_cells;
	make_instr(tmp+num_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
	make_instr(tmp+num_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+num_cells++, q->cp);
	make_call(q, tmp+num_cells);
	q->st.instr = tmp;
}

static bool consultall(query *q, cell *l, pl_idx l_ctx)
{
	if (is_string(l)) {
		do_load_file(q, l, l_ctx);
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
			do_load_file(q, h, h_ctx);
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
				case -1: q->retry = true;
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
			switch (retry_choice(q)) {
				case 0: done = true; continue;
				case -1: proceed(q); goto MORE;
				case -2: q->retry = false; break;
			}
		}

		if (!is_callable(q->st.instr)) {
			cell *p1 = deref(q, q->st.instr, q->st.curr_frame);
			pl_idx p1_ctx = q->latest_ctx;

			if (!bif_call_0(q, p1, p1_ctx)) {
				if (is_var(p1))
					break;

				continue;
			}
		}

		Trace(q, q->st.instr, q->st.curr_frame, CALL);
		cell *save_cell = q->st.instr;
		pl_idx save_ctx = q->st.curr_frame;
		q->cycle_error = q->did_throw = false;
		q->total_goals++;

		if (is_builtin(q->st.instr)) {
			q->total_inferences++;
			bool status;

#if USE_FFI
			if (q->st.instr->bif_ptr->ffi) {
				if (q->st.instr->bif_ptr->evaluable)
					status = wrap_ffi_function(q, q->st.instr->bif_ptr);
				else
					status = wrap_ffi_predicate(q, q->st.instr->bif_ptr);
			} else
#endif
				status = q->st.instr->bif_ptr->fn(q);

			if (q->retry == QUERY_NOOP) {
				q->retry = QUERY_OK;
				continue;
			}

			if (!(q->total_goals % YIELD_INTERVAL)) {
				q->s_cnt = 0;

				if (!(q->s_cnt++ % 10000))
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
				Trace(q, q->st.instr, q->st.curr_frame, FAIL);
				q->retry = QUERY_RETRY;

				if (q->yielded)
					break;

				q->total_backtracks++;
				continue;
			}

			if (q->run_hook)
				do_post_unify_hook(q, true);

			Trace(q, save_cell, save_ctx, EXIT);
			proceed(q);
		} else if (!q->run_init && is_iso_list(q->st.instr)) {
			if (!consultall(q, q->st.instr, q->st.curr_frame)) {
				Trace(q, q->st.instr, q->st.curr_frame, FAIL);
				q->retry = QUERY_RETRY;
				q->total_backtracks++;
				continue;
			}

			Trace(q, save_cell, save_ctx, EXIT);
			proceed(q);
		} else {
			q->total_inferences++;

			if (!match_head(q)) {
				Trace(q, q->st.instr, q->st.curr_frame, FAIL);
				q->retry = QUERY_RETRY;
				q->total_backtracks++;
				continue;
			}

			if (q->run_hook)
				do_post_unify_hook(q, false);
		}

		MORE:

		q->retry = QUERY_OK;

		while (!q->st.instr || is_end(q->st.instr)) {
			if (resume_frame(q)) {
				proceed(q);
				continue;
			}

			if (q->top && !q->run_init && any_outstanding_choices(q)) {
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

bool execute(query *q, cell *cells, unsigned num_vars)
{
	q->retry = q->halt = q->error = q->abort = false;
	q->pl->did_dump_vars = false;
	q->st.instr = cells;
	q->st.sp = num_vars;
	q->is_redo = false;

	// There is an initial frame (fp=0), so this
	// to the next available frame...

	q->st.fp = 1;

	// There may not be a choicepoint, so this points to the
	// next available choicepoint

	q->cp = 0;

	frame *f = q->frames;
	f->initial_slots = f->actual_slots = num_vars;
	f->dbgen = ++q->pl->dbgen;
	return start(q);
}

static void query_purge_dirty_list(query *q)
{
	unsigned cnt = 0;
	rule *r;

	while ((r = list_pop_front(&q->dirty)) != NULL) {
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

		for (pl_idx i = 0; i < a->idx; i++, c++)
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

	mp_int_clear(&q->tmp_ival);
	mp_rat_clear(&q->tmp_irat);
	query_purge_dirty_list(q);
	parser_destroy(q->p);
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
	q->p = parser_create(m);
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
	q->max_depth = m->pl->def_max_depth;
	q->vgen = 1;
	q->dump_var_num = -1;
	q->dump_var_ctx = -1;

	mp_int_init(&q->tmp_ival);
	mp_rat_init(&q->tmp_irat);

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

query *query_create_subquery(query *q, cell *instr)
{
	query *subq = query_create(q->st.m);
	if (!subq) return NULL;
	subq->parent = q;
	subq->st.fp = 1;
	subq->top = q->top;

	cell *tmp = prepare_call(subq, false, instr, q->st.curr_frame, 1);
	pl_idx num_cells = tmp->num_cells;
	make_end(tmp+num_cells);
	subq->st.instr = tmp;

	frame *fsrc = GET_FRAME(q->st.curr_frame);
	frame *fdst = subq->frames;
	fdst->initial_slots = fdst->actual_slots = fsrc->actual_slots;
	fdst->dbgen = ++q->pl->dbgen;
	subq->st.sp = fdst->actual_slots;
	return subq;
}

query *query_create_task(query *q, cell *instr)
{
	query *task = query_create_subquery(q, instr);
	if (!task) return NULL;
	task->is_task = true;
	return task;
}
