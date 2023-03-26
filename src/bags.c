#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heap.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

static bool is_ground(cell *c)
{
	pl_idx_t nbr_cells = c->nbr_cells;

	for (pl_idx_t i = 0; i < nbr_cells; i++, c++) {
		if (is_var(c))
			return false;
	}

	return true;
}

bool fn_iso_findall_3(query *q)
{
	GET_FIRST_ARG(xp1,any);
	GET_NEXT_ARG(xp2,callable);
	GET_NEXT_ARG(xp3,list_or_nil_or_var);

	// This checks for a valid list (it allows for partial but acyclic lists)...

	if (!q->retry) {
		bool is_partial = false;

		if (is_iso_list(xp3) && !check_list(q, xp3, xp3_ctx, &is_partial, NULL) && !is_partial)
			return throw_error(q, xp3, xp3_ctx, "type_error", "list");

		cell *p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, true);
		check_heap_error(p0);
		unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);
		GET_FIRST_ARG0(p1,any,p0);
		GET_NEXT_ARG(p2,any);
		GET_NEXT_ARG(p3,any);

		grab_queuen(q);
		assert(q->st.qnbr < MAX_QUEUES);
		cell *tmp = clone_to_heap(q, true, p2, 2+p1->nbr_cells+2);
		check_heap_error(tmp);
		pl_idx_t nbr_cells = 1 + p2->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p1->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += safe_copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
		make_struct(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_barrier(q));
		q->st.curr_cell = tmp;
		return true;
	}

	if (!queuen_used(q)) {
		if (q->st.end_findall) {
			q->st.end_findall = false;
			return false;
		}

		drop_queuen(q);
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, xp3, xp3_ctx, &tmp, q->st.curr_frame);
	}

	// Retry takes a copy

	pl_idx_t nbr_cells = queuen_used(q);
	cell *solns = take_queuen(q);
	init_queuen(q);

	// Now grab matching solutions

	if (!check_slot(q, MAX_VARS)) {
		free(solns);
		return false;
	}

	check_heap_error(push_choice(q), free(solns));
	choice *ch = GET_CURR_CHOICE();
	ch->st.end_findall = true;
	try_me(q, MAX_ARITY);

	for (cell *c = solns; nbr_cells;
		nbr_cells -= c->nbr_cells, c += c->nbr_cells) {
		check_heap_error(init_tmp_heap(q), free(solns));
		cell *tmp = deep_copy_to_tmp(q, c, q->st.fp, false);
		check_heap_error(tmp, free(solns));
		check_heap_error(alloc_on_queuen(q, q->st.qnbr, tmp), free(solns));
	}

	// Return matching solutions

	free(solns);
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));
	drop_queuen(q);
	check_heap_error(l);
	bool ok = unify(q, xp3, xp3_ctx, l, q->st.curr_frame);
	const frame *f = GET_CURR_FRAME();

	if (!ok || !f->overflow || is_ground(l)) {
		q->st.end_findall = false;
		drop_choice(q);
	}

	return ok;
}

static int collect_local_vars(cell *p1, pl_idx_t nbr_cells, cell **slots)
{
	int cnt = 0;

	for (unsigned i = 0; i < nbr_cells; i++, p1++) {
		if (is_var(p1)) {
			assert(p1->var_nbr < MAX_ARITY);

			if (!slots[p1->var_nbr]) {
				slots[p1->var_nbr] = p1;
				cnt++;
			}
		}
	}

	return cnt;
}

static uint64_t get_vars(cell *p)
{
	cell *slots[MAX_ARITY] = {0};
	int cnt = collect_local_vars(p, p->nbr_cells, slots);
	uint64_t mask = 0;

	if (!cnt)
		return 0;

	for (unsigned i = 0; i < MAX_ARITY; i++) {
		if (slots[i])
			mask |= 1ULL << i;
	}

	return mask;
}

static cell *redo_existentials(query *q, cell *p2, uint64_t *xs)
{
	while (is_structure(p2) && !slicecmp2(C_STR(q, p2), C_STRLEN(q, p2), "^") && !g_tpl_interrupt) {
		cell *c = ++p2;

		if (!is_var(c)) {
			for (pl_idx_t i = 0; i < c->nbr_cells; i++) {
				if (is_var(c+i)) {
					assert((c+i)->var_nbr < 64);
					*xs |= 1ULL << (c+i)->var_nbr;
				}
			}
		}

		assert(c->var_nbr < 64);

		if (is_var(c))
			*xs |= 1ULL << c->var_nbr;

		p2 += c->nbr_cells;
	}

	return p2;
}

static void pin_vars(query *q, uint64_t mask)
{
	choice *ch = GET_CURR_CHOICE();
	ch->pins = mask;
}

static void unpin_vars(query *q)
{
	choice *ch = GET_CURR_CHOICE();
	frame *f = GET_CURR_FRAME();
	uint64_t mask = 1;

	for (unsigned i = 0; i < f->actual_slots; i++, mask <<= 1) {
		if (!(ch->pins & mask))
			continue;

		slot *e = GET_SLOT(f, i);
		e->c.tag = TAG_EMPTY;
		e->c.attrs = NULL;
	}

	ch->pins = 0;
}

bool fn_iso_bagof_3(query *q)
{
	GET_FIRST_ARG(xp1,any);
	GET_NEXT_ARG(xp2,callable);
	GET_NEXT_ARG(xp3,list_or_nil_or_var);

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(xp3) && !check_list(q, xp3, xp3_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, xp3, xp3_ctx, "type_error", "list");

	cell *p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, false);
	check_heap_error(p0);
	GET_FIRST_ARG0(p1,any,p0);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,list_or_nil_or_var);

	uint64_t xs_vars = 0;
	p2 = redo_existentials(q, p2, &xs_vars);
	cell *tvars_tmp = do_term_variables(q, p2, p2_ctx);
	check_heap_error(tvars_tmp);
	cell *tvars = malloc(sizeof(cell)*tvars_tmp->nbr_cells);
	check_heap_error(tvars);
	copy_cells(tvars, tvars_tmp, tvars_tmp->nbr_cells);

	if (!is_callable(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "callable");

	// First time thru generate all solutions

	if (!q->retry) {
		q->st.qnbr++;
		assert(q->st.qnbr < MAX_QUEUES);
		cell *tmp = clone_to_heap(q, true, p2, 2+tvars->nbr_cells+1);
		check_heap_error(tmp);
		pl_idx_t nbr_cells = 1 + p2->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+tvars->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		copy_cells(tmp+nbr_cells, tvars, tvars->nbr_cells);
		nbr_cells += tvars->nbr_cells;
		make_struct(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);

		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		check_heap_error(push_barrier(q));
		q->st.curr_cell = tmp;
		free(tvars);
		return true;
	}

	if (!queuen_used(q) && !q->tmpq[q->st.qnbr]) {
		free(tvars);
		return false;
	}

	// First retry takes a copy

	if (!q->tmpq[q->st.qnbr]) {
		pl_idx_t nbr_cells = queuen_used(q);
		q->tmpq[q->st.qnbr] = malloc(sizeof(cell)*nbr_cells);
		check_heap_error(q->tmpq[q->st.qnbr]);
		copy_cells(q->tmpq[q->st.qnbr], get_queuen(q), nbr_cells);
		q->tmpq_size[q->st.qnbr] = nbr_cells;
	}

	// Now grab matching solutions

	init_queuen(q);
	check_heap_error(push_choice(q));
	uint64_t p1_vars = get_vars(p1);
	uint64_t p2_vars = get_vars(p2);
	uint64_t mask = p1_vars ^ p2_vars ^ xs_vars;
	pin_vars(q, mask);
	pl_idx_t nbr_cells = q->tmpq_size[q->st.qnbr];
	bool unmatched = false;
	frame *f = GET_CURR_FRAME();

	for (cell *c = q->tmpq[q->st.qnbr]; nbr_cells;
		nbr_cells -= c->nbr_cells, c += c->nbr_cells) {

#if 0
		fprintf(stdout, "*** ");
		print_term(q, stdout, c, p2_ctx, 1);
		fprintf(stdout, "\n");
#endif

		if (c->flags & FLAG_PROCESSED)
			continue;

		try_me(q, f->actual_slots*2);

		// FIXME: if no variables copied & any>0 redo
		// FIXME: if no variables copied & any=0 break after queueing it

		if (unify(q, tvars, p2_ctx, c, q->st.fp)) {
			check_heap_error(init_tmp_heap(q));
			cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, true);
			check_heap_error(tmp);
			check_heap_error(alloc_on_queuen(q, q->st.qnbr, tmp));
			c->flags |= FLAG_PROCESSED;
		} else
			unmatched = true;

		undo_me(q);
	}

	// No solution?

	if (!queuen_used(q)) {
		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		drop_choice(q);
		free(tvars);
		return false;
	}

	// Return matching solutions

	cell *tmp = deep_clone_to_heap(q, tvars, p2_ctx);
	check_heap_error(tmp);
	unpin_vars(q);
	unify(q, tvars, p2_ctx, tmp, q->st.curr_frame);
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));

	if (!unmatched) {
		init_queuen(q);
		free(q->tmpq[q->st.qnbr]);
		q->tmpq[q->st.qnbr] = NULL;
		drop_choice(q);
		q->st.qnbr--;
	}

	free(tvars);

	bool ok = unify(q, p3, p3_ctx, l, q->st.curr_frame);

	if (ok == true)
		unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);

	return ok;
}

