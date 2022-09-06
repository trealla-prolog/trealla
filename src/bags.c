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

static void init_queuen(query *q)
{
	free(q->queue[q->st.qnbr]);
	q->queue[q->st.qnbr] = NULL;
	q->qp[q->st.qnbr] = 0;
}

static void grab_queuen(query *q)
{
	q->st.qnbr++;
	init_queuen(q);
}

static void drop_queuen(query *q)
{
	init_queuen(q);
	q->st.qnbr--;
}

static bool is_ground(cell *c)
{
	pl_idx_t nbr_cells = c->nbr_cells;

	for (pl_idx_t i = 0; i < nbr_cells; i++, c++) {
		if (is_variable(c))
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

	bool is_partial = false;

	if (is_iso_list(xp3) && !check_list(q, xp3, xp3_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, xp3, xp3_ctx, "type_error", "list");

	if (!q->retry) {
		cell *p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, false);
		check_heap_error(p0);
		GET_FIRST_ARG0(p1,any,p0);
		GET_NEXT_ARG(p2,callable);
		GET_NEXT_ARG(p3,list_or_nil_or_var);

		//printf("*** phase1\n");

		grab_queuen(q);
		assert(q->st.qnbr < MAX_QUEUES);
		cell *tmp = clone_to_heap(q, true, p2, 2+p1->nbr_cells+2);
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
		if (q->end_findall) {
			q->end_findall = false;
			return false;
		}

		drop_queuen(q);
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, xp3, xp3_ctx, &tmp, q->st.curr_frame);
	}

	// Retry takes a copy

	//printf("*** phase2\n");

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
	ch->end_findall = true;
	check_heap_error(try_me(q, MAX_ARITY), free(solns));

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
	frame *f = GET_CURR_FRAME();

	if (!ok || !f->overflow || is_ground(l)) {
		q->end_findall = false;
		drop_choice(q);
	}

	return ok;
}
