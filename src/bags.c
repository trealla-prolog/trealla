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

#if 1
bool fn_iso_findall_3(query *q)
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

	if (!q->retry) {
		grab_queuen(q);
		assert(q->st.qnbr < MAX_QUEUES);
		cell *tmp = clone_to_heap(q, true, p2, 2+p1->nbr_cells+2);
		pl_idx_t nbr_cells = 1 + p2->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p1->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += safe_copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
		make_struct(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
		make_return(q, tmp+nbr_cells);
		check_heap_error(push_barrier(q));
		q->st.curr_cell = tmp;
		return true;
	}

	if (!queuen_used(q)) {
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

	check_heap_error(push_choice(q), free(solns));
	frame *f = GET_CURR_FRAME();

	for (cell *c = solns; nbr_cells;
		nbr_cells -= c->nbr_cells, c += c->nbr_cells) {

		// Note: we need fresh variables for each solution

		unsigned vars = f->actual_slots < 128 ? 128 : f->actual_slots;

		if (!check_slot(q, vars)) {
			free(solns);
			return false;
		}

		check_heap_error(try_me(q, vars));

		if (unify(q, p1, p1_ctx, c, q->st.fp)) {
			check_heap_error(init_tmp_heap(q), free(solns));
			cell *tmp;

			if (!is_atomic(c))
				tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
			else
				tmp = deep_clone_to_tmp(q, p1, p1_ctx);

			check_heap_error(tmp, free(solns));
			check_heap_error(alloc_on_queuen_unsafe(q, q->st.qnbr, tmp), free(solns));
		}

		undo_me(q);
	}

	// Return matching solutions

	drop_choice(q);
	free(solns);
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));
	drop_queuen(q);
	return unify(q, xp3, xp3_ctx, l, q->st.curr_frame);
}
#else
// This is sleeker fast version that *should* work, but has a
// few Logtalk fails when meta-called...
bool fn_iso_findall_3(query *q)
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

	if (!q->retry) {
		grab_queuen(q);
		assert(q->st.qnbr < MAX_QUEUES);
		cell *tmp = clone_to_heap(q, true, p2, 2+p1->nbr_cells+2);
		pl_idx_t nbr_cells = 1 + p2->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queuen_2, 2, 1+p1->nbr_cells);
		make_int(tmp+nbr_cells++, q->st.qnbr);
		nbr_cells += safe_copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
		make_struct(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
		make_return(q, tmp+nbr_cells);
		check_heap_error(push_barrier(q));
		q->st.curr_cell = tmp;
		return true;
	}

	if (!queuen_used(q)) {
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

	for (cell *c = solns; nbr_cells;
		nbr_cells -= c->nbr_cells, c += c->nbr_cells) {
		check_heap_error(init_tmp_heap(q), free(solns));
		cell *tmp;

		if (!is_atomic(c))
			tmp = deep_copy_to_tmp(q, c, q->st.curr_frame, false);
		else
			tmp = deep_clone_to_tmp(q, c, q->st.curr_frame);

		check_heap_error(tmp, free(solns));
		check_heap_error(alloc_on_queuen(q, q->st.qnbr, tmp), free(solns));
	}

	// Return matching solutions

	free(solns);
	cell *l = convert_to_list(q, get_queuen(q), queuen_used(q));
	drop_queuen(q);
	return unify(q, xp3, xp3_ctx, l, q->st.curr_frame);
}
#endif
