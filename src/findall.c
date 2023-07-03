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

bool fn_iso_findall_3(query *q)
{
	GET_FIRST_ARG(xp1,any);
	GET_NEXT_ARG(xp2,callable);
	GET_NEXT_ARG(xp3,list_or_nil_or_var);

	if (!q->retry) {
		bool is_partial = false;

		// This checks for a valid list (it allows for partial but acyclic lists)...

		if (is_iso_list(xp3) && !check_list(q, xp3, xp3_ctx, &is_partial, NULL) && !is_partial)
			return throw_error(q, xp3, xp3_ctx, "type_error", "list");

		cell *p0;

		if (is_compound(xp1) && !is_iso_list(xp1)) {
			p0 = deep_copy_to_heap(q, q->st.curr_cell, q->st.curr_frame, false);
			check_heap_error(p0);
			unify(q, q->st.curr_cell, q->st.curr_frame, p0, q->st.curr_frame);
		} else {
			p0 = deep_clone_to_heap(q, q->st.curr_cell, q->st.curr_frame);
			check_heap_error(p0);
		}

		GET_FIRST_ARG0(p1,any,p0);
		GET_NEXT_ARG(p2,any);

		grab_queuen(q);

		if (q->st.qnbr == MAX_QUEUES)
			return throw_error(q, xp2, xp2_ctx, "resource_error", "max_queues");

		cell *tmp = clone_to_heap(q, true, p2, 1+p1->nbr_cells+2);
		check_heap_error(tmp);
		pl_idx_t nbr_cells = 1 + p2->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_queue_s, fn_sys_queue_1, 1, p1->nbr_cells);
		nbr_cells += copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
		make_struct(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_barrier(q));
		q->st.curr_cell = tmp;
		return true;
	}

	if (!queuen_used(q)) {
		drop_queuen(q);
		return unify(q, xp3, xp3_ctx, make_nil(), q->st.curr_frame);
	}

	// Retry takes the queue

	pl_idx_t nbr_cells = queuen_used(q);
	cell *solns = take_queuen(q);
	drop_queuen(q);

	// Now grab matching solutions with fresh variables for each...

	const frame *f = GET_CURR_FRAME();
	try_me(q, f->actual_slots);
	check_heap_error(init_tmp_heap(q), free(solns));

	for (cell *c = solns; nbr_cells; nbr_cells -= c->nbr_cells, c += c->nbr_cells) {
		cell *tmp = alloc_on_tmp(q, 1);
		check_heap_error(tmp, free(solns));
		make_struct(tmp, g_dot_s, NULL, 2, 0);
		tmp = deep_copy_to_tmp(q, c, q->st.curr_frame, false);
		check_heap_error(tmp, free(solns));
	}

	free(solns);
	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, xp3, xp3_ctx, l, q->st.curr_frame);
}
