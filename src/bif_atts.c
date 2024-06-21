#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heap.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

#include "bif_atts.h"

static const char *do_attribute(query *q, cell *attr, unsigned arity, bool *found)
{
	for (module *m = list_front(&q->pl->modules);
		m; m = list_next(m)) {
		if ((arity == m->arity) && !CMP_STRING_TO_CSTR(q, attr, m->name)) {
			*found = true;
			return m->orig->name;
		}
	}

	*found = false;
	return q->st.m->name;
}

bool bif_attribute_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,integer);
	bool found;
	const char *m_name = do_attribute(q, p2, get_smalluint(p3), &found);
	if (!found) return false;
	cell tmp;
	make_atom(&tmp, new_atom(q->pl, m_name));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool do_put_atts(query *q, cell *attr, pl_idx attr_ctx, bool is_minus)
{
	GET_FIRST_ARG(p1,var);
	const frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);
	cell *c = deref(q, &e->c, e->c.var_ctx);
	frame *fcurr = GET_CURR_FRAME();
	fcurr->no_tco = true;

	if (!c->attrs && is_minus)
		return true;

	if (((attr->val_off == g_minus_s) || (attr->val_off == g_plus_s)) && (attr->arity == 1))
		attr++;

	if (is_nil(attr)) {
		if (e->c.attrs)
			add_trail(q, p1_ctx, p1->var_nbr, c->attrs);

		e->c.attrs = NULL;
		return true;
	}

	add_trail(q, p1_ctx, p1->var_nbr, c->attrs);

	unsigned a_arity = attr->arity;
	bool found;
	const char *m_name = do_attribute(q, attr, a_arity, &found);
	if (!found) return false;
	check_heap_error(init_tmp_heap(q));

	// Add this attribute value...

	if (!is_minus) {
		cell *tmp = alloc_on_tmp(q, 1+1);
		check_heap_error(tmp);
		make_atom(tmp, g_dot_s);
		tmp->arity = 2;
		tmp->nbr_cells += 1;
		make_atom(tmp+1, new_atom(q->pl, m_name));
		tmp[1].arity = 1;
		cell *tmp2 = deep_clone_to_tmp(q, attr, attr_ctx);
		check_heap_error(tmp2);
		cell *tmp3 = get_tmp_heap(q, 1);
		tmp3->nbr_cells += tmp2->nbr_cells;
		tmp = get_tmp_heap(q, 0);
		tmp->nbr_cells += tmp2->nbr_cells;
	}

	// If existing attributes drop old value...

	if (c->attrs) {
		cell *l = c->attrs;
		pl_idx l_ctx = q->st.curr_frame;
		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			pl_idx h_ctx = q->latest_ctx;
			cell *h1 = deref(q, h+1, h_ctx);
			pl_idx h1_ctx = q->latest_ctx;

			if (CMP_STRING_TO_CSTR(q, h, m_name)
				|| CMP_STRING_TO_STRING(q, h1, attr)
				|| (h1->arity != a_arity)) {
				append_list(q, h);
			} else if (is_minus) {
				if (!unify(q, attr, attr_ctx, h1, h1_ctx))
					return false;
			}

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}
	}

	cell *l = end_list(q);
	check_heap_error(l);

	if (is_nil(l)) {
		e->c.flags = 0;
		e->c.attrs = NULL;
		return true;
	}

	e->c.attrs = l;
	return true;
}

bool bif_put_atts_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,callable);
	bool is_minus = (p2->val_off == g_minus_s) && (p2->arity == 1);

	if (is_iso_list(p2)) {
		LIST_HANDLER(p2);

		while (is_iso_list(p2)) {
			cell *attr = LIST_HEAD(p2);
			attr = deref(q, attr, p2_ctx);
			pl_idx attr_ctx = q->latest_ctx;

			if (!do_put_atts(q, attr, attr_ctx, is_minus))
				return false;

			p2 = LIST_TAIL(p2);
			p2 = deref(q, p2, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		return true;
	} else
		return do_put_atts(q, p2, p2_ctx, is_minus);
}

bool bif_get_atts_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,callable_or_var);
	const frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);
	cell *c = deref(q, &e->c, e->c.var_ctx);
	bool is_minus = !is_var(p2) && (p2->val_off == g_minus_s) && (p2->arity == 1);

	if (!c->attrs || is_nil(c->attrs))
		return is_minus ? true : false;

	if (is_var(p2)) {
		cell *l = c->attrs;
		pl_idx l_ctx = q->st.curr_frame;
		init_tmp_heap(q);
		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			cell *h1 = deref(q, h+1, q->latest_ctx);
			pl_idx h1_ctx = q->latest_ctx;

			if (!is_nil(h1))
				append_list(q, h1);

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}

		l = end_list(q);
		check_heap_error(l);

		if (is_nil(l))
			return false;

		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	cell *attr = p2;

	if (((p2->val_off == g_minus_s) || (p2->val_off == g_plus_s)) && (p2->arity == 1))
		attr++;

	unsigned a_arity = attr->arity;
	bool found;
	const char *m_name = do_attribute(q, attr, a_arity, &found);
	if (!found) return false;
	cell *l = e->c.attrs;
	pl_idx l_ctx = q->st.curr_frame;
	LIST_HANDLER(l);

	while (is_iso_list(l)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		pl_idx h_ctx = q->latest_ctx;
		cell *h1 = deref(q, h+1, h_ctx);
		pl_idx h1_ctx = q->latest_ctx;

		if (!CMP_STRING_TO_CSTR(q, h, m_name)
			&& !CMP_STRING_TO_STRING(q, h1, attr)
			&& (h1->arity == a_arity)) {
			if (is_minus)
				return false;

			return unify(q, attr, p2_ctx, h1, h1_ctx);
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	return is_minus ? true : false;
}

static bool check_occurs(unsigned var_nbr, pl_idx var_ctx, cell *c, pl_idx c_ctx)
{
	bool any = false;

	for (unsigned nbr_cells = c->nbr_cells; nbr_cells--; c++) {
		if (!is_var(c))
			continue;

		pl_idx ctx = c_ctx;

		if (is_ref(c))
			ctx = c->var_ctx;

		if (var_nbr != c->var_nbr)
			continue;

		if (var_ctx != ctx)
			continue;

		any = true;
	}

	return !any;
}

bool any_attributed(query *q)
{
	const frame *f = GET_FRAME(0);

	for (unsigned i = 0; i < f->initial_slots; i++) {
		slot *e = GET_SLOT(f, i);
		cell *c = deref(q, &e->c, e->c.var_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (!is_empty(c) || !c->attrs || is_nil(c->attrs))
			continue;

		//DUMP_TERM("atts", c->attrs, q->st.curr_frame, 1);

		cell *v = c->attrs;
		bool any = false;

		while (is_iso_list(v)) {
			cell *h = v + 1;

			//  Ignore \== created by dif/2, but what are they?

			if (!is_op(h))
				any = true;

			v = v + 1; v += v->nbr_cells;
		}

		if (!any)
			continue;

		return true;
	}

	return false;
}

bool bif_sys_list_attributed_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	check_heap_error(init_tmp_heap(q));
	const frame *f = GET_FRAME(0);

	for (unsigned i = 0; i < f->initial_slots; i++) {
		slot *e = GET_SLOT(f, i);
		cell *c = deref(q, &e->c, e->c.var_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (!is_empty(c) || !c->attrs || is_nil(c->attrs))
			continue;

		cell tmp;
		make_ref(&tmp, i, 0);
		append_list(q, &tmp);
	}

	for (unsigned i = 0; i < f->initial_slots; i++) {
		slot *e = GET_SLOT(f, i);
		cell *c = deref(q, &e->c, e->c.var_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (!is_empty(c) || !c->attrs || is_nil(c->attrs))
			continue;

		if (!is_compound(c->attrs))
			continue;

		//DUMP_TERM("here", c->attrs, q->st.curr_frame, 0);

		collect_vars(q, c->attrs, q->st.curr_frame);

		for (unsigned i = 0; i < q->tab_idx; i++) {
			const frame *f = GET_FRAME(q->pl->tabs[i].ctx);
			slot *e = GET_SLOT(f, q->pl->tabs[i].var_nbr);
			cell *v = deref(q, &e->c, e->c.var_ctx);

			if (!is_empty(v) || !v->attrs || is_nil(v->attrs))
				continue;

			if (!q->pl->tabs[i].ctx)
				continue;

			cell tmp;
			make_ref(&tmp, q->pl->tabs[i].var_nbr, q->pl->tabs[i].ctx);
			append_list(q, &tmp);
		}
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p1, p1_ctx, l, 0);
}

bool bif_sys_attributed_var_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	const frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);
	cell *c = deref(q, &e->c, e->c.var_ctx);

	if (!c->attrs || is_nil(c->attrs))
		return false;

	cell *l = c->attrs;
	pl_idx l_ctx = q->st.curr_frame;
	init_tmp_heap(q);
	LIST_HANDLER(l);

	while (is_iso_list(l)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		cell *h1 = deref(q, h+1, l_ctx);
		pl_idx h1_ctx = q->latest_ctx;

		if (!is_nil(h1))
			append_list(q, h1);

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	l = end_list(q);
	check_heap_error(l);

	if (is_nil(l))
		return false;

	return true;
}

bool bif_sys_unattributed_var_1(query *q)
{
	return !bif_sys_attributed_var_1(q);
}

typedef struct {
	blob b;
	pl_idx lo_tp, hi_tp;
	slot e[];
} bind_state;

static void set_occurs(unsigned var_nbr, pl_idx var_ctx, cell *c, pl_idx c_ctx)
{
	for (unsigned nbr_cells = c->nbr_cells; nbr_cells--; c++) {
		if (!is_var(c))
			continue;

		pl_idx ctx = c_ctx;

		if (is_ref(c))
			ctx = c->var_ctx;

		if (var_nbr != c->var_nbr)
			continue;

		if (var_ctx != ctx)
			continue;

		c->flags |= FLAG_VAR_CYCLIC;
	}
}

bool bif_sys_undo_trail_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	q->run_hook = false;

	if (q->undo_hi_tp <= q->undo_lo_tp)
		return unify(q, p1, p1_ctx, make_nil(), q->st.curr_frame);

	pl_idx slots = q->undo_hi_tp - q->undo_lo_tp;
	bind_state *save = malloc(sizeof(bind_state)+(sizeof(slot)*slots));
	check_error(save);
	save->b.ptr = save->b.ptr2 = NULL;
	save->lo_tp = q->undo_lo_tp;
	save->hi_tp = q->undo_hi_tp;
	init_tmp_heap(q);

	// Unbind & save our vars

	for (pl_idx i = q->undo_lo_tp, j = 0; i < q->undo_hi_tp; i++, j++) {
		const trail *tr = q->trails + i;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		save->e[j] = *e;
		cell *c = &e->c;
		pl_idx c_ctx = e->c.var_ctx;
		set_occurs(tr->var_nbr, tr->var_ctx, c, c_ctx);
		//printf("*** unbind [%u:%u] hi_tp=%u, tag=%u, tr->var_ctx=%u, tr->var_nbr=%u\n", j, i, q->undo_hi_tp, e->c.tag, tr->var_ctx, tr->var_nbr);
		cell lhs, rhs;
		make_ref(&lhs, tr->var_nbr, tr->var_ctx);

		if (is_compound(c))
			make_indirect(&rhs, c, c_ctx);
		else
			rhs = *c;

		//DUMP_TERM("$undo1 rhs", &e->c, e->c.var_ctx, 0);

		cell tmp[3];
		make_struct(tmp, g_minus_s, NULL, 2, 2);
		SET_OP(&tmp[0], OP_YFX);
		tmp[1] = lhs;
		tmp[2] = rhs;
		append_list(q, tmp);
		init_cell(&e->c);
		e->c.attrs = tr->attrs;
		//if (tr->attrs) DUMP_TERM("$undo2 trail", tr->attrs, q->st.curr_frame, 0);
	}

	cell *tmp = end_list(q);
	check_heap_error(tmp, free(save));
	//DUMP_TERM("undolist tmp", tmp, q->st.curr_frame, 0);

	if (!unify(q, p1, p1_ctx, tmp, q->st.curr_frame))
		return false;

	cell tmp2;
	make_blob(&tmp2, &save->b);
	return unify(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
}

bool bif_sys_redo_trail_1(query * q)
{
	GET_FIRST_ARG(p1,any);

	if (!is_blob(p1))
		return false;

	const bind_state *save = (bind_state*)p1->val_blob;

	for (pl_idx i = save->lo_tp, j = 0; i < save->hi_tp; i++, j++) {
		//if (is_empty(&save->e[j].c))
		//	continue;
		const trail *tr = q->trails + i;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		*e = save->e[j];
		//printf("*** rebind [%u:%u] hi_tp=%u, tag=%u, ctx=%u, var=%u\n", j, i, q->undo_hi_tp, e->c.tag, tr->var_ctx, tr->var_nbr);
		//DUMP_TERM("$redo1 rhs", &e->c, e->c.var_ctx, 0);
	}

	return true;
}

bool do_post_unify_hook(query *q, bool is_builtin)
{
	q->run_hook = false;
	q->undo_lo_tp = q->before_hook_tp;
	q->undo_hi_tp = q->st.tp;
	q->before_hook_tp = 0;
	cell *tmp = alloc_on_heap(q, 3);
	check_heap_error(tmp);
	make_struct(tmp+0, g_true_s, bif_iso_true_0, 0, 0);
	make_struct(tmp+1, g_post_unify_hook_s, NULL, 0, 0);

	if (is_builtin)
		make_call(q, tmp+2);
	else
		make_call_redo(q, tmp+2);

	q->st.curr_instr = tmp;
	return true;
}

builtins g_atts_bifs[] =
{
	{"attribute", 3, bif_attribute_3, "?atom,+atom,+integer", false, false, BLAH},
	{"put_atts", 2, bif_put_atts_2, "@variable,+term", false, false, BLAH},
	{"get_atts", 2, bif_get_atts_2, "@variable,-term", false, false, BLAH},

	{"$list_attributed", 1, bif_sys_list_attributed_1, "-list", false, false, BLAH},
	{"$unattributed_var", 1, bif_sys_unattributed_var_1, "@variable", false, false, BLAH},
	{"$attributed_var", 1, bif_sys_attributed_var_1, "@variable", false, false, BLAH},

	{0}
};
