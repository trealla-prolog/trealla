#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

static const char *find_attribute(query *q, cell *attr, unsigned arity, bool *found)
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

static bool bif_attribute_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,integer);
	bool found;
	const char *m_name = find_attribute(q, p2, get_smalluint(p3), &found);
	if (!found) return false;
	cell tmp;
	make_atom(&tmp, new_atom(q->pl, m_name));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool do_put_atts(query *q, cell *attr, pl_idx attr_ctx, bool is_minus)
{
	GET_FIRST_ARG(p1,var);
	const frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_num);
	cell *c = deref(q, &e->c, e->c.var_ctx);
	pl_idx c_ctx = q->latest_ctx;

	if (!c->val_attrs && is_minus)
		return true;

	if (((attr->val_off == g_minus_s) || (attr->val_off == g_plus_s)) && (attr->arity == 1))
		attr++;

	add_trail(q, p1_ctx, p1->var_num, c->val_attrs);

	unsigned a_arity = attr->arity;
	bool found;
	const char *m_name = find_attribute(q, attr, a_arity, &found);
	if (!found) return false;
	check_memory(init_tmp_heap(q));

	// Add this attribute value...

	if (!is_minus) {
		cell *tmp = alloc_on_tmp(q, 1+1);
		check_memory(tmp);
		make_atom(tmp, g_dot_s);
		tmp->arity = 2;
		tmp->num_cells += 1;
		make_atom(tmp+1, new_atom(q->pl, m_name));
		tmp[1].arity = 1;
		cell *tmp2 = clone_term_to_tmp(q, attr, attr_ctx);
		check_memory(tmp2);
		cell *tmp3 = get_tmp_heap(q, 1);
		tmp3->num_cells += tmp2->num_cells;
		tmp = get_tmp_heap(q, 0);
		tmp->num_cells += tmp2->num_cells;
	}

	// If existing attributes drop old value...

	if (c->val_attrs) {
		cell *l = c->val_attrs;
		pl_idx l_ctx = c_ctx;
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
	check_memory(l);

	if (is_nil(l)) {
		e->c.flags = 0;
		e->c.val_attrs = NULL;
		return true;
	}

	e->c.val_attrs = l;
	return true;
}

static bool bif_put_atts_2(query *q)
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
	}

	return do_put_atts(q, p2, p2_ctx, is_minus);
}

static bool bif_get_atts_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,callable_or_var);
	const frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_num);
	cell *c = deref(q, &e->c, e->c.var_ctx);
	pl_idx c_ctx = q->latest_ctx;
	bool is_minus = !is_var(p2) && (p2->val_off == g_minus_s) && (p2->arity == 1);

	if (!c->val_attrs)
		return is_minus ? true : false;

	if (is_var(p2)) {
		cell *l = c->val_attrs;
		pl_idx l_ctx = c_ctx;
		init_tmp_heap(q);
		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			cell *h1 = deref(q, h+1, q->latest_ctx);

			if (!is_nil(h1))
				append_list(q, h1);

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}

		l = end_list(q);
		check_memory(l);

		if (is_nil(l))
			return false;

		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	cell *attr = p2;

	if (((p2->val_off == g_minus_s) || (p2->val_off == g_plus_s)) && (p2->arity == 1))
		attr++;

	unsigned a_arity = attr->arity;
	bool found;
	const char *m_name = find_attribute(q, attr, a_arity, &found);
	if (!found) return false;
	cell *l = c->val_attrs;
	pl_idx l_ctx = c_ctx;
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

#if 0
static bool check_occurs(unsigned var_num, pl_idx var_ctx, cell *c, pl_idx c_ctx)
{
	bool any = false;

	for (unsigned num_cells = c->num_cells; num_cells--; c++) {
		if (!is_var(c))
			continue;

		pl_idx ctx = c_ctx;

		if (is_ref(c))
			ctx = c->var_ctx;

		if (var_num != c->var_num)
			continue;

		if (var_ctx != ctx)
			continue;

		any = true;
	}

	return !any;
}
#endif

bool any_attributed(query *q)
{
	for (unsigned j = 0; j < q->st.fp; j++) {
		const frame *f = GET_FRAME(j);

		for (unsigned i = 0; i < f->actual_slots; i++) {
			slot *e = GET_SLOT(f, i);
			cell *c = deref(q, &e->c, e->c.var_ctx);

			if (!is_empty(c) || !c->val_attrs)
				continue;

			cell *v = c->val_attrs;
			bool any = false;

			while (is_iso_list(v)) {
				cell *h = v + 1;

				//  Ignore \== created by dif/2, but what are they?

				if (!is_op(h))
					any = true;

				v = v + 1;
				v += v->num_cells;
			}

			if (!any)
				continue;

			return true;
		}
	}

	return false;
}

static bool bif_sys_mark_start_1(query * q)
{
	GET_FIRST_ARG(p1,var);
	cell mark;
	make_uint(&mark, q->st.tp);
	unify(q, p1, p1_ctx, &mark, q->st.curr_frame);
	return true;
}

static bool bif_sys_list_attributed_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,var);
	check_memory(init_tmp_heap(q));
	pl_idx mark = get_smalluint(p1);

	for (unsigned j = mark; j < q->st.tp; j++) {
		const trail *tr = q->trails + j;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_num);
		cell *c = deref(q, &e->c, e->c.var_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (!is_empty(c) || !c->val_attrs)
			continue;

		//if (!c_ctx)
		//	continue;

		cell tmp;
		make_ref(&tmp, tr->var_num, tr->var_ctx);
		append_list(q, &tmp);

		if (!is_compound(c->val_attrs))
			continue;

		collect_vars(q, c->val_attrs, c_ctx);

		for (unsigned k = 0; k < q->tab_idx; k++) {
			const frame *f = GET_FRAME(q->pl->tabs[k].ctx);
			slot *e = GET_SLOT(f, q->pl->tabs[k].var_num);
			cell *v = &e->c;

			if (!v->val_attrs)
				continue;

			//if (!q->pl->tabs[k].ctx)
			//	continue;

			cell tmp;
			make_ref(&tmp, q->pl->tabs[k].var_num, q->pl->tabs[k].ctx);
			append_list(q, &tmp);
		}
	}

	cell *l = end_list(q);
	check_memory(l);
	return unify(q, p2, p2_ctx, l, 0);
}

static bool bif_sys_attributed_var_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	const frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_num);
	cell *c = deref(q, &e->c, e->c.var_ctx);
	pl_idx c_ctx = q->latest_ctx;

	if (!c->val_attrs)
		return false;

	cell *l = c->val_attrs;
	pl_idx l_ctx = c_ctx;
	init_tmp_heap(q);
	LIST_HANDLER(l);

	while (is_iso_list(l)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		cell *h1 = deref(q, h+1, l_ctx);

		if (!is_nil(h1))
			append_list(q, h1);

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	l = end_list(q);
	check_memory(l);

	if (is_nil(l))
		return false;

	return true;
}

static bool bif_sys_unattributed_var_1(query *q)
{
	return !bif_sys_attributed_var_1(q);
}

typedef struct {
	blob b;
	pl_idx lo_tp, hi_tp;
	slot e[];
} bind_state;

static void set_occurs(unsigned var_num, pl_idx var_ctx, cell *c, pl_idx c_ctx)
{
	for (unsigned num_cells = c->num_cells; num_cells--; c++) {
		if (!is_var(c))
			continue;

		pl_idx ctx = c_ctx;

		if (is_ref(c))
			ctx = c->var_ctx;

		if (var_num != c->var_num)
			continue;

		if (var_ctx != ctx)
			continue;

		c->flags |= FLAG_VAR_CYCLIC;
	}
}

static bool bif_sys_undo_trail_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);

	if (q->undo_hi_tp == q->undo_lo_tp) {
		unify(q, p1, p1_ctx, make_nil(), q->st.curr_frame);
		return true;
	}

	pl_idx slots = q->undo_hi_tp - q->undo_lo_tp;
	bind_state *save = malloc(sizeof(bind_state)+(sizeof(slot)*slots));
	check_memory(save);
	save->b.ptr = save->b.ptr2 = NULL;
	save->lo_tp = q->undo_lo_tp;
	save->hi_tp = q->undo_hi_tp;
	check_memory(init_tmp_heap(q), free(save));

	for (pl_idx i = q->undo_lo_tp, j = 0; i < q->undo_hi_tp; i++, j++) {
		const trail *tr = q->trails + i;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_num);
		save->e[j].c = e->c;
		cell *c = deref(q, &e->c, e->c.var_ctx);
		pl_idx c_ctx = q->latest_ctx;
		set_occurs(tr->var_num, tr->var_ctx, c, c_ctx);
		cell lhs, rhs;
		make_ref(&lhs, tr->var_num, tr->var_ctx);

		if (is_compound(c))
			make_indirect(&rhs, c, c_ctx);
		else
			rhs = *c;

		cell tmp[3];
		make_instr(tmp, g_minus_s, NULL, 2, 2);
		SET_OP(tmp, OP_YFX);
		tmp[1] = lhs;
		tmp[2] = rhs;
		append_list(q, tmp);
		init_cell(&e->c);
		e->c.val_attrs = tr->attrs;
	}

	cell *tmp = end_list(q);
	check_memory(tmp, free(save));
	unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	cell tmp2;
	make_blob(&tmp2, &save->b);
	unify(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
	return true;
}

static bool bif_sys_redo_trail_1(query * q)
{
	GET_FIRST_ARG(p1,blob);
	const bind_state *save = (bind_state*)p1->val_blob;

	for (pl_idx i = save->lo_tp, j = 0; i < save->hi_tp; i++, j++) {
		const trail *tr = q->trails + i;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_num);
		e->c = save->e[j].c;
	}

	return true;
}

bool do_post_unify_hook(query *q, bool is_builtin)
{
	q->run_hook = false;
	q->undo_lo_tp = q->before_hook_tp;
	q->undo_hi_tp = q->st.tp;
	cell *tmp = alloc_on_heap(q, 3);
	check_memory(tmp);
	make_instr(tmp+0, g_true_s, bif_iso_true_0, 0, 0);
	make_instr(tmp+1, g_post_unify_hook_s, NULL, 0, 0);
	is_builtin ? make_call(q, tmp+2) : make_call_redo(q, tmp+2);
	q->st.instr = tmp;
	return true;
}

builtins g_atts_bifs[] =
{
	{"attribute", 3, bif_attribute_3, "?atom,+atom,+integer", false, false, BLAH},
	{"put_atts", 2, bif_put_atts_2, "@variable,+term", false, false, BLAH},
	{"get_atts", 2, bif_get_atts_2, "@variable,-term", false, false, BLAH},

	{"$list_attributed", 2, bif_sys_list_attributed_2, "+integer,-list", false, false, BLAH},
	{"$unattributed_var", 1, bif_sys_unattributed_var_1, "@variable", false, false, BLAH},
	{"$attributed_var", 1, bif_sys_attributed_var_1, "@variable", false, false, BLAH},
	{"$undo_trail", 2, bif_sys_undo_trail_2, "-list,-blob", false, false, BLAH},
	{"$redo_trail", 1, bif_sys_redo_trail_1, "+blob", false, false, BLAH},
	{"$mark_start", 1, bif_sys_mark_start_1, "-integer", false, false, BLAH},

	{0}
};
