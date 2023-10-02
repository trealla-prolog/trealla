#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "attributed.h"
#include "heap.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

bool fn_sys_list_attributed_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	parser *p = q->p;
	const frame *f = GET_FIRST_FRAME();
	bool first = true;

	for (unsigned i = 0; i < p->nbr_vars; i++) {
		//if (!strcmp(p->vartab.var_name[i], "_"))
		//	continue;

		const slot *e = GET_SLOT(f, i);
		const cell *c = &e->c;

		if (!is_empty(c))
			continue;

		if (!c->attrs)
			continue;

		cell v;
		make_var(&v, new_atom(q->pl, p->vartab.var_name[i]), i);

		if (first) {
			allocate_list(q, &v);
			first = false;
		} else
			append_list(q, &v);
	}

	if (first)
		return unify(q, p1, p1_ctx, make_nil(), q->st.curr_frame);

	cell *l = end_list(q);
	return unify(q, p1, p1_ctx, l, 0);
}

bool fn_sys_put_attributes_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,list_or_nil);
	const frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);

	if (e->c.attrs || !is_nil(p2))
		add_trail(q, p1_ctx, p1->var_nbr, e->c.attrs, e->c.attrs_ctx);

	//DUMP_TERM("$put_attr", p2, p2_ctx ,true);

	if (is_nil(p2)) {
		e->c.flags = 0;
		e->c.attrs = NULL;
		e->c.attrs_ctx = 0;
		return true;
	}

	cell *tmp = deep_clone_to_heap(q, p2, p2_ctx);
	check_heap_error(tmp);
	e->c.flags = FLAG_VAR_ATTR;
	e->c.attrs = tmp;
	e->c.attrs_ctx = q->st.curr_frame;
	return true;
}

bool fn_sys_get_attributes_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	const frame *f = GET_FRAME(p1_ctx);
	const slot *e = GET_SLOT(f, p1->var_nbr);

	if (!e->c.attrs)
		return false;

	return unify(q, p2, p2_ctx, e->c.attrs, e->c.attrs_ctx);
}

bool fn_sys_unattributed_var_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	const frame *f = GET_FRAME(p1_ctx);
	const slot *e = GET_SLOT(f, p1->var_nbr);
	return !e->c.attrs;
}

bool fn_sys_attributed_var_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	const frame *f = GET_FRAME(p1_ctx);
	const slot *e = GET_SLOT(f, p1->var_nbr);
	return e->c.attrs;
}

static void make_new_var(query *q, cell *tmp, unsigned var_nbr, pl_idx var_ctx)
{
	make_ref(tmp, g_anon_s, var_nbr, var_ctx);
}

static void set_new_var(query *q, cell *tmp, cell *v, pl_idx v_ctx)
{
	*tmp = *v;
}

typedef struct {
	blob b;
	pl_idx lo_tp, hi_tp;
	slot e[];
} bind_state;

bool fn_sys_undo_trail_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	q->run_hook = false;

	if (q->undo_hi_tp <= q->undo_lo_tp) {
		unify(q, p1, p1_ctx, make_nil(), q->st.curr_frame);
		return true;
	}

	pl_idx slots = q->undo_hi_tp - q->undo_lo_tp;
	bind_state *save = malloc(sizeof(bind_state)+(sizeof(slot)*slots));
	check_error(save);
	save->b.ptr = save->b.ptr2 = NULL;
	save->lo_tp = q->undo_lo_tp;
	save->hi_tp = q->undo_hi_tp;
	bool first = true;

	// Unbind our vars

	for (pl_idx i = q->undo_lo_tp, j = 0; i < q->undo_hi_tp; i++, j++) {
		const trail *tr = q->trails + i;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		//printf("*** unbind [%u:%u] hi_tp=%u, ctx=%u, var=%u\n", j, i, q->undo_hi_tp, tr->var_ctx, tr->var_nbr);
		save->e[j] = *e;

		cell lhs, rhs;
		make_new_var(q, &lhs, tr->var_nbr, tr->var_ctx);
		set_new_var(q, &rhs, &e->c, e->c.var_ctx);
		//DUMP_TERM("$undo1 rhs", &e->c, e->c.var_ctx, 0);

		cell tmp[3];
		make_struct(tmp, g_minus_s, NULL, 2, 2);
		SET_OP(&tmp[0], OP_YFX);
		tmp[1] = lhs;
		tmp[2] = rhs;

		if (first) {
			allocate_list(q, tmp);
			first = false;
		} else
			append_list(q, tmp);

		init_cell(&e->c);

		if (tr->attrs)
			e->c.flags = FLAG_VAR_ATTR;

		e->c.flags = tr->attrs ? FLAG_VAR_ATTR : 0;
		e->c.attrs = tr->attrs;
		e->c.attrs_ctx = tr->attrs_ctx;
		//DUMP_TERM("$undo2", tr->attrs, tr->attrs_ctx, 0);
	}

	cell *tmp = end_list(q);
	check_heap_error(tmp, free(save));
	//DUMP_TERM("$undo3 tmp", tmp, q->st.curr_frame, 0);
	unify(q, p1, p1_ctx, tmp, q->st.curr_frame);

	cell tmp2 = {0};
	tmp2.tag = TAG_BLOB;
	tmp2.flags = FLAG_MANAGED;
	tmp2.nbr_cells = 1;
	tmp2.val_blob = &save->b;
	tmp2.val_blob->refcnt = 0;
	unify(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
	return true;
}

bool fn_sys_redo_trail_1(query * q)
{
	GET_FIRST_ARG(p1,any);

	if (!is_blob(p1))
		return true;

	const bind_state *save = (bind_state*)p1->val_blob;

	for (pl_idx i = save->lo_tp, j = 0; i < save->hi_tp; i++, j++) {
		const trail *tr = q->trails + i;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		//printf("*** rebind [%u:%u] hi_tp=%u, ctx=%u, var=%u\n", j, i, q->undo_hi_tp, tr->var_ctx, tr->var_nbr);
		*e = save->e[j];
	}

	return true;
}

bool do_post_unification_hook(query *q, bool is_builtin)
{
	q->run_hook = false;
	q->undo_lo_tp = q->before_hook_tp;
	q->undo_hi_tp = q->st.tp;
	q->before_hook_tp = 0;

	cell *tmp = alloc_on_heap(q, 3);
	check_heap_error(tmp);
	// Needed for follow() to work
	tmp[0].tag = TAG_INTERNED;
	tmp[0].arity = 0;
	tmp[0].nbr_cells = 1;
	tmp[0].flags = FLAG_BUILTIN;
	tmp[0].val_off = g_true_s;

	static void *s_fn_ptr1 = NULL;

	if (!s_fn_ptr1)
		s_fn_ptr1 = get_fn_ptr(fn_iso_true_0);

	tmp[0].fn_ptr = s_fn_ptr1;

	tmp[1].tag = TAG_INTERNED;
	tmp[1].nbr_cells = 1;
	tmp[1].arity = 0;
	tmp[1].flags = 0;
	tmp[1].val_off = g_post_unify_hook_s;

	static void *s_fn_ptr2 = NULL;

	if (!s_fn_ptr2)
		s_fn_ptr2 = search_predicate(q->pl->user_m, tmp+1, NULL);

	tmp[1].match = s_fn_ptr2;

	if (!tmp[1].match)
		return throw_error(q, tmp+1, q->st.curr_frame, "existence_error", "procedure");

	if (is_builtin)
		make_call(q, tmp+2);
	else
		make_call_redo(q, tmp+2);

	q->st.curr_cell = tmp;
	return true;
}

