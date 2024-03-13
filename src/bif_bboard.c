#include <stdlib.h>
#include <stdio.h>

#include "heap.h"
#include "module.h"
#include "prolog.h"
#include "query.h"

static bool bif_bb_b_put_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	int var_nbr;

	if ((var_nbr = create_vars(q, 1)) < 0)
		return false;


	snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	char *key = strdup(tmpbuf);
	check_heap_error(init_tmp_heap(q), free(key));
	cell *tmp = deep_clone_to_tmp(q, p2, p2_ctx);
	cell *value = malloc(sizeof(cell)*tmp->nbr_cells);
	dup_cells(value, tmp, tmp->nbr_cells);

	acquire_lock(&q->pl->guard);
	sl_set(q->pl->keyval, key, value);
	release_lock(&q->pl->guard);

	blob *b = calloc(1, sizeof(blob));
	b->ptr = (void*)m;
	b->ptr2 = (void*)key;
	cell c, v;
	make_ref(&c, var_nbr, q->st.curr_frame);
	make_kvref(&v, b);
	unify(q, &c, q->st.curr_frame, &v, q->st.curr_frame);
	return true;
}

static bool bif_bb_put_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	const char *key1 = tmpbuf;
	snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:nb", m->name, C_STR(q, p1));
	char *key = strdup(tmpbuf);
	check_heap_error(init_tmp_heap(q), free(key));
	cell *tmp = deep_clone_to_tmp(q, p2, p2_ctx);
	cell *value = malloc(sizeof(cell)*tmp->nbr_cells);
	dup_cells(value, tmp, tmp->nbr_cells);

	acquire_lock(&q->pl->guard);
	sl_del(q->pl->keyval, key1);
	sl_set(q->pl->keyval, key, value);
	release_lock(&q->pl->guard);

	return true;
}

static bool bif_bb_get_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	const char *key = tmpbuf;
	const void *val;

	acquire_lock(&q->pl->guard);

	if (!sl_get(q->pl->keyval, key, &val)) {
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:nb", m->name, C_STR(q, p1));
		key = tmpbuf;

		if (!sl_get(q->pl->keyval, key, &val)) {
			release_lock(&q->pl->guard);
			return false;
		}
	}

	release_lock(&q->pl->guard);

	cell *tmp = (cell*)val;
	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool bif_bb_delete_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	const char *key = tmpbuf;
	const void *val;

	acquire_lock(&q->pl->guard);

	if (!sl_get(q->pl->keyval, key, &val)) {
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:nb", m->name, C_STR(q, p1));
		key = tmpbuf;

		if (!sl_get(q->pl->keyval, key, &val)) {
			release_lock(&q->pl->guard);
			return false;
		}
	}

	cell *tmp = (cell*)val;

	if (!unify(q, p2, p2_ctx, tmp, q->st.curr_frame)) {
		release_lock(&q->pl->guard);
		return false;
	}

	bool ok = sl_del(q->pl->keyval, key);
	release_lock(&q->pl->guard);

	return ok;
}

static bool bif_bb_update_3(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	const char *key1 = tmpbuf;
	const void *val;

	acquire_lock(&q->pl->guard);

	if (!sl_get(q->pl->keyval, key1, &val)) {
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:nb", m->name, C_STR(q, p1));
		key1 = tmpbuf;

		if (!sl_get(q->pl->keyval, key1, &val)) {
			release_lock(&q->pl->guard);
			return false;
		}
	}

	cell *tmp = (cell*)val;

	if (!unify(q, p2, p2_ctx, tmp, q->st.curr_frame)) {
		release_lock(&q->pl->guard);
		return false;
	}

	char *key = strdup(key1);
	check_heap_error(init_tmp_heap(q), (release_lock(&q->pl->guard), free(key)));
	cell *tmp2 = deep_clone_to_tmp(q, p3, p3_ctx);
	cell *value = malloc(sizeof(cell)*tmp2->nbr_cells);
	dup_cells(value, tmp2, tmp2->nbr_cells);
	sl_del(q->pl->keyval, key1);
	sl_set(q->pl->keyval, key, value);
	release_lock(&q->pl->guard);

	return true;
}

builtins g_bboard_bifs[] =
{
	{"bb_b_put", 2, bif_bb_b_put_2, ":atom,+term", false, false, BLAH},
	{"bb_put", 2, bif_bb_put_2, ":atom,+term", false, false, BLAH},
	{"bb_get", 2, bif_bb_get_2, ":atom,?term", false, false, BLAH},
	{"bb_delete", 2, bif_bb_delete_2, ":atom,?term", false, false, BLAH},
	{"bb_update", 3, bif_bb_update_3, ":atom,?term,?term", false, false, BLAH},

	{0}
};
