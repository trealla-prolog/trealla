#include <stdlib.h>
#include <stdio.h>

#include "heap.h"
#include "module.h"
#include "prolog.h"
#include "query.h"

#define is_smallatomic(c) (is_atom(c) || is_smallint(c))

#define DO_DUMP 0

#define DUMP_TERM2(s,k,c,c_ctx,running) { \
	fprintf(stderr, "%s %s ", s, k); \
	q->nl = true; q->quoted = true; \
	print_term(q, stderr, c, c_ctx, running); \
	q->nl = false; q->quoted = false; \
}

static bool bif_bb_b_put_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_smallatomic(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	int var_nbr;

	if ((var_nbr = create_vars(q, 1)) < 0)
		return false;

	if (is_atom(p1))
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	else
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:b", m->name, (int)get_smallint(p1));

	if (DO_DUMP) DUMP_TERM2("bb_b_put", tmpbuf, p2, p2_ctx, 1);

	char *key = strdup(tmpbuf);
	check_heap_error(init_tmp_heap(q), free(key));
	cell *tmp = deep_clone_to_tmp(q, p2, p2_ctx);
	cell *val = malloc(sizeof(cell)*tmp->nbr_cells);
	check_heap_error(val);
	dup_cells(val, tmp, tmp->nbr_cells);

	prolog_lock(q->pl);
	sl_set(q->pl->keyval, key, val);
	prolog_unlock(q->pl);

	blob *b = calloc(1, sizeof(blob));
	b->ptr = (void*)m;
	b->ptr2 = (void*)strdup(key);
	cell c, v;
	make_ref(&c, var_nbr, q->st.curr_frame);
	make_kvref(&v, b);
	unify(q, &c, q->st.curr_frame, &v, q->st.curr_frame);
	return true;
}

static bool bif_bb_put_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_smallatomic(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	if (is_atom(p1))
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	else
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:b", m->name, (int)get_smallint(p1));

	const char *key1 = tmpbuf;

	if (is_atom(p1))
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:nb", m->name, C_STR(q, p1));
	else
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:nb", m->name, (int)get_smallint(p1));

	if (DO_DUMP) DUMP_TERM2("bb_put", tmpbuf, p2, p2_ctx, 1);

	// Note: we have to save a copy of attributes...

	char *key = strdup(tmpbuf);
	check_heap_error(init_tmp_heap(q), free(key));
	cell *tmp = deep_copy_to_tmp(q, p2, p2_ctx, true);
	cell *val = malloc(sizeof(cell)*tmp->nbr_cells);
	check_heap_error(val);
	dup_cells(val, tmp, tmp->nbr_cells);

	prolog_lock(q->pl);
	sl_del(q->pl->keyval, key1);
	sl_set(q->pl->keyval, key, val);
	prolog_unlock(q->pl);

	return true;
}

static bool bif_bb_get_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_smallatomic(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	if (is_atom(p1))
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	else
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:b", m->name, (int)get_smallint(p1));

	const char *key = tmpbuf;
	const void *val;

	prolog_lock(q->pl);

	if (!sl_get(q->pl->keyval, key, &val)) {
		if (is_atom(p1))
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:nb", m->name, C_STR(q, p1));
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:nb", m->name, (int)get_smallint(p1));

		key = tmpbuf;

		if (!sl_get(q->pl->keyval, key, &val)) {
			prolog_unlock(q->pl);
			return false;
		}
	}

	prolog_unlock(q->pl);

	cell *tmp = deep_copy_to_heap(q, (cell*)val, q->st.fp, true);
	check_heap_error(tmp);

	if (DO_DUMP) DUMP_TERM2("bb_get", tmpbuf, tmp, q->st.curr_frame, 1);

	if (is_var(p2) && is_var(tmp)) {
		const frame *f = GET_FRAME(q->st.curr_frame);
		const slot *e = GET_SLOT(f, tmp->var_nbr);
		const frame *f2 = GET_FRAME(p2_ctx);
		slot *e2 = GET_SLOT(f2, p2->var_nbr);
		*e2 = *e;
		return true;
	}

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool bif_bb_delete_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,any);

	if (is_compound(p1) &&
		((p1->val_off != g_colon_s)  ||  (p1->arity != 2)))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	module *m;
	char tmpbuf[1024];

	if (is_compound(p1)) {
		cell *p1_m = p1 + 1;
		p1 = p1_m + p1_m->nbr_cells;

		if (!is_atom(p1_m) || !is_smallatomic(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	if (is_atom(p1))
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	else
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:b", m->name, (int)get_smallint(p1));

	const char *key = tmpbuf;
	const void *val;

	prolog_lock(q->pl);

	if (!sl_get(q->pl->keyval, key, &val)) {
		if (is_atom(p1))
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:nb", m->name, C_STR(q, p1));
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:nb", m->name, (int)get_smallint(p1));

		key = tmpbuf;

		if (!sl_get(q->pl->keyval, key, &val)) {
			prolog_unlock(q->pl);
			return false;
		}
	}

	cell *tmp = deep_copy_to_heap(q, (cell*)val, q->st.fp, true);
	check_heap_error(tmp, prolog_unlock(q->pl));

	if (DO_DUMP) DUMP_TERM2("bb_delete", tmpbuf, tmp, q->st.curr_frame, 1);

	if (is_var(p2) && is_var(tmp)) {
		const frame *f = GET_FRAME(q->st.curr_frame);
		const slot *e = GET_SLOT(f, tmp->var_nbr);
		const frame *f2 = GET_FRAME(p2_ctx);
		slot *e2 = GET_SLOT(f2, p2->var_nbr);
		*e2 = *e;
		bool ok = sl_del(q->pl->keyval, key);
		prolog_unlock(q->pl);
		return ok;
	}

	if (!unify(q, p2, p2_ctx, tmp, q->st.curr_frame)) {
		prolog_unlock(q->pl);
		return false;
	}

	bool ok = sl_del(q->pl->keyval, key);
	prolog_unlock(q->pl);
	return ok;
}

static bool bif_bb_update_3(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
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

		if (!is_atom(p1_m) || !is_smallatomic(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		m = find_module(q->pl, C_STR(q, p1_m));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1_m));
	} else
		m = q->st.m;

	if (is_atom(p1))
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:b", m->name, C_STR(q, p1));
	else
		snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:b", m->name, (int)get_smallint(p1));

	const char *key1 = tmpbuf;
	const void *val;

	prolog_lock(q->pl);

	if (!sl_get(q->pl->keyval, key1, &val)) {
		if (is_atom(p1))
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%s:nb", m->name, C_STR(q, p1));
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%d:nb", m->name, (int)get_smallint(p1));

		key1 = tmpbuf;

		if (!sl_get(q->pl->keyval, key1, &val)) {
			prolog_unlock(q->pl);
			return false;
		}
	}

	cell *tmp = deep_copy_to_heap(q, (cell*)val, q->st.fp, true);
	check_heap_error(tmp, prolog_unlock(q->pl));

	if (DO_DUMP) DUMP_TERM2("bb_update", tmpbuf, p2, p2_ctx, 1);

	if (!unify(q, p2, p2_ctx, tmp, q->st.curr_frame)) {
		prolog_unlock(q->pl);
		return false;
	}

	char *key = strdup(key1);
	check_heap_error(init_tmp_heap(q), (prolog_unlock(q->pl), free(key)));
	tmp = deep_clone_to_tmp(q, p3, p3_ctx);
	cell *value = malloc(sizeof(cell)*tmp->nbr_cells);
	check_heap_error(value);
	dup_cells(value, tmp, tmp->nbr_cells);
	sl_del(q->pl->keyval, key1);
	sl_set(q->pl->keyval, key, value);

	prolog_unlock(q->pl);

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
