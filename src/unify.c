#include <stdlib.h>

#include "heap.h"
#include "query.h"

static int compare_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth);

static int compare_lists(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	cell *orig_p1 = p1, *orig_p2 = p2;
	pl_idx orig_p1_ctx = p1_ctx, orig_p2_ctx = p2_ctx;
	bool any1 = false, any2 = false;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		cell *c1 = p1 + 1, *c2 = p2 + 1;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;

#if USE_RATIONAL_TREES
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen = 0, save_vgen2 = 0;
		int both = 0;
		DEREF_VAR(any1, both, save_vgen, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_VAR(any1, both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
			if (val) return val;
		}

		if (e1) e1->vgen = save_vgen;
		if (e2) e2->vgen2 = save_vgen2;
#else
		c1 = deref(q, c1, c1_ctx);
		c1_ctx = q->latest_ctx;
		c2 = deref(q, c2, c2_ctx);
		c2_ctx = q->latest_ctx;

		int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
		if (val) return val;
#endif

		p1 = p1 + 1; p1 += p1->nbr_cells;
		p2 = p2 + 1; p2 += p2->nbr_cells;

#if USE_RATIONAL_TREES
		e1 = e2 = NULL;
		int both1 = 0, both2 = 0;
		DEREF_VAR(any2, both1, save_vgen, e1, e1->vgen, p1, p1_ctx, q->vgen);
		DEREF_VAR(any2, both2, save_vgen2, e2, e2->vgen2, p2, p2_ctx, q->vgen);

		if (both1 && both2)
			break;

		if (both1)
			q->is_cyclic1++;

		if (both2)
			q->is_cyclic2++;

		if ((q->is_cyclic1 > 2) && (q->is_cyclic2 > 2))
			break;
#else
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
#endif
	}

#if USE_RATIONAL_TREES
	if (any2 && 0) {
		p1 = orig_p1;
		p1_ctx = orig_p1_ctx;
		p2 = orig_p2;
		p2_ctx = orig_p2_ctx;
		unsigned cnt = 0;

		while (is_iso_list(p1) && is_iso_list(p2)) {
			p1 = p1 + 1; p1 += p1->nbr_cells;
			p2 = p2 + 1; p2 += p2->nbr_cells;
			cell *c1 = p1, *c2 = p2;
			pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;
			RESTORE_VAR(c1, c1_ctx, p1, p1_ctx, q->vgen);
			RESTORE_VAR2(c2, c2_ctx, p2, p2_ctx, q->vgen);

			if ((cnt > g_max_depth) || (cnt > 6000))
				return true;

			cnt++;
		}
	}
#endif

	return compare_internal(q, p1, p1_ctx, p2, p2_ctx, depth+1);
}

static int compare_structs(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	int val = CMP_STRING_TO_STRING(q, p1, p2);
	if (val) return val;

	bool any = false;
	int arity = p1->arity;
	p1 = p1 + 1;
	p2 = p2 + 1;
	int i = 0;

	while (arity--) {
		cell *c1 = p1, *c2 = p2;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;

#if USE_RATIONAL_TREES
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen = 0, save_vgen2 = 0;
		int both = 0;
		DEREF_VAR(any, both, save_vgen, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_VAR(any, both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
			if (val) return val;
		}

		if (e1) e1->vgen = save_vgen;
		if (e2) e2->vgen2 = save_vgen2;

		if (q->cycle_error > 10) // ??
			break;
#else
		c1 = deref(q, p1, p1_ctx);
		c1_ctx = q->latest_ctx;
		c2 = deref(q, p2, p2_ctx);
		c2_ctx = q->latest_ctx;
		int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
		if (val) return val;
#endif

		p1 += p1->nbr_cells;
		p2 += p2->nbr_cells;
		i++;
	}

	return 0;
}

static int compare_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
#if 1
	if ((depth > g_max_depth) || (depth > 6000)) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error++;
		return 0;
	}
#endif

	if (is_var(p1)) {
		if (is_var(p2)) {
			if (p1_ctx < p2_ctx)
				return -1;

			if (p1_ctx > p2_ctx)
				return 1;

			return p1->var_nbr < p2->var_nbr ? -1 : p1->var_nbr > p2->var_nbr ? 1 : 0;
		}

		return -1;
	}

	if (is_var(p2))
		return 1;

	if (is_rational(p1) && is_rational(p2))
		return mp_rat_compare(&p1->val_bigint->irat, &p2->val_bigint->irat);

	if (is_rational(p1) && is_bigint(p2)) {
		mpq_t tmp;
		mp_int_init_copy(&tmp.num, &p2->val_bigint->ival);
		mp_int_init_value(&tmp.den, 1);
		int ok = mp_rat_compare(&p1->val_bigint->irat, &tmp);
		mp_rat_clear(&tmp);
		return ok;
	}

	if (is_rational(p1) && is_smallint(p2))
		return mp_rat_compare_value(&p1->val_bigint->irat, p2->val_int, 1);

	if (is_rational(p1))
		return 1;

	if (is_bigint(p1) && is_rational(p2)) {
		mpq_t tmp;
		mp_int_init_copy(&tmp.num, &p1->val_bigint->ival);
		mp_int_init_value(&tmp.den, 1);
		int ok = mp_rat_compare(&p2->val_bigint->irat, &tmp);
		mp_rat_clear(&tmp);
		return ok;
	}

	if (is_bigint(p1) && is_bigint(p2))
		return mp_int_compare(&p1->val_bigint->ival, &p2->val_bigint->ival);

	if (is_bigint(p1) && is_smallint(p2))
		return mp_int_compare_value(&p1->val_bigint->ival, p2->val_int);

	if (is_bigint(p1) && is_float(p2))
		return 1;

	if (is_bigint(p2))
		return 1;

	if (is_smallint(p1) && is_rational(p2))
		return -mp_rat_compare_value(&p2->val_bigint->irat, p1->val_int, 1);

	if (is_smallint(p1) && is_bigint(p2))
		return -mp_int_compare_value(&p2->val_bigint->ival, p1->val_int);

	if (is_smallint(p1)) {
		if (is_smallint(p2))
			return p1->val_int < p2->val_int ? -1 : p1->val_int > p2->val_int ? 1 : 0;

		if (is_float(p2))
			return 1;

		return -1;
	}

	if (is_float(p1)) {
		if (is_float(p2))
			return p1->val_float < p2->val_float ? -1 : p1->val_float > p2->val_float ? 1 : 0;

		return -1;
	}

	if (is_iso_atom(p1) && is_iso_atom(p2))
		return CMP_STRING_TO_STRING(q, p1, p2);

	if (is_string(p1) && is_string(p2))
		return CMP_STRING_TO_STRING(q, p1, p2);

	if (is_iso_atom(p1)) {
		if (is_number(p2))
			return 1;

		return -1;
	}

	if (p1->arity < p2->arity)
		return -1;

	if (p1->arity > p2->arity)
		return 1;

	if (is_string(p1) && is_string(p2))
		return CMP_STRING_TO_STRING(q, p1, p2);

	if ((is_string(p1) && is_iso_list(p2))
		|| (is_string(p2) && is_iso_list(p1))) {
		LIST_HANDLER(p1);
		LIST_HANDLER(p2);

		while (is_list(p1) && is_list(p2)) {
			cell *c1 = LIST_HEAD(p1);
			c1 = deref(q, c1, p1_ctx);
			pl_idx c1_ctx = q->latest_ctx;
			cell *c2 = LIST_HEAD(p2);
			c2 = deref(q, c2, p2_ctx);
			pl_idx c2_ctx = q->latest_ctx;

			int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
			if (val) return val;

			p1 = LIST_TAIL(p1);
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;
			p2 = LIST_TAIL(p2);
			p2 = deref(q, p2, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (is_list(p1))
			return 1;

		if (is_list(p2))
			return -1;

		return compare_internal(q, p1, p1_ctx, p2, p2_ctx, depth+1);
	}

	if (is_iso_list(p1) && is_iso_list(p2))
		return compare_lists(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	return compare_structs(q, p1, p1_ctx, p2, p2_ctx, depth+1);
}

int compare(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	return compare_internal(q, p1, p1_ctx, p2, p2_ctx, 0);
}

inline static bool any_choices(const query *q, const frame *f)
{
	if (!q->cp)
		return false;

	const choice *ch = GET_CURR_CHOICE();
	return ch->chgen > f->chgen;
}

void set_var(query *q, const cell *c, pl_idx c_ctx, cell *v, pl_idx v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);
	cell *c_attrs = is_empty(&e->c) ? e->c.attrs : NULL;
	pl_idx c_attrs_ctx = c_attrs ? e->c.attrs_ctx : 0;

	if (is_managed(v)) {
		add_trail(q, c_ctx, c->var_nbr, c_attrs, c_attrs_ctx);
	} else if (c_ctx == q->st.fp) {
	} else if (c_ctx != q->st.curr_frame) {
		add_trail(q, c_ctx, c->var_nbr, c_attrs, c_attrs_ctx);
	} else if (q->in_unify || any_choices(q, f) || !is_local(c)) {
		add_trail(q, c_ctx, c->var_nbr, c_attrs, c_attrs_ctx);
	}

	if (c_attrs)
		q->run_hook = true;

	// If anything outside the current frame (q->st.curr_frame) points
	// inside the current frame then we can't TCO.
	// If anything points inside the next (q->st.fp) frame then ditto.

	if (is_compound(v)) {
		make_indirect(&e->c, v, v_ctx);

		if ((c_ctx == q->st.fp) && (v_ctx == q->st.curr_frame))
			q->no_tco = true;
		else if (v_ctx == q->st.fp)
			q->no_tco = true;
	} else if (is_var(v)) {
		make_ref(&e->c, v->var_nbr, v_ctx);

		if ((c_ctx == q->st.fp) && (v_ctx == q->st.curr_frame))
			q->no_tco = true;
	} else if (is_indirect(v)) {
		e->c = *v;

		if ((c_ctx == q->st.fp) && (v_ctx == q->st.curr_frame))
			q->no_tco = true;
	} else {
		e->c = *v;
		share_cell(v);
	}
}

void reset_var(query *q, const cell *c, pl_idx c_ctx, cell *v, pl_idx v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);

	if (is_compound(v)) {
		make_indirect(&e->c, v, v_ctx);
	} else if (is_var(v)) {
		e->c.tag = TAG_VAR;
		e->c.nbr_cells = 1;
		e->c.flags |= FLAG_VAR_REF;
		e->c.var_nbr = v->var_nbr;
		e->c.var_ctx = v_ctx;
	} else {
		e->c = *v;
		share_cell(v);
	}
}

static bool unify_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth);

// This is for when one arg is a string & the other an iso-list...

static bool unify_string_to_list(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx)
{
	LIST_HANDLER(p1);
	LIST_HANDLER(p2);

	while (is_list(p1) && is_iso_list(p2)) {
		cell *c1 = LIST_HEAD(p1);
		cell *c2 = LIST_HEAD(p2);

		pl_idx c1_ctx = p1_ctx;
		c2 = deref(q, c2, p2_ctx);
		pl_idx c2_ctx = q->latest_ctx;

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, 0))
			return false;

		c1 = LIST_TAIL(p1);
		c2 = LIST_TAIL(p2);

		p1 = c1;
		p2 = deref(q, c2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	return unify_internal(q, p1, p1_ctx, p2, p2_ctx, 0);
}

static bool unify_integers(query *q, cell *p1, cell *p2)
{
	if (is_bigint(p1)) {
		if (is_rational(p2)) {
			mpq_t tmp;
			mp_int_init_copy(&tmp.num, &p1->val_bigint->ival);
			mp_int_init_value(&tmp.den, 1);
			bool ok = !mp_rat_compare(&p2->val_bigint->irat, &tmp);
			mp_rat_clear(&tmp);
			return ok;
		}

		if (is_bigint(p2))
			return !mp_int_compare(&p1->val_bigint->ival, &p2->val_bigint->ival);

		if (is_smallint(p2))
			return !mp_int_compare_value(&p1->val_bigint->ival, p2->val_int);

		return false;
	}

	if (is_rational(p2))
		return !mp_int_compare_value(&p2->val_bigint->ival, p1->val_int);

	if (is_bigint(p2))
		return !mp_int_compare_value(&p2->val_bigint->ival, p1->val_int);

	if (p2->tag == TAG_INTEGER)
		return p1->val_int == p2->val_int;

	return false;
}

static bool unify_rationals(query *q, cell *p1, cell *p2)
{
	if (is_rational(p1) && is_rational(p2))
		return !mp_rat_compare(&p1->val_bigint->irat, &p2->val_bigint->irat);

	if (is_rational(p1) && is_bigint(p2)) {
		mpq_t tmp;
		mp_int_init_copy(&tmp.num, &p2->val_bigint->ival);
		mp_int_init_value(&tmp.den, 1);
		bool ok = !mp_rat_compare(&p1->val_bigint->irat, &tmp);
		mp_rat_clear(&tmp);
		return ok;
	}

	if (is_rational(p1) && is_integer(p2))
		return !mp_rat_compare_value(&p1->val_bigint->irat, p2->val_int, 1);

	return false;
}

static bool unify_reals(query *q, cell *p1, cell *p2)
{
	if (is_float(p2))
		return get_float(p1) == get_float(p2);

	return false;
}

static bool unify_literals(query *q, cell *p1, cell *p2)
{
	if (is_interned(p2))
		return p1->val_off == p2->val_off;

	if (is_cstring(p2) && (C_STRLEN(q, p1) == C_STRLEN(q, p2)))
		return !memcmp(C_STR(q, p2), GET_POOL(q, p1->val_off), C_STRLEN(q, p1));

	return false;
}

static bool unify_cstrings(query *q, cell *p1, cell *p2)
{
	if (is_cstring(p2) && (C_STRLEN(q, p1) == C_STRLEN(q, p2)))
		return !memcmp(C_STR(q, p1), C_STR(q, p2), C_STRLEN(q, p1));

	if (is_interned(p2) && (C_STRLEN(q, p1) == C_STRLEN(q, p2)))
		return !memcmp(C_STR(q, p1), GET_POOL(q, p2->val_off), C_STRLEN(q, p1));

	return false;
}

struct dispatch {
	uint8_t tag;
	bool (*fn)(query*, cell*, cell*);
};

static const struct dispatch g_disp[] =
{
	{TAG_EMPTY, NULL},
	{TAG_VAR, NULL},
	{TAG_INTERNED, unify_literals},
	{TAG_CSTR, unify_cstrings},
	{TAG_INTEGER, unify_integers},
	{TAG_DOUBLE, unify_reals},
	{TAG_RATIONAL, unify_rationals},
	{0}
};

static bool unify_lists(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
#if USE_RATIONAL_TREES
	cell *orig_p1 = p1, *orig_p2 = p2;
	pl_idx orig_p1_ctx = p1_ctx, orig_p2_ctx = p2_ctx;
#endif

	bool any1 = false, any2 = false;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		cell *c1 = p1 + 1, *c2 = p2 + 1;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;

#if USE_RATIONAL_TREES
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen = 0, save_vgen2 = 0;
		int both = 0;

		DEREF_VAR(any1, both, save_vgen, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_VAR(any1, both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
				return false;
		}

		if (e1) e1->vgen = save_vgen;
		if (e2) e2->vgen2 = save_vgen2;
#else
		c1 = deref(q, c1, c1_ctx);
		c1_ctx = q->latest_ctx;
		c2 = deref(q, c2, c2_ctx);
		c2_ctx = q->latest_ctx;

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
			return false;
#endif

		p1 = p1 + 1; p1 += p1->nbr_cells;
		p2 = p2 + 1; p2 += p2->nbr_cells;

#if USE_RATIONAL_TREES
		e1 = e2 = NULL;
		int both1 = 0, both2 = 0;
		DEREF_VAR(any2, both1, save_vgen, e1, e1->vgen, p1, p1_ctx, q->vgen);
		DEREF_VAR(any2, both2, save_vgen2, e2, e2->vgen2, p2, p2_ctx, q->vgen);

		if (both1 && both2)
			break;

		if (both1)
			q->is_cyclic1++;

		if (both2)
			q->is_cyclic2++;

		if ((q->is_cyclic1 > 2) && (q->is_cyclic2 > 2))
			break;
#else
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
#endif
	}

#if USE_RATIONAL_TREES
	if (any2) {
		p1 = orig_p1;
		p1_ctx = orig_p1_ctx;
		p2 = orig_p2;
		p2_ctx = orig_p2_ctx;
		unsigned cnt = 0;

		while (is_iso_list(p1) && is_iso_list(p2)) {
			p1 = p1 + 1; p1 += p1->nbr_cells;
			p2 = p2 + 1; p2 += p2->nbr_cells;
			RESTORE_VAR(p1, p1_ctx, p1, p1_ctx, q->vgen);
			RESTORE_VAR2(p2, p2_ctx, p2, p2_ctx, q->vgen);

			if ((cnt > g_max_depth) || (cnt > 6000))
				return true;

			cnt++;
		}
	}
#endif

	return unify_internal(q, p1, p1_ctx, p2, p2_ctx, depth+1);
}

static bool unify_structs(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (p1->arity != p2->arity)
		return false;

	if (p1->val_off != p2->val_off)
		return false;

	bool any = false;
	unsigned arity = p1->arity;
	p1++; p2++;

	while (arity--) {
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;
		cell *c1 = p1, *c2 = p2;

#if USE_RATIONAL_TREES
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen = 0, save_vgen2 = 0;
		int both = 0;
		DEREF_VAR(any, both, save_vgen, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_VAR(any, both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
				return false;
		}

		if (e1) e1->vgen = save_vgen;
		if (e2) e2->vgen2 = save_vgen2;

		if (q->cycle_error > 10) // ??
			break;
#else
		c1 = deref(q, c1, c1_ctx);
		c1_ctx = q->latest_ctx;
		c2 = deref(q, c2, c2_ctx);
		c2_ctx = q->latest_ctx;

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
			return false;
#endif
		p1 += p1->nbr_cells;
		p2 += p2->nbr_cells;
	}

	return true;
}

static bool unify_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
#if 1
	if ((depth > g_max_depth) || (depth > 6000)) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error++;
		return true;
	}
#endif

	if (is_var(p1) && is_var(p2)) {
		if (p2_ctx > p1_ctx)
			set_var(q, p2, p2_ctx, p1, p1_ctx);
		else if (p2_ctx < p1_ctx)
			set_var(q, p1, p1_ctx, p2, p2_ctx);
		else if (p2->var_nbr > p1->var_nbr)
			set_var(q, p2, p2_ctx, p1, p1_ctx);
		else if (p2->var_nbr < p1->var_nbr)
			set_var(q, p1, p1_ctx, p2, p2_ctx);

		return true;
	}

	if (is_var(p2)) {
		cell *tmp = p2;
		pl_idx tmp_ctx = p2_ctx;
		p2 = p1;
		p2_ctx = p1_ctx;
		p1 = tmp;
		p1_ctx = tmp_ctx;
	} else if (is_var(p1))
		q->has_vars = true;

	if (is_var(p1)) {
		bool was_cyclic = false;

		if (q->flags.occurs_check == OCCURS_CHECK_TRUE) {
			if (is_cyclic_term(q, p2, p2_ctx))
				was_cyclic = true;
		} else if (q->flags.occurs_check == OCCURS_CHECK_ERROR) {
			if (is_cyclic_term(q, p2, p2_ctx))
				was_cyclic = true;
		}

		set_var(q, p1, p1_ctx, p2, p2_ctx);

		if (q->flags.occurs_check == OCCURS_CHECK_TRUE) {
			if (!was_cyclic && is_cyclic_term(q, p2, p2_ctx))
				return false;
		} else if (q->flags.occurs_check == OCCURS_CHECK_ERROR) {
			if (!was_cyclic && is_cyclic_term(q, p2, p2_ctx)) {
				q->cycle_error = true;
				return false;
			}
		}

		return true;
	}

	if (is_string(p1)) {
		if (is_string(p2))
			return unify_cstrings(q, p1, p2);

		if (is_iso_list(p2))
			return unify_string_to_list(q, p1, p1_ctx, p2, p2_ctx);

		return false;
	}

	if (is_string(p2)) {
		if (is_iso_list(p1))
			return unify_string_to_list(q, p2, p2_ctx, p1, p1_ctx);

		return false;
	}

	if (is_iso_list(p1) && is_iso_list(p2))
		return unify_lists(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	if (p1->arity || p2->arity)
		return unify_structs(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	return g_disp[p1->tag].fn(q, p1, p2);
}

bool unify(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx)
{
	q->is_cyclic1 = q->is_cyclic2 = false;
	q->before_hook_tp = q->st.tp;
	if (++q->vgen == 0) q->vgen = 1;
	q->in_unify = true;
	bool ok = unify_internal(q, p1, p1_ctx, p2, p2_ctx, 0);
	q->in_unify = false;

	if (q->cycle_error) {
		if (q->flags.occurs_check == OCCURS_CHECK_TRUE)
			return false;

		if (q->flags.occurs_check == OCCURS_CHECK_ERROR)
			return throw_error(q, p2, p2_ctx, "representation_error", "term");
	}

	return ok;
}
