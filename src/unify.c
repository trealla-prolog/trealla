#include <stdlib.h>

#include "query.h"

typedef struct { lnode hdr; cell *c1, *c2; pl_idx c1_ctx, c2_ctx; } snode;

static int compare_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth);

static int compare_lists(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	cell *orig_p1 = p1, *orig_p2 = p2;
	pl_idx orig_p1_ctx = p1_ctx, orig_p2_ctx = p2_ctx;
	bool any1 = false, any2 = false;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		cell *c1 = p1 + 1, *c2 = p2 + 1;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen, save_vgen2;
		int both = 0;

		DEREF_VAR(any1, both, save_vgen, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_VAR(any1, both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
			if (val) return val;
		}

		if (e1) e1->vgen = save_vgen;
		if (e2) e2->vgen2 = save_vgen2;

		p1 = p1 + 1; p1 += p1->num_cells;
		p2 = p2 + 1; p2 += p2->num_cells;
		e1 = e2 = NULL;
		int both1 = 0, both2 = 0;

		DEREF_VAR(any2, both1, save_vgen, e1, e1->vgen, p1, p1_ctx, q->vgen);
		DEREF_VAR(any2, both2, save_vgen2, e2, e2->vgen2, p2, p2_ctx, q->vgen);

		if (both1)
			q->is_cyclic1++;

		if (both2)
			q->is_cyclic2++;

		if (q->is_cyclic1 && q->is_cyclic2)
			break;
	}

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

	while (arity--) {
		cell *c1 = p1, *c2 = p2;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen, save_vgen2;
		int both = 0;

		DEREF_VAR(any, both, save_vgen, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_VAR(any, both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
			if (val) return val;
		}

		if (e1) e1->vgen = save_vgen;
		if (e2) e2->vgen2 = save_vgen2;

		p1 += p1->num_cells;
		p2 += p2->num_cells;
	}

	return 0;
}

static int compare_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (depth > 30) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		return 0;
	}

	if (is_var(p1)) {
		if (is_var(p2)) {
			if (p1_ctx < p2_ctx)
				return -1;

			if (p1_ctx > p2_ctx)
				return 1;

			return p1->var_num < p2->var_num ? -1 : p1->var_num > p2->var_num ? 1 : 0;
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

	if (is_codes(p1) && is_string(p2) && !is_codes(p2))
		return -1;

	if (is_codes(p2) && is_string(p1) && !is_codes(p1))
		return -1;

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
	q->is_cyclic1 = q->is_cyclic2 = false;
	if (++q->vgen == 0) q->vgen = 1;
	return compare_internal(q, p1, p1_ctx, p2, p2_ctx, 0);
}

void add_trail(query *q, pl_idx c_ctx, unsigned c_var_nbr, cell *attrs)
{
	if (!check_trail(q)) {
		q->error = false;
		return;
	}

	trail *tr = q->trails + q->st.tp++;
	tr->val_ctx = c_ctx;
	tr->var_num = c_var_nbr;
	tr->attrs = attrs;
}

static void set_var(query *q, const cell *c, pl_idx c_ctx, cell *v, pl_idx v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = get_slot(q, f, c->var_num);
	cell *c_attrs = is_empty(&e->c) ? e->c.val_attrs : NULL;

	if (is_managed(v) || (c_ctx != q->st.fp))
		add_trail(q, c_ctx, c->var_num, c_attrs);

	if (c_attrs)
		q->run_hook = true;

	if (is_var(v)) {
		make_ref(&e->c, v->var_num, v_ctx);

		if ((c_ctx == q->st.fp)
			//&& (v_ctx >= q->st.curr_frame)
			&& !is_temporary(c) && !is_void(c)
			) {
			q->no_recov = true;
			q->total_no_recovs++;
		}
	} else if (is_compound(v)) {
		make_indirect(&e->c, v, v_ctx);

		if ((v_ctx >= q->st.curr_frame)
			&& !is_ground(v)
			){
			q->no_recov = true;
			q->total_no_recovs++;
		}
	} else {
		e->c = *v;
		share_cell(v);
	}
}

void reset_var(query *q, const cell *c, pl_idx c_ctx, cell *v, pl_idx v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = get_slot(q, f, c->var_num);
	share_cell(v);
	unshare_cell(&e->c);
	e->c = *v;
}

void undo_var(query *q, const cell *c, pl_idx c_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = get_slot(q, f, c->var_num);
	unshare_cell(&e->c);
	e->c.tag = TAG_EMPTY;
	e->c.val_attrs = NULL;
	// TO-DO: undo on trail
	q->st.tp--;
}

static bool unify_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth);

static bool unify_string_to_list(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
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

static bool unify_integers(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
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

	if (p2->tag == TAG_INT)
		return p1->val_int == p2->val_int;

	return false;
}

static bool unify_rationals(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
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

static bool unify_floats(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (is_float(p2))
		return p1->val_float == p2->val_float;

	return false;
}

static bool unify_cstrings(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (is_cstring(p2) && (C_STRLEN(q, p1) == C_STRLEN(q, p2)))
		return !memcmp(C_STR(q, p1), C_STR(q, p2), C_STRLEN(q, p1));

	if (is_interned(p2) && (C_STRLEN(q, p1) == C_STRLEN(q, p2)))
		return !memcmp(C_STR(q, p1), GET_POOL(q, p2->val_off), C_STRLEN(q, p1));

	return false;
}

static bool unify_lists(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	cell *orig_p1 = p1, *orig_p2 = p2;
	pl_idx orig_p1_ctx = p1_ctx, orig_p2_ctx = p2_ctx;
	bool any1 = false, any2 = false;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		cell *c1 = p1 + 1, *c2 = p2 + 1;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen, save_vgen2;
		int both = 0;

		DEREF_VAR(any1, both, save_vgen, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_VAR(any1, both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
				return false;
		}

		if (e1) e1->vgen = save_vgen;
		if (e2) e2->vgen2 = save_vgen2;
		p1 = p1 + 1; p1 += p1->num_cells;
		p2 = p2 + 1; p2 += p2->num_cells;
		e1 = e2 = NULL;
		int both1 = 0, both2 = 0;

		DEREF_VAR(any2, both1, save_vgen, e1, e1->vgen, p1, p1_ctx, q->vgen);
		DEREF_VAR(any2, both2, save_vgen2, e2, e2->vgen2, p2, p2_ctx, q->vgen);

		if (both1)
			q->is_cyclic1++;

		if (both2)
			q->is_cyclic2++;

		if (q->is_cyclic1 && q->is_cyclic2)
			break;
	}

	return unify_internal(q, p1, p1_ctx, p2, p2_ctx, depth+1);
}

static bool unify_structs(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (p1->arity != p2->arity)
		return false;

	if (p1->val_off != p2->val_off)
		return false;

	unsigned arity = p1->arity;
	p1++; p2++;

	while (arity--) {
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;
		cell *c1 = p1, *c2 = p2;
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen, save_vgen2;
		bool any = false;
		int both = 0;

		DEREF_VAR(any, both, save_vgen, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_VAR(any, both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
				return false;
		}

		if (e1) e1->vgen = save_vgen;
		if (e2) e2->vgen2 = save_vgen2;
		p1 += p1->num_cells;
		p2 += p2->num_cells;
	}

	return true;
}

static bool unify_var(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	bool was_cyclic = false;
	bool check = is_global(p1) || (p1_ctx == 0);

	if (q->flags.occurs_check == OCCURS_CHECK_TRUE) {
		if (check && is_cyclic_term(q, p2, p2_ctx))
			was_cyclic = true;
	} else if (q->flags.occurs_check == OCCURS_CHECK_ERROR) {
		if (check && is_cyclic_term(q, p2, p2_ctx))
			was_cyclic = true;
	}

	set_var(q, p1, p1_ctx, p2, p2_ctx);

	if (q->flags.occurs_check == OCCURS_CHECK_TRUE) {
		if (!was_cyclic && check && is_cyclic_term(q, p2, p2_ctx)) {
			q->cycle_error = 1;
			return false;
		}
	} else if (q->flags.occurs_check == OCCURS_CHECK_ERROR) {
		if (!was_cyclic && check && is_cyclic_term(q, p2, p2_ctx)) {
			q->cycle_error = 1;
			return false;
		}
	}

	return true;
}

static bool unify_interned(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (is_iso_list(p1) && is_iso_list(p2))
		return unify_lists(q, p1, p1_ctx, p2, p2_ctx, depth);

	if (p1->arity || p2->arity)
		return unify_structs(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	if (is_interned(p2))
		return p1->val_off == p2->val_off;

	if (is_cstring(p2) && (C_STRLEN(q, p1) == C_STRLEN(q, p2)))
		return !memcmp(C_STR(q, p2), GET_POOL(q, p1->val_off), C_STRLEN(q, p1));

	return false;
}

struct dispatch {
	uint8_t tag;
	bool (*fn)(query*, cell*, pl_idx, cell*, pl_idx, unsigned);
};

static const struct dispatch g_disp[] =
{
	{TAG_EMPTY, NULL},
	{TAG_VAR, NULL},
	{TAG_INTERNED, unify_interned},
	{TAG_CSTR, unify_cstrings},
	{TAG_INT, unify_integers},
	{TAG_FLOAT, unify_floats},
	{TAG_RAT, unify_rationals},
	{0}
};

static bool unify_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (is_var(p1) && is_var(p2)) {
		if (p2_ctx > p1_ctx)
			set_var(q, p2, p2_ctx, p1, p1_ctx);
		else if (p2_ctx < p1_ctx)
			set_var(q, p1, p1_ctx, p2, p2_ctx);
		else if (p2->var_num > p1->var_num)
			set_var(q, p2, p2_ctx, p1, p1_ctx);
		else if (p2->var_num < p1->var_num)
			set_var(q, p1, p1_ctx, p2, p2_ctx);

		return true;
	}

	if (is_var(p2)) {
		set_var(q, p2, p2_ctx, p1, p1_ctx);
		return true;
	} else if (is_var(p1)) {
		if (depth > 1)
			q->has_vars = true;

		return unify_var(q, p1, p1_ctx, p2, p2_ctx, depth);
	}

	if (is_codes(p1) && is_string(p2) && !is_codes(p2))
		return false;

	if (is_codes(p2) && is_string(p1) && !is_codes(p1))
		return false;

	if (is_string(p1)) {
		if (is_string(p2))
			return unify_cstrings(q, p1, p1_ctx, p2, p2_ctx, depth);

		if (is_iso_list(p2))
			return unify_string_to_list(q, p1, p1_ctx, p2, p2_ctx, depth);

		return false;
	}

	if (is_string(p2)) {
		if (is_iso_list(p1))
			return unify_string_to_list(q, p2, p2_ctx, p1, p1_ctx, depth);

		return false;
	}

	if ((q->is_cyclic1 || q->is_cyclic2)) {
		if (depth > 12) {
			//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
			q->cycle_error++;
			return true;
		}
	} else if (depth > 30) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error++;
		return true;
	}

	return g_disp[p1->tag].fn(q, p1, p1_ctx, p2, p2_ctx, depth);
}

bool unify(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx)
{
	q->is_cyclic1 = q->is_cyclic2 = false;
	q->has_vars = q->no_recov = false;
	q->before_hook_tp = q->st.tp;
	if (++q->vgen == 0) q->vgen = 1;
	bool ok;

	if (!is_var(p1) && is_var(p2))
		ok = unify_internal(q, p2, p2_ctx, p1, p1_ctx, 0);
	else
		ok = unify_internal(q, p1, p1_ctx, p2, p2_ctx, 0);

	if (q->cycle_error) {
		if (q->flags.occurs_check == OCCURS_CHECK_ERROR)
			return throw_error(q, p2, p2_ctx, "representation_error", "term");
	}

	if (!ok)
		return false;

	if (q->no_recov) {
		frame *f = GET_CURR_FRAME();
		f->no_recov = true;
	}

	return true;
}
