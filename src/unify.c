#include <stdlib.h>

#include "heap.h"
#include "query.h"

static int compare_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth);

static int compare_lists(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	cell *orig_p1 = p1, *orig_p2 = p2;
	pl_idx orig_p1_ctx = p1_ctx, orig_p2_ctx = p2_ctx;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		cell *h1 = p1 + 1, *h2 = p2 + 1;
		pl_idx h1_ctx = p1_ctx, h2_ctx = p2_ctx;

#if USE_RATIONAL_TREES
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen1 = 0, save_vgen2 = 0;
		int both = 0;
		DEREF_CHECKED(both, save_vgen1, e1, e1->vgen, h1, h1_ctx, q->vgen);
		DEREF_CHECKED(both, save_vgen2, e2, e2->vgen2, h2, h2_ctx, q->vgen);

		if (both == 0) {
			int val = compare_internal(q, h1, h1_ctx, h2, h2_ctx, depth+1);
			if (val) return val;
		} else if (both == 1) {
			h1 = deref(q, p1+1, p1_ctx);
			h1_ctx = q->latest_ctx;
			h2 = deref(q, p2+1, p2_ctx);
			h2_ctx = q->latest_ctx;
			int val = compare_internal(q, h1, h1_ctx, h2, h2_ctx, depth+1);
			if (val) return val;
		}

		if (e1) e1->vgen = save_vgen1;
		if (e2) e2->vgen2 = save_vgen2;
#else
		h1 = deref(q, h1, h1_ctx);
		h1_ctx = q->latest_ctx;
		h2 = deref(q, h2, h2_ctx);
		h2_ctx = q->latest_ctx;

		int val = compare_internal(q, h1, h1_ctx, h2, h2_ctx, depth+1);
		if (val) return val;
#endif

		p1 = p1 + 1; p1 += p1->nbr_cells;
		p2 = p2 + 1; p2 += p2->nbr_cells;

#if USE_RATIONAL_TREES
		both = 0;
		DEREF_CHECKED(both, e1->save_vgen, e1, e1->vgen, p1, p1_ctx, q->vgen);
		DEREF_CHECKED(both, e2->save_vgen2, e2, e2->vgen2, p2, p2_ctx, q->vgen);

		if (both)
			return 0;
#else
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
#endif
	}

#if USE_RATIONAL_TREES
	p1 = orig_p1;
	p1_ctx = orig_p1_ctx;
	p2 = orig_p2;
	p2_ctx = orig_p2_ctx;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		p1 = p1 + 1; p1 += p1->nbr_cells;
		p2 = p2 + 1; p2 += p2->nbr_cells;
		cell *c1 = p1, *c2 = p2;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;

		if (is_var(c1)) {
			if (is_ref(c1))
				c1_ctx = c1->var_ctx;

			const frame *f = GET_FRAME(c1_ctx);
			slot *e = GET_SLOT(f, c1->var_nbr);
			e->vgen = e->save_vgen;
			p1 = deref(q, c1, c1_ctx);
			p1_ctx = q->latest_ctx;
		}

		if (is_var(c2)) {
			if (is_ref(c2))
				c2_ctx = c2->var_ctx;

			const frame *f = GET_FRAME(c2_ctx);
			slot *e = GET_SLOT(f, c2->var_nbr);
			e->vgen = e->save_vgen2;
			p2 = deref(q, c2, c2_ctx);
			p2_ctx = q->latest_ctx;
		}
	}
#endif

	return compare_internal(q, p1, p1_ctx, p2, p2_ctx, depth+1);
}

static int compare_structs(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	int val = CMP_STR_TO_STR(q, p1, p2);
	if (val) return val;

	int arity = p1->arity;
	p1 = p1 + 1;
	p2 = p2 + 1;

	while (arity--) {
		cell *c1 = p1, *c2 = p2;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;

#if USE_RATIONAL_TREES
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen1 = 0, save_vgen2 = 0;
		int both = 0;
		DEREF_CHECKED(both, save_vgen1, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_CHECKED(both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both == 0) {
			int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
			if (val) return val;
		} else if (both == 1) {
			c1 = deref(q, p1, p1_ctx);
			c1_ctx = q->latest_ctx;
			c2 = deref(q, p2, p2_ctx);
			c2_ctx = q->latest_ctx;
			int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
			if (val) return val;
		}

		if (e1) e1->vgen = save_vgen1;
		if (e2) e2->vgen2 = save_vgen2;
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
	}

	return 0;
}

static int compare_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (depth >= g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return 0;
	}

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
		return CMP_STR_TO_STR(q, p1, p2);

	if (is_string(p1) && is_string(p2))
		return CMP_STR_TO_STR(q, p1, p2);

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
		return CMP_STR_TO_STR(q, p1, p2);

	if ((is_string(p1) && is_iso_list(p2))
		|| (is_string(p2) && is_iso_list(p1))) {
		LIST_HANDLER(p1);
		LIST_HANDLER(p2);

		while (is_list(p1) && is_list(p2)) {
			cell *h1 = LIST_HEAD(p1);
			h1 = deref(q, h1, p1_ctx);
			pl_idx h1_ctx = q->latest_ctx;
			cell *h2 = LIST_HEAD(p2);
			h2 = deref(q, h2, p2_ctx);
			pl_idx h2_ctx = q->latest_ctx;

			int val = compare_internal(q, h1, h1_ctx, h2, h2_ctx, depth+1);
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
	q->cycle_error = false;
	if (++q->vgen == 0) q->vgen = 1;
	return compare_internal(q, p1, p1_ctx, p2, p2_ctx, 0);
}

bool accum_var(query *q, const cell *c, pl_idx c_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	const slot *e = GET_SLOT(f, c->var_nbr);
	const void *v;

	if (sl_get(q->vars, e, &v)) {
		size_t idx = (size_t)v;
		q->pl->tabs[idx].cnt++;
		return true;
	}

	sl_set(q->vars, e, (void*)(size_t)q->tab_idx);

	if (!q->pl->tabs) {
		q->pl->tabs_size = 4000;
		q->pl->tabs = malloc(sizeof(var_item)*q->pl->tabs_size);
		check_error(!q->pl->tabs);
	}

	if (q->tab_idx == q->pl->tabs_size) {
		q->pl->tabs_size *= 2;
		q->pl->tabs = realloc(q->pl->tabs, sizeof(var_item)*q->pl->tabs_size);
		check_error(!q->pl->tabs);
	}

	q->pl->tabs[q->tab_idx].ctx = c_ctx;
	q->pl->tabs[q->tab_idx].var_nbr = c->var_nbr;
	q->pl->tabs[q->tab_idx].val_off = c->val_off;
	q->pl->tabs[q->tab_idx].is_anon = is_anon(c) ? true : false;
	q->pl->tabs[q->tab_idx].cnt = 1;
	q->tab_idx++;
	return false;
}

static void collect_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth);

static void collect_var_lists(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	cell *l = p1;
	pl_idx l_ctx = p1_ctx;

	while (is_iso_list(l)) {
		cell *h = l + 1;
		pl_idx h_ctx = l_ctx;
		slot *e = NULL;
		uint32_t save_vgen = 0;
		int both = 0;
		DEREF_CHECKED(both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);

		if (!both && is_var(h))
			accum_var(q, h, h_ctx);
		else if (!both)
			collect_vars_internal(q, h, h_ctx, depth+1);

		if (e) e->vgen = save_vgen;

		l = l + 1; l += l->nbr_cells;
		e = NULL;
		both = 0;
		DEREF_CHECKED(both, save_vgen, e, e->vgen, l, l_ctx, q->vgen);

		if (both)
			return;
	}

	l = p1;
	l_ctx = p1_ctx;

	while (is_iso_list(l) && !q->cycle_error) {
		l = l + 1; l += l->nbr_cells;
		cell *c = l;
		pl_idx c_ctx = l_ctx;

		if (is_var(c)) {
			if (is_ref(c))
				c_ctx = c->var_ctx;

			const frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			e->vgen = e->save_vgen;
			l = deref(q, c, c_ctx);
			l_ctx = q->latest_ctx;
		}
	}

	collect_vars_internal(q, l, l_ctx, depth+1);
}

static void collect_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (depth >= g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return;
	}

	if (is_var(p1)) {
		accum_var(q, p1, p1_ctx);
		return;
	}

	if (!is_structure(p1))
		return;

	if (is_iso_list(p1)) {
		collect_var_lists(q, p1, p1_ctx, depth+1);
		return;
	}

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = p1;
		pl_idx c_ctx = p1_ctx;
		slot *e = NULL;
		uint32_t save_vgen = 0;
		int both = 0;
		DEREF_CHECKED(both, save_vgen, e, e->vgen, c, c_ctx, q->vgen);

		if (!both && is_var(c))
			accum_var(q, c, c_ctx);
		else if (!both)
			collect_vars_internal(q, c, c_ctx, depth+1);

		if (e) e->vgen = save_vgen;

		p1 += p1->nbr_cells;
	}
}

void collect_vars(query *q, cell *p1, pl_idx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	q->tab_idx = 0;
	ensure(q->vars = sl_create(NULL, NULL, NULL));
	collect_vars_internal(q, p1, p1_ctx, 0);
	sl_destroy(q->vars);
	q->vars = NULL;
}

static bool has_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth);

static bool has_vars_lists(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	cell *l = p1;
	pl_idx l_ctx = p1_ctx;

	while (is_iso_list(l)) {
		cell *c = l + 1;
		pl_idx c_ctx = l_ctx;

		if (is_var(c)) {
			if (is_ref(c))
				c_ctx = c->var_ctx;

			const frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			c = deref(q, c, c_ctx);
			c_ctx = q->latest_ctx;

			if (is_var(c))
				return true;

			if (!is_var(c) && (e->vgen != q->vgen)) {
				uint32_t save_vgen = e->vgen;
				e->vgen = q->vgen;

				if (has_vars_internal(q, c, c_ctx, depth+1))
					return true;

				e->vgen = save_vgen;
			}
		} else {
			if (has_vars_internal(q, c, c_ctx, depth+1))
				return true;
		}

		l = l + 1; l += l->nbr_cells;

		if (is_var(l)) {
			if (is_ref(l))
				l_ctx = l->var_ctx;

			const frame *f = GET_FRAME(l_ctx);
			slot *e = GET_SLOT(f, l->var_nbr);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;

			if (!is_var(l) && (e->vgen == q->vgen))
				return false;

			e->vgen = q->vgen;
		}

		if (q->cycle_error)
			break;
	}

	l = p1;
	l_ctx = p1_ctx;

	while (is_iso_list(l) && !q->cycle_error) {
		l = l + 1; l += l->nbr_cells;
		cell *c = l;
		pl_idx c_ctx = l_ctx;

		if (is_var(c)) {
			if (is_ref(c))
				c_ctx = c->var_ctx;

			const frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			e->vgen = e->save_vgen;
			l = deref(q, c, c_ctx);
			l_ctx = q->latest_ctx;
		}
	}

	return has_vars_internal(q, l, l_ctx, depth+1);
}

static bool has_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (depth >= g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return false;
	}

	if (is_var(p1))
		return true;

	if (!is_structure(p1))
		return false;

	if (is_iso_list(p1))
		return has_vars_lists(q, p1, p1_ctx, depth+1);

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = p1;
		pl_idx c_ctx = p1_ctx;

		if (is_var(c)) {
			frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			c = deref(q, c, c_ctx);
			c_ctx = q->latest_ctx;

			if (is_var(c))
				return true;

			if (!is_var(c) && (e->vgen != q->vgen)) {
				e->vgen = q->vgen;

				if (has_vars_internal(q, c, c_ctx, depth+1))
					return true;
			} else
				e->vgen = q->vgen;
		} else {
			if (has_vars_internal(q, c, c_ctx, depth+1))
				return true;
		}

		p1 += p1->nbr_cells;
	}

	return false;
}

bool has_vars(query *q, cell *p1, pl_idx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	return has_vars_internal(q, p1, p1_ctx, 0);
}

static bool is_cyclic_term_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth);

static bool is_cyclic_term_lists(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	cell *save_p1 = p1;
	pl_idx save_p1_ctx = p1_ctx;

	while (is_iso_list(p1)) {
		cell *h = p1 + 1;
		pl_idx h_ctx = p1_ctx;
		slot *e = NULL;
		uint32_t save_vgen = 0;
		int both = 0;
		DEREF_CHECKED(both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);

		if (both)
			return true;

		if (is_cyclic_term_internal(q, h, h_ctx, depth+1))
			return true;

		if (e) e->vgen = save_vgen;

		p1 = p1 + 1; p1 += p1->nbr_cells;
		both = 0;
		DEREF_CHECKED(both, e->save_vgen, e, e->vgen, p1, p1_ctx, q->vgen);

		if (both)
			return true;
	}

	p1 = save_p1;
	p1_ctx = save_p1_ctx;

	while (is_iso_list(p1)) {
		p1 = p1 + 1; p1 += p1->nbr_cells;
		cell *c = p1;
		pl_idx c_ctx = p1_ctx;

		if (is_var(c)) {
			if (is_ref(c))
				c_ctx = c->var_ctx;

			const frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			e->vgen = e->save_vgen;
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;
		}
	}

	return is_cyclic_term_internal(q, p1, p1_ctx, depth+1);
}

static bool is_cyclic_term_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (depth > g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return true;
	}

	if (!is_structure(p1))
		return false;

	if (is_iso_list(p1))
		return is_cyclic_term_lists(q, p1, p1_ctx, depth);

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = p1;
		pl_idx c_ctx = p1_ctx;
		slot *e = NULL;
		uint32_t save_vgen = 0;
		int both = 0;
		DEREF_CHECKED(both, save_vgen, e, e->vgen, c, c_ctx, q->vgen);

		if (both)
			return true;

		if (is_cyclic_term_internal(q, c, c_ctx, depth+1))
			return true;

		if (e) e->vgen = save_vgen;
		p1 += p1->nbr_cells;
	}

	return false;
}

bool is_cyclic_term(query *q, cell *p1, pl_idx p1_ctx)
{
	q->cycle_error = false;
	if (++q->vgen == 0) q->vgen = 1;
	return is_cyclic_term_internal(q, p1, p1_ctx, 0);
}

bool is_acyclic_term(query *q, cell *p1, pl_idx p1_ctx)
{
	q->cycle_error = false;
	if (++q->vgen == 0) q->vgen = 1;
	return !is_cyclic_term_internal(q, p1, p1_ctx, 0);
}

static cell *term_next(query *q, cell *c, pl_idx *c_ctx, bool *done)
{
	if (!is_iso_list(c)) {
		*done = true;
		return c;
	}

	c += 1;
	c += c->nbr_cells;
	c = deref(q, c, *c_ctx);
	*c_ctx = q->latest_ctx;
	return c;
}

// This uses Brent's algorithm...

cell *skip_max_list(query *q, cell *head, pl_idx *head_ctx, pl_int max, pl_int *skip, cell *tmp)
{
	if (!head)
		return NULL;

	if (!max) {
		*skip = max;
		return head;
	}

	cell *slow;
	pl_int offset = 0;

LOOP:

	if (is_string(head)) {
		const char *src = C_STR(q, head);
		size_t len_src = C_STRLEN(q, head);
		const char *save_src = src;

		while ((max-- > 0) && (len_src > 0)) {
			size_t len = len_char_utf8(src);
			len_src -= len;
			src += len;
			*skip += 1;
		}

		unshare_cell(tmp);

		if (C_STRLEN(q, head) == (size_t)(src-save_src)) {
			make_atom(tmp, g_nil_s);
		} else if (src == save_src) {
			tmp = head;
		} else {
			make_stringn(tmp, src, C_STRLEN(q, head) - (src-save_src));
		}

		*skip += offset;
		return tmp;
	}

	// Handle ISO lists...

	slow = head;
	pl_idx slow_ctx = *head_ctx, fast_ctx = *head_ctx;
	bool done = false;
	cell *fast = term_next(q, head, &fast_ctx, &done);
	pl_int length = 1, cnt = 0;
	int power = 1;

	while (!done) {
		if ((fast == slow) && (fast_ctx == slow_ctx))
			break;

		if (length == power) {
			power *= 2;
			length = 0;
			slow = fast;
			slow_ctx = fast_ctx;
		}

		if (max == ++cnt) {
			*skip = cnt;
			*head_ctx = fast_ctx;
			return fast;
		}

		fast = term_next(q, fast, &fast_ctx, &done);

		if (is_string(slow)) {
			head = fast;
			max -= cnt + 1;
			max += 1;
			offset = cnt;
			goto LOOP;
		}

		++length;
	}

	if (done) {
		if (is_string(fast)) {
			cnt += C_STRLEN_UTF8(fast);
			*skip = cnt;
			make_atom(tmp, g_nil_s);
			return tmp;
		}

		*skip = cnt;
		*head_ctx = fast_ctx;
		return fast;
	}

	slow = fast = head;
	fast_ctx = slow_ctx = *head_ctx;

	while (length-- > 0) {
		fast = term_next(q, fast, &fast_ctx, &done);

		if (length == max)
			break;
	}

	pl_int len = 0;

	while (true) {
		if ((fast == slow) && (fast_ctx == slow_ctx))
			break;

		fast = term_next(q, fast, &fast_ctx, &done);
		slow = term_next(q, slow, &slow_ctx, &done);
		len++;
	}

	*skip = len;
	*head_ctx = slow_ctx;
	return slow;
}

bool check_list(query *q, cell *p1, pl_idx p1_ctx, bool *is_partial, pl_int *skip_)
{
	pl_int skip = 0, max = 1000000000;
	pl_idx c_ctx = p1_ctx;
	cell tmp = {0};

	cell *c = skip_max_list(q, p1, &c_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);

	if (skip_)
		*skip_ = skip;

	if (is_nil(c))
		return true;

	if (is_var(c)) {
		if (is_partial)
			*is_partial = true;
	} else {
		if (is_partial)
			*is_partial = false;
	}

	return false;
}

inline static void set_var(query *q, const cell *c, pl_idx c_ctx, cell *v, pl_idx v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);
	cell *c_attrs = is_empty(&e->c) ? e->c.attrs : NULL;
	pl_idx c_attrs_ctx = c_attrs ? e->c.attrs_ctx : 0;

	if ((c_ctx < q->st.fp) || is_managed(v))
		add_trail(q, c_ctx, c->var_nbr, c_attrs, c_attrs_ctx);

	if (c_attrs)
		q->run_hook = true;

	if (is_structure(v)) {
		make_indirect(&e->c, v, v_ctx);

		if ((c_ctx != q->st.curr_frame) && (v_ctx == q->st.curr_frame))
			q->no_tco = true;
		else if (v_ctx == q->st.fp)
			q->no_tco = true;
	} else if (is_var(v)) {
		e->c.tag = TAG_VAR;
		e->c.nbr_cells = 1;
		e->c.flags |= FLAG_VAR_REF;
		e->c.var_nbr = v->var_nbr;
		e->c.var_ctx = v_ctx;

		if ((c_ctx != q->st.curr_frame) && (v_ctx == q->st.curr_frame))
			q->no_tco = true;
	} else {
		share_cell(v);
		e->c = *v;
	}
}

void reset_var(query *q, const cell *c, pl_idx c_ctx, cell *v, pl_idx v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);

	if (is_structure(v)) {
		make_indirect(&e->c, v, v_ctx);
	} else if (is_var(v)) {
		e->c.tag = TAG_VAR;
		e->c.nbr_cells = 1;
		e->c.flags |= FLAG_VAR_REF;
		e->c.var_nbr = v->var_nbr;
		e->c.var_ctx = v_ctx;
	} else {
		share_cell(v);
		e->c = *v;
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

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, 0)) {
			if (q->cycle_error)
				return true;

			return false;
		}

		if (q->cycle_error)
			return true;

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
	if (is_bigint(p1) && is_rational(p2)) {
		mpq_t tmp;
		mp_int_init_copy(&tmp.num, &p1->val_bigint->ival);
		mp_int_init_value(&tmp.den, 1);
		bool ok = !mp_rat_compare(&p2->val_bigint->irat, &tmp);
		mp_rat_clear(&tmp);
		return ok;
	}

	if (is_bigint(p1) && is_bigint(p2))
		return !mp_int_compare(&p1->val_bigint->ival, &p2->val_bigint->ival);

	if (is_bigint(p1) && is_integer(p2))
		return !mp_int_compare_value(&p1->val_bigint->ival, p2->val_int);

	if (is_integer(p1) && is_rational(p2))
		return !mp_int_compare_value(&p2->val_bigint->ival, p1->val_int);

	if (is_integer(p1) && is_bigint(p2))
		return !mp_int_compare_value(&p2->val_bigint->ival, p1->val_int);

	if (is_integer(p2) && is_integer(p2))
		return (get_smallint(p1) == get_smallint(p2));

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

	bool skip = false;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		cell *h1 = p1 + 1, *h2 = p2 + 1;
		pl_idx h1_ctx = p1_ctx, h2_ctx = p2_ctx;

#if USE_RATIONAL_TREES
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen1 = 0, save_vgen2 = 0;
		int both = 0;

		DEREF_CHECKED(both, save_vgen1, e1, e1->vgen, h1, h1_ctx, q->vgen);
		DEREF_CHECKED(both, save_vgen2, e2, e2->vgen2, h2, h2_ctx, q->vgen);

		if (both != 2) {
			if (!unify_internal(q, h1, h1_ctx, h2, h2_ctx, depth+1))
				return false;
		}

		if (e1) e1->vgen = save_vgen1;
		if (e2) e2->vgen2 = save_vgen2;
#else
		h1 = deref(q, h1, h1_ctx);
		h1_ctx = q->latest_ctx;
		h2 = deref(q, h2, h2_ctx);
		h2_ctx = q->latest_ctx;

		if (!unify_internal(q, h1, h1_ctx, h2, h2_ctx, depth+1))
			return false;
#endif

		p1 = p1 + 1; p1 += p1->nbr_cells;
		p2 = p2 + 1; p2 += p2->nbr_cells;

#if USE_RATIONAL_TREES
		both = 0;
		DEREF_CHECKED(both, e1->save_vgen, e1, e1->vgen, p1, p1_ctx, q->vgen);
		DEREF_CHECKED(both, e2->save_vgen2, e2, e2->vgen2, p2, p2_ctx, q->vgen);

		if (q->cycle_error) {
			skip = true;
			break;
		}
#else
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
#endif
	}

#if USE_RATIONAL_TREES
	p1 = orig_p1;
	p1_ctx = orig_p1_ctx;
	p2 = orig_p2;
	p2_ctx = orig_p2_ctx;
	unsigned cnt = 0;

	while (is_iso_list(p1) && is_iso_list(p2) && !q->cycle_error) {
		p1 = p1 + 1; p1 += p1->nbr_cells;
		p2 = p2 + 1; p2 += p2->nbr_cells;

		if (is_var(p1)) {
			if (is_ref(p1))
				p1_ctx = p1->var_ctx;

			const frame *f = GET_FRAME(p1_ctx);
			slot *e1 = GET_SLOT(f, p1->var_nbr);
			e1->vgen = e1->save_vgen;
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;
		}

		if (is_var(p2)) {
			if (is_ref(p2))
				p2_ctx = p2->var_ctx;

			const frame *f = GET_FRAME(p2_ctx);
			slot *e2 = GET_SLOT(f, p2->var_nbr);
			e2->vgen2 = e2->save_vgen2;
			p2 = deref(q, p2, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (cnt > g_max_depth) {
			skip = true;
			break;
		}

		cnt++;
	}
#endif

	if (skip)
		return true;

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

#if USE_RATIONAL_TREES
		slot *e1 = NULL, *e2 = NULL;
		uint32_t save_vgen1 = 0, save_vgen2 = 0;
		int both = 0;
		DEREF_CHECKED(both, save_vgen1, e1, e1->vgen, c1, c1_ctx, q->vgen);
		DEREF_CHECKED(both, save_vgen2, e2, e2->vgen2, c2, c2_ctx, q->vgen);

		if (both != 2) {
			if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
				return false;
		}

		if (e1) e1->vgen = save_vgen1;
		if (e2) e2->vgen2 = save_vgen2;
#else
		c1 = deref(q, c1, c1_ctx);
		c1_ctx = q->latest_ctx;
		c2 = deref(q, c2, c2_ctx);
		c2_ctx = q->latest_ctx;

		if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
			return false;
#endif

#if !USE_RATIONAL_TREES
		if (q->cycle_error)
			return throw_error(q, p1, p1_ctx, "system_error", "cyclic_term");
#endif

		p1 += p1->nbr_cells;
		p2 += p2->nbr_cells;
	}

	return true;
}

static bool unify_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (depth > g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error++;
		return true;
	}

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
	}

	if (is_string(p2)) {
		if (is_iso_list(p1))
			return unify_string_to_list(q, p2, p2_ctx, p1, p1_ctx);
	}

	if (is_iso_list(p1) && is_iso_list(p2))
		return unify_lists(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	if (p1->arity || p2->arity)
		return unify_structs(q, p1, p1_ctx, p2, p2_ctx, depth+1);

	return g_disp[p1->tag].fn(q, p1, p2);
}

bool unify(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx)
{
	q->cycle_error = false;
	if (++q->vgen == 0) q->vgen = 1;
	bool ok = unify_internal(q, p1, p1_ctx, p2, p2_ctx, 0);

	if (q->cycle_error) {
		if (q->flags.occurs_check == OCCURS_CHECK_TRUE)
			return false;

		if (q->flags.occurs_check == OCCURS_CHECK_ERROR)
			return throw_error(q, p2, p2_ctx, "representation_error", "term");
	}

	return ok;
}
