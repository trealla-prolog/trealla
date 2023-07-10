#include <stdlib.h>

#include "heap.h"
#include "module.h"
#include "query.h"

static int compare_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth);

static int compare_lists(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	unsigned cnt = 0;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		if (g_tpl_interrupt)
			return -1;

		cell *h1 = p1 + 1;
		cell *h2 = p2 + 1;
		pl_idx h1_ctx = p1_ctx, h2_ctx = p2_ctx;

		slot *e1 = NULL, *e2 = NULL;
		uint64_t save_vgen1 = 0, save_vgen2 = 0;
		int both = 0;

		if (is_var(h1)) {
			if (is_ref(h1))
				h1_ctx = h1->var_ctx;

			const frame *f1 = GET_FRAME(h1_ctx);
			e1 = GET_SLOT(f1, h1->var_nbr);
			save_vgen1 = e1->vgen;

			if (e1->vgen == q->vgen)
				both++;
			else
				e1->vgen = q->vgen;
		}

		if (is_var(h2)) {
			if (is_ref(h2))
				h2_ctx = h2->var_ctx;

			const frame *f2 = GET_FRAME(h2_ctx);
			e2 = GET_SLOT(f2, h2->var_nbr);
			save_vgen2 = e2->vgen2;

			if (e2->vgen2 == q->vgen)
				both++;
			else
				e2->vgen2 = q->vgen;
		}

		if (both != 2) {
			h1 = deref(q, h1, h1_ctx);
			h1_ctx = q->latest_ctx;
			h2 = deref(q, h2, h2_ctx);
			h2_ctx = q->latest_ctx;
			int val = compare_internal(q, h1, h1_ctx, h2, h2_ctx, depth+1);
			if (val) return val;
		}

		if (e1) e1->vgen = save_vgen1;
		if (e2) e2->vgen2 = save_vgen2;

		p1 = p1 + 1; p1 += p1->nbr_cells;
		p2 = p2 + 1; p2 += p2->nbr_cells;
		both = 0;

		if (is_var(p1)) {
			if (is_ref(p1))
				p1_ctx = p1->var_ctx;

			const frame *f1 = GET_FRAME(p1_ctx);
			e1 = GET_SLOT(f1, p1->var_nbr);

			if (e1->vgen == q->vgen)
				both++;
			else
				e1->vgen = q->vgen;
		}

		if (is_var(p2)) {
			if (is_ref(p2))
				p2_ctx = p2->var_ctx;

			const frame *f2 = GET_FRAME(p2_ctx);
			e2 = GET_SLOT(f2, p2->var_nbr);

			if (e2->vgen2 == q->vgen)
				both++;
			else
				e2->vgen2 = q->vgen;
		}

		if (both == 2)
			return 0;

		if (both && (cnt > g_max_depth))		// HACK
			break;

		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
		cnt++;
	}

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
		if (g_tpl_interrupt)
			return 0;

		slot *e1 = NULL, *e2 = NULL;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;
		uint64_t save_vgen1 = 0, save_vgen2 = 0;
		cell *c1 = p1, *c2 = p2;
		int both = 0;

		if (is_var(c1)) {
			if (is_ref(c1))
				c1_ctx = c1->var_ctx;

			const frame *f1 = GET_FRAME(c1_ctx);
			e1 = GET_SLOT(f1, c1->var_nbr);
			save_vgen1 = e1->vgen;

			if (e1->vgen == q->vgen)
				both++;
			else
				e1->vgen = q->vgen;
		}

		if (is_var(c2)) {
			if (is_ref(c2))
				c2_ctx = c2->var_ctx;

			const frame *f2 = GET_FRAME(c2_ctx);
			e2 = GET_SLOT(f2, c2->var_nbr);
			save_vgen2 = e2->vgen2;

			if (e2->vgen2 == q->vgen)
				both++;
			else
				e2->vgen2 = q->vgen;
		}

		if (!both) {
			c1 = deref(q, p1, p1_ctx);
			c1_ctx = q->latest_ctx;
			c2 = deref(q, p2, p2_ctx);
			c2_ctx = q->latest_ctx;

			int val = compare_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1);
			if (e1) e1->vgen = save_vgen1;
			if (e2) e2->vgen2 = save_vgen2;
			if (val) return val;
		} else if (both == 1)
			break;

		if (e1) e1->vgen = save_vgen1;
		if (e2) e2->vgen2 = save_vgen2;

		if (both && q->cycle_error)		// HACK
			break;

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
			if (g_tpl_interrupt)
				return 0;

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

	if (map_get(q->vars, e, &v)) {
		size_t idx = (size_t)v;
		q->pl->tabs[idx].cnt++;
		return true;
	}

	map_set(q->vars, e, (void*)(size_t)q->tab_idx);

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
	LIST_HANDLER(l);

	while (is_iso_list(l)) {
		if (g_tpl_interrupt)
			return;

		cell *h = LIST_HEAD(l);
		pl_idx h_ctx = l_ctx;

		if (is_var(h)) {
			if (is_ref(h))
				h_ctx = h->var_ctx;

			const frame *f = GET_FRAME(h_ctx);
			slot *e = GET_SLOT(f, h->var_nbr);
			h = deref(q, h, h_ctx);
			h_ctx = q->latest_ctx;

			if (is_var(h))
				accum_var(q, h, h_ctx);

			if (!is_var(h) && (e->vgen != q->vgen)) {
				uint64_t save_vgen = e->vgen;
				e->vgen = q->vgen;
				collect_vars_internal(q, h, h_ctx, depth+1);
				e->vgen = save_vgen;
			}
		} else {
			collect_vars_internal(q, h, h_ctx, depth+1);
		}

		l = LIST_TAIL(l);

		if (is_var(l)) {
			if (is_ref(l))
				l_ctx = l->var_ctx;

			const frame *f = GET_FRAME(l_ctx);
			slot *e = GET_SLOT(f, l->var_nbr);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;

			if (!is_var(l) && (e->vgen == q->vgen))
				return;

			e->vgen = q->vgen;
		}
	}

	collect_vars_internal(q, l, l_ctx, depth+1);
}

static void collect_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (depth >= g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		//q->cycle_error = true;
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
		if (g_tpl_interrupt)
			return;

		cell *c = p1;
		pl_idx c_ctx = p1_ctx;

		if (is_var(c)) {
			if (is_ref(c))
				c_ctx = c->var_ctx;

			const frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			c = deref(q, c, c_ctx);
			c_ctx = q->latest_ctx;

			if (is_var(c))
				accum_var(q, c, c_ctx);

			if (!is_var(c) && (e->vgen != q->vgen)) {
				uint64_t save_vgen = e->vgen;
				e->vgen = q->vgen;
				collect_vars_internal(q, c, c_ctx, depth+1);
				e->vgen = save_vgen;
			}
		} else {
			collect_vars_internal(q, c, c_ctx, depth+1);
		}

		p1 += p1->nbr_cells;
	}
}

void collect_vars(query *q, cell *p1, pl_idx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	q->tab_idx = 0;
	ensure(q->vars = map_create(NULL, NULL, NULL));
	map_allow_dups(q->vars, false);
	collect_vars_internal(q, p1, p1_ctx, 0);
	map_destroy(q->vars);
	q->vars = NULL;
}

static bool has_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth);

static bool has_vars_lists(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	cell *l = p1;
	pl_idx l_ctx = p1_ctx;
	LIST_HANDLER(l);

	while (is_iso_list(l)) {
		if (g_tpl_interrupt)
			return false;

		cell *c = LIST_HEAD(l);
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

			if (e->vgen != q->vgen) {
				uint64_t save_vgen = e->vgen;
				e->vgen = q->vgen;

				if (has_vars_internal(q, c, c_ctx, depth+1))
					return true;

				e->vgen = save_vgen;
			}
		} else {
			if (has_vars_internal(q, c, c_ctx, depth+1))
				return true;
		}

		l = LIST_TAIL(l);

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
	}

	return has_vars_internal(q, l, l_ctx, depth+1);
}

static bool has_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (depth >= g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		//q->cycle_error = true;
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
		if (g_tpl_interrupt)
			return false;

		cell *c = p1;
		pl_idx c_ctx = p1_ctx;

		if (is_var(c)) {
			frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			c = deref(q, c, c_ctx);
			c_ctx = q->latest_ctx;

			if (is_var(c))
				return true;

			if (e->vgen != q->vgen) {
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
		if (g_tpl_interrupt)
			return false;

		cell *h = p1 + 1;
		pl_idx h_ctx = p1_ctx;

		if (is_var(h)) {
			if (is_ref(h))
				h_ctx = h->var_ctx;

			const frame *f = GET_FRAME(h_ctx);
			slot *e = GET_SLOT(f, h->var_nbr);

			if (e->vgen == q->vgen)
				return true;

			uint64_t save_vgen = e->vgen;
			e->vgen = q->vgen;
			h = deref(q, h, h_ctx);
			h_ctx = q->latest_ctx;

			if (is_cyclic_term_internal(q, h, h_ctx, depth+1))
				return true;

			e->vgen = save_vgen;
		} else {
			if (is_cyclic_term_internal(q, h, h_ctx, depth+1))
				return true;
		}

		p1 = p1 + 1; p1 += p1->nbr_cells;

		if (is_var(p1)) {
			if (is_ref(p1))
				p1_ctx = p1->var_ctx;

			const frame *f = GET_FRAME(p1_ctx);
			slot *e = GET_SLOT(f, p1->var_nbr);

			if (e->vgen == q->vgen)
				return true;

			e->vgen2 = e->vgen;
			e->vgen = q->vgen;
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;
		}
	}

	p1 = save_p1;
	p1_ctx = save_p1_ctx;

	while (is_iso_list(p1)) {
		if (g_tpl_interrupt)
			return false;

		p1 = p1 + 1; p1 += p1->nbr_cells;
		cell *c = p1;
		pl_idx c_ctx = p1_ctx;

		if (is_var(c)) {
			if (is_ref(c))
				c_ctx = c->var_ctx;

			const frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			e->vgen = e->vgen2;
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
		//q->cycle_error = true;
		return true;
	}

	if (!is_structure(p1))
		return false;

	if (is_iso_list(p1))
		return is_cyclic_term_lists(q, p1, p1_ctx, depth);

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		if (g_tpl_interrupt)
			return false;

		cell *c = p1;
		pl_idx c_ctx = p1_ctx;

		if (is_var(c)) {
			if (is_ref(c))
				c_ctx = c->var_ctx;

			const frame *f = GET_FRAME(c_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);

			if (e->vgen == q->vgen)
				return true;

			uint64_t save_vgen = e->vgen;
			e->vgen = q->vgen;
			c = deref(q, c, c_ctx);
			c_ctx = q->latest_ctx;

			if (is_cyclic_term_internal(q, c, c_ctx, depth+1))
				return true;

			e->vgen = save_vgen;
		} else {
			if (is_cyclic_term_internal(q, c, c_ctx, depth+1))
				return true;
		}

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

#if 0
	pl_int offset = 0;
#endif

LOOP:

#if 0
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
#endif

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

		if (is_string(fast)) {
			head = fast;
			max -= cnt + 1;
			max += 1;
#if 0
			offset = cnt;
#endif
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

static void make_new_var(query *q, cell *tmp, unsigned var_nbr, pl_idx ctx)
{
	make_var(tmp, g_anon_s, create_vars(q, 1));
	cell v;
	make_var(&v, g_anon_s, var_nbr);
	set_var(q, tmp, q->st.curr_frame, &v, ctx);
}

static void set_new_var(query *q, cell *tmp, cell *v, pl_idx ctx)
{
	make_var(tmp, g_anon_s, create_vars(q, 1));
	set_var(q, tmp, q->st.curr_frame, v, ctx);
}

bool fn_sys_undo_trail_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	q->in_hook = true;

	if (q->undo_hi_tp <= q->undo_lo_tp) {
		set_var(q, p1, p1_ctx, make_nil(), q->st.curr_frame);
		return true;
	}

	q->save_e = malloc(sizeof(slot)*(q->undo_hi_tp - q->undo_lo_tp));
	check_error(q->save_e);
	bool first = true;

	// Unbind our vars

	for (pl_idx i = q->undo_lo_tp, j = 0; i < q->undo_hi_tp; i++, j++) {
		const trail *tr = q->trails + i;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		//printf("*** unbind [%u:%u] hi_tp=%u, ctx=%u, var=%u\n", j, i, q->undo_hi_tp, tr->var_ctx, tr->var_nbr);
		q->save_e[j] = *e;

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

		e->c.tag = TAG_EMPTY;
		e->c.attrs = tr->attrs;
		e->c.attrs_ctx = tr->attrs_ctx;
		//DUMP_TERM("$undo2", tr->attrs, tr->attrs_ctx, 0);
	}

	cell *tmp = end_list(q);
	check_heap_error(tmp);
	//DUMP_TERM("$undo3 tmp", tmp, q->st.curr_frame, 0);
	set_var(q, p1, p1_ctx, tmp, q->st.curr_frame);
	return true;
}

bool fn_sys_redo_trail_0(query * q)
{
	for (pl_idx i = q->undo_lo_tp, j = 0; i < q->undo_hi_tp; i++, j++) {
		const trail *tr = q->trails + i;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		//printf("*** rebind [%u:%u] hi_tp=%u, ctx=%u, var=%u\n", j, i, q->undo_hi_tp, tr->var_ctx, tr->var_nbr);
		*e = q->save_e[j];
	}

	free(q->save_e);
	q->save_e = NULL;
	q->in_hook = false;
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
	static builtins *s_fn_ptr = NULL;

	if (!s_fn_ptr)
		s_fn_ptr = get_fn_ptr(fn_iso_true_0);

	tmp[0].fn_ptr = s_fn_ptr;

	tmp[1].tag = TAG_INTERNED;
	tmp[1].nbr_cells = 1;
	tmp[1].arity = 0;
	tmp[1].flags = 0;
	tmp[1].val_off = g_post_unify_hook_s;
	tmp[1].match = search_predicate(q->pl->user_m, tmp+1, NULL);

	if (!tmp[1].match)
		return throw_error(q, tmp+1, q->st.curr_frame, "existence_error", "procedure");

	if (is_builtin)
		make_call(q, tmp+2);
	else
		make_call_return(q, tmp+2, q->st.curr_cell);

	q->st.curr_cell = tmp;
	return true;
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
	unsigned cnt = 0;

	while (is_iso_list(p1) && is_iso_list(p2)) {
		if (g_tpl_interrupt)
			return false;

		cell *h1 = p1 + 1;
		cell *h2 = p2 + 1;
		pl_idx h1_ctx = p1_ctx, h2_ctx = p2_ctx;
		slot *e1 = NULL, *e2 = NULL;
		uint64_t save_vgen1 = 0, save_vgen2 = 0;
		int both = 0;

		if (is_var(h1)) {
			if (is_ref(h1))
				h1_ctx = h1->var_ctx;

			const frame *f1 = GET_FRAME(h1_ctx);
			e1 = GET_SLOT(f1, h1->var_nbr);
			save_vgen1 = e1->vgen;

			if (e1->vgen == q->vgen)
				both++;
			else
				e1->vgen = q->vgen;
		}

		if (is_var(h2)) {
			if (is_ref(h2))
				h2_ctx = h2->var_ctx;

			const frame *f2 = GET_FRAME(h2_ctx);
			e2 = GET_SLOT(f2, h2->var_nbr);
			save_vgen2 = e2->vgen2;

			if (e2->vgen2 == q->vgen)
				both++;
			else
				e2->vgen2 = q->vgen;
		}

		if (both != 2) {
			h1 = deref(q, h1, h1_ctx);
			h1_ctx = q->latest_ctx;
			h2 = deref(q, h2, h2_ctx);
			h2_ctx = q->latest_ctx;

			if (!unify_internal(q, h1, h1_ctx, h2, h2_ctx, depth+1))
				return false;
		}

		if (e1) e1->vgen = save_vgen1;
		if (e2) e2->vgen2 = save_vgen2;

		p1 = p1 + 1; p1 += p1->nbr_cells;
		p2 = p2 + 1; p2 += p2->nbr_cells;
		both = 0;

		if (is_var(p1)) {
			if (is_ref(p1))
				p1_ctx = p1->var_ctx;

			const frame *f1 = GET_FRAME(p1_ctx);
			e1 = GET_SLOT(f1, p1->var_nbr);

			if (e1->vgen == q->vgen)
				both++;
			else
				e1->vgen = q->vgen;
		}

		if (is_var(p2)) {
			if (is_ref(p2))
				p2_ctx = p2->var_ctx;

			const frame *f2 = GET_FRAME(p2_ctx);
			e2 = GET_SLOT(f2, p2->var_nbr);

			if (e2->vgen2 == q->vgen)
				both++;
			else
				e2->vgen2 = q->vgen;
		}

		if (both == 2)
			return true;

		if (both && (cnt > g_max_depth))		// HACK
			break;

		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
		cnt++;
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
		if (g_tpl_interrupt)
			return false;

		slot *e1 = NULL, *e2 = NULL;
		uint64_t save_vgen1 = 0, save_vgen2 = 0;
		pl_idx c1_ctx = p1_ctx, c2_ctx = p2_ctx;
		cell *c1 = p1, *c2 = p2;
		int both = 0;

		if (is_var(c1)) {
			if (is_ref(c1))
				c1_ctx = c1->var_ctx;

			const frame *f1 = GET_FRAME(c1_ctx);
			e1 = GET_SLOT(f1, c1->var_nbr);
			save_vgen1 = e1->vgen;

			if (e1->vgen == q->vgen)
				both++;
			else
				e1->vgen = q->vgen;
		}

		if (is_var(c2)) {
			if (is_ref(c2))
				c2_ctx = c2->var_ctx;

			const frame *f2 = GET_FRAME(c2_ctx);
			e2 = GET_SLOT(f2, c2->var_nbr);
			save_vgen2 = e2->vgen2;

			if (e2->vgen2 == q->vgen)
				both++;
			else
				e2->vgen2 = q->vgen;
		}

		if (both != 2) {
			c1 = deref(q, p1, p1_ctx);
			c1_ctx = q->latest_ctx;
			c2 = deref(q, p2, p2_ctx);
			c2_ctx = q->latest_ctx;

			if (!unify_internal(q, c1, c1_ctx, c2, c2_ctx, depth+1))
				return false;
		}

		if (both && q->cycle_error)		// HACK
			break;

		if (e1) e1->vgen = save_vgen1;
		if (e2) e2->vgen2 = save_vgen2;

		p1 += p1->nbr_cells;
		p2 += p2->nbr_cells;
	}

	return true;
}

static bool unify_internal(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, unsigned depth)
{
	if (depth > g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
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

	q->check_unique = true;

	if (is_string(p1) && is_string(p2))
		return unify_cstrings(q, p1, p2);

	if (is_string(p1) && is_iso_list(p2))
		return unify_string_to_list(q, p1, p1_ctx, p2, p2_ctx);

	if (is_string(p2) && is_iso_list(p1))
		return unify_string_to_list(q, p2, p2_ctx, p1, p1_ctx);

	if (p1->arity || p2->arity) {
		if (is_iso_list(p1) && is_iso_list(p2))
			return unify_lists(q, p1, p1_ctx, p2, p2_ctx, depth+1);
		else
			return unify_structs(q, p1, p1_ctx, p2, p2_ctx, depth+1);
	}

	return g_disp[p1->tag].fn(q, p1, p2);
}

bool unify(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx)
{
	q->cycle_error = false;
	if (++q->vgen == 0) q->vgen = 1;
	return unify_internal(q, p1, p1_ctx, p2, p2_ctx, 0);
}
