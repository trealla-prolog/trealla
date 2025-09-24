#include <stdlib.h>

#include "query.h"

typedef struct { lnode hdr; cell *c; pl_ctx c_ctx; slot *e; uint32_t save_vgen; } snode;

static bool accum_var(query *q, const cell *c, pl_ctx c_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	const slot *e = get_slot(q, f, c->var_num);
	const void *v;

	if (sl_get(q->vars, e, &v)) {
		size_t idx = (size_t)v;
		q->pl->tabs[idx].cnt++;
		return true;
	}

	sl_app(q->vars, e, (void*)(size_t)q->tab_idx);

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

	q->pl->tabs[q->tab_idx].val_off = c->val_off;
	q->pl->tabs[q->tab_idx].var_num = c->var_num;
	q->pl->tabs[q->tab_idx].ctx = c_ctx;
	q->pl->tabs[q->tab_idx].is_anon = is_anon(c) ? true : false;
	q->pl->tabs[q->tab_idx].cnt = 1;
	q->tab_idx++;
	return false;
}

static void collect_vars_internal(query *q, cell *p1, pl_ctx p1_ctx, unsigned depth);

static void collect_vars_lists(query *q, cell *p1, pl_ctx p1_ctx, unsigned depth)
{
	cell *l = p1;
	pl_ctx l_ctx = p1_ctx;
	bool any1 = false, any2 = false;

	while (is_iso_list(l)) {
		cell *h = l + 1;
		pl_ctx h_ctx = l_ctx;
		slot *e = NULL;
		uint32_t save_vgen;
		int both = 0;

		DEREF_VAR(any1, both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);

		if (!both && is_var(h) && !(h->flags & FLAG_VAR_CYCLIC))
			accum_var(q, h, h_ctx);
		else if (!both)
			collect_vars_internal(q, h, h_ctx, depth+1);

		if (e) e->vgen = save_vgen;
		l = l + 1; l += l->num_cells;
		e = NULL;
		both = 0;

		DEREF_VAR(any2, both, save_vgen, e, e->vgen, l, l_ctx, q->vgen);

		if (both)
			return;
	}

	if (any2) {
		l = p1;
		l_ctx = p1_ctx;

		while (is_iso_list(l)) {
			l = l + 1; l += l->num_cells;
			cell *c = l;
			pl_ctx c_ctx = l_ctx;
			RESTORE_VAR(c, c_ctx, l, l_ctx, q->vgen);
		}
	}

	collect_vars_internal(q, l, l_ctx, depth+1);
}

static void collect_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (is_var(p1)) {
		if (!(p1->flags & FLAG_VAR_CYCLIC))
			accum_var(q, p1, p1_ctx);

		return;
	}

	if (!is_compound(p1) || is_ground(p1))
		return;

	if (is_iso_list(p1)) {
		collect_vars_lists(q, p1, p1_ctx, depth+1);
		return;
	}

	// Transform recursion into stack iteration...

	list stack = {0};
	snode *n = malloc(sizeof(snode));
	n->c = p1;
	n->c_ctx = p1_ctx;
	list_push_back(&stack, n);

	while ((n = (snode*)list_pop_front(&stack)) != NULL) {
		cell *p1 = n->c;
		pl_idx p1_ctx = n->c_ctx;
		free(n);

		if (!is_compound(p1) || is_iso_list(p1)) {
			collect_vars_internal(q, p1, p1_ctx, depth+1);
			continue;
		}

		snode *last = NULL;
		bool any = false;
		unsigned arity = p1->arity;
		p1++;

		while (arity--) {
			cell *c = p1;
			pl_idx c_ctx = p1_ctx;
			slot *e = NULL;
			uint32_t save_vgen;
			int both = 0;

			DEREF_VAR(any, both, save_vgen, e, e->vgen, c, c_ctx, q->vgen);

			if (!both || is_var(c)) {
				n = malloc(sizeof(snode));
				n->c = c;
				n->c_ctx = c_ctx;

				if (!last)
					list_push_front(&stack, n);
				else
					list_insert_after(&stack, last, n);

				last = n;
			} else
				if (e) e->vgen = save_vgen;

			p1 += p1->num_cells;
		}
	}
}

void collect_vars(query *q, cell *p1, pl_ctx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	q->tab_idx = 0;
	ensure(q->vars = sl_create(NULL, NULL, NULL));
	collect_vars_internal(q, p1, p1_ctx, 0);
	sl_destroy(q->vars);
	q->vars = NULL;
}

static bool has_vars_internal(query *q, cell *p1, pl_ctx p1_ctx, unsigned depth);

static bool has_vars_lists(query *q, cell *p1, pl_ctx p1_ctx, unsigned depth)
{
	cell *l = p1;
	pl_ctx l_ctx = p1_ctx;
	bool any1 = false, any2 = false;

	while (is_iso_list(l)) {
		cell *h = l + 1;
		pl_ctx h_ctx = l_ctx;
		slot *e = NULL;
		uint32_t save_vgen;
		int both = 0;

		DEREF_VAR(any1, both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);

		if (!both)
			if (has_vars_internal(q, h, h_ctx, depth+1))
				return true;

		if (e) e->vgen = save_vgen;
		l = l + 1; l += l->num_cells;
		e = NULL;
		both = 0;

		DEREF_VAR(any2, both, save_vgen, e, e->vgen, l, l_ctx, q->vgen);

		if (both)
			return false;
	}

	if (any2) {
		l = p1;
		l_ctx = p1_ctx;

		while (is_iso_list(l)) {
			l = l + 1; l += l->num_cells;
			cell *c = l;
			pl_ctx c_ctx = l_ctx;
			RESTORE_VAR(c, c_ctx, l, l_ctx, q->vgen);
		}
	}

	return has_vars_internal(q, l, l_ctx, depth+1);
}

static bool has_vars_internal(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (is_var(p1))
		return true;

	if (!is_compound(p1) || is_ground(p1))
		return false;

	if (is_iso_list(p1))
		return has_vars_lists(q, p1, p1_ctx, depth+1);

	// Transform recursion into stack iteration...

	list stack = {0};
	snode *n = malloc(sizeof(snode));
	n->c = p1;
	n->c_ctx = p1_ctx;
	list_push_back(&stack, n);

	while ((n = (snode*)list_pop_front(&stack)) != NULL) {
		cell *p1 = n->c;
		pl_idx p1_ctx = n->c_ctx;
		free(n);

		if (!is_compound(p1) || is_iso_list(p1)) {
			if (has_vars_internal(q, p1, p1_ctx, depth+1)) {
				while ((n = (snode*)list_pop_front(&stack)) != NULL)
					free(n);

				return true;
			}
		}

		bool any = false;
		unsigned arity = p1->arity;
		p1++;

		while (arity--) {
			cell *c = p1;
			pl_idx c_ctx = p1_ctx;
			slot *e = NULL;
			uint32_t save_vgen = 0;
			int both = 0;

			DEREF_VAR(any, both, save_vgen, e, e->vgen, c, c_ctx, q->vgen);

			if (is_var(c)) {
				while ((n = (snode*)list_pop_front(&stack)) != NULL)
					free(n);

				return true;
			}

			if (!both && is_compound(c) && !is_ground(c)) {
				n = malloc(sizeof(snode));
				n->c = c;
				n->c_ctx = c_ctx;
				list_push_back(&stack, n);
			} else if (e)
				e->vgen = save_vgen;

			p1 += p1->num_cells;
		}
	}

	return false;
}

bool has_vars(query *q, cell *p1, pl_ctx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	return has_vars_internal(q, p1, p1_ctx, 0);
}

static bool is_cyclic_term_internal(query *q, cell *p1, pl_ctx p1_ctx, unsigned depth);

static bool is_cyclic_term_lists(query *q, cell *p1, pl_ctx p1_ctx, unsigned depth)
{
	cell *l = p1;
	pl_ctx l_ctx = p1_ctx;
	bool any1 = false, any2 = false;

	while (is_iso_list(l)) {
		cell *h = l + 1;
		pl_ctx h_ctx = l_ctx;
		slot *e = NULL;
		uint32_t save_vgen;
		int both = 0;

		DEREF_VAR(any1, both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);

		if (both)
			return true;

		if (is_cyclic_term_internal(q, h, h_ctx, depth+1))
			return true;

		if (e) e->vgen = save_vgen;
		l = l + 1; l += l->num_cells;
		e = NULL;
		both = 0;

		DEREF_VAR(any2, both, save_vgen, e, e->vgen, l, l_ctx, q->vgen);

		if (both)
			return true;
	}

	if (any2) {
		l = p1;
		l_ctx = p1_ctx;

		while (is_iso_list(l)) {
			l = l + 1; l += l->num_cells;
			cell *c = l;
			pl_ctx c_ctx = l_ctx;
			RESTORE_VAR(c, c_ctx, l, l_ctx, q->vgen);
		}
	}

	return is_cyclic_term_internal(q, l, l_ctx, depth+1);
}

static bool is_cyclic_term_internal(query *q, cell *p1, pl_ctx p1_ctx, unsigned depth)
{
	if (!is_compound(p1) || is_ground(p1))
		return false;

	if (is_iso_list(p1))
		return is_cyclic_term_lists(q, p1, p1_ctx, depth);

	bool any = false;
	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = p1;
		pl_ctx c_ctx = p1_ctx;
		slot *e = NULL;
		uint32_t save_vgen;
		int both = 0;

		DEREF_VAR(any, both, save_vgen, e, e->vgen, c, c_ctx, q->vgen);

		if (both)
			return true;

		if (is_cyclic_term_internal(q, c, c_ctx, depth+1))
			return true;

		if (e) e->vgen = save_vgen;
		p1 += p1->num_cells;
	}

	return false;
}

bool is_cyclic_term(query *q, cell *p1, pl_ctx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	return is_cyclic_term_internal(q, p1, p1_ctx, 0);
}

bool is_acyclic_term(query *q, cell *p1, pl_ctx p1_ctx)
{
	return !is_cyclic_term(q, p1, p1_ctx);
}

inline static cell *term_next(query *q, cell *c, pl_ctx *c_ctx, bool *done)
{
	if (!is_iso_list(c)) {
		*done = true;
		return c;
	}

	c += 1;
	c += c->num_cells;
	c = deref(q, c, *c_ctx);
	*c_ctx = q->latest_ctx;
	return c;
}

// This uses Brent's algorithm...

cell *skip_max_list(query *q, cell *head, pl_ctx *head_ctx, pl_int max, pl_int *skip, cell *tmp)
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
	pl_ctx slow_ctx = *head_ctx, fast_ctx = *head_ctx;
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

bool check_list(query *q, cell *p1, pl_ctx p1_ctx, bool *is_partial, pl_int *skip_)
{
	pl_int skip = 0, max = 1000000000;
	pl_ctx c_ctx = p1_ctx;
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

