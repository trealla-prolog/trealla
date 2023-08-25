#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heap.h"
#include "prolog.h"
#include "query.h"

struct heap_save {
	cell *heap;
	pl_idx size, hp;
};

#define push_tmp_heap(q) 								\
	struct heap_save _s;								\
	_s.heap = q->tmp_heap;								\
	_s.size = q->tmph_size;								\
	_s.hp = q->tmphp;									\
	q->tmp_heap = NULL;									\
	q->tmphp = 0;										\
	if (!init_tmp_heap(q)) return NULL;

#define pop_tmp_heap(q)									\
	free(q->tmp_heap);									\
	q->tmp_heap = _s.heap;								\
	q->tmph_size = _s.size;								\
	q->tmphp = _s.hp;

static int accum_slot(const query *q, pl_idx slot_nbr, unsigned var_nbr)
{
	const void *vnbr;

	if (sl_get(q->vars, (void*)(size_t)slot_nbr, &vnbr))
		return (unsigned)(size_t)vnbr;

	sl_set(q->vars, (void*)(size_t)slot_nbr, (void*)(size_t)var_nbr);
	return -1;
}

size_t alloc_grow(void **addr, size_t elem_size, size_t min_elements, size_t max_elements, bool zeroit)
{
	//assert(min_elements <= max_elements);

	if (min_elements > max_elements)
		max_elements = min_elements;

	size_t elements = max_elements;
	void *mem;

	do {
		mem = realloc(*addr, elem_size * elements);
		if (mem) break;
		elements = min_elements + (elements - min_elements) / 2;
		//message("memory pressure reduce %lu to %lu", max_elements, elements);
	}
	 while (elements > min_elements);

	if (!mem)
		return 0;

	if (zeroit)
		memset(mem + (min_elements * elem_size), 0, (elements - min_elements) * elem_size);

	*addr = mem;
	return elements;
}

// The tmp heap is used for temporary allocations (a scratch-pad)
// for work in progress. As such it can survive a realloc() call.

cell *init_tmp_heap(query *q)
{
	if (q->tmp_heap && (q->tmph_size > 1000)) {
		free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = 1000;
	}

	if (!q->tmp_heap) {
		q->tmp_heap = malloc(q->tmph_size * sizeof(cell));
		if (!q->tmp_heap) return NULL;
		*q->tmp_heap = (cell){0};
	}

	q->tmphp = 0;
	return q->tmp_heap;
}

cell *alloc_on_tmp(query *q, unsigned nbr_cells)
{
	if (((uint64_t)q->tmphp + nbr_cells) > UINT32_MAX)
		return NULL;

	pl_idx new_size = q->tmphp + nbr_cells;

	if (new_size >= q->tmph_size) {
		size_t elements = alloc_grow((void**)&q->tmp_heap, sizeof(cell), new_size, new_size*2, true);
		if (!elements) return NULL;
		q->tmph_size = elements;
	}

	cell *c = q->tmp_heap + q->tmphp;
	q->tmphp = new_size;
	return c;
}

// The heap is used for long-life allocations and a realloc() can't be
// done as it will invalidate existing pointers. Build any compounds
// first on the tmp heap, then allocate in one go here and copy in.
// When more space is need allocate a new page and keep them in the
// page list. Backtracking will garbage collect and free as needed.

cell *alloc_on_heap(query *q, unsigned nbr_cells)
{
	if (((uint64_t)q->st.hp + nbr_cells) > UINT32_MAX)
		return NULL;

	if (!q->pages) {
		page *a = calloc(1, sizeof(page));
		if (!a) return NULL;
		a->next = q->pages;
		unsigned n = MAX_OF(q->h_size, nbr_cells);
		a->heap = calloc(a->h_size=n, sizeof(cell));
		if (!a->heap) { free(a); return NULL; }
		a->nbr = q->st.pp++;
		q->pages = a;
	}

	if ((q->st.hp + nbr_cells) >= q->pages->h_size) {
		page *a = calloc(1, sizeof(page));
		if (!a) return NULL;
		a->next = q->pages;
		unsigned n = MAX_OF(q->h_size, nbr_cells);
		a->heap = calloc(a->h_size=n, sizeof(cell));
		if (!a->heap) { free(a); return NULL; }
		a->nbr = q->st.pp++;
		q->pages = a;
		q->st.hp = 0;
	}

	cell *c = q->pages->heap + q->st.hp;
	q->st.hp += nbr_cells;
	q->pages->hp = q->st.hp;

	if (q->pages->hp > q->pages->max_hp_used)
		q->pages->max_hp_used = q->pages->hp;

	return c;
}

void trim_heap(query *q)
{
	// q->pages is a push-down stack and points to the
	// most recent page of heap allocations...

	for (page *a = q->pages; a;) {
		if (a->nbr < q->st.pp)
			break;

		cell *c = a->heap;

		for (pl_idx i = 0; i < a->hp; i++, c++)
			unshare_cell(c);

		page *save = a;
		q->pages = a = a->next;
		free(save->heap);
		free(save);
	}

	const page *a = q->pages;

	for (pl_idx i = q->st.hp; a && (i < a->hp); i++) {
		cell *c = a->heap + i;
		unshare_cell(c);
		init_cell(c);
	}
}

#define deep_copy(c) \
	(!q->noderef || (is_ref(c) && (c->var_ctx <= q->st.curr_frame) && !is_anon(c)))

static cell *deep_clone2_to_tmp(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (depth >= g_max_depth) {
		printf("*** Stack limit %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return NULL;
	}

	pl_idx save_idx = tmp_heap_used(q);
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	// Convert vars to refs...

	if (is_var(tmp)) {
		if (!is_ref(tmp)) {
			tmp->flags |= FLAG_VAR_REF;
			tmp->var_ctx = p1_ctx;
		}
	}

	if (!is_structure(p1) || is_string(p1))
		return tmp;

	if (is_iso_list(p1)) {
		bool cyclic = false;

		while (is_iso_list(p1)) {
			cell *h = p1 + 1;
			pl_idx h_ctx = p1_ctx;

			if (is_var(h) && deep_copy(h)) {
				if (is_ref(h))
					h_ctx = h->var_ctx;

				const frame *f = GET_FRAME(h_ctx);
				slot *e = GET_SLOT(f, h->var_nbr);
				uint64_t save_vgen = e->vgen;

				if (e->vgen == q->vgen) {
					cell *rec = deep_clone2_to_tmp(q, h, h_ctx, depth+1);
					if (!rec) return NULL;
					q->cycle_error = true;
				} else {
					e->vgen = q->vgen;
					h = deref(q, h, h_ctx);
					h_ctx = q->latest_ctx;
					cell *rec = deep_clone2_to_tmp(q, h, h_ctx, depth+1);
					if (!rec) return NULL;
					e->vgen = save_vgen;
				}
			} else {
				cell *rec = deep_clone2_to_tmp(q, h, h_ctx, depth+1);
				if (!rec) return NULL;
			}

			p1 = p1 + 1; p1 += p1->nbr_cells;
			cell *t = p1;
			pl_idx t_ctx = p1_ctx;

			if (is_var(t) && deep_copy(t)) {
				if (is_ref(t))
					t_ctx = t->var_ctx;

				const frame *f = GET_FRAME(t_ctx);
				slot *e = GET_SLOT(f, t->var_nbr);

				if (e->vgen == q->vgen) {
					cell *rec = deep_clone2_to_tmp(q, t, t_ctx, depth+1);
					if (!rec) return NULL;
					cyclic = true;
					q->cycle_error = true;
					break;
				}

				e->vgen = q->vgen;
				p1 = deref(q, t, t_ctx);
				p1_ctx = q->latest_ctx;
			}

			if (is_iso_list(p1)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				copy_cells(tmp, p1, 1);
			}
		}

		if (!cyclic) {
			cell *rec = deep_clone2_to_tmp(q, p1, p1_ctx, depth+1);
			if (!rec) return NULL;
		}

		tmp = get_tmp_heap(q, save_idx);
		tmp->nbr_cells = tmp_heap_used(q) - save_idx;
		return tmp;
	}

	unsigned arity = p1->arity;
	const frame *f = GET_FRAME(p1_ctx);
	p1++;

	while (arity--) {
		cell *c = p1;
		pl_idx c_ctx = p1_ctx;

		if (is_var(c) && deep_copy(c)) {
			if (is_ref(c))
				c_ctx = c->var_ctx;

			slot *e = GET_SLOT(f, c->var_nbr);
			uint64_t save_vgen = e->vgen;

			if (e->vgen == q->vgen) {
				cell *rec = deep_clone2_to_tmp(q, c, c_ctx, depth+1);
				if (!rec) return NULL;
			} else {
				e->vgen = q->vgen;
				c = deref(q, c, c_ctx);
				c_ctx = q->latest_ctx;
				cell *rec = deep_clone2_to_tmp(q, c, c_ctx, depth+1);
				if (!rec) return NULL;
				e->vgen = save_vgen;
			}
		} else {
			cell *rec = deep_clone2_to_tmp(q, c, c_ctx, depth+1);
			if (!rec) return NULL;
		}

		p1 += p1->nbr_cells;
	}

	tmp = get_tmp_heap(q, save_idx);
	tmp->nbr_cells = tmp_heap_used(q) - save_idx;
	return tmp;
}

cell *deep_clone_to_tmp(query *q, cell *p1, pl_idx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	cell *rec = deep_clone2_to_tmp(q, p1, p1_ctx, 0);
	if (!rec) return NULL;
	return rec;
}

cell *deep_clone_to_heap(query *q, cell *p1, pl_idx p1_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	p1 = deep_clone_to_tmp(q, p1, p1_ctx);
	if (!p1) return p1;
	cell *tmp = alloc_on_heap(q, p1->nbr_cells);
	if (!tmp) return NULL;
	safe_copy_cells(tmp, p1, p1->nbr_cells);
	return tmp;
}

cell *append_to_tmp(query *q, cell *p1)
{
	cell *tmp = alloc_on_tmp(q, p1->nbr_cells);
	if (!tmp) return NULL;
	cell *src = p1, *dst = tmp;

	for (pl_idx i = 0; i < p1->nbr_cells; i++, dst++, src++) {
		*dst = *src;

		if (!is_var(src) || is_ref(src))
			continue;

		dst->flags |= FLAG_VAR_REF;
		dst->var_ctx = q->st.curr_frame;
	}

	return tmp;
}

cell *clone_to_tmp(query *q, cell *p1)
{
	return append_to_tmp(q, p1);
}

cell *prepare_call(query *q, bool prefix, cell *p1, pl_idx p1_ctx, unsigned extras)
{
	unsigned nbr_cells = (prefix ? PREFIX_LEN : NOPREFIX_LEN) + p1->nbr_cells + extras;
	cell *tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;

	if (prefix) {
		// Needed for follow() to work
		tmp->tag = TAG_INTERNED;
		tmp->arity = 0;
		tmp->nbr_cells = 1;
		tmp->flags = FLAG_BUILTIN;
		tmp->val_off = g_true_s;
		static builtins *s_fn_ptr = NULL;

		if (!s_fn_ptr)
			s_fn_ptr = get_fn_ptr(fn_iso_true_0);

		tmp->fn_ptr = s_fn_ptr;
	}

	cell *src = p1, *dst = tmp + (prefix ? PREFIX_LEN : NOPREFIX_LEN);

	for (pl_idx i = 0; i < p1->nbr_cells; i++, dst++) {
		*dst = *src++;
		share_cell(dst);

		if (is_var(dst) && !is_ref(dst)) {
			dst->flags |= FLAG_VAR_REF;
			dst->var_ctx = p1_ctx;
		}
	}

	return tmp;
}

static bool copy_vars(query *q, cell *tmp, bool copy_attrs, const cell *from, pl_idx from_ctx, const cell *to, pl_idx to_ctx)
{
	unsigned nbr_cells = tmp->nbr_cells;

	for (unsigned i = 0; i < nbr_cells; i++, tmp++) {
		if (!is_ref(tmp))
			continue;

		const frame *f = GET_FRAME(tmp->var_ctx);
		const slot *e = GET_SLOT(f, tmp->var_nbr);
		const pl_idx slot_nbr = f->base + tmp->var_nbr;
		int var_nbr;

		if ((var_nbr = accum_slot(q, slot_nbr, q->varno)) == -1)
			var_nbr = q->varno++;

		if (!q->tab_idx) {
			q->tab0_varno = var_nbr;
			q->tab_idx++;
		}

		tmp->flags |= FLAG_VAR_FRESH;

		if (from && (tmp->var_nbr == from->var_nbr) && (tmp->var_ctx == from_ctx)) {
			tmp->var_nbr = to->var_nbr;
			tmp->var_ctx = to_ctx;
		} else {
			tmp->var_nbr = var_nbr;
			tmp->var_ctx = q->st.curr_frame;

			if (copy_attrs && e->c.attrs) {
				push_tmp_heap(q);
				cell *tmp2 = deep_copy_to_heap_with_replacement(q, e->c.attrs, e->c.attrs_ctx, false, NULL, 0, NULL, 0);
				pop_tmp_heap(q);
				check_heap_error(tmp2);
				tmp->tmp_attrs = tmp2;
			}
		}
	}

	return true;
}

static cell *deep_copy_to_tmp_with_replacement(query *q, cell *p1, pl_idx p1_ctx, bool copy_attrs, cell *from, pl_idx from_ctx, cell *to, pl_idx to_ctx)
{
	cell *c = deref(q, p1, p1_ctx);
	pl_idx c_ctx = q->latest_ctx;
	void *save = q->vars;

	if (!q->vars)
		q->vars = sl_create(NULL, NULL, NULL);

	if (!q->vars) {
		q->vars = save;
		return NULL;
	}

	const frame *f = GET_CURR_FRAME();

	if (!save) {
		q->varno = f->actual_slots;
		q->tab_idx = 0;

		if (is_var(p1)) {
			const frame *f = GET_FRAME(p1_ctx);
			slot *e = GET_SLOT(f, p1->var_nbr);
			const pl_idx slot_nbr = f->base + p1->var_nbr;
			e->vgen = q->vgen+1; // +1 because that is what deep_clone_to_tmp() will do
			if (e->vgen == 0) e->vgen++;
			q->tab0_varno = q->varno;
			q->tab_idx++;
			sl_set(q->vars, (void*)(size_t)slot_nbr, (void*)(size_t)q->varno);
			q->varno++;
		}
	}

	cell *tmp = deep_clone_to_tmp(q, c, c_ctx);
	if (!tmp) {
		if (!save)
			sl_destroy(q->vars);

		q->vars = save;
		return NULL;
	}

	if (!copy_vars(q, tmp, copy_attrs, from, from_ctx, to, to_ctx)) {
		if (!save)
			sl_destroy(q->vars);

		q->vars = save;
		return NULL;
	}

	if (!save)
		sl_destroy(q->vars);

	q->vars = save;
	int cnt = q->varno - f->actual_slots;

	if (cnt) {
		if (!create_vars(q, cnt)) {
			throw_error(q, c, c_ctx, "resource_error", "stack");
			return NULL;
		}
	}

	if (is_var(p1)) {
		cell tmp2;
		tmp2 = *p1;
		tmp2.var_nbr = q->tab0_varno;
		unify(q, &tmp2, q->st.curr_frame, tmp, q->st.curr_frame);
	}

	if (!copy_attrs)
		return tmp;

	c = tmp;

	for (pl_idx i = 0; i < tmp->nbr_cells; i++, c++) {
		if (is_var(c) && is_fresh(c) && c->tmp_attrs) {
			const frame *f = GET_FRAME(c->var_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			e->c.attrs = c->tmp_attrs;
			e->c.attrs_ctx = c->var_ctx;
		}
	}

	return tmp;
}

cell *deep_copy_to_tmp(query *q, cell *p1, pl_idx p1_ctx, bool copy_attrs)
{
	return deep_copy_to_tmp_with_replacement(q, p1, p1_ctx, copy_attrs, NULL, 0, NULL, 0);
}

cell *deep_copy_to_heap(query *q, cell *p1, pl_idx p1_ctx, bool copy_attrs)
{
	if (!init_tmp_heap(q))
		return NULL;

	cell *tmp = deep_copy_to_tmp_with_replacement(q, p1, p1_ctx, copy_attrs, NULL, 0, NULL, 0);
	if (!tmp) return tmp;
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	if (!tmp2) return NULL;
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return tmp2;
}

cell *deep_copy_to_heap_with_replacement(query *q, cell *p1, pl_idx p1_ctx, bool copy_attrs, cell *from, pl_idx from_ctx, cell *to, pl_idx to_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	cell *tmp = deep_copy_to_tmp_with_replacement(q, p1, p1_ctx, copy_attrs, from, from_ctx, to, to_ctx);
	if (!tmp) return tmp;
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	if (!tmp2) return NULL;
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return tmp2;
}

cell *alloc_on_queuen(query *q, unsigned qnbr, const cell *c)
{
	if (!q->queue[qnbr]) {
		q->queue[qnbr] = malloc(sizeof(cell)*q->q_size[qnbr]);
		if (!q->queue[qnbr]) return NULL;
	}

	while ((q->qp[qnbr]+c->nbr_cells) >= q->q_size[qnbr]) {
		size_t n = q->q_size[qnbr] + q->q_size[qnbr] / 2;
		void *ptr = realloc(q->queue[qnbr], sizeof(cell)*n);
		if (!ptr) return NULL;
		q->queue[qnbr] = ptr;
		q->q_size[qnbr] = n;
	}

	cell *dst = q->queue[qnbr] + q->qp[qnbr];
	q->qp[qnbr] += safe_copy_cells(dst, c, c->nbr_cells);
	q->qcnt[qnbr]++;
	return dst;
}

void fix_list(cell *c)
{
	pl_idx cnt = c->nbr_cells;

	while (is_iso_list(c)) {
		c->nbr_cells = cnt;
		c = c + 1;					// skip .
		cnt -= 1 + c->nbr_cells;
		c = c + c->nbr_cells;		// skip head
	}
}

// Defer check until end_list()

void allocate_list(query *q, const cell *c)
{
	if (!init_tmp_heap(q))
		return;

	append_list(q, c);
}

// Defer check until end_list()

void append_list(query *q, const cell *c)
{
	cell *tmp = alloc_on_tmp(q, 1+c->nbr_cells);
	if (!tmp) return;
	tmp->tag = TAG_INTERNED;
	tmp->nbr_cells = 1 + c->nbr_cells;
	tmp->val_off = g_dot_s;
	tmp->arity = 2;
	tmp->flags = 0;
	tmp++;
	copy_cells(tmp, c, c->nbr_cells);
}

cell *end_list(query *q)
{
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	tmp->tag = TAG_INTERNED;
	tmp->nbr_cells = 1;
	tmp->val_off = g_nil_s;
	tmp->arity = tmp->flags = 0;
	pl_idx nbr_cells = tmp_heap_used(q);

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	safe_copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	fix_list(tmp);
	init_tmp_heap(q);
	return tmp;
}

cell *end_list_unsafe(query *q)
{
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	tmp->tag = TAG_INTERNED;
	tmp->nbr_cells = 1;
	tmp->val_off = g_nil_s;
	tmp->arity = tmp->flags = 0;
	pl_idx nbr_cells = tmp_heap_used(q);

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	fix_list(tmp);
	return tmp;
}

// Defer check until end_list()

void allocate_structure(query *q, const char *functor, const cell *c)
{
	if (!init_tmp_heap(q))
		return;

	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return;
	tmp->tag = TAG_INTERNED;
	tmp->nbr_cells = 1;
	tmp->val_off = index_from_pool(q->pl, functor);
	tmp->arity = 0;
	tmp->flags = 0;
	append_structure(q, c);
}

// Defer check until end_list()

void append_structure(query *q, const cell *c)
{
	cell *tmp = alloc_on_tmp(q, c->nbr_cells);
	if (!tmp) return;
	copy_cells(tmp, c, c->nbr_cells);
	tmp = q->tmp_heap;
	tmp->arity++;
}

cell *end_structure(query *q)
{
	pl_idx nbr_cells = tmp_heap_used(q);
	cell *tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	safe_copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;

	if (q->tmp_heap && (q->tmph_size > 1000)) {
		free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = 1000;
	}

	return tmp;
}
