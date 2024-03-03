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

static int accum_slot(const query *q, size_t slot_nbr, unsigned var_nbr)
{
	const void *vnbr;

	if (sl_get(q->vars, (void*)(size_t)slot_nbr, &vnbr))
		return (unsigned)(size_t)vnbr;

	sl_set(q->vars, (void*)(size_t)slot_nbr, (void*)(size_t)var_nbr);
	return -1;
}

size_t alloc_grow(query *q, void **addr, size_t elem_size, size_t min_elements, size_t max_elements, bool zeroit)
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

	if (!mem) {
		q->oom = 1;
		return 0;
	}

	if (zeroit)
		memset(mem + (min_elements * elem_size), 0, (elements - min_elements) * elem_size);

	*addr = mem;
	return elements;
}

// The tmp heap is used for temporary allocations (a scratch-pad)
// for work in progress. As such it can survive a realloc() call.

cell *init_tmp_heap(query *q)
{
	if (q->tmp_heap && (q->tmph_size > 1000) && false) {
		free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = 1000;
	}

	if (!q->tmp_heap) {
		q->tmp_heap = malloc(q->tmph_size * sizeof(cell));
		if (!q->tmp_heap) return NULL;
	}

	q->tmphp = 0;
	q->cycle_error = false;
	return q->tmp_heap;
}

cell *preinit_tmp_heap(query *q, pl_idx n)
{
	if (q->tmp_heap && (q->tmph_size < n)) {
		free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = n;
	}

	if (!q->tmp_heap) {
		q->tmp_heap = malloc(n * sizeof(cell));
		if (!q->tmp_heap) return NULL;
	}

	q->tmphp = 0;
	q->cycle_error = false;
	return q->tmp_heap;
}

cell *alloc_on_tmp(query *q, unsigned nbr_cells)
{
	if (((uint64_t)q->tmphp + nbr_cells) > UINT32_MAX)
		return NULL;

	pl_idx new_size = q->tmphp + nbr_cells;

	if (new_size >= q->tmph_size) {
		size_t elements = alloc_grow(q, (void**)&q->tmp_heap, sizeof(cell), new_size, new_size*2, true);
		if (!elements) return NULL;
		q->tmph_size = elements;
	}

	cell *c = q->tmp_heap + q->tmphp;
	q->tmphp = new_size;
	return c;
}

// The heap is used for data allocations and a realloc() can't be
// done as it will invalidate existing pointers. Build any compounds
// first on the tmp heap, then allocate in one go here and copy in.
// When more space is need allocate a new page and keep them in the
// page list. Backtracking will garbage collect and free as needed.

cell *alloc_on_heap(query *q, unsigned nbr_cells)
{
	if (((uint64_t)q->st.hp + nbr_cells) > UINT32_MAX)
		return NULL;

	if (!q->heap_pages) {
		page *a = calloc(1, sizeof(page));
		if (!a) return NULL;
		a->next = q->heap_pages;
		unsigned n = MAX_OF(q->heap_size, nbr_cells);
		a->cells = calloc(a->page_size=n, sizeof(cell));
		if (!a->cells) { free(a); return NULL; }
		a->nbr = q->st.heap_nbr++;
		q->heap_pages = a;
	}

	if ((q->st.hp + nbr_cells) >= q->heap_pages->page_size) {
		page *a = calloc(1, sizeof(page));
		if (!a) return NULL;
		a->next = q->heap_pages;
		unsigned n = MAX_OF(q->heap_size, nbr_cells);
		a->cells = calloc(a->page_size=n, sizeof(cell));
		if (!a->cells) { free(a); return NULL; }
		a->nbr = q->st.heap_nbr++;
		q->heap_pages = a;
		q->st.hp = 0;
	}

	if (q->st.heap_nbr > q->hw_heap_nbr)
		q->hw_heap_nbr = q->st.heap_nbr;

	cell *c = q->heap_pages->cells + q->st.hp;
	q->st.hp += nbr_cells;
	q->heap_pages->idx = q->st.hp;

	if (q->heap_pages->idx > q->heap_pages->max_idx_used)
		q->heap_pages->max_idx_used = q->heap_pages->idx;

	return c;
}

void trim_heap(query *q)
{
	// q->heap_pages is a push-down stack and points to the
	// most recent page of heap allocations...

	for (page *a = q->heap_pages; a;) {
		if (a->nbr < q->st.heap_nbr)
			break;

		cell *c = a->cells;

		for (pl_idx i = 0; i < a->idx; i++, c++)
			unshare_cell(c);

		page *save = a;
		q->heap_pages = a = a->next;
		free(save->cells);
		free(save);
	}

	const page *a = q->heap_pages;

	for (pl_idx i = q->st.hp; a && (i < a->idx); i++) {
		cell *c = a->cells + i;
		unshare_cell(c);
		init_cell(c);
	}
}

#define deep_copy(c) \
	(!q->noderef || (is_ref(c) && (c->var_ctx <= q->st.curr_frame) && !is_anon(c)))

static cell *deep_clone2_to_tmp(query *q, cell *p1, pl_idx p1_ctx, unsigned depth)
{
	if (depth >= g_max_depth) {
		printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return NULL;
	}

	pl_idx save_idx = tmp_heap_used(q);
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	// Convert vars to refs...

	if (is_var(tmp) && !is_ref(tmp)) {
		tmp->flags |= FLAG_VAR_REF;
		tmp->var_ctx = p1_ctx;
	}

	if (!is_compound(p1))
		return tmp;

	if (is_iso_list(p1)) {
		cell *save_p1 = p1;
		pl_idx save_p1_ctx = p1_ctx;
		bool any1 = false, any2 = false;

		while (is_iso_list(p1)) {
			slot *e = NULL;
			cell *h = p1 + 1;
			pl_idx h_ctx = p1_ctx;
			uint32_t save_vgen = 0;
			int both = 0;
			if (deep_copy(h)) DEREF_CHECKED(any1, both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);
			if (both) q->cycle_error = true;
			cell *rec = deep_clone2_to_tmp(q, h, h_ctx, depth+1);
			if (!rec) return NULL;
			if (e) e->vgen = save_vgen;

			p1 = p1 + 1; p1 += p1->nbr_cells;
			cell *t = p1;
			pl_idx t_ctx = p1_ctx;

			both = 0;
			if (deep_copy(t)) DEREF_CHECKED(any2, both, save_vgen, e, e->vgen, t, t_ctx, q->vgen);
			if (both) q->cycle_error = true;
			p1 = t;
			p1_ctx = t_ctx;

			if (is_iso_list(p1)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				copy_cells(tmp, p1, 1);
			}
		}

		cell *rec = deep_clone2_to_tmp(q, p1, p1_ctx, depth+1);
		if (!rec) return NULL;

		if (any2) {
			p1 = save_p1;
			p1_ctx = save_p1_ctx;

			while (is_iso_list(p1) && !q->cycle_error) {
				p1 = p1 + 1; p1 += p1->nbr_cells;
				cell *c = p1;
				pl_idx c_ctx = p1_ctx;
				RESTORE_VAR(c, c_ctx, p1, p1_ctx, q->vgen);
			}
		}

		tmp = get_tmp_heap(q, save_idx);
		tmp->nbr_cells = tmp_heap_used(q) - save_idx;
		return tmp;
	}

	bool any = false;
	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		slot *e = NULL;
		cell *c = p1;
		pl_idx c_ctx = p1_ctx;
		uint32_t save_vgen = 0;
		int both = 0;
		if (deep_copy(c)) DEREF_CHECKED(any, both, save_vgen, e, e->vgen, c, c_ctx, q->vgen);
		if (both) q->cycle_error = true;
		cell *rec = deep_clone2_to_tmp(q, c, c_ctx, depth+1);
		if (!rec) return NULL;
		if (e) e->vgen = save_vgen;
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

cell *clone_to_heap(query *q, cell *p1, pl_idx p1_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	p1 = clone_to_tmp(q, p1, p1_ctx);
	if (!p1) return p1;
	cell *tmp = alloc_on_heap(q, p1->nbr_cells);
	if (!tmp) return NULL;
	dup_cells(tmp, p1, p1->nbr_cells);
	return tmp;
}

cell *deep_clone_to_heap(query *q, cell *p1, pl_idx p1_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	p1 = deep_clone_to_tmp(q, p1, p1_ctx);
	if (!p1) return p1;
	cell *tmp = alloc_on_heap(q, p1->nbr_cells);
	if (!tmp) return NULL;
	dup_cells(tmp, p1, p1->nbr_cells);
	return tmp;
}

cell *append_to_tmp(query *q, cell *p1, pl_idx p1_ctx)
{
	cell *tmp = alloc_on_tmp(q, p1->nbr_cells);
	if (!tmp) return NULL;
	cell *src = p1, *dst = tmp;

	for (pl_idx i = 0; i < p1->nbr_cells; i++, dst++, src++) {
		*dst = *src;

		if (!is_var(src) || is_ref(src))
			continue;

		dst->flags |= FLAG_VAR_REF;
		dst->var_ctx = p1_ctx;
	}

	return tmp;
}

cell *clone_to_tmp(query *q, cell *p1, pl_idx p1_ctx)
{
	return append_to_tmp(q, p1, p1_ctx);
}

cell *prepare_call(query *q, bool prefix, cell *p1, pl_idx p1_ctx, unsigned extras)
{
	unsigned nbr_cells = (prefix ? PREFIX_LEN : NOPREFIX_LEN) + p1->nbr_cells + extras;
	cell *tmp = alloc_on_cache(q, nbr_cells);
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
			s_fn_ptr = get_fn_ptr(bif_iso_true_0);

		tmp->bif_ptr = s_fn_ptr;
	}

	q->in_call = true;
	cell *dst = tmp + (prefix ? PREFIX_LEN : NOPREFIX_LEN);
	dup_cells_by_ref(dst, p1, p1_ctx, p1->nbr_cells);
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
		const size_t slot_nbr = f->base + tmp->var_nbr;
		int var_nbr;

		if ((var_nbr = accum_slot(q, slot_nbr, q->varno)) == -1)
			var_nbr = q->varno++;

		if (!q->tab_idx) {
			q->tab0_varno = var_nbr;
			q->tab_idx++;
		}

		tmp->tmp_attrs = NULL;
		tmp->flags |= FLAG_VAR_FRESH;
		tmp->flags |= FLAG_VAR_ANON;

		if (from && (tmp->var_nbr == from->var_nbr) && (tmp->var_ctx == from_ctx)) {
			tmp->var_nbr = to->var_nbr;
			tmp->var_ctx = to_ctx;
		} else {
			tmp->var_nbr = var_nbr;
			tmp->var_ctx = q->st.curr_frame;

			if (copy_attrs && e->c.attrs) {
				push_tmp_heap(q);
				cell *tmp2 = deep_clone_to_heap(q, e->c.attrs, e->c.attrs_ctx);
				pop_tmp_heap(q);
				check_heap_error(tmp2);
				tmp->tmp_attrs = tmp2;
			}
		}
	}

	return true;
}

unsigned rebase_term(query *q, cell *c, unsigned start_nbr)
{
	q->vars = sl_create(NULL, NULL, NULL);
	q->varno = start_nbr;
	q->tab_idx = 0;

	if (!copy_vars(q, c, false, NULL, 0, NULL, 0)) {
		sl_destroy(q->vars);
		q->vars = NULL;
		return start_nbr;
	}

	sl_destroy(q->vars);
	q->vars = NULL;

	// Turn refs back into vars to decontextualize

	cell *tmp = c;

	for (unsigned i = 0; i < c->nbr_cells; i++, tmp++) {
		if (!is_ref(tmp))
			continue;

		tmp->flags &= ~FLAG_VAR_REF;
	}

	return q->varno;
}

static cell *deep_copy_to_tmp_with_replacement(query *q, cell *p1, pl_idx p1_ctx, bool copy_attrs, cell *from, pl_idx from_ctx, cell *to, pl_idx to_ctx)
{
	const frame *f = GET_CURR_FRAME();
	q->vars = sl_create(NULL, NULL, NULL);
	q->varno = f->actual_slots;
	q->tab_idx = 0;

	cell *c = deref(q, p1, p1_ctx);
	pl_idx c_ctx = q->latest_ctx;

	cell *tmp = deep_clone_to_tmp(q, c, c_ctx);

	if (!tmp) {
		sl_destroy(q->vars);
		q->vars = NULL;
		return NULL;
	}

	if (!copy_vars(q, tmp, copy_attrs, from, from_ctx, to, to_ctx)) {
		sl_destroy(q->vars);
		q->vars = NULL;
		return NULL;
	}

	sl_destroy(q->vars);
	q->vars = NULL;
	int cnt = q->varno - f->actual_slots;

	if (cnt) {
		if (!create_vars(q, cnt)) {
			throw_error(q, c, c_ctx, "resource_error", "stack");
			return NULL;
		}
	}

	if (!copy_attrs)
		return tmp;

	c = tmp;

	for (pl_idx i = 0; i < tmp->nbr_cells; i++, c++) {
		if (is_ref(c) && c->tmp_attrs) {
			const frame *f = GET_FRAME(c->var_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			e->c.attrs = c->tmp_attrs;
			e->c.attrs_ctx = q->st.curr_frame;
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
	dup_cells(tmp2, tmp, tmp->nbr_cells);
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
	dup_cells(tmp2, tmp, tmp->nbr_cells);
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
	q->qp[qnbr] += dup_cells(dst, c, c->nbr_cells);
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

	if (is_nil(get_tmp_heap(q, 0))) {
		init_tmp_heap(q);
		return make_nil();
	}

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	dup_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
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
	tmp->val_off = new_atom(q->pl, functor);
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
	dup_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;

	if (q->tmp_heap && (q->tmph_size > 1000)) {
		free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = 1000;
	}

	return tmp;
}
