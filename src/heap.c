#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heap.h"
#include "query.h"

static int accum_slot(const query *q, pl_idx_t slot_nbr, unsigned var_nbr)
{
	const void *vnbr;

	if (map_get(q->vars, (void*)(size_t)slot_nbr, &vnbr))
		return (unsigned)(size_t)vnbr;

	map_set(q->vars, (void*)(size_t)slot_nbr, (void*)(size_t)var_nbr);
	return -1;
}

size_t alloc_grow(void **addr, size_t elem_size, size_t min_elements, size_t max_elements)
{
	assert(min_elements <= max_elements);
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

	*addr = mem;
	return elements;
}

// The tmp heap is used for temporary allocations (a scratch-pad)
// for work in progress. As such it can survive a realloc() call.

cell *init_tmp_heap(query *q)
{
	if (q->tmp_heap && (q->tmph_size > 1024)) {
		free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = 100;
	}

	if (!q->tmp_heap) {
		q->tmp_heap = malloc(q->tmph_size * sizeof(cell));
		if (!q->tmp_heap) return NULL;
		*q->tmp_heap = (cell){0};
	}

	q->tmphp = 0;
	return q->tmp_heap;
}

cell *alloc_on_tmp(query *q, pl_idx_t nbr_cells)
{
	if (((uint64_t)q->tmphp + nbr_cells) > UINT32_MAX)
		return NULL;

	pl_idx_t new_size = q->tmphp + nbr_cells;

	while (new_size >= q->tmph_size) {
		size_t elements = alloc_grow((void**)&q->tmp_heap, sizeof(cell), new_size, (new_size*4)/3);
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

cell *alloc_on_heap(query *q, pl_idx_t nbr_cells)
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
		a->nbr = q->st.curr_page++;
		q->pages = a;
	}

	if ((q->st.hp + nbr_cells) >= q->pages->h_size) {
		page *a = calloc(1, sizeof(page));
		if (!a) return NULL;
		a->next = q->pages;
		unsigned n = MAX_OF(q->h_size, nbr_cells);
		a->heap = calloc(a->h_size=n, sizeof(cell));
		if (!a->heap) { free(a); return NULL; }
		a->nbr = q->st.curr_page++;
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

bool is_in_ref_list(cell *c, pl_idx_t c_ctx, reflist *rlist)
{
	while (rlist) {
		if ((c->var_nbr == rlist->var_nbr)
			&& (c_ctx == rlist->ctx))
			return true;

		rlist = rlist->next;
	}

	return false;
}

static bool is_in_ref_list2(cell *c, pl_idx_t c_ctx, reflist *rlist)
{
	while (rlist) {
		if ((c == rlist->ptr)
			&& (c_ctx == rlist->ctx))
			return true;

		rlist = rlist->next;
	}

	return false;
}

// FIXME: rewrite this using efficient sweep/mark methodology...

static cell *deep_copy2_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx, unsigned depth, reflist *list)
{
	if (depth >= MAX_DEPTH) {
		printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return NULL;
	}

	const pl_idx_t save_idx = tmp_heap_used(q);
	cell *save_p1 = p1;
	p1 = deref(q, p1, p1_ctx);
	p1_ctx = q->latest_ctx;

	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	if (!is_structure(p1)) {
		if (!is_variable(p1))
			return tmp;

		const frame *f = GET_FRAME(p1_ctx);
		const slot *e = GET_SLOT(f, p1->var_nbr);
		const pl_idx_t slot_nbr = e - q->slots;
		int var_nbr;

		if ((var_nbr = accum_slot(q, slot_nbr, q->varno)) == -1)
			var_nbr = q->varno++;

		if (!q->tab_idx) {
			q->tab0_varno = var_nbr;
			q->tab_idx++;
		}

		tmp->var_nbr = var_nbr;
		tmp->flags = FLAG_VAR_FRESH;

		if (from && (p1->var_nbr == from->var_nbr) && (p1_ctx == from_ctx)) {
			tmp->flags |= FLAG_REF;
			tmp->val_off = to->val_off;
			tmp->var_nbr = to->var_nbr;
			tmp->var_ctx = to_ctx;
		} else if (copy_attrs && e->c.attrs) {
			push_tmp_heap(q);
			cell *tmp2 = deep_copy_to_heap_with_replacement(q, e->c.attrs, e->c.attrs_ctx, false, NULL, 0, NULL, 0);
			pop_tmp_heap(q);
			if (!tmp2) return NULL;
			tmp->tmp_attrs = tmp2;
			tmp->var_ctx = q->st.curr_frame;
		}

		return tmp;
	}

	pl_idx_t save_p1_ctx = p1_ctx;
	bool cyclic = false;

	if (q->lists_ok && is_iso_list(p1)) {
		LIST_HANDLER(p1);

		while (is_iso_list(p1)) {
			if (g_tpl_interrupt) {
				if (check_interrupt(q))
					break;
			}

			cell *h = LIST_HEAD(p1);
			cell *c = h;
			pl_idx_t c_ctx = p1_ctx;
			c = deref(q, c, c_ctx);
			c_ctx = q->latest_ctx;

			if (is_in_ref_list2(c, c_ctx, list)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				*tmp = *h;
				tmp->var_nbr = q->tab0_varno;
				tmp->flags |= FLAG_VAR_FRESH;
				tmp->tmp_attrs = NULL;
			} else {
				reflist nlist = {0};
				nlist.next = list;
				nlist.ptr = save_p1;
				nlist.ctx = save_p1_ctx;
				cell *rec = deep_copy2_to_tmp(q, c, c_ctx, copy_attrs, from, from_ctx, to, to_ctx, depth+1, &nlist);
				if (!rec) return rec;
			}

			p1 = LIST_TAIL(p1);
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;

			reflist nlist = {0};
			nlist.next = list;
			nlist.ptr = save_p1;
			nlist.ctx = save_p1_ctx;

			if (is_in_ref_list2(p1, p1_ctx, &nlist)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				tmp->tag = TAG_VAR;
				tmp->flags = 0;
				tmp->nbr_cells = 1;
				tmp->val_off = g_anon_s;
				tmp->var_nbr = q->tab0_varno;
				tmp->flags |= FLAG_VAR_FRESH;
				tmp->tmp_attrs = NULL;
				cyclic = true;
				break;
			}

			if (is_iso_list(p1)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				copy_cells(tmp, p1, 1);
			}
		}

		if (!cyclic) {
			cell *rec = deep_copy2_to_tmp(q, p1, p1_ctx, copy_attrs, from, from_ctx, to, to_ctx, depth+1, list);
			if (!rec) return rec;
		}

		tmp = get_tmp_heap(q, save_idx);
		tmp->nbr_cells = tmp_heap_used(q) - save_idx;
		fix_list(tmp);
	} else {
		unsigned arity = p1->arity;
		p1++;

		while (arity--) {
			cell *c = p1;
			pl_idx_t c_ctx = p1_ctx;
			c = deref(q, c, c_ctx);
			c_ctx = q->latest_ctx;
			reflist nlist = {0};

			if (is_in_ref_list2(c, c_ctx, list)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				*tmp = *p1;
				tmp->var_nbr = q->tab0_varno;
				tmp->flags |= FLAG_VAR_FRESH;
				tmp->tmp_attrs = NULL;
			} else {
				nlist.next = list;
				nlist.ptr = save_p1;
				nlist.ctx = save_p1_ctx;

				cell *rec = deep_copy2_to_tmp(q, c, c_ctx, copy_attrs, from, from_ctx, to, to_ctx, depth+1, !q->lists_ok ? &nlist : NULL);
				if (!rec) return rec;
			}

			p1 += p1->nbr_cells;
		}

		tmp = get_tmp_heap(q, save_idx);
		tmp->nbr_cells = tmp_heap_used(q) - save_idx;
	}

	return tmp;
}

cell *deep_raw_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx)
{
	frame *f = GET_CURR_FRAME();
	q->varno = f->actual_slots;
	q->tab_idx = 0;
	q->cycle_error = false;
	reflist nlist = {0};
	nlist.ptr = p1;
	nlist.ctx = p1_ctx;
	q->vars = map_create(NULL, NULL, NULL);
	if (!q->vars) return NULL;
	cell *rec = deep_copy2_to_tmp(q, p1, p1_ctx, false, NULL, 0, NULL, 0, 0, &nlist);
	map_destroy(q->vars);
	q->vars = NULL;
	if (!rec) return rec;
	return q->tmp_heap;
}

static cell *deep_copy_to_tmp_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx)
{
	cell *save_p1 = p1;
	pl_idx_t save_p1_ctx = p1_ctx;
	cell *c = deref(q, p1, p1_ctx);
	pl_idx_t c_ctx = q->latest_ctx;
	frame *f = GET_CURR_FRAME();

	if (is_iso_list(c)) {
		bool is_partial;

		if (check_list(q, c, c_ctx, &is_partial, NULL))
			q->lists_ok = true;
		else
			q->lists_ok = false;
	} else
		q->lists_ok = false;

	q->cycle_error = false;

	if (q->vars && is_variable(save_p1)) {
		const frame *f = GET_FRAME(p1_ctx);
		const slot *e = GET_SLOT(f, p1->var_nbr);
		const pl_idx_t slot_nbr = e - q->slots;

		if (!q->tab_idx) {
			q->tab0_varno = q->varno;
			q->tab_idx++;
		}

		map_set(q->vars, (void*)(size_t)slot_nbr, (void*)(size_t)q->varno);
		q->varno++;
	}

	if (is_variable(p1)) {
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	reflist nlist = {0};
	nlist.ptr = c;
	nlist.ctx = c_ctx;
	cell *rec = deep_copy2_to_tmp(q, c, c_ctx, copy_attrs, from, from_ctx, to, to_ctx, 0, !q->lists_ok ? &nlist : NULL);
	q->lists_ok = false;
	if (!rec) return rec;
	int cnt = q->varno - f->actual_slots;

#if 0
	printf("*** f=%u, f->actual_slots=%u, f->initial_slots=%u, q->varno=%u, cnt=%d\n",
		(unsigned)q->st.fp, (unsigned)f->actual_slots, (unsigned)f->initial_slots, (unsigned)q->varno, cnt);
#endif

	if (cnt) {
		if (!create_vars(q, cnt)) {
			throw_error(q, p1, p1_ctx, "resource_error", "stack");
			return NULL;
		}
	}

	if (is_variable(save_p1)) {
		cell tmp;
		tmp = *save_p1;
		tmp.var_nbr = q->tab0_varno;
		unify(q, &tmp, q->st.curr_frame, rec, q->st.curr_frame);
	}

	if (!copy_attrs)
		return get_tmp_heap_start(q);

	c = get_tmp_heap_start(q);

	for (pl_idx_t i = 0; i < rec->nbr_cells; i++, c++) {
		if (is_variable(c) && is_fresh(c) && c->tmp_attrs) {
			//printf("*** got one var_nbr=%u / %u\n", c->var_nbr, c->var_ctx);
			frame *f = GET_FRAME(c->var_ctx);
			slot *e = GET_SLOT(f, c->var_nbr);
			e->c.attrs = c->tmp_attrs;
			e->c.attrs_ctx = c->var_ctx;
		}
	}

	return get_tmp_heap_start(q);
}

cell *deep_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs)
{
	q->vars = map_create(NULL, NULL, NULL);
	if (!q->vars) return NULL;
	frame *f = GET_CURR_FRAME();
	q->varno = f->actual_slots;
	q->tab_idx = 0;
	cell *tmp = deep_copy_to_tmp_with_replacement(q, p1, p1_ctx, copy_attrs, NULL, 0, NULL, 0);
	map_destroy(q->vars);
	q->vars = NULL;
	return tmp;
}

cell *deep_copy_to_heap(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs)
{
	if (!init_tmp_heap(q))
		return NULL;

	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, copy_attrs);
	if (!tmp) return tmp;
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	if (!tmp2) return NULL;
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return tmp2;
}

cell *deep_copy_to_heap_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx)
{
	q->vars = map_create(NULL, NULL, NULL);
	if (!q->vars) return NULL;
	frame *f = GET_CURR_FRAME();
	q->varno = f->actual_slots;
	q->tab_idx = 0;
	cell *tmp = deep_copy_to_tmp_with_replacement(q, p1, p1_ctx, copy_attrs, from, from_ctx, to, to_ctx);
	map_destroy(q->vars);
	q->vars = NULL;
	if (!tmp) return NULL;
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	if (!tmp2) return NULL;
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return tmp2;
}

static cell *deep_clone2_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, unsigned depth, reflist *list)
{
	if (depth >= MAX_DEPTH) {
		printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return NULL;
	}

	pl_idx_t save_idx = tmp_heap_used(q);
	cell *save_p1 = p1;
	pl_idx_t save_p1_ctx = p1_ctx;
	p1 = deref(q, p1, p1_ctx);
	p1_ctx = q->latest_ctx;
	cell *tmp = alloc_on_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	if (is_variable(tmp) && !is_ref(tmp)) {
		tmp->flags |= FLAG_REF;
		tmp->var_ctx = p1_ctx;
	}

	if (!is_structure(p1))
		return tmp;

	bool cyclic = false;
	bool is_partial = false;

	if (!q->lists_ok && is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, NULL))
		is_partial = true;

	if (!is_partial && is_iso_list(p1)) {
		LIST_HANDLER(p1);

		while (is_iso_list(p1)) {
			if (g_tpl_interrupt) {
				if (check_interrupt(q))
					break;
			}

			cell *h = LIST_HEAD(p1);
			cell *c = deref(q, h, p1_ctx);
			pl_idx_t c_ctx = q->latest_ctx;

			if (is_in_ref_list2(c, c_ctx, list)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				*tmp = *h;
			} else {
				reflist nlist = {0};
				nlist.next = list;
				nlist.ptr = save_p1;
				nlist.ctx = save_p1_ctx;
				cell *rec = deep_clone2_to_tmp(q, c, c_ctx, depth+1, &nlist);
				if (!rec) return rec;
			}

			p1 = LIST_TAIL(p1);
			cell *tmp_p1 = p1;
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;

			reflist nlist = {0};
			nlist.next = list;
			nlist.ptr = save_p1;
			nlist.ctx = save_p1_ctx;

			if (is_in_ref_list2(p1, p1_ctx, &nlist)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				*tmp = *tmp_p1;
				cyclic = true;
				break;
			}

			if (is_iso_list(p1)) {
				cell *tmp = alloc_on_tmp(q, 1);
				if (!tmp) return NULL;
				copy_cells(tmp, p1, 1);
			}
		}

		if (!cyclic) {
			cell *rec = deep_clone2_to_tmp(q, p1, p1_ctx, depth+1, list);
			if (!rec) return rec;
		}

		tmp = get_tmp_heap(q, save_idx);
		tmp->nbr_cells = tmp_heap_used(q) - save_idx;
		return tmp;
	}

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		cell *c = deref(q, p1, p1_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		reflist nlist = {0};

		if (is_in_ref_list2(c, c_ctx, list)) {
			cell *tmp = alloc_on_tmp(q, 1);
			if (!tmp) return NULL;
			*tmp = *p1;
		} else {
			nlist.next = list;
			nlist.ptr = save_p1;
			nlist.ctx = save_p1_ctx;

			cell *rec = deep_clone2_to_tmp(q, c, c_ctx, depth+1, &nlist);
			if (!rec) return rec;
		}

		p1 += p1->nbr_cells;
	}

	tmp = get_tmp_heap(q, save_idx);
	tmp->nbr_cells = tmp_heap_used(q) - save_idx;
	return tmp;
}

cell *deep_clone_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx)
{
	q->cycle_error = false;
	reflist nlist = {0};
	nlist.ptr = p1;
	nlist.ctx = p1_ctx;

	cell *rec = deep_clone2_to_tmp(q, p1, p1_ctx, 0, &nlist);
	if (!rec) return rec;
	return q->tmp_heap;
}

cell *deep_clone_to_heap(query *q, cell *p1, pl_idx_t p1_ctx)
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

	for (pl_idx_t i = 0; i < p1->nbr_cells; i++, dst++, src++) {
		*dst = *src;

		if (!is_variable(src) || is_ref(src))
			continue;

		dst->flags |= FLAG_REF;
		dst->var_ctx = q->st.curr_frame;
	}

	return tmp;
}

cell *clone_to_tmp(query *q, cell *p1)
{
	return append_to_tmp(q, p1);
}

cell *clone_to_heap(query *q, bool prefix, cell *p1, pl_idx_t suffix)
{
	cell *tmp = alloc_on_heap(q, (prefix?1:0)+p1->nbr_cells+suffix);
	if (!tmp) return NULL;
	frame *f = GET_CURR_FRAME();

	if (prefix) {
		// Needed for follow() to work
		*tmp = (cell){0};
		tmp->tag = TAG_EMPTY;
		tmp->nbr_cells = 1;
		tmp->flags = FLAG_BUILTIN;
	}

	cell *src = p1, *dst = tmp+(prefix?1:0);

	for (pl_idx_t i = 0; i < p1->nbr_cells; i++, dst++, src++) {
		*dst = *src;
		share_cell(src);

		if (!is_variable(src) || is_ref(src))
			continue;

		dst->flags |= FLAG_REF;
		dst->var_ctx = q->st.curr_frame;
	}

	return tmp;
}

cell *alloc_on_queuen(query *q, int qnbr, const cell *c)
{
	if (!q->queue[qnbr]) {
		q->queue[qnbr] = calloc(q->q_size[qnbr], sizeof(cell));
		check_error(q->queue[qnbr]);
	}

	while ((q->qp[qnbr]+c->nbr_cells) >= q->q_size[qnbr]) {
		q->q_size[qnbr] += q->q_size[qnbr] / 2;
		q->queue[qnbr] = realloc(q->queue[qnbr], sizeof(cell)*q->q_size[qnbr]);
		check_error(q->queue[qnbr]);
	}

	cell *dst = q->queue[qnbr] + q->qp[qnbr];
	q->qp[qnbr] += safe_copy_cells(dst, c, c->nbr_cells);
	return dst;
}

cell *alloc_on_queuen_unsafe(query *q, int qnbr, const cell *c)
{
	if (!q->queue[qnbr]) {
		q->queue[qnbr] = calloc(q->q_size[qnbr], sizeof(cell));
		check_error(q->queue[qnbr]);
	}

	while ((q->qp[qnbr]+c->nbr_cells) >= q->q_size[qnbr]) {
		q->q_size[qnbr] += q->q_size[qnbr] / 2;
		q->queue[qnbr] = realloc(q->queue[qnbr], sizeof(cell)*q->q_size[qnbr]);
		check_error(q->queue[qnbr]);
	}

	cell *dst = q->queue[qnbr] + q->qp[qnbr];
	q->qp[qnbr] += copy_cells(dst, c, c->nbr_cells);
	return dst;
}

void fix_list(cell *c)
{
	pl_idx_t cnt = c->nbr_cells;

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
	pl_idx_t nbr_cells = tmp_heap_used(q);

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	safe_copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	fix_list(tmp);
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
	pl_idx_t nbr_cells = tmp_heap_used(q);

	tmp = alloc_on_heap(q, nbr_cells);
	if (!tmp) return NULL;
	copy_cells(tmp, get_tmp_heap(q, 0), nbr_cells);
	tmp->nbr_cells = nbr_cells;
	fix_list(tmp);
	return tmp;
}
