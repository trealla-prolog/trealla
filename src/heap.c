#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#include "prolog.h"
#include "query.h"

struct heap_save {
	cell *heap;
	pl_idx size, hp;
};

static int accum_slot(const query *q, size_t slot_nbr, unsigned var_num)
{
	const void *vnbr;

	if (sl_get(q->vars, (void*)slot_nbr, &vnbr))
		return (unsigned)(size_t)vnbr;

	sl_app(q->vars, (void*)slot_nbr, (void*)(size_t)var_num);
	return -1;
}

size_t alloc_grow(query *q, void **addr, size_t elem_size, size_t min_elements, size_t max_elements)
{
	if (min_elements > max_elements)
		max_elements = min_elements;

	size_t elements = max_elements;
	void *mem;

	do {
		mem = realloc(*addr, elem_size * elements);
		if (mem) break;
		elements = min_elements + (elements - min_elements) / 2;
	}
	 while (elements > min_elements);

	if (!mem) {
		q->oom = true;
		return 0;
	}

	*addr = mem;
	return elements;
}

cell *init_tmp_heap(query *q)
{
	if (!q->tmp_heap) {
		q->tmp_heap = malloc(q->tmph_size * sizeof(cell));
		if (!q->tmp_heap) return NULL;
	}

	q->tmphp = 0;
	return q->tmp_heap;
}

// The tmp heap is used for temporary allocations (a scratch-pad)
// for work in progress. As such it can survive a realloc() call.
// No need to incr refcnt on tmp heap cells.

cell *alloc_tmp(query *q, unsigned num_cells)
{
	pl_idx new_size = q->tmphp + num_cells;

	if (new_size >= q->tmph_size) {
		size_t elements = alloc_grow(q, (void**)&q->tmp_heap, sizeof(cell), new_size, new_size*5/4);
		if (!elements) return NULL;
		q->tmph_size = elements;
	}

	cell *c = q->tmp_heap + q->tmphp;
	q->tmphp = new_size;
	return c;
}

#define deep_copy(c) \
	(!q->noderef || (is_ref(c) && (c->val_ctx <= q->st.cur_ctx) && !is_anon(c)))

// Note: convert vars to refs
// Note: doesn't increment ref counts

static cell *clone_term_to_tmp_internal(query *q, cell *p1, pl_ctx p1_ctx, unsigned depth)
{
	if (depth >= g_max_depth) {
		printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		return NULL;
	}

	pl_idx save_idx = tmp_heap_used(q);
	cell *tmp = alloc_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, p1, 1);

	if (is_var(p1))
		q->has_vars = true;

	if (is_var(tmp) && !is_ref(tmp) && !q->noderef) {
		tmp->flags |= FLAG_VAR_REF;
		tmp->val_ctx = p1_ctx;
	}

	if (!is_compound(p1))
		return tmp;

	if (is_iso_list(p1)) {
		cell *save_p1 = p1;
		pl_ctx save_p1_ctx = p1_ctx;
		bool any1 = false, any2 = false;

		while (is_iso_list(p1)) {
			slot *e = NULL;
			cell *h = p1 + 1;
			pl_ctx h_ctx = p1_ctx;
			uint32_t save_vgen = 0;
			int both = 0;
			if (deep_copy(h)) DEREF_CHECKED(any1, both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);
			if (both) q->cycle_error = true;
			cell *rec = clone_term_to_tmp_internal(q, h, h_ctx, depth+1);
			if (!rec) return NULL;
			if (e) e->vgen = save_vgen;

			p1 = p1 + 1; p1 += p1->num_cells;
			cell *t = p1;
			pl_ctx t_ctx = p1_ctx;

			if (is_var(t) && (t->var_num == q->dump_var_num) && (t_ctx == q->dump_var_ctx)) {
				q->cycle_error = true;
				break;
			}

			both = 0;
			if (deep_copy(t)) DEREF_CHECKED(any2, both, save_vgen, e, e->vgen, t, t_ctx, q->vgen);

			if (both)
				q->cycle_error = true;

			p1 = t;
			p1_ctx = t_ctx;

			if (is_iso_list(p1)) {
				cell *tmp = alloc_tmp(q, 1);
				if (!tmp) return NULL;
				copy_cells(tmp, p1, 1);
			}
		}

		cell *rec = clone_term_to_tmp_internal(q, p1, p1_ctx, depth+1);
		if (!rec) return NULL;

		if (any2) {
			p1 = save_p1;
			p1_ctx = save_p1_ctx;

			while (is_iso_list(p1) && !q->cycle_error) {
				p1 = p1 + 1; p1 += p1->num_cells;
				cell *c = p1;
				pl_ctx c_ctx = p1_ctx;
				RESTORE_VAR(c, c_ctx, p1, p1_ctx, q->vgen);
			}
		}

		tmp = get_tmp_heap(q, save_idx);
		tmp->num_cells = tmp_heap_used(q) - save_idx;

		if (!q->has_vars)
			tmp->flags |= FLAG_INTERNED_GROUND;

		return tmp;
	}

	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		slot *e = NULL;
		cell *c = p1;
		pl_ctx c_ctx = p1_ctx;
		uint32_t save_vgen = 0;
		bool any = false;
		int both = 0;
		if (deep_copy(c)) DEREF_CHECKED(any, both, save_vgen, e, e->vgen, c, c_ctx, q->vgen);
		if (both) q->cycle_error = true;
		cell *rec = clone_term_to_tmp_internal(q, c, c_ctx, depth+1);
		if (!rec) return NULL;
		if (e) e->vgen = save_vgen;
		p1 += p1->num_cells;
	}

	tmp = get_tmp_heap(q, save_idx);
	tmp->num_cells = tmp_heap_used(q) - save_idx;

	if (!q->has_vars)
		tmp->flags |= FLAG_INTERNED_GROUND;

	return tmp;
}

cell *clone_term_to_tmp(query *q, cell *p1, pl_ctx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	q->has_vars = false;
	cell *rec = clone_term_to_tmp_internal(q, p1, p1_ctx, 0);
	if (!rec) return NULL;
	return rec;
}

cell *append_to_tmp(query *q, cell *p1, pl_ctx p1_ctx)
{
	cell *tmp = alloc_tmp(q, p1->num_cells);
	if (!tmp) return NULL;
	copy_cells_by_ref(tmp, p1, p1_ctx, p1->num_cells);
	return tmp;
}

static bool copy_vars(query *q, cell *c, bool copy_attrs, cell *from, pl_ctx from_ctx, cell *to, pl_ctx to_ctx)
{
	unsigned num_cells = c->num_cells;
	unsigned cnt = 0;

	for (unsigned i = 0; i < num_cells; i++, c++) {
		if (!is_ref(c))
			continue;

		c->flags |= FLAG_VAR_LOCAL;

		if (from && (c->var_num == from->var_num) && (c->val_ctx == from_ctx)) {
			c->var_num = to->var_num;
			c->val_ctx = to_ctx;
		} else {
			const frame *f = GET_FRAME(c->val_ctx);
			const slot *e = get_slot(q, f, c->var_num);
			cell *attrs = c->tmp_attrs ? c->tmp_attrs : e->c.val_attrs;
			const size_t slot_nbr = get_ordered_slot_num(q, f, c->var_num);
			int var_num;

			if ((var_num = accum_slot(q, slot_nbr, q->varno)) == -1) {
				var_num = q->varno++;
				cnt++;

				if (create_vars(q, 1) < 0)
					return false;
			}

			if (!q->tab_idx) {
				q->tab0_varno = var_num;
				q->tab_idx++;
			}

			c->var_num = var_num;
			c->val_ctx = q->st.cur_ctx;

			if (copy_attrs && attrs) {
				cell *save_tmp_heap = q->tmp_heap;
				pl_idx save_tmp_hp = q->tmphp;
				q->tmp_heap = NULL;

				if (!c->tmp_attrs) {
					cell *tmp =
						from ?copy_term_to_heap_with_replacement(q, attrs, q->st.cur_ctx, true, from, from_ctx, to, to_ctx)
						:copy_term_to_heap(q, attrs, q->st.cur_ctx, true);
					checked(tmp);
					c->tmp_attrs = tmp;
				}

				free(q->tmp_heap);
				q->tmp_heap = save_tmp_heap;
				q->tmphp = save_tmp_hp;
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

	if (!copy_vars(q, c, true, NULL, 0, NULL, 0)) {
		sl_destroy(q->vars);
		q->vars = NULL;
		return start_nbr;
	}

	sl_destroy(q->vars);
	q->vars = NULL;

	// Turn refs back into vars to recontextualize

	cell *tmp = c;

	for (unsigned i = 0; i < c->num_cells; i++, tmp++) {
		if (!is_ref(tmp))
			continue;

		tmp->flags &= ~FLAG_VAR_REF;
	}

	return q->varno;
}

static cell *copy_term_to_tmp_with_replacement(query *q, cell *p1, pl_ctx p1_ctx, bool copy_attrs, cell *from, pl_ctx from_ctx, cell *to, pl_ctx to_ctx)
{
	cell *c = deref(q, p1, p1_ctx);
	pl_ctx c_ctx = q->latest_ctx;
	cell *tmp = clone_term_to_tmp(q, c, c_ctx);

	if (!tmp)
		return NULL;

	bool created = false;

	if (!q->vars) {
		q->vars = sl_create(NULL, NULL, NULL);
		created = true;
		const frame *f = GET_CURR_FRAME();
		q->varno = f->actual_slots;
		q->tab_idx = 0;
	}

	bool ok = copy_vars(q, tmp, copy_attrs, from, from_ctx, to, to_ctx);

	if (created) {
		sl_destroy(q->vars);
		q->vars = NULL;
	}

	c = tmp;

	for (pl_idx i = 0; i < tmp->num_cells; i++, c++) {
		if (is_var(c) && c->tmp_attrs) {
			const frame *f = GET_FRAME(c->val_ctx);
			slot *e = get_slot(q, f, c->var_num);
			e->c.val_attrs = c->tmp_attrs;
			c->tmp_attrs = NULL;
			add_trail(q, c->val_ctx, c->var_num, NULL);
		}
	}

	return ok ? tmp : NULL;
}

cell *copy_term_to_tmp(query *q, cell *p1, pl_ctx p1_ctx, bool copy_attrs)
{
	q->has_vars = false;
	return copy_term_to_tmp_with_replacement(q, p1, p1_ctx, copy_attrs, NULL, 0, NULL, 0);
}

cell *alloc_heap(query *q, unsigned num_cells)
{
	size_t page_size = q->heap_pages ? q->heap_pages->page_size * 2 : q->heap_size;

	if (!q->heap_pages || ((q->st.hp + num_cells) >= q->heap_pages->page_size))  {
		page *a = calloc(1, sizeof(page));
		if (!a) return NULL;
		a->next = q->heap_pages;
		unsigned n = MAX_OF(page_size, num_cells);
		a->cells = calloc(a->page_size=n, sizeof(cell));
		if (!a->cells) { free(a); return NULL; }
		a->num = ++q->st.heap_num;
		q->heap_pages = a;
		q->st.hp = 0;
	}

	if (q->st.heap_num > q->hw_heap_num)
		q->hw_heap_num = q->st.heap_num;

	cell *c = q->heap_pages->cells + q->st.hp;
	q->st.hp += num_cells;
	q->heap_pages->idx = q->st.hp;
	return c;
}

void trim_heap(query *q)
{
	for (page *a = q->heap_pages; a;) {
		if (a->num <= q->st.heap_num)
			break;

		cell *c = a->cells;

		for (pl_idx i = 0; i < a->idx; i++, c++)
			unshare_cell(c);

		page *save = a;
		q->heap_pages = a = a->next;
		free(save->cells);
		free(save);
	}

	if (!q->heap_pages)
		return;

	while (q->heap_pages->idx > q->st.hp) {
		cell *c = q->heap_pages->cells + --q->heap_pages->idx;
		unshare_cell(c);
		c->tag = TAG_EMPTY;
		c->flags = 0;
		c->val_attrs = NULL;
	}
}

cell *clone_term_to_heap(query *q, cell *p1, pl_ctx p1_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	q->has_vars = false;
	p1 = clone_term_to_tmp(q, p1, p1_ctx);
	if (!p1) return p1;
	cell *tmp = alloc_heap(q, p1->num_cells);
	if (!tmp) return NULL;
	dup_cells(tmp, p1, p1->num_cells);
	return tmp;
}

cell *copy_term_to_heap(query *q, cell *p1, pl_ctx p1_ctx, bool copy_attrs)
{
	if (!init_tmp_heap(q))
		return NULL;

	q->has_vars = false;
	cell *tmp = copy_term_to_tmp_with_replacement(q, p1, p1_ctx, copy_attrs, NULL, 0, NULL, 0);
	if (!tmp) return tmp;
	cell *tmp2 = alloc_heap(q, tmp->num_cells);
	if (!tmp2) return NULL;
	dup_cells(tmp2, tmp, tmp->num_cells);

	if (!copy_attrs)
		return tmp2;

	cell *c = tmp2;

	for (pl_idx i = 0; i < tmp2->num_cells; i++, c++) {
		if (is_var(c) && c->tmp_attrs) {
			const frame *f = GET_FRAME(c->val_ctx);
			slot *e = get_slot(q, f, c->var_num);
			e->c.val_attrs = c->tmp_attrs;
			c->tmp_attrs = NULL;
			add_trail(q, c->val_ctx, c->var_num, NULL);
		}
	}

	return tmp2;
}

cell *copy_term_to_heap_with_replacement(query *q, cell *p1, pl_ctx p1_ctx, bool copy_attrs, cell *from, pl_ctx from_ctx, cell *to, pl_ctx to_ctx)
{
	if (!init_tmp_heap(q))
		return NULL;

	cell *tmp = copy_term_to_tmp_with_replacement(q, p1, p1_ctx, copy_attrs, is_var(from)?from:NULL, from_ctx, is_var(to)?to:NULL, to_ctx);
	if (!tmp) return tmp;
	cell *tmp2 = alloc_heap(q, tmp->num_cells);
	if (!tmp2) return NULL;
	dup_cells(tmp2, tmp, tmp->num_cells);

	if (!copy_attrs)
		return tmp2;

	cell *c = tmp2;

	for (pl_idx i = 0; i < tmp2->num_cells; i++, c++) {
		if (is_var(c) && c->tmp_attrs) {
			const frame *f = GET_FRAME(c->val_ctx);
			slot *e = get_slot(q, f, c->var_num);
			e->c.val_attrs = c->tmp_attrs;
			c->tmp_attrs = NULL;
		}
	}

	return tmp2;
}

void fix_list(cell *c)
{
	pl_idx cnt = c->num_cells;

	while (is_iso_list(c)) {
		c->num_cells = cnt;
		c = c + 1;					// skip .
		cnt -= 1 + c->num_cells;
		c = c + c->num_cells;		// skip head
	}
}

// Defer check until end_list()

cell *allocate_list(query *q, const cell *c)
{
	if (!init_tmp_heap(q))
		return NULL;

	append_list(q, c);
	return get_tmp_heap(q, 0);
}

// Defer check until end_list()

cell *append_list(query *q, const cell *c)
{
	cell *tmp = alloc_tmp(q, 1+c->num_cells);
	if (!tmp) return NULL;
	cell *save = tmp;
	tmp->tag = TAG_INTERNED;
	tmp->num_cells = 1 + c->num_cells;
	tmp->val_off = g_dot_s;
	tmp->arity = 2;
	tmp->flags = 0;
	tmp++;
	copy_cells(tmp, c, c->num_cells);
	return save;
}

cell *end_list(query *q)
{
	cell *tmp = alloc_tmp(q, 1);
	if (!tmp) return NULL;
	tmp->tag = TAG_INTERNED;
	tmp->num_cells = 1;
	tmp->val_off = g_nil_s;
	tmp->arity = tmp->flags = 0;
	pl_idx num_cells = tmp_heap_used(q);

	if (is_nil(get_tmp_heap(q, 0))) {
		init_tmp_heap(q);
		return make_nil();
	}

	tmp = alloc_heap(q, num_cells);
	if (!tmp) return NULL;
	dup_cells(tmp, get_tmp_heap(q, 0), num_cells);
	tmp->num_cells = num_cells;
	fix_list(tmp);
	init_tmp_heap(q);
	return tmp;
}

cell *end_list_unsafe(query *q)
{
	cell *tmp = alloc_tmp(q, 1);
	if (!tmp) return NULL;
	tmp->tag = TAG_INTERNED;
	tmp->num_cells = 1;
	tmp->val_off = g_nil_s;
	tmp->arity = tmp->flags = 0;
	pl_idx num_cells = tmp_heap_used(q);

	tmp = alloc_heap(q, num_cells);
	if (!tmp) return NULL;
	copy_cells(tmp, get_tmp_heap(q, 0), num_cells);
	tmp->num_cells = num_cells;
	fix_list(tmp);
	return tmp;
}

// Defer check until end_list()

cell *allocate_structure(query *q, const char *functor, const cell *c)
{
	if (!init_tmp_heap(q))
		return NULL;

	cell *tmp = alloc_tmp(q, 1);
	if (!tmp) return NULL;
	tmp->tag = TAG_INTERNED;
	tmp->num_cells = 1;
	tmp->val_off = new_atom(q->pl, functor);
	tmp->arity = 0;
	tmp->flags = 0;
	append_structure(q, c);
	return get_tmp_heap(q, 0);
}

// Defer check until end_list()

cell *append_structure(query *q, const cell *c)
{
	cell *tmp = alloc_tmp(q, c->num_cells);
	if (!tmp) return NULL;
	copy_cells(tmp, c, c->num_cells);
	tmp = q->tmp_heap;
	tmp->arity++;
	return tmp;
}

cell *end_structure(query *q)
{
	pl_idx num_cells = tmp_heap_used(q);
	cell *tmp = alloc_heap(q, num_cells);
	if (!tmp) return NULL;
	dup_cells(tmp, get_tmp_heap(q, 0), num_cells);
	tmp->num_cells = num_cells;

	if (q->tmp_heap && (q->tmph_size > 1000)) {
		free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = 1000;
	}

	return tmp;
}

cell *alloc_queuen(query *q, unsigned qnum, const cell *c)
{
	if (!q->queue[qnum]) {
		q->queue[qnum] = malloc(sizeof(cell)*q->q_size[qnum]);
		if (!q->queue[qnum]) return NULL;
	}

	while ((q->qp[qnum]+c->num_cells) >= q->q_size[qnum]) {
		size_t n = q->q_size[qnum] + q->q_size[qnum] / 2;
		void *ptr = realloc(q->queue[qnum], sizeof(cell)*n);
		if (!ptr) return NULL;
		q->queue[qnum] = ptr;
		q->q_size[qnum] = n;
	}

	cell *dst = q->queue[qnum] + q->qp[qnum];
	q->qp[qnum] += dup_cells(dst, c, c->num_cells);
	q->qcnt[qnum]++;
	return dst;
}

