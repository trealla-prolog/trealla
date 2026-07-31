#include <ctype.h>
#include <errno.h>
#include <stdlib.h>

#include "prolog.h"
#include "query.h"

struct heap_save {
	cell *heap;
	pl_idx size, hp;
};

size_t alloc_grow(query *q, void **addr, size_t elem_size, size_t min_elements, size_t max_elements)
{
	if (min_elements > max_elements)
		max_elements = min_elements;

	size_t elements = max_elements;
	void *mem = NULL;

	do {
		if (q->pl->limit && (elem_size * elements) > (1024LL*1024*1024*4))
			break;

		mem = TPL_realloc(*addr, elem_size * elements);
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
		q->tmp_heap = TPL_malloc(q->tmph_size * sizeof(cell));
		if (!q->tmp_heap) return NULL;
	}

	q->tmphp = 0;
	return q->tmp_heap;
}

// The tmp heap is used for temporary allocations (a scratch-pad)
// for work in progress. As such it can survive a TPL_realloc() call.
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

static inline bool ref_is_live(const query *q, const cell *c)
{
	if (c->val_ctx >= q->st.fp)
		return false;

	const frame *f = GET_FRAME(c->val_ctx);
	return c->var_num < f->actual_slots;
}

#define deep_copy(c) \
 (!q->noderef || (is_ref(c) && (c->val_ctx <= q->st.cur_ctx) && ref_is_live(q, c) && !is_anon(c)))

// ---------------------------------------------------------------------------
// copy_term pipeline
//
//   1. clone   — structure onto the tmp-heap (FLAG_VAR_REF into source
//                frames). Cycles leave the raw variable as a back-edge.
//   2. rename  — copy_vars() maps source slots → fresh vars (+ optional
//                attributes).
//   3. promote — callers dup the result onto the permanent heap.
//
// Phase 1 is one iterative walk for every compound, lists included.
// ---------------------------------------------------------------------------

// The slot a variable ultimately names, following the chain of refs the
// way deref() does.

static slot *ultimate_slot(const query *q, const cell *c, pl_ctx c_ctx)
{
	if (is_ref(c))
		c_ctx = c->val_ctx;

	const frame *f = GET_FRAME(c_ctx);
	slot *e = get_slot(q, f, c->var_num);

	while (is_var(&e->c)) {
		c_ctx = e->c.val_ctx;
		c = &e->c;

		if (is_ref(c))
			c_ctx = c->val_ctx;

		f = GET_FRAME(c_ctx);
		slot *e2 = get_slot(q, f, c->var_num);

		if (e == e2)
			break;

		e = e2;
	}

	return e;
}

// Whether a variable in the term being copied denotes the same thing as
// the variable copy_term/2 is replacing, so that a reference back to it
// becomes the target variable instead of a newly invented one.
//
// Comparing var_num and context is not enough, and neither is comparing
// slots. Given
//
//     cyclic(X) :- X = f(g(X,_),_).
//     wrap(X, Y) :- copy_term(X, Y).
//
// the back-reference inside the term names the slot of whoever built it
// (cyclic/1's X), while copy_term/2 is handed the term through another
// slot again (wrap/2's X), arguments being passed by value. All three
// name the same term, which is what makes the reference a cycle, so a
// bound variable is compared by the term it denotes. Getting this wrong
// left copy_term/2 with nothing to replace, and it then invented a
// variable for the back-reference: an acyclic unrolling of a cyclic
// term, with one variable too many (#1002).
//
// A term is a cell together with a context, and both halves have to
// match: activations of a recursive clause share its cells and are told
// apart only by their context.

static bool denotes_same(query *q, cell *c, pl_ctx c_ctx, const cell *from_val, pl_ctx from_val_ctx, const slot *from_e)
{
	if (!from_val)
		return ultimate_slot(q, c, c_ctx) == from_e;

	const pl_ctx save_latest_ctx = q->latest_ctx;
	const cell *val = deref(q, c, c_ctx);
	const pl_ctx val_ctx = q->latest_ctx;
	q->latest_ctx = save_latest_ctx;
	return (val == from_val) && (val_ctx == from_val_ctx);
}

// Whether a dereferenced argument is a reference back to the whole term
// being copied. Only the raw argument tells us: dereferencing it lands
// on the term itself, and it is the variable that named it which has to
// be kept, for copy_vars() to turn into the target variable.
//
// The context matters as much as the cell: the arguments of a recursive
// clause's head are the same cells at every depth, and comparing cells
// alone takes the deeper one for the term the copy started from.

static bool cycles_back(const query *q, const cell *c, pl_ctx c_ctx)
{
	return q->clone_root && (c == q->clone_root) && (c_ctx == q->clone_root_ctx);
}

static bool is_dump_root_var(const query *q, const cell *raw, pl_ctx raw_ctx)
{
	return is_var(raw)
		&& (raw->var_num == q->dump_var_num)
		&& (raw_ctx == q->dump_var_ctx);
}

static cell *clone_leaf_to_tmp(query *q, cell *c, pl_ctx c_ctx)
{
	cell *tmp = alloc_tmp(q, 1);
	if (!tmp) return NULL;
	copy_cells(tmp, c, 1);

	if (is_var(c))
		q->has_vars = true;

	if (is_var(tmp) && !is_ref(tmp) && !q->noderef) {
		tmp->flags |= FLAG_VAR_REF;
		tmp->val_ctx = c_ctx;
	}

	return tmp;
}

// One unfinished compound on the clone walk. 'e'/'save_vgen' belong to
// the parent argument slot that caused the descent; restored when this
// subtree is done.

typedef struct {
	lnode hdr;
	cell *arg;
	pl_ctx arg_ctx;
	int arity_left;
	pl_idx save_idx;
	slot *e;
	uint32_t save_vgen;
} clone_frame;

static void clone_stack_clear(list *stack)
{
	clone_frame *n;

	while ((n = (clone_frame*)list_pop_back(stack)) != NULL)
		TPL_free(n);
}

// Deref one argument with cycle detection. On a back-edge to the clone
// root (or the dump-root variable), keep the raw variable so phase 2 can
// rename it.

static void clone_resolve_arg(query *q, cell *raw, pl_ctx raw_ctx,
	cell **out_c, pl_ctx *out_ctx, slot **out_e, uint32_t *out_save_vgen, bool *back_edge)
{
	cell *c = raw;
	pl_ctx c_ctx = raw_ctx;
	slot *e = NULL;
	uint32_t save_vgen = 0;
	*back_edge = false;

	bool any = false;
	int both = 0;

	if (deep_copy(raw))
		DEREF_CHECKED(any, both, save_vgen, e, e->vgen, c, c_ctx, q->vgen);

	if (both)
		q->cycle_error = true;

	if (is_var(raw) && cycles_back(q, c, c_ctx)) {
		c = raw;
		c_ctx = raw_ctx;
		*back_edge = true;
		q->cycle_error = true;
	} else if (is_dump_root_var(q, raw, raw_ctx)) {
		c = raw;
		c_ctx = raw_ctx;
		*back_edge = true;
		q->cycle_error = true;
	}

	*out_c = c;
	*out_ctx = c_ctx;
	*out_e = e;
	*out_save_vgen = save_vgen;
}

static cell *clone_term_to_tmp_internal(query *q, cell *p1, pl_ctx p1_ctx)
{
	pl_idx root_idx = tmp_heap_used(q);

	if (!is_compound(p1))
		return clone_leaf_to_tmp(q, p1, p1_ctx);

	cell *hdr = alloc_tmp(q, 1);
	if (!hdr) return NULL;
	copy_cells(hdr, p1, 1);

	list stack = {0};
	clone_frame *n = TPL_malloc(sizeof(clone_frame));
	if (!n) return NULL;
	n->arity_left = p1->arity;
	n->arg = p1 + 1;
	n->arg_ctx = p1_ctx;
	n->save_idx = root_idx;
	n->e = NULL;
	n->save_vgen = 0;
	list_push_back(&stack, n);

	cell *result = NULL;

	while ((n = (clone_frame*)list_back(&stack)) != NULL) {
		if (n->arity_left <= 0) {
			cell *done = get_tmp_heap(q, n->save_idx);
			done->num_cells = tmp_heap_used(q) - n->save_idx;

			if (!q->has_vars)
				done->flags |= FLAG_INTERNED_GROUND;

			result = done;

			slot *pending_e = n->e;
			uint32_t pending_vgen = n->save_vgen;
			list_pop_back(&stack);
			TPL_free(n);

			if (pending_e)
				pending_e->vgen = pending_vgen;

			continue;
		}

		n->arity_left--;
		cell *raw = n->arg;
		pl_ctx raw_ctx = n->arg_ctx;
		n->arg += raw->num_cells;

		cell *c;
		pl_ctx c_ctx;
		slot *e;
		uint32_t save_vgen;
		bool back_edge;
		clone_resolve_arg(q, raw, raw_ctx, &c, &c_ctx, &e, &save_vgen, &back_edge);

		if (back_edge || !is_compound(c)) {
			if (!clone_leaf_to_tmp(q, c, c_ctx)) {
				clone_stack_clear(&stack);
				return NULL;
			}

			if (e)
				e->vgen = save_vgen;

			continue;
		}

		pl_idx child_idx = tmp_heap_used(q);
		cell *child = alloc_tmp(q, 1);

		if (!child) {
			clone_stack_clear(&stack);
			return NULL;
		}

		copy_cells(child, c, 1);

		clone_frame *cn = TPL_malloc(sizeof(clone_frame));

		if (!cn) {
			clone_stack_clear(&stack);
			return NULL;
		}

		cn->arity_left = c->arity;
		cn->arg = c + 1;
		cn->arg_ctx = c_ctx;
		cn->save_idx = child_idx;
		cn->e = e;
		cn->save_vgen = save_vgen;
		list_push_back(&stack, cn);
	}

	return result;
}

cell *clone_term_to_tmp(query *q, cell *p1, pl_ctx p1_ctx)
{
	if (++q->vgen == 0) q->vgen = 1;
	q->has_vars = false;
	return clone_term_to_tmp_internal(q, p1, p1_ctx);
}

cell *append_to_tmp(query *q, cell *p1, pl_ctx p1_ctx)
{
	cell *tmp = alloc_tmp(q, p1->num_cells);
	if (!tmp) return NULL;
	copy_cells_by_ref(tmp, p1, p1_ctx, p1->num_cells);
	return tmp;
}

static int accum_slot(query *q, size_t slot_nbr, unsigned var_num)
{
	const void *vnbr;

	if (q->vars && sl_get(q->vars, (void*)slot_nbr, &vnbr))
		return (unsigned)(size_t)vnbr;

	if (!q->vars)
		q->vars = sl_create(NULL, NULL, NULL);

	sl_app(q->vars, (void*)slot_nbr, (void*)(size_t)var_num);
	return -1;
}

static bool copy_vars(query *q, cell *c, bool copy_attrs, cell *from, pl_ctx from_ctx, cell *to, pl_ctx to_ctx)
{
	unsigned num_cells = c->num_cells;
	unsigned cnt = 0;
	const slot *from_e = NULL;			// the slot 'from' names
	cell *from_val = NULL;				// the term it denotes, if bound
	pl_ctx from_val_ctx = 0;

	if (from) {
		const pl_ctx save_latest_ctx = q->latest_ctx;
		from_val = deref(q, from, from_ctx);
		from_val_ctx = q->latest_ctx;
		q->latest_ctx = save_latest_ctx;
		from_e = ultimate_slot(q, from, from_ctx);

		if (is_var(from_val))
			from_val = NULL;
	}

	for (unsigned i = 0; i < num_cells; i++, c++) {
		if (!is_ref(c))
			continue;

		c->flags |= FLAG_VAR_LOCAL;

		if (from && denotes_same(q, c, c->val_ctx, from_val, from_val_ctx, from_e)) {
			c->var_num = to->var_num;
			c->val_ctx = to_ctx;

			// BUGFIX: the replacement fast-path must still carry over
			// attributes from the source variable onto the target.

			if (copy_attrs && !c->tmp_attrs) {
				cell *attrs = from_e->c.val_attrs;

				if (attrs) {
					cell *save_tmp_heap = q->tmp_heap;
					pl_idx save_tmp_hp = q->tmphp;
					q->tmp_heap = NULL;
					cell *tmp = copy_term_to_heap(q, attrs, q->st.cur_ctx, false);
					CHECKED(tmp);
					c->tmp_attrs = tmp;
					TPL_free(q->tmp_heap);
					q->tmp_heap = save_tmp_heap;
					q->tmphp = save_tmp_hp;
				}
			}
		} else {
			const frame *f = GET_FRAME(c->val_ctx);
			// NB. get_ordered_slot_num is pure arithmetic (no deref), so
			// it is safe even when c->val_ctx names a long-dead frame, as
			// happens when rebasing an imported (detached) term image.
			// Only consult the slot itself when attributes are wanted:
			// dereferencing a dead frame's slot is undefined.
			const size_t slot_nbr = get_ordered_slot_num(q, f, c->var_num);
			cell *attrs = NULL;

			if (copy_attrs) {
				const slot *e = get_slot(q, f, c->var_num);
				attrs = c->tmp_attrs ? c->tmp_attrs : e->c.val_attrs;
			}
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
						from ?copy_term_to_heap_with_replacement(q, attrs, q->st.cur_ctx, false, from, from_ctx, to, to_ctx)
						:copy_term_to_heap(q, attrs, q->st.cur_ctx, false);
					CHECKED(tmp);
					c->tmp_attrs = tmp;
				}

				TPL_free(q->tmp_heap);
				q->tmp_heap = save_tmp_heap;
				q->tmphp = save_tmp_hp;
			}
		}
	}

	return true;
}

unsigned rebase_term(query *q, cell *c, unsigned start_nbr, bool copy_attrs)
{
	q->vars = NULL;
	q->varno = start_nbr;
	q->tab_idx = 0;

	if (!copy_vars(q, c, copy_attrs, NULL, 0, NULL, 0)) {
		if (q->vars)
			sl_destroy(q->vars);

		q->vars = NULL;
		return q->varno;
	}

	if (q->vars) {
		sl_destroy(q->vars);
		q->vars = NULL;
	}

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

	// Have the walk stop at references back to the whole term, but only
	// when there is a target variable for such a reference to become:
	// without one it has to be a newly invented variable wherever the
	// cycle is caught, and catching it later at least keeps a level of
	// the structure.

	cell *save_root = q->clone_root;
	pl_ctx save_root_ctx = q->clone_root_ctx;
	q->clone_root = (from && to && is_compound(c)) ? c : NULL;
	q->clone_root_ctx = c_ctx;
	cell *tmp = clone_term_to_tmp(q, c, c_ctx);
	q->clone_root = save_root;
	q->clone_root_ctx = save_root_ctx;

	if (!tmp)
		return NULL;

	bool created = false;

	if (!q->vars) {
		created = true;
		const frame *f = GET_CURR_FRAME();
		q->varno = f->actual_slots;
		q->tab_idx = 0;
	}

	bool ok = copy_vars(q, tmp, copy_attrs, from, from_ctx, to, to_ctx);

	if (created) {
		if (q->vars)
			sl_destroy(q->vars);

		q->vars = NULL;
	}

	c = tmp;

	for (pl_idx i = 0; i < tmp->num_cells; i++, c++) {
		if (is_var(c) && copy_attrs && c->tmp_attrs) {
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
	size_t page_size = q->heap_pages ? q->heap_pages->page_size*2 : q->heap_size;

	if (!q->heap_pages || ((q->st.hp + num_cells) >= q->heap_pages->page_size))  {
		page *a = TPL_calloc(1, sizeof(page));
		if (!a) return NULL;
		a->next = q->heap_pages;
		unsigned n = MAX_OF(page_size, num_cells);
		a->cells = TPL_calloc(a->page_size=n, sizeof(cell));
		if (!a->cells) { TPL_free(a); return NULL; }
		a->num = q->st.hp_num++;
		q->heap_pages = a;
		q->st.hp = 0;
	}

	cell *c = q->heap_pages->cells + q->st.hp;
	q->st.hp += num_cells;
	q->heap_pages->idx = q->st.hp;
	return c;
}

void trim_heap(query *q)
{
	for (page *a = q->heap_pages; a;) {
		if (a->num <= q->st.hp_num)
			break;

		cell *c = a->cells;

		for (pl_idx i = 0; i < a->idx; i++, c++)
			unshare_cell(c);

		page *save = a;
		q->heap_pages = a = a->next;
		TPL_free(save->cells);
		TPL_free(save);
	}

	if (!q->heap_pages)
		return;

	while (q->heap_pages->idx > q->st.hp) {
		cell *c = q->heap_pages->cells + --q->heap_pages->idx;
		unshare_cell(c);
		c->tag = TAG_EMPTY;
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
		if (is_var(c) && copy_attrs && c->tmp_attrs) {
			const frame *f = GET_FRAME(c->val_ctx);
			slot *e = get_slot(q, f, c->var_num);
			e->c.val_attrs = c->tmp_attrs;
			c->tmp_attrs = NULL;
		}
	}

	return tmp2;
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
		if (is_var(c) && copy_attrs && c->tmp_attrs) {
			const frame *f = GET_FRAME(c->val_ctx);
			slot *e = get_slot(q, f, c->var_num);
			e->c.val_attrs = c->tmp_attrs;
			c->tmp_attrs = NULL;
			add_trail(q, c->val_ctx, c->var_num, NULL);
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

cell *allocate_list(query *q, const cell *c)
{
	if (!init_tmp_heap(q))
		return NULL;

	append_list(q, c);
	return get_tmp_heap(q, 0);
}

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

	if (is_nil(get_tmp_heap(q, 0))) {
		init_tmp_heap(q);
		return make_nil();
	}

	pl_idx num_cells = tmp_heap_used(q);
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

	if (is_nil(get_tmp_heap(q, 0))) {
		init_tmp_heap(q);
		return make_nil();
	}
	pl_idx num_cells = tmp_heap_used(q);
	tmp = alloc_heap(q, num_cells);
	if (!tmp) return NULL;
	copy_cells(tmp, get_tmp_heap(q, 0), num_cells);
	tmp->num_cells = num_cells;
	fix_list(tmp);
	init_tmp_heap(q);
	return tmp;
}

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

cell *append_structure(query *q, const cell *c)
{
	cell *tmp = alloc_tmp(q, c->num_cells);
	if (!tmp) return NULL;
	copy_cells(tmp, c, c->num_cells);
	tmp = q->tmp_heap;
	tmp->arity++;
	return tmp;
}

cell *end_structure_heap(query *q)
{
	pl_idx num_cells = tmp_heap_used(q);
	cell *tmp = alloc_heap(q, num_cells);
	if (!tmp) return NULL;
	dup_cells(tmp, get_tmp_heap(q, 0), num_cells);
	tmp->num_cells = num_cells;

	if (q->tmp_heap && (q->tmph_size > 1000)) {
		TPL_free(q->tmp_heap);
		q->tmp_heap = NULL;
		q->tmph_size = 1000;
	}

	return tmp;
}

cell *alloc_queuen(query *q, unsigned qnum, const cell *c)
{
	if (!q->queue[qnum]) {
		q->queue[qnum] = TPL_malloc(sizeof(cell)*q->q_size[qnum]);
		if (!q->queue[qnum]) return NULL;
	}

	while ((q->qp[qnum]+c->num_cells) >= q->q_size[qnum]) {
		size_t n = q->q_size[qnum] + q->q_size[qnum] / 2;
		void *ptr = TPL_realloc(q->queue[qnum], sizeof(cell)*n);
		if (!ptr) return NULL;
		q->queue[qnum] = ptr;
		q->q_size[qnum] = n;
	}

	cell *dst = q->queue[qnum] + q->qp[qnum];
	q->qp[qnum] += dup_cells(dst, c, c->num_cells);
	q->qcnt[qnum]++;
	return dst;
}

cell *import_term(query *q, cell *c, pl_ctx c_ctx)
{
	cell *tmp = TPL_malloc(sizeof(cell) * c->num_cells);
	if (!tmp) return NULL;
	dup_cells_by_ref(tmp, c, c_ctx, c->num_cells);
	const frame *f = GET_CURR_FRAME();
	rebase_term(q, tmp, f->actual_slots, false);
	undo_on_backtrack(q, tmp, UNDO_CELLS);
	return tmp;
}
