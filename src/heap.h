#pragma once

#include "internal.h"

size_t alloc_grow(void **addr, size_t elem_size, size_t min_elements, size_t max_elements);

cell *append_to_tmp(query *q, cell *p1);
cell *clone_to_tmp(query *q, cell *p1);
cell *clone_to_heap(query *q, bool prefix, cell *p1, unsigned extras);

cell *deep_clone_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx);
cell *deep_clone_to_heap(query *q, cell *p1, pl_idx_t p1_ctx);

cell *deep_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs);
cell *deep_copy_to_heap(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs);

cell *deep_raw_copy_to_tmp(query *q, cell *p1, pl_idx_t p1_ctx);

cell *alloc_on_heap(query *q, unsigned nbr_cells);
cell *alloc_on_tmp(query *q, unsigned nbr_cells);
cell *alloc_on_queuen(query *q, unsigned qnbr, const cell *c);
cell *alloc_on_queuen_unsafe(query *q, unsigned qnbr, const cell *c);

cell *init_tmp_heap(query *q);

// Used for copying attributes and doesn't init tmp heap...
cell *deep_copy_to_heap_with_replacement(query *q, cell *p1, pl_idx_t p1_ctx, bool copy_attrs, cell *from, pl_idx_t from_ctx, cell *to, pl_idx_t to_ctx);

#define get_tmp_heap_start(q) (q)->tmp_heap
#define get_tmp_heap(q,i) ((q)->tmp_heap + (i))
#define tmp_heap_used(q) (q)->tmphp

struct heap_save {
	cell *heap;
	pl_idx_t size, hp;
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

void fix_list(cell *c);
bool is_in_ref_list(cell *c, pl_idx_t c_ctx, reflist *rlist);

void allocate_list(query *q, const cell *c);
void append_list(query *q, const cell *c);
cell *end_list(query *q);
cell *end_list_unsafe(query *q);
