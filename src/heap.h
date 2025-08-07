#pragma once

#define CALL_SKIP false
#define CALL_NOSKIP true

size_t alloc_grow(query *q, void **addr, size_t elem_size, size_t min_elements, size_t max_elements, bool zeroit);

cell *alloc_on_tmp(query *q, unsigned num_cells);
cell *append_to_tmp(query *q, cell *p1, pl_idx p1_ctx);
cell *clone_term_to_tmp(query *q, cell *p1, pl_idx p1_ctx);
cell *copy_term_to_tmp(query *q, cell *p1, pl_idx p1_ctx, bool copy_attrs);

#define get_tmp_heap_start(q) (q)->tmp_heap
#define get_tmp_heap(q,i) ((q)->tmp_heap + (i))
#define tmp_heap_used(q) (q)->tmphp

cell *alloc_on_heap(query *q, unsigned num_cells);
cell *clone_term_to_heap(query *q, cell *p1, pl_idx p1_ctx);
cell *copy_term_to_heap(query *q, cell *p1, pl_idx p1_ctx, bool copy_attrs);
void trim_heap(query *q);

cell *alloc_on_queuen(query *q, unsigned qnum, const cell *c);

void fix_list(cell *c);
unsigned rebase_term(query *q, cell *c, unsigned start_nbr);

// These allocate on the heap...

cell *allocate_structure(query *q, const char *functor, const cell *c);
cell *append_structure(query *q, const cell *c);
cell *end_structure(query *q);

cell *allocate_list(query *q, const cell *c);
cell *append_list(query *q, const cell *c);
cell *end_list_unsafe(query *q);
cell *end_list(query *q);

cell *init_tmp_heap(query *q);
