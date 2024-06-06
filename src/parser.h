#pragma once

#include "internal.h"

#define DUMP_ERRS 0

parser *parser_create(module *m);
void parser_destroy(parser *p);

unsigned tokenize(parser *p, bool args, bool consing);
void reset(parser *p);
void term_to_body(parser *p);
cell *check_body_callable(cell *c);
bool run(parser *p, const char *src, bool dump, query **subq, unsigned int yield_time_in_ms);
char *eat_space(parser *p);
bool virtual_term(parser *p, const char *src);
bool get_token(parser *p, bool last_op, bool was_postfix);
void read_integer(parser *p, mp_int v2, int base, const char **srcptr);

void make_uint(cell *tmp, pl_uint v);
void make_int(cell *tmp, pl_int v);
void make_float(cell *tmp, pl_flt v);
void make_ptr(cell *tmp, void *v);
void make_struct_(cell *tmp, pl_idx offset, unsigned arity, pl_idx extra_cells);
void make_var(cell *tmp, pl_idx off, unsigned var_nbr);
void make_ref(cell *tmp, unsigned var_nbr, pl_idx ctx);
void make_end(cell *tmp);
void make_atom(cell *tmp, pl_idx offset);
cell *make_nil(void);
void make_smalln(cell *tmp, const char *s, size_t n);
bool make_cstringn(cell *tmp, const char *s, size_t n);
bool make_stringn(cell *tmp, const char *s, size_t n);
void make_blob(cell *tmp, void *ptr);
void make_dbref(cell *tmp, void *ptr);
void make_kvref(cell *tmp, void *ptr);

void clear_clause(clause *t);
void do_reduce(cell *n);
cell *get_logical_body(cell *c);

#if USE_FFI
void *do_dlopen(const char *filename, int flag);
bool do_register_predicate(module *m, query *q, void *handle, const char *symbol, cell *l, pl_idx l_ctx, const char *ret);
bool do_register_struct(module *m, query *q, void *handle, const char *symbol, cell *l, pl_idx l_ctx, const char *ret);
int do_dlclose(void *handle);
#endif

#define make_struct(tmp, offset, fn, arity, extra_cells) { \
	cell *tmp_make = tmp; \
	make_struct_(tmp_make, offset, arity, extra_cells); \
	\
	if (fn != NULL) { \
		static builtins *s_fn_ptr_##fn = NULL; \
		if (!s_fn_ptr_##fn) \
			s_fn_ptr_##fn = get_fn_ptr(fn); \
		\
		tmp_make->bif_ptr = s_fn_ptr_##fn; \
		tmp_make->flags = FLAG_BUILTIN; \
	} \
}

extern const char *g_solo;
