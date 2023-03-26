#pragma once

#include "internal.h"

#define DUMP_ERRS 0

parser *parser_create(module *m);
void parser_destroy(parser *p);

unsigned tokenize(parser *p, bool args, bool consing);
void reset(parser *p);
void term_to_body(parser *p);
cell *check_body_callable(parser *p, cell *c);
bool run(parser *p, const char *src, bool dump, query **subq, unsigned int yield_time_in_ms);
char *eat_space(parser *p);
bool virtual_term(parser *p, const char *src);
bool get_token(parser *p, bool last_op, bool was_postfix);
void read_integer(parser *p, mp_int v2, int base, const char *src,  const char **srcptr);

void clear_rule(clause *t);
void do_reduce(cell *n);
cell *get_logical_body(cell *c);

#if USE_FFI
void *do_dlopen(const char *filename, int flag);
bool do_register_predicate(module *m, query *q, void *handle, const char *symbol, cell *l, pl_idx_t l_ctx, const char *ret);
bool do_register_struct(module *m, query *q, void *handle, const char *symbol, cell *l, pl_idx_t l_ctx, const char *ret);
int do_dlclose(void *handle);
#endif

extern const char *g_solo;
