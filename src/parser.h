#pragma once

#include "internal.h"

#define DUMP_ERRS 0

parser *create_parser(module *m);
void destroy_parser(parser *p);

unsigned tokenize(parser *p, bool args, bool consing);
void reset(parser *p);
void term_to_body(parser *p);
cell *check_body_callable(parser *p, cell *c);
bool run(parser *p, const char *src, bool dump, query **subquery);
char *eat_space(parser *p);
bool virtual_term(parser *p, const char *src);
bool get_token(parser *p, bool last_op, bool was_postfix);
void read_integer(parser *p, mp_int v2, int base, const char *src,  const char **srcptr);

void clear_rule(clause *t);
void do_reduce(cell *n);
cell *get_body(cell *c);
cell *get_logical_body(cell *c);

inline static bool is_a_rule(const cell *c)
{
	if (is_structure(c) && (c->val_off == g_neck_s) && (c->arity == 2))
		return true;

	return false;
}

inline static cell *get_head(cell *c)
{
	if (is_a_rule(c))
		return c + 1;

	return c;
}

extern const char *g_solo;
