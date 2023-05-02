#pragma once

#include "builtins.h"

query *query_create(module *m, bool sub_query);
query *query_create_subquery(query *q, cell *curr_cell);
void query_destroy(query *q);

bool push_choice(query *q);
bool push_barrier(query *q);
bool push_call_barrier(query *q);
bool push_catcher(query *q, enum q_retry type);

bool do_retract(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type is_retract);
bool do_read_term(query *q, stream *str, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx, char *src);
bool do_yield(query *q, int msecs);
void do_yield_at(query *q, unsigned int time_in_ms);

cell *do_term_variables(query *q, cell *p1, pl_idx_t p1_ctx);
bool query_redo(query *q);
bool has_next_key(query *q);
void next_key(query *q);
void purge_predicate_dirty_list(query *q, predicate *pr);
void purge_dirty_list(query *q);
bool check_slot(query *q, unsigned cnt);
void cut_me(query *q);
void prune_me(query *q, bool soft_cut);
bool execute(query *q, cell *cells, unsigned nbr_vars);
bool fn_call_0(query *q, cell *p1, pl_idx_t p1_ctx);
void undo_me(query *q);
void drop_choice(query *q);
int retry_choice(query *q);
void term_assign_vars(parser *p, unsigned start, bool rebase);
bool start(query *q);
bool match_rule(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type is_retract);
bool match_clause(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type retract);
void try_me(query *q, unsigned vars);
void call_attrs(query *q, cell *attrs);
void stash_me(query *q, const clause *cl, bool last_match);
bool do_post_unification_hook(query *q, bool is_builtin);
bool check_redo(query *q);
void dump_vars(query *q, bool partial);
int check_interrupt(query *q);
bool make_slice(query *q, cell *d, const cell *orig, size_t off, size_t n);

bool find_exception_handler(query *q, char *e);
bool throw_error(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected);
bool throw_error3(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected, cell *goal);
bool throw_error2(query *q, cell *c, pl_idx_t c_ctx, const char *err_type, const char *expected, cell *goal);

size_t scan_is_chars_list2(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes, bool *has_var, bool *is_partial);
size_t scan_is_chars_list(query *q, cell *l, pl_idx_t l_ctx, bool allow_codes);
char *chars_list_to_string(query *q, cell *p_chars, pl_idx_t p_chars_ctx, size_t len);
cell *string_to_chars_list(query *q, cell *p, pl_idx_t p_ctx);

unsigned create_vars(query *q, unsigned nbr);
cell *skip_max_list(query *q, cell *head, pl_idx_t *head_ctx, pl_int_t max, pl_int_t *skip, cell *tmp);
bool is_cyclic_term(query *q, cell *p1, pl_idx_t p1_ctx);
bool is_acyclic_term(query *q, cell *p1, pl_idx_t p1_ctx);
bool do_format(query *q, cell *str, pl_idx_t str_ctx, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx);
size_t slicecpy(char *dst, size_t dstlen, const char *src, size_t len);
int get_stream(query *q, cell *p1);
bool call_builtin(query *q, cell *c, pl_idx_t c_ctx);
bool call_userfun(query *q, cell *c, pl_idx_t c_ctx);
void do_cleanup(query *q, cell *p1, pl_idx_t c_ctx);
bool drop_barrier(query *q);
void collect_vars(query *q, cell *p1, pl_idx_t p1_ctx);
bool check_list(query *q, cell *p1, pl_idx_t p1_ctx, bool *is_partial, pl_int_t *skip);
bool parse_write_params(query *q, cell *c, pl_idx_t c_ctx, cell **vnames, pl_idx_t *vnames_ctx);
bool has_vars(query *q, cell *p1, pl_idx_t p1_ctx);
bool accum_var(query *q, const cell *c, pl_idx_t c_ctx);
bool do_parse_csv_line(query *q, int sep, int quote, bool trim, bool numbers, bool use_strings, unsigned arity, const char *functor, const char *src, cell *p2, pl_idx_t p2_ctx);

int compare(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx);
bool unify(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx);

ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx_t c_ctx, int running, bool cons, unsigned depth);
bool print_term(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running);
bool print_term_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running);
char *print_term_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running);
void clear_write_options(query *q);

bool print_canonical(query *q, FILE *fp, cell *c, pl_idx_t c_ctx, int running);
char *print_canonical_to_strbuf(query *q, cell *c, pl_idx_t c_ctx, int running);
bool print_canonical_to_stream(query *q, stream *str, cell *c, pl_idx_t c_ctx, int running);

void dump_term(query *q, const char *s, const cell *c);

bool fn_sys_drop_barrier_0(query *q);
bool fn_iso_throw_1(query *q);
bool fn_sys_call_cleanup_3(query *q);
bool fn_iso_catch_3(query *q);
bool fn_sys_block_catcher_0(query *q);
bool fn_iso_negation_1(query *q);
bool fn_iso_conjunction_2(query *q);
bool fn_iso_disjunction_2(query *q);
bool fn_if_3(query *q);
bool fn_if_2(query *q);
bool fn_ignore_1(query *q);

bool fn_iso_if_then_2(query *q);
bool fn_iso_invoke_2(query *q);
bool fn_iso_once_1(query *q);
bool fn_iso_call_1(query *q);
bool fn_iso_call_n(query *q);
bool fn_iso_cut_0(query *q);
bool fn_sys_prune_0(query *q);
bool fn_sys_prune_1(query *q);
bool fn_iso_fail_0(query *q);
bool fn_iso_true_0(query *q);
bool fn_sys_undo_trail_1(query *q);
bool fn_sys_redo_trail_0(query *q);
bool fn_sys_soft_prune_0(query *q);
bool fn_iso_unify_2(query *q);
bool fn_sys_block_catcher_1(query *q);
bool fn_sys_cleanup_if_det_0(query *q);
bool fn_sys_cut_if_det_0(query *q);
bool fn_sys_queuen_2(query *q);
bool fn_iso_findall_3(query *q);
bool fn_iso_bagof_3(query *q);

cell *convert_to_list(query *q, cell *c, pl_idx_t nbr_cells);

#define FEOF(str) feof(str->fp) && !str->ungetch

#ifdef _WIN32
#include <io.h>
#endif

#define PROMPT ""

#define DUMP_TERM(s,c,c_ctx,running) {				\
	printf("*** %s ", s);							\
	q->quoted = true;								\
	print_term(q, stdout, c, c_ctx, running);		\
	q->quoted = false;								\
	printf("\n");									\
}

#define CHECK_INTERRUPT()							\
	if (g_tpl_interrupt) {							\
		if (check_interrupt(q))						\
			break;									\
	}

inline static bool make_cstring(cell *d, const char *s)
{
	return make_cstringn(d, s, strlen(s));
}

inline static bool make_string(cell *d, const char *s)
{
	return make_stringn(d, s, strlen(s));
}

inline static bool is_a_rule(const cell *c)
{
	return is_interned(c) && (c->arity == 2) && (c->val_off == g_neck_s);
}

inline static cell *get_head(cell *c)
{
	return is_a_rule(c) ? c + 1 : c;
}

inline static cell *get_body(cell *c)
{
	if (is_a_rule(c)) {
		cell *h = c + 1;
		cell *b = h + h->nbr_cells;

		if (is_end(b))
			return NULL;

		return b;
	}

	return NULL;
}

#define init_cell(c) { 				\
	(c)->tag = TAG_EMPTY;			\
	(c)->flags = 0;					\
	(c)->nbr_cells = 0;				\
	(c)->arity = 0;					\
	(c)->attrs = NULL;				\
}

