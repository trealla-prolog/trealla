#pragma once

#include "parser.h"
#include "builtins.h"

typedef struct {
	int sep, quote;
	unsigned arity;
	bool trim, numbers, use_strings;
	const char *functor;
} csv;

query *query_create(module *m);
query *query_create_subquery(query *q, cell *instr);
query *query_create_task(query *q, cell *instr);
void query_destroy(query *q);

bool push_choice(query *q);
bool push_barrier(query *q);
bool push_succeed_on_retry_with_barrier(query *q, pl_idx skip);
bool push_succeed_on_retry(query *q, pl_idx skip);
bool push_fail_on_retry_with_barrier(query *q);
bool push_reset_handler(query *q);
bool push_catcher(query *q, enum q_retry type);

bool do_retract(query *q, cell *p1, pl_ctx p1_ctx, enum clause_type is_retract);
bool do_read_term(query *q, stream *str, cell *p1, pl_ctx p1_ctx, cell *p2, pl_ctx p2_ctx, char *src);
bool do_yield(query *q, int msecs);
void do_yield_at(query *q, unsigned int time_in_ms);

bool check_slot(query *q, unsigned cnt);
bool check_trail(query *q);

char *url_encode(const char *src, int len, char *dstbuf);
char *url_decode(const char *src, char *dstbuf);
bool query_redo(query *q);
bool has_next_key(query *q);
void cut(query *q);
bool execute(query *q, cell *cells, unsigned num_vars);
void undo_me(query *q);
int retry_choice(query *q);
void assign_vars(parser *p, unsigned start, bool rebase);
bool start(query *q);
bool match_rule(query *q, cell *p1, pl_ctx p1_ctx, enum clause_type is_retract);
bool match_clause(query *q, cell *p1, pl_ctx p1_ctx, enum clause_type retract);
void try_me(query *q, unsigned vars);
void call_attrs(query *q, cell *attrs);
void stash_frame(query *q, const clause *cl, bool last_match);
bool check_redo(query *q);
void dump_vars(query *q, bool partial);
int check_interrupt(query *q);
bool make_slice(query *q, cell *d, const cell *orig, size_t off, size_t n);
void check_pressure(query *q);
cell *prepare_call(query *q, bool noskip, cell *p1, pl_ctx p1_ctx, unsigned extras);
bool call_check(query *q, cell *tmp2, bool *status, bool calln);
bool make_slice(query *q, cell *d, const cell *orig, size_t off, size_t n);
bool match_head(query *q);
bool check_frame(query *q, unsigned max_vars);

bool throw_error(query *q, cell *c, pl_ctx c_ctx, const char *err_type, const char *expected);
bool throw_error3(query *q, cell *c, pl_ctx c_ctx, const char *err_type, const char *expected, cell *goal);
bool throw_error2(query *q, cell *c, pl_ctx c_ctx, const char *err_type, const char *expected, cell *goal);

size_t scan_is_chars_list2(query *q, cell *l, pl_ctx l_ctx, bool allow_codes, bool *has_var, bool *is_partial, cell **);
size_t scan_is_chars_list(query *q, cell *l, pl_ctx l_ctx, bool allow_codes);
char *chars_list_to_string(query *q, cell *p_chars, pl_ctx p_chars_ctx);
cell *string_to_chars_list(query *q, cell *p, pl_ctx p_ctx);

int create_vars(query *q, unsigned cnt);
cell *skip_max_list(query *q, cell *head, pl_ctx *head_ctx, pl_int max, pl_int *skip, cell *tmp);
bool is_cyclic_term(query *q, cell *p1, pl_ctx p1_ctx);
bool is_acyclic_term(query *q, cell *p1, pl_ctx p1_ctx);
bool do_format(query *q, cell *str, pl_ctx str_ctx, cell *p1, pl_ctx p1_ctx, cell *p2, pl_ctx p2_ctx);
size_t slicecpy(char *dst, size_t dstlen, const char *src, size_t len);
int new_stream(prolog *pl);
int get_stream(query *q, cell *p1);
int get_named_stream(prolog *pl, const char *name, size_t len);
void do_cleanup(query *q, cell *p1, pl_ctx c_ctx);
bool drop_barrier(query *q, pl_idx cp);
void collect_vars(query *q, cell *p1, pl_ctx p1_ctx);
bool check_list(query *q, cell *p1, pl_ctx p1_ctx, bool *is_partial, pl_int *skip);
bool parse_write_params(query *q, cell *c, pl_ctx c_ctx, cell **vnames, pl_ctx *vnames_ctx);
bool has_vars(query *q, cell *p1, pl_ctx p1_ctx);
void add_trail(query *q, pl_ctx c_ctx, unsigned c_var_nbr, cell *attrs);
void reset_var(query *q, const cell *c, pl_ctx c_ctx, cell *v, pl_ctx v_ctx);
void undo_var(query *q, const cell *c, pl_ctx c_ctx);
bool valid_list(query *q, cell *c, pl_ctx c_ctx);
void make_call(query *q, cell *tmp);
void make_call_redo(query *q, cell *tmp);
bool do_post_unify_hook(query *q, bool is_builtin);
bool any_attributed(query *q);
bool do_load_file(query *q, cell *p1, pl_ctx p1_ctx);

#if USE_THREADS
bool do_signal(query *q, void *thread_ptr);
#endif

int compare(query *q, cell *p1, pl_ctx p1_ctx, cell *p2, pl_ctx p2_ctx);
bool unify(query *q, cell *p1, pl_ctx p1_ctx, cell *p2, pl_ctx p2_ctx);

bool print_term(query *q, FILE *fp, cell *c, pl_ctx c_ctx, int running);
bool print_term_to_stream(query *q, stream *str, cell *c, pl_ctx c_ctx, int running);
char *print_term_to_strbuf(query *q, cell *c, pl_ctx c_ctx, int running);
void clear_write_options(query *q);

bool print_canonical(query *q, FILE *fp, cell *c, pl_ctx c_ctx, int running);
char *print_canonical_to_strbuf(query *q, cell *c, pl_ctx c_ctx, int running);
bool print_canonical_to_stream(query *q, stream *str, cell *c, pl_ctx c_ctx, int running);

void dump_term(query *q, const char *s, const cell *c);

bool bif_iso_halt_0(query *q);
bool bif_iso_close_1(query *q);
bool bif_iso_true_0(query *q);
bool bif_iso_fail_0(query *q);
bool bif_iso_call_1(query *q);
bool bif_iso_conjunction_2(query *q);
bool bif_iso_qualify_2(query *q);
bool bif_iso_cut_0(query *q);
bool bif_iso_unify_2(query *q);
bool bif_iso_if_then_2(query *q);
bool bif_soft_if_then_2(query *q);
bool bif_sys_call_check_1(query *q);
bool bif_sys_succeed_on_retry_1(query *q);
bool bif_sys_succeed_on_retry_2(query *q);
bool bif_sys_fail_on_retry_1(query *q);
bool bif_sys_drop_barrier_1(query *q);
bool bif_sys_reset_handler_1(query *q);
bool bif_sys_call_cleanup_3(query *q);
bool bif_sys_queue_1(query *q);
bool bif_sys_get_level_1(query *q);
bool bif_sys_set_if_var_2(query *q);
bool bif_sys_jump_1(query *q);
bool bif_sys_jump_if_nil_2(query *q);
bool bif_sys_cut_1(query *q);
bool bif_parse_csv_file_2(query *q);
bool bif_parse_csv_line_3(query *q);
bool bif_parse_csv_line_2(query *q);
bool bif_sre_compile_2(query *q);
bool bif_sre_matchp_4(query *q);
bool bif_sre_match_4(query *q);
bool bif_sre_substp_4(query *q);
bool bif_sre_subst_4(query *q);
bool bif_call_0(query *q, cell *p1, pl_ctx p1_ctx);
bool bif_statistics_0(query *q);
bool bif_sys_module_1(query *q);
bool bif_sys_undo_1(query *q);
bool bif_sys_create_var_1(query *q);
bool bif_sys_match_1(query *q);

void save_db(FILE *fp, query *q, int logging);
char *uuid_to_buf(const uuid *u, char *buf, size_t buflen);
bool do_abolish(query *q, cell *c_orig, cell *c_pi, bool hard);

enum log_type { LOG_ASSERTA=1, LOG_ASSERTZ=2, LOG_ERASE=3 };

int uuid_from_buf(const char *s, uuid *u);
builtins *get_fn_ptr(void *fn);

#define FEOF(str) feof(str->fp) && !str->ungetch

#ifdef _WIN32
#include <io.h>
#endif

#define DUMP_TERM(s,c,c_ctx,running) { \
	q->nl = true; q->quoted = true; \
	print_term(q, stderr, c, c_ctx, running); \
	q->nl = false; q->quoted = false; \
}

#define CHECK_INTERRUPT() \
	if (g_tpl_interrupt) { \
		if (check_interrupt(q)) \
			break; \
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
		cell *b = h + h->num_cells;

		if (is_end(b))
			return NULL;

		return b;
	}

	return NULL;
}

inline static void drop_choice(query *q)
{
	--q->cp;
}

inline static pl_idx get_ordered_slot_num(const query *q, const frame *f, unsigned var_num)
{
	return ((f - q->frames) * 100) + var_num;
}

inline static pl_idx get_actual_slot_num(const query *q, const frame *f, unsigned var_num)
{
	return get_slot(q, f, var_num) - q->slots;
}


#ifdef _WIN32
typedef intptr_t ssize_t;
extern ssize_t getline(char **lineptr, size_t *n, FILE *stream);
#endif
