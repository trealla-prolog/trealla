#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "library.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

#if !defined(_WIN32) && !defined(__wasi__) && !defined(__ANDROID__)
#include <sys/resource.h>
#endif

void convert_path(char *filename);

static const size_t INITIAL_POOL_SIZE = 64000;	// bytes

pl_idx g_empty_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
pl_idx g_anon_s, g_neck_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s, g_false_s;
pl_idx g_sys_elapsed_s, g_sys_queue_s, g_braces_s, g_call_s, g_braces_s;
pl_idx g_sys_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
pl_idx g_plus_s, g_minus_s, g_once_s, g_post_unify_hook_s, g_sys_record_key_s;
pl_idx g_conjunction_s, g_disjunction_s, g_at_s, g_sys_ne_s, g_sys_incr_s;
pl_idx g_dcg_s, g_throw_s, g_sys_block_catcher_s, g_sys_drop_barrier_s;
pl_idx g_if_then_s, g_soft_cut_s, g_negation_s;
pl_idx g_error_s, g_slash_s, g_sys_cleanup_if_det_s;
pl_idx g_goal_expansion_s, g_term_expansion_s, g_tm_s, g_float_s;
pl_idx g_sys_cut_if_det_s, g_as_s, g_colon_s, g_member_s;
pl_idx g_caret_s, g_sys_counter_s, g_catch_s, g_memberchk_s;

unsigned g_cpu_count = 4;
char *g_tpl_lib = NULL;
int g_ac = 0, g_avc = 1;
char **g_av = NULL, *g_argv0 = NULL;
unsigned g_max_depth = 6000;

static atomic_t int g_tpl_count = 0;

bool is_multifile_in_db(prolog *pl, const char *mod, const char *name, unsigned arity)
{
	module *m = find_module(pl, mod);
	if (!m) return false;

	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = new_atom(m->pl, name);
	if (tmp.val_off == ERR_IDX) return false;
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) return false;
	return pr->is_multifile ? true : false;
}

static pl_idx add_to_pool(prolog *pl, const char *name)
{
	size_t offset = pl->pool_offset, len = strlen(name);

	while ((offset+len+1+1) >= pl->pool_size) {
		size_t nbytes = (size_t)pl->pool_size * 3 / 2;
		char *tmp = realloc(pl->pool, nbytes);
		if (!tmp) return ERR_IDX;
		pl->pool = tmp;
		memset(pl->pool + pl->pool_size, 0, nbytes - pl->pool_size);
		pl->pool_size = nbytes;
	}

	if ((offset + len + 1) >= UINT32_MAX)
		return ERR_IDX;

	memcpy(pl->pool + offset, name, len+1);
	pl->pool_offset += len + 1;
	const char *key = strdup(name);
	sl_set(pl->symtab, key, (void*)(size_t)offset);
	return (pl_idx)offset;
}

pl_idx new_atom(prolog *pl, const char *name)
{
	const void *val;

	if (sl_get(pl->symtab, name, &val))
		return (pl_idx)(size_t)val;

	return add_to_pool(pl, name);
}

module *find_module(prolog *pl, const char *name)
{
	for (module *m = pl->modules; m; m = m->next) {
		if (!strcmp(m->name, name)) {
			if (m->orig)
				return m->orig;
			else
				return m;
		}
	}

	return NULL;
}

bool get_halt(prolog *pl) { return pl->halt; }
bool get_error(prolog *pl) { return pl->error; }
bool get_status(prolog *pl) { return pl->status; }
bool get_redo(prolog *pl) { return pl->is_redo; }
bool did_dump_vars(prolog *pl) { return pl->did_dump_vars; }
int get_halt_code(prolog *pl) { return pl->halt_code; }

void set_trace(prolog *pl) { pl->trace = true; }
void set_quiet(prolog *pl) { pl->quiet = true; }
void set_opt(prolog *pl, int level) { pl->opt = level; }

bool pl_isatty(prolog* pl) { return isatty(fileno(pl->streams[0].fp)); }
FILE *pl_stdin(prolog *pl) { return pl->streams[0].fp; }

bool pl_eval(prolog *pl, const char *s, bool interactive)
{
	if (!*s)
		return false;

	pl->p = parser_create(pl->curr_m);
	if (!pl->p) return false;

	if (interactive && isatty(fileno(stdin)))
		pl->p->fp = stdin;

	pl->p->interactive = interactive;
	pl->p->command = true;
	bool ok = run(pl->p, s, true, NULL, 0);
	if (get_status(pl)) pl->curr_m = pl->p->m;
	parser_destroy(pl->p);
	pl->p = NULL;
	return ok;
}

bool pl_query(prolog *pl, const char *s, pl_sub_query **subq, unsigned int yield_time_in_ms)
{
	if (!pl || !*s || !subq)
		return false;

	if (!pl->p)
		pl->p = parser_create(pl->curr_m);
	else
		reset(pl->p);

	if (!pl->p) return false;
	pl->p->command = true;
	pl->is_query = true;
	bool ok = run(pl->p, s, true, (query**)subq, yield_time_in_ms);
	if (get_status(pl)) pl->curr_m = pl->p->m;
	return ok;
}

bool pl_redo(pl_sub_query *subq)
{
	if (!subq)
		return false;

	query *q = (query*)subq;

	if (query_redo(q))
		return true;

	query_destroy(q);
	return false;
}

bool pl_yield_at(pl_sub_query *subq, unsigned int time_in_ms)
{
	if (!subq)
		return false;

	query *q = (query*)subq;
	do_yield_at(q, time_in_ms);
	return true;
}

bool pl_did_yield(pl_sub_query *subq)
{
	if (!subq)
		return false;

	query *q = (query*)subq;
	return q->yielded;
}

bool pl_done(pl_sub_query *subq)
{
	if (!subq)
		return false;

	query *q = (query*)subq;
	query_destroy(q);
	return true;
}

bool pl_consult_fp(prolog *pl, FILE *fp, const char *filename)
{
	return load_fp(pl->user_m, fp, filename, false) != NULL;
}

bool pl_consult(prolog *pl, const char *filename)
{
	return load_file(pl->user_m, filename, false);
}

bool pl_logging(prolog *pl, const char *filename)
{
	pl->logfp = fopen(filename, "a");
	return pl->logfp ? true : false;
}

bool pl_restore(prolog *pl, const char *filename)
{
	return restore_log(pl->user_m, filename);
}

static void g_destroy()
{
	free(g_tpl_lib);
}

void ptrfree(const void *key, const void *val, const void *p)
{
	builtins *ptr = (void*)val;

	if (ptr->via_directive) {
		if (ptr->help2) free((void*)ptr->help2);
		if (ptr->desc) free((void*)ptr->desc);
		if (ptr->name) free((void*)ptr->name);
		free((void*)ptr);
	}
}

void keyfree(const void *key, const void *val, const void *p)
{
	free((void*)key);
}

void keyvalfree(const void *key, const void *val, const void *p)
{
	free((void*)key);
	free((void*)val);
}

builtins *get_help(prolog *pl, const char *name, unsigned arity, bool *found, bool *evaluable)
{
	sliter *iter = sl_find_key(pl->help, name);
	builtins *ptr;

	while (sl_next_key(iter, (void**)&ptr)) {
		if (ptr->arity == arity) {
			if (found) *found = true;
			if (evaluable) *evaluable = ptr->evaluable;
			sl_done(iter);
			return ptr;
		}
	}

	if (found) *found = false;
	if (evaluable) *evaluable = false;
	sl_done(iter);
	return NULL;
}

builtins *get_builtin(prolog *pl, const char *name, size_t len, unsigned arity, bool *found, bool *evaluable)
{
	// TODO: use 'len' in comparison
	sliter *iter = sl_find_key(pl->biftab, name);
	builtins *ptr;

	while (sl_next_key(iter, (void**)&ptr)) {
		if (ptr->arity == arity) {
			if (found) *found = true;
			if (evaluable) *evaluable = ptr->evaluable;
			sl_done(iter);
			return ptr;
		}
	}

	if (found) *found = false;
	if (evaluable) *evaluable = false;
	sl_done(iter);
	return NULL;
}

builtins *get_fn_ptr(void *fn)
{
	for (builtins *ptr = g_iso_bifs; ptr->name; ptr++) {
		if (ptr->fn == fn)
			return ptr;
	}

	for (builtins *ptr = g_evaluable_bifs; ptr->name; ptr++) {
		if (ptr->fn == fn)
			return ptr;
	}

	for (builtins *ptr = g_other_bifs; ptr->name; ptr++) {
		if (ptr->fn == fn)
			return ptr;
	}

	for (builtins *ptr = g_files_bifs; ptr->name; ptr++) {
		if (ptr->fn == fn)
			return ptr;
	}

	for (builtins *ptr = g_ffi_bifs; ptr->name; ptr++) {
		if (ptr->fn == fn)
			return ptr;
	}

	for (builtins *ptr = g_posix_bifs; ptr->name; ptr++) {
		if (ptr->fn == fn)
			return ptr;
	}

	for (builtins *ptr = g_contrib_bifs; ptr->name; ptr++) {
		if (ptr->fn == fn)
			return ptr;
	}

	return NULL;
}

void load_builtins(prolog *pl)
{
	for (const builtins *ptr = g_iso_bifs; ptr->name; ptr++) {
		sl_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		sl_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_evaluable_bifs; ptr->name; ptr++) {
		sl_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		sl_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_other_bifs; ptr->name; ptr++) {
		sl_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		sl_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_files_bifs; ptr->name; ptr++) {
		sl_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		sl_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_ffi_bifs; ptr->name; ptr++) {
		sl_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		sl_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_posix_bifs; ptr->name; ptr++) {
		sl_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		sl_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_contrib_bifs; ptr->name; ptr++) {
		sl_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		sl_app(pl->help, ptr->name, ptr);
	}
}

static bool g_init(prolog *pl)
{
	char *ptr = getenv("TPL_LIBRARY_PATH");
	bool error = false;

	if (ptr) {
		g_tpl_lib = strdup(ptr);
		convert_path(g_tpl_lib);
	}

#if !defined(_WIN32) && !defined(__wasi__) && !defined(__ANDROID__)
	struct rlimit rlp;
	getrlimit(RLIMIT_STACK, &rlp);
	g_max_depth = rlp.rlim_cur / 1024;
#endif

	pl->pool = calloc(1, pl->pool_size=INITIAL_POOL_SIZE);
	CHECK_SENTINEL(pl->symtab = sl_create((void*)fake_strcmp, (void*)keyfree, NULL), NULL);

	CHECK_SENTINEL(new_atom(pl, "dummy"), ERR_IDX);
	CHECK_SENTINEL(g_false_s = new_atom(pl, "false"), ERR_IDX);
	CHECK_SENTINEL(g_true_s = new_atom(pl, "true"), ERR_IDX);
	CHECK_SENTINEL(g_at_s = new_atom(pl, "@"), ERR_IDX);
	CHECK_SENTINEL(g_conjunction_s = new_atom(pl, ","), ERR_IDX);
	CHECK_SENTINEL(g_disjunction_s = new_atom(pl, ";"), ERR_IDX);
	CHECK_SENTINEL(g_if_then_s = new_atom(pl, "->"), ERR_IDX);
	CHECK_SENTINEL(g_soft_cut_s = new_atom(pl, "*->"), ERR_IDX);
	CHECK_SENTINEL(g_negation_s = new_atom(pl, "\\+"), ERR_IDX);
	CHECK_SENTINEL(g_dot_s = new_atom(pl, "."), ERR_IDX);
	CHECK_SENTINEL(g_plus_s = new_atom(pl, "+"), ERR_IDX);
	CHECK_SENTINEL(g_minus_s = new_atom(pl, "-"), ERR_IDX);
	CHECK_SENTINEL(g_empty_s = new_atom(pl, ""), ERR_IDX);
	CHECK_SENTINEL(g_anon_s = new_atom(pl, "_"), ERR_IDX);
	CHECK_SENTINEL(g_dcg_s = new_atom(pl, "-->"), ERR_IDX);
	CHECK_SENTINEL(g_call_s = new_atom(pl, "call"), ERR_IDX);
	CHECK_SENTINEL(g_catch_s = new_atom(pl, "catch"), ERR_IDX);
	CHECK_SENTINEL(g_member_s = new_atom(pl, "member"), ERR_IDX);
	CHECK_SENTINEL(g_memberchk_s = new_atom(pl, "memberchk"), ERR_IDX);
	CHECK_SENTINEL(g_sys_counter_s = new_atom(pl, "$counter"), ERR_IDX);
	CHECK_SENTINEL(g_braces_s = new_atom(pl, "braces"), ERR_IDX);
	CHECK_SENTINEL(g_unify_s = new_atom(pl, "="), ERR_IDX);
	CHECK_SENTINEL(g_on_s = new_atom(pl, "on"), ERR_IDX);
	CHECK_SENTINEL(g_off_s = new_atom(pl, "off"), ERR_IDX);
	CHECK_SENTINEL(g_cut_s = new_atom(pl, "!"), ERR_IDX);
	CHECK_SENTINEL(g_nil_s = new_atom(pl, "[]"), ERR_IDX);
	CHECK_SENTINEL(g_braces_s = new_atom(pl, "{}"), ERR_IDX);
	CHECK_SENTINEL(g_fail_s = new_atom(pl, "fail"), ERR_IDX);
	CHECK_SENTINEL(g_neck_s = new_atom(pl, ":-"), ERR_IDX);
	CHECK_SENTINEL(g_eof_s = new_atom(pl, "end_of_file"), ERR_IDX);
	CHECK_SENTINEL(g_lt_s = new_atom(pl, "<"), ERR_IDX);
	CHECK_SENTINEL(g_gt_s = new_atom(pl, ">"), ERR_IDX);
	CHECK_SENTINEL(g_eq_s = new_atom(pl, "="), ERR_IDX);
	CHECK_SENTINEL(g_once_s = new_atom(pl, "once"), ERR_IDX);
	CHECK_SENTINEL(g_throw_s = new_atom(pl, "throw"), ERR_IDX);
	CHECK_SENTINEL(g_error_s = new_atom(pl, "error"), ERR_IDX);
	CHECK_SENTINEL(g_slash_s = new_atom(pl, "/"), ERR_IDX);
	CHECK_SENTINEL(g_goal_expansion_s = new_atom(pl, "goal_expansion"), ERR_IDX);
	CHECK_SENTINEL(g_term_expansion_s = new_atom(pl, "term_expansion"), ERR_IDX);
	CHECK_SENTINEL(g_tm_s = new_atom(pl, "tm"), ERR_IDX);
	CHECK_SENTINEL(g_float_s = new_atom(pl, "float"), ERR_IDX);

	CHECK_SENTINEL(g_sys_elapsed_s = new_atom(pl, "$elapsed"), ERR_IDX);
	CHECK_SENTINEL(g_sys_queue_s = new_atom(pl, "$queue"), ERR_IDX);
	CHECK_SENTINEL(g_sys_var_s = new_atom(pl, "$VAR"), ERR_IDX);
	CHECK_SENTINEL(g_sys_stream_property_s = new_atom(pl, "$stream_property"), ERR_IDX);
	CHECK_SENTINEL(g_post_unify_hook_s = new_atom(pl, "$post_unify_hook"), ERR_IDX);
	CHECK_SENTINEL(g_sys_record_key_s = new_atom(pl, "$record_key"), ERR_IDX);
	CHECK_SENTINEL(g_sys_ne_s = new_atom(pl, "$ne"), ERR_IDX);
	CHECK_SENTINEL(g_sys_incr_s = new_atom(pl, "$incr"), ERR_IDX);
	CHECK_SENTINEL(g_sys_block_catcher_s = new_atom(pl, "$block_catcher"), ERR_IDX);
	CHECK_SENTINEL(g_sys_drop_barrier_s = new_atom(pl, "$drop_barrier"), ERR_IDX);
	CHECK_SENTINEL(g_sys_cleanup_if_det_s = new_atom(pl, "$cleanup_if_det"), ERR_IDX);
	CHECK_SENTINEL(g_sys_cut_if_det_s = new_atom(pl, "$cut_if_det"), ERR_IDX);
	CHECK_SENTINEL(g_as_s = new_atom(pl, "as"), ERR_IDX);
	CHECK_SENTINEL(g_colon_s = new_atom(pl, ":"), ERR_IDX);
	CHECK_SENTINEL(g_caret_s = new_atom(pl, "^"), ERR_IDX);

	return error;
}

void pl_destroy(prolog *pl)
{
	if (!pl) return;

	if (pl->logfp)
		fclose(pl->logfp);

	module_destroy(pl->system_m);
	module_destroy(pl->user_m);
	sl_destroy(pl->biftab);
	sl_destroy(pl->symtab);

	while (pl->modules)
		module_destroy(pl->modules);

	sl_destroy(pl->fortab);
	sl_destroy(pl->keyval);
	sl_destroy(pl->help);
	free(pl->tabs);
	pl->pool_offset = 0;

	if (!--g_tpl_count)
		g_destroy();

	for (int i = 0; i < MAX_STREAMS; i++) {
		stream *str = &pl->streams[i];

		if (str->fp) {
			if ((str->fp != stdin)
				&& (str->fp != stdout)
				&& (str->fp != stderr)
			) {
				if (str->is_map)
					sl_destroy(str->keyval);
				else if (str->is_engine)
					query_destroy(str->engine);
				else if (str->fp && (i > 2))
					fclose(str->fp);
			}

			parser_destroy(str->p);
			str->p = NULL;
			sl_destroy(str->alias);
			free(str->mode);
			free(str->filename);
			free(str->data);
		}
	}

	memset(pl->streams, 0, sizeof(pl->streams));
	parser_destroy(pl->p);
	free(pl->pool);
	free(pl);
}

prolog *pl_create()
{
	//printf("*** sizeof(cell) = %u bytes\n", (unsigned)sizeof(cell));
	//assert(sizeof(cell) == 24);

	prolog *pl = calloc(1, sizeof(prolog));
	if (!pl) return NULL;
	bool error = false;

	if (!g_tpl_count++)
		g_init(pl);

	if (!g_tpl_lib) {
		g_tpl_lib = realpath(g_argv0, NULL);

		if (g_tpl_lib) {
			char *src = g_tpl_lib + strlen(g_tpl_lib) - 1;

			while ((src != g_tpl_lib) && (*src != PATH_SEP_CHAR))
				src--;

			*src = '\0';
			g_tpl_lib = realloc(g_tpl_lib, strlen(g_tpl_lib)+40);
			strcat(g_tpl_lib, "/library");
		} else
			g_tpl_lib = strdup("../library");
	}

	CHECK_SENTINEL(pl->keyval = sl_create((void*)fake_strcmp, (void*)keyvalfree, NULL), NULL);

	if (error) {
		free(pl->pool);
		return NULL;
	}

	pl->streams[0].fp = stdin;
	CHECK_SENTINEL(pl->streams[0].alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL), NULL);
	CHECK_SENTINEL(pl->streams[0].filename = strdup("stdin"), NULL);
	CHECK_SENTINEL(pl->streams[0].mode = strdup("read"), NULL);
	sl_set(pl->streams[0].alias, strdup("user_input"), NULL);
	pl->streams[0].eof_action = eof_action_reset;

	pl->streams[1].fp = stdout;
	CHECK_SENTINEL(pl->streams[1].alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL), NULL);
	CHECK_SENTINEL(pl->streams[1].filename = strdup("stdout"), NULL);
	CHECK_SENTINEL(pl->streams[1].mode = strdup("append"), NULL);
	sl_set(pl->streams[1].alias, strdup("user_output"), NULL);
	pl->streams[1].eof_action = eof_action_reset;

	pl->streams[2].fp = stderr;
	CHECK_SENTINEL(pl->streams[2].alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL), NULL);
	CHECK_SENTINEL(pl->streams[2].filename = strdup("stderr"), NULL);
	CHECK_SENTINEL(pl->streams[2].mode = strdup("append"), NULL);
	sl_set(pl->streams[2].alias, strdup("user_error"), NULL);
	pl->streams[2].eof_action = eof_action_reset;

	pl->streams[3].ignore = true;

	pl->help = sl_create((void*)fake_strcmp, (void*)ptrfree, NULL);
	pl->fortab = sl_create((void*)fake_strcmp, NULL, NULL);
	pl->biftab = sl_create((void*)fake_strcmp, NULL, NULL);

	if (pl->biftab)
		load_builtins(pl);

	//printf("Library: %s\n", g_tpl_lib);

	pl->system_m = module_create(pl, "system");

	if (!pl->system_m || pl->system_m->error) {
		pl_destroy(pl);
		pl = NULL;
		return pl;
	}

	pl->user_m = module_create(pl, "user");

	if (!pl->user_m || pl->user_m->error) {
		pl_destroy(pl);
		pl = NULL;
		return pl;
	}

	pl->user_m->flags.strict_iso = false;
	pl->curr_m = pl->user_m;

	pl->current_input = 0;		// STDIN
	pl->current_output = 1;		// STDOUT
	pl->current_error = 2;		// STDERR
	pl->def_max_depth = 19;
	pl->def_quoted = true;
	pl->def_double_quotes = true;

	// In user space...

	set_discontiguous_in_db(pl->user_m, "$predicate_property", 3);

	set_multifile_in_db(pl->user_m, "$predicate_property", 3);

	set_dynamic_in_db(pl->user_m, "$record_key", 2);
	set_dynamic_in_db(pl->user_m, "$op", 3);
	set_dynamic_in_db(pl->user_m, "$predicate_property", 3);
	set_dynamic_in_db(pl->user_m, "$current_prolog_flag", 2);
	set_dynamic_in_db(pl->user_m, "$stream_property", 2);
	set_dynamic_in_db(pl->user_m, "$directive", 1);

	pl->user_m->prebuilt = true;
	const char *save_filename = pl->user_m->filename;

	// Load some common libraries...

	for (library *lib = g_libs; lib->name; lib++) {
		if (!strcmp(lib->name, "builtins")			// Always need this
			|| !strcmp(lib->name, "apply")			// Common
			|| !strcmp(lib->name, "lists")			// Common
			|| !strcmp(lib->name, "dcgs")			// Common
			|| !strcmp(lib->name, "freeze")			// Common
			|| !strcmp(lib->name, "dif")			// Common
			) {
			size_t len = *lib->len;
			char *src = malloc(len+1);
			check_error(src, pl_destroy(pl));
			memcpy(src, lib->start, len);
			src[len] = '\0';
			SB(s1);
			SB_sprintf(s1, "library/%s", lib->name);
			module *m = load_text(pl->user_m, src, SB_cstr(s1));
			SB_free(s1);
			free(src);
			check_error(m, pl_destroy(pl));
		}
	}

	pl->user_m->filename = save_filename;
	pl->user_m->prebuilt = false;
	return pl;
}
