#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "library.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

void convert_path(char *filename);

static const size_t INITIAL_POOL_SIZE = 64000;	// bytes

pl_idx_t g_empty_s, g_pair_s, g_dot_s, g_cut_s, g_nil_s, g_true_s, g_fail_s;
pl_idx_t g_anon_s, g_neck_s, g_eof_s, g_lt_s, g_gt_s, g_eq_s, g_false_s;
pl_idx_t g_sys_elapsed_s, g_sys_queue_s, g_braces_s, g_call_s, g_braces_s;
pl_idx_t g_sys_stream_property_s, g_unify_s, g_on_s, g_off_s, g_sys_var_s;
pl_idx_t g_plus_s, g_minus_s, g_once_s, g_post_unify_hook_s, g_sys_record_key_s;
pl_idx_t g_conjunction_s, g_disjunction_s, g_at_s, g_sys_ne_s, g_sys_incr_s;
pl_idx_t g_dcg_s, g_throw_s, g_sys_block_catcher_s, g_sys_drop_barrier;
pl_idx_t g_sys_soft_cut_s, g_if_then_s, g_soft_cut_s, g_negation_s;
pl_idx_t g_error_s, g_slash_s, g_sys_cleanup_if_det_s, g_sys_table_s;
pl_idx_t g_goal_expansion_s, g_term_expansion_s;

unsigned g_cpu_count = 4;
char *g_tpl_lib = NULL;
int g_ac = 0, g_avc = 1;
char **g_av = NULL, *g_argv0 = NULL;

static atomic_t int g_tpl_count = 0;

bool is_multifile_in_db(prolog *pl, const char *mod, const char *name, unsigned arity)
{
	module *m = find_module(pl, mod);
	if (!m) return false;

	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = index_from_pool(m->pl, name);
	if (tmp.val_off == ERR_IDX) return false;
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) return false;
	return pr->is_multifile ? true : false;
}

static pl_idx_t add_to_pool(prolog *pl, const char *name)
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
	map_set(pl->symtab, key, (void*)(size_t)offset);
	g_interned_cnt++;
	return (pl_idx_t)offset;
}

pl_idx_t index_from_pool(prolog *pl, const char *name)
{
	const void *val;

	if (map_get(pl->symtab, name, &val))
		return (pl_idx_t)(size_t)val;

	return add_to_pool(pl, name);
}

module *find_next_module(prolog *pl, module *m)
{
	if (!m)
		return pl->modules;

	return m->next;
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
bool get_status(prolog *pl) { return pl->status; }
bool get_redo(prolog *pl) { return pl->is_redo; }
bool did_dump_vars(prolog *pl) { return pl->did_dump_vars; }
int get_halt_code(prolog *pl) { return pl->halt_code; }

void set_trace(prolog *pl) { pl->trace = true; }
void set_quiet(prolog *pl) { pl->quiet = true; }
void set_opt(prolog *pl, int level) { pl->opt = level; }

bool pl_eval(prolog *pl, const char *s)
{
	if (!*s)
		return false;

	pl->p = create_parser(pl->curr_m);
	if (!pl->p) return false;
	pl->p->command = true;
	bool ok = run(pl->p, s, true);
	pl->curr_m = pl->p->m;
	destroy_parser(pl->p);
	pl->p = NULL;
	return ok;
}

bool pl_query(prolog *pl, const char *s, pl_sub_query **subq)
{
	if (!pl || !*s || !subq)
		return false;

	pl->p = create_parser(pl->curr_m);
	if (!pl->p) return false;
	pl->p->command = true;
	pl->is_query = true;
	bool ok = run(pl->p, s, true);
	*subq = (pl_sub_query*)pl->curr_query;
	pl->curr_m = pl->p->m;
	return ok;
}

bool pl_redo(pl_sub_query *subq)
{
	if (!subq)
		return false;

	query *q = (query*)subq;

	if (query_redo(q))
		return true;

	destroy_query(q);
	return false;
}

bool pl_done(pl_sub_query *subq)
{
	if (!subq)
		return false;

	query *q = (query*)subq;
	destroy_query(q);
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

static void g_destroy()
{
	free(g_tpl_lib);
}

static void keyfree(const void *key, const void *val, const void *p)
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
	miter *iter = map_find_key(pl->help, name);
	builtins *ptr;

	while (map_next_key(iter, (void**)&ptr)) {
		if (ptr->arity == arity) {
			if (found) *found = true;
			if (evaluable) *evaluable = ptr->evaluable;
			map_done(iter);
			return ptr;
		}
	}

	if (found) *found = false;
	if (evaluable) *evaluable = false;
	map_done(iter);
	return NULL;
}

builtins *get_builtin(prolog *pl, const char *name, unsigned arity, bool *found, bool *evaluable)
{
	miter *iter = map_find_key(pl->biftab, name);
	builtins *ptr;

	while (map_next_key(iter, (void**)&ptr)) {
		if (ptr->arity == arity) {
			if (found) *found = true;
			if (evaluable) *evaluable = ptr->evaluable;
			map_done(iter);
			return ptr;
		}
	}

	if (found) *found = false;
	if (evaluable) *evaluable = false;
	map_done(iter);
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

	for (builtins *ptr = g_contrib_bifs; ptr->name; ptr++) {
		if (ptr->fn == fn)
			return ptr;
	}

	return NULL;
}

static int max_ffi_idx = 0;

void register_ffi(prolog *pl, const char *name, unsigned arity, void *fn, uint8_t *types, uint8_t ret_type, bool evaluable)
{
	builtins *ptr = &g_ffi_bifs[max_ffi_idx++];
	ptr->name = name;
	ptr->arity = arity;
	ptr->fn = fn;
	ptr->help = NULL;
	ptr->evaluable = evaluable;
	ptr->ffi = true;

	for (unsigned i = 0; i < arity; i++)
		ptr->types[i] = types[i];

	ptr->ret_type = ret_type;
	map_app(pl->biftab, ptr->name, ptr);
}

void load_builtins(prolog *pl)
{
	for (const builtins *ptr = g_iso_bifs; ptr->name; ptr++) {
		map_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		map_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_evaluable_bifs; ptr->name; ptr++) {
		map_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		map_app(pl->help, ptr->name, ptr);
		max_ffi_idx++;
	}

	for (const builtins *ptr = g_other_bifs; ptr->name; ptr++) {
		map_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		map_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_files_bifs; ptr->name; ptr++) {
		map_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		map_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_ffi_bifs; ptr->name; ptr++) {
		map_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		map_app(pl->help, ptr->name, ptr);
	}

	for (const builtins *ptr = g_contrib_bifs; ptr->name; ptr++) {
		map_app(pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		map_app(pl->help, ptr->name, ptr);
	}
}

void g_init()
{
	char *ptr = getenv("TPL_LIBRARY_PATH");

	if (ptr) {
		g_tpl_lib = strdup(ptr);
		convert_path(g_tpl_lib);
	}
}

void pl_destroy(prolog *pl)
{
	if (!pl) return;

	destroy_module(pl->system_m);
	destroy_module(pl->user_m);

	while (pl->modules)
		destroy_module(pl->modules);

	map_destroy(pl->biftab);
	map_destroy(pl->symtab);
	map_destroy(pl->keyval);
	map_destroy(pl->help);
	free(pl->pool);
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
					map_destroy(str->keyval);
				else
					fclose(str->fp);
			}

			if (str->p)
				destroy_parser(str->p);

			free(str->mode);
			free(str->filename);
			free(str->name);
			free(str->data);
		}
	}

	memset(pl->streams, 0, sizeof(pl->streams));

	free(pl);
}

prolog *pl_create()
{
	//printf("*** sizeof(cell) = %u bytes\n", (unsigned)sizeof(cell));
	assert(sizeof(cell) == 24);

	prolog *pl = calloc(1, sizeof(prolog));

	if (!g_tpl_count++)
#ifdef __wasi__
		;
	// Wizer workaround: we need to avoid touching the environment until main
	// is run, otherwise libc will cache the wrong environment.
#else
		g_init();
#endif

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

	pl->pool = calloc(1, pl->pool_size=INITIAL_POOL_SIZE);
	if (!pl->pool) return NULL;
	bool error = false;

	CHECK_SENTINEL(pl->symtab = map_create((void*)fake_strcmp, (void*)keyfree, NULL), NULL);
	CHECK_SENTINEL(pl->keyval = map_create((void*)fake_strcmp, (void*)keyvalfree, NULL), NULL);
	map_allow_dups(pl->symtab, false);
	map_allow_dups(pl->keyval, false);

	if (error) {
		free(pl->pool);
		return NULL;
	}

	CHECK_SENTINEL(index_from_pool(pl, "dummy"), ERR_IDX);
	CHECK_SENTINEL(g_false_s = index_from_pool(pl, "false"), ERR_IDX);
	CHECK_SENTINEL(g_true_s = index_from_pool(pl, "true"), ERR_IDX);
	CHECK_SENTINEL(g_at_s = index_from_pool(pl, "@"), ERR_IDX);
	CHECK_SENTINEL(g_conjunction_s = index_from_pool(pl, ","), ERR_IDX);
	CHECK_SENTINEL(g_disjunction_s = index_from_pool(pl, ";"), ERR_IDX);
	CHECK_SENTINEL(g_if_then_s = index_from_pool(pl, "->"), ERR_IDX);
	CHECK_SENTINEL(g_soft_cut_s = index_from_pool(pl, "*->"), ERR_IDX);
	CHECK_SENTINEL(g_negation_s = index_from_pool(pl, "\\+"), ERR_IDX);
	CHECK_SENTINEL(g_dot_s = index_from_pool(pl, "."), ERR_IDX);
	CHECK_SENTINEL(g_plus_s = index_from_pool(pl, "+"), ERR_IDX);
	CHECK_SENTINEL(g_minus_s = index_from_pool(pl, "-"), ERR_IDX);
	CHECK_SENTINEL(g_pair_s = index_from_pool(pl, ":"), ERR_IDX);
	CHECK_SENTINEL(g_empty_s = index_from_pool(pl, ""), ERR_IDX);
	CHECK_SENTINEL(g_anon_s = index_from_pool(pl, "_"), ERR_IDX);
	CHECK_SENTINEL(g_dcg_s = index_from_pool(pl, "-->"), ERR_IDX);
	CHECK_SENTINEL(g_call_s = index_from_pool(pl, "call"), ERR_IDX);
	CHECK_SENTINEL(g_braces_s = index_from_pool(pl, "braces"), ERR_IDX);
	CHECK_SENTINEL(g_unify_s = index_from_pool(pl, "="), ERR_IDX);
	CHECK_SENTINEL(g_on_s = index_from_pool(pl, "on"), ERR_IDX);
	CHECK_SENTINEL(g_off_s = index_from_pool(pl, "off"), ERR_IDX);
	CHECK_SENTINEL(g_cut_s = index_from_pool(pl, "!"), ERR_IDX);
	CHECK_SENTINEL(g_nil_s = index_from_pool(pl, "[]"), ERR_IDX);
	CHECK_SENTINEL(g_braces_s = index_from_pool(pl, "{}"), ERR_IDX);
	CHECK_SENTINEL(g_fail_s = index_from_pool(pl, "fail"), ERR_IDX);
	CHECK_SENTINEL(g_neck_s = index_from_pool(pl, ":-"), ERR_IDX);
	CHECK_SENTINEL(g_eof_s = index_from_pool(pl, "end_of_file"), ERR_IDX);
	CHECK_SENTINEL(g_lt_s = index_from_pool(pl, "<"), ERR_IDX);
	CHECK_SENTINEL(g_gt_s = index_from_pool(pl, ">"), ERR_IDX);
	CHECK_SENTINEL(g_eq_s = index_from_pool(pl, "="), ERR_IDX);
	CHECK_SENTINEL(g_once_s = index_from_pool(pl, "once"), ERR_IDX);
	CHECK_SENTINEL(g_throw_s = index_from_pool(pl, "throw"), ERR_IDX);
	CHECK_SENTINEL(g_error_s = index_from_pool(pl, "error"), ERR_IDX);
	CHECK_SENTINEL(g_slash_s = index_from_pool(pl, "/"), ERR_IDX);
	CHECK_SENTINEL(g_goal_expansion_s = index_from_pool(pl, "goal_expansion"), ERR_IDX);
	CHECK_SENTINEL(g_term_expansion_s = index_from_pool(pl, "term_expansion"), ERR_IDX);

	CHECK_SENTINEL(g_sys_elapsed_s = index_from_pool(pl, "$elapsed"), ERR_IDX);
	CHECK_SENTINEL(g_sys_queue_s = index_from_pool(pl, "$queue"), ERR_IDX);
	CHECK_SENTINEL(g_sys_var_s = index_from_pool(pl, "$VAR"), ERR_IDX);
	CHECK_SENTINEL(g_sys_stream_property_s = index_from_pool(pl, "$stream_property"), ERR_IDX);
	CHECK_SENTINEL(g_post_unify_hook_s = index_from_pool(pl, "$post_unify_hook"), ERR_IDX);
	CHECK_SENTINEL(g_sys_record_key_s = index_from_pool(pl, "$record_key"), ERR_IDX);
	CHECK_SENTINEL(g_sys_ne_s = index_from_pool(pl, "$ne"), ERR_IDX);
	CHECK_SENTINEL(g_sys_incr_s = index_from_pool(pl, "$incr"), ERR_IDX);
	CHECK_SENTINEL(g_sys_block_catcher_s = index_from_pool(pl, "$block_catcher"), ERR_IDX);
	CHECK_SENTINEL(g_sys_soft_cut_s = index_from_pool(pl, "$soft_cut"), ERR_IDX);
	CHECK_SENTINEL(g_sys_drop_barrier = index_from_pool(pl, "$drop_barrier"), ERR_IDX);
	CHECK_SENTINEL(g_sys_cleanup_if_det_s = index_from_pool(pl, "$cleanup_if_det"), ERR_IDX);
	CHECK_SENTINEL(g_sys_table_s = index_from_pool(pl, "$table"), ERR_IDX);

	if (error)
		return NULL;

	pl->streams[0].fp = stdin;
	CHECK_SENTINEL(pl->streams[0].filename = strdup("stdin"), NULL);
	CHECK_SENTINEL(pl->streams[0].name = strdup("user_input"), NULL);
	CHECK_SENTINEL(pl->streams[0].mode = strdup("read"), NULL);
	pl->streams[0].eof_action = eof_action_reset;

	pl->streams[1].fp = stdout;
	CHECK_SENTINEL(pl->streams[1].filename = strdup("stdout"), NULL);
	CHECK_SENTINEL(pl->streams[1].name = strdup("user_output"), NULL);
	CHECK_SENTINEL(pl->streams[1].mode = strdup("append"), NULL);
	pl->streams[1].eof_action = eof_action_reset;

	pl->streams[2].fp = stderr;
	CHECK_SENTINEL(pl->streams[2].filename = strdup("stderr"), NULL);
	CHECK_SENTINEL(pl->streams[2].name = strdup("user_error"), NULL);
	CHECK_SENTINEL(pl->streams[2].mode = strdup("append"), NULL);
	pl->streams[2].eof_action = eof_action_reset;

	pl->streams[3].ignore = true;;

	pl->help = map_create((void*)fake_strcmp, NULL, NULL);
	map_allow_dups(pl->help, false);

	pl->biftab = map_create((void*)fake_strcmp, NULL, NULL);
	map_allow_dups(pl->biftab, false);

	if (pl->biftab)
		load_builtins(pl);

	//printf("Library: %s\n", g_tpl_lib);

	pl->system_m = create_module(pl, "system");

	if (!pl->system_m || pl->system_m->error) {
		pl_destroy(pl);
		pl = NULL;
		return pl;
	}

	pl->user_m = create_module(pl, "user");

	if (!pl->user_m || pl->user_m->error) {
		pl_destroy(pl);
		pl = NULL;
		return pl;
	}

	pl->curr_m = pl->user_m;
	pl->tabs_size = 0;
	pl->tabs = NULL;
	pl->s_last = 0;
	pl->s_cnt = 0;
	pl->seed = 0;

	pl->current_input = 0;		// STDIN
	pl->current_output = 1;		// STDOUT
	pl->current_error = 2;		// STDERR

	// In user space...

	set_multifile_in_db(pl->user_m, "$predicate_property", 2);
	set_multifile_in_db(pl->user_m, ":-", 1);

	set_dynamic_in_db(pl->user_m, "$record_key", 2);
	set_dynamic_in_db(pl->user_m, "$current_op", 3);
	set_dynamic_in_db(pl->user_m, "$predicate_property", 2);
	set_dynamic_in_db(pl->user_m, "$current_prolog_flag", 2);
	set_dynamic_in_db(pl->user_m, "$stream_property", 2);
	set_dynamic_in_db(pl->user_m, "initialization", 1);
	set_dynamic_in_db(pl->user_m, ":-", 1);

	pl->user_m->prebuilt = true;

	// Load some common libraries...

	for (library *lib = g_libs; lib->name; lib++) {
		if (!strcmp(lib->name, "builtins")			// Always need this
			|| !strcmp(lib->name, "lists")			// Common
			|| !strcmp(lib->name, "apply")			// Common
			|| !strcmp(lib->name, "freeze")			// Common
			|| !strcmp(lib->name, "dif")			// Common?
			|| !strcmp(lib->name, "when")			// Common?
#ifdef __wasi__
			|| !strcmp(lib->name, "js_toplevel")	// Needed for WASM toplevel
			|| !strcmp(lib->name, "pseudojson")		// Likewise
#endif
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

	pl->user_m->prebuilt = false;
	return pl;
}
