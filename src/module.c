#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#include "module.h"
#include "parser.h"
#include "history.h"
#include "library.h"
#include "prolog.h"
#include "query.h"

struct loaded_file_ {
	loaded_file *next;
	char *filename;
	char *orig_filename;
	time_t when_loaded;
	bool is_loaded:1;
};

static const op_table g_ops[] =
{
	{":-", OP_XFX, 1200},
	{":-", OP_FX, 1200},
	{"-->", OP_XFX, 1200},
	{"?-", OP_FX, 1200},
	{"|", OP_XFY, 1105},
	{";", OP_XFY, 1100},
	{"->", OP_XFY, 1050},
	{"*->", OP_XFY, 1050},
	{",", OP_XFY, 1000},

	{"public", OP_FX, 1150},
	{"discontiguous", OP_FX, 1150},
	{"multifile", OP_FX, 1150},
	{"attribute", OP_FX, 1150},
	//{"op", OP_FX, 1150},
	{"table", OP_FX, 1150},
	{"dynamic", OP_FX, 1150},
	{"initialization", OP_FX, 1150},
	//{"set_prolog_flag", OP_FX, 1150},
	//{"module", OP_FX, 1150},
	{"use_module", OP_FX, 1150},
	{"ensure_loaded", OP_FX, 1150},
	{"meta_predicate", OP_FX, 1150},

	{"\\+", OP_FY, 900},
	{"as", OP_XFX, 700},
	{"is", OP_XFX, 700},
	{"=", OP_XFX, 700},
	{"\\=", OP_XFX, 700},
	{"==", OP_XFX, 700},
	{"\\==", OP_XFX, 700},
	{"=:=", OP_XFX, 700},
	{"=\\=", OP_XFX, 700},
	{"<", OP_XFX, 700},
	{"=<", OP_XFX, 700},
	{">", OP_XFX, 700},
	{">=", OP_XFX, 700},
	{"@<", OP_XFX, 700},
	{"@=<", OP_XFX, 700},
	{"@>", OP_XFX, 700},
	{"@>=", OP_XFX, 700},
	{"=..", OP_XFX, 700},
	{":", OP_XFY, 600},
	{"+", OP_YFX, 500},
	{"-", OP_YFX, 500},
	{"?", OP_FX, 500},
	{"/\\", OP_YFX, 500},
	{"\\/", OP_YFX, 500},
	{"*", OP_YFX, 400},
	{"/", OP_YFX, 400},
	{"//", OP_YFX, 400},
	{"div", OP_YFX, 400},
	{"rdiv", OP_YFX, 400},
	{"rem", OP_YFX, 400},
	{"mod", OP_YFX, 400},
	{"xor", OP_YFX, 400},
	{"<<", OP_YFX, 400},
	{">>", OP_YFX, 400},
	{"**", OP_XFX, 200},
	{"^", OP_XFY, 200},
	{"\\", OP_FY, 200},
	{"-", OP_FY, 200},
	{"+", OP_FY, 200},

	{"++", OP_FY, 100},			// used in mode declarations
	{"--", OP_FY, 100},			// used in mode declarations
	{"@", OP_FY, 100},			// used in mode declarations
	{":", OP_FY, 100},			// used in mode declarations

	//{"$", OP_FX, 1},

	{0,0,0}
};

builtins *get_builtin_term(module *m, cell *c, bool *found, bool *evaluable)
{
	prolog *pl = m->pl;
	const char *name = C_STR(m, c);
	size_t len = C_STRLEN(m, c);
	unsigned arity = c->arity;
	return get_builtin(pl, name, len, arity, found, evaluable);
}

builtins *get_module_help(module *m, const char *name, unsigned arity, bool *found, bool *evaluable)
{
	miter *iter = map_find_key(m->pl->help, name);
	builtins *ptr;

	while (map_next_key(iter, (void**)&ptr)) {
		if (ptr->m != m)
			continue;

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

static const char *set_known(module *m, const char *filename)
{
	loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (!strcmp(ptr->filename, filename))
			return ptr->filename;

		ptr = ptr->next;
	}

	ptr = malloc(sizeof(*ptr));
	ptr->next = m->loaded_files;
	ptr->orig_filename = strdup(filename);
	ptr->filename = strdup(filename);
	ptr->is_loaded = false;
	m->loaded_files = ptr;
	return ptr->filename;
}

static const char *set_loaded(module *m, const char *filename, const char *orig_filename)
{
	loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (!strcmp(ptr->filename, filename)) {
			ptr->is_loaded = true;
			return ptr->filename;
		}

		ptr = ptr->next;
	}

	ptr = malloc(sizeof(*ptr));
	ptr->next = m->loaded_files;
	ptr->orig_filename = strdup(orig_filename);
	ptr->filename = strdup(filename);
	ptr->when_loaded = time(0);
	ptr->is_loaded = true;
	m->loaded_files = ptr;
	return ptr->filename;
}

static void set_unloaded(module *m, const char *filename)
{
	loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (!strcmp(ptr->filename, filename)) {
			ptr->is_loaded = false;
			return;
		}

		ptr = ptr->next;
	}
}

static bool is_loaded(const module *m, const char *filename)
{
	loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (ptr->is_loaded && !strcmp(ptr->filename, filename))
			return true;

		ptr = ptr->next;
	}

	return false;
}

const char *get_loaded(const module *m, const char *filename)
{
	loaded_file *ptr = m->loaded_files;

	while (ptr) {
		if (ptr->is_loaded && !strcmp(ptr->filename, filename))
			return ptr->orig_filename;

		ptr = ptr->next;
	}

	return filename;
}

static void clear_loaded(const module *m)
{
	loaded_file *ptr = m->loaded_files;

	while (ptr) {
		loaded_file *save = ptr;
		ptr = ptr->next;
		free(save->orig_filename);
		free(save->filename);
		free(save);
	}
}

predicate *create_predicate(module *m, cell *c)
{
	bool found, evaluable;

	if (strcmp(m->name, "format") && 0) {
		if (get_builtin_term(m, c, &found, &evaluable), found && !evaluable) {
			fprintf(stdout, "Error: permission error modifying %s/%u\n", C_STR(m, c), c->arity);
			return NULL;
		}
	}

	predicate *pr = calloc(1, sizeof(predicate));
	check_error(pr);
	pr->prev = m->tail;

	if (m->tail)
		m->tail->next = pr;

	m->tail = pr;

	if (!m->head)
		m->head = pr;

	pr->filename = m->filename;
	pr->m = m;
	pr->key = *c;
	pr->key.tag = TAG_INTERNED;
	pr->key.nbr_cells = 1;
	pr->is_noindex = m->pl->noindex || !pr->key.arity;
	map_app(m->index, &pr->key, pr);
	return pr;
}

static void destroy_predicate(module *m, predicate *pr)
{
#if 0
	if (pr->cnt != 0) {
		printf("*** Warning: unreleased (%u) predicate '%s'/%u\n",
		(unsigned)pr->cnt, C_STR(m, &pr->key), pr->key.arity);
	}
#endif

	map_del(m->index, &pr->key);

	for (db_entry *dbe = pr->head; dbe;) {
		db_entry *save = dbe->next;
		clear_rule(&dbe->cl);
		free(dbe);
		dbe = save;
	}

	map_destroy(pr->idx2);
	map_destroy(pr->idx);
	free(pr);
}

static int predicate_cmpkey(const void *ptr1, const void *ptr2, const void *param, void *l)
{
	const module *m = (const module*)param;
	const cell *p1 = (const cell*)ptr1;
	const cell *p2 = (const cell*)ptr2;

	if (p1->arity < p2->arity)
		return -1;

	if (p1->arity > p2->arity)
		return 1;

	if (p1->val_off == p2->val_off)
		return 0;

	return strcmp(m->pl->pool+p1->val_off, m->pl->pool+p2->val_off);
}

static int index_cmpkey_(const void *ptr1, const void *ptr2, const void *param, void *l, unsigned depth)
{
	const module *m = (const module*)param;
	const cell *p1 = (const cell*)ptr1;
	const cell *p2 = (const cell*)ptr2;

	if (is_smallint(p1)) {
		if (is_smallint(p2)) {
			if (get_smallint(p1) < get_smallint(p2))
				return -1;
			else if (get_smallint(p1) > get_smallint(p2))
				return 1;
			else
				return 0;
		} else if (is_bigint(p2)) {
			return -mp_int_compare_value(&p2->val_bigint->ival, p1->val_int);
		} else if (is_rational(p2)) {
			return -mp_rat_compare_value(&p2->val_bigint->irat, p1->val_int, 1);
		} else if (!is_var(p2))
			return -1;
	} else if (is_bigint(p1)) {
		if (is_bigint(p2)) {
			return mp_int_compare(&p1->val_bigint->ival, &p2->val_bigint->ival);
		} else if (is_smallint(p2)) {
			return mp_int_compare_value(&p1->val_bigint->ival, p2->val_int);
		} else if (!is_var(p2))
			return -1;
	} else if (is_rational(p1)) {
		if (is_rational(p2)) {
			return mp_rat_compare(&p1->val_bigint->irat, &p2->val_bigint->irat);
		} else if (is_bigint(p2)) {
			mpq_t tmp;
			mp_int_init_copy(&tmp.num, &p2->val_bigint->ival);
			mp_int_init_value(&tmp.den, 1);
			int ok = mp_rat_compare(&p1->val_bigint->irat, &tmp);
			mp_rat_clear(&tmp);
			return ok;
		} else if (is_smallint(p2)) {
			return mp_rat_compare_value(&p1->val_bigint->irat, p2->val_int, 1);
		} else if (!is_var(p2))
			return -1;
	} else if (is_float(p1)) {
		if (is_float(p2)) {
			if (get_float(p1) < get_float(p2))
				return -1;
			else if (get_float(p1) > get_float(p2))
				return 1;
			else
				return 0;
		} else if (is_integer(p2))
			return 1;
		else if (!is_var(p2))
			return -1;
	} else if (is_interned(p1) && !p1->arity) {
		if (is_interned(p2) && !p2->arity) {
			if (p1->val_off == p2->val_off)
				return 0;

			return strcmp(C_STR(m, p1), C_STR(m, p2));
		} else if (is_atom(p2))
			return strcmp(C_STR(m, p1), C_STR(m, p2));
		else if (is_number(p2))
			return 1;
		else if (!is_var(p2))
			return -1;
	} else if (is_atom(p1)) {
		if (is_atom(p2))
			return strcmp(C_STR(m, p1), C_STR(m, p2));
		else if (is_number(p2))
			return 1;
		else if (!is_var(p2))
			return -1;
	} else if (is_structure(p1)) {
		if (is_structure(p2)) {
			if (p1->arity < p2->arity)
				return -1;

			if (p1->arity > p2->arity)
				return 1;

			if (p1->val_off != p2->val_off)
				return strcmp(C_STR(m, p1), C_STR(m, p2));

			int arity = p1->arity;
			p1++; p2++;

			while (arity--) {
				if (is_var(p1) || is_var(p2)) {
					if (map_is_find(l))
						break;

					map_wild_card(l);
					p1 += p1->nbr_cells;
					p2 += p2->nbr_cells;
					continue;
				}

				int ok = index_cmpkey_(p1, p2, param, l, depth+1);

				if (ok != 0)
					return ok;

				p1 += p1->nbr_cells;
				p2 += p2->nbr_cells;
			}

			return 0;
		} else if (!is_var(p2))
			return 1;
	}

	return 0;
}

int index_cmpkey(const void *ptr1, const void *ptr2, const void *param, void *l)
{
	return index_cmpkey_(ptr1, ptr2, param, l, 0);
}

db_entry *find_in_db(module *m, uuid *ref)
{
	for (predicate *pr = m->head; pr; pr = pr->next) {
		for (db_entry *dbe = pr->head ; dbe; dbe = dbe->next) {
			if (dbe->cl.dgen_erased)
				continue;

			if (!memcmp(&dbe->u, ref, sizeof(uuid)))
				return dbe;
		}
	}

	return NULL;
}

static void push_property(module *m, const char *name, unsigned arity, const char *type)
{
	char tmpbuf[1024];
	format_property(m, tmpbuf, sizeof(tmpbuf), name, arity, type);
	parser *p = parser_create(m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	p->internal = true;
	tokenize(p, false, false);
	parser_destroy(p);
}

db_entry *erase_from_db(module *m, uuid *ref)
{
	db_entry *dbe = find_in_db(m, ref);
	if (!dbe) return 0;
	dbe->cl.dgen_erased = ++m->pl->ugen;
	return dbe;
}

void set_discontiguous_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		push_property(m, name, arity, "discontiguous");
		pr->is_discontiguous = true;
	} else
		m->error = true;
}

void set_multifile_in_db(module *m, const char *name, pl_idx_t arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		push_property(m, name, arity, "multifile");
		pr->is_multifile = true;
	} else
		m->error = true;
}

void set_dynamic_in_db(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		push_property(m, name, arity, "dynamic");
		pr->is_dynamic = true;
	} else
		m->error = true;
}

void set_meta_predicate_in_db(module *m, cell *c)
{
	const char *name = C_STR(m, c);
	unsigned arity = c->arity;
	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = index_from_pool(m->pl, name);
	ensure(tmp.val_off != ERR_IDX);
	tmp.arity = arity;
	predicate *pr = find_predicate(m, &tmp);
	if (!pr) pr = create_predicate(m, &tmp);

	if (pr) {
		query q = (query){0};
		q.pl = m->pl;
		q.st.m = m;
		char *dst = print_canonical_to_strbuf(&q, c, 0, 0);
		char tmpbuf[1024];
		snprintf(tmpbuf, sizeof(tmpbuf), "meta_predicate(%s)", dst);
		free(dst);
		push_property(m, name, arity, tmpbuf);
		pr->is_meta_predicate = true;
	} else
		m->error = true;
}

static bool is_check_directive(const cell *c)
{
	if (is_structure(c) && (c->val_off == g_neck_s) && (c->arity == 1))
		return true;

	return false;
}

bool do_use_module_1(module *curr_m, cell *p)
{
	cell *p1 = p + 1;
	const char *name = C_STR(curr_m, p1);
	char dstbuf[1024*4];

	if (is_structure(p1) && !strcmp(name, "library")) {
		p1 = p1 + 1;
		if (!is_interned(p1)) return false;
		name = C_STR(curr_m, p1);
		module *m;

		if ((m = find_module(curr_m->pl, name)) != NULL) {
			if (m != curr_m)
				curr_m->used[curr_m->idx_used++] = m;

			return true;
		}

		if (!strcmp(name, "between")
		    || !strcmp(name, "samsort")
		    || !strcmp(name, "terms")
		    || !strcmp(name, "types")
			|| !strcmp(name, "iso_ext")
			|| !strcmp(name, "loader")
		    || !strcmp(name, "files"))
			return true;

		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, name))
				continue;

			char *src = malloc(*lib->len+1);
			check_heap_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			SB(s1);
			SB_sprintf(s1, "library%c%s", PATH_SEP_CHAR, lib->name);
			m = load_text(curr_m, src, SB_cstr(s1));
			SB_free(s1);
			free(src);

			if (m != curr_m)
				curr_m->used[curr_m->idx_used++] = m;

			return true;
		}

		snprintf(dstbuf, sizeof(dstbuf), "%s%c%s", g_tpl_lib, PATH_SEP_CHAR, C_STR(curr_m, p1));
		name = dstbuf;
	}

	if (true) {
		module *m;

		if ((m = find_module(curr_m->pl, name)) != NULL) {
			if (m != curr_m)
				curr_m->used[curr_m->idx_used++] = m;

			return true;
		}

		if (!strcmp(name, "between")
		    || !strcmp(name, "samsort")
		    || !strcmp(name, "terms")
		    || !strcmp(name, "types")
			|| !strcmp(name, "iso_ext")
		    || !strcmp(name, "files"))
			return true;

		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, name))
				continue;

			char *src = malloc(*lib->len+1);
			check_heap_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			SB(s1);
			SB_sprintf(s1, "library/%s", lib->name);
			m = load_text(curr_m, src, SB_cstr(s1));
			SB_free(s1);
			free(src);

			if (m != curr_m)
				curr_m->used[curr_m->idx_used++] = m;

			return true;
		}
	}

	char *filename = relative_to(curr_m->filename, name);
	module *m;

	if (!(m = load_file(curr_m, filename, false))) {
		fprintf(stdout, "Error: module file not found: %s\n", filename);
		free(filename);
		return false;
	}

	free(filename);

	if (m != curr_m)
		curr_m->used[curr_m->idx_used++] = m;

	return true;
}

bool do_use_module_2(module *curr_m, cell *p)
{
	cell *p1 = p + 1;
	cell *p2 = p1 + p1->nbr_cells;
	LIST_HANDLER(p2);

	if (!do_use_module_1(curr_m, p))
		return false;

	while (is_iso_list(p2)) {
		cell *head = LIST_HEAD(p2);

		if (is_interned(head) && (head->arity == 2)
			&& ((head->val_off == g_as_s) || (head->val_off == g_colon_s))) {
			cell *lhs = head + 1;
			cell *rhs = lhs + lhs->nbr_cells;

			if (is_compound(lhs) && (lhs->arity == 2)
				&& (lhs->val_off == g_slash_s)
				&& is_atom(rhs)) {
				cell tmp = *(lhs+1);
				tmp.arity = get_smalluint(lhs+2);
				predicate *pr = find_predicate(curr_m->used[curr_m->idx_used-1], &tmp);
				tmp.val_off = rhs->val_off;
				predicate *pr2 = create_predicate(curr_m, &tmp);
				pr2->alias = pr;
			} else if (is_compound(lhs) && (lhs->arity == 2)
				&& (lhs->val_off == g_slash_s)
				&& is_compound(lhs) && (lhs->arity == rhs->arity)) {
				cell tmp = *(lhs+1);
				tmp.arity = get_smalluint(lhs+2);
				predicate *pr = find_predicate(curr_m->used[curr_m->idx_used-1], &tmp);
				tmp.val_off = (rhs+1)->val_off;
				predicate *pr2 = create_predicate(curr_m, &tmp);
				pr2->alias = pr;
			} else if (is_compound(lhs) && is_compound(rhs)) {
				// assertz(goal_expansion(rhs, module:lhs))
				query *q = query_create(curr_m, false);
				check_error(q);
				q->varnames = true;
				char *dst1 = print_canonical_to_strbuf(q, rhs, 0, 0);
				char *dst2 = print_canonical_to_strbuf(q, lhs, 0, 0);
				q->varnames = false;
				SB(s);
				module *mod = curr_m->used[curr_m->idx_used-1];
				SB_sprintf(s, "assertz(goal_expansion(%s,%s:%s)).", dst1, mod->name, dst2);
				free(dst2);
				free(dst1);

				parser *p2 = parser_create(curr_m);
				check_error(p2, query_destroy(q));
				q->p = p2;
				p2->skip = true;
				p2->srcptr = SB_cstr(s);
				tokenize(p2, false, false);
				xref_rule(p2->m, p2->cl, NULL);
				execute(q, p2->cl->cells, p2->cl->nbr_vars);
				SB_free(s);
			}
#if 0
		} else {
			cell *lhs = head;

			if (is_compound(lhs) && (lhs->arity == 2) && (lhs->val_off == g_slash_s)) {
				// assertz(goal_expansion(rhs, module:lhs))
				query *q = query_create(curr_m, false);
				check_error(q);
				q->varnames = true;
				char *dst1 = print_canonical_to_strbuf(q, lhs+1, 0, 0);
				q->varnames = false;
				SB(s);
				module *mod = curr_m->used[curr_m->idx_used-1];
				SB_sprintf(s, "assertz(goal_expansion(%s", dst1);
				unsigned arity = get_smalluint(lhs+2);
				unsigned i = 0;

				while (arity--) {
					if (i) { SB_sprintf(s, "%s", ","); }
					else { SB_sprintf(s, "%s", "("); }
					SB_sprintf(s, "_%u", i);
					i++;
				}

				if (i) { SB_sprintf(s, "%s", ")"); }
				SB_sprintf(s, ",%s:%s", mod->name, dst1);

				arity = get_smalluint(lhs+2);
				i = 0;

				while (arity--) {
					if (i) { SB_sprintf(s, "%s", ","); }
					else { SB_sprintf(s, "%s", "("); }
					SB_sprintf(s, "_%u", i);
					i++;
				}

				if (i) { SB_sprintf(s, "%s", ")"); }

				SB_sprintf(s, "%s", ")).");
				free(dst1);

				parser *p2 = parser_create(curr_m);
				check_error(p2, query_destroy(q));
				q->p = p2;
				p2->skip = true;
				p2->srcptr = SB_cstr(s);
				tokenize(p2, false, false);
				xref_rule(p2->m, p2->cl, NULL);
				execute(q, p2->cl->cells, p2->cl->nbr_vars);
				SB_free(s);
			}
#endif
		}

		p2 = LIST_TAIL(p2);
	}

	return true;
}

#if USE_FFI
bool do_foreign_struct(module *m, cell *p)
{
	cell *p1 = p + 1;
	cell *p2 = p1 + p1->nbr_cells;
	LIST_HANDLER(p2);
	const char *symbol = C_STR(m, p1);
	cell *l = p2;
	pl_idx_t l_ctx = 0;

	void *handle = NULL;
	do_register_struct(m, NULL, handle, symbol, l, 0, "invalid");
	return true;
}

bool do_use_foreign_module(module *m, cell *p)
{
	cell *p1 = p + 1;
	cell *p2 = p1 + p1->nbr_cells;
	LIST_HANDLER(p2);

	const char *name = C_STR(m, p1);
	void *handle = do_dlopen(name, 0);

	if (!handle)
		return false;

	while (is_iso_list(p2)) {
		cell *h = LIST_HEAD(p2);
		const char *symbol = C_STR(m, h);
		cell *l = h + 1;
		cell *r = l + l->nbr_cells;
		const char *ret_type = C_STR(m, r);
		do_register_predicate(m, NULL, handle, symbol, l, 0, ret_type);
		p2 = LIST_TAIL(p2);
	}

	return true;
}
#endif

void convert_to_literal(module *m, cell *c)
{
	char *src = DUP_STR(m, c);
	pl_idx_t off = index_from_pool(m->pl, src);
	unshare_cell(c);
	c->tag = TAG_INTERNED;
	c->val_off = off;
	c->match = NULL;
	c->flags = 0;
	free(src);
}

predicate *find_predicate(module *m, cell *c)
{
	cell tmp = *c;
	tmp.tag = TAG_INTERNED;
	tmp.flags = 0;
	tmp.nbr_cells = 1;

	if (is_cstring(c)) {
		tmp.val_off = index_from_pool(m->pl, C_STR(m, c));
	}

	miter *iter = map_find_key(m->index, &tmp);
	predicate *pr = NULL;

	while (map_next_key(iter, (void*)&pr)) {
		if (pr->is_abolished)
			continue;

		map_done(iter);
		return pr;
	}

	map_done(iter);
	return NULL;
}

predicate *find_functor(module *m, const char *name, unsigned arity)
{
	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = index_from_pool(m->pl, name);
	tmp.arity = arity;
	return find_predicate(m, &tmp);
}

predicate *search_predicate(module *m, cell *c, bool *prebuilt)
{
	if (prebuilt)
		*prebuilt = false;

	predicate *pr = find_predicate(m, c);

	if (pr) {
		if (pr->is_prebuilt && prebuilt)
			*prebuilt = true;

		return pr;
	}

	for (unsigned i = 0; i < m->idx_used; i++) {
		module *tmp_m = m->used[i];
		pr = find_predicate(tmp_m, c);

		if (pr) {
			if (pr->is_prebuilt && prebuilt)
				*prebuilt = true;

			return pr;
		}
	}

	for (module *tmp_m = m->pl->modules; tmp_m; tmp_m = tmp_m->next) {
		if (m == tmp_m)
			continue;

		pr = find_predicate(tmp_m, c);

		if (pr) {
			if (pr->is_prebuilt && prebuilt)
				*prebuilt = true;

			m->used[m->idx_used++] = tmp_m;
			return pr;
		}
	}

	return NULL;
}

#define DUMP_KEYS 0

#if DUMP_KEYS
static const char *dump_key(const void *k, const void *v, const void *p)
{
	(void)p; (void)k;
	const op_table *op = (const op_table*)v;
	static char tmpbuf[1024];
	snprintf(tmpbuf, sizeof(tmpbuf), "'%s:%u:%u'", op->name, op->specifier, op->priority);
	return tmpbuf;
}
#endif

bool set_op(module *m, const char *name, unsigned specifier, unsigned priority)
{
	miter *iter = map_find_key(m->ops, name);
	op_table *ptr;

	while (map_next_key(iter, (void**)&ptr)) {
		if (IS_INFIX(ptr->specifier) != IS_INFIX(specifier))
			continue;

		if (!priority) {
			ptr->specifier = 0;
			ptr->priority = 0;
			map_done(iter);
			return true;
		}

		ptr->priority = priority;
		ptr->specifier = specifier;
		map_done(iter);
		return true;
	}

	map_done(iter);
	iter = map_find_key(m->defops, name);

	while (map_next_key(iter, (void**)&ptr)) {
		if (IS_INFIX(ptr->specifier) != IS_INFIX(specifier))
			continue;

		if (!priority) {
			ptr->specifier = 0;
			ptr->priority = 0;
			map_done(iter);
			return true;
		}

		ptr->priority = priority;
		ptr->specifier = specifier;
		map_done(iter);
		return true;
	}

	map_done(iter);
	op_table *tmp = malloc(sizeof(op_table));
	tmp->name = set_known(m, name);
	tmp->priority = priority;
	tmp->specifier = specifier;
	m->user_ops = true;
	map_app(m->ops, tmp->name, tmp);

#if DUMP_KEYS
	sl_dump(m->ops, dump_key, m);
	sl_dump(m->defops, dump_key, m);
#endif

	return true;
}

static unsigned find_op_internal(module *m, const char *name, unsigned specifier)
{
	op_table *ptr;
	miter *iter = map_find_key(m->ops, name);

	while (map_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (ptr->specifier == specifier) {
			map_done(iter);
			return ptr->priority;
		}
	}

	map_done(iter);
	iter = map_find_key(m->defops, name);

	while (map_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (ptr->specifier == specifier) {
			map_done(iter);
			return ptr->priority;
		}
	}

	map_done(iter);
	return 0;
}

unsigned find_op(module *m, const char *name, unsigned specifier)
{
	unsigned priority = find_op_internal(m, name, specifier);

	if (priority)
		return priority;

	for (unsigned i = 0; i < m->idx_used; i++) {
		module *tmp_m = m->used[i];

		if ((m == tmp_m) || !tmp_m->user_ops)
			continue;

		priority = find_op_internal(tmp_m, name, specifier);

		if (priority)
			return priority;
	}

	return 0;
}

static unsigned search_op_internal(module *m, const char *name, unsigned *specifier, bool hint_prefix)
{
	op_table *ptr;
	miter *iter = map_find_key(m->defops, name);

	while (map_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (!IS_INFIX(ptr->specifier))
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		unsigned n = ptr->priority;
		map_done(iter);
		return n;
	}

	map_done(iter);
	iter = map_find_key(m->ops, name);

	while (map_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (!IS_INFIX(ptr->specifier))
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		unsigned n = ptr->priority;
		map_done(iter);
		return n;
	}

	map_done(iter);
	iter = map_find_key(m->defops, name);

	while (map_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (IS_INFIX(ptr->specifier))
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		unsigned n = ptr->priority;
		map_done(iter);
		return n;
	}

	map_done(iter);
	iter = map_find_key(m->ops, name);

	while (map_next_key(iter, (void**)&ptr)) {
		if (!ptr->priority)
			continue;

		if (IS_INFIX(ptr->specifier))
			continue;

		if (hint_prefix && !IS_PREFIX(ptr->specifier))
			continue;

		if (specifier) *specifier = ptr->specifier;
		unsigned n = ptr->priority;
		map_done(iter);
		return n;
	}

	map_done(iter);

	if (hint_prefix)
		return search_op_internal(m, name, specifier, false);

	return 0;
}

unsigned search_op(module *m, const char *name, unsigned *specifier, bool hint_prefix)
{
	unsigned priority = search_op_internal(m, name, specifier, hint_prefix);

	if (priority)
		return priority;

	for (unsigned i = 0; i < m->idx_used; i++) {
		module *tmp_m = m->used[i];

		if ((m == tmp_m) || !tmp_m->user_ops)
			continue;

		priority = search_op_internal(tmp_m, name, specifier, hint_prefix);

		if (priority)
			return priority;
	}

#if 0
	for (module *tmp_m = m->pl->modules; tmp_m; tmp_m = tmp_m->next) {
		if ((m == tmp_m) || !tmp_m->user_ops)
			continue;

		priority = search_op_internal(tmp_m, name, specifier, hint_prefix);

		if (priority) {
			//m->used[m->idx_used++] = tmp_m;
			return priority;
		}
	}
#endif

	return 0;
}

static bool check_multifile(module *m, predicate *pr, db_entry *dbe)
{
	if (pr->head && !pr->is_multifile && !pr->is_dynamic
		&& (C_STR(m, &pr->key)[0] != '$')) {
		if ((dbe->filename != pr->head->filename) || pr->is_reload) {
			for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
				retract_from_db(dbe);
				pr->is_processed = false;
				pr->is_reload = false;
			}

			if (dbe->owner->cnt)
				fprintf(stderr, "Warning: overwriting %s/%u\n", C_STR(m, &pr->key), pr->key.arity);

			map_destroy(pr->idx2);
			map_destroy(pr->idx);
			pr->idx2 = pr->idx = NULL;
			pr->head = pr->tail = NULL;
			dbe->owner->cnt = 0;
			return false;
		}
	}

	return true;
}

static void optimize_rule(module *m, db_entry *dbe_orig)
{
	predicate *pr = dbe_orig->owner;
	clause *cl = &dbe_orig->cl;
	cell *head = get_head(cl->cells);
	cell *p1 = head + 1, *p2 = NULL, *p3 = NULL;
	bool matched = false;
	bool p1_matched = false, p2_matched = false, p3_matched = false;
	cl->arg1_is_unique = cl->arg2_is_unique = cl->arg3_is_unique = false;
	cl->is_unique = false;

	if (pr->key.arity > 1)
		p2 = p1 + p1->nbr_cells;

	if (pr->key.arity > 2)
		p3 = p2 + p2->nbr_cells;

	for (db_entry *dbe = dbe_orig->next; dbe; dbe = dbe->next) {
		if (dbe->cl.dgen_erased)
			continue;

		cell *head2 = get_head(dbe->cl.cells);
		cell *h21 = head2 + 1, *h22 = NULL, *h23 = NULL;

		if (pr->key.arity > 1)
			h22 = h21 + h21->nbr_cells;

		if (pr->key.arity > 2)
			h23 = h22 + h22->nbr_cells;

		if (!index_cmpkey(p1, h21, m, NULL))
			p1_matched = true;

		if (pr->key.arity > 1) {
			if (!index_cmpkey(p2, h22, m, NULL))
				p2_matched = true;
		}

		if (pr->key.arity > 2) {
			if (!index_cmpkey(p3, h23, m, NULL))
				p3_matched = true;
		}

		if (!index_cmpkey(head, head2, m, NULL)) {
			matched = true;
			break;
		}
	}

	if (!matched)
		cl->is_unique = true;

	if (!p1_matched && cl->is_unique)
		cl->arg1_is_unique = true;

	if (!p2_matched && cl->is_unique)
		cl->arg2_is_unique = true;

	if (!p3_matched && cl->is_unique)
		cl->arg3_is_unique = true;
}

void just_in_time_rebuild(predicate *pr)
{
	pr->is_processed = true;

	for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
		if (dbe->cl.dgen_erased)
			continue;

		optimize_rule(pr->m, dbe);
	}
}

static db_entry *assert_begin(module *m, unsigned nbr_vars, unsigned nbr_temporaries, cell *p1, bool consulting)
{
	cell *c = p1;

	if (!is_check_directive(c)) {
		c = get_head(p1);

		// Remove module from head if present...

		if ((p1->val_off == g_neck_s) && (c->val_off == g_colon_s) && (c->arity == 2) && is_atom(c+1)) {
			const char *name = C_STR(m, c+1);
			module *tmp_m = find_module(m->pl, name);

			if (!tmp_m)
				m = module_create(m->pl, name);
			else
				m = tmp_m;

			copy_cells(p1+1, p1+3, p1->nbr_cells-3);
			p1->nbr_cells -= 2;
			c = get_head(p1);
		} else if ((c->val_off == g_colon_s) && (c->arity == 2) && is_atom(c+1)) {
			const char *name = C_STR(m, c+1);
			module *tmp_m = find_module(m->pl, name);

			if (!tmp_m)
				m = module_create(m->pl, name);
			else
				m = tmp_m;

			copy_cells(p1, p1+2, p1->nbr_cells-2);
			c = get_head(p1);
		}
	}

	if (!c || !m)
		return NULL;

	predicate *pr = find_predicate(m, c);

	if (pr && !consulting && !pr->is_dynamic)
		return NULL;

	if (!pr) {
		bool found = false, evaluable = false;

		/*
		if (get_builtin_term(m, c, &found, &evaluable), found && !evaluable) {
			//fprintf(stdout, "Error: permission error modifying %s/%u\n", C_STR(m, c), c->arity);
			return NULL;
		}
		*/

		pr = create_predicate(m, c);
		check_error(pr);

		if (is_check_directive(p1))
			pr->is_check_directive = true;

		if (!consulting) {
			push_property(m, C_STR(m, c), c->arity, "dynamic");
			pr->is_dynamic = true;
		} else {
			if (m->prebuilt)
				push_property(m, C_STR(m, c), c->arity, "built_in");

			push_property(m, C_STR(m, c), c->arity, "static");
		}

		if (consulting && m->make_public) {
			push_property(m, C_STR(m, c), c->arity, "public");
			pr->is_public = true;
		}
	}

	if (m->prebuilt)
		pr->is_prebuilt = true;

	size_t dbe_size = sizeof(db_entry) + (sizeof(cell) * (p1->nbr_cells+1));
	db_entry *dbe = calloc(1, dbe_size);

	if (!dbe) {
		pr->is_abolished = true;
		return NULL;
	}

	copy_cells(dbe->cl.cells, p1, p1->nbr_cells);
	dbe->cl.cells[p1->nbr_cells] = (cell){0};
	dbe->cl.cells[p1->nbr_cells].tag = TAG_END;
	dbe->cl.nbr_temporaries = nbr_temporaries;
	dbe->cl.nbr_vars = nbr_vars;
	dbe->cl.allocated_cells = p1->nbr_cells;
	dbe->cl.cidx = p1->nbr_cells+1;
	dbe->cl.dgen_created = ++m->pl->ugen;
	dbe->filename = m->filename;
	dbe->owner = pr;
	return dbe;
}

static void assert_commit(module *m, db_entry *dbe, predicate *pr, bool append)
{
	if (pr->db_id)
		dbe->db_id = append ? pr->db_id : -pr->db_id;

	pr->db_id++;
	pr->cnt++;

	clause *cl = &dbe->cl;
	cl->arg1_is_unique = false;
	cl->arg2_is_unique = false;
	cl->arg3_is_unique = false;

	uuid_gen(m->pl, &dbe->u);

	if (pr->is_noindex)
		return;

	if (!pr->idx) {
		bool sys = C_STR(m, &pr->key)[0] == '$';

		if (pr->cnt < 1500)
			return;

		pr->idx = map_create(index_cmpkey, NULL, m);
		ensure(pr->idx);
		map_allow_dups(pr->idx, true);

		if (pr->key.arity > 1) {
			pr->idx2 = map_create(index_cmpkey, NULL, m);
			ensure(pr->idx2);
			map_allow_dups(pr->idx2, true);
		}

		for (db_entry *cl2 = pr->head; cl2; cl2 = cl2->next) {
			cell *c = get_head(cl2->cl.cells);

			if (cl2->cl.dgen_erased)
				continue;

			map_app(pr->idx, c, cl2);

			if (pr->idx2) {
				cell *arg1 = c + 1;
				cell *arg2 = arg1 + arg1->nbr_cells;
				map_app(pr->idx2, arg2, cl2);
			}
		}

		return;
	}

	cell *c = get_head(dbe->cl.cells);
	cell *arg1 = c->arity ? c + 1 : NULL;
	cell *arg2 = arg1 ? arg1 + arg1->nbr_cells : NULL;

	if (arg1 && is_var(arg1))
		pr->is_var_in_first_arg = true;

	if (!append) {
		map_set(pr->idx, c, dbe);

		if (pr->idx2 && arg2)
			map_set(pr->idx2, arg2, dbe);
	} else {
		map_app(pr->idx, c, dbe);

		if (pr->idx2 && arg2)
			map_app(pr->idx2, arg2, dbe);
	}
}

db_entry *asserta_to_db(module *m, unsigned nbr_vars, unsigned nbr_temporaries, cell *p1, bool consulting)
{
	db_entry *dbe;
	predicate *pr;

	do {
		dbe = assert_begin(m, nbr_vars, nbr_temporaries, p1, consulting);
		if (!dbe) return NULL;
		pr = dbe->owner;

		if (pr->head)
			pr->head->prev = dbe;
	}
	 while (!check_multifile(m, pr, dbe));

	dbe->next = pr->head;
	pr->head = dbe;

	if (!pr->tail)
		pr->tail = dbe;

	assert_commit(m, dbe, pr, false);

	if (!consulting && !pr->idx)
		pr->is_processed = false;

	return dbe;
}

db_entry *assertz_to_db(module *m, unsigned nbr_vars, unsigned nbr_temporaries, cell *p1, bool consulting)
{
	db_entry *dbe;
	predicate *pr;

	do {
		dbe = assert_begin(m, nbr_vars, nbr_temporaries, p1, consulting);
		if (!dbe) return NULL;
		pr = dbe->owner;

		if (pr->tail)
			pr->tail->next = dbe;
	}
	 while (!check_multifile(m, pr, dbe));

	dbe->prev = pr->tail;
	pr->tail = dbe;

	if (!pr->head)
		pr->head = dbe;

	assert_commit(m, dbe, pr, true);

	if (!consulting && !pr->idx)
		pr->is_processed = false;

	return dbe;
}

static bool retract_from_predicate(db_entry *dbe)
{
	if (dbe->cl.dgen_erased)
		return false;

	predicate *pr = dbe->owner;
	module *m = pr->m;
	dbe->cl.dgen_erased = ++m->pl->ugen;
	dbe->filename = NULL;
	pr->cnt--;

	if (pr->idx && !pr->cnt && 0) {
		map_destroy(pr->idx2);
		map_destroy(pr->idx);
		pr->idx2 = NULL;

		pr->idx = map_create(index_cmpkey, NULL, m);
		ensure(pr->idx);
		map_allow_dups(pr->idx, true);

		if (pr->key.arity > 1) {
			pr->idx2 = map_create(index_cmpkey, NULL, m);
			ensure(pr->idx2);
			map_allow_dups(pr->idx2, true);
		}
	}

	return true;
}

void retract_from_db(db_entry *dbe)
{
	if (!retract_from_predicate(dbe))
		return;

	predicate *pr = dbe->owner;
	dbe->dirty = pr->dirty_list;
	pr->dirty_list = dbe;
}

static void xref_cell(module *m, clause *cl, cell *c, predicate *parent)
{
	unsigned specifier;

	if ((c->arity == 2)
		&& !GET_OP(c)
		&& (c->val_off != g_braces_s)
		&& search_op(m, C_STR(m, c), &specifier, false)) {
		if (IS_INFIX(specifier))
			SET_OP(c, specifier);
	}

	bool found = false, evaluable = false;
	c->fn_ptr = get_builtin_term(m, c, &found, &evaluable);

	if (found) {
		if (evaluable)
			c->flags |= FLAG_EVALUABLE;
		else
			c->flags |= FLAG_BUILTIN;

		return;
	} else
		c->fn_ptr = NULL;

	if ((c+c->nbr_cells) >= (cl->cells + cl->cidx-1)) {
		if (parent && (parent->key.val_off == c->val_off) && (parent->key.arity == c->arity)) {
			c->flags |= FLAG_TAIL_REC;
			cl->is_tail_rec = true;
		}
	}
}

void xref_rule(module *m, clause *cl, predicate *parent)
{
	cl->arg1_is_unique = false;
	cl->arg2_is_unique = false;
	cl->arg3_is_unique = false;
	cl->is_unique = false;
	cl->is_tail_rec = false;

	cell *c = cl->cells;

	if (c->val_off == g_sys_record_key_s)
		return;

	for (pl_idx_t i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;
		c->flags &= ~FLAG_TAIL_REC;

		if (!is_interned(c))
			continue;

		xref_cell(m, cl, c, parent);
	}
}

void xref_db(module *m)
{
	for (predicate *pr = m->head; pr && !g_tpl_interrupt; pr = pr->next) {
		if (pr->is_processed)
			continue;

		pr->is_processed = true;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next)
			xref_rule(m, &dbe->cl, pr);

		if (pr->is_dynamic || pr->idx)
			continue;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next)
			optimize_rule(m, dbe);
	}
}

module *load_text(module *m, const char *src, const char *filename)
{
	parser *p = parser_create(m);
	check_error(p);
	const char *save_filename = p->m->filename;
	p->m->filename = set_known(m, filename);
	p->consulting = true;
	p->srcptr = (char*)src;
	tokenize(p, false, false);

	if (!p->error && !p->already_loaded && !p->end_of_term && p->cl->cidx) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement, %s:%d\n", filename, p->line_nbr);

		p->error = true;
	}

	if (!p->error) {
		xref_db(p->m);
		int save = p->m->pl->quiet;
		//p->m->pl->quiet = true;
		p->m->pl->halt = false;
		p->directive = true;

		if (p->run_init) {
			p->consulting = false;
			p->command = true;
			SB(src);
			SB_sprintf(src, "forall(%s:retract((:- initialization(__G_))), (__G_ -> true ; format('Warning: call(~w) failed~n', [__G_])))", p->m->name);

			if (run(p, SB_cstr(src), false, NULL, 0))
				p->m->pl->status = false;

			SB_free(src);
		}

		p->command = p->directive = false;
		p->m->pl->quiet = save;
	}

	module *save_m = p->m;
	p->m->filename = save_filename;
	parser_destroy(p);
	return save_m;
}

static bool unload_realfile(module *m, const char *filename)
{
	for (predicate *pr = m->head; pr; pr = pr->next) {
		if (pr->is_multifile || pr->is_dynamic)
			continue;

		if (pr->filename && strcmp(pr->filename, filename))
			continue;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
			if (dbe->cl.dgen_erased)
				continue;

			if (dbe->filename && !strcmp(dbe->filename, filename)) {
				if (!retract_from_predicate(dbe))
					continue;

				dbe->dirty = pr->dirty_list;
				pr->dirty_list = dbe;
				pr->is_processed = false;
			}
		}

		map_destroy(pr->idx2);
		map_destroy(pr->idx);
		pr->idx2 = pr->idx = NULL;

		if (!pr->cnt) {
			if (!pr->is_multifile && !pr->is_dynamic)
				pr->is_abolished = true;
		} else
			xref_db(m);
	}

	set_unloaded(m, filename);
	return true;
}

bool unload_file(module *m, const char *filename)
{
	size_t len = strlen(filename);
	char *tmpbuf = malloc(len + 20);
	memcpy(tmpbuf, filename, len+1);

	if (tmpbuf[0] == '~') {
		const char *ptr = getenv("HOME");

		if (ptr) {
			tmpbuf = realloc(tmpbuf, strlen(ptr) + 10 + strlen(filename) + 20);
			strcpy(tmpbuf, ptr);
			strcat(tmpbuf, filename+1);
			convert_path(tmpbuf);
		}
	}

	char *savebuf = strdup(tmpbuf);
	char *realbuf = NULL;

	if (!(realbuf = realpath(tmpbuf, NULL))) {
		strcpy(tmpbuf, savebuf);

		strcat(tmpbuf, ".pl");

		if (!(realbuf = realpath(tmpbuf, NULL))) {
			free(tmpbuf);
			return false;
		}
	}

	free(savebuf);
	free(tmpbuf);
	filename = realbuf;
	bool ok = unload_realfile(m, filename);
	free(realbuf);
	return ok;
}

module *load_fp(module *m, FILE *fp, const char *filename, bool including)
{
	parser *p = parser_create(m);
	if (!p) return NULL;
	const char *save_filename = m->filename;
	if (!including) m->filename = set_known(m, filename);
	p->consulting = true;
	p->fp = fp;
	bool ok = false;

	virtual_term(p, "begin_of_file.");
	tokenize(p, false, false);

	do {
		if (getline(&p->save_line, &p->n_line, p->fp) == -1) {
			virtual_term(p, "end_of_file.");
			break;
		}

		p->srcptr = p->save_line;

		if (!tokenize(p, false, false))
			break;

		ok = !p->error;
	}
	 while (ok && !p->already_loaded && !g_tpl_interrupt);

	if (!p->error && !p->already_loaded && !p->end_of_term && p->cl->cidx) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, incomplete statement, %s:%d\n", filename, p->line_nbr);

		p->error = true;
	}

	module *save_m = p->m;

	if (!p->error && !p->already_loaded) {
		xref_db(p->m);
		int save = p->m->pl->quiet;
		//p->m->pl->quiet = true;
		p->directive = true;

		if (p->run_init) {
			p->command = true;
			p->consulting = false;
			SB(src);
			SB_sprintf(src, "forall(%s:retract((:- initialization(__G_))), (__G_ -> true ; format('Warning: call(~w) failed~n', [__G_])))", p->m->name);

			if (run(p, SB_cstr(src), false, NULL, 0))
				p->m->pl->status = false;

			SB_free(src);
		}

		p->command = p->directive = false;
		p->m->pl->quiet = save;
	}

	ok = !p->error;
	parser_destroy(p);
	m->filename = save_filename;

	if (!ok)
		unload_realfile(m, filename);

	return save_m;
}

module *load_file(module *m, const char *filename, bool including)
{
	const char *orig_filename = filename;

	if (!strcmp(filename, "user")) {
		m = m->pl->user_m;

		for (int i = 0; i < MAX_STREAMS; i++) {
			stream *str = &m->pl->streams[i];
			char tmpbuf[256];
			snprintf(tmpbuf, sizeof(tmpbuf), "user");
			filename = set_loaded(m, tmpbuf, orig_filename);

			if (!map_get(str->alias, "user_input", NULL))
				continue;

			for (predicate *pr = m->head; pr; pr = pr->next)
				pr->is_reload = true;

			// Process extra input line text...

			while (m->pl->p && m->pl->p->srcptr && *m->pl->p->srcptr) {
				m->filename = filename;
				parser *p = parser_create(m);
				if (!p) return NULL;
				p->srcptr = m->pl->p->srcptr;
				p->consulting = true;
				p->m = m;

				if (!tokenize(p, false, false))
					break;

				m->pl->p->srcptr = p->srcptr;
				parser_destroy(p);
			}

			module *save_m = load_fp(m, str->fp, filename, including);
			clearerr(str->fp);
			return save_m;
		}
	}

	size_t len = strlen(filename);
	char *tmpbuf = malloc(len + 20);
	memcpy(tmpbuf, filename, len+1);

	if (tmpbuf[0] == '~') {
		const char *ptr = getenv("HOME");

		if (ptr) {
			tmpbuf = realloc(tmpbuf, strlen(ptr) + 10 + strlen(filename) + 20);
			strcpy(tmpbuf, ptr);
			strcat(tmpbuf, filename+1);
			convert_path(tmpbuf);
		}
	}

	char *savebuf = strdup(tmpbuf);
	char *realbuf = NULL;
	strcpy(tmpbuf, filename);

	if (!realbuf) {
		if (!(realbuf = realpath(tmpbuf, NULL))) {
			strcpy(tmpbuf, savebuf);
			strcat(tmpbuf, ".pl");
			realbuf = realpath(tmpbuf, NULL);
		}
	}

	if (!realbuf) {
		if (!(realbuf = realpath(tmpbuf, NULL))) {
			strcpy(tmpbuf, savebuf);
			strcat(tmpbuf, ".pro");
			realbuf = realpath(tmpbuf, NULL);
		}
	}

	if (!realbuf) {
		if (!(realbuf = realpath(tmpbuf, NULL))) {
			strcpy(tmpbuf, savebuf);
			strcat(tmpbuf, ".prolog");
			realbuf = realpath(tmpbuf, NULL);
		}
	}

	free(savebuf);
	free(tmpbuf);

	if (!realbuf)
		return NULL;

	if (is_loaded(m, realbuf))
		return m;

	filename = set_loaded(m, realbuf, orig_filename);

	struct stat st = {0};
	stat(filename, &st);

	if ((st.st_mode & S_IFMT) == S_IFDIR) {
		char *tmpbuf = malloc(strlen(orig_filename)+20);
		strcpy(tmpbuf, orig_filename);
		strcat(tmpbuf, ".pl");
		m = load_file(m, tmpbuf, including);
		free(tmpbuf);
		return m;
	}

	FILE *fp = fopen(filename, "r");

	if (!fp) {
		free(realbuf);
		return NULL;
	}

	// Check for a BOM

	int ch = getc_utf8(fp);

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);

	clearerr(fp);
	module *save_m = load_fp(m, fp, filename, including);
	fclose(fp);
	free(realbuf);
	return save_m;
}

static void module_save_fp(module *m, FILE *fp, int canonical, int dq)
{
	(void) dq;
	pl_idx_t ctx = 0;
	query q = (query){0};
	q.pl = m->pl;
	q.st.m = m;

	for (predicate *pr = m->head; pr; pr = pr->next) {
		if (pr->is_prebuilt)
			continue;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
			if (dbe->cl.dgen_erased)
				continue;

			if (canonical)
				print_canonical(&q, fp, dbe->cl.cells, ctx, 0);
			else
				print_term(&q, fp, dbe->cl.cells, ctx, 0);

			fprintf(fp, "\n");
		}
	}
}

bool save_file(module *m, const char *filename)
{
	FILE *fp = fopen(filename, "w");

	if (!fp) {
		fprintf(stdout, "Error: file '%s' cannot be created\n", filename);
		return false;
	}

	module_save_fp(m, fp, 0, 0);
	fclose(fp);
	return true;
}

#if 0
static void make_rule(module *m, const char *src)
{
	m->prebuilt = true;
	bool save = m->p->consulting;
	m->p->consulting = true;
	m->p->srcptr = (char*)src;
	m->p->line_nbr = 1;
	tokenize(m->p, false, false);
	m->prebuilt = false;
	m->p->consulting = save;
}
#endif

void module_destroy(module *m)
{
	miter *iter = map_first(m->defops);
	op_table *opptr;

	while (map_next(iter, (void**)&opptr))
		free(opptr);

	map_done(iter);
	map_destroy(m->defops);
	iter = map_first(m->ops);

	while (map_next(iter, (void**)&opptr))
		free(opptr);

	map_done(iter);
	map_destroy(m->ops);

	for (predicate *pr = m->head; pr;) {
		predicate *save = pr->next;
		destroy_predicate(m, pr);
		pr = save;
	}

	if (m->pl->modules == m) {
		m->pl->modules = m->next;
	} else {
		for (module *tmp = m->pl->modules; tmp; tmp = tmp->next) {
			if (tmp->next == m) {
				tmp->next = m->next;
				break;
			}
		}
	}

	if (m->fp)
		fclose(m->fp);

	map_destroy(m->index);
	parser_destroy(m->p);
	clear_loaded(m);
	free(m);
}

void module_duplicate(prolog *pl, module *m, const char *name, unsigned arity)
{
	module *tmp_m = module_create(pl, name);
	tmp_m->orig = m;
	tmp_m->arity = arity;
}

module *module_create(prolog *pl, const char *name)
{
	module *m = calloc(1, sizeof(module));
	check_error(m);
	m->pl = pl;
	m->filename = set_known(m, name);
	m->name = set_known(m, name);
	m->flags.unknown = UNK_ERROR;
	m->flags.syntax_error = UNK_ERROR;
	m->flags.double_quote_chars = true;
	m->flags.character_escapes = true;
	m->error = false;
	m->id = ++pl->next_mod_id;
	m->defops = map_create((void*)fake_strcmp, NULL, NULL);
	map_allow_dups(m->defops, false);
	pl->modmap[m->id] = m;

	if (strcmp(name, "system")) {
		for (const op_table *ptr = g_ops; ptr->name; ptr++) {
			op_table *tmp = malloc(sizeof(op_table));
			memcpy(tmp, ptr, sizeof(op_table));
			map_app(m->defops, tmp->name, tmp);
		}
	}

	m->ops = map_create((void*)fake_strcmp, NULL, NULL);
	map_allow_dups(m->ops, false);
	m->index = map_create(predicate_cmpkey, NULL, m);
	map_allow_dups(m->index, false);
	m->p = parser_create(m);
	check_error(m->p);
	set_multifile_in_db(m, "$predicate_property", 2);
	set_multifile_in_db(m, ":-", 1);

	parser *p = parser_create(m);
	if (p) {
		p->consulting = true;
		xref_db(p->m);
		parser_destroy(p);
	}

	if (!m->name || !m->p || m->error || !p) {
		module_destroy(m);
		m = NULL;
	}

	m->next = pl->modules;
	pl->modules = m;

	set_discontiguous_in_db(m, "term_expansion", 2);
	set_discontiguous_in_db(m, "goal_expansion", 2);

	set_multifile_in_db(m, "term_expansion", 2);
	set_multifile_in_db(m, "goal_expansion", 2);
	set_multifile_in_db(m, "initialization", 1);

	set_dynamic_in_db(m, "term_expansion", 2);
	set_dynamic_in_db(m, "goal_expansion", 2);
	set_dynamic_in_db(m, "initialization", 1);
	set_dynamic_in_db(m, ":-", 1);

	return m;
}
