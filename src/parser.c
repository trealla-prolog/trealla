#include <ctype.h>
#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "heap.h"
#include "history.h"
#include "library.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

static const unsigned INITIAL_NBR_CELLS = 1000;
const char *g_solo = "!(){}[]|,;`'\"";

char *slicedup(const char *s, size_t n)
{
	char *ptr = malloc(n+1);
	if (!ptr) return NULL;
	memcpy(ptr, s, n);
	ptr[n] = '\0';
	return ptr;
}

int slicecmp(const char *s1, size_t len1, const char *s2, size_t len2)
{
	size_t min_len = len1 < len2 ? len1 : len2;
	int val = memcmp(s1, s2, min_len);
	if (val) return val > 0 ? 1 : -1;
	return len1 < len2 ? -1 : len1 > len2 ? 1 : 0;
}

static const char *get_filename(const char *path)
{
	const char *ptr = strrchr(path, PATH_SEP_CHAR);

	if (!ptr)
		return path;

	return ptr+1;
}

cell *list_head(cell *l, cell *tmp)
{
	if (!is_string(l))
		return l + 1;

	const char *src = is_slice(l) ? l->val_str : is_strbuf(l) ? (char*)l->val_strb->cstr + l->strb_off : (char*)l->val_chr + l->val_off;
	size_t char_len = len_char_utf8(src);
	tmp->tag = TAG_CSTR;
	tmp->nbr_cells = 1;
	tmp->flags = 0;
	tmp->arity = 0;
	memcpy(tmp->val_chr, src, char_len);
	tmp->val_chr[char_len] = '\0';
	tmp->chr_len = char_len;
	return tmp;
}

cell *list_tail(cell *l, cell *tmp)
{
	if (!is_string(l)) {
		cell *h = l + 1;
		return h + h->nbr_cells;
	}

	const char *src = is_slice(l) ? l->val_str : is_strbuf(l) ? (char*)l->val_strb->cstr + l->strb_off : (char*)l->val_chr;
	size_t char_len = len_char_utf8(src);
	size_t str_len = is_slice(l) ? (size_t)l->str_len : is_strbuf(l) ? (size_t)l->val_strb->len - l->strb_off : (unsigned)l->chr_len;

	if (str_len == char_len) {
		tmp->tag = TAG_INTERNED;
		tmp->nbr_cells = 1;
		tmp->arity = 0;
		tmp->flags = 0;
		tmp->val_off = g_nil_s;
		return tmp;
	}

	if (is_slice(l)) {
		*tmp = *l;
		tmp->val_str = l->val_str + char_len;
		tmp->str_len = l->str_len - char_len;
		return tmp;
	}

	if (is_strbuf(l)) {
		*tmp = *l;
		tmp->strb_off = l->strb_off + char_len;
		tmp->strb_len = l->strb_len - char_len;
		return tmp;
	}

	*tmp = *l;
	memcpy(tmp->val_chr, l->val_chr + char_len, l->chr_len - char_len);
	tmp->val_chr[l->chr_len - char_len] = '\0';
	tmp->chr_len = l->chr_len - char_len;
	return tmp;
}

cell *get_logical_body(cell *c)
{
	cell *body = get_body(c);

	if (!body)
		return NULL;

	// A body of just 'true' is equivalent to no body at all,
	// and of course vice-versa.

	if (!body->arity && is_interned(body) && (body->val_off == g_true_s))
		return NULL;

	return body;
}

void clear_rule(clause *cl)
{
	cell *c = cl->cells;

	for (pl_idx_t i = 0; i < cl->cidx; i++, c++) {
		unshare_cell(c);
	}

	cl->cidx = 0;
}

static bool make_room(parser *p, unsigned nbr)
{
	if ((p->cl->cidx+nbr) >= p->cl->allocated_cells) {
		pl_idx_t nbr_cells = (p->cl->allocated_cells + nbr) * 3 / 2;

		clause *cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*nbr_cells));
		if (!cl) {
			p->error = true;
			return false;
		}

		p->cl = cl;
		p->cl->allocated_cells = nbr_cells;
	}

	return true;
}

static cell *make_a_cell(parser *p)
{
	make_room(p, 1);
	cell *ret = p->cl->cells + p->cl->cidx++;
	*ret = (cell){0};
	return ret;
}

void parser_destroy(parser *p)
{
	if (!p) return;
	SB_free(p->token);

	if (p->save_line)
		free(p->save_line);

	if (p->cl) {
		clear_rule(p->cl);
		free(p->cl);
	}

	free(p);
}

parser *parser_create(module *m)
{
	parser *p = calloc(1, sizeof(parser));
	check_error(p);
	p->pl = m->pl;
	p->m = m;
	pl_idx_t nbr_cells = INITIAL_NBR_CELLS;
	p->cl = calloc(1, sizeof(clause)+(sizeof(cell)*nbr_cells));
	check_error(p->cl, free(p));
	p->cl->allocated_cells = nbr_cells;
	p->start_term = true;
	p->flags = m->flags;
	p->line_nbr = 1;
	return p;
}

static void consultall(parser *p, cell *l)
{
	LIST_HANDLER(l);

	while (is_list(l) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(l);

		if (is_iso_list(h))
			consultall(p, h);
		else {
			char *s = C_STR(p, h);

			if (!load_file(p->m, s, false))
				fprintf(stdout, "Error: file not found: '%s'\n", s);
		}

		l = LIST_TAIL(l);
	}
}

char *relative_to(const char *basefile, const char *relfile)
{
	char *tmpbuf = malloc(strlen(basefile) + strlen(relfile) + 256);
	char *ptr = tmpbuf;

	if (!strncmp(relfile, "../", 3)) {
		strcpy(tmpbuf, basefile);
		ptr = tmpbuf + strlen(tmpbuf) - 1;

		while ((ptr != tmpbuf) && (*ptr != PATH_SEP_CHAR))
			ptr--;

		if (ptr != tmpbuf)
			*ptr++ = PATH_SEP_CHAR;

		*ptr = '\0';
	}

	strcpy(ptr, relfile);
	return tmpbuf;
}

static void do_op(parser *p, cell *c, bool make_public)
{
	cell *p1 = c + 1, *p2 = c + 2, *p3 = c + 3;

	if (!is_integer(p1) || !is_interned(p2) || (!is_atom(p3) && !is_list(p3))) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: unknown op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

		p->error = true;
		return;
	}

	unsigned specifier;
	char *spec = DUP_STR(p, p2);

	if (!strcmp(spec, "fx"))
		specifier = OP_FX;
	else if (!strcmp(spec, "fy"))
		specifier = OP_FY;
	else if (!strcmp(spec, "xf"))
		specifier = OP_XF;
	else if (!strcmp(spec, "yf"))
		specifier = OP_YF;
	else if (!strcmp(spec, "xfx"))
		specifier = OP_XFX;
	else if (!strcmp(spec, "xfy"))
		specifier = OP_XFY;
	else if (!strcmp(spec, "yfx"))
		specifier = OP_YFX;
	else {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: unknown op spec tag, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

		free(spec);
		return;
	}

	free(spec);
	LIST_HANDLER(p3);

	while (is_list(p3) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p3);

		if (is_atom(h)) {
			char *name = DUP_STR(p, h);

			if (!set_op(p->m, name, specifier, get_smallint(p1))) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: could not set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				free(name);
				continue;
			}

			if (make_public) {
				if (!set_op(p->pl->user_m, name, specifier, get_smallint(p1))) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: could not set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					free(name);
					continue;
				}
			}

			free(name);
		}

		p3 = LIST_TAIL(p3);
	}

	if (is_atom(p3) && !is_nil(p3)) {
		char *name = DUP_STR(p, p3);

		if (!set_op(p->m, name, specifier, get_smallint(p1))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: could not set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			free(name);
			return;
		}

		if (make_public) {
			if (!set_op(p->pl->user_m, name, specifier, get_smallint(p1))) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: could not set op, %s:%u\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				free(name);
				return;
			}
		}

		free(name);
	}
}

static bool goal_run(parser *p, cell *goal)
{
	if (p->error || p->internal || !is_interned(goal))
		return false;

	if ((goal->val_off == g_goal_expansion_s) || (goal->val_off == g_cut_s))
		return false;

	query *q = query_create(p->m, false);
	execute(q, goal, MAX_ARITY);

	if (q->retry != QUERY_OK) {
		query_destroy(q);
		return false;
	}

	return true;
}

static bool conditionals(parser *p, cell *d)
{
	p->skip = false;

	if (!is_interned(d))
		return false;

	if (strcmp(C_STR(p, d), ":-"))
		return false;

	cell *c = d + 1;

	if (!is_interned(c))
		return false;

	const char *dirname = C_STR(p, c);

	if (!strcmp(dirname, "endif") && (c->arity == 0) && !p->m->ifs_blocked[p->m->if_depth]) {
		--p->m->if_depth;
		return true;
	}

	if (!strcmp(dirname, "endif") && (c->arity == 0)) {
		--p->m->if_depth;
		return true;
	}

	if (!strcmp(dirname, "if") && (c->arity == 1) && !p->m->ifs_done[p->m->if_depth] && !p->m->ifs_blocked[p->m->if_depth]) {
		bool ok = goal_run(p, c+1);
		p->m->ifs_blocked[++p->m->if_depth] = !ok;
		p->m->ifs_done[p->m->if_depth] = ok;
		return true;
	}

	if (!strcmp(dirname, "if") && (c->arity == 1)) {
		bool save1 = p->m->ifs_blocked[p->m->if_depth];
		bool save2 = p->m->ifs_done[p->m->if_depth];
		p->m->ifs_blocked[++p->m->if_depth] = save1;
		p->m->ifs_done[p->m->if_depth] = true;
		return true;
	}

	if (!strcmp(dirname, "elif") && (c->arity == 1) && !p->m->ifs_done[p->m->if_depth] && p->m->ifs_blocked[p->m->if_depth]) {
		bool ok = goal_run(p, c+1);
		p->m->ifs_blocked[p->m->if_depth] = !ok;
		p->m->ifs_done[p->m->if_depth] = ok;
		return true;
	}

	if (!strcmp(dirname, "elif") && (c->arity == 1)) {
		p->m->ifs_blocked[p->m->if_depth] = true;
		return true;
	}

	if (!strcmp(dirname, "else") && (c->arity == 0) && !p->m->ifs_done[p->m->if_depth] && p->m->ifs_blocked[p->m->if_depth]) {
		p->m->ifs_blocked[p->m->if_depth] = false;
		p->m->ifs_done[p->m->if_depth] = true;
		return true;
	}

	if (!strcmp(dirname, "else") && (c->arity == 0)) {
		p->m->ifs_blocked[p->m->if_depth] = true;
		return true;
	}

	return false;
}

static void directives(parser *p, cell *d)
{
	p->skip = false;

	if (!is_interned(d))
		return;

	if (is_list(d) && p->command) {
		consultall(p, d);
		p->skip = true;
		return;
	}

	if (strcmp(C_STR(p, d), ":-"))
		return;

	cell *c = d + 1;

	if (!is_interned(c))
		return;

	const char *dirname = C_STR(p, c);

	if (d->arity != 1)
		return;

	if (!strcmp(dirname, "initialization") && (c->arity <= 2)) {
		p->run_init = true;
		return;
	}

	if (!strcmp(dirname, "info") && (c->arity == 1)) {
		printf("INFO: %s\n", C_STR(p, c+1));
		return;
	}

	cell *p1 = c + 1;

	if (!strcmp(dirname, "help") && (c->arity == 2)) {
		if (!is_structure(p1)) return;
		cell *p2 = p1 + p1->nbr_cells;
		if (!is_iso_list_or_nil(p2)) return;
		LIST_HANDLER(p2);
		char *desc = NULL;
		bool iso = false;

		while (is_iso_list(p2)) {
			cell *h = LIST_HEAD(p2);

			if (is_structure(h) && is_atom(h+1) && !strcmp(C_STR(p, h), "iso")) {
				cell *arg = h + 1;
				iso = !strcmp(C_STR(p, arg), "true");
			}

			if (is_structure(h) && is_atom(h+1) && !strcmp(C_STR(p, h), "desc")) {
				cell *arg = h + 1;
				desc = DUP_STR(p, arg);
			}

			p2 = LIST_TAIL(p2);
		}

		pl_idx_t p1_ctx = 0;
		query q = (query){0};
		q.pl = p->pl;
		q.st.m = p->m;
		char *dst = print_term_to_strbuf(&q, p1, p1_ctx, 0);
		builtins *ptr = calloc(1, sizeof(builtins));
		ptr->name = strdup(C_STR(p, p1));
		ptr->arity = p1->arity;
		ptr->m = p->m;
		ptr->desc = desc;
		char *src = dst;

		while (*src && (*src != '('))
			src++;

		if (*src == '(')
			src++;

		char *end = dst + strlen(dst) - 1;
		*end = '\0';

		ptr->help = *src ? src : dst;
		ptr->help2 = dst;
		ptr->iso = iso;
		ptr->via_directive = true;
		map_app(p->pl->help, ptr->name, ptr);
		return;
	}

	if (!strcmp(dirname, "include") && (c->arity == 1)) {
		if (!is_atom(p1)) return;
		unsigned save_line_nbr = p->line_nbr;
		const char *name = C_STR(p, p1);
		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename, true)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: not found: %s:%d\n", filename, p->line_nbr);

			free(filename);
			p->line_nbr = save_line_nbr;
			p->error = true;
			return;
		}

		free(filename);
		p->line_nbr = save_line_nbr;
		return;
	}

	if (!strcmp(dirname, "ensure_loaded") && (c->arity == 1)) {
		if (!is_atom(p1)) return;
		unsigned save_line_nbr = p->line_nbr;
		const char *name = C_STR(p, p1);
		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename, false)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: not found: %s:%d\n", filename, p->line_nbr);

			free(filename);
			p->line_nbr = save_line_nbr;
			p->error = true;
			return;
		}

		free(filename);
		p->line_nbr = save_line_nbr;
		return;
	}

	if (!strcmp(dirname, "pragma") && (c->arity == 2)) {
		cell *p2 = c + 2;
		const char *name = "";
		char tmpbuf[1024];

		if (is_var(p1)) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", p->m->filename);
			char *ptr = tmpbuf + strlen(tmpbuf) - 1;

			while (*ptr && (*ptr != '.') && (ptr != tmpbuf))
				ptr--;

			if (*ptr == '.')
				*ptr = '\0';

			name = tmpbuf;
		} else if (!is_atom(p1)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: pragma name not an atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error = true;
			return;
		} else
			name = C_STR(p, p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->m->pl, name)) != NULL) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: module already loaded: %s, %s:%d\n", name, get_loaded(p->m, p->m->filename), p->line_nbr);
			//
			p->already_loaded = true;
			p->m = tmp_m;

			if (tmp_m != p->m)
				p->m->used[p->m->idx_used++] = tmp_m;

			return;
		}

		tmp_m = module_create(p->m->pl, name);
		if (!tmp_m) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module creation failed: %s, %s:%d\n", name, get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error = true;
			return;
		}

		if (tmp_m != p->m)
			p->m->used[p->m->idx_used++] = tmp_m;

		LIST_HANDLER(p2);

		while (is_iso_list(p2) && !g_tpl_interrupt) {
			LIST_HEAD(p2);
			p2 = LIST_TAIL(p2);
		}

		return;
	}

	if (!strcmp(dirname, "attribute") && (c->arity == 1)) {
		cell *arg = c + 1;

		if (arg->val_off == g_slash_s) {
			cell *f = arg;
			char *name = C_STR(p->m, f+1);
			unsigned arity = get_smallint(f+2);
			module_duplicate(p->m->pl, p->m, name, arity);
			return;
		}

		while (arg->val_off == g_conjunction_s) {
			cell *f = arg + 1;

			if (!is_structure(f))
				break;

			if (f->val_off != g_slash_s)
				break;

			char *name = C_STR(p->m, f+1);
			unsigned arity = get_smallint(f+2);
			module_duplicate(p->m->pl, p->m, name, arity);
			arg += 4;
		}

		return;
	}

	if (!strcmp(dirname, "module") && (c->arity >= 1)) {
		const char *name = "";
		char tmpbuf[1024];

		if (is_var(p1)) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", p->m->filename);
			char *ptr = tmpbuf + strlen(tmpbuf) - 1;

			while (*ptr && (*ptr != '.') && (ptr != tmpbuf))
				ptr--;

			if (*ptr == '.')
				*ptr = '\0';

			name = tmpbuf;
		} else if (!is_atom(p1)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module name not an atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error = true;
			return;
		} else
			name = C_STR(p, p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->m->pl, name)) != NULL) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: module already loaded: %s, %s:%d\n", name, get_loaded(p->m, p->m->filename), p->line_nbr);
			//
			p->already_loaded = true;
			p->m = tmp_m;
			return;
		}

		tmp_m = module_create(p->m->pl, name);

		if (!tmp_m) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module creation failed: %s, %s:%d\n", name, get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error = true;
			return;
		}

		p->m = tmp_m;

		if (c->arity == 1)
			return;

		cell *p2 = c + 2;
		LIST_HANDLER(p2);

		while (is_iso_list(p2) && !g_tpl_interrupt) {
			cell *head = LIST_HEAD(p2);

			if (is_structure(head)) {
				if (!strcmp(C_STR(p, head), "/")
					|| !strcmp(C_STR(p, head), "//")) {
					cell *f = head+1, *a = f+1;
					if (!is_interned(f)) return;
					if (!is_integer(a)) return;
					cell tmp = *f;
					tmp.arity = get_smallint(a);

					if (!strcmp(C_STR(p, head), "//"))
						tmp.arity += 2;

					predicate *pr = find_predicate(p->m, &tmp);
					if (!pr) pr = create_predicate(p->m, &tmp);
					if (!pr) {
						module_destroy(p->m);
						p->m = NULL;
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: predicate creation failed, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

						p->error = true;
						return;
					}

					pr->is_public = true;
				} else if (!strcmp(C_STR(p, head), "op") && (head->arity == 3)) {
					do_op(p, head, true);
				}
			}

			p2 = LIST_TAIL(p2);
		}

		return;
	}

	if ((!strcmp(dirname, "use_module") || !strcmp(dirname, "autoload") || !strcmp(dirname, "reexport")) && (c->arity >= 1)) {
		if (!is_callable(p1))
			return;

		c->arity == 1 ? do_use_module_1(p->m, c) : do_use_module_2(p->m, c);
		return;
	}

#if USE_FFI
	if (!strcmp(dirname, "foreign_struct") && (c->arity == 2)) {
		if (!is_iso_atom(p1)) {
			p->error = true;
			return;
		}

		do_foreign_struct(p->m, c);
		return;
	}

	if (!strcmp(dirname, "use_foreign_module") && (c->arity == 2)) {
		if (!is_atom(p1)) {
			p->error = true;
			return;
		}

		do_use_foreign_module(p->m, c);
		return;
	}
#endif

	if (!strcmp(dirname, "meta_predicate") && (c->arity == 1)) {
		if (!is_structure(p1))
			return;
	}

	if (!strcmp(dirname, "set_prolog_flag") && (c->arity == 2)) {
		cell *p2 = c + 2;

		if (!is_interned(p2))
			return;

		if (!strcmp(C_STR(p, p1), "double_quotes")) {
			if (!strcmp(C_STR(p, p2), "atom")) {
				p->m->flags.double_quote_chars = p->m->flags.double_quote_codes = false;
				p->m->flags.double_quote_atom = true;
			} else if (!strcmp(C_STR(p, p2), "codes")) {
				p->m->flags.double_quote_chars = p->m->flags.double_quote_atom = false;
				p->m->flags.double_quote_codes = true;
			} else if (!strcmp(C_STR(p, p2), "chars")) {
				p->m->flags.double_quote_atom = p->m->flags.double_quote_codes = false;
				p->m->flags.double_quote_chars = true;
			} else {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: unknown value, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error = true;
				return;
			}
		} else if (!strcmp(C_STR(p, p1), "character_escapes")) {
			if (!strcmp(C_STR(p, p2), "true") || !strcmp(C_STR(p, p2), "on"))
				p->m->flags.character_escapes = true;
			else if (!strcmp(C_STR(p, p2), "false") || !strcmp(C_STR(p, p2), "off"))
				p->m->flags.character_escapes = false;
		} else {
			//fprintf(stdout, "Warning: unknown flag: %s\n", C_STR(p, p1));
		}

		p->flags = p->m->flags;
		return;
	}

	if (!strcmp(dirname, "op") && (c->arity == 3)) {
		do_op(p, c, false);
		return;
	}

	LIST_HANDLER(p1);

	while (is_list(p1) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p1);

		if (is_interned(h) && (!strcmp(C_STR(p, h), "/") || !strcmp(C_STR(p, h), "//")) && (h->arity == 2)) {
			cell *c_name = h + 1;

			if (!is_atom(c_name))
				continue;

			cell *c_arity = h + 2;

			if (!is_integer(c_arity))
				continue;

			unsigned arity = get_smallint(c_arity);

			if (!strcmp(C_STR(p, h), "//"))
				arity += 2;

			cell tmp = *c_name;
			tmp.arity = arity;

			if (!strcmp(dirname, "dynamic")) {
				predicate * pr = find_predicate(p->m, &tmp);

				if (pr && !pr->is_dynamic && pr->cnt) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: no permission to modify static predicate %s:%s/%u, %s:%d\n", p->m->name, C_STR(p->m, c_name), arity, get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error = true;
					return;
				}

				set_dynamic_in_db(p->m, C_STR(p, c_name), arity);
			} else if (!strcmp(dirname, "encoding")) {
			} else if (!strcmp(dirname, "public")) {
			} else if (!strcmp(dirname, "export")) {
			} else if (!strcmp(dirname, "discontiguous")) {
				set_discontiguous_in_db(p->m, C_STR(p, c_name), arity);
			} else if (!strcmp(dirname, "multifile")) {
				const char *src = C_STR(p, c_name);

				if (!strchr(src, ':')) {
					set_multifile_in_db(p->m, src, arity);
				} else {
					char mod[256], name[256];
					mod[0] = name[0] = '\0';
					sscanf(src, "%255[^:]:%255s", mod, name);
					mod[sizeof(mod)-1] = name[sizeof(name)-1] = '\0';

					if (!is_multifile_in_db(p->m->pl, mod, name, arity)) {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: not multifile %s:%s/%u\n", mod, name, arity);

						p->error = true;
						return;
					}
				}
			} else {
				if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
					fprintf(stdout, "Warning: unknown directive: %s/%d\n", dirname, c->arity);
			}
		}

		p1 = LIST_TAIL(p1);
	}

	if (is_nil(p1))
		return;

	while (is_interned(p1) && !g_tpl_interrupt) {
		module *m = p->m;
		cell *c_id = p1;

		if (!strcmp(C_STR(p, p1), ":") && (p1->arity == 2)) {
			cell *c_mod = p1 + 1;

			if (!is_atom(c_mod))
				return;

			m = find_module(p->m->pl, C_STR(p, c_mod));

			if (!m)
				m = module_create(p->pl, C_STR(p, c_mod));

			c_id = p1 + 2;
		}

		if (!strcmp(C_STR(p, c_id), "/") && (p1->arity == 2)) {
			cell *c_name = c_id + 1;

			if (!is_atom(c_name))
				return;

			cell *c_arity = c_id + 2;

			if (!is_integer(c_arity))
				return;

			unsigned arity = get_smallint(c_arity);
			cell tmp = *c_name;
			tmp.arity = arity;

			if (!strcmp(C_STR(p, c_id), "//"))
				arity += 2;

			if (!strcmp(dirname, "multifile"))
				set_multifile_in_db(m, C_STR(p, c_name), arity);
			else if (!strcmp(dirname, "discontiguous"))
				set_discontiguous_in_db(m, C_STR(p, c_name), arity);
			else if (!strcmp(dirname, "public"))
				;
			else if (!strcmp(dirname, "export"))
				;
			else if (!strcmp(dirname, "dynamic")) {
				predicate * pr = find_predicate(p->m, &tmp);

				if (pr && !pr->is_dynamic && pr->cnt) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: no permission to modify static predicate %s:%s/%u, %s:%d\n", m->name, C_STR(p->m, c_name), arity, get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error = true;
					return;
				}

				set_dynamic_in_db(m, C_STR(p, c_name), arity);
			} else {
				if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
					fprintf(stdout, "Warning: unknown directive: %s/%d\n", dirname, c->arity);
			}

			p1 += p1->nbr_cells;
		} else if (!strcmp(dirname, "create_prolog_flag")) {
			p1 += 1;
		} else if (!strcmp(dirname, "encoding")) {
			p1 += 1;
		} else if (!strcmp(dirname, "meta_predicate")) {
			set_meta_predicate_in_db(m, p1);
			p1 += p1->nbr_cells;
		} else if (!strcmp(C_STR(p, p1), ",") && (p1->arity == 2))
			p1 += 1;
		else {
			if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
				fprintf(stdout, "Warning: unknown directive: %s/%d\n", dirname, c->arity);

			break;
		}
	}
}

static void check_first_cut(clause *cl)
{
	cell *c = get_body(cl->cells);

	if (!c)
		return;

	if (c->val_off == g_cut_s) {
		cl->is_first_cut = true;
		cl->is_cut_only = true;
		return;
	}

	if (c->val_off == g_conjunction_s) {
		c += 1;

		if (c->val_off == g_cut_s) {
			cl->is_first_cut = true;
			return;
		}
	}
}

static pl_idx_t get_varno(parser *p, const char *src)
{
	int anon = !strcmp(src, "_");
	size_t offset = 0;
	unsigned i = 0;

	while (p->vartab.var_pool[offset]) {
		if (!strcmp(p->vartab.var_pool+offset, src) && !anon)
			return i;

		offset += strlen(p->vartab.var_pool+offset) + 1;
		i++;
	}

	size_t len = strlen(src);

	if ((offset+len+1) >= MAX_VAR_POOL_SIZE) {
		fprintf(stdout, "Error: var pool exhausted, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);
		p->error = true;
		return 0;
	}

	memcpy(p->vartab.var_pool+offset, src, len+1);
	return i;
}

void term_assign_vars(parser *p, unsigned start, bool rebase)
{
	if (!p || p->error)
		return;

	p->start_term = true;
	p->nbr_vars = 0;
	clause *cl = p->cl;

	if (!p->reuse) {
		memset(&p->vartab, 0, sizeof(p->vartab));
		cl->nbr_vars = 0;
	}

	cl->nbr_temporaries = 0;
	cl->is_first_cut = false;
	cl->is_cut_only = false;

	// Any var that only occurs in the head of
	// a clause we consider a temporary var...

	cell *body = get_body(cl->cells);
	bool in_body = false;

	for (pl_idx_t i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;

		if (body && (c == body))
			in_body = true;

		if (!is_var(c))
			continue;

		if (c->val_off == g_anon_s)
			c->flags |= FLAG_VAR_ANON;

		if (!in_body)
			c->flags |= FLAG_VAR_TEMPORARY;
		else
			c->flags &= ~FLAG_VAR_TEMPORARY;
	}

	// Don't assign temporaries yet...

	for (pl_idx_t i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;

		if (!is_var(c) || is_temporary(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "_V%u", c->var_nbr);
			c->var_nbr = get_varno(p, tmpbuf);
		} else
			c->var_nbr = get_varno(p, C_STR(p, c));

		c->var_nbr += start;

		if (c->var_nbr == MAX_VARS) {
			fprintf(stdout, "Error: max vars reached, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);
			p->error = true;
			return;
		}

		p->vartab.var_name[c->var_nbr] = C_STR(p, c);

		if (p->vartab.var_used[c->var_nbr]++ == 0) {
			cl->nbr_vars++;
			p->nbr_vars++;
		} else
			cl->is_complex = true;
	}

	// Do temporaries last...

	for (pl_idx_t i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;

		if (!is_var(c) || !is_temporary(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "_V%u", c->var_nbr);
			c->var_nbr = get_varno(p, tmpbuf);
		} else
			c->var_nbr = get_varno(p, C_STR(p, c));

		c->var_nbr += start;

		if (c->var_nbr == MAX_VARS) {
			fprintf(stdout, "Error: max vars reached, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);
			p->error = true;
			return;
		}

		p->vartab.var_name[c->var_nbr] = C_STR(p, c);

		if (p->vartab.var_used[c->var_nbr]++ == 0) {
			cl->nbr_temporaries++;
			cl->nbr_vars++;
			p->nbr_vars++;
		}
	}

	for (pl_idx_t i = 0; i < cl->nbr_vars; i++) {
		if (p->consulting && !p->do_read_term && (p->vartab.var_used[i] == 1) &&
			(p->vartab.var_name[i][strlen(p->vartab.var_name[i])-1] != '_') &&
			(*p->vartab.var_name[i] != '_')) {
			if (!p->m->pl->quiet)
				fprintf(stdout, "Warning: singleton: %s, near %s:%d\n", p->vartab.var_name[i], get_loaded(p->m, p->m->filename), p->line_nbr);
		}
	}

	cell *c = make_a_cell(p);
	ensure(c);
	c->tag = TAG_END;
	c->nbr_cells = 1;
}

static bool reduce(parser *p, pl_idx_t start_idx, bool last_op)
{
	pl_idx_t lowest = IDX_MAX, work_idx, end_idx = p->cl->cidx - 1;
	bool do_work = false, bind_le = false;

	for (pl_idx_t i = start_idx; i < p->cl->cidx;) {
		cell *c = p->cl->cells + i;

		if ((c->nbr_cells > 1) || !is_interned(c) || !c->priority) {
			i += c->nbr_cells;
			continue;
		}

#if 0
		if (!p->consulting)
			printf("*** OP1 start=%u '%s' type=%u, specifier=%u, pri=%u, last_op=%d, is_op=%d\n", start_idx, C_STR(p, c), c->tag, GET_OP(c), c->priority, last_op, IS_OP(c));
#endif

		if ((i == start_idx) && (i == end_idx)) {
			c->priority = 0;
			i++;
			continue;
		}

		if (bind_le ? c->priority <= lowest : c->priority < lowest) {
			lowest = c->priority;
			work_idx = i;
			do_work = true;
		}

		bind_le = is_xfy(c) || is_fy(c) ? true : false;
		i++;
	}

	if (!do_work)
		return false;

	pl_idx_t last_idx = (unsigned)-1;

	for (pl_idx_t i = start_idx; i <= end_idx;) {
		cell *c = p->cl->cells + i;

		if ((c->nbr_cells > 1) || !is_interned(c) || !c->priority) {
			last_idx = i;
			i += c->nbr_cells;
			continue;
		}

		if ((c->priority != lowest) || (i != work_idx)) {
			last_idx = i;
			i += c->nbr_cells;
			continue;
		}

#if 0
		if (!p->consulting)
			printf("*** OP2 last=%u/start=%u '%s' type=%u, specifier=%u, pri=%u, last_op=%d, is_op=%d\n", last_idx, start_idx, C_STR(p, c), c->tag, GET_OP(c), c->priority, last_op, IS_OP(c));
#endif

		c->tag = TAG_INTERNED;
		c->arity = 1;

		// Prefix...

		if (is_fx(c)) {
			cell *rhs = c + 1;

			if (is_fx(rhs) && !rhs->arity && (rhs->priority == c->priority) && !is_quoted(rhs)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}

			rhs += rhs->nbr_cells;

			if ((((pl_idx_t)(rhs - p->cl->cells)) < end_idx)
				&& is_xf(rhs) && (rhs->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		if (is_prefix(c)) {
			cell *rhs = c + 1;

			if (is_infix(rhs) && !rhs->arity && (rhs->priority > c->priority) && !is_quoted(rhs)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}

			if (is_prefix(rhs) && !rhs->arity && (rhs->priority > c->priority) && !is_quoted(rhs)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		if (is_prefix(c)) {
			cell *rhs = c + 1;
			c->nbr_cells += rhs->nbr_cells;
			pl_idx_t off = (pl_idx_t)(rhs - p->cl->cells);

			if (off > end_idx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing operand, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "operand_missing";
				p->error = true;
				return false;
			}

			break;
		}

		// Postfix...

		cell *rhs = c + 1;
		cell save = *c;

		if (is_xf(rhs) && (rhs->priority == c->priority) && !is_quoted(rhs)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		if (is_prefix(rhs) && !rhs->arity && (rhs->priority > c->priority)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		if (is_postfix(c)) {
			cell *lhs = p->cl->cells + last_idx;
			save.nbr_cells += lhs->nbr_cells;
			pl_idx_t cells_to_move = lhs->nbr_cells;
			lhs = c - 1;

			while (cells_to_move--)
				*c-- = *lhs--;

			*c = save;
			break;
		}

		// Infix...

		if (is_infix(rhs) && !rhs->arity && !is_quoted(rhs)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		pl_idx_t off = (pl_idx_t)(rhs - p->cl->cells);
		bool nolhs = (last_idx == (unsigned)-1);
		if (i == start_idx) nolhs = true;

		if (nolhs || (off > end_idx)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, missing operand, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "operand_missing";
			p->error = true;
			return false;
		}

		cell *lhs = p->cl->cells + last_idx;

		if (is_infix(lhs) && !lhs->arity && !is_quoted(lhs)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		save.nbr_cells += lhs->nbr_cells;
		pl_idx_t cells_to_move = lhs->nbr_cells;
		lhs = c - 1;

		while (cells_to_move--)
			*c-- = *lhs--;

		*c = save;
		c->nbr_cells += rhs->nbr_cells;
		c->arity = 2;

		if (is_xfx(c)) {
			cell *next = c + c->nbr_cells;
			i = next - p->cl->cells;

			if ((i <= end_idx)
				&& (is_xfx(next))
				&& (next->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		break;
	}

	return true;
}

static bool analyze(parser *p, pl_idx_t start_idx, bool last_op)
{
	while (reduce(p, start_idx, last_op))
		;

	return !p->error;
}

void reset(parser *p)
{
	clear_rule(p->cl);
	p->cl->cidx = 0;
	p->start_term = true;
	p->comment = false;
	p->error = false;
	p->last_close = false;
	p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
	p->error_desc = NULL;
}

static bool autoload_dcg_library(parser *p)
{
	if (p->m->pl->dcgs || find_module(p->m->pl, "dcgs"))
		return true;

	for (library *lib = g_libs; lib->name; lib++) {
		if (strcmp(lib->name, "dcgs"))
			continue;

		char *src = malloc(*lib->len+1);
		check_error(src);
		memcpy(src, lib->start, *lib->len);
		src[*lib->len] = '\0';
		SB(s);
		SB_sprintf(s, "library/%s", lib->name);
		module *tmp_m = load_text(p->m, src, SB_cstr(s));
		SB_free(s);

		if (tmp_m) {
			p->m->used[p->m->idx_used++] = tmp_m;
			p->m->pl->dcgs = tmp_m;
		}

		free(src);
		break;
	}

	return true;
}

static bool dcg_expansion(parser *p)
{
	if (!autoload_dcg_library(p))
		return false;

	query *q = query_create(p->m, false);
	check_error(q);

	cell *tmp = alloc_on_heap(q, 1+p->cl->cells->nbr_cells+1+1);
	make_struct(tmp, index_from_pool(p->pl, "dcg_translate"), NULL, 2, p->cl->cells->nbr_cells+1);
	safe_copy_cells(tmp+1, p->cl->cells, p->cl->cells->nbr_cells);
	make_var(tmp+1+p->cl->cells->nbr_cells, g_anon_s, p->cl->nbr_vars);
	make_end(tmp+1+p->cl->cells->nbr_cells+1);
	execute(q, tmp, p->cl->nbr_vars+1);

	cell *arg1 = tmp + 1;
	cell *arg2 = arg1 + arg1->nbr_cells;
	cell *c = deref(q, arg2, 0);
	char *src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);
	strcat(src, ".");
	query_destroy(q);

	if (!src) {
		p->error = true;
		return false;
	}

	parser *p2 = parser_create(p->m);
	check_error(p2);
	p2->srcptr = src;
	tokenize(p2, false, false);
	//xref_rule(p2->m, p2->cl, NULL);
	free(src);

	clear_rule(p->cl);
	free(p->cl);
	p->cl = p2->cl;					// Take the completed clause
	p->nbr_vars = p2->nbr_vars;
	p2->cl = NULL;
	parser_destroy(p2);
	return true;
}

static bool term_expansion(parser *p)
{
	if (p->error || p->internal || !is_interned(p->cl->cells))
		return false;

	if (p->cl->cells->val_off == g_dcg_s)
		return dcg_expansion(p);

	predicate *pr = find_functor(p->m, "term_expansion", 2);

	if (!pr || !pr->cnt)
		return false;

	cell *h = get_head(p->cl->cells);
	const char *pred = C_STR(p, h);

	if (h->val_off == g_term_expansion_s)
		return false;

	query *q = query_create(p->m, false);
	check_error(q);
	char *dst = print_canonical_to_strbuf(q, p->cl->cells, 0, 0);
	SB(s);
	SB_sprintf(s, "term_expansion((%s),_TermOut), !.", dst);
	free(dst);

	parser *p2 = parser_create(p->m);
	check_error(p2, query_destroy(q));
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = SB_cstr(s);
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	execute(q, p2->cl->cells, p2->cl->nbr_vars);
	SB_free(s);

	if (q->retry != QUERY_OK) {
		parser_destroy(p2);
		query_destroy(q);
		return false;
	}

	frame *f = GET_FIRST_FRAME();
	char *src = NULL;

	for (unsigned i = 0; i < p2->cl->nbr_vars; i++) {
		if (strcmp(p2->vartab.var_name[i], "_TermOut"))
			continue;

		slot *e = GET_SLOT(f, i);

		if (is_empty(&e->c))
			continue;

		cell *c = deref(q, &e->c, e->c.var_ctx);
		src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);
		strcat(src, ".");
		break;
	}

	if (!src) {
		parser_destroy(p2);
		query_destroy(q);
		p->error = true;
		return false;
	}

	reset(p2);
	p2->srcptr = src;
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	free(src);

	clear_rule(p->cl);
	free(p->cl);
	p->cl = p2->cl;					// Take the completed clause
	p->nbr_vars = p2->nbr_vars;
	p2->cl = NULL;

	parser_destroy(p2);
	query_destroy(q);

	return term_expansion(p);
}

static cell *goal_expansion(parser *p, cell *goal)
{
	if (p->error || p->internal || !is_interned(goal))
		return goal;

	if ((goal->val_off == g_goal_expansion_s) || (goal->val_off == g_cut_s))
		return goal;

	predicate *pr = find_functor(p->m, "goal_expansion", 2);

	if (!pr || !pr->cnt)
		return goal;

	if (get_builtin_term(p->m, goal, NULL, NULL) /*|| is_op(goal)*/)
		return goal;

	//if (search_predicate(p->m, goal))
	//	return goal;

	query *q = query_create(p->m, false);
	check_error(q);
	q->varnames = true;
	char *dst = print_canonical_to_strbuf(q, goal, 0, 0);
	q->varnames = false;
	SB(s);
	SB_sprintf(s, "goal_expansion((%s),_TermOut), !.", dst);
	free(dst);

	//DUMP_TERM("old", p->cl->cells, 0, 0);

	// Note: since only parsing goals we need to preserve
	// the varnames so they get reused. Only genuinely new
	// variables should create anew. Hence we pull the
	// vartab from the main parser...

	parser *p2 = parser_create(p->m);
	check_error(p2, query_destroy(q));
	q->p = p2;
	p2->cl->nbr_vars = p->cl->nbr_vars;
	p2->vartab = p->vartab;
	p2->reuse = true;
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = SB_cstr(s);
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	execute(q, p2->cl->cells, p2->cl->nbr_vars);
	SB_free(s);

	if (q->retry != QUERY_OK) {
		parser_destroy(p2);
		query_destroy(q);
		return goal;
	}

	for (unsigned i = 0; i < p->cl->nbr_vars; i++)
		q->ignores[i] = true;

	p->cl->nbr_vars = p2->cl->nbr_vars;
	frame *f = GET_FIRST_FRAME();
	char *src = NULL;

	for (unsigned i = 0; i < p2->cl->nbr_vars; i++) {
		if (strcmp(p2->vartab.var_name[i], "_TermOut"))
			continue;

		slot *e = GET_SLOT(f, i);

		if (is_empty(&e->c))
			continue;

		cell *c = deref(q, &e->c, e->c.var_ctx);
		q->varnames = true;
		src = print_term_to_strbuf(q, c, q->latest_ctx, 1);
		q->varnames = false;
		strcat(src, ".");
		break;
	}

	if (!src) {
		parser_destroy(p2);
		query_destroy(q);
		p->error = true;
		return goal;
	}

	reset(p2);
	p2->cl->nbr_vars = p->cl->nbr_vars;
	p2->vartab = p->vartab;
	p2->reuse = true;
	p2->srcptr = src;
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	free(src);

	// Push the updated vatab back...

	p->cl->nbr_vars = p2->cl->nbr_vars;
	p->vartab = p2->vartab;

	// snip the old goal...

	const unsigned goal_idx = goal - p->cl->cells;
	unsigned trailing = p->cl->cidx - (goal_idx + goal->nbr_cells);
	p->cl->cidx -= goal->nbr_cells;
	memmove(goal, goal + goal->nbr_cells, sizeof(cell)*trailing);

	// make room for new goal...

	const unsigned new_cells = p2->cl->cidx-1;		// skip TAG_END
	trailing = p->cl->cidx - goal_idx;
	make_room(p, new_cells);
	goal = p->cl->cells + goal_idx;
	memmove(goal+new_cells, goal, sizeof(cell)*trailing);

	// paste the new goal...

	memcpy(goal, p2->cl->cells, sizeof(cell)*new_cells);
	p->cl->cidx += new_cells;
	clear_rule(p2->cl);
	free(p2->cl);
	p2->cl = NULL;

	//DUMP_TERM("new", p->cl->cells, 0, 0);

	// done

	parser_destroy(p2);
	query_destroy(q);

	return goal;
}

static cell *insert_call_here(parser *p, cell *c, cell *p1)
{
	pl_idx_t c_idx = c - p->cl->cells, p1_idx = p1 - p->cl->cells;
	make_room(p, 1);

	cell *last = p->cl->cells + (p->cl->cidx - 1);
	pl_idx_t cells_to_move = p->cl->cidx - p1_idx;
	cell *dst = last + 1;

	while (cells_to_move--)
		*dst-- = *last--;

	p1 = p->cl->cells + p1_idx;
	p1->tag = TAG_INTERNED;
	p1->flags = FLAG_BUILTIN;
	p1->fn_ptr = get_fn_ptr(fn_iso_call_1);
	p1->val_off = g_call_s;
	p1->nbr_cells = 2;
	p1->arity = 1;

	p->cl->cidx++;
	return p->cl->cells + c_idx;
}

static cell *term_to_body_conversion(parser *p, cell *c)
{
	pl_idx_t c_idx = c - p->cl->cells;

	if (is_xfx(c) || is_xfy(c)) {
		if ((c->val_off == g_conjunction_s)
			|| (c->val_off == g_disjunction_s)
			|| (c->val_off == g_if_then_s)
			|| (c->val_off == g_soft_cut_s)
			|| (c->val_off == g_neck_s)) {
			cell *save_c = c;
			cell *lhs = c + 1;
			bool norhs = false;

			//if (c->val_off == g_soft_cut_s)
			//	norhs = true;

			if (is_var(lhs)) {
				c = insert_call_here(p, c, lhs);
				lhs = c + 1;
			} else {
				if ((c->val_off != g_neck_s))
					lhs = goal_expansion(p, lhs);

				lhs = term_to_body_conversion(p, lhs);
			}

			cell *rhs = lhs + lhs->nbr_cells;
			c = p->cl->cells + c_idx;

			if (is_var(rhs) && !norhs)
				c = insert_call_here(p, c, rhs);
			else {
				rhs = goal_expansion(p, rhs);
				rhs = term_to_body_conversion(p, rhs);
			}

			c->nbr_cells = 1 + lhs->nbr_cells + rhs->nbr_cells;
		}
	} else if (is_prefix(c)) {
		if ((c->val_off == g_negation_s)
			|| (c->val_off == g_neck_s)) {
			cell *save_c = c;
			cell *rhs = c + 1;

			if (is_var(rhs)) {
				c = insert_call_here(p, c, rhs);
				rhs = c + 1;
			} else {
				rhs = goal_expansion(p, rhs);
				rhs = term_to_body_conversion(p, rhs);
			}

			c->nbr_cells = 1 + rhs->nbr_cells;
		}
	} else if (c->arity) {
		predicate *pr = find_predicate(p->m, c);

		if (pr && pr->is_meta_predicate) {
			cell *arg = c + 1;
			unsigned arity = c->arity;

			while (arity--) {
				c->nbr_cells -= arg->nbr_cells;
				arg = goal_expansion(p, arg);
				c->nbr_cells += arg->nbr_cells;
				arg += arg->nbr_cells;
			}
		}
	}

	return p->cl->cells + c_idx;
}

void term_to_body(parser *p)
{
	term_to_body_conversion(p, p->cl->cells);
	p->cl->cells->nbr_cells = p->cl->cidx - 1;	// Drops TAG_END
}

cell *check_body_callable(cell *c)
{
	if ((c->arity == 2) && (is_xfx(c) || is_xfy(c))) {
		if ((c->val_off == g_conjunction_s)
			|| (c->val_off == g_disjunction_s)
			|| (c->val_off == g_if_then_s)
			|| (c->val_off == g_soft_cut_s)
			|| (c->val_off == g_neck_s)) {
			cell *lhs = c + 1;
			cell *tmp;

			if ((tmp = check_body_callable(lhs)) != NULL)
				return tmp;

			cell *rhs = lhs + lhs->nbr_cells;

			if ((tmp = check_body_callable(rhs)) != NULL)
				return tmp;
		}

		return NULL;
	}

	return !is_callable(c) && !is_var(c) ? c : NULL;
}

bool virtual_term(parser *p, const char *src)
{
	parser *p2 = parser_create(p->m);
	check_error(p2);
	p2->consulting = true;
	p2->srcptr = (char*)src;
	tokenize(p2, false, false);
	parser_destroy(p2);
	return true;
}

cell *make_interned(parser *p, pl_idx_t offset)
{
	cell *c = make_a_cell(p);
	c->tag = TAG_INTERNED;
	c->nbr_cells = 1;
	c->val_off = offset;
	return c;
}

static int get_octal(const char **srcptr)
{
	const char *src = *srcptr;
	int v = 0;

	while (*src == '0')
		src++;

	while ((*src >= '0') && (*src <= '7')) {
		v *= 8;
		char ch = *src++;
		v += ch - '0';
	}

	*srcptr = src;
	return v;
}

static int get_hex(const char **srcptr, unsigned n, bool *error)
{
	const char *src = *srcptr;
	unsigned orig_n = n;
	int v = 0;

	while (*src == '0') {
		src++; n--;
	}

	while (((*src >= '0') && (*src <= '9')) ||
		((*src >= 'a') && (*src <= 'f')) ||
		((*src >= 'A') && (*src <= 'F'))) {
		v *= 16;
		char ch = *src++;
		n--;

		if ((ch >= 'a') && (ch <= 'f'))
			v += 10 + (ch - 'a');
		else if ((ch >= 'A') && (ch <= 'F'))
			v += 10 + (ch - 'A');
		else
			v += ch - '0';

		if (!n)
			break;
	}

	if (n && ((orig_n == 4) || (orig_n == 8))) {
		*error = true;
		return 0;
	}

	*srcptr = src;
	return v;
}

const char *g_escapes = "\e\a\f\b\t\v\r\n\x20\x7F\'\\\"`";
const char *g_anti_escapes = "eafbtvrnsd'\\\"`";

static int get_escape(parser *p, const char **_src, bool *error, bool number)
{
	const char *src = *_src;
	int ch = *src++;
	const char *ptr = strchr(g_anti_escapes, ch);

	if (ptr)
		ch = g_escapes[ptr-g_anti_escapes];
	else if ((isdigit(ch) || (ch == 'x')
		|| (ch == 'u') || (ch == 'U')
		)
		&& !number) {
		bool unicode = false;

		if (ch == 'x')
			ch = get_hex(&src, UINT_MAX, error);
		else if (ch == 'U') {
			ch = get_hex(&src, 8, error);
			unicode = true;
		} else if (ch == 'u') {
			ch = get_hex(&src, 4, error);
			unicode = true;
		} else {
			src--;
			ch = get_octal(&src);
		}

		if (!p->error && (*src != '\\') && unicode && p->flags.json)
			src--;
		else if (!unicode && (*src++ != '\\')) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: syntax error, closing \\ missing\n");
			*_src = src;
			*error = true;
			return 0;
		}

		if ((unsigned)ch > 0x10FFFF) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: syntax error, illegal character code\n");
			*_src = src;
			*error = true;
			return 0;
		}
	} else if ((((ch != '\\') && (ch != '"') && (ch != '\'') && (ch != '\r') && (ch != '\n'))
		|| number) && !isdigit(ch)) {
		*_src = --src;
		*error = true;
		return 0;
	}

	*_src = src;
	return ch;
}

#define isbdigit(ch) (((ch) >= '0') && ((ch) <= '1'))
#define isodigit(ch) (((ch) >= '0') && ((ch) <= '7'))

void read_integer(parser *p, mp_int v2, int base, const char **srcptr)
{
	const char *src = *srcptr;
	int spaces = 0;

	while (*src) {
		if ((base == 2) && ((*src < '0') || (*src > '1')))
			break;

		if ((base == 8) && ((*src < '0') || (*src > '7')))
			break;

		if ((base == 10) && ((*src < '0') || (*src > '9')))
			break;

		if ((base == 16) && !isxdigit(*src))
			break;

		if (spaces > 1) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, illegal character, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			*srcptr = src;
			p->error = true;
			return;
		}

		spaces = 0;
		SB_putchar(p->token, *src);
		src++;

		int last_ch = *src;

		while (*src == '_') {
			spaces++;
			src++;
		}

		if (last_ch == '_') {
			p->srcptr = (char*)src;
			src = eat_space(p);
		}
	}

	if ((base != 16) && !isdigit(src[-1]))
		src--;
	else if ((base == 16) && !isxdigit(src[-1]))
		src--;

	mp_int_read_cstring(v2, base, (char*)SB_cstr(p->token), NULL);
	SB_free(p->token);
	*srcptr = src;
}

static bool parse_number(parser *p, const char **srcptr, bool neg)
{
	set_smallint(&p->v, 0);
	p->v.flags = 0;
	const char *s = *srcptr;

	LOOP:

	if ((*s == '.') && isdigit(s[1])) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, parsing number1, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	if (!isdigit(*s))
		return false;

	if ((*s == '0') && (s[1] == '\'') && !((s[2] == '\\') && (s[3] == '\n'))
		&& (!search_op(p->m, "", NULL, false) || ((s[2] == '\'') && (s[3] == '\'')))) {
		s += 2;
		int v;

		if (*s == '\\') {
			s++;

			if ((*s == '+') || (*s == '-')) {
				if (*s == '-')
					neg = true;

				s++;

				if (*s != '\'') {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, parsing number2, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error_desc = "number";
					p->error = true;
					return false;
				}

				s++;
				goto LOOP;
			}

			if (!*s || iscntrl(*s)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, parsing number3, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "number";
				p->error = true;
				return false;
			}

			v = get_escape(p, &s, &p->error, true);
		} else if ((*s == '\'') && s[1] == '\'') {
			s++;
			v = *s++;
#if 1
		} else if ((*s == '\'') && !p->flags.not_strict_iso && search_op(p->m, "", NULL, false)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing number4, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "number";
			p->error = true;
			return false;
#endif
		} else
			v = get_char_utf8(&s);

		if (p->error) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing number5, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "number";
			p->error = true;
			return false;
		}

		p->v.tag = TAG_INTEGER;
		set_smallint(&p->v, v);
		if (neg) set_smallint(&p->v, -get_smallint(&p->v));
		*srcptr = s;
		return true;
	}

	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;
	char *tmpptr = (char*)s;

	if ((*s == '0') && (s[1] == 'b')) {
		s += 2;

		read_integer(p, &v2, 2, &s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INTEGER;
		p->v.flags |= FLAG_INT_BINARY;
		*srcptr = s;
		return true;
	}

	if ((*s == '0') && (s[1] == 'o')) {
		s += 2;

		read_integer(p, &v2, 8, &s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INTEGER;
		p->v.flags |= FLAG_INT_OCTAL;
		*srcptr = s;
		return true;
	}

	if ((*s == '0') && (s[1] == 'x')) {
		s += 2;

		read_integer(p, &v2, 16, &s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INTEGER;
		p->v.flags |= FLAG_INT_HEX;
		*srcptr = s;
		return true;
	}

	read_integer(p, &v2, 10, &s);

	if (p->flags.json && s && ((*s == 'e') || (*s == 'E')) && isdigit(s[1])) {
		p->v.tag = TAG_DOUBLE;
		errno = 0;
		pl_flt_t v = strtod(tmpptr, &tmpptr);

		if ((int)v && (errno == ERANGE)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, float overflow %g, %s:%d\n", v, get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "float_overflow";
			p->error = true;
			return false;
		}

		set_float(&p->v, neg?-v:v);
		*srcptr = tmpptr;
		mp_int_clear(&v2);
		return true;
	}

	if (s && (*s == '.') && isdigit(s[1])) {
		p->v.tag = TAG_DOUBLE;
		errno = 0;
		pl_flt_t v = strtod(tmpptr, &tmpptr);

		if ((int)v && (errno == ERANGE)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, float overflow %g, %s:%d\n", v, get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "float_overflow";
			p->error = true;
			return false;
		}

		set_float(&p->v, neg?-v:v);
		*srcptr = tmpptr;
		mp_int_clear(&v2);
		return true;
	}

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		p->v.val_bigint = malloc(sizeof(bigint));
		p->v.val_bigint->refcnt = 1;
		mp_int_init_copy(&p->v.val_bigint->ival, &v2);
		if (neg) p->v.val_bigint->ival.sign = MP_NEG;
		p->v.flags |= FLAG_MANAGED;
	} else {
		set_smallint(&p->v, val);
		if (neg) p->v.val_int = -p->v.val_int;
	}

	mp_int_clear(&v2);
	int ch;
	p->v.tag = TAG_INTEGER;

	if ((s[-1] == '.') || isspace(s[-1]))
		s--;

	*srcptr = s;
	ch = peek_char_utf8(s);

	if (ch == '(') {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	return true;
}

inline static bool is_matching_pair(int ch, int next_ch, int lh, int rh)
{
	return (ch == lh) && (next_ch == rh);
}

static bool valid_float(const char *src)
{
	if (*src == '-')
		src++;

	while (isdigit(*src))
		src++;

	if (*src != '.')
		return false;

	src++;

	if (!isdigit(*src))
		return false;

	return true;
}

char *eat_space(parser *p)
{
	p->did_getline = false;
	const char *src = p->srcptr;
	bool done;

	do {
		done = true;
		int ch = peek_char_utf8(src);

		while (iswspace(ch)) {
			if (ch == '\n')
				p->line_nbr++;

			get_char_utf8(&src);
			ch = peek_char_utf8(src);
		}

		if ((*src == '%') && !p->fp) {
			while (*src && (*src != '\n'))
				src++;

			if (*src == '\n')
				p->line_nbr++;

			src++;
			done = false;
			continue;
		}

		if ((!*src || (*src == '%')) && p->fp) {
			while (*src && (*src != '\n'))
				src++;

			if (*src == '\n')
				p->line_nbr++;

			if (*src) {
				src++;
				done = false;
				continue;
			}

			if (p->no_fp || getline(&p->save_line, &p->n_line, p->fp) == -1)
				return p->srcptr = "";

			p->did_getline = true;
			src = p->srcptr = p->save_line;
			done = false;
			continue;
		}

		do {
			if (!p->comment && (src[0] == '/') && (src[1] == '*')) {
				p->comment = true;
				src += 2;
				continue;
			}

			if (p->comment && (src[0] == '*') && (src[1] == '/')) {
				p->comment = false;
				src += 2;

				if (!is_number(&p->v))	// For number_chars
					p->srcptr = (char*)src;

				done = false;
				continue;
			}

			if (*src == '\n')
				p->line_nbr++;

			if (p->comment)
				src++;

			if (!*src && p->comment && p->fp) {
				if (p->no_fp || getline(&p->save_line, &p->n_line, p->fp) == -1) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, parsing number1, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error = true;
					return NULL;
				}

				p->did_getline = true;
				src = p->srcptr = p->save_line;
			}
		}
		 while (*src && p->comment);
	}
	 while (!done);

	return (char*)src;
}

static bool check_space_before_function(parser *p, int ch, const char *src)
{
	if (iswspace(ch) && SB_strcmp(p->token, ".")) {
		p->srcptr = (char*)src;
		src = eat_space(p);

		if (!src || !*src) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, incomplete statement, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "incomplete_statement";
			p->error = true;
			return false;
		}

		if (!p->is_op && (*src == '(')) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, operator expected before parens, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "operator_expected";
			p->error = true;
			return false;
		}
	}

	return true;
}

static bool contains_null(const char *src, size_t len)
{
	for (size_t i = 0; i < len; i++) {
		if (!*src++)
			return true;
	}

	return false;
}

bool get_token(parser *p, bool last_op, bool was_postfix)
{
	if (p->error || !p->srcptr || !*p->srcptr)
		return false;

	const char *src = p->srcptr;

	SB_init(p->token);
	bool neg = false;
	p->v.tag = TAG_INTERNED;
	p->v.flags = 0;
	p->v.nbr_cells = 1;
	p->quote_char = 0;
	p->was_string = p->string = p->is_quoted = p->is_var = p->is_op = p->symbol = false;

	if (p->dq_consing && (*src == '"') && (src[1] == '"')) {
		src++;
	} else if (p->dq_consing && (*src == '"')) {
		SB_strcat(p->token, "]");
		p->srcptr = (char*)++src;
		p->dq_consing = 0;
		return true;
	}

	if (p->dq_consing < 0) {
		SB_strcat(p->token, ",");
		p->dq_consing = 1;
		return true;
	}

	if (p->dq_consing) {
		int ch = get_char_utf8(&src);

		if ((ch == '\\') && p->flags.character_escapes) {
			ch = get_escape(p, &src, &p->error, false);

			if (p->error) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, %s:%d\n", p->srcptr, get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "illegal_character_escape";
				p->error = true;
				return false;
			}
		}

		SB_sprintf(p->token, "%u", ch);
		p->srcptr = (char*)src;
		set_smallint(&p->v, ch);
		p->v.tag = TAG_INTEGER;
		p->dq_consing = -1;
		return true;
	}

	src = eat_space(p);

	if (!src || !*src) {
		p->srcptr = (char*)src;
		return false;
	}

	// -ve numbers (note there are no explicitly +ve numbers)

	bool is_neg = false;
	const char *save_src = src;

	if ((*src == '-') && last_op && !was_postfix) {
		is_neg = true;
		src += 1;
	}

	if ((*src == '\'') && (src[1] == '-') && (src[2] == '\'') && last_op) {
		is_neg = true;
		src += 3;
	}

	if (is_neg) {
		p->srcptr = (char*)src;
		int next_ch = peek_char_utf8(src);

		if (next_ch == '/') {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, near %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "incomplete_statement";
			p->error = true;
			return false;
		}

		src = eat_space(p);

		if (!src || !*src) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, incomplete statement, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "incomplete_statement";
			p->error = true;
			return false;
		}

		if (isdigit(*src))
			neg = true;
		else
			src = save_src;
	}

	// Numbers...

	const char *tmpptr = src;

	if ((*src != '-') && parse_number(p, &src, neg)) {
		SB_strcatn(p->token, tmpptr, src-tmpptr);
		const char *dst = SB_cstr(p->token);

		if ((dst[0] != '0') && (dst[1] != 'x')) {
			if ((strchr(dst, '.') || strchr(dst, 'e') || strchr(dst, 'E')) && !strchr(dst, '\'')) {
				if (!valid_float(SB_cstr(p->token))) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, float, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error_desc = "float";
					p->error = true;
					return false;
				}
			}
		}

		p->srcptr = (char*)src;
		int ch = peek_char_utf8(src);

		if (!check_space_before_function(p, ch, src))
			return false;

		src = p->srcptr;
		ch = peek_char_utf8(src);

		if (ch == '(') {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "number";
			p->error = true;
			return false;
		}

		return true;
	}

	// Quoted...

	if ((*src == '"') || (*src == '`') || (*src == '\'')) {
		p->quote_char = *src++;
		p->is_quoted = true;

		if ((p->quote_char == '"') && p->flags.double_quote_codes) {
			SB_strcpy(p->token, "[");

			if ((*src == '"') && (src[1] != '"')) {
				SB_strcat(p->token, "]");
				p->srcptr = (char*)++src;
				return true;
			}

			p->dq_consing = 1;
			p->quote_char = 0;
			p->srcptr = (char*)src;
			return true;
		} else if ((p->quote_char == '"') && p->flags.double_quote_chars)
			p->string = true;

		if (p->string && !p->flags.json && (*src == p->quote_char) && (*src == '"')) {
			SB_strcpy(p->token, "[]");
			src++;
			p->was_string = true;
			p->string = false;
			p->srcptr = (char*)src;
			int ch = peek_char_utf8(src);

			if (!check_space_before_function(p, ch, src))
				return false;

			src = p->srcptr;
			return true;
		}

		for (;;) {
			for (int ch; (ch = get_char_utf8(&src));) {
				if (ch == '\n') {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, unterminated quoted atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error_desc = "unterminated_quoted_atom";
					p->error = true;
					p->srcptr = (char*)src;
					return false;
				}

				if ((ch == p->quote_char) && (*src == ch)) {
					ch = *src++;
				} else if (ch == p->quote_char) {
					p->quote_char = 0;
					break;
				}

				if (ch < ' ') {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, invalid quoted character, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error_desc = "invalid_quoted_character";
					p->error = true;
					p->srcptr = (char*)src;
					return false;
				}

				if ((ch == '\\') && p->flags.character_escapes) {
					int ch2 = *src;
					ch = get_escape(p, &src, &p->error, false);

					if (!p->error) {
						if (ch2 == '\n') {
							//p->line_nbr++;
							continue;
						}
					} else {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, %s:%d\n", p->srcptr, get_loaded(p->m, p->m->filename), p->line_nbr);

						p->error_desc = "illegal_character_escape";
						p->error = true;
						p->srcptr = (char*)src;
						return false;
					}
				}

				SB_putchar(p->token, ch);
			}

			if (p->quote_char && p->fp) {
				if (p->no_fp || getline(&p->save_line, &p->n_line, p->fp) == -1) {
					p->srcptr = "";

					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, unterminated quoted atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error_desc = "unterminated_quoted_atom";
					p->error = true;
					return false;
				}

				src = p->srcptr = p->save_line;
				continue;
			}

			if (!p->string
				&& SB_strcmp(p->token, "[")
				&& SB_strcmp(p->token, "(")
				&& SB_strcmp(p->token, "{")
				&& SB_strcmp(p->token, "]")
				&& SB_strcmp(p->token, ")")
				&& SB_strcmp(p->token, "}"))
			{
				if (SB_strlen(p->token) && contains_null(SB_cstr(p->token), SB_strlen(p->token))) {
					p->quote_char = -1;
				} else if (search_op(p->m, SB_cstr(p->token), NULL, false)) {
					p->is_op = true;

					if (!SB_strcmp(p->token, ","))
						p->quote_char = -1;
				} else
					p->quote_char = -1;
			} else
				p->quote_char = -1;

			p->srcptr = (char*)src;
			return true;
		}
	}

	// Atoms (including variables)...

	int ch = peek_char_utf8(src);

	if (iswalpha(ch)
#ifdef __APPLE__
		|| iswideogram(ch)
#endif
		|| (ch == '_')) {
		while (iswalnum(ch)
#ifdef __APPLE__
			|| iswideogram(ch)
#endif
			|| (ch == '_')) {
			get_char_utf8(&src);
			SB_putchar(p->token, ch);
			ch = peek_char_utf8(src);
		}

		int ch_start = peek_char_utf8(SB_cstr(p->token));

		if ((!p->flags.var_prefix && !p->flags.json && iswupper(ch_start)) || (ch_start == '_'))
			p->is_var = true;
		else if (search_op(p->m, SB_cstr(p->token), NULL, false))
			p->is_op = true;

		p->srcptr = (char*)src;
		int ch = peek_char_utf8(src);

		if (!check_space_before_function(p, ch, src))
			return false;

		src = p->srcptr;
		return true;
	}

	ch = get_char_utf8(&src);
	int next_ch = peek_char_utf8(src);

	if ((ch == '.') && iswspace(next_ch)) {
		SB_putchar(p->token, ch);
		p->is_op = search_op(p->m, SB_cstr(p->token), NULL, false);
		p->srcptr = (char*)src;
		return true;
	}

	p->srcptr = (char*)src;

	if ((ch == '(') || (ch == '[') || (ch == '{') || (ch == ',')
		|| (ch == '}') || (ch == ']') || (ch == ')')) {
		src = eat_space(p);

		if (!src || !*src) {
			SB_putchar(p->token, ch);
			p->is_op = search_op(p->m, SB_cstr(p->token), NULL, false);
			p->srcptr = (char*)src;
			return true;
		}

		next_ch = peek_char_utf8(src);

		if (is_matching_pair(ch, next_ch, '[',']')) {
			SB_strcpy(p->token, "[]");
			get_char_utf8(&src);
			p->srcptr = (char*)src;
			int ch = peek_char_utf8(src);

			if (!check_space_before_function(p, ch, src))
				return false;

			src = p->srcptr;
			return true;
		}

		if (is_matching_pair(ch, next_ch, '{','}')) {
			SB_strcpy(p->token, "{}");
			get_char_utf8(&src);
			p->srcptr = (char*)src;
			return true;
		}
	} else
		next_ch = peek_char_utf8(src);

	// Symbols...

	if (is_matching_pair(ch, next_ch, ')','(') ||
		is_matching_pair(ch, next_ch, ']','(') ||
		is_matching_pair(ch, next_ch, '}','(') ||
		is_matching_pair(ch, next_ch, '}','(')) {
		if (DUMP_ERRS || !p->do_read_term)
			fprintf(stdout, "Error: syntax error, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

		p->error_desc = "operator_expected";
		p->error = true;
		p->srcptr = (char*)src;
		return false;
	}

	p->symbol = true;

	do {
		SB_putchar(p->token, ch);

		if (((ch < 256) && strchr(g_solo, ch)) || iswspace(ch))
			break;

		int ch_next = peek_char_utf8(src);

		if (ch_next == '%')
			break;

		if ((ch == '.') && iswspace(ch_next))
			break;

		if (p->flags.json && (ch_next == '-'))
			break;

		ch = ch_next;

		if (((ch < 256) && strchr(g_solo, ch)) || iswspace(ch) || iswalnum(ch) || (ch == '_'))
			break;

		ch = get_char_utf8(&src);
	}
	 while (ch);

	p->is_op = search_op(p->m, SB_cstr(p->token), NULL, false);
	p->srcptr = (char*)src;

	ch = peek_char_utf8(src);

	if (SB_strcmp(p->token, "(") && !check_space_before_function(p, ch, src))
		return false;

	return true;
}

static bool process_term(parser *p, cell *p1)
{
	if (conditionals(p, p1))
		return true;

	if (p->m->ifs_blocked[p->m->if_depth])
		return true;

	directives(p, p1);

	cell *h = get_head(p1);

	if (is_var(h)) {
		if (DUMP_ERRS || !p->do_read_term)
			printf("Error: instantiation error, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

		p->error_desc = "instantiation_error";
		p->error = true;
		return false;
	} else if (is_number(h)) {
		if (DUMP_ERRS || !p->do_read_term)
			printf("Error: type error, callable, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

		p->error_desc = "type_error";
		p->error = true;
		return false;
	}

	if (is_cstring(h)) {
		pl_idx_t off = index_from_pool(p->m->pl, C_STR(p, h));
		if (off == ERR_IDX) {
			p->error = true;
			return false;
		}

		unshare_cell(h);
		h->tag = TAG_INTERNED;
		h->val_off = off;
		h->flags = 0;
		h->arity = 0;
	}

	db_entry *dbe;

	if ((dbe = assertz_to_db(p->m, p->cl->nbr_vars, p->cl->nbr_temporaries, p1, 1)) == NULL) {
		if (DUMP_ERRS || !p->do_read_term)
			printf("Error: assertion failed '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

		p->error = true;
		return false;
	}

	check_first_cut(&dbe->cl);
	dbe->cl.is_fact = !get_logical_body(dbe->cl.cells);
	return true;
}

unsigned tokenize(parser *p, bool args, bool consing)
{
	pl_idx_t arg_idx = p->cl->cidx, save_idx = 0;
	bool last_op = true, is_func = false, last_num = false;
	bool last_bar = false, last_quoted = false, last_postfix = false;
	unsigned arity = 1;
	p->depth++;

	while (get_token(p, last_op, last_postfix)) {
		if (p->error && !p->do_read_term)
			break;

#if 0
		int ch = peek_char_utf8(SB_cstr(p->token));
		fprintf(stderr,
			"Debug: '%s' (%d) line_nbr=%d, symbol=%d, quoted=%d, tag=%u, op=%d, lastop=%d, string=%d\n",
			SB_cstr(p->token), ch, p->line_nbr, p->symbol, p->quote_char, p->v.tag, p->is_op, last_op, p->string);
#endif

		if (!p->quote_char
			&& !SB_strcmp(p->token, ".")
			&& !iswalpha(*p->srcptr)
		    && (*p->srcptr != '"')
		    && (*p->srcptr != '(')
		    && (*p->srcptr != ',')
		    && (*p->srcptr != ';')
		    && (*p->srcptr != ')')
		    && (*p->srcptr != ']')
		    && (*p->srcptr != '}')
		    && (*p->srcptr != '|')) {

			if (p->nesting_parens || p->nesting_brackets || p->nesting_braces) {
				if (DUMP_ERRS || !p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "mismatched_parens_or_brackets_or_braces";
				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			if (!p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, incomplete statement, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "incomplete_statement";
				p->error = true;
				return false;
			}

			if (analyze(p, 0, last_op)) {
				if (p->cl->cells->nbr_cells <= (p->cl->cidx-1)) {
					if (DUMP_ERRS || !p->do_read_term)
						printf("Error: syntax error, operator expected unfinished input '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error_desc = "operator_expected";
					p->error = true;
					return 0;
				}

				term_assign_vars(p, p->read_term_slots, false);

				if (p->consulting && check_body_callable(p->cl->cells)) {
					if (DUMP_ERRS || !p->do_read_term)
						printf("Error: type error, not callable, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error_desc = "operator_expected";
					p->error = true;
					return 0;
				}

				xref_rule(p->m, p->cl, NULL);
				term_to_body(p);

				if (p->consulting && !p->skip) {
					if (is_var(p->cl->cells)) {
						if (DUMP_ERRS || !p->do_read_term)
							printf("Error: instantiation error, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

						p->error_desc = "instantiation_error";
						p->error = true;
						return 0;
					} else if (is_number(p->cl->cells)) {
						if (DUMP_ERRS || !p->do_read_term)
							printf("Error: type error, callable, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

						p->error_desc = "type_error";
						p->error = true;
						return 0;
					}

					term_expansion(p);
					cell *p1 = p->cl->cells;

					if (!p1->arity && !strcmp(C_STR(p, p1), "begin_of_file")) {
						p->end_of_term = true;
						last_op = true;
						last_num = false;
						p->cl->cidx = 0;
						continue;
					}

					if (!p1->arity && !strcmp(C_STR(p, p1), "end_of_file")) {
						p->end_of_term = true;
						return 0;
					}
				}

				if (p->consulting && !p->skip) {
					// Term expansion can return a list...

					cell *p1 = p->cl->cells;
					LIST_HANDLER(p1);
					bool tail = false;

					while (is_list(p1) && !g_tpl_interrupt) {
						cell *h = LIST_HEAD(p1);

						if (!process_term(p, h))
							return false;

						if (p->already_loaded)
							return false;

						p1 = LIST_TAIL(p1);

						if (is_nil(p1))
							tail = true;
					}

					if (!tail && !process_term(p, p1))
						return false;

					if (p->already_loaded)
						return false;

					p->cl->cidx = 0;
				}
			}

			p->end_of_term = true;
			last_op = true;

			if (p->one_shot)
				break;

			last_num = false;
			continue;
		}

		p->end_of_term = false;

		if (!p->quote_char && !last_op &&
			(!SB_strcmp(p->token, "[") || !SB_strcmp(p->token, "{"))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, needs operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "needs_operator";
			p->error = true;
			break;
		}

		if (!p->quote_char && p->last_close && !SB_strcmp(p->token, "(")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, needs operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "needs_operator";
			p->error = true;
			break;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "[")) {
			save_idx = p->cl->cidx;
			cell *c = make_interned(p, g_dot_s);
			c->arity = 2;
			p->start_term = true;
			p->nesting_brackets++;
			bool was_consing = p->was_consing;
			p->was_consing = false;
			tokenize(p, true, true);

			if (!p->was_consing)
				make_interned(p, g_nil_s);

			p->start_term = p->last_close = false;
			p->was_consing = was_consing;
			last_bar = last_op = false;

			if (p->error)
				break;

			c = p->cl->cells + save_idx;
			c->nbr_cells = p->cl->cidx - save_idx;
			fix_list(c);
			last_num = false;
			continue;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "{")) {
			save_idx = p->cl->cidx;
			cell *c = make_interned(p, g_braces_s);
			check_error(c);
			c->arity = 1;
			p->start_term = true;
			p->nesting_braces++;
			tokenize(p, false, false);
			last_bar = false;

			if (p->error)
				break;

			c = p->cl->cells+save_idx;
			c->nbr_cells = p->cl->cidx - save_idx;
			p->start_term = p->last_close = false;
			last_op = false;
			last_num = false;
			continue;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "(")) {
			p->start_term = true;
			p->nesting_parens++;
			unsigned tmp_arity = tokenize(p, is_func, false);
			last_bar = false;

			if (p->error)
				break;

			if (is_func) {
				cell *c = p->cl->cells + save_idx;
				c->arity = tmp_arity;
				c->nbr_cells = p->cl->cidx - save_idx;
			}

			is_func = last_op = false;
			last_num = false;
			p->start_term = p->last_close = false;
			continue;
		}

		if (!p->quote_char && !args && !consing && last_op && !SB_strcmp(p->token, ",")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, quotes needed around operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "quotes_needed";
			p->error = true;
			break;
		}

		if (!p->quote_char && args && !consing && p->is_op /*&& last_op*/ && SB_strcmp(p->token, ",")) {
			unsigned specifier = 0;
			unsigned priority = search_op(p->m, SB_cstr(p->token), &specifier, last_op && !last_postfix);

			if (!last_op && (priority > 999)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, parens needed around operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "parens_needed";
				p->error = true;
				break;
			}
		}

		if (!p->quote_char && consing && p->is_op && SB_strcmp(p->token, ",") && SB_strcmp(p->token, "|")) {
			unsigned specifier = 0;
			unsigned priority = search_op(p->m, SB_cstr(p->token), &specifier, last_op && !last_postfix);

			if (!last_op && (priority > 999)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, parens needed around operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "parens_needed";
				p->error = true;
				break;
			}
		}

		if (!p->quote_char && consing && !SB_strcmp(p->token, ",")) {
			if ((arg_idx == p->cl->cidx) || !p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if ((*p->srcptr == ',') && !p->flags.double_quote_codes) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing element '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "missing_element";
				p->error = true;
				break;
			}

			if (p->was_consing || last_op) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, parsing list '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "list";
				p->error = true;
				break;
			}

			cell *c = make_interned(p, g_dot_s);
			c->arity = 2;
			p->start_term = last_op = true;
			last_num = false;
			p->last_close = false;
			continue;
		}

		if (!p->quote_char &&
			((args && !SB_strcmp(p->token, ",")) ||
			(consing && !p->was_consing && !p->start_term && (!SB_strcmp(p->token, ",") || !SB_strcmp(p->token, "|")))
			)) {
			if ((arg_idx == p->cl->cidx) || !p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			analyze(p, arg_idx, last_op);
			arg_idx = p->cl->cidx;

			if (*p->srcptr == ',') {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if (args) {
				arity++;

				if (arity > MAX_ARITY) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: max arity reached, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

					p->error_desc = "max_arity";
					p->error = true;
					break;
				}
			}

			if (consing && !SB_strcmp(p->token, "|")) {
				p->was_consing = last_bar = true;
				//consing = false;
			}

			p->last_close = false;
			last_op = true;
			last_num = false;
			continue;
		}

		if (!p->is_quoted && consing && p->start_term && !SB_strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing list '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->is_quoted && p->was_consing && consing && !SB_strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing list '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->is_quoted && p->was_consing && last_bar && !SB_strcmp(p->token, "]")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing list '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->quote_char && p->start_term &&
			(!SB_strcmp(p->token, "]") || !SB_strcmp(p->token, ")") || !SB_strcmp(p->token, "}"))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, start of rule expected, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "start_expected";
			p->error = true;
			break;
		}

		if (!p->quote_char && !SB_strcmp(p->token, ")")) {
			if (arg_idx == p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			p->last_close = true;
			p->nesting_parens--;
			analyze(p, arg_idx, last_op=false);
			return arity;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "]")) {
			if (arg_idx == p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			p->last_close = true;
			p->nesting_brackets--;
			analyze(p, arg_idx, last_op=false);
			return arity;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "}")) {
			if (arg_idx == p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			p->last_close = true;
			p->nesting_braces--;
			analyze(p, arg_idx, last_op=false);
			return arity;
		}

		p->last_close = false;

		if (p->is_var && (*p->srcptr == '(')) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, var as functor, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "variable_cannot_be_functor";
			p->error = true;
			break;
		}

		unsigned specifier = 0;
		int priority = 0;

		if (is_interned(&p->v)) {
			char *s = eat_space(p);

			if (!s || !*s) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, incomplete, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "incomplete";
				p->error = true;
				break;
			}

			int nextch = *s;
			bool noneg = (!SB_strcmp(p->token, "-") || !SB_strcmp(p->token, "+")) && (nextch == '='); // Hack

			if (noneg) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, incomplete, needs parenthesis, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "incomplete";
				p->error = true;
				break;
			}

			priority = search_op(p->m, SB_cstr(p->token), &specifier, last_op && !last_postfix);
		}

		if (!SB_strcmp(p->token, "!") &&
			((*p->srcptr == ')') || (*p->srcptr == ';') || (*p->srcptr == ',') || (*p->srcptr == '.')))
			p->quote_char = 1;

		if (p->quote_char) {
			specifier = 0;
			priority = 0;
		}

		if (priority && (last_op || last_bar)
			&& !IS_POSTFIX(specifier)) {
			char *s = eat_space(p);

			if (!s || !*s) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, incomplete, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);

				p->error_desc = "error_incomplete";
				p->error = true;
				break;
			}

			int nextch = *s;

			if ((nextch == ',')
				|| (nextch == ';')
				|| (nextch == ')')
				|| (nextch == '|')
				|| (nextch == ']')
				|| (nextch == '}')
			) {
				specifier = 0;
				priority = 0;
			}
		}

		if (priority && IS_POSTFIX(specifier) && args && last_op) {
			specifier = 0;
			priority = 0;
		}

		if (priority && IS_INFIX(specifier) && last_op && !last_postfix && !last_quoted) {
			specifier = 0;
			priority = 0;
		}

		// Operators in canonical form..

		if (last_op && priority && (*p->srcptr == '(')) {
			p->v.tag = TAG_INTERNED;
			specifier = 0;
			priority = 0;
			p->quote_char = 0;
		}

		is_func = last_op && is_interned(&p->v) && !specifier && !last_num && (*p->srcptr == '(');

#if 0
		printf("*** =%s, last_op=%d, p->is_op=%d, is_func=%d, prefix=%d\n",
			SB_cstr(p->token), last_op, p->is_op, is_func, IS_PREFIX(specifier));
#endif

		if ((p->was_string || p->string) && is_func) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, near \"%s\", expected atom, %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "expected_atom";
			p->error = true;
			break;
		}

		if (is_func) {
			p->is_op = false;
			specifier = 0;
			save_idx = p->cl->cidx;
		}

		if (!p->is_op && !is_func && last_op && last_postfix) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, near '%s', operator expected postfix '%s', %s:%d\n", SB_cstr(p->token), p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		if ((!p->is_op || IS_PREFIX(specifier)) && !is_func && !last_op) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, near '%s', operator expected, %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_nbr);

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		last_quoted = p->is_quoted;
		last_op = SB_strcmp(p->token, ")") && priority;
		last_postfix = last_op && IS_POSTFIX(specifier);

		p->start_term = false;
		cell *c = make_a_cell(p);
		c->nbr_cells = 1;
		c->tag = p->v.tag;
		c->flags = p->v.flags;
		SET_OP(c,specifier);
		c->priority = priority;
		bool found = false;

		last_num = is_number(c);

		if (is_bigint(&p->v)) {
			c->val_bigint = p->v.val_bigint;
		} else if (is_smallint(&p->v)) {
			set_smallint(c, get_smallint(&p->v));
		} else if (p->v.tag == TAG_DOUBLE) {
			set_float(c, get_float(&p->v));
		} else if (!p->string && (!p->is_quoted || is_func || p->is_op || p->is_var
			|| (get_builtin(p->m->pl, SB_cstr(p->token), SB_strlen(p->token), 0, &found, NULL), found)
			|| (p->is_quoted && !SB_strcmp(p->token, "[]"))
			)) {

			if (is_func && !SB_strcmp(p->token, "."))
				c->priority = 0;

			if (p->is_var)
				c->tag = TAG_VAR;

			if (p->is_quoted)
				c->flags |= FLAG_CSTR_QUOTED;

			c->val_off = index_from_pool(p->m->pl, SB_cstr(p->token));
			ensure(c->val_off != ERR_IDX);
		} else {
			c->tag = TAG_CSTR;
			size_t toklen =SB_strlen(p->token);

			if ((toklen < MAX_SMALL_STRING) && !p->string) {
				memcpy(c->val_chr, SB_cstr(p->token), toklen);
				c->val_chr[toklen] = '\0';
				c->chr_len = toklen;
			} else {
				if (p->string) {
					c->flags |= FLAG_CSTR_STRING;
					c->arity = 2;
				}

				SET_STR(c, SB_cstr(p->token), toklen, 0);
			}
		}

		last_bar = false;
	}

	p->depth--;
	return !p->error;
}

bool run(parser *p, const char *pSrc, bool dump, query **subq, unsigned int yield_time_in_ms)
{
	if ((*pSrc == '.') && !pSrc[1]) {
		fprintf(stdout, "Error: syntax error, unexpected end of rule, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);
		return false;
	}

	SB(src);
	SB_sprintf(src, "true. %s", pSrc);
	SB_trim_ws(src);
	SB_trim(src, '.');
	SB_strcat(src, ".");

	p->srcptr = SB_cstr(src);
	bool ok;

	while (p->srcptr && *p->srcptr && !g_tpl_interrupt) {
		reset(p);
		p->line_nbr = 1;
		p->one_shot = true;
		p->consulting = false;

		if (!tokenize(p, false, false))
			break;

		if (!p->error && !p->end_of_term && !p->run_init) {
			fprintf(stdout, "Error: syntax error, missing operand or operator, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_nbr);
			p->error = true;
		}

		if (p->error) {
			p->pl->did_dump_vars = true;
			p->srcptr = NULL;
			SB_free(src);
			return false;
		}

		if (p->skip) {
			p->m->pl->status = true;
			p->srcptr = NULL;
			SB_free(src);
			return true;
		}

		query *q = query_create(p->m, false);

		if (!q) {
			p->srcptr = NULL;
			SB_free(src);
			return false;
		}

		if (subq)
			*subq = q;

		if (yield_time_in_ms > 0)
			do_yield_at(q, yield_time_in_ms);

		p->pl->curr_query = q;
		q->p = p;
		q->do_dump_vars = dump;
		q->run_init = p->run_init;
		execute(q, p->cl->cells, p->cl->nbr_vars);

		p->m->pl->halt = q->halt;
		p->m->pl->halt_code = q->halt_code;
		p->m->pl->status = q->status;
		p->m->pl->is_redo = q->is_redo;

		ok = !q->error;
		p->m = q->st.m;

		if (q->pl->is_query)
			break;

		query_destroy(q);

		if (!ok)
			break;
	}

	p->srcptr = NULL;
	SB_free(src);
	return ok;
}
