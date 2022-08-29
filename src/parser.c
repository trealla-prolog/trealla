#include <ctype.h>
#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "heap.h"
#include "library.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"
#include "utf8.h"

static const unsigned INITIAL_TOKEN_SIZE = 100;		// bytes
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

	const char *src = is_static(l) ? l->val_str : is_strbuf(l) ? (char*)l->val_strb->cstr + l->strb_off : (char*)l->val_chr + l->val_off;
	size_t len = len_char_utf8(src);
	tmp->tag = TAG_CSTR;
	tmp->nbr_cells = 1;
	tmp->flags = 0;
	tmp->arity = 0;
	memcpy(tmp->val_chr, src, len);
	tmp->val_chr[len] = '\0';
	tmp->chr_len = len;
	return tmp;
}

cell *list_tail(cell *l, cell *tmp)
{
	if (!l) return NULL;

	if (!is_string(l)) {
		cell *h = l + 1;
		return h + h->nbr_cells;
	}

	const char *src = is_static(l) ? l->val_str : is_strbuf(l) ? (char*)l->val_strb->cstr + l->strb_off : (char*)l->val_chr;
	size_t str_len = is_static(l) ? (size_t)l->str_len : is_strbuf(l) ? (size_t)l->val_strb->len - l->strb_off : (unsigned)l->chr_len;
	size_t len = len_char_utf8(src);

	if (str_len == len) {
		tmp->tag = TAG_INTERNED;
		tmp->nbr_cells = 1;
		tmp->arity = 0;
		tmp->flags = 0;
		tmp->val_off = g_nil_s;
		return tmp;
	}

	if (is_static(l)) {
		tmp->flags = FLAG_CSTR_BLOB | FLAG_STATIC | FLAG_CSTR_STRING;
		tmp->nbr_cells = 1;
		tmp->arity = 2;
		tmp->val_str = l->val_str + len;
		tmp->str_len = l->str_len - len;
		return tmp;
	}

	if (is_strbuf(l)) {
		copy_cells(tmp, l, 1);
		tmp->strb_off = l->strb_off + len;
		tmp->strb_len = l->strb_len - len;
		return tmp;
	}

	copy_cells(tmp, l, 1);
	memcpy(tmp->val_chr, l->val_chr + len, l->chr_len - len);
	tmp->val_chr[l->chr_len - len] = '\0';
	tmp->chr_len = l->chr_len - len;
	return tmp;
}

cell *get_body(cell *c)
{
	if (is_a_rule(c)) {
		c = c + 1;
		c += c->nbr_cells;

		if (is_end(c))
			return NULL;

		return c;
	}

	return NULL;
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
	if (!cl)
		return;

	for (pl_idx_t i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;
		unshare_cell(c);
		*c = (cell){0};
		//c->tag = TAG_EMPTY;
	}

	cl->cidx = 0;
}

static bool make_room(parser *p, unsigned nbr)
{
	while ((p->cl->cidx+nbr) >= p->cl->nbr_cells) {
		pl_idx_t nbr_cells = p->cl->nbr_cells * 3 / 2;

		clause *cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*nbr_cells));
		if (!cl) {
			p->error = true;
			return false;
		}

		p->cl = cl;
		p->cl->nbr_cells = nbr_cells;
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

void destroy_parser(parser *p)
{
	free(p->tmpbuf);
	free(p->save_line);
	free(p->token);
	clear_rule(p->cl);
	free(p->cl);
	free(p);
}

parser *create_parser(module *m)
{
	parser *p = calloc(1, sizeof(parser));
	check_error(p);
	p->pl = m->pl;
	p->m = m;
	p->token = calloc(1, p->token_size=INITIAL_TOKEN_SIZE+1);
	check_error(p->token, free(p));
	pl_idx_t nbr_cells = INITIAL_NBR_CELLS;
	p->cl = calloc(1, sizeof(clause)+(sizeof(cell)*nbr_cells));
	check_error(p->cl, (free(p->token), free(p)));
	p->cl->nbr_cells = nbr_cells;
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
			fprintf(stdout, "Error: unknown op\n");

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
		fprintf(stdout, "Error: unknown op spec tag\n");
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
					fprintf(stdout, "Error: could not set op\n");

				free(name);
				continue;
			}

			if (make_public) {
				if (!set_op(p->pl->user_m, name, specifier, get_smallint(p1))) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: could not set op\n");

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
				fprintf(stdout, "Error: could not set op\n");

			free(name);
			return;
		}

		if (make_public) {
			if (!set_op(p->pl->user_m, name, specifier, get_smallint(p1))) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: could not set op\n");

				free(name);
				return;
			}
		}

		free(name);
	}
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

	if (strcmp(C_STR(p, d), ":-") || (d->arity != 1))
		return;

	cell *c = d + 1;

	if (!is_interned(c))
		return;

	const char *dirname = C_STR(p, c);

	if (!strcmp(dirname, "initialization") && (c->arity <= 2)) {
		p->run_init = true;
		return;
	}

	cell *p1 = c + 1;

	if (!strcmp(dirname, "include") && (c->arity == 1)) {
		if (!is_atom(p1)) return;
		unsigned save_line_nbr = p->line_nbr;
		const char *name = C_STR(p, p1);
		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename, true)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: not found: %s\n", filename);

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
				fprintf(stdout, "Error: not found: %s\n", filename);

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

		if (is_variable(p1)) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", p->m->filename);
			char *ptr = tmpbuf + strlen(tmpbuf) - 1;

			while (*ptr && (*ptr != '.') && (ptr != tmpbuf))
				ptr--;

			if (*ptr == '.')
				*ptr = '\0';

			name = tmpbuf;
		} else if (!is_atom(p1)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: pragma name not an atom\n");

			p->error = true;
			return;
		} else
			name = C_STR(p, p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->m->pl, name)) != NULL) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: module already loaded: %s\n", name);
			//
			p->already_loaded = true;
			p->m = tmp_m;

			if (tmp_m != p->m)
				p->m->used[p->m->idx_used++] = tmp_m;

			return;
		}

		tmp_m = create_module(p->m->pl, name);
		if (!tmp_m) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module creation failed: %s\n", name);

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
		cell *attr = arg+1;
		const char *name = C_STR(p, attr);

		if (strcmp(name, p->m->name))
			duplicate_module(p->m->pl, p->m, name);

		return;
	}

	if (!strcmp(dirname, "module") && (c->arity == 2)) {
		cell *p2 = c + 2;
		const char *name = "";
		char tmpbuf[1024];

		if (is_variable(p1)) {
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", p->m->filename);
			char *ptr = tmpbuf + strlen(tmpbuf) - 1;

			while (*ptr && (*ptr != '.') && (ptr != tmpbuf))
				ptr--;

			if (*ptr == '.')
				*ptr = '\0';

			name = tmpbuf;
		} else if (!is_atom(p1)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module name not an atom\n");

			p->error = true;
			return;
		} else
			name = C_STR(p, p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->m->pl, name)) != NULL) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: module already loaded: %s\n", name);
			//
			p->already_loaded = true;
			p->m = tmp_m;
			return;
		}

		tmp_m = create_module(p->m->pl, name);

		if (!tmp_m) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: module creation failed: %s\n", name);

			p->error = true;
			return;
		}

		p->m = tmp_m;
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
						destroy_module(p->m);
						p->m = NULL;
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: predicate creation failed\n");

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
		if (!is_atom(p1) && !is_structure(p1)) return;
		const char *name = C_STR(p, p1);
		char dstbuf[1024*2];

		if (!strcmp(name, "library")) {
			p1 = p1 + 1;
			if (!is_interned(p1)) return;
			name = C_STR(p, p1);
			module *m;

			if ((m = find_module(p->m->pl, name)) != NULL) {
				if (!m->fp)
					do_db_load(m);

				if (m != p->m)
					p->m->used[p->m->idx_used++] = m;

				return;
			}

			// These might be found in other Prologs...

			if (!strcmp(name, "between")
				|| !strcmp(name, "error")
				|| !strcmp(name, "samsort")
				|| !strcmp(name, "terms")
				|| !strcmp(name, "types")
				|| !strcmp(name, "iso_ext")
				|| !strcmp(name, "files")
				|| !strcmp(name, "read_util")
				|| !strcmp(name, "dcg/basics")
				)
				return;

			for (library *lib = g_libs; lib->name; lib++) {
				if (strcmp(lib->name, name))
					continue;

				ASTRING(src);
				ASTRING_strcatn(src, lib->start, *lib->len);
				ASTRING(s1);
				ASTRING_sprintf(s1, "library/%s", lib->name);
				m = load_text(p->m, ASTRING_cstr(src), ASTRING_cstr(s1));
				ASTRING_free(s1);
				ASTRING_free(src);

				if (m != p->m)
					do_db_load(m);

				if (m != p->m)
					p->m->used[p->m->idx_used++] = m;

				return;
			}

			query q = (query){0};
			q.pl = p->pl;
			q.st.m = p->m;
			snprintf(dstbuf, sizeof(dstbuf), "%s%c", g_tpl_lib, PATH_SEP_CHAR);
			char *dst = dstbuf + strlen(dstbuf);
			pl_idx_t ctx = 0;
			print_term_to_buf(&q, dst, sizeof(dstbuf)-strlen(g_tpl_lib), p1, ctx, 1, false, 0);
			name = dstbuf;
		}

		char *filename = relative_to(p->m->filename, name);
		module *m;

		if (!(m = load_file(p->m, filename, false))) {
			//if (DUMP_ERRS || !p->do_read_term)
			//	fprintf(stdout, "Error: using module file: %s\n", filename);

			//p->error = true;
			free(filename);
			return;
		}

		free(filename);

		if (m != p->m)
			p->m->used[p->m->idx_used++] = m;

		return;
	}

	if (!strcmp(dirname, "meta_predicate") && (c->arity == 1)) {
		if (!is_structure(p1)) return;
	}

	if (!strcmp(dirname, "set_prolog_flag") && (c->arity == 2)) {
		cell *p2 = c + 2;

		if (!strcmp(C_STR(p, p1), "indexing_threshold") && is_smallint(p2))
			p->m->indexing_threshold = get_smallint(p2);

		if (!is_interned(p2)) return;

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
					fprintf(stdout, "Error: unknown value\n");

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

	if (!strcmp(dirname, "if") && (c->arity == 1)) {
		if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
			fprintf(stdout, "Warning: unknown directive: %s\n", dirname);

		return;
	}

	if (!strcmp(dirname, "else") && (c->arity == 1)) {
		if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
			fprintf(stdout, "Warning: unknown directive: %s\n", dirname);

		return;
	}

	if (!strcmp(dirname, "endif") && (c->arity == 1)) {
		if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
			fprintf(stdout, "Warning: unknown directive: %s\n", dirname);

		return;
	}

	LIST_HANDLER(p1);

	while (is_list(p1) && !g_tpl_interrupt) {
		cell *h = LIST_HEAD(p1);

		if (is_interned(h) && (!strcmp(C_STR(p, h), "/") || !strcmp(C_STR(p, h), "//")) && (h->arity == 2)) {
			cell *c_name = h + 1;
			if (!is_atom(c_name)) continue;
			cell *c_arity = h + 2;
			if (!is_integer(c_arity)) continue;
			unsigned arity = get_smallint(c_arity);

			if (!strcmp(C_STR(p, h), "//"))
				arity += 2;

			cell tmp = *c_name;
			tmp.arity = arity;

			//printf("*** %s => %s / %u\n", dirname, C_STR(p, c_name), arity);

			if (!strcmp(dirname, "dynamic")) {
				predicate * pr = find_predicate(p->m, &tmp);

				if (pr && !pr->is_dynamic && pr->cnt) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: no permission to modify static predicate %s:%s/%u\n", p->m->name, C_STR(p->m, c_name), arity);

					p->error = true;
					return;
				}

				set_dynamic_in_db(p->m, C_STR(p, c_name), arity);
			} else if (!strcmp(dirname, "persist")) {
				predicate * pr = find_predicate(p->m, &tmp);

				if (pr && !pr->is_dynamic && pr->cnt) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: no permission to modify static predicate %s:%s/%u\n", p->m->name, C_STR(p->m, c_name), arity);

					p->error = true;
					return;
				}

				set_persist_in_db(p->m, C_STR(p, c_name), arity);
			} else if (!strcmp(dirname, "public")) {
			} else if (!strcmp(dirname, "table") && false) {
				set_table_in_db(p->m, C_STR(p, c_name), arity);
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
					fprintf(stdout, "Warning: unknown directive: %s\n", dirname);
			}
		}

		p1 = LIST_TAIL(p1);
	}

	if (is_nil(p1))
		return;

	//printf("*** %s\n", dirname);

	while (is_interned(p1) && !g_tpl_interrupt) {
		module *m = p->m;
		cell *c_id = p1;

		if (!strcmp(C_STR(p, p1), ":") && (p1->arity == 2)) {
			cell *c_mod = p1 + 1;
			if (!is_atom(c_mod)) return;
			m = find_module(p->m->pl, C_STR(p, c_mod));
			c_id = p1 + 2;
		}

		if (!strcmp(C_STR(p, c_id), "/") && (p1->arity == 2)) {
			cell *c_name = c_id + 1;
			if (!is_atom(c_name)) return;
			cell *c_arity = c_id + 2;
			if (!is_integer(c_arity)) return;
			unsigned arity = get_smallint(c_arity);
			cell tmp = *c_name;
			tmp.arity = arity;

			//printf("*** *** *** %s : %s / %u\n", m->name, C_STR(p, c_name), arity);

			if (!strcmp(C_STR(p, c_id), "//"))
				arity += 2;

			if (!strcmp(dirname, "multifile"))
				set_multifile_in_db(m, C_STR(p, c_name), arity);
			else if (!strcmp(dirname, "discontiguous"))
				set_discontiguous_in_db(m, C_STR(p, c_name), arity);
			else if (!strcmp(dirname, "public"))
				;
			else if (!strcmp(dirname, "table") && false)
				set_table_in_db(m, C_STR(p, c_name), arity);
			else if (!strcmp(dirname, "dynamic")) {
				predicate * pr = find_predicate(p->m, &tmp);

				if (pr && !pr->is_dynamic && pr->cnt) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: no permission to modify static predicate %s:%s/%u\n", m->name, C_STR(p->m, c_name), arity);

					p->error = true;
					return;
				}

				set_dynamic_in_db(m, C_STR(p, c_name), arity);
			} else if (!strcmp(dirname, "persist")) {
				if (find_predicate(p->m, &tmp)) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: no permission to modify static predicate %s:%s/%u\n", p->m->name, C_STR(p->m, c_name), arity);

					p->error = true;
					return;
				}

				set_persist_in_db(m, C_STR(p, c_name), arity);
			} else {
				if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
					fprintf(stdout, "Warning: unknown directive: %s\n", dirname);
			}

			p1 += p1->nbr_cells;
		} else if (!strcmp(dirname, "meta_predicate")) {
			set_meta_predicate_in_db(m, p1);
			p1 += p1->nbr_cells;
		} else if (!strcmp(C_STR(p, p1), ",") && (p1->arity == 2))
			p1 += 1;
		else {
			if (((DUMP_ERRS || !p->do_read_term)) && !p->m->pl->quiet)
				fprintf(stdout, "Warning: unknown directive: %s\n", dirname);

			break;
		}
	}
}

static void check_first_cut(parser *p)
{
	cell *c = get_body(p->cl->cells);
	int is_cut_only = true;

	if (!c)
		return;

	while (!is_end(c)) {
		if (!(c->flags&FLAG_BUILTIN))
			break;

		if (!strcmp(C_STR(p, c), ",")
			|| !strcmp(C_STR(p, c), ";")
			|| !strcmp(C_STR(p, c), "->")
			|| !strcmp(C_STR(p, c), "*->")
			|| !strcmp(C_STR(p, c), "-->")
			)
			;
		else if (!IS_OP(c) && !strcmp(C_STR(p, c), "!")) {
			p->cl->is_first_cut = true;
			break;
		} else {
			is_cut_only = false;
			break;
		}

		c += c->nbr_cells;
	}

	if (p->cl->is_first_cut && is_cut_only)
		p->cl->is_cut_only = true;
}

static pl_idx_t get_varno(parser *p, const char *src)
{
	int anon = !strcmp(src, "_");
	size_t offset = 0;
	int i = 0;

	while (p->vartab.var_pool[offset]) {
		if (!strcmp(p->vartab.var_pool+offset, src) && !anon)
			return i;

		offset += strlen(p->vartab.var_pool+offset) + 1;
		i++;
	}

	size_t len = strlen(src);

	if ((offset+len+1) >= MAX_VAR_POOL_SIZE) {
		fprintf(stdout, "Error: variable pool exhausted\n");
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

	// Any variable that only occurs in the head of
	// a clause we consider a temporary variable...

	bool in_body = false;

	for (pl_idx_t i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;

		if (c->val_off == g_neck_s)
			in_body = true;

		if (!is_variable(c))
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

		if (!is_variable(c) || is_temporary(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "_V%u", c->var_nbr);
			c->var_nbr = get_varno(p, tmpbuf);
		} else
			c->var_nbr = get_varno(p, C_STR(p, c));

		c->var_nbr += start;

		if (c->var_nbr == MAX_VARS) {
			fprintf(stdout, "Error: max vars reached\n");
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

		if (!is_variable(c) || !is_temporary(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "_V%u", c->var_nbr);
			c->var_nbr = get_varno(p, tmpbuf);
		} else
			c->var_nbr = get_varno(p, C_STR(p, c));

		c->var_nbr += start;

		if (c->var_nbr == MAX_VARS) {
			fprintf(stdout, "Error: max vars reached\n");
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
				fprintf(stdout, "Warning: singleton: %s, near line %u, file '%s'\n", p->vartab.var_name[i], p->line_nbr, get_filename(p->m->filename));
		}
	}

	cell *c = make_a_cell(p);
	ensure(c);
	c->tag = TAG_END;
	c->nbr_cells = 1;
	check_first_cut(p);
	p->cl->is_fact = !get_logical_body(p->cl->cells);
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
					fprintf(stdout, "Error: syntax error, operator clash, line %u\n", p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}

			rhs += rhs->nbr_cells;

			if ((((pl_idx_t)(rhs - p->cl->cells)) < end_idx)
				&& is_xf(rhs) && (rhs->priority == c->priority)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, line %u\n", p->line_nbr);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		if (is_prefix(c)) {
			cell *rhs = c + 1;

			if (is_infix(rhs) && !rhs->arity && (rhs->priority > c->priority) && !is_quoted(rhs)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, line %u\n", p->line_nbr);

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
					fprintf(stdout, "Error: syntax error, missing operand to '%s', line %u\n", C_STR(p, c), p->line_nbr);

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
					fprintf(stdout, "Error: syntax error, operator clash, line %u\n", p->line_nbr);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		if (is_prefix(rhs) && !rhs->arity && (rhs->priority > c->priority)) {
			if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, line %u\n", p->line_nbr);

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
					fprintf(stdout, "Error: syntax error, operator clash, line %u\n", p->line_nbr);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		pl_idx_t off = (pl_idx_t)(rhs - p->cl->cells);
		bool nolhs = (last_idx == (unsigned)-1);
		if (i == start_idx) nolhs = true;

		if (nolhs || (off > end_idx)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, missing operand to '%s', line %u\n", C_STR(p, c), p->line_nbr);

			p->error_desc = "operand_missing";
			p->error = true;
			return false;
		}

		cell *lhs = p->cl->cells + last_idx;

		if (is_infix(lhs) && !lhs->arity && !is_quoted(lhs)) {
			if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, operator clash, line %u\n", p->line_nbr);

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
					fprintf(stdout, "Error: syntax error, operator clash, line %u\n", p->line_nbr);

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

static bool dcg_expansion(parser *p)
{
	if (!p->m->pl->dcgs && !find_module(p->m->pl, "dcgs")) {
		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, "dcgs"))
				continue;

			char *src = malloc(*lib->len+1);
			check_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			ASTRING(s);
			ASTRING_sprintf(s, "library/%s", lib->name);
			module *tmp_m = load_text(p->m, src, ASTRING_cstr(s));
			ASTRING_free(s);

			if (tmp_m) {
				p->m->used[p->m->idx_used++] = tmp_m;
				p->m->pl->dcgs = tmp_m;
			}

			free(src);
			break;
		}
	}

	query *q = create_query(p->m, false);
	check_error(q);
	char *dst = print_canonical_to_strbuf(q, p->cl->cells, 0, 0);
	ASTRING(s);
	ASTRING_sprintf(s, "dcg_translate((%s),_TermOut).", dst);
	free(dst);

	parser *p2 = create_parser(p->m);
	check_error(p2, destroy_query(q));
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = ASTRING_cstr(s);
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	execute(q, p2->cl->cells, p2->cl->nbr_vars);
	ASTRING_free(s);
	frame *f = GET_FIRST_FRAME();
	char *src = NULL;

	for (unsigned i = 0; i < p2->cl->nbr_vars; i++) {
		if (strcmp(p2->vartab.var_name[i], "_TermOut"))
			continue;

		slot *e = GET_SLOT(f, i);

		if (is_empty(&e->c))
			break;

		q->latest_ctx = e->c.var_ctx;
		cell *c;

		if (is_indirect(&e->c)) {
			c = e->c.val_ptr;
		} else
			c = deref(q, &e->c, e->c.var_ctx);

		src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);
		strcat(src, ".");
		break;
	}

	if (!src) {
		destroy_parser(p2);
		destroy_query(q);
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

	destroy_parser(p2);
	destroy_query(q);
	return true;
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

	const char *functor = C_STR(p, goal);

	if (get_builtin(p->pl, functor, goal->arity, NULL, NULL) /*|| is_op(goal)*/)
		return goal;

	//if (search_predicate(p->m, goal))
	//	return goal;

	for (unsigned i = 0; i < goal->nbr_cells; i++) {
		if (!is_variable(&goal[i]))
			continue;

		//printf("*** Var %s / %u\n", GET_POOL(p, goal[i].val_off), goal[i].var_nbr);
	}

	query *q = create_query(p->m, false);
	check_error(q);
	char *dst = print_canonical_to_strbuf(q, goal, 0, 0);
	ASTRING(s);
	ASTRING_sprintf(s, "goal_expansion((%s),_TermOut).", dst);
	free(dst);

	//printf("*** GE1 %s\n", ASTRING_cstr(s));

	parser *p2 = create_parser(p->m);
	check_error(p2, destroy_query(q));
	q->p = p2;
	p2->cl->nbr_vars = p->cl->nbr_vars;
	p2->vartab = p->vartab;
	p2->reuse = true;
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = ASTRING_cstr(s);
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	execute(q, p2->cl->cells, p2->cl->nbr_vars);

	ASTRING_free(s);

	if (q->retry != QUERY_OK) {
		destroy_parser(p2);
		destroy_query(q);
		return goal;
	}

	frame *f = GET_FIRST_FRAME();
	char *src = NULL;

	for (unsigned i = 0; i < p2->cl->nbr_vars; i++) {
		//printf("*** vartab %u %s\n", i, p2->vartab.var_name[i]);

		slot *e = GET_SLOT(f, i);

		if (is_empty(&e->c))
			continue;

		q->latest_ctx = e->c.var_ctx;
		cell *c;

		if (is_indirect(&e->c)) {
			c = e->c.val_ptr;
		} else
			c = deref(q, &e->c, e->c.var_ctx);

		if (strcmp(p2->vartab.var_name[i], "_TermOut"))
			continue;

		q->varnames = true;
		src = print_term_to_strbuf(q, c, q->latest_ctx, 1);
		strcat(src, ".");
		//printf("*** GE2 %s\n", src);
		break;
	}

	if (!src) {
		destroy_parser(p2);
		destroy_query(q);
		p->error = true;
		return goal;
	}

	reset(p2);
	p2->srcptr = src;
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	free(src);

	// snip the old goal...

	unsigned goal_idx = goal - p->cl->cells;
	unsigned old_cells = goal->nbr_cells;
	unsigned rem_cells = p->cl->cidx - (goal_idx + old_cells);
	memmove(goal, goal + old_cells, sizeof(cell)*rem_cells);
	p->cl->cidx -= old_cells;

	// make room for new goal...

	unsigned new_cells = p2->cl->cidx-1;
	unsigned trailing_cells = p->cl->cidx - goal_idx;

	if ((p->cl->cidx + new_cells) > p->cl->nbr_cells) {
		unsigned extra = (p->cl->cidx + new_cells) - p->cl->nbr_cells;
		make_room(p, extra);
		goal = p->cl->cells + goal_idx;	// in case of a realloc
	}

	memmove(goal+new_cells, goal, sizeof(cell)*trailing_cells);

	// paste the new goal...

	memcpy(goal, p2->cl->cells, sizeof(cell)*new_cells);
	p->cl->cidx += new_cells;
	p2->cl = NULL;

	// done

	destroy_parser(p2);
	destroy_query(q);

	return goal;
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

	query *q = create_query(p->m, false);
	check_error(q);
	char *dst = print_canonical_to_strbuf(q, p->cl->cells, 0, 0);
	ASTRING(s);
	ASTRING_sprintf(s, "term_expansion((%s),_TermOut).", dst);
	free(dst);

	parser *p2 = create_parser(p->m);
	check_error(p2, destroy_query(q));
	p2->line_nbr = p->line_nbr;
	p2->skip = true;
	p2->srcptr = ASTRING_cstr(s);
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	execute(q, p2->cl->cells, p2->cl->nbr_vars);

	//printf("*** TE1 %s\n", ASTRING_cstr(s));

	ASTRING_free(s);

	if (q->retry != QUERY_OK) {
		destroy_parser(p2);
		destroy_query(q);
		return false;
	}

	frame *f = GET_FIRST_FRAME();
	char *src = NULL;

	for (unsigned i = 0; i < p2->cl->nbr_vars; i++) {
		slot *e = GET_SLOT(f, i);

		if (is_empty(&e->c))
			continue;

		q->latest_ctx = e->c.var_ctx;
		cell *c;

		if (is_indirect(&e->c)) {
			c = e->c.val_ptr;
		} else
			c = deref(q, &e->c, e->c.var_ctx);

		if (strcmp(p2->vartab.var_name[i], "_TermOut"))
			continue;

		src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);
		strcat(src, ".");
		//printf("*** TE2 %s\n", src);
		break;
	}

	if (!src) {
		destroy_parser(p2);
		destroy_query(q);
		p->error = true;
		return false;
	}

	//printf("*** TE3 %s\n", src);

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

	destroy_parser(p2);
	destroy_query(q);

	return term_expansion(p);
}

static cell *insert_here(parser *p, cell *c, cell *p1)
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
	p1->fn_ptr = get_fn_ptr(fn_iso_call_n);
	p1->val_off = g_call_s;
	p1->nbr_cells = 2;
	p1->arity = 1;

	p->cl->cidx++;
	return p->cl->cells + c_idx;
}

cell *check_body_callable(parser *p, cell *c)
{
	if (is_xfx(c) || is_xfy(c)) {
		if (!strcmp(C_STR(p, c), ",")
			|| !strcmp(C_STR(p, c), ";")
			|| !strcmp(C_STR(p, c), "->")
			|| !strcmp(C_STR(p, c), "*->")
			|| !strcmp(C_STR(p, c), ":-")) {
			cell *lhs = c + 1;
			cell *tmp;

			if ((tmp = check_body_callable(p, lhs)) != NULL)
				return tmp;

			cell *rhs = lhs + lhs->nbr_cells;

			if ((tmp = check_body_callable(p, rhs)) != NULL)
				return tmp;
		}
	}

	return !is_callable(c) && !is_variable(c) ? c : NULL;
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

			if (is_variable(lhs)) {
				c = insert_here(p, c, lhs);
				lhs = c + 1;
			} else {
				if ((c->val_off != g_neck_s))
					lhs = goal_expansion(p, lhs);

				lhs = term_to_body_conversion(p, lhs);
			}

			cell *rhs = lhs + lhs->nbr_cells;
			c = p->cl->cells + c_idx;

			if (is_variable(rhs) && !norhs)
				c = insert_here(p, c, rhs);
			else {
				rhs = goal_expansion(p, rhs);
				rhs = term_to_body_conversion(p, rhs);
			}

			c->nbr_cells = 1 + lhs->nbr_cells + rhs->nbr_cells;
		}
	} else if (is_fx(c) || is_fy(c)) {
		if ((c->val_off == g_negation_s)
			|| (c->val_off == g_neck_s)) {
			cell *save_c = c;
			cell *rhs = c + 1;

			if (is_variable(rhs)) {
				c = insert_here(p, c, rhs);
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

bool virtual_term(parser *p, const char *src)
{
	parser *p2 = create_parser(p->m);
	check_error(p2);
	p2->consulting = true;
	p2->srcptr = (char*)src;
	tokenize(p2, false, false);
	destroy_parser(p2);
	return true;
}

static cell *make_interned(parser *p, pl_idx_t offset)
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
#define ALLOW_UNICODE_ESCAPE true

static int get_escape(const char **_src, bool *error, bool number)
{
	const char *src = *_src;
	int ch = *src++;
	const char *ptr = strchr(g_anti_escapes, ch);

	if (ptr)
		ch = g_escapes[ptr-g_anti_escapes];
	else if ((isdigit(ch) || (ch == 'x')
#if ALLOW_UNICODE_ESCAPE
		|| (ch == 'u') || (ch == 'U')
#endif
		)
		&& !number) {
		int unicode = 0;

		if (ch == 'x')
			ch = get_hex(&src, UINT_MAX, error);
#if ALLOW_UNICODE_ESCAPE
		else if (ch == 'U') {
			ch = get_hex(&src, 8, error);
			unicode = 1;
		} else if (ch == 'u') {
			ch = get_hex(&src, 4, error);
			unicode = 1;
		}
#endif
		else {
			src--;
			ch = get_octal(&src);
		}

		if (!unicode && (*src++ != '\\')) {
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

void read_integer(parser *p, mp_int v2, int base, const char *src,  const char **srcptr)
{
	if (!p->tmpbuf)
		p->tmpbuf = malloc(p->tmpbuf_size=256);

	char *dst = p->tmpbuf;
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
				fprintf(stdout, "Error: syntax error, illegal character\n");
			*srcptr = src;
			p->error = true;
			return;
		}

		spaces = 0;
		*dst++ = *src++;

		if ((size_t)(dst - p->tmpbuf) >= (p->tmpbuf_size-1)) {
			size_t offset = dst - p->tmpbuf;
			p->tmpbuf = realloc(p->tmpbuf, p->tmpbuf_size*=2);
			dst = p->tmpbuf + offset;
		}

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

	*dst = '\0';

	if ((base != 16) && !isdigit(src[-1]))
		src--;
	else if ((base == 16) && !isxdigit(src[-1]))
		src--;

	mp_int_read_cstring(v2, base, (char*)p->tmpbuf, NULL);
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
			fprintf(stdout, "Error: syntax error, parsing number, line %u\n", p->line_nbr);

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	if (!isdigit(*s))
		return false;

	if ((*s == '0') && (s[1] == '\'')
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
						fprintf(stdout, "Error: syntax error, parsing number, line %u\n", p->line_nbr);

					p->error_desc = "number";
					p->error = true;
					return false;
				}

				s++;
				goto LOOP;
			}

			if (!*s || iscntrl(*s)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, parsing number, line %u\n", p->line_nbr);

				p->error_desc = "number";
				p->error = true;
				return false;
			}

			v = get_escape(&s, &p->error, true);
		} else if ((*s == '\'') && s[1] == '\'') {
			s++;
			v = *s++;
#if 1
		} else if ((*s == '\'') && !p->flags.not_strict_iso && search_op(p->m, "", NULL, false)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing number, line %u\n", p->line_nbr);

			p->error_desc = "number";
			p->error = true;
			return false;
#endif
		} else
			v = get_char_utf8(&s);

		if (p->error) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing number, line %u\n", p->line_nbr);

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

		read_integer(p, &v2, 2, s, &s);

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

		read_integer(p, &v2, 8, s, &s);

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

		read_integer(p, &v2, 16, s, &s);

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

	read_integer(p, &v2, 10, s, &s);

	if (s && (*s == '.') && isdigit(s[1])) {
		p->v.tag = TAG_FLOAT;
		errno = 0;
		double v = strtod(tmpptr, &tmpptr);

		if ((int)v && (errno == ERANGE)) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, float overflow %g, line %u\n", v, p->line_nbr);

			p->error_desc = "float_overflow";
			p->error = true;
			return false;
		}

		set_float(&p->v, v);
		if (neg) p->v.val_float = -p->v.val_float;
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
			fprintf(stdout, "Error: syntax error, parsing number, line %u\n", p->line_nbr);

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
				p->srcptr = (char*)src;
				done = false;
				continue;
			}

			if (*src == '\n')
				p->line_nbr++;

			if (p->comment)
				src++;

			if (!*src && p->comment && p->fp) {
				if (p->no_fp || getline(&p->save_line, &p->n_line, p->fp) == -1)
					return p->srcptr = "";

				p->did_getline = true;
				src = p->srcptr = p->save_line;
			}
		}
		 while (*src && p->comment);
	}
	 while (!done);

	return (char*)src;
}

static bool eat_comment(parser *p)
{
	char *src = p->srcptr;

	if (*src != '/')
		return true;

	src++;

	if (*src != '*')
		return true;

	src++;

	while (*src) {
		if ((src[0] == '*') && (src[1] == '/')) {
			src += 2;
			p->srcptr = src;
			return true;
		}

		src++;
	}

	return true;
}

static bool check_space_before_function(parser *p, int ch, const char *src)
{
	if (iswspace(ch) && strcmp(p->token, ".")) {
		p->srcptr = (char*)src;
		src = eat_space(p);

		if (!src || !*src) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, incomplete statement, line %d\n", p->line_nbr);

			p->error_desc = "incomplete_statement";
			p->error = true;
			return false;
		}

		if (!p->is_op && (*src == '(')) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, operator expected before parens, line %d: %s\n", p->line_nbr, p->token);

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

	if (!p->token || (strlen(src) >= p->token_size)) {
		p->token = realloc(p->token, p->token_size = strlen(src)+1);
		check_error(p->token);
	}

	char *dst = p->token;
	*dst = '\0';
	bool neg = false;
	p->v.tag = TAG_INTERNED;
	p->v.flags = 0;
	p->v.nbr_cells = 1;
	p->quote_char = 0;
	p->was_string = p->string = p->is_quoted = p->is_variable = p->is_op = p->symbol = false;

	if (p->dq_consing && (*src == '"') && (src[1] == '"')) {
		src++;
	} else if (p->dq_consing && (*src == '"')) {
		*dst++ = ']';
		*dst = '\0';
		p->srcptr = (char*)++src;
		p->dq_consing = 0;
		p->toklen = dst - p->token;
		return true;
	}

	if (p->dq_consing < 0) {
		*dst++ = ',';
		*dst = '\0';
		p->dq_consing = 1;
		p->toklen = dst - p->token;
		return true;
	}

	if (p->dq_consing) {
		int ch = get_char_utf8(&src);

		if ((ch == '\\') && p->flags.character_escapes) {
			ch = get_escape(&src, &p->error, false);

			if (p->error) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, line %d\n", p->srcptr, p->line_nbr);

				p->error_desc = "illegal_character_escape";
				p->error = true;
				return false;
			}
		}

		dst += snprintf(dst, 8, "%u", ch);
		*dst = '\0';
		p->srcptr = (char*)src;
		set_smallint(&p->v, ch);
		p->v.tag = TAG_INTEGER;
		p->dq_consing = -1;
		p->toklen = dst - p->token;
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
				fprintf(stdout, "Error: syntax error, near line %d\n", p->line_nbr);

			p->error_desc = "incomplete_statement";
			p->error = true;
			return false;
		}

		src = eat_space(p);

		if (!src || !*src) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, incomplete statement, line %d\n", p->line_nbr);

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
		if ((size_t)(src-tmpptr) >= p->token_size) {
			size_t offset = dst - p->token;
			p->token = realloc(p->token, p->token_size = (src-tmpptr)+1);
			check_error(p->token);
			dst = p->token+offset;
		}

		strncpy(dst, tmpptr, src-tmpptr);
		dst[src-tmpptr] = '\0';

		if ((dst[0] != '0') && (dst[1] != 'x')) {
			if ((strchr(p->tmpbuf, '.') || strchr(p->tmpbuf, 'e') || strchr(dst, 'E')) && !strchr(p->tmpbuf, '\'')) {
				if (!valid_float(p->token)) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, float, line %u\n", p->line_nbr);

					p->error_desc = "float";
					p->error = true;
					return false;
				}
			}
		}

		p->srcptr = (char*)src;
		p->toklen = dst - p->token;
		int ch = peek_char_utf8(src);

		if (!check_space_before_function(p, ch, src))
			return false;

		src = p->srcptr;
		ch = peek_char_utf8(src);

		if (ch == '(') {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing number, line %u\n", p->line_nbr);

			p->error_desc = "number";
			p->error = true;
			return false;
		}
		return eat_comment(p);
	}

	// Quoted...

	if ((*src == '"') || (*src == '`') || (*src == '\'')) {
		p->quote_char = *src++;
		p->is_quoted = true;

		if ((p->quote_char == '"') && p->flags.double_quote_codes) {
			*dst++ = '[';

			if ((*src == '"') && (src[1] != '"')) {
				*dst++ = ']';
				*dst = '\0';
				p->srcptr = (char*)++src;
				p->toklen = dst - p->token;
				return true;
			}

			*dst = '\0';
			p->dq_consing = 1;
			p->quote_char = 0;
			p->srcptr = (char*)src;
			p->toklen = dst - p->token;
			return true;
		} else if ((p->quote_char == '"') && p->flags.double_quote_chars)
			p->string = true;

		if (p->string && (*src == p->quote_char) && (*src == '"')) {
			*dst++ = '[';
			*dst++ = ']';
			*dst = '\0';
			src++;
			p->was_string = true;
			p->string = false;
			p->srcptr = (char*)src;
			p->toklen = dst - p->token;
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
						fprintf(stdout, "Error: syntax error, unterminated quoted atom, line %d\n", p->line_nbr);

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
						fprintf(stdout, "Error: syntax error, invalid quoted character, line %d\n", p->line_nbr);

					p->error_desc = "invalid_quoted_character";
					p->error = true;
					p->srcptr = (char*)src;
					return false;
				}

				if ((ch == '\\') && p->flags.character_escapes) {
					int ch2 = *src;
					ch = get_escape(&src, &p->error, false);

					if (!p->error) {
						if (ch2 == '\n') {
							//p->line_nbr++;
							continue;
						}
					} else {
						if (DUMP_ERRS || !p->do_read_term)
							fprintf(stdout, "Error: syntax error, illegal character escape <<%s>>, line %d\n", p->srcptr, p->line_nbr);

						p->error_desc = "illegal_character_escape";
						p->error = true;
						p->srcptr = (char*)src;
						return false;
					}
				}

				size_t len = (dst - p->token) + put_len_utf8(ch) + 1;

				if (len >= p->token_size) {
					size_t offset = dst - p->token;
					p->token = realloc(p->token, p->token_size*=2);
					check_error(p->token);
					dst = p->token + offset;
				}

				dst += put_char_utf8(dst, ch);
			}

			*dst = '\0';

			if (p->quote_char && p->fp) {
				if (p->no_fp || getline(&p->save_line, &p->n_line, p->fp) == -1) {
					p->srcptr = "";

					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: syntax error, unterminated quoted atom, line %d\n", p->line_nbr);

					p->error_desc = "unterminated_quoted_atom";
					p->error = true;
					return false;
				}

				src = p->srcptr = p->save_line;
				continue;
			}

			p->toklen = dst - p->token;

			if (!p->string
				&& strcmp(p->token, "[")
				&& strcmp(p->token, "(")
				&& strcmp(p->token, "{")
				&& strcmp(p->token, "]")
				&& strcmp(p->token, ")")
				&& strcmp(p->token, "}"))
			{
				if (p->toklen && contains_null(p->token, p->toklen)) {
					p->quote_char = -1;
				} else if (search_op(p->m, p->token, NULL, false)) {
					p->is_op = true;

					if (!strcmp(p->token, ","))
						p->quote_char = -1;
				} else
					p->quote_char = -1;
			} else
				p->quote_char = -1;

			p->srcptr = (char*)src;
			return true;
		}
	}

	// Atoms...

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
			size_t len = (dst + put_len_utf8(ch) + 1) - p->token;

			if (len >= p->token_size) {
				size_t offset = dst - p->token;
				p->token = realloc(p->token, p->token_size*=2);
				check_error(p->token);
				dst = p->token + offset;
			}

			dst += put_char_utf8(dst, ch);
			ch = peek_char_utf8(src);
		}

		*dst = '\0';

		int ch_start = peek_char_utf8(p->token);

		if (iswupper(ch_start) || (ch_start == '_'))
			p->is_variable = true;
		else if (search_op(p->m, p->token, NULL, false))
			p->is_op = true;

		p->srcptr = (char*)src;
		p->toklen = dst - p->token;
		int ch = peek_char_utf8(src);

		if (!check_space_before_function(p, ch, src))
			return false;

		src = p->srcptr;
		return true;
	}

	ch = get_char_utf8(&src);
	int next_ch = peek_char_utf8(src);

	if ((ch == '.') && iswspace(next_ch)) {
		dst += put_char_utf8(dst, ch);
		p->toklen = dst - p->token;
		p->is_op = search_op(p->m, p->token, NULL, false);
		p->srcptr = (char*)src;
		return true;
	}

	p->srcptr = (char*)src;

	if ((ch == '(') || (ch == '[') || (ch == '{') || (ch == ',')
		|| (ch == '}') || (ch == ']') || (ch == ')')) {
		src = eat_space(p);

		if (!src || !*src) {
			dst += put_char_utf8(dst, ch);
			p->toklen = dst - p->token;
			p->is_op = search_op(p->m, p->token, NULL, false);
			p->srcptr = (char*)src;
			return true;
		}

		next_ch = peek_char_utf8(src);

		if (is_matching_pair(ch, next_ch, '[',']')) {
			strcpy(p->token, "[]");
			p->toklen = 2;
			get_char_utf8(&src);
			p->srcptr = (char*)src;
			int ch = peek_char_utf8(src);

			if (!check_space_before_function(p, ch, src))
				return false;

			src = p->srcptr;
			return true;
		}

		if (is_matching_pair(ch, next_ch, '{','}')) {
			strcpy(p->token, "{}");
			p->toklen = 2;
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
			fprintf(stdout, "Error: syntax error, line %d: '%s'\n", p->line_nbr, p->srcptr);

		p->error_desc = "operator_expected";
		p->error = true;
		p->srcptr = (char*)src;
		return false;
	}

	p->symbol = true;

	do {
		size_t len = (dst + put_len_utf8(ch) + 1) - p->token;

		if (len >= p->token_size) {
			size_t offset = dst - p->token;
			p->token = realloc(p->token, p->token_size*=2);
			check_error(p->token);
			dst = p->token + offset;
		}

		dst += put_char_utf8(dst, ch);

		if (((ch < 256) && strchr(g_solo, ch)) || iswspace(ch))
			break;

		int ch_next = peek_char_utf8(src);

		if (ch_next == '%')
			break;

		if ((ch == '.') && iswspace(ch_next))
			break;

		ch = ch_next;

		if (((ch < 256) && strchr(g_solo, ch)) || iswspace(ch) || iswalnum(ch) || (ch == '_'))
			break;

		ch = get_char_utf8(&src);
	}
	 while (ch);

	p->toklen = dst - p->token;
	p->is_op = search_op(p->m, p->token, NULL, false);
	p->srcptr = (char*)src;

	ch = peek_char_utf8(src);

	if (strcmp(p->token, "(") && !check_space_before_function(p, ch, src))
		return false;

	return true;
}

static bool process_term(parser *p, cell *p1)
{
	directives(p, p1);
	cell *h = get_head(p1);

	if (is_variable(h)) {
		if (DUMP_ERRS || !p->do_read_term)
			printf("Error: instantiation error, line %u\n", p->line_nbr);

		p->error_desc = "instantiation_error";
		p->error = true;
		return false;
	} else if (is_number(h)) {
		if (DUMP_ERRS || !p->do_read_term)
			printf("Error: type error, callable, line %u\n", p->line_nbr);

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

	if (!assertz_to_db(p->m, p->cl->nbr_vars, p->cl->nbr_temporaries, p1, 1)) {
		if (DUMP_ERRS || !p->do_read_term)
			printf("Error: assertion failed '%s', line %u, '%s'\n", p->token, p->line_nbr, p->srcptr);

		p->error = true;
		return false;
	}

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
		int ch = peek_char_utf8(p->token);
		fprintf(stderr,
			"Debug: token '%s' (%d) line_nbr=%d, symbol=%d, quoted=%d, tag=%u, op=%d, lastop=%d, string=%d\n",
			p->token, ch, p->line_nbr, p->symbol, p->quote_char, p->v.tag, p->is_op, last_op, p->string);
#endif

		if (!p->quote_char && !strcmp(p->token, ".")
			&& !iswalpha(*p->srcptr)
		    && (*p->srcptr != '"')
		    && (*p->srcptr != '(')
		    && (*p->srcptr != ',')
		    && (*p->srcptr != ')')
		    && (*p->srcptr != ']')
		    && (*p->srcptr != '|')) {

			if (p->nesting_parens || p->nesting_brackets || p->nesting_braces) {
				if (DUMP_ERRS || !p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, line %u\n", p->line_nbr);

				p->error_desc = "mismatched_parens_or_brackets_or_braces";
				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			if (!p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, incomplete statement, line %u\n", p->line_nbr);

				p->error_desc = "incomplete_statement";
				p->error = true;
				return false;
			}

			if (analyze(p, 0, last_op)) {
				if (p->cl->cells->nbr_cells <= (p->cl->cidx-1)) {
					if (DUMP_ERRS || !p->do_read_term)
						printf("Error: syntax error, operator expected unfinished input '%s', line %u\n", p->token, p->line_nbr);

					p->error_desc = "operator_expected";
					p->error = true;
				}

				term_assign_vars(p, p->read_term, false);
				term_to_body(p);

				if (p->consulting && !p->skip) {
					if (is_variable(p->cl->cells)) {
						if (DUMP_ERRS || !p->do_read_term)
							printf("Error: instantiation error, line %u\n", p->line_nbr);

						p->error_desc = "instantiation_error";
						p->error = true;
						return 0;
					} else if (is_number(p->cl->cells)) {
						if (DUMP_ERRS || !p->do_read_term)
							printf("Error: type error, callable, line %u\n", p->line_nbr);

						p->error_desc = "type_error";
						p->error = true;
						return 0;
					}

					xref_rule(p->m, p->cl, NULL);
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
			(!strcmp(p->token, "[") || !strcmp(p->token, "{"))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, needs operator '%s', line %d\n", p->token, p->line_nbr);

			p->error_desc = "needs_operator";
			p->error = true;
			break;
		}

		if (!p->quote_char && p->last_close && !strcmp(p->token, "(")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, needs operator '%s', line %d\n", p->token, p->line_nbr);

			p->error_desc = "needs_operator";
			p->error = true;
			break;
		}

		if (!p->quote_char && !strcmp(p->token, "[")) {
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

		if (!p->quote_char && !strcmp(p->token, "{")) {
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

		if (!p->quote_char && !strcmp(p->token, "(")) {
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

		if (!p->quote_char && !args && !consing && last_op && !strcmp(p->token, ",")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, quotes needed around operator '%s', line %d\n", p->token, p->line_nbr);

			p->error_desc = "quotes_needed";
			p->error = true;
			break;
		}

		if (!p->quote_char && args && !consing && p->is_op /*&& last_op*/ && strcmp(p->token, ",")) {
			unsigned specifier = 0;
			unsigned priority = search_op(p->m, p->token, &specifier, last_op && !last_postfix);

			if (!last_op && (priority > 999)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, parens needed around operator '%s', line %d\n", p->token, p->line_nbr);

				p->error_desc = "parens_needed";
				p->error = true;
				break;
			}
		}

		if (!p->quote_char && consing && p->is_op && strcmp(p->token, ",") && strcmp(p->token, "|")) {
			unsigned specifier = 0;
			unsigned priority = search_op(p->m, p->token, &specifier, last_op && !last_postfix);

			if (!last_op && (priority > 999)) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, parens needed around operator '%s', line %d\n", p->token, p->line_nbr);

				p->error_desc = "parens_needed";
				p->error = true;
				break;
			}
		}

		if (!p->quote_char && consing && !strcmp(p->token, ",")) {
			if ((arg_idx == p->cl->cidx) || !p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if ((*p->srcptr == ',') && !p->flags.double_quote_codes) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing element '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "missing_element";
				p->error = true;
				break;
			}

			if (p->was_consing || last_op) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, parsing list '%s'\n", p->save_line?p->save_line:"");

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
			((args && !strcmp(p->token, ",")) ||
			(consing && !p->was_consing && !p->start_term && (!strcmp(p->token, ",") || !strcmp(p->token, "|")))
			)) {
			if ((arg_idx == p->cl->cidx) || !p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "args";
				p->error = true;
				break;
			}

			analyze(p, arg_idx, last_op);
			arg_idx = p->cl->cidx;

			if (*p->srcptr == ',') {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if (args) {
				arity++;

				if (arity > MAX_ARITY) {
					if (DUMP_ERRS || !p->do_read_term)
						fprintf(stdout, "Error: max arity reached, line %d\n", p->line_nbr);

					p->error_desc = "max_arity";
					p->error = true;
					break;
				}
			}

			if (consing && !strcmp(p->token, "|")) {
				p->was_consing = last_bar = true;
				//consing = false;
			}

			p->last_close = false;
			last_op = true;
			last_num = false;
			continue;
		}

		if (!p->is_quoted && consing && p->start_term && !strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing list '%s'\n", p->save_line?p->save_line:"");

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->is_quoted && p->was_consing && consing && !strcmp(p->token, "|")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing list '%s'\n", p->save_line?p->save_line:"");

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->is_quoted && p->was_consing && last_bar && !strcmp(p->token, "]")) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, parsing list '%s'\n", p->save_line?p->save_line:"");

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->quote_char && p->start_term &&
			(!strcmp(p->token, "]") || !strcmp(p->token, ")") || !strcmp(p->token, "}"))) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, start of rule expected, line %u\n", p->line_nbr);

			p->error_desc = "start_expected";
			p->error = true;
			break;
		}

		if (!p->quote_char && !strcmp(p->token, ")")) {
			if (arg_idx == p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "args";
				p->error = true;
				break;
			}

			p->last_close = true;
			p->nesting_parens--;
			analyze(p, arg_idx, last_op=false);
			return arity;
		}

		if (!p->quote_char && !strcmp(p->token, "]")) {
			if (arg_idx == p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s'\n", p->save_line?p->save_line:"");

				p->error_desc = "args";
				p->error = true;
				break;
			}

			p->last_close = true;
			p->nesting_brackets--;
			analyze(p, arg_idx, last_op=false);
			return arity;
		}

		if (!p->quote_char && !strcmp(p->token, "}")) {
			if (arg_idx == p->cl->cidx) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, missing arg '%s'\n", p->save_line?p->save_line:"");

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

		if (p->is_variable && (*p->srcptr == '(')) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, variable as functor, line %u\n", p->line_nbr);

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
					fprintf(stdout, "Error: syntax error, incomplete, line %u\n", p->line_nbr);

				p->error_desc = "incomplete";
				p->error = true;
				break;
			}

			int nextch = *s;
			bool noneg = (!strcmp(p->token, "-") || !strcmp(p->token, "+")) && (nextch == '='); // Hack

			if (noneg) {
				if (DUMP_ERRS || !p->do_read_term)
					fprintf(stdout, "Error: syntax error, incomplete, needs parenthesis, line %u\n", p->line_nbr);

				p->error_desc = "incomplete";
				p->error = true;
				break;
			}

			priority = search_op(p->m, p->token, &specifier, last_op && !last_postfix);
		}

		if (!strcmp(p->token, "!") &&
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
					fprintf(stdout, "Error: syntax error, incomplete, line %u\n", p->line_nbr);

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
		printf("*** token=%s, last_op=%d, p->is_op=%d, is_func=%d, prefix=%d\n",
			p->token, last_op, p->is_op, is_func, IS_PREFIX(specifier));
#endif

		if ((p->was_string || p->string) && is_func) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, near \"%s\", expected atom\n", p->token);

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
				fprintf(stdout, "Error: syntax error, near '%s', operator expected postfix '%s'\n", p->token, p->save_line?p->save_line:"");

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		if ((!p->is_op || IS_PREFIX(specifier)) && !is_func && !last_op) {
			if (DUMP_ERRS || !p->do_read_term)
				fprintf(stdout, "Error: syntax error, near '%s', operator expected\n", p->token);

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		last_quoted = p->is_quoted;
		last_op = strcmp(p->token, ")") && priority;
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
		} else if (p->v.tag == TAG_FLOAT) {
			set_float(c, get_float(&p->v));
		} else if ((!p->is_quoted || is_func || p->is_op || p->is_variable
			|| (get_builtin(p->m->pl, p->token, 0, &found, NULL), found)
			//|| !strcmp(p->token, "[]")
			) && !p->string) {

			if (is_func && !strcmp(p->token, "."))
				c->priority = 0;

			if (p->is_variable)
				c->tag = TAG_VAR;

			if (p->is_quoted)
				c->flags |= FLAG_CSTR_QUOTED;

			c->val_off = index_from_pool(p->m->pl, p->token);
			ensure(c->val_off != ERR_IDX);
		} else {
			c->tag = TAG_CSTR;

			if ((p->toklen < MAX_SMALL_STRING) && !p->string) {
				memcpy(c->val_chr, p->token, p->toklen);
				c->val_chr[p->toklen] = '\0';
				c->chr_len = p->toklen;
			} else {
				if (p->string) {
					c->flags |= FLAG_CSTR_STRING;
					c->arity = 2;
				}

				SET_STR(c, p->token, p->toklen, 0);
			}
		}

		last_bar = false;
	}

	p->depth--;
	return !p->error;
}

bool run(parser *p, const char *pSrc, bool dump)
{
	if ((*pSrc == '.') && !pSrc[1]) {
		fprintf(stdout, "Error: syntax error, unexpected end of rule\n");
		return false;
	}

	ASTRING(src);
	ASTRING_sprintf(src, "true,%s", pSrc);
	ASTRING_trim_ws(src);
	ASTRING_trim(src, '.');
	ASTRING_strcat(src, ".");

	p->srcptr = ASTRING_cstr(src);
	bool ok;

	while (p->srcptr && *p->srcptr && !g_tpl_interrupt) {
		reset(p);
		p->line_nbr = 1;
		p->one_shot = true;
		p->consulting = false;

		if (!tokenize(p, false, false))
			break;

		if (!p->error && !p->end_of_term && !p->run_init) {
			fprintf(stdout, "Error: syntax error, missing operand or operator\n");
			p->error = true;
		}

		if (p->error) {
			p->pl->did_dump_vars = true;
			p->srcptr = NULL;
			ASTRING_free(src);
			return false;
		}

		if (p->skip) {
			p->m->pl->status = true;
			p->srcptr = NULL;
			ASTRING_free(src);
			return true;
		}

		if (!analyze(p, 0, true)) {
			p->srcptr = NULL;
			ASTRING_free(src);
			return false;
		}

		term_assign_vars(p, 0, false);
		term_to_body(p);

		xref_rule(p->m, p->cl, NULL);

		if (!p->command)
			term_expansion(p);

		query *q = create_query(p->m, false);

		if (!q) {
			p->srcptr = NULL;
			ASTRING_free(src);
			return false;
		}

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
		destroy_query(q);

		if (!ok)
			break;
	}

	p->srcptr = NULL;
	ASTRING_free(src);
	return ok;
}
