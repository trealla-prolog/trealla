#include <ctype.h>
#include <fenv.h>
#include <float.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

static const unsigned INITIAL_NBR_CELLS = 1000;
const char *g_solo = "!(){}[]|,;`'\"";

char *slicedup(const char *s, size_t n)
{
	char *ptr = malloc(n+1);
	ensure (ptr);
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

cell *list_head(cell *l, cell *tmp)
{
	if (!is_string(l))
		return l + 1;

	const char *src = is_slice(l) ? l->val_str : is_strbuf(l) ? (char*)l->val_strb->cstr + l->strb_off : (char*)l->val_chr + l->val_off;
	tmp->num_cells = 1;
	tmp->flags = 0;
	tmp->arity = 0;

	if (is_codes(l)) {
		tmp->tag = TAG_INT;
		tmp->val_int = peek_char_utf8(src);
	} else {
		size_t char_len = len_char_utf8(src);

		if (char_len <= MAX_SMALL_STRING) {
			tmp->tag = TAG_CSTR;
			memcpy(tmp->val_chr, src, char_len);
			tmp->val_chr[char_len] = '\0';
			tmp->chr_len = char_len;
		} else {
			tmp->tag = TAG_INTERNED;
			tmp->val_off = g_nil_s;
		}
	}

	return tmp;
}

cell *list_tail(cell *l, cell *tmp)
{
	if (!is_string(l)) {
		cell *h = l + 1;
		return h + h->num_cells;
	}

	const char *src = is_slice(l) ? l->val_str : is_strbuf(l) ? (char*)l->val_strb->cstr + l->strb_off : (char*)l->val_chr;
	size_t char_len = len_char_utf8(src);
	size_t str_len = is_slice(l) ? (size_t)l->str_len : is_strbuf(l) ? (size_t)l->val_strb->len - l->strb_off : (unsigned)l->chr_len;

	if (str_len == char_len) {
		tmp->tag = TAG_INTERNED;
		tmp->num_cells = 1;
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

	if (!body->arity && is_interned(body) && (body->val_off == g_true_s))
		return NULL;

	return body;
}

size_t slicecpy(char *dst, size_t dstlen, const char *src, size_t len)
{
	char *save = dst;

	while ((dstlen-1) && len) {
		*dst++ = *src++;
		dstlen--;
		len--;
	}

	*dst = '\0';
	return dst - save;
}

static void *make_string_internal(cell *c, const char *s, size_t n, size_t off)
{
	strbuf *strb = malloc(sizeof(strbuf) + n + 1);
	if (!strb) return NULL;
	memcpy(strb->cstr, s, n);
	strb->cstr[n] = 0;
	strb->len = n;
	strb->refcnt = 1;
	c->val_strb = strb;
	c->strb_off = off;
	c->strb_len = n;
	c->flags |= (FLAG_MANAGED | FLAG_CSTR_BLOB);
	return strb;
}

bool make_cstringn(cell *d, const char *s, size_t n)
{
	if (!n) {
		make_atom(d, g_empty_s);
		return true;
	}

	if (n < MAX_SMALL_STRING) {
		make_smalln(d, s, n);
		return true;
	}

	*d = (cell){0};
	d->tag = TAG_CSTR;
	d->num_cells = 1;
	make_string_internal(d, s, n, 0);
	return true;
}

bool make_stringn(cell *d, const char *s, size_t n)
{
	if (!n) {
		make_atom(d, g_empty_s);
		return true;
	}

	*d = (cell){0};
	d->tag = TAG_CSTR;
	d->flags = FLAG_CSTR_STRING;
	d->num_cells = 1;
	d->arity = 2;
	make_string_internal(d, s, n, 0);
	return true;
}

void make_atom(cell *tmp, pl_idx offset)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INTERNED;
	tmp->num_cells = 1;
	tmp->val_off = offset;
}

cell *make_nil(void)
{
	static cell tmp = {
		.tag = TAG_INTERNED,
		.num_cells = 1,
		.flags = 0,
		.arity = 0,
		.val_off = 0
	};

	tmp.val_off = g_nil_s;
	return &tmp;
}

void make_smalln(cell *tmp, const char *s, size_t n)
{
	*tmp = (cell){0};
	tmp->tag = TAG_CSTR;
	tmp->num_cells = 1;
	memcpy(tmp->val_chr, s, n);
	tmp->val_chr[n] = '\0';
	tmp->chr_len = n;
}

void make_var(cell *tmp, pl_idx off, unsigned var_num)
{
	*tmp = (cell){0};
	tmp->tag = TAG_VAR;
	tmp->flags = FLAG_VAR_LOCAL;
	tmp->num_cells = 1;
	tmp->var_num = var_num;
	tmp->val_off = off;

	if (off == g_anon_s)
		tmp->flags |= FLAG_VAR_ANON;
}

void make_float(cell *tmp, pl_flt v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_FLOAT;
	tmp->num_cells = 1;
	tmp->val_float = v;
}

void make_int(cell *tmp, pl_int v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INT;
	tmp->num_cells = 1;
	set_smallint(tmp, v);
}

void make_uint(cell *tmp, pl_uint v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INT;
	tmp->num_cells = 1;
	set_smalluint(tmp, v);
}

void make_ptr(cell *tmp, void *v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INT;
	tmp->num_cells = 1;
	tmp->val_ptr = v;
}

void make_struct(cell *tmp, pl_idx offset, unsigned arity, pl_idx extra_cells)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INTERNED;
	tmp->num_cells = 1 + extra_cells;
	tmp->arity = arity;
	tmp->val_off = offset;
}

void make_end(cell *tmp)
{
	*tmp = (cell){0};
	tmp->tag = TAG_END;
	tmp->num_cells = 1;
}

void make_blob(cell *tmp, void *ptr)
{
	*tmp = (cell){0};
	tmp->tag = TAG_BLOB;
	tmp->flags = FLAG_MANAGED;
	tmp->num_cells = 1;
	tmp->val_blob = ptr;
	tmp->val_blob->refcnt = 0;
}

void make_dbref(cell *tmp, void *ptr)
{
	*tmp = (cell){0};
	tmp->tag = TAG_DBID;
	tmp->flags = FLAG_MANAGED;
	tmp->num_cells = 1;
	tmp->val_blob = ptr;
	tmp->val_blob->refcnt = 0;
}

void make_kvref(cell *tmp, void *ptr)
{
	*tmp = (cell){0};
	tmp->tag = TAG_KVID;
	tmp->flags = FLAG_MANAGED;
	tmp->num_cells = 1;
	tmp->val_blob = ptr;
	tmp->val_blob->refcnt = 0;
}

void share_cells(cell *src, pl_idx num_cells)
{
	for (pl_idx i = 0; i < num_cells; i++, src++)
		share_cell(src);
}

void unshare_cells(cell *src, pl_idx num_cells)
{
	for (pl_idx i = 0; i < num_cells; i++, src++)
		unshare_cell(src);
}


void clear_clause(clause *cl)
{
	unshare_cells(cl->cells, cl->cidx);
	free(cl->alt);
	cl->alt = NULL;
	cl->num_vars = 0;
	cl->cidx = 0;
}

static bool make_room(parser *p, unsigned num)
{
	if ((p->cl->cidx+num) >= p->cl->num_allocated_cells) {
		pl_idx num_cells = (p->cl->num_allocated_cells + num) * 3 / 2;

		clause *cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*num_cells));
		ensure(cl);
		p->cl = cl;
		p->cl->num_allocated_cells = num_cells;
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

void parser_reset(parser *p)
{
	p->was_consing = p->was_string = p->was_partial = p->did_getline = false;
	p->already_loaded_error = p->do_read_term = p->internal = p->one_shot = false;
	p->start_term = p->end_of_term = p->end_of_file = p->is_directive  = false;
	p->is_command = p->is_comment = p->is_consulting = p->is_symbol = false;
	p->is_string = p->is_quoted = p->is_var = p->is_op = p->skip = p->last_close = false;
	p->last_neg = p->no_fp = p->reuse= p->in_body = false;
	p->is_number_chars = false;

	SB_init(p->token);
	memset(&p->vartab, 0, sizeof(p->vartab));
	p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
	p->num_vars = 0;
	p->start_term = true;
	p->error = false;
	p->dq_consing = 0;
	p->error_desc = NULL;
	p->cl->cidx = 0;
	p->flags = p->m->flags;
}

void parser_destroy(parser *p)
{
	if (!p) return;
	SB_free(p->token);
	free(p->save_line);

	if (p->cl) {
		clear_clause(p->cl);
		free(p->cl);
	}

	free(p);
}

parser *parser_create(module *m)
{
	parser *p = calloc(1, sizeof(parser));
	ensure(p);
	p->pl = m->pl;
	p->m = m;
	pl_idx num_cells = INITIAL_NBR_CELLS;
	p->cl = calloc(1, sizeof(clause)+(sizeof(cell)*num_cells));
	ensure(p->cl, free(p));
	p->cl->num_allocated_cells = num_cells;
	p->start_term = true;
	p->flags = m->flags;
	p->line_num = 1;
	return p;
}

static void consultall(parser *p, cell *l)
{
	LIST_HANDLER(l);

	while (is_list(l)) {
		cell *h = LIST_HEAD(l);

		if (is_iso_list(h))
			consultall(p, h);
		else {
			char *s = C_STR(p, h);

			if (!load_file(p->m, s, false, true))
				fprintf(stderr, "Error: file not found: '%s'\n", s);
		}

		l = LIST_TAIL(l);
	}
}

char *relative_to(const char *basefile, const char *relfile)
{
	char *tmpbuf = malloc(strlen(basefile) + strlen(relfile) + 256);
	ensure(tmpbuf);
	char *ptr = tmpbuf;

	if (!strncmp(relfile, "../", 3) || !strchr(relfile, '/')) {
		strcpy(tmpbuf, basefile);
		ptr = tmpbuf + strlen(tmpbuf) - 1;

		while ((ptr != tmpbuf) && (*ptr != '/'))
			ptr--;

		if (ptr != tmpbuf)
			*ptr++ = '/';

		*ptr = '\0';
	}

	strcpy(ptr, relfile);
	return tmpbuf;
}

static void do_op(parser *p, cell *c, bool make_public)
{
	cell *p1 = c + 1, *p2 = c + 2, *p3 = c + 3;

	if (!is_integer(p1) || !is_interned(p2) || (!is_atom(p3) && !is_list(p3))) {
		if (!p->do_read_term)
			fprintf(stderr, "Error: unknown op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error = true;
		return;
	}

	unsigned specifier;
	char *spec = DUP_STRING(p, p2);

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
		if (!p->do_read_term)
			fprintf(stderr, "Error: unknown op spec tag, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		free(spec);
		return;
	}

	free(spec);
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);

		if (is_atom(h)) {
			char *name = DUP_STRING(p, h);

			unsigned tmp_optype = 0;
			unsigned tmp_pri = match_op(p->m, name, &tmp_optype, p3->arity);

			if (IS_INFIX(specifier) && IS_POSTFIX(tmp_optype) && (true || p->m->flags.strict_iso)) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: permission error set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				free(name);
				return;
			}

			if (IS_POSTFIX(specifier) && IS_INFIX(tmp_optype) && (true || p->m->flags.strict_iso)) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: permission error set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				free(name);
				return;
			}

			if (!set_op(p->m, name, specifier, get_smallint(p1))) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: could not set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				free(name);
				continue;
			}

			if (make_public) {
				if (!set_op(p->pl->user_m, name, specifier, get_smallint(p1))) {
					if (!p->do_read_term)
						fprintf(stderr, "Error: could not set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

					free(name);
					continue;
				}
			}

			free(name);
		}

		p3 = LIST_TAIL(p3);
	}

	if (is_atom(p3) && !is_nil(p3)) {
		char *name = DUP_STRING(p, p3);
		unsigned tmp_optype = 0;
		unsigned tmp_pri = match_op(p->m, name, &tmp_optype, p3->arity);

		if (IS_INFIX(specifier) && IS_POSTFIX(tmp_optype) && (true || p->m->flags.strict_iso)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: permission error set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			free(name);
			return;
		}

		if (IS_POSTFIX(specifier) && IS_INFIX(tmp_optype) && (true || p->m->flags.strict_iso)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: permission error set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			free(name);
			return;
		}

		if (!set_op(p->m, name, specifier, get_smallint(p1))) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: could not set op, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			free(name);
			return;
		}

		if (make_public) {
			if (!set_op(p->pl->user_m, name, specifier, get_smallint(p1))) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: could not set op, %s:%u\n", get_loaded(p->m, p->m->filename), p->line_num);

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

	if ((goal->val_off == g_goal_expansion_s) && (goal->arity == 2))
		return false;

	if (goal->val_off == g_cut_s)
		return false;

	query *q = query_create(p->m);
	execute(q, goal, p->cl->num_vars);

	if (q->retry != QUERY_OK) {
		query_destroy(q);
		return false;
	}

	query_destroy(q);
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

	if (!strcmp(dirname, "if") && (c->arity == 1) && !p->m->ifs_done[p->m->if_depth] && !p->m->ifs_blocked[p->m->if_depth]) {
		bool ok = goal_run(p, FIRST_ARG(c));
		p->m->ifs_blocked[++p->m->if_depth] = !ok;
		p->m->ifs_done[p->m->if_depth] = ok;
		return true;
	}

	if (!strcmp(dirname, "if") && (c->arity == 1)) {
		bool save1 = p->m->ifs_blocked[p->m->if_depth];
		p->m->ifs_blocked[++p->m->if_depth] = save1;
		p->m->ifs_done[p->m->if_depth] = true;
		return true;
	}

	if (!strcmp(dirname, "elif") && (c->arity == 1) && !p->m->ifs_done[p->m->if_depth] && p->m->ifs_blocked[p->m->if_depth]) {
		bool ok = goal_run(p, FIRST_ARG(c));
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

	if (!strcmp(dirname, "endif") && (c->arity == 0)) {
		--p->m->if_depth;
		return true;
	}

	return false;
}

static bool directives(parser *p, cell *d)
{
	p->skip = false;

	if (!is_interned(d))
		return false;

	if (is_list(d) && p->is_command) {
		consultall(p, d);
		p->skip = true;
		return false;
	}

	if (strcmp(C_STR(p, d), ":-"))
		return false;

	cell *c = d + 1;

	if (!is_interned(c))
		return false;

	const char *dirname = C_STR(p, c);

	if (d->arity != 1)
		return false;

	if (is_list(c)) {
		printf("WARNING: directive to load '%s' not allowed\n", C_STR(p, c+1));
		p->error = true;
		return false;
	}

	d->val_off = new_atom(p->pl, "$directive");
	CLR_OP(d);

	if (!strcmp(dirname, "initialization") && (c->arity == 1)) {
		p->m->run_init = true;
		return false;
	}

	if (!strcmp(dirname, "info") && (c->arity == 1)) {
		printf("INFO: %s\n", C_STR(p, FIRST_ARG(c)));
		return true;
	}

	cell *p1 = c + 1;

	if (!strcmp(dirname, "help") && (c->arity == 2)) {
		if (!is_compound(p1)) return true;
		cell *p2 = p1 + p1->num_cells;
		if (!is_iso_list_or_nil(p2)) return true;
		LIST_HANDLER(p2);
		char *desc = NULL;
		bool iso = false;

		while (is_iso_list(p2)) {
			cell *h = LIST_HEAD(p2);

			if (is_compound(h) && is_atom(h+1) && !strcmp(C_STR(p, h), "iso")) {
				cell *arg = h + 1;
				iso = !strcmp(C_STR(p, arg), "true");

				if (iso) {
					predicate *pr = find_predicate(p->m, p1);

					if (pr)
						pr->is_iso = true;
				}
			}

			if (is_compound(h) && is_atom(h+1) && !strcmp(C_STR(p, h), "desc")) {
				cell *arg = h + 1;
				desc = DUP_STRING(p, arg);
			}

			p2 = LIST_TAIL(p2);
		}

		pl_ctx p1_ctx = 0;
		query q = (query){0};
		q.pl = p->pl;
		q.st.m = p->m;
		char *dst = print_term_to_strbuf(&q, p1, p1_ctx, 0);
		builtins *ptr = calloc(1, sizeof(builtins));
		ensure(ptr);
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
		sl_app(p->pl->help, ptr->name, ptr);

		if (ptr->iso)
			push_property(p->m, ptr->name, ptr->arity, "iso");

		push_template(p->m, ptr->name, ptr->arity, ptr);
		return true;
	}

	if (!strcmp(dirname, "det") && (c->arity == 1)) {
		printf("WARNING: %s\n", dirname);
		return true;
	}

	if (!strcmp(dirname, "include") && (c->arity == 1)) {
		if (!is_atom(p1)) return true;
		unsigned save_line_nbr = p->line_num;
		const char *name = C_STR(p, p1);
		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename, true, false)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: not found: %s:%d\n", filename, p->line_num);

			free(filename);
			p->line_num = save_line_nbr;
			p->error = true;
			return true;
		}

		set_parent(p->m, p->m->actual_filename, p->m->filename);
		free(filename);
		p->line_num = save_line_nbr;
		return true;
	}

	if (!strcmp(dirname, "ensure_loaded") && (c->arity == 1)) {
		if (!is_atom(p1)) return true;
		unsigned save_line_nbr = p->line_num;
		const char *name = C_STR(p, p1);
		char *filename = relative_to(p->m->filename, name);

		if (!load_file(p->m, filename, false, false)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: not found: %s:%d\n", filename, p->line_num);

			free(filename);
			p->line_num = save_line_nbr;
			p->error = true;
			return true;
		}

		free(filename);
		p->line_num = save_line_nbr;
		return true;
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
			if (!p->do_read_term)
				fprintf(stderr, "Error: pragma name not an atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error = true;
			return true;
		} else
			name = C_STR(p, p1);

		module *tmp_m;

		if ((tmp_m = find_module(p->pl, name)) != NULL) {
			//if (!p->do_read_term)
			//	fprintf(stderr, "Error: module already loaded: %s, %s:%d\n", name, get_loaded(p->m, p->m->filename), p->line_num);
			//
			p->already_loaded_error = true;
			p->m = tmp_m;
			return true;
		}

		tmp_m = module_create(p->pl, name);
		if (!tmp_m) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: module creation failed: %s, %s:%d\n", name, get_loaded(p->m, p->m->filename), p->line_num);

			p->error = true;
			return true;
		}

		if (tmp_m != p->m)
			p->m->used[p->m->idx_used++] = tmp_m;

		LIST_HANDLER(p2);

		while (is_iso_list(p2)) {
			LIST_HEAD(p2);
			p2 = LIST_TAIL(p2);
		}

		return true;
	}

	if (!strcmp(dirname, "attribute") && (c->arity == 1)) {
		cell *arg = c + 1;

		if (arg->val_off == g_slash_s) {
			cell *f = arg;
			char *name = C_STR(p->m, f+1);
			unsigned arity = get_smallint(f+2);
			module_duplicate(p->pl, p->m, name, arity);
			return true;
		}

		while (arg->val_off == g_conjunction_s) {
			cell *f = arg + 1;

			if ((!is_compound(f)) || (f->val_off != g_slash_s))
				break;

			char *name = C_STR(p->m, f+1);
			unsigned arity = get_smallint(f+2);
			module_duplicate(p->pl, p->m, name, arity);
			arg += 4;
		}

		cell *f = arg;

		if ((!is_compound(f)) || (f->val_off != g_slash_s))
			return true;

		char *name = C_STR(p->m, f+1);
		unsigned arity = get_smallint(f+2);
		module_duplicate(p->pl, p->m, name, arity);
		return true;
	}

	if (!strcmp(dirname, "module") && (c->arity >= 1)) {
		module *save_m = p->m;
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
			if (!p->do_read_term)
				fprintf(stderr, "Error: module name not an atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error = true;
			return true;
		} else
			name = C_STR(p, p1);

		if (!p->m->make) {
			module *tmp_m;

			if ((tmp_m = find_module(p->pl, name)) != NULL) {
				//if (!p->do_read_term)
				//	fprintf(stderr, "Error: module already loaded: %s, %s:%d\n", name, get_loaded(p->m, p->m->filename), p->line_num);
				//
				p->already_loaded_error = true;
				p->m = tmp_m;
				return true;
			}

			tmp_m = module_create(p->pl, name);

			if (!tmp_m) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: module creation failed: %s, %s:%d\n", name, get_loaded(p->m, p->m->filename), p->line_num);

				p->error = true;
				return true;
			}

			if (tmp_m != p->m)
				p->m->used[p->m->idx_used++] = tmp_m;

			p->m = tmp_m;
		}

		if (c->arity == 1)
			return true;

		cell *p2 = c + 2;
		LIST_HANDLER(p2);

		while (is_iso_list(p2)) {
			cell *head = LIST_HEAD(p2);

			if (is_compound(head)) {
				if (!strcmp(C_STR(p, head), "/")
					|| !strcmp(C_STR(p, head), "//")) {
					cell *f = head+1, *a = f+1;
					if (!is_interned(f)) return true;
					if (!is_integer(a)) return true;
					cell tmp = *f;
					tmp.arity = get_smallint(a);

					if (!strcmp(C_STR(p, head), "//"))
						tmp.arity += 2;

					predicate *pr = find_predicate(p->m, &tmp);
					if (!pr) pr = create_predicate(p->m, &tmp, NULL);

					if (!pr) {
						module_destroy(p->m);
						p->m = NULL;
						if (!p->do_read_term)
							fprintf(stderr, "Error: predicate creation failed, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

						p->error = true;
						return true;
					}

					pr->is_public = true;
				} else if (!strcmp(C_STR(p, head), "op") && (head->arity == 3)) {
					do_op(p, head, true);
				} else {
					if (!p->do_read_term)
						fprintf(stderr, "Error: predicate export failed, '%s' in %s:%d\n", C_STR(p, head), get_loaded(p->m, p->m->filename), p->line_num);

					p->error = true;
					return true;
				}
			}

			p2 = LIST_TAIL(p2);
		}

		return true;
	}

	if ((!strcmp(dirname, "use_module") || !strcmp(dirname, "autoload") || !strcmp(dirname, "reexport")) && (c->arity >= 1)) {
		if (!is_callable(p1))
			return true;

		c->arity == 1 ? do_use_module_1(p->m, c) : do_use_module_2(p->m, c);
		return true;
	}

#if USE_FFI
	if (!strcmp(dirname, "foreign_struct") && (c->arity == 2)) {
		if (!is_iso_atom(p1)) {
			p->error = true;
			return true;
		}

		do_foreign_struct(p->m, c);
		return true;
	}

	if (!strcmp(dirname, "use_foreign_module") && (c->arity == 2)) {
		if (!is_atom(p1)) {
			p->error = true;
			return true;
		}

		if (!do_use_foreign_module(p->m, c)) {
			p->error = true;
			return true;
		}

		return true;
	}
#endif

	if (!strcmp(dirname, "meta_predicate") && (c->arity == 1)) {
		if (!is_compound(p1))
			return true;
	}

	if (!strcmp(dirname, "set_prolog_flag") && (c->arity == 2)) {
		cell *p2 = c + 2;

		if (!is_interned(p2))
			return true;

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
				if (!p->do_read_term)
					fprintf(stderr, "Error: unknown value, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error = true;
				return true;
			}
		} else if (!strcmp(C_STR(p, p1), "character_escapes")) {
			if (!strcmp(C_STR(p, p2), "true") || !strcmp(C_STR(p, p2), "on"))
				p->m->flags.character_escapes = true;
			else if (!strcmp(C_STR(p, p2), "false") || !strcmp(C_STR(p, p2), "off"))
				p->m->flags.character_escapes = false;
		} else if (!strcmp(C_STR(p, p1), "occurs_check")) {
			if (!strcmp(C_STR(p, p2), "true") || !strcmp(C_STR(p, p2), "on"))
				p->m->flags.occurs_check = true;
			else if (!strcmp(C_STR(p, p2), "false") || !strcmp(C_STR(p, p2), "off"))
				p->m->flags.occurs_check = false;
		} else if (!strcmp(C_STR(p, p1), "strict_iso")) {
			if (!strcmp(C_STR(p, p2), "true") || !strcmp(C_STR(p, p2), "on"))
				p->m->flags.strict_iso = true;
			else if (!strcmp(C_STR(p, p2), "false") || !strcmp(C_STR(p, p2), "off"))
				p->m->flags.strict_iso = false;
		} else {
			//fprintf(stderr, "Warning: unknown flag: %s\n", C_STR(p, p1));
		}

		p->flags = p->m->flags;
		return true;
	}

	if (!strcmp(dirname, "op") && (c->arity == 3)) {
		do_op(p, c, false);
		return true;
	}

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);

		if (is_interned(h) && (!strcmp(C_STR(p, h), "/") || !strcmp(C_STR(p, h), "//")) && (h->arity == 2)) {
			cell *c_name = h + 1;

			if (is_var(c_name)) {
				if (((!p->do_read_term)) && !p->pl->quiet)
					fprintf(stderr, "Error: uninstantiated: %s/%d\n", dirname, c->arity);

				p->error = true;
				return true;
			}

			if (!is_atom(c_name)) {
				fprintf(stderr, "Error: predicate-indicator %s, %s:%d\n", p->m->name, get_loaded(p->m, p->m->filename), p->line_num);
				p->error = true;
				return true;
			}

			cell *c_arity = h + 2;

			if (!is_integer(c_arity)) {
				fprintf(stderr, "Error: predicate-indicator %s, %s:%d\n", p->m->name, get_loaded(p->m, p->m->filename), p->line_num);
				p->error = true;
				return true;
			}

			unsigned arity = get_smallint(c_arity);

			if (!strcmp(C_STR(p, h), "//"))
				arity += 2;

			cell tmp = *c_name;
			tmp.arity = arity;

			if (!strcmp(dirname, "dynamic")) {
				predicate * pr = find_predicate(p->m, &tmp);

				if (pr && !pr->is_dynamic && pr->head) {
					if (!p->do_read_term)
						fprintf(stderr, "Error: no permission to modify static predicate %s:%s/%u, %s:%d\n", p->m->name, C_STR(p->m, c_name), arity, get_loaded(p->m, p->m->filename), p->line_num);

					p->error = true;
					return true;
				}

				set_dynamic_in_db(p->m, C_STR(p, c_name), arity);
				p->error = p->m->error;
			} else if (!strcmp(dirname, "encoding")) {
			} else if (!strcmp(dirname, "public")) {
			} else if (!strcmp(dirname, "export")) {
			} else if (!strcmp(dirname, "discontiguous")) {
				set_discontiguous_in_db(p->m, C_STR(p, c_name), arity);
				p->error = p->m->error;
			} else if (!strcmp(dirname, "multifile")) {
				const char *src = C_STR(p, c_name);

				if (strcmp(src, ":")) {
					set_multifile_in_db(p->m, src, arity);
					p->error = p->m->error;
				} else {
					// multifile(:(mod,/(name,arity)))
					cell *c_mod = c_name + 1;				// FIXME: verify
					cell *c_slash = c_name + 2;				// FIXME: verify
					cell *c_functor = c_slash + 1;			// FIXME: verify
					cell *c_arity = c_slash + 2;			// FIXME: verify
					const char *mod = C_STR(p, c_mod);
					const char *name = C_STR(p, c_functor);
					arity = get_smalluint(c_arity);

					if (!strcmp(C_STR(p, c_slash), "//"))
						arity += 2;

					if (!is_multifile_in_db(p->pl, mod, name, arity)) {
						if (!p->do_read_term)
							fprintf(stderr, "Error: not multifile %s:%s/%u\n", mod, name, arity);

						p->error = true;
						return true;
					}
				}
			} else {
				if (((!p->do_read_term)) && !p->pl->quiet)
					fprintf(stderr, "Error: unknown directive: %s/%d\n", dirname, c->arity);

				p->error = true;
				return true;
			}
		}

		p1 = LIST_TAIL(p1);
	}

	if (is_nil(p1))
		return true;

	if (is_var(p1)) {
		if (((!p->do_read_term)) && !p->pl->quiet)
			fprintf(stderr, "Error: uninstantiated: %s/%d\n", dirname, c->arity);

		p->error = true;
		return true;
	}

	while (is_interned(p1) && (p1->val_off != g_dot_s)) {
		module *m = p->m;
		cell *c_id = p1;

		if (!strcmp(C_STR(p, p1), ":") && (p1->arity == 2)) {
			cell *c_mod = p1 + 1;

			if (!is_atom(c_mod))
				return true;

			m = find_module(p->pl, C_STR(p, c_mod));

			if (!m)
				m = module_create(p->pl, C_STR(p, c_mod));

			c_id = p1 + 2;
		}

		if ((!strcmp(C_STR(p, c_id), "/") || !strcmp(C_STR(p, c_id), "//"))
			&& (p1->arity == 2)) {
			cell *c_name = c_id + 1;

			if (is_var(c_name)) {
				if (((!p->do_read_term)) && !p->pl->quiet)
					fprintf(stderr, "Error: uninstantiated: %s/%d\n", dirname, c->arity);

				p->error = true;
				return true;
			}

			if (!is_atom(c_name)) {
				fprintf(stderr, "Error: predicate-indicator %s, %s:%d\n", p->m->name, get_loaded(p->m, p->m->filename), p->line_num);
				p->error = true;
				return true;
			}

			cell *c_arity = c_id + 2;

			if (!is_integer(c_arity)) {
				fprintf(stderr, "Error: predicate-indicator %s, %s:%d\n", p->m->name, get_loaded(p->m, p->m->filename), p->line_num);
				p->error = true;
				return true;
			}

			unsigned arity = get_smallint(c_arity);
			cell tmp = *c_name;
			tmp.arity = arity;


			if (!strcmp(C_STR(p, c_id), "//"))
				arity += 2;

			if (!strcmp(dirname, "multifile")) {
				set_multifile_in_db(m, C_STR(p, c_name), arity);
				p->error = m->error;
			} else if (!strcmp(dirname, "discontiguous")) {
				set_discontiguous_in_db(m, C_STR(p, c_name), arity);
				p->error = m->error;
			} else if (!strcmp(dirname, "public"))
				;
			else if (!strcmp(dirname, "export"))
				;
			else if (!strcmp(dirname, "dynamic")) {
				predicate * pr = find_predicate(p->m, &tmp);

				if (pr && !pr->is_dynamic && pr->head) {
					if (!p->do_read_term)
						fprintf(stderr, "Error: no permission to modify static predicate %s:%s/%u, %s:%d\n", m->name, C_STR(p->m, c_name), arity, get_loaded(p->m, p->m->filename), p->line_num);

					p->error = true;
					return true;
				}

				set_dynamic_in_db(m, C_STR(p, c_name), arity);
				p->error = m->error;
			} else {
				if (((!p->do_read_term)) && !p->pl->quiet)
					fprintf(stderr, "Error: unknown directive: %s/%d\n", dirname, c->arity);

				p->error = true;
				return true;
			}

			p1 += p1->num_cells;
		} else if (!strcmp(dirname, "create_prolog_flag")) {
			p1 += 1;
		} else if (!strcmp(dirname, "encoding")) {
			p1 += 1;
		} else if (!strcmp(dirname, "meta_predicate")) {
			if (p1->val_off == g_conjunction_s)
				p1 += 1;

			set_meta_predicate_in_db(m, p1);
			p->error = m->error;
			p1 += p1->num_cells;
		} else if (!strcmp(C_STR(p, p1), ",") && (p1->arity == 2))
			p1 += 1;
		else {
			if (((!p->do_read_term)) && !p->pl->quiet)
				fprintf(stderr, "Error: unknown directive2: %s/%d\n", dirname, c->arity);

			p->error = true;
			return true;
			p1 += 1;
		}
	}

	return true;
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

static pl_idx get_varno(parser *p, const char *src, bool in_body, unsigned depth)
{
	int anon = !strcmp(src, "_");
	size_t offset = 0;
	unsigned i = 0, nesting_offset = p->is_consulting ? 0 : 2;

	while (p->vartab.pool[offset]) {
		if (!strcmp(p->vartab.pool+offset, src) && !anon) {
			if (in_body)
				p->vartab.in_body[i]++;
			else
				p->vartab.in_head[i]++;

			if (depth > p->vartab.depth[i])
				p->vartab.depth[i] = depth - nesting_offset;

			return i;
		}

		offset += strlen(p->vartab.pool+offset) + 1;
		i++;
	}

	size_t len = strlen(src);

	if ((offset+len+1) >= MAX_VAR_POOL_SIZE) {
		fprintf(stderr, "Error: var pool exhausted, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);
		p->error = true;
		return 0;
	}

	memcpy(p->vartab.pool+offset, src, len+1);

	if (in_body)
		p->vartab.in_body[i]++;
	else
		p->vartab.in_head[i]++;

	p->vartab.depth[i] = depth - nesting_offset;
	p->vartab.num_vars++;
	return i;
}

static unsigned get_in_head(parser *p, const char *name)
{
	bool anon = !strcmp(name, "_");
	size_t offset = 0;
	unsigned i = 0;

	while (p->vartab.pool[offset]) {
		if (!strcmp(p->vartab.pool+offset, name) && !anon) {
			return p->vartab.in_head[i];
		}

		offset += strlen(p->vartab.pool+offset) + 1;
		i++;
	}

	return 0;
}

static unsigned get_in_body(parser *p, const char *name)
{
	bool anon = !strcmp(name, "_");
	size_t offset = 0;
	unsigned i = 0;

	while (p->vartab.pool[offset]) {
		if (!strcmp(p->vartab.pool+offset, name) && !anon) {
			return p->vartab.in_body[i];
		}

		offset += strlen(p->vartab.pool+offset) + 1;
		i++;
	}

	return 0;
}

void assign_vars(parser *p, unsigned start, bool rebase)
{
	if (!p || p->error)
		return;

	clause *cl = p->cl;
	cl->is_first_cut = false;
	cl->is_cut_only = false;
	p->start_term = true;

	if (!p->reuse) {
		memset(&p->vartab, 0, sizeof(p->vartab));
		cl->num_vars = 0;
		p->num_vars = 0;
	}

	// Assign body variables first (why?)...

	const cell *body = get_body(cl->cells);
	bool in_body = p->in_body;

	for (unsigned i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;

		if (c == body)
			in_body = true;

		if (!in_body)
			continue;

		if (!is_var(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "___V%u", c->var_num);
			c->var_num = get_varno(p, tmpbuf, in_body, c->var_num);
		} else
			c->var_num = get_varno(p, C_STR(p, c), in_body, c->var_num);

		c->var_num += start;

		if (c->var_num == MAX_VARS) {
			fprintf(stderr, "Error: max vars reached, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);
			p->error = true;
			return;
		}

		p->vartab.off[c->var_num] = c->val_off;
		p->vartab.used[c->var_num]++;
	}

	// ... then the head...

	in_body = p->in_body;

	for (unsigned i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;

		if (c == body)
			in_body = true;

		if (in_body)
			break;

		if (!is_var(c))
			continue;

		if (rebase) {
			char tmpbuf[20];
			snprintf(tmpbuf, sizeof(tmpbuf), "___V%u", c->var_num);
			c->var_num = get_varno(p, tmpbuf, in_body, c->var_num);
		} else
			c->var_num = get_varno(p, C_STR(p, c), in_body, c->var_num);

		c->var_num += start;

		if (c->var_num == MAX_VARS) {
			fprintf(stderr, "Error: max vars reached, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);
			p->error = true;
			return;
		}

		p->vartab.off[c->var_num] = c->val_off;
		p->vartab.used[c->var_num]++;
	}

	cl->num_vars = p->vartab.num_vars;
	p->num_vars = p->vartab.num_vars;

	// Now set flags...

	for (unsigned i = 0; i < cl->cidx; i++) {
		cell *c = cl->cells + i;

		if (!is_var(c))
			continue;

		c->flags &= ~FLAG_VAR_REF;

		if (c->val_off == g_anon_s)
			c->flags |= FLAG_VAR_ANON;

		unsigned var_in_head = get_in_head(p, C_STR(p, c));
		unsigned var_in_body = get_in_body(p, C_STR(p, c));
		unsigned occurrances = var_in_head + var_in_body;
		bool var_is_global = is_global(c);

		if (var_in_head && (p->vartab.depth[c->var_num] > 1)) {
			var_is_global = true;
		} else if (var_in_body && (p->vartab.depth[c->var_num] > 1)) {
			var_is_global = true;
		}

		if (!occurrances)		// Anonymous vars weren't
			occurrances = 1;	// counted it seems

		if (var_is_global) {
			c->flags |= FLAG_VAR_GLOBAL;
		} else {
			if (occurrances == 1)
				c->flags |= FLAG_VAR_VOID;
			else if (!var_in_body)
				c->flags |= FLAG_VAR_TEMPORARY;
			else if (var_in_body)
				c->flags |= FLAG_VAR_LOCAL;
		}
	}

	for (unsigned i = 0; i < cl->num_vars; i++) {
		if (p->is_consulting && !p->do_read_term && (p->vartab.used[i] == 1)
			// && (p->vartab.name[i][strlen(p->vartab.name[i])-1] != '_')
			&& (GET_POOL(p, p->vartab.off[i])[0] != '_')) {
			if (!p->pl->quiet
				&& !((cl->cells->val_off == g_neck_s) && cl->cells->arity == 1))
				fprintf(stderr, "Warning: singleton: %s, near %s:%d\n", GET_POOL(p, p->vartab.off[i]), get_loaded(p->m, p->m->filename), p->line_num);
		}
	}

	cell *c = make_a_cell(p);
	ensure(c);
	c->tag = TAG_END;
	c->num_cells = 1;
}

// Reduce a vector of cells in token order to a parse tree. This is
// done in two passes: first find the lowest priority un-applied
// operator then apply args to that operator.

static bool reduce(parser *p, pl_idx start_idx, bool last_op)
{
	pl_idx lowest = IDX_MAX, work_idx, end_idx = p->cl->cidx - 1;
	bool do_work = false, bind_le = false;

	for (pl_idx i = start_idx; i < p->cl->cidx;) {
		cell *c = p->cl->cells + i;

		if ((c->num_cells > 1) || !is_interned(c) || !c->priority) {
			i += c->num_cells;
			continue;
		}

#if 0
		if (!p->is_consulting)
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

		bind_le = is_xfy(c) || is_fy(c) ? true || is_yf(c): false;
		i++;
	}

	if (!do_work)
		return false;

	pl_idx last_idx = IDX_MAX;

	for (pl_idx i = start_idx; i <= end_idx;) {
		cell *c = p->cl->cells + i;

		if ((c->num_cells > 1) || !is_interned(c) || !c->priority) {
			last_idx = i;
			i += c->num_cells;
			continue;
		}

		if ((c->priority != lowest) || (i != work_idx)) {
			last_idx = i;
			i += c->num_cells;
			continue;
		}

#if 0
		if (!p->is_consulting)
			printf("*** OP2 last=%u/start=%u '%s' type=%u, specifier=%u, pri=%u, last_op=%d, is_op=%d\n", last_idx, start_idx, C_STR(p, c), c->tag, GET_OP(c), c->priority, last_op, IS_OP(c));
#endif

		c->tag = TAG_INTERNED;
		c->arity = 1;

		// Prefix...

		if (is_fx(c)) {
			const cell *rhs = c + 1;

			if (is_fx(rhs) && !rhs->arity && (rhs->priority == c->priority)) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}

			rhs += rhs->num_cells;

			if ((((pl_idx)(rhs - p->cl->cells)) < end_idx)
				&& is_xf(rhs) && (rhs->priority == c->priority)) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		if (is_prefix(c)) {
			cell *rhs = c + 1;

			if (is_infix(rhs) && !rhs->arity && (rhs->priority > c->priority)) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}

			if (is_prefix(rhs) && !rhs->arity && (rhs->priority > c->priority)) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		if (is_prefix(c)) {
			const cell *rhs = c + 1;
			pl_idx off = (pl_idx)(rhs - p->cl->cells);

			if (off > end_idx) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing operand to prefix, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "operand_missing";
				p->error = true;
				return false;
			}

			c->num_cells += rhs->num_cells;
			break;
		}

		// Postfix...

		cell *rhs = c + 1;

		if (is_xf(rhs) && (rhs->priority == c->priority)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		if (is_prefix(rhs) && !rhs->arity && (rhs->priority > c->priority)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		cell save = *c;

		if (is_postfix(c)) {
			pl_idx off = last_idx;

			if (off > end_idx) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing operand to postfix, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "operand_missing";
				p->error = true;
				return false;
			}

			cell *lhs = p->cl->cells + last_idx;
			save.num_cells += lhs->num_cells;
			pl_idx cells_to_move = lhs->num_cells;
			cell *save_c = lhs;
			const cell *src = c - 1;
			cell *dst = c;

			while (cells_to_move--)
				*dst-- = *src--;

			*save_c = save;
			break;
		}

		// Infix...

		if (is_infix(rhs) && !rhs->arity) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		pl_idx off = (pl_idx)(rhs - p->cl->cells);
		bool nolhs = last_idx == IDX_MAX;

		if (i == start_idx) nolhs = true;

		if (nolhs || (off > end_idx)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, missing operand to infix, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operand_missing";
			p->error = true;
			return false;
		}

		cell *lhs = p->cl->cells + last_idx;

		if (is_infix(lhs) && !lhs->arity) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operator_clash";
			p->error = true;
			return false;
		}

		save.num_cells += lhs->num_cells;
		pl_idx cells_to_move = lhs->num_cells;
		lhs = c - 1;

		while (cells_to_move--)
			*c-- = *lhs--;

		*c = save;
		c->num_cells += rhs->num_cells;
		c->arity = 2;

		if (is_xfx(c)) {
			cell *next = c + c->num_cells;
			i = next - p->cl->cells;

			if ((i <= end_idx)
				&& (is_xfx(next))
				&& (next->priority == c->priority)) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, operator clash, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "operator_clash";
				p->error = true;
				return false;
			}
		}

		c = p->cl->cells + last_idx;
		lhs = c + 1;
		rhs = lhs + lhs->num_cells;

		if (is_var(lhs) && (c->val_off == g_eq_s)) {
			c = rhs;

			for (unsigned i = 0; i < rhs->num_cells; i++, c++) {
				if (is_var(c))
					c->flags |= FLAG_VAR_GLOBAL;
			}
		}

		break;
	}

	return true;
}

// Stop when no more reductions to do.

static bool analyze(parser *p, pl_idx start_idx, bool last_op)
{
	while (reduce(p, start_idx, last_op))
		;

	return !p->error;
}

static bool dcg_expansion(parser *p)
{
	query *q = query_create(p->m);
	check_error(q);

	q->trace = false;
	cell *c = p->cl->cells;
	cell *tmp = alloc_heap(q, 1+c->num_cells+1+1);
	make_instr(tmp, new_atom(p->pl, "dcg_translate"), NULL, 2, c->num_cells+1);
	dup_cells(tmp+1, p->cl->cells, c->num_cells);
	make_ref(tmp+1+c->num_cells, p->cl->num_vars, 0);
	make_end(tmp+1+c->num_cells+1);
	bool ok = execute(q, tmp, p->cl->num_vars+MAX_ARITY);

	if (!ok || q->abort) {
		query_destroy(q);
		p->error = true;
		return false;
	}

	cell *arg1 = tmp + 1;
	cell *arg2 = arg1 + arg1->num_cells;
	c = deref(q, arg2, 0);
	q->max_depth = -1;
	char *src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);

	if (!src) {
		query_destroy(q);
		p->error = true;
		return false;
	}

	strcat(src, ".");
	query_destroy(q);

	parser *p2 = parser_create(p->m);
	check_error(p2);
	p2->srcptr = src;
	tokenize(p2, false, false);

	if (p2->error) {
		parser_destroy(p2);
		query_destroy(q);
		p->error = true;
		return false;
	}

	process_clause(p2->m, p2->cl, NULL);
	free(src);

	clear_clause(p->cl);
	free(p->cl);
	p->cl = p2->cl;					// Take the completed clause
	p->num_vars = p2->num_vars;
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

	module *m = p->m;
	predicate *pr = find_functor(m, "term_expansion", 2);

	if (!pr || !pr->head) {
		m = p->pl->user_m;
		pr = find_functor(m, "term_expansion", 2);
	}

	if (!pr || !pr->head)
		return false;

	cell *h = get_head(p->cl->cells);

	if (h->val_off == g_term_expansion_s)
		return false;

	if (h->val_off == g_colon_s)
		return false;

	query *q = query_create(m);
	check_error(q);
	q->trace = false;
	cell *c = p->cl->cells;
	cell *tmp = alloc_heap(q, 1+c->num_cells+2);
	unsigned num_cells = 0;
	make_instr(tmp+num_cells++, new_atom(p->pl, "term_expansion"), NULL, 2, c->num_cells+1);
	dup_cells(tmp+num_cells, p->cl->cells, c->num_cells);
	num_cells += c->num_cells;
	make_ref(tmp+num_cells++, p->cl->num_vars, 0);
	make_end(tmp+num_cells);
	execute(q, tmp, p->cl->num_vars+1);

	if (q->retry != QUERY_OK) {
		query_destroy(q);
		return false;
	}

	cell *arg1 = tmp + 1;
	cell *arg2 = arg1 + arg1->num_cells;
	c = deref(q, arg2, 0);
	q->max_depth = -1;
	char *src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);

	if (!src) {
		query_destroy(q);
		p->error = true;
		return false;
	}

	strcat(src, ".");
	parser *p2 = parser_create(p->m);
	check_error(p2);
	p2->srcptr = src;
	tokenize(p2, false, false);

	if (p2->error) {
		parser_destroy(p2);
		query_destroy(q);
		p->error = true;
		return false;
	}

	process_clause(p2->m, p2->cl, NULL);
	free(src);

	clear_clause(p->cl);
	free(p->cl);
	p->cl = p2->cl;					// Take the completed clause
	p->num_vars = p2->num_vars;
	p2->cl = NULL;

	parser_destroy(p2);
	query_destroy(q);

	return term_expansion(p);
}

static cell *goal_expansion(parser *p, cell *goal)
{
	if (p->error || p->internal || !is_interned(goal) || !is_callable(goal))
		return goal;

	if ((goal->val_off == g_goal_expansion_s) && (goal->arity == 2))
		return goal;

	if (goal->val_off == g_cut_s)
		return goal;

	if (get_builtin_term(p->m, goal, NULL, NULL) /*|| is_op(goal)*/)
		return goal;

	if (!search_goal_expansion(p->m, goal))
		return goal;

	if (!CMP_STRING_TO_CSTR(p, goal, "phrase") && !p->is_consulting)
		return goal;

	//if (search_predicate(p->m, goal, NULL))
	//	return goal;

	if (p->pl->in_goal_expansion) {
		//printf("??? goal_expansion %s/%u\n", C_STR(p, goal), goal->arity);
		return goal;
	}

	query *q = query_create(p->m);
	check_error(q);
	q->trace = false;
	q->varnames = true;
	q->max_depth = -1;
	char *dst = print_canonical_to_strbuf(q, goal, 0, 0);
	q->varnames = false;
	SB(s);
	SB_sprintf(s, "goal_expansion((%s),_TermOut), !.", dst);
	free(dst);

	//DUMP_TERM("old", p->cl->cells, 0, 0);

	// Note: since only parsing goals we need to preserve
	// the varnames so they get reused. Only genuinely new
	// variables should create anew. Hence we pull the
	// vartab from the main parser... IS THIS TRUE?

	//printf("+++ goal_expansion %s/%u\n", C_STR(p, goal), goal->arity);
	p->pl->in_goal_expansion = true;
	parser *p2 = parser_create(p->m);
	check_error(p2, query_destroy(q));
	q->top = p2;
	p2->cl->num_vars = p->cl->num_vars;
	p2->vartab = p->vartab;
	p2->reuse = true;
	p2->line_num = p->line_num;
	p2->skip = true;
	p2->srcptr = SB_cstr(s);
	tokenize(p2, false, false);

	if (p2->error) {
		parser_destroy(p2);
		query_destroy(q);
		p->error = true;
		SB_free(s);
		return goal;
	}

	process_clause(p2->m, p2->cl, NULL);
	execute(q, p2->cl->cells, p2->cl->num_vars);
	SB_free(s);
	p->pl->in_goal_expansion = false;
	//printf("-- goal_expansion %s/%u\n", C_STR(p, goal), goal->arity);

	if (q->retry != QUERY_OK) {
		parser_destroy(p2);
		query_destroy(q);
		return goal;
	}

	for (unsigned i = 0; i < p->cl->num_vars; i++)
		q->ignores[i] = true;

	p->cl->num_vars = p2->cl->num_vars;
	frame *f = GET_FRAME(0);
	char *src = NULL;

	for (unsigned i = 0; i < p2->cl->num_vars; i++) {
		if (!p2->vartab.off[i])
			continue;

		if (strcmp(GET_POOL(p, p2->vartab.off[i]), "_TermOut"))
			continue;

		slot *e = get_slot(q, f, i);

		if (is_empty(&e->c))
			continue;

		cell *c = deref(q, &e->c, e->c.val_ctx);
		q->varnames = true;
		q->max_depth = -1;
		src = print_canonical_to_strbuf(q, c, q->latest_ctx, 1);
		q->varnames = false;
		strcat(src, ".");
		break;
	}

	if (!src) {
		parser_destroy(p2);
		query_destroy(q);
		p->error = true;
		free(src);
		return goal;
	}

	parser_reset(p2);
	p2->cl->num_vars = p->cl->num_vars;
	p2->vartab = p->vartab;
	p2->reuse = true;
	p2->srcptr = src;
	tokenize(p2, false, false);

	if (is_var(p2->cl->cells)) {
		if (!p2->do_read_term)
			fprintf(stderr, "Error: instantiation error, goal_expansion/2, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p2->error_desc = "instantiation_error";
		p2->error = true;
	}

	if (p2->error) {
		parser_destroy(p2);
		query_destroy(q);
		p->error = true;
		free(src);
		return goal;
	}

	process_clause(p2->m, p2->cl, NULL);
	free(src);

	// Push the updated vartab back...

	p->cl->num_vars = p2->cl->num_vars;
	p->vartab = p2->vartab;

	// snip the old goal...

	const unsigned goal_idx = goal - p->cl->cells;
	unsigned trailing = p->cl->cidx - (goal_idx + goal->num_cells);
	p->cl->cidx -= goal->num_cells;
	unshare_cells(goal, goal->num_cells);
	memmove(goal, goal + goal->num_cells, sizeof(cell)*trailing);

	// make room for new goal...

	const unsigned new_cells = p2->cl->cidx-1;		// skip TAG_END
	trailing = p->cl->cidx - goal_idx;
	make_room(p, new_cells);
	goal = p->cl->cells + goal_idx;

	// shift up...

	memmove(goal+new_cells, goal, sizeof(cell)*trailing);

	// paste the new goal...

	memcpy(goal, p2->cl->cells, sizeof(cell)*new_cells);
	p->cl->cidx += new_cells;

	//DUMP_TERM("new", p->cl->cells, 0, 0);

	// done

	parser_destroy(p2);
	query_destroy(q);

	return goal;
}

static void expand_meta_predicate(parser *p, predicate *pr, cell *goal)
{
	unsigned arity = goal->arity;

	for (cell *k = goal+1, *m = pr->meta_args+1; arity--; k += k->num_cells, m += m->num_cells) {
		cell tmpbuf[2];

		if (is_interned(k) && (k->val_off == g_call_s))
			continue;
		else if ((k->arity == 2) && (k->val_off == g_colon_s) && is_atom(FIRST_ARG(k)))
			continue;
		else if (!is_interned(k) || is_iso_list(k))
			continue;
		else if (is_interned(m) && (m->val_off == g_colon_s)) {
			make_instr(tmpbuf+0, g_colon_s, bif_iso_qualify_2, 2, 1+k->num_cells);
			SET_OP(tmpbuf+0, OP_XFY);;
			make_atom(tmpbuf+1, new_atom(p->pl, p->m->name));
		} else if (is_smallint(m) && is_positive(m) && (get_smallint(m) <= 9)) {
			make_instr(tmpbuf+0, g_colon_s, bif_iso_qualify_2, 2, 1+k->num_cells);
			SET_OP(tmpbuf+0, OP_XFY);
			make_atom(tmpbuf+1, new_atom(p->pl, p->m->name));
		} else
			continue;

		// get some space...

		unsigned new_cells = 2, k_idx = k - p->cl->cells;
		unsigned trailing = (p->cl->cidx - k_idx) + 1;
		make_room(p, new_cells);

		// shift up...

		memmove(k+new_cells, k, sizeof(cell)*trailing);

		// paste the new goal...

		memcpy(k, tmpbuf, sizeof(cell)*new_cells);
		p->cl->cidx += new_cells;
		goal->num_cells += new_cells;
	}
}

static bool is_meta_arg(predicate *pr, cell *c, unsigned arg, int *extra)
{
	if (!pr->meta_args)
		return false;

	unsigned i = 0;

	for (cell *m = pr->meta_args+1; m && (i < c->arity); m += m->num_cells, i++) {
		if (!is_integer(m) || (i != arg))
			continue;

		if (extra)
			*extra = get_smallint(m);

		return true;
	}

	return false;
}

static cell *insert_call_here(parser *p, cell *c, cell *p1)
{
	pl_idx c_idx = c - p->cl->cells, p1_idx = p1 - p->cl->cells;
	make_room(p, 1);

	cell *last = p->cl->cells + (p->cl->cidx - 1);
	pl_idx cells_to_move = p->cl->cidx - p1_idx;
	cell *dst = last + 1;

	while (cells_to_move--)
		*dst-- = *last--;

	p1 = p->cl->cells + p1_idx;
	make_instr(p1, g_call_s, bif_iso_call_1, 1, 1);
	p->cl->cidx++;
	return p->cl->cells + c_idx;
}

static cell *term_to_body_conversion(parser *p, cell *c)
{
	pl_idx c_idx = c - p->cl->cells;
	bool is_head = c_idx == 0;

	if (is_xfx(c) || is_xfy(c)) {
		if ((c->val_off == g_conjunction_s)
			|| (c->val_off == g_disjunction_s)
			|| (c->val_off == g_if_then_s)
			|| (c->val_off == g_soft_cut_s)
			|| (c->val_off == g_neck_s)) {
			cell *lhs = c + 1;
			int extra = 0;

			if (is_var(lhs)) {
				c = insert_call_here(p, c, lhs);
				lhs = c + 1;
			} else {
				lhs->arity += extra;

				if ((c->val_off != g_neck_s))
					lhs = goal_expansion(p, lhs);

				lhs = term_to_body_conversion(p, lhs);
				lhs->arity -= extra;
			}

			cell *rhs = lhs + lhs->num_cells;
			extra = 0;
			c = p->cl->cells + c_idx;

			if (is_var(rhs))
				c = insert_call_here(p, c, rhs);
			else {
				rhs->arity += extra;
				rhs = goal_expansion(p, rhs);
				rhs = term_to_body_conversion(p, rhs);
				rhs->arity -= extra;
			}

			c->num_cells = 1 + lhs->num_cells + rhs->num_cells;
		}
	} else if (is_prefix(c)) {
		if (c->val_off == g_neck_s) {
			cell *rhs = c + 1;

			if (is_var(rhs)) {
				c = insert_call_here(p, c, rhs);
				rhs = c + 1;
			} else {
				rhs = goal_expansion(p, rhs);
				rhs = term_to_body_conversion(p, rhs);
			}

			c->num_cells = 1 + rhs->num_cells;
		} else if (c->val_off == g_negation_s) {
			cell *rhs = c + 1;

			if (!is_var(rhs)) {
				rhs = goal_expansion(p, rhs);
				c->num_cells = 1 + rhs->num_cells;
			}
		}
	} else if (!is_head && c->arity) {
		bool is_goal_expansion = find_goal_expansion(p->m, c);
		predicate *pr = find_predicate(p->m, c);
		bool meta = !pr || pr->is_meta_predicate || is_goal_expansion || p->m->wild_goal_expansion;
		bool control = false;

		if ((c->val_off == g_throw_s) && (c->arity == 1))
			control = true;
		else if ((c->val_off == g_catch_s) && (c->arity == 3))
			control = true;

		if (pr) {
			if (pr->alias)
				pr = pr->alias;

			if (pr->is_meta_predicate)
				expand_meta_predicate(p, pr, c);
		}

		if (meta) {
			c = goal_expansion(p, c);
		}

		cell *arg = c + 1;
		unsigned arity = c->arity, i = 0;

		while (arity--) {
			int extra = 0;
			bool meta = pr ? is_meta_arg(pr, c, i, &extra) : false;
			c->num_cells -= arg->num_cells;
			arg->arity += extra;

			if (meta)
				arg = goal_expansion(p, arg);

			if (control || meta)
				arg = term_to_body_conversion(p, arg);

			arg->arity -= extra;
			c->num_cells += arg->num_cells;
			arg += arg->num_cells;
			i++;
		}
	}

	c_idx = c - p->cl->cells;
	return p->cl->cells + c_idx;
}

void term_to_body(parser *p)
{
	term_to_body_conversion(p, p->cl->cells);
	p->cl->cells->num_cells = p->cl->cidx - 1;	// Drops TAG_END
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

			cell *rhs = lhs + lhs->num_cells;

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
	p2->is_consulting = true;
	p2->srcptr = (char*)src;
	tokenize(p2, false, false);

	if (p2->error) {
		parser_destroy(p2);
		p->error = true;
		return false;
	}

	parser_destroy(p2);
	return true;
}

cell *make_interned(parser *p, pl_idx offset)
{
	cell *c = make_a_cell(p);
	c->tag = TAG_INTERNED;
	c->num_cells = 1;
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

	if (*src == '\\') {
		*error = true;
		return 0;
	}

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

	if (ptr && ((ch != 's') || !p->flags.strict_iso))
		ch = g_escapes[ptr-g_anti_escapes];
	else if ((isdigit(ch) || (ch == 'x')
		|| (((ch == 'u') || (ch == 'U')) && (p->flags.json || !p->flags.strict_iso))
		)
		&& !number) {
		bool unicode = false;

		if (ch == 'x') {
			ch = get_hex(&src, UINT_MAX, error);
		} else if (ch == 'U') {
			ch = get_hex(&src, 8, error);
			unicode = true;
		} else if (ch == 'u') {
			ch = get_hex(&src, 4, error);
			unicode = true;

#if 0
			if (((unsigned)ch > 0xd800) && (src[0] == '\\') && (src[1] == 'u')) {
				src += 2;
				int ch2 = get_hex(&src, 4, error);
				ch = (((unsigned)ch - 0xd800) * 0x400) + ((unsigned)ch2 - 0xdc00) + 0x10000;
			}
#endif
		} else {
			src--;
			ch = get_octal(&src);
		}

		if (!p->error && (*src != '\\') && unicode && p->flags.json)
			src--;
		else if (!unicode && (*src++ != '\\')) {
			//if (!p->do_read_term)
			//	fprintf(stderr, "Error: syntax error, closing \\ missing\n");
			*_src = src;
			*error = true;
			return 0;
		}

		if ((unsigned)ch > 0x10FFFF) {
			//if (!p->do_read_term)
			//	fprintf(stderr, "Error: syntax error, illegal character code\n");
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
	int spacers = 0;

	while (*src) {
		if ((base == 2) && !isbdigit(*src))
			break;

		if ((base == 8) && !isodigit(*src))
			break;

		if ((base == 10) && !isdigit(*src))
			break;

		if ((base == 16) && !isxdigit(*src))
			break;

		if (spacers > 1) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, illegal character, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			*srcptr = src;
			p->error = true;
			return;
		}

		spacers = 0;
		SB_putchar(p->token, *src);
		src++;

		int last_ch = *src;

		while (*src == '_') {
			spacers++;
			src++;
		}

		if (last_ch == '_') {
			p->srcptr = (char*)src;
			src = eat_space(p);
		}
	}

	if (spacers) {
		if (!p->do_read_term)
			fprintf(stderr, "Error: syntax error, illegal character, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		*srcptr = src;
		p->error = true;
		return;
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

	if (*s == '.')
		return false;

	LOOP:

	if ((*s == '.') && isdigit(s[1])) {
		if (!p->do_read_term)
			fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	if (!isdigit(*s))
		return false;

	if ((s[0] == '0') && (s[1] == '\'') && iscntrl(s[2])) {
		if (!p->do_read_term)
			fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	if ((s[0] == '0') && (s[1] == '\'') && (s[2] == '\'')
		&& (iswspace(s[3]) || (s[3] == '.') || (s[3] == ',') || (s[3] == ';') || !s[3])) {
		if (!p->do_read_term)
			fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	if ((s[0] == '0') && (s[1] == '\'') && (s[2] == '\\') && !s[3]) {
		if (!p->do_read_term)
			fprintf(stderr, "Error: syntax error, parsing octal, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	if ((s[0] == '0') && (s[1] == '\'') && (s[2] == '\\') && isdigit(s[3])) {
		char *s2 = (char*)s+3;
		long long v = strtoll(s2, &s2, 8);

		if ((*s2 != '\\') || (v >= INT64_MAX)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parsing octal, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "number";
			p->error = true;
			return false;
		}

		s2++;
		p->v.tag = TAG_INT;
		set_smallint(&p->v, v);
		*srcptr = s2;
		return true;
	}

	if ((s[0] == '0') && (s[1] == '\'') && (s[2] == '\\') && ((s[3] == '+') || (s[3] == '-'))) {
		if (!p->do_read_term)
			fprintf(stderr, "Error: syntax error, parsing octal, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "number";
		p->error = true;
		return false;
	}

	if ((s[0] == '0') && (s[1] == '\'') && !((s[2] == '\\') && (s[3] == '\n'))
		&& (!search_op(p->m, "", NULL, false) || ((s[2] == '\'') && (s[3] == '\'')))) {
		if (!s[2] || (s[2] == '\n')) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "number";
			p->error = true;
			return false;
		}

		s += 2;
		int v;

		if (*s == '\\') {
			s++;

			if ((*s == '+') || (*s == '-')) {
				if (*s == '-')
					neg = true;

				s++;

				if (*s != '\'') {
					if (!p->do_read_term)
						fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

					p->error_desc = "number";
					p->error = true;
					return false;
				}

				s++;
				goto LOOP;
			}

			int save_ch = s[0];
			v = get_escape(p, &s, &p->error, false);

			if ((((save_ch == '0')) && !iscntrl(v)) || p->error) {
				//printf("*** *s=%d, iscntrl=%d, save_ch=%d, v=%d\n", *s, iscntrl(*s), save_ch, v);

				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "number";
				p->error = true;
				return false;
			}

		} else if ((*s == '\'') && s[1] == '\'') {
			s++;
			v = *s++;
		} else if ((*s == '\'') && p->flags.strict_iso && search_op(p->m, "", NULL, false)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "number";
			p->error = true;
			return false;
		} else
			v = get_char_utf8(&s);

		if (p->error) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "number";
			p->error = true;
			return false;
		}

		p->v.tag = TAG_INT;
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
			ensure(p->v.val_bigint);
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_INT_BIG | FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INT;
		*srcptr = s;
		return true;
	}

	if ((*s == '0') && (s[1] == 'o')) {
		s += 2;

		read_integer(p, &v2, 8, &s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			ensure(p->v.val_bigint);
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_INT_BIG | FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INT;
		*srcptr = s;
		return true;
	}

	if ((*s == '0') && (s[1] == 'x')) {
		s += 2;

		read_integer(p, &v2, 16, &s);

		if (mp_int_to_int(&v2, &val) == MP_RANGE) {
			p->v.val_bigint = malloc(sizeof(bigint));
			ensure(p->v.val_bigint);
			p->v.val_bigint->refcnt = 1;
			mp_int_init_copy(&p->v.val_bigint->ival, &v2);
			if (neg) p->v.val_bigint->ival.sign = MP_NEG;
			p->v.flags |= FLAG_INT_BIG | FLAG_MANAGED;
		} else {
			set_smallint(&p->v, val);
			if (neg) p->v.val_int = -p->v.val_int;
			mp_int_clear(&v2);
		}

		p->v.tag = TAG_INT;
		*srcptr = s;
		return true;
	}

	read_integer(p, &v2, 10, &s);

	if (p->flags.json && s && ((*s == 'e') || (*s == 'E')) && isdigit(s[1])) {
		p->v.tag = TAG_FLOAT;
		errno = 0;
		pl_flt v = strtod(tmpptr, &tmpptr);

		if ((int)v && (errno == ERANGE)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, float op %g, %s:%d\n", v, get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "float_overflow";
			p->error = true;
			return false;
		}

		set_float(&p->v, neg?-v:v);
#ifdef FE_INVALID
		feclearexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#endif
		*srcptr = tmpptr;
		mp_int_clear(&v2);
		return true;
	}

	if (s && (*s == '.') && isdigit(s[1])) {
		p->v.tag = TAG_FLOAT;
		errno = 0;
		pl_flt v = strtod(tmpptr, &tmpptr);

		if ((int)v && (errno == ERANGE)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, float op %g, %s:%d\n", v, get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "float_overflow";
			p->error = true;
			return false;
		}

		set_float(&p->v, neg?-v:v);
#ifdef FE_INVALID
		feclearexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#endif
		*srcptr = tmpptr;
		mp_int_clear(&v2);
		return true;
	}

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		p->v.val_bigint = malloc(sizeof(bigint));
		ensure(p->v.val_bigint);
		p->v.val_bigint->refcnt = 1;
		mp_int_init_copy(&p->v.val_bigint->ival, &v2);
		if (neg) p->v.val_bigint->ival.sign = MP_NEG;
		p->v.flags |= FLAG_INT_BIG | FLAG_MANAGED;
	} else {
		set_smallint(&p->v, val);
		if (neg) p->v.val_int = -p->v.val_int;
	}

	mp_int_clear(&v2);
	int ch;
	p->v.tag = TAG_INT;

	if ((s[-1] == '.') || isspace(s[-1]))
		s--;

	*srcptr = s;
	ch = peek_char_utf8(s);

	if (ch == '(') {
		if (!p->do_read_term)
			fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "unexpected_char";
		p->error = true;
		return false;
	}

	return true;
}

inline static bool is_matching_pair(int ch, int next_ch, int lh, int rh)
{
	return (ch == lh) && (next_ch == rh);
}

// FIXME

static bool valid_float(const char *src)
{
	if (*src == '.')
		return false;

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
	if (!*p->srcptr)
		return p->srcptr;

	p->did_getline = false;
	const char *src = p->srcptr;
	bool done;

	do {
		if (!src) {
			if (p->no_fp || getline(&p->save_line, &p->n_line, p->fp) == -1) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error = true;
				return NULL;
			}

			p->did_getline = true;
			src = p->srcptr = p->save_line;
		}

		done = true;
		int ch = peek_char_utf8(src);

		while (iswspace(ch)) {
			if (ch == '\n')
				p->line_num++;

			get_char_utf8(&src);
			ch = peek_char_utf8(src);
		}

		if ((*src == '%') && !p->fp) {
			while (*src && (*src != '\n'))
				src++;

			if (*src == '\n')
				p->line_num++;

			//src++;
			done = false;
			continue;
		}

		if ((!*src || (*src == '%')) && p->fp) {
			while (*src && (*src != '\n'))
				src++;

			if (*src == '\n')
				p->line_num++;

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
			if (!p->is_comment && (src[0] == '/') && (src[1] == '*')) {
				p->is_comment = true;
				src += 2;
				continue;
			}

			if (p->is_comment && (src[0] == '*') && (src[1] == '/')) {
				p->is_comment = false;
				src += 2;

				if (!is_number(&p->v))	// For number_chars
					p->srcptr = (char*)src;

				done = false;
				continue;
			}

			if (*src == '\n')
				p->line_num++;

			if (p->is_comment)
				src++;

			if ((!src || !*src) && p->is_comment && p->fp) {
				if (p->no_fp || getline(&p->save_line, &p->n_line, p->fp) == -1) {
					if (!p->do_read_term)
						fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

					p->error = true;
					return NULL;
				}

				p->did_getline = true;
				src = p->srcptr = p->save_line;
			}
		}
		 while (*src && p->is_comment);
	}
	 while (!done);

	return (char*)src;
}

static bool check_space_before_function(parser *p, int ch, const char *src)
{
	if (iswspace(ch) && (SB_strcmp(p->token, ".") || p->is_quoted)) {
		p->srcptr = (char*)src;
		//src = eat_space(p);

		while (iswblank(*src))
			src++;

		if (!src || !*src) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, incomplete statement, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "incomplete_statement";
			p->error = true;
			return false;
		}

		if (!p->is_op && (*src == '(')) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, operator expected before parens, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operator_expected";
			p->error = true;
			return false;
		}
	}

	return true;
}

static bool contains_null(const char *src, size_t len)
{
	if (!*src)
		return false;

	for (size_t i = 0; i < len; i++) {
		if (!*src++)
			return true;
	}

	return false;
}

const char *eat_continuation(const char* src)
{
	while ((src[0] == '\\') && (src[1] == '\n'))
		src += 2;

	return src;
}

bool get_token(parser *p, bool last_op, bool was_postfix)
{
	if (p->error || !p->srcptr || !*p->srcptr)
		return false;

	const char *src = p->srcptr;

	SB_init(p->token);
	p->v.tag = TAG_INTERNED;
	p->v.flags = 0;
	p->v.num_cells = 1;
	p->quote_char = 0;
	p->was_string = p->is_string = p->is_quoted = p->is_var = p->is_op = p->is_symbol = false;

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
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, illegal character escape <<%s>>, %s:%d\n", p->srcptr, get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "illegal_character_escape";
				p->error = true;
				return false;
			}
		}

		SB_sprintf(p->token, "%u", ch);
		p->srcptr = (char*)src;
		set_smallint(&p->v, ch);
		p->v.tag = TAG_INT;
		p->dq_consing = -1;
		return true;
	}

	src = eat_space(p);

	if (!src || !*src) {
		p->srcptr = (char*)src;
		return false;
	}

	// Numbers...

	const char *tmpptr = src;
	bool neg = false;

	if (p->last_neg) {
		p->last_neg = false;
		neg = true;
	}

	if ((*src != '-') && parse_number(p, &src, neg)) {
		if (neg) p->cl->cidx--;
		SB_strcatn(p->token, tmpptr, src-tmpptr);
		const char *dst = SB_cstr(p->token);

		if ((dst[0] != '0') && (dst[1] != 'x')) {
			if ((strchr(dst, '.') || strchr(dst, 'e') || strchr(dst, 'E')) && !strchr(dst, '\'')) {
				if (!valid_float(SB_cstr(p->token))) {
					if (!p->do_read_term)
						fprintf(stderr, "Error: syntax error, float, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

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
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parsing number, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "number";
			p->error = true;
			return false;
		}

		return true;
	}

	p->last_neg = false;

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
			p->is_string = true;

		for (;;) {
			int ch = 0;

			for (; *src && (ch = get_char_utf8(&src));) {
				if (ch == '\n') {
					if (!p->do_read_term)
						fprintf(stderr, "Error: syntax error, unterminated quoted atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

					p->error_desc = "unterminated_quoted_atom";
					p->error = true;
					p->srcptr = (char*)src;
					return false;
				}

				if ((ch == p->quote_char) && (*src == ch)) {
					ch = *src++;
				} else if ((ch == p->quote_char) && (ch == '\'')) {
					p->quote_char = 0;
					break;
				} else if (ch == p->quote_char) {
#if 1 // Double-bar
					const char *save_src = src;
					p->srcptr = (char*)src;
					src = eat_space(p);
					if (*src != '|') {
						p->quote_char = 0;
						break;
					}

					src++;
					p->srcptr = (char*)src;
					src = eat_space(p);

					if (*src != '|') {
						src = (char*)save_src;
						p->quote_char = 0;
						break;
					}

					src++;
					p->srcptr = (char*)src;
					src = eat_space(p);
					ch = *src;

					if (iswalnum(ch) || (ch == '_')
						|| (ch == '(') || ch == ')'
						|| (ch == '[') || ch == ']'
						|| (ch == '{') || ch == '}'
						|| (ch == '-')
						) {
						src = (char*)src;
						p->quote_char = 0;
						char *save_src = strdup(SB_cstr(p->token));
						SB_init(p->token);

						if (strlen(save_src)) {
							const char *src2 = save_src;
							SB_putchar(p->token, '[');
							bool any = false;

							while ((ch = get_char_utf8(&src2)) != 0) {
								if (any)
									SB_putchar(p->token, ',');

								SB_putchar(p->token, '\'');

								char *ptr = strchr(g_escapes, ch);

								if (ptr) {
									size_t n = ptr - g_escapes;
									SB_putchar(p->token, '\\');
									SB_putchar(p->token, g_anti_escapes[n]);
								} else
									SB_putchar(p->token, ch);

								SB_putchar(p->token, '\'');
								any = true;
							}

							SB_putchar(p->token, '|');
						} else {
							SB_putchar(p->token, '(');
						}

						int depth = 0;

						while ((ch = peek_char_utf8(src)) != 0) {
							if (!iswalnum(ch) && (ch != '_')
								&& !iswspace(ch)
								&& (ch != '(') && (ch != ')')
								&& (ch != '[') && (ch != ']')
								&& (ch != '{') && (ch != '}')
								&& (ch != '-') && (ch != '+')
								&& !depth
								)
								break;

							if ((ch == '(') || (ch == '[') || (ch == '{'))
								depth++;

							if ((ch == ')') || (ch == ']') || (ch == '}'))
								depth--;

							if (depth < 0)
								break;

							get_char_utf8(&src);
							SB_putchar(p->token, ch);

							if (!depth && ((ch == ')') || (ch == ']') || (ch == '}')))
								break;
						}

						if (strlen(save_src)) {
							SB_putchar(p->token, ']');
						} else {
							SB_putchar(p->token, ')');
						}

						free(save_src);
						save_src = strdup(SB_cstr(p->token));
						//printf("*** p->token=%s\n", save_src);
						SB_init(p->token);
						p->srcptr = save_src;
						p->no_fp = 1;
						tokenize(p, true, true);
						p->no_fp = 0;
						free(save_src);
						//printf("*** src=%s\n", src);
						p->srcptr = (char*)src;
						p->was_consing = true;
						SB_init(p->token);
						p->is_quoted = false;
						p->was_partial = true;
						break;
					} else if (*src != '"') {
						src = (char*)save_src;
						p->quote_char = 0;
						break;
					}
					src++;
					continue;
#else
					p->quote_char = 0;
					break;
#endif
				}

				if (ch < ' ') {
					if (!p->do_read_term)
						fprintf(stderr, "Error: syntax error, invalid quoted character, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

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
							//p->line_num++;
							continue;
						}
					} else {
						if (!p->do_read_term)
							fprintf(stderr, "Error: syntax error, illegal character escape <<%s>>, %s:%d\n", p->srcptr, get_loaded(p->m, p->m->filename), p->line_num);

						p->error_desc = "illegal_character_escape";
						p->error = true;
						p->srcptr = (char*)src;
						return false;
					}
				}

				SB_putchar(p->token, ch);

				if (!*src)
					break;
			}

			if (p->quote_char && p->fp) {
				if (p->no_fp || getline(&p->save_line, &p->n_line, p->fp) == -1) {
					p->srcptr = "";

					if (!p->do_read_term)
						fprintf(stderr, "Error: syntax error, unterminated quoted atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

					p->error_desc = "unterminated_quoted_atom";
					p->error = true;
					return false;
				}

				src = p->srcptr = p->save_line;
				continue;
			}

			if (p->is_string && !p->flags.json && !SB_strlen(p->token)) {
				SB_strcpy(p->token, "[]");
				p->was_string = true;
				p->is_string = false;
				p->srcptr = (char*)src;
				return true;
			}

			if (!p->is_string
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

			if (!src || !*src || !ch) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, unterminated quoted atom, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "unterminated_quoted_atom";
				p->error = true;
				return false;
			}

			if (*src) {
				p->srcptr = (char*)src;
				ch = peek_char_utf8(src);

				if (!check_space_before_function(p, ch, src))
					return false;

				if (!strcmp(SB_cstr(p->token), "-") && last_op && !was_postfix)
					p->last_neg = true;
			} else
				src = NULL;

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

		if ((!p->flags.var_prefix && !p->flags.json && iswupper(ch_start)) || (ch_start == '_')) {
			if (!p->is_number_chars) p->is_var = true;
		} else if (search_op(p->m, SB_cstr(p->token), NULL, false)) {
			if (!p->is_number_chars) p->is_op = true;
		}

		p->srcptr = (char*)src;
		int ch = peek_char_utf8(src);

		if (!check_space_before_function(p, ch, src))
			return false;

		src = p->srcptr;
		return true;
	}

	ch = get_char_utf8(&src);
	int next_ch = peek_char_utf8(src);
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
		if (!p->do_read_term)
			fprintf(stderr, "Error: syntax error, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "operator_expected";
		p->error = true;
		p->srcptr = (char*)src;
		return false;
	}

	p->is_symbol = true;

	do {
		SB_putchar(p->token, ch);

		if (((ch < 256) && strchr(g_solo, ch)) || iswspace(ch))
			break;

		int ch_next = peek_char_utf8(src);

		if (ch_next == '%')
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

	if (*src) {
		while (iswspace(*src))
			src++;
	}

	ch = peek_char_utf8(src);

	if (SB_strcmp(p->token, "(") && !check_space_before_function(p, ch, p->srcptr))
		return false;

	if (!SB_strcmp(p->token, ".") && ch == '|')
		p->quote_char = '\'';

	if (!strcmp(SB_cstr(p->token), "-") && last_op && !was_postfix)
		p->last_neg = true;

	return true;
}

static bool process_term(parser *p, cell *p1)
{
	if (conditionals(p, p1))
		return true;

	if (p->m->ifs_blocked[p->m->if_depth])
		return true;

	// Note: we actually assert directives after processing
	// so that they can be examined.

	directives(p, p1);

	if (p->error)
		return false;

	bool consulting = true;

	cell *h = get_head(p1);

	if (is_var(h)) {
		if (!p->do_read_term)
			printf("Error: instantiation error, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "instantiation_error";
		p->error = true;
		return false;
	} else if (is_number(h)) {
		if (!p->do_read_term)
			printf("Error: type error, callable, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

		p->error_desc = "type_error";
		p->error = true;
		return false;
	}

	if (is_cstring(h)) {
		pl_idx off = new_atom(p->pl, C_STR(p, h));
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

	rule *r;

	if ((r = assertz_to_db(p->m, p->cl->num_vars, p1, consulting)) == NULL) {
		if ((!p->do_read_term) && 0)
			printf("Error: assertion failed '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

		p->error = true;
		return false;
	}

	check_first_cut(&r->cl);
	r->cl.is_fact = !get_logical_body(r->cl.cells);
	r->line_num_start = p->line_num_start;
	r->line_num_end = p->line_num;
	p->line_num_start = 0;
	return true;
}

unsigned tokenize(parser *p, bool is_arg_processing, bool is_consing)
{
	pl_idx arg_idx = p->cl->cidx, save_idx = 0;
	bool last_op = true, is_func = false, last_num = false;
	bool last_bar = false, last_prefix = false, last_postfix = false;
	int entered = p->entered;
	unsigned arity = 1;
	p->depth++;

	while (get_token(p, last_op, last_postfix)) {
		if (p->error && !p->do_read_term)
			break;

		if (p->was_partial) {
			p->was_partial = false;
			last_op = false;
			continue;
		}

#if 0
		int ch = peek_char_utf8(SB_cstr(p->token));
		fprintf(stderr,
			"Debug: '%s' (%d) line_num=%d, symbol=%d, quoted=%d, tag=%u, op=%d, lastop=%d, string=%d\n",
			SB_cstr(p->token), ch, p->line_num, p->is_symbol, p->quote_char, p->v.tag, p->is_op, last_op, p->is_string);
#endif

		if (!p->quote_char
			&& !SB_strcmp(p->token, ".")
			&& (*p->srcptr != ',')
			&& (*p->srcptr != '(')
			&& (*p->srcptr != ')')
			&& (*p->srcptr != ']')
			&& (*p->srcptr != '}')
			&& !iswalnum(*p->srcptr)
			&& (*p->srcptr != '_')
			&& ((*p->srcptr != ' ') || !p->is_op)
			) {

			if (p->nesting_parens || p->nesting_brackets || p->nesting_braces) {
				if (!p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "mismatched_parens_or_brackets_or_braces";
				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			if (!p->cl->cidx) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, incomplete statement, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "incomplete_statement";
				p->error = true;
				return 0;
			}

			if (analyze(p, 0, last_op)) {
				if (p->cl->cells->num_cells < p->cl->cidx) {
					if (!p->do_read_term)
						printf("Error: syntax error, operator expected unfinished input '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

					p->error_desc = "operator_expected";
					p->error = true;
					return 0;
				}

				assign_vars(p, p->read_term_slots, false);

				if (p->error) return 0;

				if ((p->is_consulting /*|| p->is_command*/) && !p->skip && check_body_callable(p->cl->cells)) {
					if (!p->do_read_term)
						printf("Error: type error, not callable, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

					p->error_desc = "callable";
					p->error = true;
					return 0;
				}

				process_clause(p->m, p->cl, NULL);

				if (!p->one_shot)
					term_to_body(p);

				if ((p->is_consulting /*|| p->is_command*/) && !p->skip) {
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
						p->end_of_file = true;
						process_module(p->m);
						return 0;
					}
				}

				if (p->is_consulting && !p->skip) {
					// Term expansion can return a list...

					cell *p1 = p->cl->cells;
					LIST_HANDLER(p1);
					bool tail = false;

					while (is_iso_list(p1)) {
						cell *h = LIST_HEAD(p1);

						if (!process_term(p, h))
							return 0;

						if (p->already_loaded_error)
							return 0;

						p1 = LIST_TAIL(p1);

						if (is_nil(p1) || is_var(p1))
							tail = true;
					}

					if (!tail && !process_term(p, p1)) {
						p->error = true;
						return 0;
					}

					if (p->already_loaded_error)
						return 0;

					p->cl->cidx = 0;
				}
			}

			p->end_of_term = true;
			last_op = true;

			if (p->interactive)
				p->fp = NULL;

			if (p->one_shot)
				break;

			last_num = false;
			continue;
		}

		if (!p->line_num_start)
			p->line_num_start = p->line_num;

		p->end_of_term = false;

		if (!p->quote_char && !last_op &&
			(!SB_strcmp(p->token, "[") || !SB_strcmp(p->token, "{"))) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, needs operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "needs_operator";
			p->error = true;
			break;
		}

		if (!p->quote_char && p->last_close && !SB_strcmp(p->token, "(")) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, needs operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

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
			p->entered = '[';
			tokenize(p, true, true);

			if (!p->was_consing)
				make_interned(p, g_nil_s);

			p->start_term = p->last_close = false;
			p->was_consing = was_consing;
			last_bar = last_op = false;

			if (p->error)
				break;

			c = p->cl->cells + save_idx;
			c->num_cells = p->cl->cidx - save_idx;
			fix_list(c);
			last_num = false;
			continue;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "{")) {
			save_idx = p->cl->cidx;
			cell *c = make_interned(p, g_braces_s);
			ensure(c);
			c->arity = 1;
			p->start_term = true;
			p->nesting_braces++;
			p->entered = '{';
			tokenize(p, false, false);
			last_bar = false;

			if (p->error)
				break;

			c = p->cl->cells+save_idx;
			c->num_cells = p->cl->cidx - save_idx;
			p->start_term = p->last_close = false;
			last_op = false;
			last_num = false;
			continue;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "(")) {
			p->start_term = true;
			p->nesting_parens++;
			p->entered = '(';
			unsigned tmp_arity = tokenize(p, is_func, false);
			last_bar = false;

			if (p->error)
				break;

			if (is_func) {
				cell *c = p->cl->cells + save_idx;
				c->arity = tmp_arity;
				c->num_cells = p->cl->cidx - save_idx;
			}

			is_func = last_op = false;
			last_num = false;
			p->start_term = p->last_close = false;
			continue;
		}

		if (!p->quote_char && !is_arg_processing && !is_consing && last_op && !last_postfix && !SB_strcmp(p->token, ",")) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, quotes needed around operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "quotes_needed";
			p->error = true;
			break;
		}

		if (!p->quote_char && is_arg_processing && !is_consing && p->is_op /*&& last_op*/
			&& SB_strcmp(p->token, "|")
 			&& SB_strcmp(p->token, ",")
 			) {
			unsigned priority = search_op(p->m, SB_cstr(p->token), NULL, false);

			if (!last_op && (priority > 999)) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, parens needed around operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "parens_needed";
				p->error = true;
				break;
			}
		}

		if (!p->quote_char && is_consing && !SB_strcmp(p->token, ",")) {
			if ((arg_idx == p->cl->cidx) || !p->cl->cidx) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if ((*p->srcptr == ',') && !p->flags.double_quote_codes) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing element '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "missing_element";
				p->error = true;
				break;
			}

			if (p->was_consing || last_op) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, parsing list '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

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
			((is_arg_processing && !SB_strcmp(p->token, ",")) ||
			(is_consing && !p->was_consing && !p->start_term && (!SB_strcmp(p->token, ",") || !SB_strcmp(p->token, "|")))
			)) {
			if ((arg_idx == p->cl->cidx) || !p->cl->cidx) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			analyze(p, arg_idx, last_op);
			arg_idx = p->cl->cidx;

			if (*p->srcptr == ',') {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if (is_arg_processing) {
				arity++;

				if (arity > MAX_ARITY) {
					if (!p->do_read_term)
						fprintf(stderr, "Error: max arity reached, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

					p->error_desc = "max_arity";
					p->error = true;
					break;
				}
			}

			if (is_consing && !SB_strcmp(p->token, "|")) {
				p->was_consing = last_bar = true;
				//is_consing = false;
			}

			p->last_close = false;
			last_op = true;
			last_num = false;
			continue;
		}

		if (!p->is_quoted && is_consing && p->start_term && !SB_strcmp(p->token, "|")) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parsing list '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->is_quoted && p->was_consing && is_consing && !SB_strcmp(p->token, "|")) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parsing list '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->is_quoted && p->was_consing && last_bar && !SB_strcmp(p->token, "]")) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parsing list '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "list";
			p->error = true;
			break;
		}

		if (!p->quote_char && p->start_term &&
			(!SB_strcmp(p->token, "]") || !SB_strcmp(p->token, ")") || !SB_strcmp(p->token, "}"))) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, start of rule expected, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "start_expected";
			p->error = true;
			break;
		}

		if (!p->quote_char && !SB_strcmp(p->token, ")")) {
			if (arg_idx == p->cl->cidx) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if (entered != '(') {
				if (!p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "mismatched_parens_or_brackets_or_braces";
				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			p->last_close = true;
			p->nesting_parens--;
			analyze(p, arg_idx, last_op=false);
			return arity;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "]")) {
			if (arg_idx == p->cl->cidx) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if (entered != '[') {
				if (!p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "mismatched_parens_or_brackets_or_braces";
				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			p->last_close = true;
			p->nesting_brackets--;
			analyze(p, arg_idx, last_op=false);
			return arity;
		}

		if (!p->quote_char && !SB_strcmp(p->token, "}")) {
			if (arg_idx == p->cl->cidx) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, missing arg '%s', %s:%d\n", p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "args";
				p->error = true;
				break;
			}

			if (entered != '{') {
				if (!p->do_read_term)
					printf("Error: syntax error, mismatched parens/brackets/braces, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "mismatched_parens_or_brackets_or_braces";
				p->error = true;
				p->nesting_parens = p->nesting_brackets = p->nesting_braces = 0;
			}

			p->last_close = true;
			p->nesting_braces--;
			analyze(p, arg_idx, last_op=false);
			return arity;
		}

		p->last_close = false;

		if (p->is_var && (*p->srcptr == '(')) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, var as functor, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "variable_cannot_be_functor";
			p->error = true;
			break;
		}

		unsigned specifier = 0;
		int priority = 0;

		if (is_interned(&p->v)) {
			char *s = eat_space(p);

			if (!s || !*s) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, incomplete, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "incomplete";
				p->error = true;
				break;
			}

			int nextch = *s;
			bool noneg = (!SB_strcmp(p->token, "-") || !SB_strcmp(p->token, "+")) && (nextch == '='); // Hack

			if (noneg && !p->is_string) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, incomplete, needs parenthesis, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "incomplete";
				p->error = true;
				break;
			}

			if (!last_op) priority = get_op(p->m, SB_cstr(p->token), specifier=OP_XF);
			if (!priority && !last_op) priority = get_op(p->m, SB_cstr(p->token), specifier=OP_YF);
			if (!priority) specifier = 0;
			const char *src = eat_space(p);
			int ch = peek_char_utf8(src);
			bool blah = !iswalpha(ch) && (ch != '_') && (ch != '(') && priority;

			if (!blah) {
				bool prefer_unifix = last_op || blah;
				priority = search_op(p->m, SB_cstr(p->token), &specifier, prefer_unifix);
			}
		}

		if (!SB_strcmp(p->token, "!") &&
			((*p->srcptr == ')') || (*p->srcptr == ';') || (*p->srcptr == ',') || (*p->srcptr == '.')))
			p->quote_char = 1;

		if (p->quote_char && last_op) {
			specifier = 0;
			priority = 0;
		}

		if (priority && (last_op || last_bar)
			&& !IS_POSTFIX(specifier)) {
			char *s = eat_space(p);

			if (!s || !*s) {
				if (!p->do_read_term)
					fprintf(stderr, "Error: syntax error, incomplete, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);

				p->error_desc = "error_incomplete";
				p->error = true;
				break;
			}

			int nextch = *s;

			if (IS_PREFIX(specifier) && p->is_symbol && last_prefix)
				;
			else if ((nextch == ',')
				|| (nextch == ';')
				|| (nextch == ')')
				|| (nextch == '|')
				|| (nextch == ']')
				|| (nextch == '}')
			) {
				if ((SB_strcmp(p->token, "-") && SB_strcmp(p->token, "+")) || is_consing) {
					specifier = 0;
					priority = 0;
				}
			}
		}

		if (priority && IS_POSTFIX(specifier) && is_arg_processing && last_op && !last_postfix) {
			specifier = 0;
			priority = 0;
		}

		if (priority && IS_INFIX(specifier) && last_op && !last_postfix) {
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

		if (!is_func && last_op && (is_arg_processing && priority >= 1200) && !p->is_quoted) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, parens needed around operator '%s', %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "parens_needed";
			p->error = true;
			break;
		}

		if ((p->was_string || p->is_string) && is_func) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, near \"%s\", expected atom, %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "expected_atom";
			p->error = true;
			break;
		}

		if (is_func) {
			p->is_op = false;
			specifier = 0;
			save_idx = p->cl->cidx;
		}

		if (!p->is_op && !is_func && last_op && last_postfix && 0) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, near '%s', operator expected postfix '%s', %s:%d\n", SB_cstr(p->token), p->save_line?p->save_line:"", get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		if (is_consing && IS_INFIX(specifier) && (priority >= 1000)) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, near '%s', expected, %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		if ((!p->is_op || IS_PREFIX(specifier)) && !is_func && !last_op) {
			if (!p->do_read_term)
				fprintf(stderr, "Error: syntax error, near '%s', operator expected, %s:%d\n", SB_cstr(p->token), get_loaded(p->m, p->m->filename), p->line_num);

			p->error_desc = "operator_expected";
			p->error = true;
			break;
		}

		last_op = SB_strcmp(p->token, ")") && priority;
		last_postfix = IS_POSTFIX(specifier);
		last_prefix = last_op && IS_PREFIX(specifier);

		p->start_term = false;
		cell *c = make_a_cell(p);
		c->num_cells = 1;
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
#ifdef FE_INVALID
			feclearexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW);
#endif
		} else if (!p->is_string
			&& (!p->is_quoted || is_func || p->is_op || p->is_var || p->is_consulting
			|| (get_builtin(p->pl, SB_cstr(p->token), SB_strlen(p->token), 0, &found, NULL), found)
			|| !SB_strcmp(p->token, "[]"))
			) {
			if (is_func && !SB_strcmp(p->token, "."))
				c->priority = 0;

			// We temporarily make use of 'var_num' to hold the nesting info
			// on the var, used for determining later if it's a global or not.

			if (p->is_var) {
				c->tag = TAG_VAR;
				c->var_num = (2*p->nesting_braces) + (2*p->nesting_brackets) + p->nesting_parens;
			}

			if (!p->is_number_chars) {
				c->val_off = new_atom(p->pl, SB_cstr(p->token));
				ensure(c->val_off != ERR_IDX);
			}
		} else {
			c->tag = TAG_CSTR;
			size_t toklen = SB_strlen(p->token);

			if ((toklen < MAX_SMALL_STRING) && !p->is_string) {
				memcpy(c->val_chr, SB_cstr(p->token), toklen);
				c->val_chr[toklen] = '\0';
				c->chr_len = toklen;
			} else {
				if (p->is_string) {
					c->flags |= FLAG_CSTR_STRING;
					c->arity = 2;
				}

				if (!p->is_number_chars)
					make_string_internal(c, SB_cstr(p->token), toklen, 0);
			}
		}

		last_bar = false;
	}

	p->depth--;
	return !p->error ? 1 : 0;
}

bool run(parser *p, const char *prolog_src, bool dump, query **subq, unsigned int yield_time_in_ms)
{
	if ((*prolog_src == '.') && !prolog_src[1]) {
		fprintf(stderr, "Error: syntax error, incomplete statement, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);
		return false;
	}

	SB(pr);

	if (dump) {
		SB_strcat(pr, "true,");
	}

	if (*prolog_src != '[') {
		SB_sprintf(pr, "((%s", prolog_src);
	} else {
		SB_sprintf(pr, "%s", prolog_src);
	}

	SB_trim_ws(pr);
	SB_trim(pr, '.');

	if (*prolog_src != '[') {
		SB_strcat(pr, ")).");
	} else {
		SB_strcat(pr, ".");
	}

	p->in_body = true;
	p->srcptr = SB_cstr(pr);
	bool ok;

	while (p->srcptr && *p->srcptr) {
		parser_reset(p);
		p->line_num_start = 0;
		p->line_num = 1;
		p->one_shot = true;
		p->is_consulting = false;
		tokenize(p, false, false);

		if (p->error) {
			p->pl->error = p->error;
			break;
		}

		if (!p->cl->cidx)
			break;

		if (!p->error && !p->end_of_term && !p->m->run_init) {
			fprintf(stderr, "Error: syntax error, missing operand or operator, %s:%d\n", get_loaded(p->m, p->m->filename), p->line_num);
			p->error = true;
		}

		if (p->error) {
			p->pl->did_dump_vars = true;
			p->srcptr = NULL;
			p->pl->error = p->error;
			SB_free(pr);
			return false;
		}

		if (p->skip) {
			p->pl->status = true;
			p->srcptr = NULL;
			SB_free(pr);
			return true;
		}

		query *q = query_create(p->m);
		checked(q, p->srcptr = NULL, SB_free(pr));

		if (subq)
			*subq = q;

		if (yield_time_in_ms > 0)
			do_yield_at(q, yield_time_in_ms);

		q->top = p;
		q->do_dump_vars = dump;
		q->run_init = p->m->run_init;
		execute(q, p->cl->cells, p->cl->num_vars);

		if (q->halt) {
			p->pl->halt = q->halt;
			p->pl->halt_code = q->halt_code;
		}

		p->pl->status = q->status;
		p->pl->error = q->error;
		p->pl->is_redo = q->is_redo;
		ok = !q->error;
		p->m = q->st.m;

		if (!subq)
			query_destroy(q);

		if (p->pl->is_query)
			break;

		if (!ok)
			break;
	}

	stream *str = &p->pl->streams[0];
	fflush(str->fp);
	free(str->data);
	str->data = NULL;
	str->data_len = 0;
	str->srclen = 0;
	p->srcptr = NULL;
	SB_free(pr);
	return ok;
}
