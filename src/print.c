#include <ctype.h>
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "heap.h"
#include "module.h"
#include "network.h"
#include "parser.h"
#include "query.h"

typedef struct visit_ visit;

struct visit_ {
	visit *next;
	cell *c;
	pl_idx c_ctx;
};

static bool has_visited(visit *visited, cell *c, pl_idx c_ctx)
{
	while (visited) {
		if ((visited->c == c) && (visited->c_ctx == c_ctx))
			return true;

		visited = visited->next;
	}

	return false;
}

static void clear_visited(visit *visited, visit *save_visited)
{
	while (visited != save_visited) {
		visit *tmp = visited;
		visited = visited->next;
		free(tmp);
	}
}

cell *string_to_chars_list(query *q, cell *p, pl_idx p_ctx)
{
	LIST_HANDLER(p);
	init_tmp_heap(q);

	while (is_list(p)) {
		cell *h = LIST_HEAD(p);
		append_list(q, h);
		p = LIST_TAIL(p);
	}

	return end_list(q);
}

char *chars_list_to_string(query *q, cell *p_chars, pl_idx p_chars_ctx, size_t len)
{
	char *tmp = malloc(len+1+1);
	check_error(tmp);
	char *dst = tmp;
	LIST_HANDLER(p_chars);

	while (is_list(p_chars)) {
		cell *h = LIST_HEAD(p_chars);
		h = deref(q, h, p_chars_ctx);

		if (is_integer(h)) {
			int ch = get_smallint(h);
			dst += put_char_utf8(dst, ch);
		} else {
			const char *p = C_STR(q, h);
			int ch = peek_char_utf8(p);
			dst += put_char_utf8(dst, ch);
		}

		p_chars = LIST_TAIL(p_chars);
		p_chars = deref(q, p_chars, p_chars_ctx);
		p_chars_ctx = q->latest_ctx;
	}

	*dst = '\0';
	return tmp;
}

bool needs_quoting(module *m, const char *src, int srclen)
{
	if (!*src)
		return true;

	if (!strcmp(src, ",") || !strcmp(src, ".") || !strcmp(src, "|"))
		return true;

	if (!strcmp(src, "{}") || !strcmp(src, "[]")
		|| !strcmp(src, "!") || !strcmp(src, ";")
		|| !strcmp(src, "\\")	// ???????
		)
		return false;

	if ((src[0] == '/') && (src[1] == '*'))
		return true;

	int ch = peek_char_utf8(src);

	if (!iswalnum(ch) && strchr(src, '_'))
		return true;

	if (iswupper(ch) || isdigit(ch) || (ch == '_'))
		return true;

	const char *s = src;
	int slen = srclen;

	while (slen > 0) {
		slen -= len_char_utf8(s);
		int ch = get_char_utf8(&s);

		if (((ch < 256) && strchr(g_solo, ch))
			|| iswspace(ch)
			|| (ch == 0xA0)
			)
			return true;
	}

	int cnt = 0, alphas = 0, graphs = 0;

	while (srclen > 0) {
		srclen -= len_char_utf8(src);
		int ch = get_char_utf8(&src);
		cnt++;

		if (iswalnum(ch)
#ifdef __APPLE__
			|| iswideogram(ch)
#endif
			|| (ch == '_')
			)
			alphas++;
		else if ((ch < 256) && iswgraph(ch) && (ch != '%'))
			graphs++;
		else if (iswgraph(ch) && (ch != '%')
			//&& (cnt == 1)	// Hack
			)
			graphs++;
	}

	if (cnt == alphas)
		return false;

	if (cnt == graphs)
		return false;

	return true;
}

static bool op_needs_quoting(module *m, const char *src, int srclen)
{
	if (!strcmp(src, "{}") || !strcmp(src, "[]") || !strcmp(src, "!"))
		return false;

	int ch = peek_char_utf8(src);

	if (iswupper(ch) || isdigit(ch) || (ch == '_'))
		return true;

	if (search_op(m, src, NULL, false))
		return strchr(src, ' ')
			|| strchr(src, '\'')
			|| strchr(src, '\"')
			|| !strcmp(src, "(")
			|| !strcmp(src, ")")
			|| !strcmp(src, "[")
			|| !strcmp(src, "]")
			|| !strcmp(src, "{")
			|| !strcmp(src, "}");

	if (!iswlower(ch) || !iswalpha(ch)) { // NO %/
		static const char *s_symbols = "+-*<>=@#^~\\:$.";
		int quote = false;

		while (srclen--) {
			if (!strchr(s_symbols, *src)) {
				quote = true;
				break;
			}

			src++;
		}

		return quote;
	}

	while (srclen > 0) {
		int lench = len_char_utf8(src);
		int ch = get_char_utf8(&src);
		srclen -= lench;

		if (!iswalnum(ch) && (ch != '_'))
			return true;
	}

	return false;
}

static bool has_spaces(const char *src, int srclen)
{
	if (!*src)
		return true;

	while (srclen > 0) {
		int lench = len_char_utf8(src);
		int ch = get_char_utf8(&src);
		srclen -= lench;

		if (isspace(ch))
			return true;
	}

	return false;
}

char *formatted(const char *src, int srclen, bool dq, bool json)
{
	extern const char *g_escapes;
	extern const char *g_anti_escapes;
	SB(sb);

	while (srclen > 0) {
		int lench = len_char_utf8(src);
		int ch = get_char_utf8(&src);
		srclen -= lench;
		const char *ptr = (lench == 1) && (ch != ' ') && (ch != '\e') ? strchr(g_escapes, ch) : NULL;

		if ((ch == '\'') && dq)
			ptr = 0;

		if (ch && ptr) {
			SB_putchar(sb, '\\');
			SB_putchar(sb, g_anti_escapes[ptr-g_escapes]);
		} else if (!json && !dq && (ch == '\'')) {
			SB_putchar(sb, '\'');
			SB_putchar(sb, ch);
		} else if (ch == (dq?'"':'\'')) {
			SB_putchar(sb, '\\');
			SB_putchar(sb, ch);
		} else if (!json && (ch < ' ')) {
			SB_putchar(sb, '\\');
			SB_putchar(sb, 'x');
			SB_sprintf(sb, "%x", ch);
			SB_putchar(sb, '\\');
		} else if (json && (ch < ' ')) {
			SB_putchar(sb, '\\');
			switch (ch) {
			case '\b': SB_putchar(sb, 'b'); break;
			case '\n': SB_putchar(sb, 'n'); break;
			case '\f': SB_putchar(sb, 'f'); break;
			case '\r': SB_putchar(sb, 'r'); break;
			case '\t': SB_putchar(sb, 't'); break;
			default: SB_sprintf(sb, "u%04X", ch);
			}
		} else if (((unsigned)ch > 0x10000) && json && false) {
			SB_putchar(sb, '\\');
			SB_sprintf(sb, "U%08X", ch);
		} else if (((unsigned)ch > 0x10000) && json) {
			unsigned ch1 = (ch - 0x10000) / 0x400 + 0xd800;
			unsigned ch2 = (ch - 0x10000) % 0x400 + 0xdc00;
			SB_putchar(sb, '\\');
			SB_sprintf(sb, "u%04X", ch1);
			SB_putchar(sb, '\\');
			SB_sprintf(sb, "u%04X", ch2);
		} else if (ch == '\\') {
			SB_putchar(sb, '\\');
			SB_putchar(sb, ch);
		} else {
			SB_putchar(sb, ch);
		}
	}

	char *dst = strdup(SB_cstr(sb));
	SB_free(sb);
	return dst;
}

static size_t sprint_int_(char *dst, size_t dstlen, pl_int n, int pbase)
{
	int base = abs(pbase);
	const char *save_dst = dst;

	if ((n / base) > 0)
		dst += sprint_int_(dst, dstlen, n / base, pbase);

	int n2 = n % base;

	if (n2 > 9) {
		n2 -= 10;
		n2 += pbase < 0 ? 'A' : 'a';
	} else
		n2 += '0';

	if (dstlen)
		*dst++ = n2;
	else
		dst++;

	return dst - save_dst;
}

size_t sprint_int(char *dst, size_t dstlen, pl_int n, int base)
{
	const char *save_dst = dst;

	if ((n < 0) && (base == 10)) {
		if (dstlen)
			*dst++ = '-';
		else
			dst++;

		// NOTE: according to the man heap_pages:
		//
		//		"Trying to take the absolute value of
		// 		the most negative integer is not defined."
		//

		if (n == PL_INT_MIN)
			n = imaxabs(n+1) - 1;
		else
			n = imaxabs(n);
	}

	if (n == 0) {
		if (dstlen)
			*dst++ = '0';
		else
			dst++;

		if (dstlen)
			*dst = '\0';

		return dst - save_dst;
	}

	dst += sprint_int_(dst, dstlen, n, base);

	if (dstlen)
		*dst = '\0';

	return dst - save_dst;
}

static void reformat_float(query *q, char *tmpbuf, pl_flt v)
{
	if ((!strchr(tmpbuf, 'e') && !strchr(tmpbuf, 'E'))
		&& !q->ignore_ops) {
		char tmpbuf3[256];
		sprintf(tmpbuf3, "%.*g", 15, v);
		size_t len3 = strlen(tmpbuf3);
		size_t len = strlen(tmpbuf);

		if ((len - len3) > 1)
			strcpy(tmpbuf, tmpbuf3);
	}

	char tmpbuf2[256];
	strcpy(tmpbuf2, tmpbuf);
	const char *src = tmpbuf2;
	char *dst = tmpbuf;

	if (*src == '-')
		*dst++ = *src++;

	while (isdigit(*src))
		*dst++ = *src++;

	if ((*src != '.') && (*src != ',')) {
		*dst++ = '.';
		*dst++ = '0';
	} else if (*src == ',') {
		*dst++ = '.';
		src++;
	}

	while (*src)
		*dst++ = *src++;

	*dst = '\0';
}

static const char *varformat2(char *tmpbuf, size_t tmpbuf_len, cell *c, unsigned nv_start)
{
	mpz_t tmp;

	if (is_smallint(c))
		mp_int_init_value(&tmp, c->val_int);
	else
		mp_int_init_copy(&tmp, &c->val_bigint->ival);

	mp_small nbr;
	mp_int_mod_value(&tmp, 26, &nbr);
	char *dst = tmpbuf;
	dst += sprintf(dst, "%c", 'A'+(unsigned)(nbr));
	mp_int_div_value(&tmp, 26, &tmp, NULL);

	if (mp_int_compare_zero(&tmp) > 0)
		dst += mp_int_to_string(&tmp, 10, dst, tmpbuf_len);

	mp_int_clear(&tmp);
	return tmpbuf;
}

static const char *varformat(char *tmpbuf, unsigned long long nbr)
{
	char *dst = tmpbuf;
	dst += sprintf(dst, "%c", 'A'+(unsigned)(nbr%26));
	if ((nbr/26) > 0) dst += sprintf(dst, "%"PRIu64"", (int64_t)(nbr/26));
	return tmpbuf;
}

static const char *get_slot_name(query *q, pl_idx slot_nbr)
{
	for (unsigned i = 0; i < q->print_idx; i++) {
		if (q->pl->tab1[i] == slot_nbr) {
			return varformat(q->pl->tmpbuf, q->pl->tab2[i]);
		}
	}

	unsigned j, i = q->print_idx++;
	q->pl->tab1[i] = slot_nbr;

	for (j = 0; j < MAX_IGNORES; j++) {
		if (!q->ignores[j]) {
			q->ignores[j] = true;
			break;
		}
	}

	q->pl->tab2[i] = j;
	return varformat(q->pl->tmpbuf, i);
}

void print_variable(query *q, const cell *c, pl_idx c_ctx, bool running)
{
	const frame *f = GET_FRAME(running ? c_ctx : 0);
	pl_idx slot_nbr = running ?
		(GET_SLOT(f, c->var_nbr)-q->slots)
		: (unsigned)c->var_nbr;

	if (q->varnames && !is_anon(c) && running && !q->cycle_error && (c_ctx == 0)) {
		if (q->varnames && q->p->vartab.var_name[c->var_nbr] && !is_fresh(c)) {
			SB_sprintf(q->sb, "%s", q->p->vartab.var_name[c->var_nbr]);
		} else {
			SB_sprintf(q->sb, "%s", get_slot_name(q, slot_nbr));
		}
	} else if (q->portray_vars || (q->is_dump_vars && q->cycle_error)) {
		SB_sprintf(q->sb, "%s", get_slot_name(q, slot_nbr));
	} else if (q->is_dump_vars) {
		if ((c_ctx == 0) && !is_fresh(c) && !is_anon(c) && (c->var_nbr < q->p->nbr_vars)) {
			SB_sprintf(q->sb, "%s", q->p->vartab.var_name[c->var_nbr]);
		} else {
			SB_sprintf(q->sb, "_%s", get_slot_name(q, slot_nbr));
		}
	} else if (q->listing) {
		SB_sprintf(q->sb, "%s", get_slot_name(q, slot_nbr));
	} else if (!running && !is_ref(c)) {
		SB_sprintf(q->sb, "%s", C_STR(q, c));
	} else {
		SB_sprintf(q->sb, "_%u", (unsigned)slot_nbr);
	}
}

static bool dump_variable(query *q, cell *c, pl_idx c_ctx, bool running)
{
	cell *l = q->variable_names;
	pl_idx l_ctx = q->variable_names_ctx;
	LIST_HANDLER(l);

	while (is_iso_list(l)) {
		cell *h = LIST_HEAD(l);
		h = running ? deref(q, h, l_ctx) : h;
		pl_idx h_ctx = running ? q->latest_ctx : 0;
		cell *name = running ? deref(q, h+1, h_ctx) : h+1;
		cell *v = running ? deref(q, h+2, h_ctx) : h+2;
		pl_idx v_ctx = running ? q->latest_ctx : 0;

		if (is_var(v) && (v->var_nbr == c->var_nbr) && (v_ctx == c_ctx)) {
			if (0 && !strcmp(C_STR(q, name), "_")) {
				print_variable(q, v, v_ctx, running);
			} else {
				SB_sprintf(q->sb, "%s", C_STR(q, name));
			}

			q->last_thing = WAS_OTHER;
			return true;
		}

		l = LIST_TAIL(l);
		l = running ? deref(q, l, l_ctx) : l;
		l_ctx = running ? q->latest_ctx : 0;
	}

	return false;
}

static void print_string_canonical(query *q, cell *c, pl_idx c_ctx, int running, bool cons, unsigned depth)
{
	unsigned cnt = 1;
	LIST_HANDLER(c);

	SB_sprintf(q->sb, "%s", "'.'(");

	while (is_list(c)) {
		cell *h = LIST_HEAD(c);

		const char *src = C_STR(q, h);

		if (needs_quoting(q->st.m, src, strlen(src))) {
			SB_sprintf(q->sb, "%s", "'");
			SB_strcat_and_free(q->sb, formatted(C_STR(q, h), C_STRLEN(q, h), false, false));
			SB_sprintf(q->sb, "%s", "'");
		} else
			SB_sprintf(q->sb, "%s", C_STR(q, h));

		c = LIST_TAIL(c);

		if (!is_list(c)) {
			SB_sprintf(q->sb, "%s", ",[]");
			break;
		}

		SB_sprintf(q->sb, "%s", ",'.'(");
		cnt++;
	}

	while (cnt--) {
		SB_sprintf(q->sb, "%s", ")");
	}
}

static void print_string_list(query *q, cell *c, pl_idx c_ctx, int running, bool cons, unsigned depth)
{
	LIST_HANDLER(c);
	if (!cons) { SB_sprintf(q->sb, "%s", "["); }

	while (is_list(c)) {
		cell *h = LIST_HEAD(c);

		const char *src = C_STR(q, h);

		if (needs_quoting(q->st.m, src, strlen(src)) && q->quoted) {
			SB_sprintf(q->sb, "%s", "'");
			SB_strcat_and_free(q->sb, formatted(C_STR(q, h), C_STRLEN(q, h), false, false));
			SB_sprintf(q->sb, "%s", "'");
		} else {
			SB_sprintf(q->sb, "%s", C_STR(q, h));
		}

		c = LIST_TAIL(c);

		if (!is_list(c))
			break;

		SB_sprintf(q->sb, "%s", ",");
	}

	if (!cons) { SB_sprintf(q->sb, "%s", "]"); }
}

static bool print_term_to_buf_(query *q, cell *c, pl_idx c_ctx, int running, int cons, unsigned print_depth, unsigned depth, visit *);

static void print_iso_list(query *q, cell *c, pl_idx c_ctx, int running, bool cons, unsigned print_depth, unsigned depth, visit *visited)
{
	visit *save_visited = visited;
	cell *orig_c = c;
	pl_idx orig_c_ctx = c_ctx;
	unsigned print_list = 0;
	bool any1 = false, any2 = false;

	while (is_iso_list(c)) {
		cell *save_c = c;
		pl_idx save_c_ctx = c_ctx;

		if (q->max_depth && (print_list >= q->max_depth)) {
			SB_ungetchar(q->sb);
			SB_sprintf(q->sb, "%s", "|...]");
			q->last_thing = WAS_OTHER;
			q->cycle_error = true;
			break;
		}

		if (!cons) {
			SB_sprintf(q->sb, "%s", "[");
			q->last_thing = WAS_OTHER;
		}

		cell *head = c + 1;
		pl_idx head_ctx = c_ctx;
		cell *save_head = head;
		pl_idx save_head_ctx = head_ctx;
		if (running) head  = deref(q, head, head_ctx);
		if (running) head_ctx = q->latest_ctx;
		uint64_t save_vgen = 0;
		int parens = 0;

		if (has_visited(visited, head, head_ctx)) {
			cell v = *(c+1);
			pl_idx v_ctx = c_ctx;

#if 0
			if (is_var(c+1)) printf("*** c+1 = %u/%u\n", (c+1)->var_nbr, c_ctx);
			if (is_var(head)) printf("*** head = %u/%u\n", head->var_nbr, head_ctx);
			if (is_var(save_head)) printf("*** save_head = %u/%u\n", save_head->var_nbr, save_head_ctx);
			if (is_var(save_c)) printf("*** save_c = %u/%u\n", save_c->var_nbr, save_c_ctx);
			if (is_var(orig_c)) printf("*** orig_c = %u/%u\n", orig_c->var_nbr, orig_c_ctx);
#endif

			if (q->portray_vars || q->do_dump_vars) {
				//SB_sprintf(q->sb, "%s", q->p->vartab.var_name[q->dump_var_nbr]);
				SB_sprintf(q->sb, "%s", q->p->vartab.var_name[save_head->var_nbr]);
				//print_variable(q, save_head, save_head_ctx, running);
			} else {
				SB_sprintf(q->sb, "%s", "...");
			}
		} else {
			bool special_op = false;

			if (is_interned(head)) {
				special_op = (
					!strcmp(C_STR(q, head), ",")
					|| !strcmp(C_STR(q, head), "|")
					|| !strcmp(C_STR(q, head), ";")
					|| !strcmp(C_STR(q, head), ":-")
					|| !strcmp(C_STR(q, head), "->")
					|| !strcmp(C_STR(q, head), "*->")
					|| !strcmp(C_STR(q, head), "-->"));
			}

			visit me = {.next = visited, .c = head, .c_ctx = head_ctx};
			parens = is_compound(head) && special_op;
			if (parens) {  SB_sprintf(q->sb, "%s", "("); }
			q->parens = parens;
			print_term_to_buf_(q, head, head_ctx, running, -1, 0, depth+1, &me);
			q->parens = false;
		}

		q->cycle_error = false;
		if (parens) { SB_sprintf(q->sb, "%s", ")"); }
		bool possible_chars = false;

		if (is_interned(head) && (C_STRLEN_UTF8(head) == 1) && q->double_quotes)
			possible_chars = true;

		cell *tail = c + 1; tail += tail->nbr_cells;
		pl_idx tail_ctx = c_ctx;
		cell *save_tail = tail;
		pl_idx save_tail_ctx = tail_ctx;
		if (running) tail = deref(q, tail, tail_ctx);
		if (running) tail_ctx = q->latest_ctx;

		if (has_visited(visited, tail, tail_ctx)
			|| ((tail == save_c) && (tail_ctx == save_c_ctx))
			|| (q->max_depth && (print_depth >= q->max_depth))) {
			SB_sprintf(q->sb, "%s", "|");

#if 0
			if (is_var(c+1)) printf("*** c+1 = %u/%u\n", (c+1)->var_nbr, c_ctx);
			if (is_var(tail)) printf("*** tail = %u/%u\n", tail->var_nbr, tail_ctx);
			if (is_var(save_tail)) printf("*** save_tail = %u/%u\n", save_tail->var_nbr, save_tail_ctx);
			if (is_var(save_c)) printf("*** save_c = %u/%u\n", save_c->var_nbr, save_c_ctx);
			if (is_var(orig_c)) printf("*** orig_c = %u/%u\n", orig_c->var_nbr, orig_c_ctx);
#endif

			cell v = *(c+1);
			pl_idx v_ctx = c_ctx;

			if (is_var(c+1) && !q->do_dump_vars) {
				v = *(c+1);
				v_ctx = c_ctx;
			} else if (is_var(save_tail) && !q->do_dump_vars) {
				v = *save_tail;
				v_ctx = save_tail_ctx;
			} else {
				v.var_nbr = q->dump_var_nbr;
				v_ctx = 0;
			}

			if (q->portray_vars || q->do_dump_vars) {
				//SB_sprintf(q->sb, "%s", q->p->vartab.var_name[q->dump_var_nbr]);
				SB_sprintf(q->sb, "%s", q->p->vartab.var_name[save_tail->var_nbr]);
				//print_variable(q, save_head, save_head_ctx, running);
			} else {
				SB_sprintf(q->sb, "%s", "...");
			}

			SB_sprintf(q->sb, "%s", "]");
			q->last_thing = WAS_OTHER;
			q->cycle_error = true;
			break;
		}

		size_t tmp_len = 0;

		if (is_interned(tail) && !is_compound(tail)) {
			const char *src = C_STR(q, tail);

			if (strcmp(src, "[]")) {
				SB_sprintf(q->sb, "%s", "|");
				print_term_to_buf_(q, tail, tail_ctx, running, true, depth+1, depth+1, visited);
			}
		} else if (q->st.m->flags.double_quote_chars && running
			&& !q->ignore_ops && possible_chars
			&& (tmp_len = scan_is_chars_list(q, tail, tail_ctx, false) > 0))
			{
			char *tmp_src = chars_list_to_string(q, tail, tail_ctx, tmp_len);

			if ((strlen(tmp_src) == 1) && (*tmp_src == '\'')) {
				SB_sprintf(q->sb, "|\"%s\"", tmp_src);
			} else if ((strlen(tmp_src) == 1) && needs_quoting(q->st.m, tmp_src, 1)) {
				SB_sprintf(q->sb, ",'%s'", tmp_src);
			} else if (strlen(tmp_src) == 1) {
				SB_sprintf(q->sb, ",%s", tmp_src);
			} else {
				SB_sprintf(q->sb, "|\"%s\"", tmp_src);
			}

			free(tmp_src);
			print_list++;
		} else if (is_string(tail) && !q->double_quotes) {
			SB_sprintf(q->sb, "%s", ",");
			print_string_list(q, tail, tail_ctx, running, 1, depth+1);
			SB_sprintf(q->sb, "%s", "]");
			q->last_thing = WAS_OTHER;
			//q->cycle_error = true;
			break;
		} else if (is_iso_list(tail)) {
			if ((tail == save_c) && (tail_ctx == save_c_ctx) && running) {
				SB_sprintf(q->sb, "%s", "|");
				q->cycle_error = true;

				if (q->is_dump_vars) {
					if (!dump_variable(q, save_tail, save_tail_ctx, running))
						print_variable(q, save_tail, save_tail_ctx, 0);
				} else
					print_variable(q, save_tail, save_tail_ctx, 1);
			} else {
				SB_sprintf(q->sb, "%s", ",");
				q->last_thing = WAS_COMMA;
				c = tail;
				c_ctx = tail_ctx;
				print_list++;
				cons = true;
				continue;
			}
		} else if (is_string(tail) && q->double_quotes) {
			SB_sprintf(q->sb, "%s", "|\"");
			SB_strcat_and_free(q->sb, formatted(C_STR(q, tail), C_STRLEN(q, tail), true, false));
			SB_sprintf(q->sb, "%s", "\"");
			print_list++;
			q->last_thing = WAS_OTHER;
		} else {
			SB_sprintf(q->sb, "%s", "|");

			if (is_var(tail)) {
				print_variable(q, tail, tail_ctx, running);
			} else {
				visit *me = malloc(sizeof(visit));;
				me->next = visited;
				me->c = tail;
				me->c_ctx = tail_ctx;
				visited = me;
				unsigned specifier = 0;
				unsigned priority = search_op(q->st.m, C_STR(q, tail), &specifier, false);
				bool parens = is_infix(tail) && (priority >= 1000);
				if (parens) { SB_sprintf(q->sb, "%s", "("); }
				print_term_to_buf_(q, tail, tail_ctx, running, true, depth+1, depth+1, visited);
				if (parens) { SB_sprintf(q->sb, "%s", ")"); }
			}
		}

		if (!cons || print_list) {
			SB_sprintf(q->sb, "%s", "]");
			q->last_thing = WAS_OTHER;
		}

		break;
	}

	clear_visited(visited, save_visited);
}

static bool print_term_to_buf_(query *q, cell *c, pl_idx c_ctx, int running, int cons, unsigned print_depth, unsigned depth, visit *visited)
{
	cell *save_c = c;
	pl_idx save_c_ctx = c_ctx;

	if (depth > g_max_depth) {
		//printf("*** OOPS %s %d\n", __FILE__, __LINE__);
		q->cycle_error = true;
		q->last_thing = WAS_OTHER;
		return false;
	}

	if (q->is_dump_vars && (c->tag == TAG_INTEGER) && (c->flags & FLAG_INT_STREAM)) {
		int n = get_stream(q, c);
		stream *str = n >= 0 ? &q->pl->streams[n] : NULL;
		sliter *iter = str && str->alias ? sl_first(str->alias) : NULL;

		if (iter && sl_next(iter, NULL)) {
			const char *alias = sl_key(iter);

			if (strcmp(alias, "user_input") && strcmp(alias, "user_output") && strcmp(alias, "user_error")) {
				SB_strcat_and_free(q->sb, formatted(alias, strlen(alias), false, q->json));
			} else if (str->is_queue) {
				SB_sprintf(q->sb, "'<$queue>'(%d)", (int)get_smallint(c));
			} else if (str->is_mutex) {
				SB_sprintf(q->sb, "'<$mutex>'(%d)", (int)get_smallint(c));
			} else if (str->is_thread) {
				SB_sprintf(q->sb, "'<$thread>'(%d)", (int)get_smallint(c));
			} else {
				SB_sprintf(q->sb, "'<$stream>'(%d)", (int)get_smallint(c));
			}

			sl_done(iter);
		} else if (str && str->is_queue) {
			SB_sprintf(q->sb, "'<$queue>'(%d)", (int)get_smallint(c));
		} else if (str && str->is_mutex) {
			SB_sprintf(q->sb, "'<$mutex>'(%d)", (int)get_smallint(c));
		} else if (str && str->is_thread) {
			SB_sprintf(q->sb, "'<$thread>'(%d)", (int)get_smallint(c));
		} else {
			SB_sprintf(q->sb, "'<$stream>'(%d)", (int)get_smallint(c));
		}

		q->last_thing = WAS_OTHER;
		return true;
	}

	if ((c->tag == TAG_INTEGER) && (c->flags & FLAG_INT_STREAM)) {
		int n = get_stream(q, c);
		stream *str = &q->pl->streams[n];

		if (str->is_queue) {
			SB_sprintf(q->sb, "'<$queue>'(%d)", (int)get_smallint(c));
		} else if (str->is_mutex) {
			SB_sprintf(q->sb, "'<$mutex>'(%d)", (int)get_smallint(c));
		} else if (str->is_thread) {
			SB_sprintf(q->sb, "'<$thread>'(%d)", (int)get_smallint(c));
		} else {
			SB_sprintf(q->sb, "'<$stream>'(%d)", (int)get_smallint(c));
		}

		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_blob(c)) {
		SB_sprintf(q->sb, "'<$blob>'(%p)", c->val_ptr);
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_indirect(c)) {
		c = c->val_ptr;
		c_ctx = c->var_ctx;
	}

	if (is_number(c) && is_negative(c)) {
		if (is_negative(c) && (q->last_thing == WAS_SYMBOL)) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}
	}

	if (is_rational(c)) {
		int radix = 10;
		size_t len = mp_int_string_len(&c->val_bigint->irat.num, radix) - 1;
		char *dst2 = malloc(len+1);
		mp_int_to_string(&c->val_bigint->irat.num, radix, dst2, len+1);
		SB_sprintf(q->sb, "%s", dst2);
		free(dst2);
		SB_sprintf(q->sb, "%s", " div ");
		len = mp_int_string_len(&c->val_bigint->irat.den, radix) - 1;
		dst2 = malloc(len+1);
		mp_int_to_string(&c->val_bigint->irat.den, radix, dst2, len+1);
		SB_sprintf(q->sb, "%s", dst2);
		free(dst2);
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_bigint(c)) {
		int radix = 10;

		if (q->listing) {
			if (q->listing) {
				if (c->flags & FLAG_INT_BINARY)
					radix = 2;
				else if (c->flags & FLAG_INT_HEX)
					radix = 16;
				else if ((c->flags & FLAG_INT_OCTAL) && !running)
					radix = 8;
			}

			if (c->flags & FLAG_INT_BINARY) {
				SB_sprintf(q->sb, "%s0b", is_negative(c)?"-":"");
			} else if (c->flags & FLAG_INT_HEX) {
				SB_sprintf(q->sb, "%s0x", is_negative(c)?"-":"");
			} else if ((c->flags & FLAG_INT_OCTAL) && !running) {
				SB_sprintf(q->sb, "%s0o", is_negative(c)?"-":"");
			}
		}

		size_t len = mp_int_string_len(&c->val_bigint->ival, radix) - 1;
		char *dst2 = malloc(len+1);
		mp_int_to_string(&c->val_bigint->ival, radix, dst2, len+1);
		SB_sprintf(q->sb, "%s", dst2);
		free(dst2);
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_smallint(c)) {
		//if (dstlen) printf("*** int %d,  was=%d, is=%d\n", (int)c->val_int, q->last_thing_was_symbol, false);
		char tmpbuf[256];

		if (q->listing) {
			if (((c->flags & FLAG_INT_HEX) || (c->flags & FLAG_INT_BINARY))) {
				SB_sprintf(q->sb, "%s0x", get_smallint(c)<0?"-":"");
				sprint_int(tmpbuf, sizeof(tmpbuf), get_smallint(c), 16);
				SB_sprintf(q->sb, "%s", tmpbuf);
			} else if ((c->flags & FLAG_INT_OCTAL) && !running) {
				SB_sprintf(q->sb, "%s0o", get_smallint(c)<0?"-":"");
				sprint_int(tmpbuf, sizeof(tmpbuf), get_smallint(c), 8);
				SB_sprintf(q->sb, "%s", tmpbuf);
			} else {
				sprint_int(tmpbuf, sizeof(tmpbuf), get_smallint(c), 10);
				SB_sprintf(q->sb, "%s", tmpbuf);
			}
		} else {
			sprint_int(tmpbuf, sizeof(tmpbuf), get_smallint(c), 10);
			SB_sprintf(q->sb, "%s", tmpbuf);
		}

		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_float(c) && (get_float(c) == M_PI)) {
		SB_sprintf(q->sb, "%s", "3.141592653589793");
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_float(c) && (get_float(c) == M_E)) {
		SB_sprintf(q->sb, "%s", "2.718281828459045");
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_float(c)) {
		if (c->val_float == 0.0)
			c->val_float = fabs(c->val_float);

		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", 17, get_float(c));
		if (!q->json) reformat_float(q, tmpbuf, c->val_float);
		SB_sprintf(q->sb, "%s", tmpbuf);
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_string(c) && !q->ignore_ops && q->double_quotes && 0) {
		SB_sprintf(q->sb, "%s", "\"");
		SB_strcat_and_free(q->sb, formatted(C_STR(q, c), C_STRLEN(q, c), true, q->json));
		SB_sprintf(q->sb, "%s", "\"");
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_string(c) && q->ignore_ops) {
		print_string_canonical(q, c, c_ctx, running, cons > 0, depth+1);
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_string(c) && !q->double_quotes) {
			print_string_list(q, c, c_ctx, running, cons > 0, depth+1);
		q->last_thing = WAS_OTHER;
		return true;
	}

	int is_chars_list = is_string(c) && q->double_quotes;
	bool possible_chars = false;

	if (is_interned(c) && (C_STRLEN_UTF8(c) == 1) && !q->ignore_ops && q->double_quotes)
		possible_chars = true;

	if (!is_chars_list && running && possible_chars
		&& (scan_is_chars_list(q, c, c_ctx, false) > 0))
		is_chars_list += q->st.m->flags.double_quote_chars && scan_is_chars_list(q, c, c_ctx, false);

	if (is_chars_list) {
		cell *l = c;
		SB_sprintf(q->sb, "%s", "\"");
		unsigned cnt = 0;
		LIST_HANDLER(l);
		bool closing_quote = true;

		while (is_list(l)) {
			if (q->max_depth && (cnt++ >= q->max_depth)) {
				SB_sprintf(q->sb, "%s", "\"||... ");
				closing_quote = false;
				break;
			}

			cell *h = LIST_HEAD(l);
			cell *c = running ? deref(q, h, c_ctx) : h;
			SB_strcat_and_free(q->sb, formatted(C_STR(q, c), C_STRLEN(q, c), true, q->json));
			l = LIST_TAIL(l);
			l = running ? deref(q, l, c_ctx) : l;
			c_ctx = running ? q->latest_ctx : 0;
		}

		if (closing_quote) SB_sprintf(q->sb, "%s", "\"");
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_iso_list(c) && !q->ignore_ops) {
		print_iso_list(q, c, c_ctx, running, cons > 0, print_depth+1, depth+1, visited);
		q->last_thing = WAS_OTHER;
		return true;
	}

	const char *src = !is_ref(c) ? C_STR(q, c) : "_";
	size_t src_len = !is_ref(c) ? C_STRLEN(q, c) : 1;
	unsigned specifier = 0, pri = 0, spec = 0;
	unsigned my_priority = search_op(q->st.m, src, &specifier, false);
	bool is_op = IS_OP(c);

	if (!is_op && !is_var(c) && (c->arity == 1)
		&& (pri = search_op(q->st.m, src, &spec, true))) {
		if (IS_PREFIX(spec)) {
			SET_OP(c, spec);
			specifier = spec;
			my_priority = pri;
		}
	}

	if (!specifier)
		CLR_OP(c);

	if (q->ignore_ops || !IS_OP(c) || !c->arity) {
		bool is_needs_quoting = needs_quoting(q->st.m, src, src_len);
		int quote = ((running <= 0) || q->quoted) && !is_var(c) && is_needs_quoting;
		int dq = 0, braces = 0;
		if (is_string(c) && q->double_quotes) dq = quote = 1;
		if (q->quoted < 0) quote = 0;
		if ((c->arity == 1) && is_interned(c) && !strcmp(src, "{}")) braces = 1;
		cell *c1 = c->arity ? deref(q, FIRST_ARG(c), c_ctx) : NULL;

		if (running && is_interned(c) && c->arity
			&& q->numbervars && !strcmp(src, "$VAR") && c1
			&& is_integer(c1) && (get_smallint(c1) >= 0)) {
			SB_sprintf(q->sb, "%s", varformat2(q->pl->tmpbuf, sizeof(q->pl->tmpbuf), c1, 0));
			q->last_thing = WAS_OTHER;
			return true;
		}

		if (is_var(c) /*&& !is_anon(c)*/ && q->variable_names) {
			if (dump_variable(q, c, c_ctx, running))
				return true;
		}

		SB_sprintf(q->sb, "%s", !braces&&quote?dq?"\"":"'":"");

		if (is_var(c)) {
			print_variable(q, c, c_ctx, running);
			q->last_thing = WAS_OTHER;
			return true;
		}

		unsigned len_str = src_len;

		if (braces && !q->ignore_ops)
			;
		else if (quote) {
			if (is_blob(c) && q->max_depth && (len_str >= q->max_depth) && (src_len > 128))
				len_str = q->max_depth;

			SB_strcat_and_free(q->sb, formatted(src, len_str, dq, q->json));

			if (is_blob(c) && q->max_depth && (len_str > q->max_depth) && (src_len > 128)) {
				SB_ungetchar(q->sb);
				SB_sprintf(q->sb, "%s", "...");
				q->last_thing = WAS_SYMBOL;
			} else
				q->last_thing = WAS_OTHER;
		} else {
			int ch = peek_char_utf8(src);
			bool is_symbol = !needs_quoting(q->st.m, src, src_len) && !iswalpha(ch)
				&& strcmp(src, "\\") && strcmp(src, ",") && strcmp(src, ";")
				&& strcmp(src, "[]") && strcmp(src, "{}") && !q->parens;

			if ((q->last_thing == WAS_SYMBOL) && is_symbol && !q->parens && !q->quoted) {
				SB_sprintf(q->sb, "%s", " ");
				q->last_thing = WAS_SPACE;
			}

			SB_strcatn(q->sb, src, len_str);
			q->last_thing = is_symbol ? WAS_SYMBOL : WAS_OTHER;;
		}

		SB_sprintf(q->sb, "%s", !braces&&quote?dq?"\"":"'":"");
		q->did_quote = !braces&&quote;

		if (is_compound(c) && !is_string(c)) {
			pl_idx arity = c->arity;
			SB_sprintf(q->sb, "%s", braces&&!q->ignore_ops?"{":"(");
			q->parens = true;
			bool any = false;

			for (c++; arity--; c += c->nbr_cells) {
				cell *tmp = c;
				pl_idx tmp_ctx = c_ctx;
				if (running) tmp = deref(q, tmp, tmp_ctx);
				if (running) tmp_ctx = q->latest_ctx;

				if (has_visited(visited, tmp, tmp_ctx)) {
					tmp = c;
					tmp_ctx = c_ctx;
					SB_sprintf(q->sb, "%s", !is_ref(tmp) ? C_STR(q, tmp) : "_");
					if (arity) {SB_sprintf(q->sb, "%s", ","); }
					continue;
				}

				if (q->max_depth && ((depth+!braces) >= q->max_depth)) {
					SB_sprintf(q->sb, "%s", "...");
					q->last_thing = WAS_SYMBOL;

					if (arity) {
						SB_sprintf(q->sb, "%s", ",");
						q->last_thing = WAS_OTHER;
					}
					continue;
				}

				bool parens = false;

				if (!braces && is_interned(tmp) && !q->ignore_ops) {
					unsigned tmp_priority = search_op(q->st.m, C_STR(q, tmp), NULL, tmp->arity==1);

					if ((tmp_priority >= 1000) && tmp->arity)
						q->parens = parens = true;
				}

				if (parens) { SB_sprintf(q->sb, "%s", "("); }

				visit me = {.next = visited, .c = tmp, .c_ctx = tmp_ctx};
				q->parens = parens;
				print_term_to_buf_(q, tmp, tmp_ctx, running, 0, depth+1, depth+1, &me);
				q->parens = false;
				if (parens) {SB_sprintf(q->sb, "%s", ")"); }
				if (arity) {SB_sprintf(q->sb, "%s", ","); }
			}

			SB_sprintf(q->sb, "%s", braces&&!q->ignore_ops?"}":")");
			q->parens = false;
		}

		if (q->last_thing != WAS_SYMBOL)
			q->last_thing = WAS_OTHER;

		return true;
	}

	size_t srclen = src_len;

	if (is_postfix(c)) {
		cell *lhs = c + 1;
		cell *save_lhs = lhs;
		pl_idx lhs_ctx = c_ctx;
		if (running) lhs = deref(q, lhs, lhs_ctx);
		if (running) lhs_ctx = q->latest_ctx;

		bool any = false;

		if (!is_var(lhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
			if (q->last_thing != WAS_SPACE) SB_sprintf(q->sb, "%s", " ");
			SB_sprintf(q->sb, "%s", "...");
			q->last_thing = WAS_SYMBOL;
			return true;
		} else {
			visit me;
			me.next = visited;
			me.c = lhs;
			me.c_ctx = lhs_ctx;
			pl_idx lhs_ctx = running ? q->latest_ctx : 0;
			print_term_to_buf_(q, lhs, lhs_ctx, running, 0, 0, depth+1, &me);
		}

		bool space = (c->val_off == g_minus_s) && (is_number(lhs) || search_op(q->st.m, C_STR(q, lhs), NULL, true));
		if ((c->val_off == g_plus_s) && search_op(q->st.m, C_STR(q, lhs), NULL, true) && lhs->arity) space = true;
		if (isalpha(*src)) space = true;

		if (has_visited(visited, lhs, lhs_ctx)) {
			if (q->is_dump_vars) {
				SB_sprintf(q->sb, "%s", !is_ref(save_lhs) ? C_STR(q, save_lhs) : "_");
			} else
				print_variable(q, save_lhs, lhs_ctx, 1);

			q->last_thing = WAS_OTHER;
			return true;
		}

		if ((q->last_thing != WAS_SPACE) && space) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		int quote = q->quoted && has_spaces(src, src_len);
		if (quote) { SB_sprintf(q->sb, "%s", quote?" '":""); }

		SB_strcatn(q->sb, src, srclen);
		if (quote) { SB_sprintf(q->sb, "%s", quote?"'":""); }
		q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_prefix(c)) {
		if (q->last_thing == WAS_SYMBOL) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		cell *rhs = c + 1;
		cell *save_rhs = rhs;
		pl_idx save_rhs_ctx = c_ctx;
		pl_idx rhs_ctx = c_ctx;
		if (running) rhs = deref(q, rhs, rhs_ctx);
		if (running) rhs_ctx = q->latest_ctx;
		bool any = false;

		unsigned my_priority = search_op(q->st.m, src, NULL, true);
		unsigned rhs_pri = is_interned(rhs) ? search_op(q->st.m, C_STR(q, rhs), NULL, true) : 0;

		if ((q->last_thing == WAS_SYMBOL) && !strcmp(src, "\\+")) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		bool space = (c->val_off == g_minus_s) && (is_number(rhs) || search_op(q->st.m, C_STR(q, rhs), NULL, true));
		if ((c->val_off == g_plus_s) && search_op(q->st.m, C_STR(q, rhs), NULL, true) && rhs->arity) space = true;
		if (isalpha(*src)) space = true;
		if (is_op(rhs) || is_negative(rhs) || is_float(rhs)) space = true;

		bool parens = false;
		if (!strcmp(src, "+") && (is_infix(rhs) || is_postfix(rhs))) parens = true;
		if (rhs_pri > my_priority) parens = true;
		if (my_priority && (rhs_pri == my_priority) && strcmp(src, "-") && strcmp(src, "+")) parens = true;
		if (!strcmp(src, "-") && (rhs_pri == my_priority) && (rhs->arity > 1)) parens = true;
		if ((c->val_off == g_minus_s) && is_number(rhs) && !is_negative(rhs)) parens = true;
		if ((c->val_off == g_minus_s) && search_op(q->st.m, C_STR(q, rhs), NULL, true) && !rhs->arity) parens = true;
		if ((c->val_off == g_plus_s) && search_op(q->st.m, C_STR(q, rhs), NULL, true) && !rhs->arity) parens = true;

		bool quote = q->quoted && has_spaces(src, src_len);

		if (is_interned(rhs) && !rhs->arity && !parens) {
			const char *rhs_src = C_STR(q, rhs);
			if (!iswalpha(*rhs_src) && !isdigit(*rhs_src) && strcmp(rhs_src, "[]") && strcmp(rhs_src, "{}"))
				space = 1;
		}

		if (quote) { SB_sprintf(q->sb, "%s", quote?"'":""); }
		SB_strcatn(q->sb, src, srclen);
		if (quote) { SB_sprintf(q->sb, "%s", quote?"' ":""); }

		if (has_visited(visited, rhs, rhs_ctx)) {
			if (q->is_dump_vars) {
				SB_sprintf(q->sb, "%s", !is_ref(save_rhs) ? C_STR(q, save_rhs) : "_");
			} else
				print_variable(q, save_rhs, rhs_ctx, 1);

			q->last_thing = WAS_OTHER;
			return true;
		}

		if (space || parens) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		if (!is_var(rhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
			if (!space) { SB_sprintf(q->sb, "%s", " "); }
			SB_sprintf(q->sb, "%s", "...");
			q->last_thing = WAS_SYMBOL;
			return true;
		}

		visit me;
		me.next = visited;
		me.c = rhs;
		me.c_ctx = rhs_ctx;

		q->last_thing = WAS_OTHER;
		if (parens) { SB_sprintf(q->sb, "%s", "("); q->last_thing = WAS_OTHER; }
		q->parens = parens;
		print_term_to_buf_(q, rhs, rhs_ctx, running, 0, 0, depth+1, &me);
		q->parens = false;
		if (parens) { SB_sprintf(q->sb, "%s", ")"); q->last_thing = WAS_OTHER; }
		return true;
	}

	// Infix..

	cell *lhs = c + 1;
	cell *save_lhs = lhs;
	pl_idx lhs_ctx = c_ctx;
	cell *rhs = lhs + lhs->nbr_cells;
	cell *save_rhs = rhs;
	pl_idx rhs_ctx = c_ctx;
	if (running) lhs = deref(q, lhs, lhs_ctx);
	if (running) lhs_ctx = q->latest_ctx;
	if (running) rhs = deref(q, rhs, rhs_ctx);
	if (running) rhs_ctx = q->latest_ctx;
	bool any = false;

	// Print LHS..

	unsigned lhs_pri_1 = is_interned(lhs) ? search_op(q->st.m, C_STR(q, lhs), NULL, is_prefix(rhs)) : 0;
	unsigned lhs_pri_2 = is_interned(lhs) && !lhs->arity ? search_op(q->st.m, C_STR(q, lhs), NULL, false) : 0;

	bool lhs_parens = lhs_pri_1 >= my_priority;
	if ((lhs_pri_1 == my_priority) && is_yfx(c)) lhs_parens = false;
	if (lhs_pri_2 > 0) lhs_parens = true;
	if (is_compound(lhs) && (lhs_pri_1 <= my_priority) && (lhs->val_off == g_plus_s)) { lhs_parens = false; }
	bool lhs_space = false;

	if ((q->last_thing != WAS_SPACE) && lhs_space) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	if (!is_var(lhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
		if (q->last_thing != WAS_SPACE) SB_sprintf(q->sb, "%s", " ");
		SB_sprintf(q->sb, "%s", "...");
		q->last_thing = WAS_SYMBOL;
	} else if (has_visited(visited, lhs, lhs_ctx)) {
		if (q->is_dump_vars) {
			SB_sprintf(q->sb, "%s", !is_ref(save_lhs) ? C_STR(q, save_lhs) : "_");
		} else
			print_variable(q, save_lhs, lhs_ctx, 1);

		q->last_thing = WAS_OTHER;
	} else {
		visit me;
		me.next = visited;
		me.c = lhs;
		me.c_ctx = lhs_ctx;
		if (lhs_parens) { SB_sprintf(q->sb, "%s", "("); }
		q->parens = lhs_parens;
		print_term_to_buf_(q, lhs, lhs_ctx, running, 0, 0, depth+1, &me);
		q->parens = false;
		if (lhs_parens) { SB_sprintf(q->sb, "%s", ")"); }
	}

	bool space = false;

	if (is_interned(lhs) && !lhs->arity && !lhs_parens) {
		const char *lhs_src = C_STR(q, lhs);
		if (!isalpha(*lhs_src) && !isdigit(*lhs_src) && (*lhs_src != '$')
			&& strcmp(src, ",") && strcmp(src, ";")
			&& strcmp(lhs_src, "[]") && strcmp(lhs_src, "{}")
			)
			space = true;
	}

	if ((q->last_thing != WAS_SPACE) && space) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	int ch = peek_char_utf8(src);
	bool is_symbol = !needs_quoting(q->st.m, src, src_len)
		&& !iswalpha(ch) && strcmp(src, ",") && strcmp(src, ";")
		&& strcmp(src, "[]") && strcmp(src, "{}") && !q->parens;

	//if (dstlen) printf("*** op '%s',  was=%d, is=%d\n", src, q->last_thing_was_symbol, is_symbol);

	if (!*src || ((q->last_thing == WAS_SYMBOL) && is_symbol && !lhs_parens && !q->parens))
		space = true;

	if ((q->last_thing != WAS_SPACE) && !is_symbol && space) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	// Print OP..

	//q->last_thing_was_symbol += is_symbol;
	space = iswalpha(*src) || (q->last_thing == WAS_SYMBOL);

	if (q->listing && !depth && !strcmp(src, ":-"))
		space = true;

	if ((q->last_thing != WAS_SPACE) && space) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	int quote = q->quoted && has_spaces(src, src_len);
	if (op_needs_quoting(q->st.m, src, src_len)) quote = 1;
	if (quote) { SB_sprintf(q->sb, "%s", quote?" '":""); }
	SB_strcatn(q->sb, src, srclen);
	if (quote) { SB_sprintf(q->sb, "%s", quote?"' ":""); }
	q->last_thing = strcmp(src, "|") ? WAS_SYMBOL : WAS_OTHER;

	if (q->listing && !depth && !strcmp(src, ":-")) {
		SB_sprintf(q->sb, "%s", "\n  ");
	}

	if ((q->last_thing != WAS_SPACE) && space) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	// Print RHS..

	unsigned rhs_pri_1 = is_interned(rhs) ? search_op(q->st.m, C_STR(q, rhs), NULL, is_prefix(rhs)) : 0;
	unsigned rhs_pri_2 = is_interned(rhs) && !rhs->arity ? search_op(q->st.m, C_STR(q, rhs), NULL, false) : 0;
	bool rhs_parens = rhs_pri_1 >= my_priority;
	space = is_number(rhs) && is_negative(rhs);

	if (!rhs_parens && is_prefix(rhs) && strcmp(src, "|"))
		space = true;

	bool rhs_is_symbol = is_interned(rhs) && !rhs->arity
		&& !iswalpha(*C_STR(q, rhs))
		&& !needs_quoting(q->st.m, C_STR(q, rhs), C_STRLEN(q, rhs))
		&& strcmp(C_STR(q, rhs), "[]") && strcmp(C_STR(q, rhs), "{}")
		&& !rhs_parens;

	if (is_atom(rhs) && (peek_char_utf8(C_STR(q, rhs)) > 255)
		&& !needs_quoting(q->st.m, C_STR(q, rhs), C_STRLEN(q, rhs))
		)
		space = true;

	if (rhs_is_symbol && strcmp(C_STR(q, rhs), "[]") && strcmp(C_STR(q, rhs), "{}") && strcmp(C_STR(q, rhs), "!"))
		space = true;

	if ((rhs_pri_1 == my_priority) && is_xfy(c))
		rhs_parens = false;

	if (rhs_pri_2 > 0)
		rhs_parens = true;

	if ((q->last_thing != WAS_SPACE) && space && !rhs_parens) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	if (!is_var(rhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
		if (q->last_thing != WAS_SPACE) SB_sprintf(q->sb, "%s", " ");
		SB_sprintf(q->sb, "%s", "...");
		q->last_thing = WAS_SYMBOL;
	} else if (has_visited(visited, rhs, rhs_ctx)) {
		if (q->is_dump_vars) {
			SB_sprintf(q->sb, "%s", !is_ref(save_rhs) ? C_STR(q, save_rhs) : "_");
		} else
			print_variable(q, save_rhs, rhs_ctx, 1);

		q->last_thing = WAS_OTHER;
	} else {
		visit me;
		me.next = visited;
		me.c = rhs;
		me.c_ctx = rhs_ctx;
		if (rhs_parens) { SB_sprintf(q->sb, "%s", "("); q->last_thing = WAS_OTHER; }
		q->parens = rhs_parens || space;
		print_term_to_buf_(q, rhs, rhs_ctx, running, 0, 0, depth+1, &me);
		q->parens = false;
		if (rhs_parens) { SB_sprintf(q->sb, "%s", ")"); q->last_thing = WAS_OTHER; }
		else if (rhs_is_symbol) { q->last_thing = WAS_SYMBOL; }
	}

	return true;
}

bool print_term_to_buf(query *q, cell *c, pl_idx c_ctx, int running, int cons)
{
	visit me;
	me.next = NULL;
	me.c = c;
	me.c_ctx = c_ctx;
	return print_term_to_buf_(q, c, c_ctx, running, cons, 0, 0, &me);
}

char *print_canonical_to_strbuf(query *q, cell *c, pl_idx c_ctx, int running)
{
	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	SB_init(q->sb);
	print_term_to_buf(q, c, c_ctx, running, false);
	if (q->nl) SB_putchar(q->sb, '\n');
	q->ignore_ops = false;
	q->quoted = 0;
	char *buf = malloc(SB_strlen(q->sb)+1+1); // dcg_expansion needs this extra char space
	strcpy(buf, SB_cstr(q->sb));
	SB_free(q->sb);
	return buf;
}

bool print_canonical_to_stream(query *q, stream *str, cell *c, pl_idx c_ctx, int running)
{
	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	SB_init(q->sb);
	print_term_to_buf(q, c, c_ctx, running, false);
	if (q->nl) SB_putchar(q->sb, '\n');
	q->ignore_ops = false;
	q->quoted = 0;
	const char *src = SB_cstr(q->sb);
	ssize_t len = SB_strlen(q->sb);

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			SB_free(q->sb);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	SB_free(q->sb);
	return true;
}

bool print_canonical(query *q, FILE *fp, cell *c, pl_idx c_ctx, int running)
{
	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	SB_init(q->sb);
	print_term_to_buf(q, c, c_ctx, running, false);
	if (q->nl) SB_putchar(q->sb, '\n');
	q->ignore_ops = false;
	q->quoted = 0;
	const char *src = SB_cstr(q->sb);
	ssize_t len = SB_strlen(q->sb);

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			SB_free(q->sb);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	SB_free(q->sb);
	return true;
}

char *print_term_to_strbuf(query *q, cell *c, pl_idx c_ctx, int running)
{
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	//q->last_thing_was_space = true;
	SB_init(q->sb);
	print_term_to_buf(q, c, c_ctx, running, false);
	char *buf = malloc(SB_strlen(q->sb)+1+1); // dcg_expansion needs this extra char space
	strcpy(buf, SB_cstr(q->sb));
	SB_free(q->sb);
	return buf;
}

bool print_term_to_stream(query *q, stream *str, cell *c, pl_idx c_ctx, int running)
{
	q->did_quote = false;
	q->last_thing = WAS_SPACE;
	SB_init(q->sb);
	print_term_to_buf(q, c, c_ctx, running, false);
	if (q->nl) SB_putchar(q->sb, '\n');
	const char *src = SB_cstr(q->sb);
	ssize_t len = SB_strlen(q->sb);

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			SB_free(q->sb);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	SB_free(q->sb);
	return true;
}

bool print_term(query *q, FILE *fp, cell *c, pl_idx c_ctx, int running)
{
	q->did_quote = false;
	q->last_thing = WAS_SPACE;
	SB_init(q->sb);
	print_term_to_buf(q, c, c_ctx, running, false);
	if (q->nl) SB_putchar(q->sb, '\n');
	const char *src = SB_cstr(q->sb);
	ssize_t len = SB_strlen(q->sb);

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			SB_free(q->sb);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	SB_free(q->sb);
	return true;
}

void clear_write_options(query *q)
{
	q->print_idx = 0;
	q->max_depth = q->quoted = 0;
	q->nl = q->fullstop = q->varnames = q->ignore_ops = false;
	q->parens = q->numbervars = q->json = q->double_quotes = false;
	q->last_thing = WAS_OTHER;
	q->variable_names = NULL;
	q->cycle_error = false;
	memset(q->ignores, 0, sizeof(q->ignores));
}
