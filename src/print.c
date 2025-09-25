#include <ctype.h>
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "module.h"
#include "network.h"
#include "parser.h"
#include "query.h"

typedef struct visit_ visit;

struct visit_ {
	visit *next;
	cell *c;
	pl_ctx c_ctx;
};

static bool has_visited(visit *visited, cell *c, pl_ctx c_ctx)
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

cell *string_to_chars_list(query *q, cell *p, pl_ctx p_ctx)
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

char *chars_list_to_string(query *q, cell *p_chars, pl_ctx p_chars_ctx)
{
	LIST_HANDLER(p_chars);
	SB(pr);

	while (is_list(p_chars)) {
		cell *h = LIST_HEAD(p_chars);
		h = deref(q, h, p_chars_ctx);

		if (is_integer(h)) {
			int ch = get_smallint(h);
			SB_putchar(pr, ch);
		} else {
			const char *p = C_STR(q, h);
			int ch = peek_char_utf8(p);
			SB_putchar(pr, ch);
		}

		p_chars = LIST_TAIL(p_chars);
		p_chars = deref(q, p_chars, p_chars_ctx);
		p_chars_ctx = q->latest_ctx;
	}

	char *tmp = malloc(SB_strlen(pr)+1+1);	// Allow for optional '.' at end, plus null
	check_error(tmp);
	strcpy(tmp, SB_cstr(pr));
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

	if (iswupper(ch) || iswdigit(ch) || (ch == '_'))
		return true;

	const char *s = src;
	int slen = srclen;

	while (slen > 0) {
		slen -= len_char_utf8(s);
		int ch = get_char_utf8(&s);

		if (((ch < 256) && strchr(g_solo, ch))
			|| iswspace(ch) || iswblank(ch)
			|| (ch == 0xa0) || (ch == 0x85)
			|| (ch == 0x2007) || (ch == 0x202f)
			)
			return true;

		if (!iswalnum(ch) && (ch != '_') && !(iswgraph(ch) && (ch <= 255)))
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

	if (!srclen)
		return true;

	int ch = peek_char_utf8(src);

	if (iswupper(ch) || iswdigit(ch) || (ch == '_'))
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
		} else if (!json && (ch > 127) && (iswblank(ch) || iswspace(ch))) {
			SB_putchar(sb, '\\');
			SB_putchar(sb, 'x');
			SB_sprintf(sb, "%x", ch);
			SB_putchar(sb, '\\');
		} else if (!json && ((ch == 0x85) || (ch == 0xA0) || (ch == 0x2007) || (ch == 0x202f))) {
			SB_putchar(sb, '\\');
			SB_putchar(sb, 'x');
			SB_sprintf(sb, "%x", ch);
			SB_putchar(sb, '\\');
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

static void format_double(double num, char *res) {
	sprintf(res,"%.16g", num);

	if (strtod(res, NULL) != num)
		sprintf(res, "%.17g", num);
}

// Make sure we have a trailing dot if needed...

static void reformat_float(query *q, char *tmpbuf, pl_flt v)
{
	format_double(v, tmpbuf);
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

	mp_small num;
	mp_int_mod_value(&tmp, 26, &num);
	char *dst = tmpbuf;
	dst += sprintf(dst, "%c", 'A'+(unsigned)(num));
	mp_int_div_value(&tmp, 26, &tmp, NULL);

	if (mp_int_compare_zero(&tmp) > 0)
		dst += mp_int_to_string(&tmp, 10, dst, tmpbuf_len);

	mp_int_clear(&tmp);
	return tmpbuf;
}

static const char *varformat(char *tmpbuf, unsigned long long num, bool listing)
{
	char *dst = tmpbuf;
	dst += sprintf(dst, "%s%c", listing?"":"_", 'A'+(unsigned)(num%26));
	if ((num/26) > 0) dst += sprintf(dst, "%"PRIu64"", (int64_t)(num/26));
	return tmpbuf;
}

static const char *get_slot_name(query *q, pl_idx slot_nbr, bool listing)
{
	for (unsigned i = 0; i < q->print_idx; i++) {
		if (q->pl->tab1[i] == slot_nbr) {
			return varformat(q->tmpbuf, q->pl->tab2[i], listing);
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
	return varformat(q->tmpbuf, i, listing);
}

static void print_variable(query *q, cell *c, pl_ctx c_ctx, bool running)
{
	const frame *f = GET_FRAME(running ? c_ctx : 0);
	pl_idx slot_nbr = running ?
		(pl_idx)(get_actual_slot_num(q, f, c->var_num))
		: c->var_num;

	if (q->varnames && !is_anon(c) && running && !q->cycle_error && (c_ctx == 0)) {
		if (q->varnames && q->top->vartab.off[c->var_num]) {
			SB_sprintf(q->sb, "%s", GET_POOL(q, q->top->vartab.off[c->var_num]));
		} else {
			SB_sprintf(q->sb, "%s", get_slot_name(q, slot_nbr, q->listing||q->portray_vars));
		}
	} else if (q->portray_vars || (q->is_dump_vars && q->cycle_error)) {
		SB_sprintf(q->sb, "%s", get_slot_name(q, slot_nbr, q->listing||q->portray_vars));
	} else if (q->is_dump_vars) {
		if ((c_ctx == 0) && (c->var_num < q->top->num_vars)) {
			SB_sprintf(q->sb, "%s", GET_POOL(q, q->top->vartab.off[c->var_num]));
		} else {
			SB_sprintf(q->sb, "%s", get_slot_name(q, slot_nbr, q->listing||q->portray_vars));
		}
	} else if (q->listing && is_anon(c)) {
		SB_sprintf(q->sb, "%s", C_STR(q, c));
	} else if (q->listing) {
		SB_sprintf(q->sb, "%s", get_slot_name(q, slot_nbr, q->listing||q->portray_vars));
	} else if (!running && !is_ref(c)) {
		SB_sprintf(q->sb, "%s", C_STR(q, c));
	} else {
		SB_sprintf(q->sb, "_%u", (unsigned)slot_nbr);
	}

#if 0
	if (is_global(c)) {
		SB_sprintf(q->sb, "%s", "g");
	} else if (is_void(c)) {
		SB_sprintf(q->sb, "%s", "v");
	} else if (is_local(c)) {
		SB_sprintf(q->sb, "%s", "l");
	} else if (is_temporary(c)) {
		SB_sprintf(q->sb, "%s", "t");
	}
#endif
}

static bool dump_variable(query *q, cell *c, pl_ctx c_ctx, bool running)
{
	if (!q->variable_names)
		return false;

	cell *l = q->variable_names;
	pl_ctx l_ctx = q->variable_names_ctx;
	LIST_HANDLER(l);

	while (is_iso_list(l)) {
		cell *h = LIST_HEAD(l);
		h = running ? deref(q, h, l_ctx) : h;
		pl_ctx h_ctx = running ? q->latest_ctx : 0;
		cell *name = running ? deref(q, h+1, h_ctx) : h+1;
		cell *v = running ? deref(q, h+2, h_ctx) : h+2;
		pl_ctx v_ctx = running ? q->latest_ctx : 0;

		const frame *f = GET_FRAME(running ? v_ctx : 0);
		pl_idx slot_nbr = running ?
			(pl_idx)(get_actual_slot_num(q, f, v->var_num))
			: v->var_num;

		if (is_var(v) && (v->var_num == c->var_num) && (v_ctx == c_ctx)) {
			if (0 && !strcmp(C_STR(q, name), "_")) {
				print_variable(q, v, v_ctx, running);
			} else if (q->is_dump_vars && !strcmp(C_STR(q, name), "_")) {
				SB_sprintf(q->sb, "%s", get_slot_name(q, slot_nbr, q->listing||q->portray_vars));
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

	c = deref(q, c, c_ctx);
	c_ctx = q->latest_ctx;

	if (q->do_dump_vars && is_cyclic_term(q, c, c_ctx)) {
		SB_sprintf(q->sb, "%s", GET_POOL(q, q->top->vartab.off[q->dump_var_num]));
		return true;
	}

	//printf("*** no dump %u ctx=%u\n", c->var_num, c_ctx);
	return false;
}

static void print_string_canonical(query *q, cell *c, pl_ctx c_ctx, int running, bool cons, unsigned depth)
{
	unsigned cnt = 1;
	LIST_HANDLER(c);

	SB_sprintf(q->sb, "%s", "'.'(");

	while (is_list(c)) {
		cell *h = LIST_HEAD(c);

		if (is_number(h)) {
			SB_sprintf(q->sb, "%d", (int)h->val_int);
		} else if (needs_quoting(q->st.m, C_STR(q, h), C_STRLEN(q, h))) {
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

static void print_string_list(query *q, cell *c, pl_ctx c_ctx, int running, bool cons, unsigned depth)
{
	LIST_HANDLER(c);
	if (!cons) { SB_sprintf(q->sb, "%s", "["); }

	while (is_list(c)) {
		cell *h = LIST_HEAD(c);

		if (is_number(h)) {
			SB_sprintf(q->sb, "%d", (int)h->val_int);
		} else if (needs_quoting(q->st.m, C_STR(q, h), C_STRLEN(q, h)) && q->quoted) {
			SB_sprintf(q->sb, "%s", "'");
			SB_strcat_and_free(q->sb, formatted(C_STR(q, h), C_STRLEN(q, h), false, false));
			SB_sprintf(q->sb, "%s", "'");
		} else{
			SB_strcat_and_free(q->sb, formatted(C_STR(q, h), C_STRLEN(q, h), false, false));
		}

		c = LIST_TAIL(c);

		if (!is_list(c))
			break;

		SB_sprintf(q->sb, "%s", ",");
	}

	if (!cons) { SB_sprintf(q->sb, "%s", "]"); }
}

static bool print_term_to_buf_(query *q, cell *c, pl_ctx c_ctx, int running, int cons, unsigned print_depth, unsigned depth, visit *);

static void print_iso_list(query *q, cell *c, pl_ctx c_ctx, int running, bool cons, unsigned print_depth, unsigned depth, visit *visited)
{
	visit *save_visited = visited;
	pl_ctx orig_c_ctx = c_ctx;
	unsigned print_list = 0;

	while (is_iso_list(c)) {
		CHECK_INTERRUPT();
		cell *save_c = c;
		pl_ctx save_c_ctx = c_ctx;

		if (q->max_depth && (print_list >= q->max_depth)) {
			SB_ungetchar(q->sb);
			SB_sprintf(q->sb, "%s", "|...]");
			q->last_thing = WAS_OTHER;
			//q->cycle_error = true;
			break;
		}

		if (!cons) {
			SB_sprintf(q->sb, "%s", "[");
			q->last_thing = WAS_OTHER;
		}

		cell *head = c + 1;
		pl_ctx head_ctx = c_ctx;
		cell *save_head = head;
		if (running) head  = deref(q, head, head_ctx);
		if (running) head_ctx = q->latest_ctx;
		int parens = 0;

		if (q->do_dump_vars && is_var(save_head) && 0 && is_cyclic_term(q, head, c_ctx)) {
			print_variable(q, save_head, c_ctx, 0);
			q->last_thing = WAS_OTHER;
		} else if (has_visited(visited, head, head_ctx)) {
			if ((q->portray_vars || q->do_dump_vars) && ((unsigned)q->dump_var_num != (unsigned)-1)) {
				SB_sprintf(q->sb, "%s", GET_POOL(q, q->top->vartab.off[q->dump_var_num]));
			} else {
				SB_sprintf(q->sb, "%s", "...");
			}
		} else {
			bool special_op = false;

			if (is_interned(head)) {
				unsigned specifier = 0;
				unsigned priority = match_op(q->st.m, C_STR(q, head), &specifier, head->arity);
				special_op = (priority >= 1000);
			}

			visit me = {.next = visited, .c = head, .c_ctx = head_ctx};
			parens = is_compound(head) && special_op;
			if (parens) {  SB_sprintf(q->sb, "%s", "("); q->last_thing = WAS_OTHER; }
			q->parens = parens;
			print_term_to_buf_(q, head, head_ctx, running, -1, 0, depth+1, &me);
			q->parens = false;
		}

		q->cycle_error = false;
		if (parens) { SB_sprintf(q->sb, "%s", ")"); }
		bool possible_chars = false;

		if (is_interned(head) && (C_STRLEN_UTF8(head) == 1) && q->double_quotes)
			possible_chars = true;

		cell *tail = c + 1; tail += tail->num_cells;
		pl_ctx tail_ctx = c_ctx;
		cell *save_tail = tail;
		pl_ctx save_tail_ctx = tail_ctx;
		if (running) tail = deref(q, tail, tail_ctx);
		if (running) tail_ctx = q->latest_ctx;

		if (q->do_dump_vars && is_var(save_tail) && 0 && is_cyclic_term(q, tail, c_ctx)) {
			SB_sprintf(q->sb, "%s", "|");
			print_variable(q, save_tail, c_ctx, 0);
			SB_sprintf(q->sb, "%s", "]");
			q->last_thing = WAS_OTHER;
			break;
		} else if (has_visited(visited, tail, tail_ctx)
			|| ((tail == save_c) && (tail_ctx == save_c_ctx))
			//|| (q->max_depth && (print_depth >= q->max_depth))
			) {
			SB_sprintf(q->sb, "%s", "|");

#if 0
			if (is_var(c+1)) printf("*** c+1 = %u/%u\n", (c+1)->var_num, c_ctx);
			if (is_var(tail)) printf("*** tail = %u/%u\n", tail->var_num, tail_ctx);
			if (is_var(save_tail)) printf("*** save_tail = %u/%u\n", save_tail->var_num, save_tail_ctx);
			if (is_var(save_c)) printf("*** save_c = %u/%u\n", save_c->var_num, save_c_ctx);
			if (is_var(orig_c)) printf("*** orig_c = %u/%u\n", orig_c->var_num, orig_c_ctx);
#endif

			cell v = *(c+1);
			pl_ctx v_ctx = c_ctx;

			if ((q->portray_vars || q->do_dump_vars) && (orig_c_ctx == 0) && q->is_dump_vars) {
				if (q->do_dump_vars) {
					if (!dump_variable(q, save_tail, tail_ctx, running))
						print_variable(q, save_tail, save_tail_ctx, running);
				} else
					SB_sprintf(q->sb, "%s", GET_POOL(q, q->top->vartab.off[v.var_num]));
			} else {
				SB_sprintf(q->sb, "%s", "...");
			}

			SB_sprintf(q->sb, "%s", "]");
			q->last_thing = WAS_OTHER;
			q->cycle_error = true;
			break;
		}

		bool has_vars = false, is_partial = false;

		if (is_interned(tail) && !is_compound(tail)) {
			const char *src = C_STR(q, tail);

			if (strcmp(src, "[]")) {
				SB_sprintf(q->sb, "%s", "|");
				print_term_to_buf_(q, tail, tail_ctx, running, true, depth+1, depth+1, visited);
			}
		} else if (q->st.m->flags.double_quote_chars && running
			&& !q->ignore_ops && possible_chars
			&& (scan_is_chars_list2(q, tail, tail_ctx, false, &has_vars, &is_partial, NULL) > 0)
			&& !is_partial)
			{
			char *tmp_src = chars_list_to_string(q, tail, tail_ctx);

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
				//q->cycle_error = true;

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
				visit *me = malloc(sizeof(visit));
				me->next = visited;
				me->c = tail;
				me->c_ctx = tail_ctx;
				visited = me;
				unsigned specifier = 0;
				unsigned priority = match_op(q->st.m, C_STR(q, tail), &specifier, tail->arity);
				bool parens = (is_infix(tail) || is_prefix(tail)) && (priority >= 1000);
				if (parens) { SB_sprintf(q->sb, "%s", "("); q->last_thing = WAS_OTHER; }
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

static bool print_interned(query *q, cell *c, pl_ctx c_ctx, bool running, unsigned depth, visit *visited)
{
	// ATOM / COMPOUND

	const char *src = !is_ref(c) ? C_STR(q, c) : "_";
	size_t src_len = !is_ref(c) ? C_STRLEN(q, c) : 1;
	unsigned my_specifier = 0;
	unsigned my_priority = match_op(q->st.m, src, &my_specifier, c->arity);

	if (!my_priority
		|| ((IS_PREFIX(my_specifier) || IS_POSTFIX(my_specifier)) && (c->arity != 1))
		|| (IS_INFIX(my_specifier) && (c->arity != 2))
		) {
		my_priority = 0;
	}

	bool is_op = my_priority;
	unsigned pri = 0, spec = 0;

	if (!is_op && !is_var(c) && (c->arity == 1)
		&& (pri = match_op(q->st.m, src, &spec, c->arity))) {
		if (IS_PREFIX(spec)) {
			is_op = true;
			my_specifier = spec;
			my_priority = pri;
		}
	}

	// CANONICAL

	if (q->ignore_ops || !is_op || !c->arity) {
		bool is_needs_quoting = needs_quoting(q->st.m, src, src_len);
		int quote = ((running <= 0) || q->quoted) && !is_var(c) && is_needs_quoting;
		int dq = 0, braces = 0;
		if (is_string(c) && q->double_quotes) dq = quote = 1;
		if (q->quoted < 0) quote = 0;
		if ((c->arity == 1) && is_interned(c) && !strcmp(src, "{}")) braces = 1;
		cell *c1 = c->arity && running ? deref(q, FIRST_ARG(c), c_ctx) : NULL;

		if (running && is_interned(c) && c->arity
			&& q->numbervars && (c->val_off == g_sys_var_s) && c1
			&& is_integer(c1) && (get_smallint(c1) >= 0)) {
			SB_sprintf(q->sb, "%s", varformat2(q->tmpbuf, sizeof(q->tmpbuf), c1, 0));
			q->last_thing = WAS_OTHER;
			return true;
		}

		if (is_var(c) && q->variable_names) {
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

			if ((q->last_thing == WAS_SYMBOL) && is_symbol && !q->parens && !quote
				&& (c->arity == 1) // Only if prefix
				) {
				SB_sprintf(q->sb, "%s", " ");
				q->last_thing = WAS_SPACE;
			}

			SB_strcatn(q->sb, src, len_str);
			q->last_thing = is_symbol ? WAS_SYMBOL : WAS_OTHER;
		}

		SB_sprintf(q->sb, "%s", !braces&&quote?dq?"\"":"'":"");
		q->did_quote = !braces&&quote;

		if (is_compound(c) && !is_string(c)) {
			pl_idx arity = c->arity;
			SB_sprintf(q->sb, "%s", braces&&!q->ignore_ops?"{":"(");
			q->last_thing = WAS_OTHER;
			q->parens = true;

			for (c++; arity--; c += c->num_cells) {
				cell *tmp = c;
				pl_ctx tmp_ctx = c_ctx;
				if (running) tmp = deref(q, tmp, tmp_ctx);
				if (running) tmp_ctx = q->latest_ctx;

				if (q->do_dump_vars && is_var(c) && 0 && is_cyclic_term(q, tmp, c_ctx)) {
					print_variable(q, c, c_ctx, 0);
					if (arity) {SB_sprintf(q->sb, "%s", ","); }
					q->last_thing = WAS_OTHER;
					continue;
				} else if (q->is_dump_vars && has_visited(visited, tmp, tmp_ctx)) {
					tmp = c;
					tmp_ctx = c_ctx;
					if (c_ctx == 0) { SB_sprintf(q->sb, "%s", GET_POOL(q, q->top->vartab.off[c->var_num])); }
					else { SB_sprintf(q->sb, "%s", !is_ref(tmp) ? "..." : "_"); }
					if (arity) {SB_sprintf(q->sb, "%s", ","); }
					q->last_thing = WAS_OTHER;
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
					unsigned tmp_priority = match_op(q->st.m, C_STR(q, tmp), NULL, tmp->arity);

					if ((tmp_priority >= 1000) && tmp->arity)
						q->parens = parens = true;
				}

				if (parens) { SB_sprintf(q->sb, "%s", "("); q->last_thing = WAS_OTHER; }

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

	// OP

	bool is_op_infix = is_op && IS_INFIX(my_specifier);
	bool is_op_prefix = is_op && IS_PREFIX(my_specifier);
	bool is_op_postfix = is_op && IS_POSTFIX(my_specifier);
	bool is_op_yfx = is_op_infix && (my_specifier == OP_YFX);
	bool is_op_xfy = is_op_infix && (my_specifier == OP_XFY);
	size_t srclen = src_len;

	if (is_op_postfix) {
		cell *lhs = c + 1;
		cell *save_lhs = lhs;
		pl_ctx lhs_ctx = c_ctx;
		if (running) lhs = deref(q, lhs, lhs_ctx);
		if (running) lhs_ctx = q->latest_ctx;
		unsigned lhs_specifier = false;
		unsigned lhs_pri = is_interned(lhs) ? match_op(q->st.m, C_STR(q, lhs), &lhs_specifier, lhs->arity) : 0;
		bool is_lhs_postfix = IS_POSTFIX(lhs_specifier);
		bool is_lhs_xf = IS_XF(lhs_specifier);
		bool is_lhs_yf = IS_YF(lhs_specifier);
		bool is_op_lhs = lhs_pri;
		bool parens = is_lhs_xf;
		bool space = (c->val_off == g_minus_s) && (is_number(lhs) || is_op_lhs);
		if ((c->val_off == g_plus_s) && is_op_lhs) space = true;
		int ch = peek_char_utf8(src);
		if (iswalpha(ch)) space = true;
		if (lhs_pri > my_priority) { parens = true; space = false; }

		if (q->do_dump_vars && is_var(save_lhs) && 0 && is_cyclic_term(q, lhs, c_ctx)) {
			print_variable(q, save_lhs, lhs_ctx, 0);
			q->last_thing = WAS_OTHER;
			return true;
		} else if (!is_var(lhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
			if (q->last_thing != WAS_SPACE) SB_sprintf(q->sb, "%s", " ");
			SB_sprintf(q->sb, "%s", "...");
			q->last_thing = WAS_SYMBOL;
			return true;
		} else {
			visit me;
			me.next = visited;
			me.c = lhs;
			me.c_ctx = lhs_ctx;
			pl_ctx lhs_ctx = running ? q->latest_ctx : 0;

			if (parens) { SB_sprintf(q->sb, "%s", "("); q->last_thing = WAS_OTHER; }
			print_term_to_buf_(q, lhs, lhs_ctx, running, 0, 0, depth+1, &me);
			if (parens) { SB_sprintf(q->sb, "%s", ")"); q->last_thing = WAS_OTHER; }
			q->last_thing = WAS_OTHER;
		}

		if (q->is_dump_vars && has_visited(visited, lhs, lhs_ctx)) {
			if (q->is_dump_vars) {
				SB_sprintf(q->sb, "%s", !is_ref(save_lhs) ? C_STR(q, save_lhs) : "_");
			} else
				print_variable(q, save_lhs, lhs_ctx, 1);

			q->last_thing = WAS_OTHER;
			return true;
		}

		if ((q->last_thing != WAS_SPACE) && (space || is_lhs_yf)) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		int quote = q->quoted && needs_quoting(q->st.m, src, src_len);
		if (quote) {
			if (!parens) SB_sprintf(q->sb, "%s", " ");
			SB_sprintf(q->sb, "%s", quote?"'":"");
		}

		SB_strcatn(q->sb, src, srclen);
		if (quote) { SB_sprintf(q->sb, "%s", quote?"'":""); }
		//if (q->last_thing != WAS_SPACE) { SB_sprintf(q->sb, "%s", " "); q->last_thing = WAS_SPACE; }
		else q->last_thing = WAS_OTHER;
		return true;
	}

	if (is_op_prefix) {
		if (q->last_thing == WAS_SYMBOL) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		cell *rhs = c + 1;
		cell *save_rhs = rhs;
		pl_ctx rhs_ctx = c_ctx;
		const char *rhs_src = C_STR(q, rhs);
		if (running) rhs = deref(q, rhs, rhs_ctx);
		if (running) rhs_ctx = q->latest_ctx;
		unsigned rhs_pri = is_interned(rhs) ? match_op(q->st.m, C_STR(q, rhs), NULL, rhs->arity) : 0;
		bool is_op_rhs = rhs_pri;

		if ((q->last_thing == WAS_SYMBOL) && !strcmp(src, "\\+")) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		bool space = (c->val_off == g_minus_s) && (is_number(rhs) || is_op_rhs);
		if ((c->val_off == g_plus_s) && is_op_rhs) space = true;
		int ch = peek_char_utf8(src);
		if (iswalpha(ch)) space = true;
		if (/*is_op_rhs ||*/ is_negative(rhs) || is_float(rhs)) space = true;
		if (is_interned(rhs) && !iswalpha(peek_char_utf8(rhs_src)) && !is_op(rhs)) space = true;

		bool parens = false;
		if ((!strcmp(src, ":-") || !strcmp(src, "?-")) && (rhs_pri >= my_priority)) parens = true;
		if (!strcmp(src, "+") && (is_infix(rhs) || is_postfix(rhs))) parens = true;
		if (rhs_pri > my_priority) parens = true;
		if ((rhs_pri > 0) && !rhs->arity) parens = true;
		//if (my_priority && (rhs_pri == my_priority) && strcmp(src, "-") && strcmp(src, "+")) parens = true;
		if (!strcmp(src, "-") && (rhs_pri == my_priority) && (rhs->arity > 1)) parens = true;
		if ((c->val_off == g_minus_s) && is_number(rhs) && !is_negative(rhs)) parens = true;
		if ((c->val_off == g_minus_s) && search_op(q->st.m, C_STR(q, rhs), NULL, true) && !rhs->arity) parens = true;
		if ((c->val_off == g_plus_s) && search_op(q->st.m, C_STR(q, rhs), NULL, true) && !rhs->arity) parens = true;

		if (!strcmp(src, "?-") || !strcmp(src, ":-")) space = 1;

		bool quote = q->quoted && needs_quoting(q->st.m, src, src_len);

		if (is_interned(rhs) && !rhs->arity && !parens) {
			if (!iswalnum(peek_char_utf8(rhs_src)) && strcmp(rhs_src, "[]") && strcmp(rhs_src, "{}"))
				space = 1;
		}

		if (quote) { SB_sprintf(q->sb, "%s", quote?"'":""); }
		SB_strcatn(q->sb, src, srclen);
		if (quote)
			{ SB_sprintf(q->sb, "%s", quote?"' ":""); q->last_thing = WAS_SPACE; }
		else if (!iswalpha(peek_char_utf8(src)))
			q->last_thing = WAS_SYMBOL;
		else
			q->last_thing = WAS_OTHER;

		if (q->do_dump_vars && is_var(save_rhs) && 0 && is_cyclic_term(q, rhs, c_ctx)) {
			print_variable(q, save_rhs, rhs_ctx, 0);
			q->last_thing = WAS_OTHER;
			return true;
		} else if (q->is_dump_vars && has_visited(visited, rhs, rhs_ctx)) {
			if (q->is_dump_vars) {
				if (!dump_variable(q, save_rhs, rhs_ctx, 1))
					print_variable(q, save_rhs, rhs_ctx, 1);
			} else
				print_variable(q, save_rhs, rhs_ctx, 1);

			q->last_thing = WAS_OTHER;
			return true;
		}

		if ((q->last_thing != WAS_SPACE) && (space || parens)) {
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

		if (parens) { SB_sprintf(q->sb, "%s", "("); q->last_thing = WAS_OTHER; }
		q->parens = parens;
		print_term_to_buf_(q, rhs, rhs_ctx, running, 0, 0, depth+1, &me);
		q->parens = false;
		if (parens) { SB_sprintf(q->sb, "%s", ")"); q->last_thing = WAS_OTHER; }
		return true;
	}

	// Is infix

	cell *lhs = c + 1;
	cell *save_lhs = lhs;
	pl_ctx lhs_ctx = c_ctx;
	cell *rhs = lhs + lhs->num_cells;
	cell *save_rhs = rhs;
	pl_ctx rhs_ctx = c_ctx;
	const char *lhs_src = C_STR(q, lhs);
	const char *rhs_src = C_STR(q, rhs);
	if (running) lhs = deref(q, lhs, lhs_ctx);
	if (running) lhs_ctx = q->latest_ctx;
	if (running) rhs = deref(q, rhs, rhs_ctx);
	if (running) rhs_ctx = q->latest_ctx;

	int quote = q->quoted && has_spaces(src, src_len);
	if (op_needs_quoting(q->st.m, src, src_len)) quote = 1;

	// Print LHS..


	unsigned lhs_specifier = 0;
	unsigned lhs_pri_1 = is_interned(lhs) ? match_op(q->st.m, C_STR(q, lhs), &lhs_specifier, lhs->arity) : 0;
	unsigned lhs_pri_2 = is_interned(lhs) && !lhs->arity ? search_op(q->st.m, C_STR(q, lhs), &lhs_specifier, true) : 0;
	bool lhs_postfix = (lhs->arity == 1) && IS_POSTFIX(lhs_specifier);

	bool lhs_parens = lhs_pri_1 >= my_priority;
	//if (lhs_postfix) lhs_parens = true;
	if ((lhs_pri_1 == my_priority) && is_op_yfx) lhs_parens = false;
	if (lhs_pri_2 > 0) lhs_parens = true;
	if (is_compound(lhs) && (lhs_pri_1 <= my_priority) && (lhs->val_off == g_plus_s)) { lhs_parens = false; }
	bool lhs_space = lhs_postfix;

	if ((q->last_thing != WAS_SPACE) && lhs_space) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	if (q->do_dump_vars && is_var(save_lhs) && 0 && is_cyclic_term(q, lhs, c_ctx)) {
		dump_variable(q, save_lhs, c_ctx, 0);
		q->last_thing = WAS_OTHER;
	} else if (!is_var(lhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
		if (q->last_thing != WAS_SPACE) SB_sprintf(q->sb, "%s", " ");
		SB_sprintf(q->sb, "%s", "...");
		q->last_thing = WAS_SYMBOL;
	} else if (q->is_dump_vars && has_visited(visited, lhs, lhs_ctx)) {
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
		if (lhs_parens) { SB_sprintf(q->sb, "%s", "("); q->last_thing = WAS_OTHER; }
		q->parens = lhs_parens;
		print_term_to_buf_(q, lhs, lhs_ctx, running, 0, 0, depth+1, &me);
		q->parens = false;
		if (lhs_parens) { SB_sprintf(q->sb, "%s", ")"); q->last_thing = WAS_OTHER; }
	}

	bool space = false;

	if (is_interned(lhs) && !lhs->arity && !lhs_parens) {
		const char *lhs_src = C_STR(q, lhs);
		if (!iswalpha(peek_char_utf8(lhs_src)) && !iswdigit(peek_char_utf8(lhs_src)) && (peek_char_utf8(lhs_src) != '$')
			&& strcmp(src, ",") && strcmp(src, ";")
			&& strcmp(lhs_src, "[]") && strcmp(lhs_src, "{}")
			)
			space = true;
	}

	bool extra_space = false;

	if ((q->last_thing != WAS_SPACE) && (space || lhs_space) && !quote) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
		if (!lhs_space) extra_space = true;
	}

	int ch = peek_char_utf8(src);
	bool is_symbol = !needs_quoting(q->st.m, src, src_len)
		&& !iswalpha(ch) && strcmp(src, ",") && strcmp(src, ";")
		&& strcmp(src, "[]") && strcmp(src, "{}") && !q->parens;

	if (!*src || ((q->last_thing == WAS_SYMBOL) && is_symbol && !lhs_parens && !q->parens))
		space = true;

	if ((q->last_thing != WAS_SPACE) && !is_symbol && space && !quote) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	// Print OP..

	//q->last_thing_was_symbol += is_symbol;
	space = iswalpha(peek_char_utf8(src)) || (q->last_thing == WAS_SYMBOL);

	if (!strcmp(lhs_src, "!"))
		space = false;

	if (q->listing && !depth && !strcmp(src, ":-"))
		space = true;

	if ((q->last_thing != WAS_SPACE) && space && !quote) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	if (quote) { SB_sprintf(q->sb, "%s", quote?"'":""); }
	SB_strcatn(q->sb, src, srclen);
	if (quote) { SB_sprintf(q->sb, "%s", quote?"'":""); }
	q->last_thing = strcmp(src, "|") ? WAS_SYMBOL : WAS_OTHER;

	if (q->listing && !depth && !strcmp(src, ":-")) {
		SB_sprintf(q->sb, "%s", "\n  ");
	}

	if (extra_space)
		space = true;

	if ((q->last_thing != WAS_SPACE) && space && !quote) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	// Print RHS..

	unsigned rhs_pri_1 = is_interned(rhs) ? match_op(q->st.m, C_STR(q, rhs), NULL, rhs->arity) : 0;
	unsigned rhs_pri_2 = is_interned(rhs) && !rhs->arity ? search_op(q->st.m, C_STR(q, rhs), NULL, true) : 0;
	bool rhs_parens = rhs_pri_1 >= my_priority;
	space = is_number(rhs) && is_negative(rhs);

	if (!rhs_parens && is_prefix(rhs) && strcmp(src, "|"))
		space = true;

	bool rhs_is_symbol = is_interned(rhs) && !rhs->arity
		&& !iswalpha(peek_char_utf8(rhs_src))
		&& !needs_quoting(q->st.m, C_STR(q, rhs), C_STRLEN(q, rhs))
		&& strcmp(C_STR(q, rhs), "[]") && strcmp(C_STR(q, rhs), "{}")
		&& !rhs_parens;

	if (rhs_is_symbol && strcmp(C_STR(q, rhs), "[]") && strcmp(C_STR(q, rhs), "{}") && strcmp(C_STR(q, rhs), "!"))
		space = true;

	if ((rhs_pri_1 == my_priority) && is_op_xfy)
		rhs_parens = false;

	if (rhs_pri_2 > 0)
		rhs_parens = true;

	if ((q->last_thing != WAS_SPACE) && space && !rhs_parens && !quote) {
		SB_sprintf(q->sb, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	if (q->do_dump_vars && is_var(save_rhs) && 0 && is_cyclic_term(q, rhs, c_ctx)) {
		print_variable(q, save_rhs, rhs_ctx, 0);
		q->last_thing = WAS_OTHER;
	} else if (!is_var(rhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
		if (q->last_thing != WAS_SPACE) SB_sprintf(q->sb, "%s", " ");
		SB_sprintf(q->sb, "%s", "...");
		q->last_thing = WAS_SYMBOL;
	} else if (q->is_dump_vars && has_visited(visited, rhs, rhs_ctx)) {
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

static bool print_term_to_buf_(query *q, cell *c, pl_ctx c_ctx, int running, int cons, unsigned print_depth, unsigned depth, visit *visited)
{
	if (depth > g_max_depth) {
		//printf("*** OOPS %u, %s %d\n", depth, __FILE__, __LINE__);
		SB_sprintf(q->sb, "%s", "...");
		q->cycle_error = true;
		q->last_thing = WAS_OTHER;
		return false;
	}

	// THREAD OBJECTS

	if ((c->tag == TAG_INT) && (c->flags & FLAG_INT_THREAD)) {
		int n = get_smallint(c);
		thread *t = &q->pl->threads[n];

		if (t->is_queue_only) {
			SB_sprintf(q->sb, "'$queue'(%d)", (int)get_smallint(c));
		} else if (t->is_mutex_only) {
			SB_sprintf(q->sb, "'$mutex'(%d)", (int)get_smallint(c));
		} else {
			SB_sprintf(q->sb, "'$thread'(%d)", (int)get_smallint(c));
		}

		q->last_thing = WAS_OTHER;
		return true;
	}

	// ALIAS

	if ((c->tag == TAG_INT) && (c->flags & FLAG_INT_ALIAS)) {
		SB_sprintf(q->sb, "'$alias'(%d)", (int)get_smallint(c));
		q->last_thing = WAS_OTHER;
		return true;
	}

	// MAP

	if ((c->tag == TAG_INT) && (c->flags & FLAG_INT_MAP)) {
		SB_sprintf(q->sb, "'$map'(%d)", (int)get_smallint(c));
		q->last_thing = WAS_OTHER;
		return true;
	}

	// STREAM

	if ((c->tag == TAG_INT) && (c->flags & FLAG_INT_STREAM)) {
		SB_sprintf(q->sb, "'$stream'(%d)", (int)get_smallint(c));
		q->last_thing = WAS_OTHER;
		return true;
	}

	// BLOB

	if (is_blob(c)) {
		SB_sprintf(q->sb, "'$blob'(%p)", c->val_ptr);
		q->last_thing = WAS_OTHER;
		return true;
	}

	// NEGATIVE

	if (is_number(c) && is_negative(c)) {
		if (is_negative(c) && (q->last_thing == WAS_SYMBOL)) {
			SB_sprintf(q->sb, "%s", " ");
			q->last_thing = WAS_SPACE;
		}
	}

	// RATIONAL

	if (is_rational(c)) {
		int radix = 10;
		size_t len = mp_int_string_len(&c->val_bigint->irat.num, radix) - 1;
		char *dst2 = malloc(len+1);
		checked(dst2);
		mp_int_to_string(&c->val_bigint->irat.num, radix, dst2, len+1);
		SB_sprintf(q->sb, "%s", dst2);
		free(dst2);
		SB_sprintf(q->sb, "%s", " rdiv ");
		len = mp_int_string_len(&c->val_bigint->irat.den, radix) - 1;
		dst2 = malloc(len+1);
		mp_int_to_string(&c->val_bigint->irat.den, radix, dst2, len+1);
		SB_sprintf(q->sb, "%s", dst2);
		free(dst2);
		q->last_thing = WAS_OTHER;
		return true;
	}

	// BIG INTEGER

	if (is_bigint(c)) {
		int radix = 10;
		size_t len = mp_int_string_len(&c->val_bigint->ival, radix) - 1;
		char *dst2 = malloc(len+1);
		checked(dst2);
		mp_int_to_string(&c->val_bigint->ival, radix, dst2, len+1);
		SB_sprintf(q->sb, "%s", dst2);
		free(dst2);
		q->last_thing = WAS_OTHER;
		return true;
	}

	// SMALL INTEGER

	if (is_smallint(c)) {
		char tmpbuf[256];
		sprint_int(tmpbuf, sizeof(tmpbuf), get_smallint(c), 10);
		SB_sprintf(q->sb, "%s", tmpbuf);
		q->last_thing = WAS_OTHER;
		return true;
	}

	// FLOAT

	if (is_float(c)) {
		if (c->val_float == 0.0)
			c->val_float = fabs(c->val_float);

		char tmpbuf[256];

		if (!q->json && !isnan(c->val_float) && !isinf(c->val_float))
			reformat_float(q, tmpbuf, c->val_float);
		else
			sprintf(tmpbuf, "%.*g", 17, get_float(c));

		SB_sprintf(q->sb, "%s", tmpbuf);
		q->last_thing = WAS_OTHER;
		return true;
	}

	// STRING

	if (is_string(c) && q->ignore_ops) {
		print_string_canonical(q, c, c_ctx, running, cons > 0, depth+1);
		q->last_thing = WAS_OTHER;
		return true;
	}

	// STRING

	if (is_string(c) && !q->double_quotes) {
		print_string_list(q, c, c_ctx, running, cons > 0, depth+1);
		q->last_thing = WAS_OTHER;
		return true;
	}

	// STRING / CHARS

	int is_chars_list = is_string(c) && q->double_quotes;
	bool possible_chars = false, has_var = false, is_partial = false;
	cell *v = NULL;

	if (is_interned(c) && (C_STRLEN_UTF8(c) == 1) && !q->ignore_ops && q->double_quotes)
		possible_chars = true;

	if (!is_chars_list && running && possible_chars
		&& (scan_is_chars_list2(q, c, c_ctx, false, &has_var, &is_partial, &v) > 0))
		is_chars_list += q->st.m->flags.double_quote_chars && scan_is_chars_list2(q, c, c_ctx, false, &has_var, &is_partial, &v);

	if (is_chars_list) {
		cell *l = c;
		pl_ctx l_ctx = c_ctx;
		SB_sprintf(q->sb, "%s", "\"");
		unsigned cnt = 0;
		LIST_HANDLER(l);
		bool closing_quote = true;
		bool any = false;

		while (is_list(l)) {
			if (q->max_depth && (cnt++ >= q->max_depth)) {
				SB_sprintf(q->sb, "%s", "\"||... ");
				closing_quote = false;
				break;
			}

			cell *h = LIST_HEAD(l);
			pl_ctx h_ctx = l_ctx;
			slot *e = NULL;
			uint32_t save_vgen = 0;
			int both = 0;

			if (running) {
				DEREF_VAR(any, both, save_vgen, e, e->vgen, h, h_ctx, q->vgen);
				if (e) e->vgen = save_vgen;
			}

			if (is_smallint(h) && !both) {
				SB_putchar(q->sb, h->val_uint);
			} else {
				SB_strcat_and_free(q->sb, formatted(C_STR(q, h), C_STRLEN(q, h), true, q->json));
			}

			l = LIST_TAIL(l);
			e = NULL;
			both = 0;
			any = false;

			if (running) DEREF_VAR(any, both, save_vgen, e, e->vgen, l, l_ctx, q->vgen);

			if (both) {
				q->cycle_error = true;
				break;
			}
		}

		if (closing_quote) SB_sprintf(q->sb, "%s", "\"");

		if (is_partial) {
			SB_strcat(q->sb, "||");
			if (is_op(l)) SB_putchar(q->sb, '(');
			if (q->cycle_error) {
				if (!dump_variable(q, v?v:c, c_ctx, !v))
					print_variable(q, v?v:c, c_ctx, !v);
			} else
				print_term_to_buf_(q, l, 0, running, 0, depth+1, depth+1, NULL);
			if (is_op(l)) SB_putchar(q->sb, ')');
		}

		q->last_thing = WAS_OTHER;
		return true;
	}

	// LIST

	if (is_iso_list(c) && !q->ignore_ops) {
		print_iso_list(q, c, c_ctx, running, cons > 0, print_depth+1, depth+1, visited);
		q->last_thing = WAS_OTHER;
		return true;
	}

	// VAR

	if (is_var(c) && q->is_dump_vars) {
		if (!dump_variable(q, c, c_ctx, running))
			print_variable(q, c, c_ctx, running);

		return true;
	}

	return print_interned(q, c, c_ctx, running, depth, visited);
}

static bool print_term_to_buf(query *q, cell *c, pl_ctx c_ctx, int running, int cons)
{
	visit me;
	me.next = NULL;
	me.c = c;
	me.c_ctx = c_ctx;
	return print_term_to_buf_(q, c, c_ctx, running, cons, 0, 0, &me);
}

char *print_canonical_to_strbuf(query *q, cell *c, pl_ctx c_ctx, int running)
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

bool print_canonical_to_stream(query *q, stream *str, cell *c, pl_ctx c_ctx, int running)
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

bool print_canonical(query *q, FILE *fp, cell *c, pl_ctx c_ctx, int running)
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

char *print_term_to_strbuf(query *q, cell *c, pl_ctx c_ctx, int running)
{
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	//q->last_thing_was_space = true;
	SB_init(q->sb);
	print_term_to_buf(q, c, c_ctx, running, false);
	char *buf = malloc(SB_strlen(q->sb)+1+1); // dcg_expansion needs this extra char space
	if (!buf) return NULL;
	strcpy(buf, SB_cstr(q->sb));
	SB_free(q->sb);
	return buf;
}

bool print_term_to_stream(query *q, stream *str, cell *c, pl_ctx c_ctx, int running)
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

bool print_term(query *q, FILE *fp, cell *c, pl_ctx c_ctx, int running)
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
	q->max_depth = q->pl->def_max_depth;
	q->quoted = 0;
	q->nl = q->fullstop = q->varnames = q->ignore_ops = false;
	q->parens = q->numbervars = q->json = q->double_quotes = false;
	q->portrayed = false;
	q->last_thing = WAS_OTHER;
	q->variable_names = NULL;
	q->cycle_error = false;
	memset(q->ignores, 0, sizeof(q->ignores));
}
