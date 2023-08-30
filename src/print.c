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

cell *string_to_chars_list(query *q, cell *p, pl_idx p_ctx)
{
	LIST_HANDLER(p);
	int i = 0;

	while (is_list(p)) {
		cell *h = LIST_HEAD(p);

		if (i == 0)
			allocate_list(q, h);
		else
			append_list(q, h);

		p = LIST_TAIL(p);
		i++;
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

size_t formatted(char *dst, size_t dstlen, const char *src, int srclen, bool dq, bool json)
{
	extern const char *g_escapes;
	extern const char *g_anti_escapes;
	size_t len = 0;

	while (srclen > 0) {
		int lench = len_char_utf8(src);
		int ch = get_char_utf8(&src);
		srclen -= lench;
		const char *ptr = (lench == 1) && (ch != ' ') && (ch != '\e') ? strchr(g_escapes, ch) : NULL;

		if ((ch == '\'') && dq)
			ptr = 0;

		if (ch && ptr) {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = g_anti_escapes[ptr-g_escapes];
			}

			len += 2;
		} else if (!json && !dq && (ch == '\'')) {
			if (dstlen) {
				*dst++ = '\'';
				*dst++ = ch;
			}

			len += 2;
		} else if (ch == (dq?'"':'\'')) {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = ch;
			}

			len += 2;
		} else if (!json && (ch < ' ')) {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = 'x';
			}

			size_t n = snprintf(dst, dstlen, "%x", ch);
			len += n;
			if (dstlen) dst += n;

			if (dstlen)
				*dst++ = '\\';

			len += 3;
		} else if (json && (ch < ' ')) {
			if (dstlen) {
				*dst++ = '\\';
			}

			switch (ch) {
			case '\b': if (dstlen) *dst++ = 'b'; break;
			case '\n': if (dstlen) *dst++ = 'n'; break;
			case '\f': if (dstlen) *dst++ = 'f'; break;
			case '\r': if (dstlen) *dst++ = 'r'; break;
			case '\t': if (dstlen) *dst++ = 't'; break;
			default: {
				size_t n = snprintf(dst, dstlen, "u%04x", ch);
				len += n;
				if (dstlen) dst += n;

				len++;
			}
			}
		} else if (ch == '\\') {
			if (dstlen) {
				*dst++ = '\\';
				*dst++ = ch;
			}

			len += 2;
		} else {
			if (dstlen)
				dst += put_char_utf8(dst, ch);

			len += lench;
		}
	}

	if (dstlen)
		*dst = '\0';

	return len;
}

static size_t plain(char *dst, size_t dstlen, const char *src, int srclen)
{
	if (dstlen) {
		memcpy(dst, src, srclen);
		dst[srclen] = '\0';
	}

	return srclen;
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

		// NOTE: according to the man pages:
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

static unsigned count_non_anons(const bool *mask, unsigned bit)
{
	unsigned bits = 0;

	for (unsigned i = 0; i < bit; i++) {
		if (mask[i])
			bits++;
	}

	return bits;
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

ssize_t print_variable(query *q, char *dst, size_t dstlen, const cell *c, pl_idx c_ctx, bool running)
{
	char *save_dst = dst;
	const frame *f = GET_FRAME(running ? c_ctx : 0);
	const slot *e = GET_SLOT(f, c->var_nbr);
	pl_idx slot_nbr = running ? (unsigned)(f->base + c->var_nbr) : (unsigned)c->var_nbr;

	if (q->varnames && !is_anon(c) && running) {
		if (q->p->vartab.var_name[c->var_nbr])
			dst += snprintf(dst, dstlen, "%s", q->p->vartab.var_name[c->var_nbr]);
		else
			dst += snprintf(dst, dstlen, "%s", get_slot_name(q, slot_nbr));
	} else if (q->portray_vars) {
		dst += snprintf(dst, dstlen, "%s", get_slot_name(q, slot_nbr));
	} else if (q->is_dump_vars) {
		dst += snprintf(dst, dstlen, "_%s", get_slot_name(q, slot_nbr));
	} else if (q->listing) {
		dst += snprintf(dst, dstlen, "%s", get_slot_name(q, slot_nbr));
	} else if (!running && !is_ref(c)) {
		dst += snprintf(dst, dstlen, "%s", C_STR(q, c));
	} else
		dst += snprintf(dst, dstlen, "_%u", (unsigned)slot_nbr);

	return dst - save_dst;
}

static ssize_t print_string_canonical(query *q, char *save_dst, char *dst, size_t dstlen, cell *c, pl_idx c_ctx, int running, bool cons, unsigned depth)
{
	unsigned print_list = 0, cnt = 1;
	LIST_HANDLER(c);

	dst += snprintf(dst, dstlen, "%s", "'.'(");

	while (is_list(c)) {
		cell *h = LIST_HEAD(c);

		const char *src = C_STR(q, h);
		int ch = peek_char_utf8(src);

		if (needs_quoting(q->st.m, src, strlen(src))) {
			dst += snprintf(dst, dstlen, "%s", "'");
			dst += formatted(dst, dstlen, C_STR(q, h), C_STRLEN(q, h), false, false);
			dst += snprintf(dst, dstlen, "%s", "'");
		} else
			dst += snprintf(dst, dstlen, "%s", C_STR(q, h));

		c = LIST_TAIL(c);

		if (!is_list(c)) {
			dst += snprintf(dst, dstlen, "%s", ",[]");
			break;
		}

		dst += snprintf(dst, dstlen, "%s", ",'.'(");
		cnt++;
	}

	while (cnt--)
		dst += snprintf(dst, dstlen, "%s", ")");

	return dst - save_dst;
}

static ssize_t print_string_list(query *q, char *save_dst, char *dst, size_t dstlen, cell *c, pl_idx c_ctx, int running, bool cons, unsigned depth)
{
	LIST_HANDLER(c);
	if (!cons) dst += snprintf(dst, dstlen, "%s", "[");

	while (is_list(c)) {
		cell *h = LIST_HEAD(c);

		const char *src = C_STR(q, h);
		int ch = peek_char_utf8(src);

		if (needs_quoting(q->st.m, src, strlen(src)) && q->quoted) {
			dst += snprintf(dst, dstlen, "%s", "'");
			dst += formatted(dst, dstlen, C_STR(q, h), C_STRLEN(q, h), false, false);
			dst += snprintf(dst, dstlen, "%s", "'");
		} else
			dst += snprintf(dst, dstlen, "%s", C_STR(q, h));

		c = LIST_TAIL(c);

		if (!is_list(c))
			break;

		dst += snprintf(dst, dstlen, "%s", ",");
	}

	if (!cons) dst += snprintf(dst, dstlen, "%s", "]");
	return dst - save_dst;
}

static ssize_t print_term_to_buf_(query *q, char *dst, size_t dstlen, cell *c, pl_idx c_ctx, int running, int cons, unsigned print_depth, unsigned depth);

static ssize_t print_iso_list(query *q, char *save_dst, char *dst, size_t dstlen, cell *c, pl_idx c_ctx, int running, bool cons, unsigned print_depth, unsigned depth)
{
	unsigned print_list = 0;
	LIST_HANDLER(c);

	while (is_iso_list(c)) {
		if (g_tpl_interrupt)
			return 0;

		cell *save_c = c;
		pl_idx save_c_ctx = c_ctx;

		if (q->max_depth && (print_list >= q->max_depth)) {
			dst--;
			dst += snprintf(dst, dstlen, "%s", "|...]");
			q->last_thing = WAS_OTHER;
			return dst - save_dst;
		}

		if (!cons) {
			dst += snprintf(dst, dstlen, "%s", "[");
			q->last_thing = WAS_OTHER;
		}

		cell *head = LIST_HEAD(c);
		pl_idx head_ctx = c_ctx;
		head = running ? deref(q, head, c_ctx) : head;
		head_ctx = running ? q->latest_ctx : 0;
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

		int parens = is_structure(head) && special_op;
		if (parens) dst += snprintf(dst, dstlen, "%s", "(");
		q->parens = parens;
		ssize_t res = print_term_to_buf_(q, dst, dstlen, head, head_ctx, running, -1, 0, depth+1);
		q->parens = false;
		if (res < 0) return -1;
		dst += res;
		if (parens) dst += snprintf(dst, dstlen, "%s", ")");
		bool possible_chars = false;

		if (is_interned(head) && (C_STRLEN_UTF8(head) == 1) && q->double_quotes)
			possible_chars = true;

		cell *tail = LIST_TAIL(c);
		pl_idx tail_ctx = c_ctx;
		cell *save_tail = tail;

		if (q->max_depth && (print_depth >= q->max_depth)) {
			dst += snprintf(dst, dstlen, "|...]");
			q->last_thing = WAS_OTHER;
			return dst - save_dst;
		}

		if (is_var(tail) && running) {
			if (is_ref(tail))
				tail_ctx = tail->var_ctx;

			const frame *f = GET_FRAME(tail_ctx);
			slot *e = GET_SLOT(f, tail->var_nbr);
			tail = deref(q, tail, c_ctx);
			c_ctx = q->latest_ctx;

			if (e->vgen == q->vgen) {
				tail = save_tail;
			} else {
				e->vgen = q->vgen;
				possible_chars = false;
			}
		}

		size_t tmp_len = 0;

		if (is_interned(tail) && !is_structure(tail)) {
			const char *src = C_STR(q, tail);

			if (strcmp(src, "[]")) {
				dst += snprintf(dst, dstlen, "%s", "|");
				ssize_t res = print_term_to_buf_(q, dst, dstlen, tail, c_ctx, running, true, depth+1, depth+1);
				if (res < 0) return -1;
				dst += res;
			}
		} else if (q->st.m->flags.double_quote_chars && running
			&& !q->ignore_ops && possible_chars
			&& (tmp_len = scan_is_chars_list(q, tail, c_ctx, false) > 0))
			{
			char *tmp_src = chars_list_to_string(q, tail, c_ctx, tmp_len);

			if ((strlen(tmp_src) == 1) && (*tmp_src == '\''))
				dst += snprintf(dst, dstlen, "|\"%s\"", tmp_src);
			else if ((strlen(tmp_src) == 1) && needs_quoting(q->st.m, tmp_src, 1))
				dst += snprintf(dst, dstlen, ",'%s'", tmp_src);
			else if (strlen(tmp_src) == 1)
				dst += snprintf(dst, dstlen, ",%s", tmp_src);
			else
				dst += snprintf(dst, dstlen, "|\"%s\"", tmp_src);

			free(tmp_src);
			print_list++;
		} else if (is_string(tail) && !q->double_quotes) {
			dst += snprintf(dst, dstlen, "%s", ",");
			dst += print_string_list(q, dst, dst, dstlen, tail, c_ctx, running, 1, depth+1);
			dst += snprintf(dst, dstlen, "%s", "]");
			q->last_thing = WAS_OTHER;
			break;
		} else if (is_iso_list(tail)) {
			if ((tail == save_c) && (c_ctx == save_c_ctx) && running) {
				dst += snprintf(dst, dstlen, "%s", "|");
				dst += snprintf(dst, dstlen, "%s", !is_ref(save_tail) ? C_STR(q, save_tail) : "_");
			} else {
				dst += snprintf(dst, dstlen, "%s", ",");
				q->last_thing = WAS_COMMA;
				c = tail;
				print_list++;
				cons = true;
				continue;
			}
		} else if (is_string(tail) && q->double_quotes) {
			dst+= snprintf(dst, dstlen, "%s", "|\"");
			dst += formatted(dst, dstlen, C_STR(q, tail), C_STRLEN(q, tail), true, false);
			dst += snprintf(dst, dstlen, "%s", "\"");
			print_list++;
			q->last_thing = WAS_OTHER;
		} else {
			dst += snprintf(dst, dstlen, "%s", "|");
			bool parens = is_op(tail);
			if (parens) dst += snprintf(dst, dstlen, "%s", "(");
			ssize_t res = print_term_to_buf_(q, dst, dstlen, tail, c_ctx, running, true, depth+1, depth+1);
			if (res < 0) return -1;
			dst += res;
			if (parens) dst += snprintf(dst, dstlen, "%s", ")");
		}

		if (!cons || print_list) {
			dst += snprintf(dst, dstlen, "%s", "]");
			q->last_thing = WAS_OTHER;
		}

		return dst - save_dst;
	}

	return dst - save_dst;
}

static ssize_t print_term_to_buf_(query *q, char *dst, size_t dstlen, cell *c, pl_idx c_ctx, int running, int cons, unsigned print_depth, unsigned depth)
{
	char *save_dst = dst;

	if (depth > g_max_depth) {
		q->cycle_error = true;
		q->last_thing = WAS_OTHER;
		return -1;
	}

	if (q->is_dump_vars && (c->tag == TAG_INTEGER) && is_stream(c)) {
		int n = get_stream(q, c);
		stream *str = &q->pl->streams[n];
		sliter *iter = sl_first(str->alias);

		if (iter && sl_next(iter, NULL)) {
			const char *alias = sl_key(iter);

			if (strcmp(alias, "user_input") && strcmp(alias, "user_output") && strcmp(alias, "user_error"))
				dst += formatted(dst, dstlen, alias, strlen(alias), false, q->json);
			else
				dst += snprintf(dst, dstlen, "'<$stream>'(%d)", (int)get_smallint(c));

			sl_done(iter);
		} else
			dst += snprintf(dst, dstlen, "'<$stream>'(%d)", (int)get_smallint(c));

		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if ((c->tag == TAG_INTEGER) && (c->flags & FLAG_INT_STREAM)) {
		int n = get_stream(q, c);
		dst += snprintf(dst, dstlen, "'<$stream>'(%d)", (int)get_smallint(c));
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (q->is_dump_vars && is_blob(c)) {
		dst += snprintf(dst, dstlen, "'<$blob>'(0x%X)", (int)get_smallint(c));
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_number(c) && is_negative(c)) {
		if (is_negative(c) && (q->last_thing == WAS_SYMBOL)) {
			dst += snprintf(dst, dstlen, " ");
			q->last_thing = WAS_SPACE;
		}
	}

	if (is_rational(c)) {
		int radix = 10;

		if (!dstlen)
			dst += mp_int_string_len(&c->val_bigint->irat.num, radix) - 1;
		else {
			size_t len = mp_int_string_len(&c->val_bigint->irat.num, radix) - 1;
			mp_int_to_string(&c->val_bigint->irat.num, radix, dst, len+1);
			dst += strlen(dst);
		}

		dst += snprintf(dst, dstlen, " div ");

		if (!dstlen)
			dst += mp_int_string_len(&c->val_bigint->irat.den, radix) - 1;
		else {
			size_t len = mp_int_string_len(&c->val_bigint->irat.den, radix) - 1;
			mp_int_to_string(&c->val_bigint->irat.den, radix, dst, len+1);
			dst += strlen(dst);
		}

		if (dstlen) *dst = 0;
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
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

			if (c->flags & FLAG_INT_BINARY)
				dst += snprintf(dst, dstlen, "%s0b", is_negative(c)?"-":"");
			else if (c->flags & FLAG_INT_HEX)
				dst += snprintf(dst, dstlen, "%s0x", is_negative(c)?"-":"");
			else if ((c->flags & FLAG_INT_OCTAL) && !running)
				dst += snprintf(dst, dstlen, "%s0o", is_negative(c)?"-":"");
		}

		if (!dstlen)
			dst += mp_int_string_len(&c->val_bigint->ival, radix) - 1;
		else {
			size_t len = mp_int_string_len(&c->val_bigint->ival, radix) - 1;
			mp_int_to_string(&c->val_bigint->ival, radix, dst, len+1);
			dst += strlen(dst);
		}

		if (dstlen) *dst = 0;
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_smallint(c)) {
		//if (dstlen) printf("*** int %d,  was=%d, is=%d\n", (int)c->val_int, q->last_thing_was_symbol, false);

		if (q->listing) {
			if (((c->flags & FLAG_INT_HEX) || (c->flags & FLAG_INT_BINARY))) {
				dst += snprintf(dst, dstlen, "%s0x", get_smallint(c)<0?"-":"");
				dst += sprint_int(dst, dstlen, get_smallint(c), 16);
			} else if ((c->flags & FLAG_INT_OCTAL) && !running) {
				dst += snprintf(dst, dstlen, "%s0o", get_smallint(c)<0?"-":"");
				dst += sprint_int(dst, dstlen, get_smallint(c), 8);
			} else
				dst += sprint_int(dst, dstlen, get_smallint(c), 10);
		} else
			dst += sprint_int(dst, dstlen, get_smallint(c), 10);

		if (dstlen) *dst = 0;
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_float(c) && (get_float(c) == M_PI)) {
		dst += snprintf(dst, dstlen, "3.141592653589793");
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_float(c) && (get_float(c) == M_E)) {
		dst += snprintf(dst, dstlen, "2.718281828459045");
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_float(c)) {
		if (c->val_float == 0.0)
			c->val_float = fabs(c->val_float);

		char tmpbuf[256];
		sprintf(tmpbuf, "%.*g", 17, get_float(c));
		if (!q->json) reformat_float(q, tmpbuf, c->val_float);
		dst += snprintf(dst, dstlen, "%s", tmpbuf);
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_string(c) && !q->ignore_ops && q->double_quotes && 0) {
		dst += snprintf(dst, dstlen, "%s", "\"");
		dst += formatted(dst, dstlen, C_STR(q, c), C_STRLEN(q, c), true, q->json);
		dst += snprintf(dst, dstlen, "%s", "\"");
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_string(c) && q->ignore_ops) {
		ssize_t n = print_string_canonical(q, save_dst, dst, dstlen, c, c_ctx, running, cons > 0, depth+1);
		q->last_thing = WAS_OTHER;
		return n;
	}

	if (is_string(c) && !q->double_quotes) {
		ssize_t n = print_string_list(q, save_dst, dst, dstlen, c, c_ctx, running, cons > 0, depth+1);
		q->last_thing = WAS_OTHER;
		return n;
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
		dst += snprintf(dst, dstlen, "%s", "\"");
		unsigned cnt = 0;
		LIST_HANDLER(l);
		bool closing_quote = true;

		while (is_list(l)) {
			if (q->max_depth && (cnt++ >= q->max_depth)) {
				dst += snprintf(dst, dstlen, "%s", "\"||... ");
				closing_quote = false;
				break;
			}

			cell *h = LIST_HEAD(l);
			cell *c = running ? deref(q, h, c_ctx) : h;
			dst += formatted(dst, dstlen, C_STR(q, c), C_STRLEN(q, c), true, q->json);
			l = LIST_TAIL(l);
			l = running ? deref(q, l, c_ctx) : l;
			c_ctx = running ? q->latest_ctx : 0;
		}

		if (closing_quote) dst += snprintf(dst, dstlen, "%s", "\"");
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_iso_list(c) && !q->ignore_ops) {
		ssize_t n = print_iso_list(q, save_dst, dst, dstlen, c, c_ctx, running, cons > 0, print_depth+1, depth+1);
		q->last_thing = WAS_OTHER;
		return n;
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
		int quote = ((running <= 0) || q->quoted) && !is_var(c) && needs_quoting(q->st.m, src, src_len);
		int dq = 0, braces = 0;
		if (is_string(c) && q->double_quotes) dq = quote = 1;
		if (q->quoted < 0) quote = 0;
		if ((c->arity == 1) && is_interned(c) && !strcmp(src, "{}")) braces = 1;
		cell *c1 = c->arity ? deref(q, FIRST_ARG(c), c_ctx) : NULL;

		if (running && is_interned(c) && c->arity
			&& q->numbervars && !strcmp(src, "$VAR") && c1
			&& is_integer(c1) && (get_smallint(c1) >= 0)) {
			dst += snprintf(dst, dstlen, "%s", varformat2(q->pl->tmpbuf, sizeof(q->pl->tmpbuf), c1, 0));
		q->last_thing = WAS_OTHER;
			return dst - save_dst;
		}

		if (is_var(c) && !is_anon(c) && q->variable_names) {
			cell *l = q->variable_names;
			pl_idx l_ctx = q->variable_names_ctx;
			LIST_HANDLER(l);

			while (is_iso_list(l)) {
				cell *h = LIST_HEAD(l);
				h = running ? deref(q, h, l_ctx) : h;
				pl_idx h_ctx = running ? q->latest_ctx : 0;
				cell *name = running ? deref(q, h+1, h_ctx) : h+1;
				cell *var = running ? deref(q, h+2, h_ctx) : h+2;
				pl_idx var_ctx = running ? q->latest_ctx : h_ctx;

				if (is_var(var) && (var->var_nbr == c->var_nbr) && (var_ctx == c_ctx)) {
					if (!strcmp(C_STR(q, name), "_"))
						dst += print_variable(q, dst, dstlen, var, var_ctx, running);
					else
						dst += snprintf(dst, dstlen, "%s", C_STR(q, name));

					q->last_thing = WAS_OTHER;
					return dst - save_dst;
				}

				l = LIST_TAIL(l);
				l = running ? deref(q, l, l_ctx) : l;
				l_ctx = running ? q->latest_ctx : 0;
			}
		}

		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");

		if (is_var(c)) {
			dst += print_variable(q, dst, dstlen, c, c_ctx, running);
			q->last_thing = WAS_OTHER;
			return dst - save_dst;
		}

		unsigned len_str = src_len;

		if (braces && !q->ignore_ops)
			;
		else if (quote) {
			if (is_blob(c) && q->max_depth && (len_str >= q->max_depth) && (src_len > 128))
				len_str = q->max_depth;

			dst += formatted(dst, dstlen, src, len_str, dq, q->json);

			if (is_blob(c) && q->max_depth && (len_str > q->max_depth) && (src_len > 128)) {
				dst--;
				dst += snprintf(dst, dstlen, "%s", "...");
				q->last_thing = WAS_SYMBOL;
			} else
				q->last_thing = WAS_OTHER;
		} else {
			int ch = peek_char_utf8(src);
			bool is_symbol = !needs_quoting(q->st.m, src, src_len) && !iswalpha(ch)
				&& strcmp(src, "\\") && strcmp(src, ",") && strcmp(src, ";")
				&& strcmp(src, "[]") && strcmp(src, "{}") && !q->parens;

			if ((q->last_thing == WAS_SYMBOL) && is_symbol && !q->parens && !q->quoted) {
				dst += snprintf(dst, dstlen, "%s", " ");
				q->last_thing = WAS_SPACE;
			}

			dst += plain(dst, dstlen, src, len_str);
			q->last_thing = is_symbol ? WAS_SYMBOL : WAS_OTHER;;
		}

		dst += snprintf(dst, dstlen, "%s", !braces&&quote?dq?"\"":"'":"");
		q->did_quote = !braces&&quote;

		if (is_structure(c) && !is_string(c)) {
			pl_idx arity = c->arity;
			dst += snprintf(dst, dstlen, "%s", braces&&!q->ignore_ops?"{":"(");
			q->parens = true;

			for (c++; arity--; c += c->nbr_cells) {
				cell *tmp = c;
				pl_idx tmp_ctx = c_ctx;

				slot *e = NULL;

				uint64_t save_vgen = q->vgen - 1;

				if (is_var(c)) {
					if (is_ref(tmp))
						tmp_ctx = tmp->var_ctx;

					const frame *f = GET_FRAME(tmp_ctx);
					e = GET_SLOT(f, tmp->var_nbr);
					save_vgen = e->vgen;

					if (e->vgen == q->vgen) {
					} else {
						e->vgen = q->vgen;
						tmp = running ? deref(q, c, c_ctx) : c;
						tmp_ctx = running ? q->latest_ctx : 0;
					}
				}

				if (q->max_depth && ((depth+1) >= q->max_depth)) {
					dst += snprintf(dst, dstlen, "%s", "...");
					q->last_thing = WAS_SYMBOL;

					if (arity) {
						dst += snprintf(dst, dstlen, "%s", ",");
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

				if (parens) dst += snprintf(dst, dstlen, "%s", "(");

				q->parens = parens;
				ssize_t res = print_term_to_buf_(q, dst, dstlen, tmp, tmp_ctx, running, 0, depth+1, depth+1);
				q->parens = false;
				if (res < 0) return -1;
				dst += res;
				if (e) e->vgen = save_vgen;

				if (parens)
					dst += snprintf(dst, dstlen, "%s", ")");

				if (arity)
					dst += snprintf(dst, dstlen, "%s", ",");
			}

			dst += snprintf(dst, dstlen, "%s", braces&&!q->ignore_ops?"}":")");
			q->parens = false;
		}

		if (q->last_thing != WAS_SYMBOL)
			q->last_thing = WAS_OTHER;

		return dst - save_dst;
	}

	size_t srclen = src_len;

	if (is_postfix(c)) {
		cell *lhs = c + 1;
		lhs = running ? deref(q, lhs, c_ctx) : lhs;

		if (!is_var(lhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
			if (q->last_thing != WAS_SPACE) dst += snprintf(dst, dstlen, "%s", " ");
			dst += snprintf(dst, dstlen, "%s", "...");
			q->last_thing = WAS_SYMBOL;
			return dst - save_dst;
		} else {
			pl_idx lhs_ctx = running ? q->latest_ctx : 0;
			ssize_t res = print_term_to_buf_(q, dst, dstlen, lhs, lhs_ctx, running, 0, 0, depth+1);
			if (res < 0) return -1;
			dst += res;
		}

		bool space = (c->val_off == g_minus_s) && (is_number(lhs) || search_op(q->st.m, C_STR(q, lhs), NULL, true));
		if ((c->val_off == g_plus_s) && search_op(q->st.m, C_STR(q, lhs), NULL, true) && lhs->arity) space = true;
		if (isalpha(*src)) space = true;

		if ((q->last_thing != WAS_SPACE) && space) {
			dst += snprintf(dst, dstlen, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		int quote = q->quoted && has_spaces(src, src_len);
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?" '":"");

		dst += plain(dst, dstlen, src, srclen);
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");
		q->last_thing = WAS_OTHER;
		return dst - save_dst;
	}

	if (is_prefix(c)) {
		if (q->last_thing == WAS_SYMBOL) {
			dst += snprintf(dst, dstlen, " ");
			q->last_thing = WAS_SPACE;
		}

		cell *rhs = c + 1;
		rhs = running ? deref(q, rhs, c_ctx) : rhs;
		pl_idx rhs_ctx = running ? q->latest_ctx : 0;
		unsigned my_priority = search_op(q->st.m, src, NULL, true);
		unsigned rhs_pri = is_interned(rhs) ? search_op(q->st.m, C_STR(q, rhs), NULL, true) : 0;

		if ((q->last_thing == WAS_SYMBOL) && !strcmp(src, "\\+")) {
			dst += snprintf(dst, dstlen, "%s", " ");
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

		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"'":"");
		dst += plain(dst, dstlen, src, srclen);
		if (quote) dst += snprintf(dst, dstlen, "%s", quote?"' ":"");

		if (space || parens) {
			dst += snprintf(dst, dstlen, "%s", " ");
			q->last_thing = WAS_SPACE;
		}

		if (!is_var(rhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
			if (!space) dst += snprintf(dst, dstlen, "%s", " ");
			dst += snprintf(dst, dstlen, "%s", "...");
			q->last_thing = WAS_SYMBOL;
			return dst - save_dst;
		}

		if (parens) { dst += snprintf(dst, dstlen, "%s", "("); q->last_thing = WAS_OTHER; }
		q->parens = parens;
		ssize_t res = print_term_to_buf_(q, dst, dstlen, rhs, rhs_ctx, running, 0, 0, depth+1);
		q->parens = false;
		if (res < 0) return -1;
		dst += res;
		if (parens) { dst += snprintf(dst, dstlen, "%s", ")"); q->last_thing = WAS_OTHER; }

		return dst - save_dst;
	}

	// Infix..

	cell *lhs = c + 1;
	cell *rhs = lhs + lhs->nbr_cells;
	lhs = running ? deref(q, lhs, c_ctx) : lhs;
	pl_idx lhs_ctx = running ? q->latest_ctx : 0;
	rhs = running ? deref(q, rhs, c_ctx) : rhs;
	pl_idx rhs_ctx = running ? q->latest_ctx : 0;

	unsigned lhs_pri_1 = is_interned(lhs) ? search_op(q->st.m, C_STR(q, lhs), NULL, is_prefix(rhs)) : 0;
	unsigned lhs_pri_2 = is_interned(lhs) && !lhs->arity ? search_op(q->st.m, C_STR(q, lhs), NULL, false) : 0;
	unsigned rhs_pri_1 = is_interned(rhs) ? search_op(q->st.m, C_STR(q, rhs), NULL, is_prefix(rhs)) : 0;
	unsigned rhs_pri_2 = is_interned(rhs) && !rhs->arity ? search_op(q->st.m, C_STR(q, rhs), NULL, false) : 0;

	// Print LHS..

	bool lhs_parens = lhs_pri_1 >= my_priority;
	if ((lhs_pri_1 == my_priority) && is_yfx(c)) lhs_parens = false;
	if (lhs_pri_2 > 0) lhs_parens = true;
	if (is_structure(lhs) && (lhs_pri_1 <= my_priority) && (lhs->val_off == g_plus_s)) { lhs_parens = false; }
	bool lhs_space = false;

	if ((q->last_thing != WAS_SPACE) && lhs_space) {
		dst += snprintf(dst, dstlen, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	ssize_t res;

	if (!is_var(lhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
		if (q->last_thing != WAS_SPACE) dst += snprintf(dst, dstlen, "%s", " ");
		dst += snprintf(dst, dstlen, "%s", "...");
		q->last_thing = WAS_SYMBOL;
	} else {
		if (lhs_parens) dst += snprintf(dst, dstlen, "%s", "(");
		q->parens = lhs_parens;
		res = print_term_to_buf_(q, dst, dstlen, lhs, lhs_ctx, running, 0, 0, depth+1);
		q->parens = false;
		if (res < 0) return -1;
		dst += res;
		if (lhs_parens) dst += snprintf(dst, dstlen, "%s", ")");
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
		dst += snprintf(dst, dstlen, "%s", " ");
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
		dst += snprintf(dst, dstlen, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	// Print OP..

	//q->last_thing_was_symbol += is_symbol;
	space = iswalpha(*src) || (q->last_thing == WAS_SYMBOL);

	if (q->listing && !depth && !strcmp(src, ":-"))
		space = true;

	if ((q->last_thing != WAS_SPACE) && space) {
		dst += snprintf(dst, dstlen, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	int quote = q->quoted && has_spaces(src, src_len);
	if (op_needs_quoting(q->st.m, src, src_len)) quote = 1;
	if (quote) dst += snprintf(dst, dstlen, "%s", quote?" '":"");
	dst += plain(dst, dstlen, src, srclen);
	if (quote) dst += snprintf(dst, dstlen, "%s", quote?"' ":"");
	q->last_thing = strcmp(src, "|") ? WAS_SYMBOL : WAS_OTHER;

	if (q->listing && !depth && !strcmp(src, ":-")) {
		dst += snprintf(dst, dstlen, "%s", "\n  ");
	}

	if ((q->last_thing != WAS_SPACE) && space) {
		dst += snprintf(dst, dstlen, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	// Print RHS..

	bool rhs_parens = rhs_pri_1 >= my_priority;
	space = is_number(rhs) && is_negative(rhs);

	if (!rhs_parens && is_prefix(rhs) && strcmp(src, "|"))
		space = true;

	bool rhs_is_symbol = is_interned(rhs) && !rhs->arity
		&& !iswalpha(*C_STR(q, rhs)) && !needs_quoting(q->st.m, C_STR(q, rhs), C_STRLEN(q, rhs))
		&& strcmp(C_STR(q, rhs), "[]") && strcmp(C_STR(q, rhs), "{}")
		&& !rhs_parens;

	if (rhs_is_symbol && strcmp(C_STR(q, rhs), "[]") && strcmp(C_STR(q, rhs), "{}") && strcmp(C_STR(q, rhs), "!"))
		space = true;

	if ((rhs_pri_1 == my_priority) && is_xfy(c))
		rhs_parens = false;

	if (rhs_pri_2 > 0)
		rhs_parens = true;

	if ((q->last_thing != WAS_SPACE) && space && !rhs_parens) {
		dst += snprintf(dst, dstlen, "%s", " ");
		q->last_thing = WAS_SPACE;
	}

	if (!is_var(rhs) && q->max_depth && ((depth+1) >= q->max_depth)) {
		if (q->last_thing != WAS_SPACE) dst += snprintf(dst, dstlen, "%s", " ");
		dst += snprintf(dst, dstlen, "%s", "...");
		q->last_thing = WAS_SYMBOL;
	} else {
		if (rhs_parens) { dst += snprintf(dst, dstlen, "%s", "("); q->last_thing = WAS_OTHER; }
		q->parens = rhs_parens || space;
		res = print_term_to_buf_(q, dst, dstlen, rhs, rhs_ctx, running, 0, 0, depth+1);
		q->parens = false;
		if (res < 0) return -1;
		dst += res;
		if (rhs_parens) { dst += snprintf(dst, dstlen, "%s", ")"); q->last_thing = WAS_OTHER; }
		else if (rhs_is_symbol) { q->last_thing = WAS_SYMBOL; }
	}

	return dst - save_dst;
}

ssize_t print_term_to_buf(query *q, char *dst, size_t dstlen, cell *c, pl_idx c_ctx, int running, int cons)
{
	return print_term_to_buf_(q, dst, dstlen, c, c_ctx, running, cons, 0, 0);
}

char *print_canonical_to_strbuf(query *q, cell *c, pl_idx c_ctx, int running)
{
	pl_int skip = 0, max = 1000000000;
	pl_idx tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_var(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	if (++q->vgen == 0) q->vgen = 1;
	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false);
	char *buf = malloc(len*2+1);
	check_error(buf, clear_write_options(q));
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	len = print_term_to_buf(q, buf, len+1, c, c_ctx, running, false);
	q->ignore_ops = false;
	q->quoted = 0;
	return buf;
}

bool print_canonical_to_stream(query *q, stream *str, cell *c, pl_idx c_ctx, int running)
{
	pl_int skip = 0, max = 1000000000;
	pl_idx tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_var(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	if (++q->vgen == 0) q->vgen = 1;
	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false);
	char *dst = malloc(len*2+1);
	check_heap_error(dst, clear_write_options(q));
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, false);
	q->ignore_ops = false;
	q->quoted = 0;
	const char *src = dst;

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			free(dst);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return true;
}

bool print_canonical(query *q, FILE *fp, cell *c, pl_idx c_ctx, int running)
{
	pl_int skip = 0, max = 1000000000;
	pl_idx tmp_ctx = c_ctx;
	cell tmp = {0};

	if (running && is_iso_list(c)) {
		cell *t = skip_max_list(q, c, &tmp_ctx, max, &skip, &tmp);

		if (t && !is_var(t) && !skip)
			running = 0;
	} else if (running && is_cyclic_term(q, c, c_ctx)) {
		running = 0;
	}

	if (++q->vgen == 0) q->vgen = 1;
	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false);
	q->did_quote = false;
	char *dst = malloc(len*2+1);
	check_heap_error(dst, clear_write_options(q));
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, false);
	q->ignore_ops = false;
	q->quoted = 0;
	const char *src = dst;

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			free(dst);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return true;
}

char *print_term_to_strbuf(query *q, cell *c, pl_idx c_ctx, int running)
{
	if (++q->vgen == 0) q->vgen = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	//q->last_thing_was_space = true;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false);

	if ((len < 0) || q->cycle_error) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false);
	}

	char *buf = malloc(len*2+1);
	check_error(buf, clear_write_options(q));
	if (++q->vgen == 0) q->vgen = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	//q->last_thing_was_space = true;
	len = print_term_to_buf(q, buf, len+1, c, c_ctx, running, false);
	return buf;
}

bool print_term_to_stream(query *q, stream *str, cell *c, pl_idx c_ctx, int running)
{
	if (++q->vgen == 0) q->vgen = 1;
	q->did_quote = false;
	q->last_thing = WAS_SPACE;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false);

	if ((len < 0) || q->cycle_error) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false);
	}

	char *dst = malloc(len*2+1);
	check_heap_error(dst, clear_write_options(q));
	if (++q->vgen == 0) q->vgen = 1;
	q->did_quote = false;
	q->last_thing = WAS_SPACE;
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, false);
	const char *src = dst;

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (feof(str->fp)) {
			q->error = true;
			free(dst);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
	return true;
}

bool print_term(query *q, FILE *fp, cell *c, pl_idx c_ctx, int running)
{
	if (++q->vgen == 0) q->vgen = 1;
	q->did_quote = false;
	q->last_thing = WAS_SPACE;
	ssize_t len = print_term_to_buf(q, NULL, 0, c, c_ctx, running, false);

	if ((len < 0) || q->cycle_error) {
		len = print_term_to_buf(q, NULL, 0, c, c_ctx, running=0, false);
	}

	char *dst = malloc(len*2+1);
	check_heap_error(dst, clear_write_options(q));
	if (++q->vgen == 0) q->vgen = 1;
	q->did_quote = false;
	q->last_thing = WAS_SPACE;
	len = print_term_to_buf(q, dst, len+1, c, c_ctx, running, false);
	const char *src = dst;

	while (len) {
		size_t nbytes = fwrite(src, 1, len, fp);

		if (feof(fp)) {
			q->error = true;
			free(dst);
			return false;
		}

		len -= nbytes;
		src += nbytes;
	}

	free(dst);
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
	memset(q->ignores, 0, sizeof(q->ignores));
}
