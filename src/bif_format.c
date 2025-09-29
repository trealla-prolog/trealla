#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "module.h"
#include "network.h"
#include "prolog.h"
#include "query.h"

static int format_integer(char *dst, cell *c, int grouping, int sep, int decimals, int radix)
{
	char *tmpbuf1 = NULL, *tmpbuf2 = NULL;
	char xtmpbuf1[256], xtmpbuf2[256];

	if (is_smallint(c)) {
		pl_int v = get_smallint(c);
		sprint_int(xtmpbuf1, sizeof(xtmpbuf1), v, radix);
		tmpbuf1 = xtmpbuf1;
		tmpbuf2 = xtmpbuf2;
	} else {
		size_t len = mp_int_string_len(&c->val_bigint->ival, radix) - 1;
		tmpbuf1 = malloc(len+1);
		check_error(tmpbuf1);
		mp_int_to_string(&c->val_bigint->ival, radix, tmpbuf1, len+1);
		len *= 2;
		tmpbuf2 = malloc(len+1);
		check_error(tmpbuf2);
	}

	const char *src = tmpbuf1 + strlen(tmpbuf1) - 1;	// start from back
	char *dst2 = tmpbuf2;
	int i = 1, j = 1;

	while (src > tmpbuf1) {
		*dst2++ = *src--;

		if (grouping && !decimals && !(i++ % grouping) && *src)
			*dst2++ = sep;

		if (decimals && (j++ == decimals)) {
			*dst2++ = '.';
			decimals = 0;
			i = 1;
		}
	}

	*dst2++ = *src;
	*dst2 = '\0';
	src = tmpbuf2 + (strlen(tmpbuf2) - 1);
	dst2 = dst;

	while (src >= tmpbuf2)
		*dst2++ = *src--;

	*dst2 = '\0';

	if (!is_smallint(c)) {
		free(tmpbuf1);
		free(tmpbuf2);
	}

	return dst2 - dst;
}

typedef struct {
	cell *p;
	pl_ctx p_ctx;
	const char *srcbuf;
	const char *src;
	size_t srclen;
}
 list_reader_t;

static int get_next_char(query *q, list_reader_t *fmt)
{
	if (fmt->src) {
		int len = len_char_utf8(fmt->src);
		int ch = get_char_utf8(&fmt->src);
		fmt->srclen -= len;
		return ch;
	}

	fmt->p = fmt->p + 1;
	cell *head = deref(q, fmt->p, fmt->p_ctx);
	int ch;

	if (is_smallint(head))
		ch = get_smallint(head);
	else if (is_atom(head)) {
		const char *s = C_STR(q, head);
		ch = peek_char_utf8(s);
	} else
		return -1;

	fmt->p = fmt->p + fmt->p->num_cells;
	fmt->p = deref(q, fmt->p, fmt->p_ctx);
	fmt->p_ctx = q->latest_ctx;
	return ch;
}

static cell *get_next_cell(query *q, list_reader_t *fmt, bool *is_var, pl_ctx *ctx)
{
	*is_var = false;

	if (fmt->src)
		return NULL;

	if (!is_list(fmt->p)) {
		if (is_var(fmt->p))
			*is_var = true;

		return NULL;
	}

	cell *head = fmt->p + 1;
	cell *tail = head + head->num_cells;

	head = deref(q, head, fmt->p_ctx);
	*ctx = q->latest_ctx;

	fmt->p = deref(q, tail, fmt->p_ctx);
	fmt->p_ctx = q->latest_ctx;
	return head;
}

static bool is_more_data(query *q, list_reader_t *fmt)
{
	(void)q;

	if (fmt->src)
		return fmt->srclen;

	if (!fmt->p)
		return false;

	return is_list(fmt->p);
}

#define CHECK_BUF(len) {									\
	unsigned n = ((len) > 0 ? (len) : 1) + 1;	            \
	if (n >= tmpbuf_free) {									\
		size_t save_offset = dst - tmpbuf;					\
		tmpbuf_size += n;									\
		tmpbuf = realloc(tmpbuf, (tmpbuf_size*=2));			\
		checked(tmpbuf);							\
		dst = tmpbuf + save_offset;							\
		tmpbuf_free = tmpbuf_size - save_offset;			\
	}                                                       \
	tmpbuf_free -= len;										\
}

bool do_format(query *q, cell *str, pl_ctx str_ctx, cell *p1, pl_ctx p1_ctx, cell *p2, pl_ctx p2_ctx)
{
	list_reader_t fmt1 = {0}, fmt2 = {0};
	list_reader_t save_fmt1 = {0}, save_fmt2 = {0};
	fmt1.p = p1;
	fmt1.p_ctx = p1_ctx;
	fmt1.srcbuf = is_atom(p1) ? C_STR(q, p1) : NULL;
	fmt1.srclen = is_atom(p1) ? C_STRLEN(q, p1) : 0;
	fmt1.src = fmt1.srcbuf;
	fmt2.p = p2;
	fmt2.p_ctx = p2_ctx;

	size_t tmpbuf_size = 1024*8;
	char *tmpbuf = malloc(tmpbuf_size);
	checked(tmpbuf);
	char *dst = tmpbuf;
	*dst = '\0';
	size_t tmpbuf_free = tmpbuf_size;
	bool redo = false, start_of_line = true;
	int tab_at = 1, tabs = 0, diff = 0, last_at = 0, tab_char = ' ';
	save_fmt1 = fmt1;
	save_fmt2 = fmt2;

	while (is_more_data(q, &fmt1)) {
		int argval = 0, noargval = 1, argval_specified = 0;
		int pos = dst - tmpbuf + 1;
		list_reader_t tmp_fmt1 = fmt1, tmp_fmt2 = fmt2;

		int ch = get_next_char(q, &fmt1);

		if (ch != '~') {
			CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
			dst += put_char_utf8(dst, ch);
			start_of_line = ch == '\n';
			continue;
		}

		ch = get_next_char(q, &fmt1);
		pl_ctx c_ctx = p2_ctx;

		if (ch == '*') {
			bool is_var;
			cell *c = get_next_cell(q, &fmt2, &is_var, &c_ctx);

			if (is_var)
				return throw_error(q, p2, p2_ctx, "instantiation_error", "atom");

			noargval = 0;

			if (!c || !is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "integer");
			}

			argval = get_smallint(c);
			argval_specified = 1;
			ch = get_next_char(q, &fmt1);
		} else if (ch == '`') {
			ch = get_next_char(q, &fmt1);
			argval = ch;
			ch = get_next_char(q, &fmt1);
		} else {
			while (isdigit(ch)) {
				argval_specified = 1;
				noargval = 0;
				argval *= 10;
				argval += ch - '0';
				ch = get_next_char(q, &fmt1);
				continue;
			}
		}

		CHECK_BUF(argval);

		if (ch == 'n') {
			while (argval-- > 1)
				*dst++ = '\n';

			*dst++ = '\n';
			start_of_line = true;
			continue;
		}

		if (ch == 'N') {
			if (!start_of_line) {
				start_of_line = true;
				CHECK_BUF(1);
				*dst++ = '\n';
			}

			continue;
		}

		if (ch == 't') {
			if (!redo && !tabs) {
				save_fmt1 = tmp_fmt1;
				save_fmt2 = tmp_fmt2;
				tab_at = pos;
				tabs++;
			} else if (!redo) {
				tabs++;
			} else if (redo) {
				tab_char = argval ? argval : ' ';

				for (int i = 0; i < diff; i++) {
					CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
					dst += put_char_utf8(dst, tab_char);
				}
			}

			continue;
		}

		if (ch == '|') {
			int at = last_at = argval ? argval : pos;

			if (!argval)
				last_at -= 1;

			if (!tabs) {
				for (int i = 0; i < (argval+1)-pos; i++) {
					CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
					dst += put_char_utf8(dst, tab_char);
				}

				continue;
			}

			if (!redo) {
				if (!tabs) {
					tab_at = pos;
					dst = tmpbuf + tab_at - 1;
					diff = (at - pos) + 1;

					for (int i = 0; i < diff; i++) {
						CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
						dst += put_char_utf8(dst, tab_char);
					}
				} else {
					fmt1 = save_fmt1;
					fmt2 = save_fmt2;
					dst = tmpbuf + tab_at - 1;
					diff = ((at - pos) + 1) / tabs;
				}
			} else {
				tabs = 0;
			}

			redo = !redo;
			continue;
		}

		if (ch == '+') {
			if (!tabs)
				continue;

			if (!redo) {
				int at = last_at = argval ? (last_at+argval) : 8;

				if (!tabs) {
					tab_at = pos;
					dst = tmpbuf + tab_at - 1;
					diff = (at - pos) + 1;

					for (int i = 0; i < diff; i++) {
						CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
						dst += put_char_utf8(dst, tab_char);
					}
				} else {
					fmt1 = save_fmt1;
					fmt2 = save_fmt2;
					dst = tmpbuf + tab_at - 1;
					diff = ((at - pos) + 1) / tabs;
				}
			} else {
				tabs = 0;
			}

			redo = !redo;
			continue;
		}

		if (ch == '~') {
			CHECK_BUF(1);
			*dst++ = '~';
			start_of_line = false;
			continue;
		}

		if (!p2 || !is_list(p2))
			return throw_error(q, make_nil(), q->st.cur_frame, "domain_error", "non_empty_list");

		bool is_var;
		cell *c = get_next_cell(q, &fmt2, &is_var, &c_ctx);

		if (is_var)
			return throw_error(q, p2, p2_ctx, "instantiation_error", "atom");

		if (!c)
			return throw_error(q, make_nil(), p2_ctx, "domain_error", "non_empty_list");

		if (ch == 'i')
			continue;

		start_of_line = false;
		size_t len = 0;

		if ((ch == 'a') && !is_atom(c)) {
			free(tmpbuf);
			return throw_error(q, c, q->st.cur_frame, "type_error", "atom");
		}

		switch(ch) {
		case 's':
			if (is_nil(c)) {
			} else if (is_atom(c)) {
				int len = noargval ? (int)C_STRLEN_UTF8(c) : MIN_OF(argval, (int)C_STRLEN_UTF8(c));
				const char *src = C_STR(q, c);

				while (len--) {
					int ch = get_char_utf8(&src);
					CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
					dst += put_char_utf8(dst, ch);
					argval--;
				}

				argval -= len;
			} else {
				list_reader_t fmt3 = {0};
				fmt3.p = c;
				fmt3.p_ctx = c_ctx;
				int len = noargval ? INT_MAX : argval;

				while (is_more_data(q, &fmt3) && len--) {
					int ch = get_next_char(q, &fmt3);
					CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
					dst += put_char_utf8(dst, ch);
					argval--;
				}

				len = 0;
			}

			while (!noargval && argval--) {
				CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
				dst += put_char_utf8(dst, ' ');
			}

			break;

		case 'c':
			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "integer");
			}

			while (argval-- > 1) {
				CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
				dst += put_char_utf8(dst, (int)get_smallint(c));
			}

			CHECK_BUF(MAX_BYTES_PER_CODEPOINT);
			dst += put_char_utf8(dst, (int)get_smallint(c));
			len = 0;
			break;

		case 'e':
		case 'E':
			if (!is_float(c) && !is_smallint(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "float");
			}

			len = argval < 4096 ? 4096 : argval;
			CHECK_BUF(len);

			if (argval || argval_specified) {
				if (ch == 'e')
					//len = snprintf(dst, len, "%.*e", argval?argval:1, is_float(c) ? (argval?get_float(c):floor(get_float(c))) : get_smallint(c));
					len = snprintf(dst, len, "%.*e", argval, is_float(c) ? get_float(c) : get_smallint(c));
				else
					//len = snprintf(dst, len, "%.*", argval?argval:1, is_float(c) ? (argval?get_float(c):floor(get_float(c))) : get_smallint(c));
					len = snprintf(dst, len, "%.*E", argval, is_float(c) ? get_float(c) : get_smallint(c));
			} else {
				if (ch == 'e')
					len = snprintf(dst, len, "%e", is_float(c) ? get_float(c) : get_smallint(c));
				else
					len = snprintf(dst, len, "%E", is_float(c) ? get_float(c) : get_smallint(c));
			}

			break;

		case 'g':
		case 'G':
			if (!is_float(c) && !is_smallint(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "float");
			}

			len = argval < 4096 ? 4096 : argval;
			CHECK_BUF(len);

			if (argval || argval_specified) {
				if (ch == 'g')
					//len = snprintf(dst, len, "%.*g", argval?argval:1, is_float(c) ? (argval?get_float(c):floor(get_float(c))) : get_smallint(c));
					len = snprintf(dst, len, "%.*g", argval, is_float(c) ? get_float(c) : get_smallint(c));
				else
					//len = snprintf(dst, len, "%.*G", argval?argval:1, is_float(c) ? (argval?get_float(c):floor(get_float(c))) : get_smallint(c));
					len = snprintf(dst, len, "%.*G", argval, is_float(c) ? get_float(c) : get_smallint(c));
			} else {
				if (ch == 'g')
					len = snprintf(dst, len, "%g", is_float(c) ? get_float(c) : get_smallint(c));
				else
					len = snprintf(dst, len, "%G", is_float(c) ? get_float(c) : get_smallint(c));
			}

			break;

		case 'f':
			if (!is_float(c) && !is_smallint(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "float");
			}

			len = argval < 4096 ? 4096 : argval;
			CHECK_BUF(len);

			if (argval || argval_specified)
				//len = snprintf(dst, len, "%.*f", argval?argval:1, is_float(c) ? (argval?get_float(c):floor(get_float(c))) : get_smallint(c));
				len = snprintf(dst, len, "%.*f", argval, is_float(c) ? get_float(c) : get_smallint(c));
			else
				len = snprintf(dst, len, "%f", is_float(c) ? get_float(c) : get_smallint(c));

			break;

		case 'I':
			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "integer");
			}

			char *tmpbuf2 = print_term_to_strbuf(q, c, 0, 0);
			len = strlen(tmpbuf2);
			free(tmpbuf2);
			CHECK_BUF(len*2+1);
			len = format_integer(dst, c, noargval?3:argval, '_', 0, 10);
			break;

		case 'd':
			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "integer");
			}

			tmpbuf2 = print_term_to_strbuf(q, c, 0, 0);
			len = strlen(tmpbuf2);
			free(tmpbuf2);
			CHECK_BUF(len*2+1);
			len = format_integer(dst, c, 0, ',', noargval?0:argval, 10);
			break;

		case 'D':
			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "integer");
			}

			tmpbuf2 = print_term_to_strbuf(q, c, 0, 0);
			len = strlen(tmpbuf2);
			free(tmpbuf2);
			CHECK_BUF(len*2+1);
			len = format_integer(dst, c, 3, ',', noargval?0:argval, 10);
			break;

		case 'r':
			if (!noargval && ((argval < 2) || (argval > 36))) {
				free(tmpbuf);
				return throw_error(q, p1, p1_ctx, "domain_error", "radix_invalid");
			}

			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "integer");
			}

			tmpbuf2 = print_term_to_strbuf(q, c, 0, 0);
			len = strlen(tmpbuf2);
			free(tmpbuf2);
			CHECK_BUF(len*10);
			len = format_integer(dst, c, 0, ',', 0, !argval?8:argval);
			break;

		case 'R':
			if (!noargval && ((argval < 2) || (argval > 36))) {
				free(tmpbuf);
				return throw_error(q, p1, p1_ctx, "domain_error", "radix_invalid");
			}

			if (!is_integer(c)) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "type_error", "integer");
			}

			tmpbuf2 = print_term_to_strbuf(q, c, 0, 0);
			len = strlen(tmpbuf2);
			free(tmpbuf2);
			CHECK_BUF(len*10);
			len = format_integer(dst, c, 0, ',', 0, !argval?-8:-argval);
			break;

		case 'p': {
			cell *tmp;
			pl_idx num_cells;
			cell pc = {0};
			make_atom(&pc, new_atom(q->pl, "portray"));
			pc.arity = 1;

			predicate *pr = find_predicate(q->st.m, &pc);

			if (pr && pr->cnt) {
				if (str) {
					cell p1[1+1+c->num_cells];
					make_instr(p1+0, new_atom(q->pl, "$portray"), NULL, 2, 1+c->num_cells);
					p1[1] = *str;
					dup_cells_by_ref(p1+2, c, c_ctx, c->num_cells);
					tmp = prepare_call(q, CALL_SKIP, p1, q->st.cur_frame, 1);
					num_cells = p1->num_cells;
				} else {
					cell p1[1+c->num_cells];
					make_instr(p1+0, new_atom(q->pl, "$portray"), NULL, 1, c->num_cells);
					dup_cells_by_ref(p1+1, c, c_ctx, c->num_cells);
					tmp = prepare_call(q, CALL_SKIP, p1, q->st.cur_frame, 1);
					num_cells = p1->num_cells;
				}

				make_end(tmp+num_cells);
				query *q2 = query_create_subquery(q, tmp);
				start(q2);
				query_destroy(q2);
				clear_write_options(q);
				break;
			}

			if (!str) {
				int n = q->pl->current_output;
				stream *str2 = &q->pl->streams[n];
				q->quoted = 1;
				q->numbervars = true;
				print_term_to_stream(q, str2, c, c_ctx, 1);
				q->numbervars = false;
				q->quoted = 0;

				if (isatty(fileno(str2->fp)))
					fflush(str2->fp);
			} else {
				int n = get_stream(q, str);
				stream *str2 = &q->pl->streams[n];
				q->quoted = 1;
				q->numbervars = true;
				print_term_to_stream(q, str2, c, c_ctx, 1);
				q->numbervars = false;
				q->quoted = 0;

				if (isatty(fileno(str2->fp)))
					fflush(str2->fp);
			}

			break;
		}

		case 'k':
		case 'q':
		case 'w':
		case 'a':
		{
			int saveq = q->quoted;
			bool canonical = false, quoted = false;
			q->numbervars = true;
			q->double_quotes = true;

			if (ch == 'k') {
				canonical = true;
			} else if ((ch == 'q') ||(ch == 'p')) {
				quoted = true;
			}

			if (quoted)
				q->quoted = 1;

			if (is_string(c) && !q->quoted)
				q->quoted = -1;

			if (argval)
				q->max_depth = argval;

			if (canonical) {
				q->ignore_ops = true;
				q->quoted = 1;
			}

			char *tmpbuf2 = print_term_to_strbuf(q, c, c_ctx, 1);

			if (q->cycle_error) {
				free(tmpbuf);
				return throw_error(q, c, q->st.cur_frame, "resource_error", "cyclic");
			}

			len = strlen(tmpbuf2);
			CHECK_BUF(len);
			strcpy(dst, tmpbuf2);
			free(tmpbuf2);
			clear_write_options(q);
			q->quoted = saveq;
			break;
		}

		default:
			free(tmpbuf);
			return throw_error(q, c, q->st.cur_frame, "existence_error", "format_character");
		}

		dst += len;
	}

	*dst = '\0';
	size_t len = dst - tmpbuf;

	if (fmt2.p) {
		cell *save_l = fmt2.p;
		pl_ctx save_l_ctx = fmt2.p_ctx, c_ctx;
		bool is_var;
		cell *c = get_next_cell(q, &fmt2, &is_var, &c_ctx);

		if (is_var)
			return throw_error(q, p2, p2_ctx, "instantiation_error", "atom");

		if (c)
			return throw_error(q, save_l, save_l_ctx, "domain_error", "empty_list");
	}

	if (str == NULL) {
		int n = q->st.m->pl->current_output;
		stream *str = &q->pl->streams[n];
		const char *tmpsrc = tmpbuf;

		while (len) {
			size_t tmpbuf_free = net_write(tmpsrc, len, str);

			if (!tmpbuf_free) {
				if (feof(str->fp) || ferror(str->fp)) {
					free(tmpbuf);
					fprintf(stderr, "Error: end of file on write\n");
					return false;
				}
			}

			clearerr(str->fp);
			len -= tmpbuf_free;
			tmpsrc += tmpbuf_free;
		}
	} else if (is_compound(str)
		&& ((CMP_STRING_TO_CSTR(q, str, "atom")
		&& CMP_STRING_TO_CSTR(q, str, "chars")
		&& CMP_STRING_TO_CSTR(q, str, "string"))
		|| (str->arity > 1) || !is_var(str+1))) {
		free(tmpbuf);
		return throw_error(q, str, str_ctx, "type_error", "structure");
	} else if (is_compound(str) && !CMP_STRING_TO_CSTR(q, str, "atom")) {
		cell *c = deref(q, str+1, str_ctx);
		cell tmp;
		checked(make_cstringn(&tmp, tmpbuf, len), free(tmpbuf));
		unify(q, c, q->latest_ctx, &tmp, q->st.cur_frame);
		unshare_cell(&tmp);
	} else if (is_compound(str)) {
		cell *c = deref(q, str+1, str_ctx);
		cell tmp;

		if (strlen(tmpbuf))
			checked(make_stringn(&tmp, tmpbuf, len), free(tmpbuf));
		else
			make_atom(&tmp, g_nil_s);

		unify(q, c, q->latest_ctx, &tmp, q->st.cur_frame);
		unshare_cell(&tmp);
	} else if (is_stream(str)) {
		int n = get_stream(q, str);
		stream *str = &q->pl->streams[n];
		const char *tmpsrc = tmpbuf;

		while (len) {
			size_t tmpbuf_free = net_write(tmpsrc, len, str);

			if (!tmpbuf_free) {
				if (feof(str->fp) || ferror(str->fp)) {
					free(tmpbuf);
					fprintf(stderr, "Error: end of file on write\n");
					return false;
				}
			}

			clearerr(str->fp);
			len -= tmpbuf_free;
			tmpsrc += tmpbuf_free;
		}
	} else {
		free(tmpbuf);
		return throw_error(q, str, str_ctx, "domain_error", "stream_or_alias");
	}

	free(tmpbuf);
	return true;
}

static bool bif_format_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		return throw_error(q, &tmp, q->st.cur_frame, "permission_error", "output,binary_stream");
	}

	return do_format(q, NULL, 0, p1, p1_ctx, NULL, q->st.cur_frame);
}

static bool bif_format_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		return throw_error(q, &tmp, q->st.cur_frame, "permission_error", "output,binary_stream");
	}

	if (is_nil(p1)) {
		if (is_nil(p2))
			return true;
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "list");
	}

	return do_format(q, NULL, 0, p1, p1_ctx, !is_nil(p2)?p2:NULL, p2_ctx);
}

static bool bif_format_3(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,list_or_nil);

	if (is_closed_stream(q->pl, pstr))
		return throw_error(q, pstr, pstr_ctx, "existence_error", "stream");

	int n = get_stream(q, pstr);

	if ((n < 0) && !is_compound(pstr)) {
		return throw_error(q, pstr, pstr_ctx, "domain_error", "stream_or_alias");
	} else if (!is_compound(pstr)) {
		stream *str = &q->pl->streams[n];

		if (!strcmp(str->mode, "read"))
			return throw_error(q, pstr, pstr_ctx, "permission_error", "output,stream");

		if (str->binary) {
			cell tmp;
			make_int(&tmp, n);
			return throw_error(q, &tmp, pstr_ctx, "permission_error", "output,binary_stream");
		}

		if (is_nil(p1)) {
			if (is_nil(p2))
				return true;
			else
				return throw_error(q, p2, p2_ctx, "domain_error", "list");
		}
	}

	return do_format(q, pstr, pstr_ctx, p1, p1_ctx, !is_nil(p2)?p2:NULL, p2_ctx);
}

builtins g_format_bifs[] =
{
	{"format", 1, bif_format_1, "+string", false, false, BLAH},
	{"format", 2, bif_format_2, "+string,+list", false, false, BLAH},
	{"format", 3, bif_format_3, "+stream,+string,+list", false, false, BLAH},

	{0}
};
