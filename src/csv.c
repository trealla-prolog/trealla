#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heap.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

bool do_parse_csv_line(query *q, int sep, int quote, bool trim, bool numbers, bool use_strings, unsigned arity, const char *functor, const char *src, cell *p2, pl_idx_t p2_ctx)
{
	bool quoted = false, was_quoted = false, first = true, was_sep = false;
	unsigned chars = 0, args = 0;
	SB(pr);

	if (trim) {
		while (iswspace(*src))
			get_char_utf8(&src);
	}

	while (*src && (*src != '\r') && (*src != '\n')) {
		int ch = get_char_utf8(&src);

		if (trim) {
			while (!quoted && !chars && iswspace(ch))
				ch = get_char_utf8(&src);
		}

		if (!quoted && (ch == quote)) {
			was_quoted = quoted = 1;
			continue;
		}

		if (quoted && (ch == sep)) {
			SB_putchar(pr, ch);
			continue;
		}

		if (quoted && (ch == quote)) {
			ch = get_char_utf8(&src);
			quoted = 0;
		}

		was_sep = ch == sep;

		if ((ch != sep) && ch && (ch != '\r') && (ch != '\n')) {
			SB_putchar(pr, ch);
			chars++;

			if (*src && (*src != '\r') && (*src != '\n'))
				continue;

			if (quoted && q->p->fp) {
				ssize_t len;

				if ((len = getline(&q->p->save_line, &q->p->n_line, q->p->fp)) == -1)
					return true;

				char *line = q->p->save_line;
				src = q->p->save_line;
				continue;
			}
		}

		if (!ch || (ch == '\r') || (ch == '\n'))
			src--;

		if (trim)
			SB_trim_ws(pr);

		cell tmpc = {0};
        int dots = 0, bad = 0;
		bool num = numbers && !was_quoted;

		if (num && !was_quoted) {
			const char *tmp_src = SB_cstr(pr);

			if (*tmp_src == '-')
				tmp_src++;

			while (*tmp_src) {
				if (*tmp_src == '.')
					dots++;
				else if (!isdigit(*tmp_src))
					bad++;

				tmp_src++;
			}

			if (was_quoted || bad || (dots > 1))
				num = false;
		}

		if (num) {
			if (dots)
				make_float(&tmpc,strtod(SB_cstr(pr), NULL));
			else
				make_int(&tmpc, strtoll(SB_cstr(pr), NULL, 10));
		} else if (use_strings) {
			if (SB_strlen(pr)) {
				cell tmp;
				unsigned vnbr = create_vars(q, 1);
				make_var(&tmp, g_anon_s, vnbr);
				check_heap_error(make_stringn(&tmpc, SB_cstr(pr), SB_strlen(pr)));
				unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
				unshare_cell(&tmpc);
			} else
				make_atom(&tmpc, g_nil_s);
		} else {
			cell tmp;
			unsigned vnbr = create_vars(q, 1);
			make_var(&tmp, g_anon_s, vnbr);
			check_heap_error(make_cstringn(&tmpc, SB_cstr(pr), SB_strlen(pr)));
			unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
			unshare_cell(&tmpc);
		}

		chars = 0;

		if (first) {
			if (functor)
				allocate_structure(q, functor, &tmpc);
			else
				allocate_list(q, &tmpc);

			first = false;
		} else {
			if (functor)
				append_structure(q, &tmpc);
			else
				append_list(q, &tmpc);
		}

		args++;
		quoted = was_quoted = false;
		SB_init(pr);

		if (trim) {
			while (iswspace(*src))
				get_char_utf8(&src);
		}
	}

	if (was_sep && !trim) {
		cell tmpc;

		if (use_strings) {
			if (SB_strlen(pr)) {
				cell tmp;
				unsigned vnbr = create_vars(q, 1);
				make_var(&tmp, g_anon_s, vnbr);
				check_heap_error(make_stringn(&tmpc, SB_cstr(pr), SB_strlen(pr)));
				unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
				unshare_cell(&tmpc);
			} else
				make_atom(&tmpc, g_nil_s);
		} else {
			cell tmp;
			unsigned vnbr = create_vars(q, 1);
			make_var(&tmp, g_anon_s, vnbr);
			check_heap_error(make_cstringn(&tmpc, SB_cstr(pr), SB_strlen(pr)));
			unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
			unshare_cell(&tmpc);
		}

		if (first) {
			if (functor)
				allocate_structure(q, functor, &tmpc);
			else
				allocate_list(q, &tmpc);

			first = false;
		} else {
			if (functor)
				append_structure(q, &tmpc);
			else
				append_list(q, &tmpc);
		}

		args++;
	}

	SB_free(pr);

	if ((arity > 0) && (args != arity))
		return throw_error(q, p2, p2_ctx, "domain_error", "row_arity");

	cell *l = functor ? end_structure(q) : end_list(q);
	check_heap_error(l);

	if (p2)
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);

	bool found = false, evaluable = false;

	if (get_builtin_term(q->st.m, l, &found, &evaluable), found && !evaluable) {
		if (!GET_OP(l))
			return throw_error(q, l, q->st.curr_frame, "permission_error", "modify,static_procedure");
	}

	if (!assertz_to_db(q->st.m, 0, 0, l, 0))
		return throw_error(q, l, q->st.curr_frame, "permission_error", "modify_static_procedure");

	return true;
}
