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
#include "utf8.h"

bool do_parse_csv_line(query *q, int sep, bool trim, bool numbers, cell *p1, cell *p2, pl_idx_t p2_ctx)
{
	const char *src = C_STR(q, p1);
	bool quoted = false, was_quoted = false, first = true, was_sep = false;
	unsigned chars = 0;
	SB(pr);

	if (trim) {
		while (iswspace(*src))
			get_char_utf8(&src);
	}

	while (*src) {
		int ch = get_char_utf8(&src);

		if (trim) {
			while (!quoted && !chars && iswspace(ch))
				ch = get_char_utf8(&src);
		}

		if (!quoted && (ch == '"')) {
			was_quoted = quoted = 1;
			continue;
		}

		if (quoted && (ch == sep)) {
			SB_putchar(pr, ch);
			continue;
		}

		if (quoted && (ch == '"')) {
			quoted = 0;
			ch = get_char_utf8(&src);
		}

		was_sep = ch == sep;

		if ((ch != sep) && ch) {
			SB_putchar(pr, ch);
			chars++;

			if (*src)
				continue;
		}

		if (!ch)
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
		} else if (is_string(p1)) {
			if (SB_strlen(pr)) {
				unsigned vnbr = create_vars(q, 1);
				make_var(&tmpc, g_anon_s, vnbr);
				cell tmp;
				check_heap_error(make_stringn(&tmp, SB_cstr(pr), SB_strlen(pr)));
				unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
			} else
				make_atom(&tmpc, g_nil_s);
		} else {
			unsigned vnbr = create_vars(q, 1);
			make_var(&tmpc, g_anon_s, vnbr);
			cell tmp;
			check_heap_error(make_cstringn(&tmp, SB_cstr(pr), SB_strlen(pr)));
			unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
			unshare_cell(&tmp);
		}

		chars = 0;

		if (first) {
			allocate_list(q, &tmpc);
			first = false;
		} else
			append_list(q, &tmpc);

		quoted = was_quoted = false;
		SB_init(pr);

		if (trim) {
			while (iswspace(*src))
				get_char_utf8(&src);
		}
	}

	if (was_sep && !trim) {
		cell tmpc;

		if (is_string(p1)) {
			if (SB_strlen(pr)) {
				unsigned vnbr = create_vars(q, 1);
				make_var(&tmpc, g_anon_s, vnbr);
				cell tmp;
				check_heap_error(make_stringn(&tmp, SB_cstr(pr), SB_strlen(pr)));
				unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
			} else
				make_atom(&tmpc, g_nil_s);
		} else {
			unsigned vnbr = create_vars(q, 1);
			make_var(&tmpc, g_anon_s, vnbr);
			cell tmp;
			check_heap_error(make_cstringn(&tmp, SB_cstr(pr), SB_strlen(pr)));
			unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
			unshare_cell(&tmp);
		}

		if (first) {
			allocate_list(q, &tmpc);
			first = false;
		} else
			append_list(q, &tmpc);
	}

	SB_free(pr);
	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}
