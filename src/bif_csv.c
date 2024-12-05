#include <stdlib.h>
#include <stdio.h>

#include "module.h"
#include "query.h"

bool do_parse_csv_line(query *q, csv *params, const char *src, cell *p2, pl_idx p2_ctx)
{
	bool quoted = false, was_quoted = false, first = true, was_sep = false;
	unsigned chars = 0, args = 0;
	SB(pr);

	if (params->trim) {
		while (iswspace(*src))
			get_char_utf8(&src);
	}

	while (*src && (*src != '\r') && (*src != '\n')) {
		int ch = get_char_utf8(&src);

		if (params->trim) {
			while (!quoted && !chars && iswspace(ch))
				ch = get_char_utf8(&src);
		}

		if (!quoted && (ch == params->quote)) {
			was_quoted = quoted = 1;
			continue;
		}

		if (quoted && (ch == params->sep)) {
			SB_putchar(pr, ch);
			continue;
		}

		if (quoted && (ch == params->quote)) {
			ch = get_char_utf8(&src);
			quoted = 0;
		}

		was_sep = ch == params->sep;

		if ((ch != params->sep) && ch && (ch != '\r') && (ch != '\n')) {
			SB_putchar(pr, ch);
			chars++;

			if (*src && (*src != '\r') && (*src != '\n'))
				continue;

			if (quoted && q->p->fp) {
				ssize_t len;

				if ((len = getline(&q->p->save_line, &q->p->n_line, q->p->fp)) == -1)
					return true;

				src = q->p->save_line;
				continue;
			}
		}

		if (!ch || (ch == '\r') || (ch == '\n'))
			src--;

		if (params->trim)
			SB_trim_ws(pr);

		cell tmpc = {0};
        int dots = 0, bad = 0;
		bool num = params->numbers && !was_quoted;

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
		} else if (params->use_strings) {
			if (SB_strlen(pr)) {
				cell tmp;
				int vnbr = create_vars(q, 1);
				check_heap_error(vnbr != -1);
				make_ref(&tmp, vnbr, q->st.curr_frame);
				check_heap_error(make_stringn(&tmpc, SB_cstr(pr), SB_strlen(pr)));
				unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
				unshare_cell(&tmpc);
			} else
				make_atom(&tmpc, g_nil_s);
		} else {
			cell tmp;
			int vnbr = create_vars(q, 1);
			check_heap_error(vnbr != -1);
			make_ref(&tmp, vnbr, q->st.curr_frame);
			check_heap_error(make_cstringn(&tmpc, SB_cstr(pr), SB_strlen(pr)));
			unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
			unshare_cell(&tmpc);
		}

		chars = 0;

		if (first) {
			if (params->functor)
				allocate_structure(q, params->functor, &tmpc);
			else
				allocate_list(q, &tmpc);

			first = false;
		} else {
			if (params->functor)
				append_structure(q, &tmpc);
			else
				append_list(q, &tmpc);
		}

		args++;
		quoted = was_quoted = false;
		SB_init(pr);

		if (params->trim) {
			while (iswspace(*src))
				get_char_utf8(&src);
		}
	}

	if (was_sep && !params->trim) {
		cell tmpc;

		if (params->use_strings) {
			if (SB_strlen(pr)) {
				cell tmp;
				int vnbr = create_vars(q, 1);
				check_heap_error(vnbr != -1);
				make_ref(&tmp, vnbr, q->st.curr_frame);
				check_heap_error(make_stringn(&tmpc, SB_cstr(pr), SB_strlen(pr)));
				unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
				unshare_cell(&tmpc);
			} else
				make_atom(&tmpc, g_nil_s);
		} else {
			cell tmp;
			int vnbr = create_vars(q, 1);
			check_heap_error(vnbr != -1);
			make_ref(&tmp, vnbr, q->st.curr_frame);
			check_heap_error(make_cstringn(&tmpc, SB_cstr(pr), SB_strlen(pr)));
			unify(q, &tmpc, q->st.curr_frame, &tmp, q->st.curr_frame);
			unshare_cell(&tmpc);
		}

		if (first) {
			if (params->functor)
				allocate_structure(q, params->functor, &tmpc);
			else
				allocate_list(q, &tmpc);

			first = false;
		} else {
			if (params->functor)
				append_structure(q, &tmpc);
			else
				append_list(q, &tmpc);
		}

		args++;
	}

	SB_free(pr);

	if ((params->arity > 0) && (args != params->arity)) {
		cell tmp;
		make_int(&tmp, params->arity);
		return throw_error(q, &tmp, q->st.curr_frame, "domain_error", "row_arity");
	}

	cell *l = params->functor ? end_structure(q) : end_list(q);
	check_heap_error(l);

	if (p2)
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);

	bool found = false, evaluable = false;

	if (get_builtin_term(q->st.curr_m, l, &found, &evaluable), found && !evaluable) {
		if (!GET_OP(l))
			return throw_error(q, l, q->st.curr_frame, "permission_error", "modify,static_procedure");
	}

	if (!assertz_to_db(q->st.curr_m, 0, l, false))
		return throw_error(q, l, q->st.curr_frame, "permission_error", "modify_static_procedure");

	return true;
}

bool bif_parse_csv_line_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	csv params = {.sep=',', .quote='"', .arity=0, .trim=false, .numbers=false, .use_strings=is_string(p1), .functor=NULL};
	return do_parse_csv_line(q, &params, C_STR(q,p1), p2, p2_ctx);
}

bool bif_parse_csv_line_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,list_or_nil);
	bool trim = false, numbers = false, use_strings = is_string(p1), do_assert = false;
	const char *functor = NULL;
	int sep = ',', quote = '"';
	unsigned arity = 0;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		h = deref(q,h,p3_ctx);

		if (is_compound(h) && (h->arity == 1)) {
			cell *c = h + 1;

			if (!strcmp("trim", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				trim = true;
			else if (!strcmp("numbers", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				numbers = true;
			else if (!strcmp("strings", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				use_strings = true;
			else if (!strcmp("strings", C_STR(q, h)) && is_atom(c) && (c->val_off == g_false_s))
				use_strings = false;
			else if (!strcmp("arity", C_STR(q, h)) && is_smallint(c))
				arity = get_smallint(c);
			else if (!strcmp("functor", C_STR(q, h)) && is_atom(c))
				functor = C_STR(q, c);
			else if (!strcmp("assert", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				do_assert = true;
			else if (!strcmp("sep", C_STR(q, h)) && is_atom(c) && (C_STRLEN_UTF8(c) == 1))
				sep = peek_char_utf8(C_STR(q, c));
			else if (!strcmp("quote", C_STR(q, h)) && is_atom(c) && (C_STRLEN_UTF8(c) == 1))
				quote = peek_char_utf8(C_STR(q, c));
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q,p3,p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	csv params = {.sep=sep, .quote=quote, .arity=arity, .trim=trim, .numbers=numbers, .use_strings=use_strings, .functor=functor};
	return do_parse_csv_line(q, &params, C_STR(q,p1), !do_assert||!functor ? p2 : NULL, p2_ctx);
}

bool bif_parse_csv_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p3,list_or_nil);
	bool trim = false, numbers = false, use_strings = false;
	bool header = false, comments = false;
	const char *functor = NULL;
	int sep = ',', quote = '"', comment = '#';
	unsigned arity = 0;
	LIST_HANDLER(p3);

	const char *ext = strrchr(C_STR(q, p1), '.');

	if (ext && !strcmp(ext, ".tsv"))
		sep = '\t';

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		h = deref(q,h,p3_ctx);

		if (is_compound(h) && (h->arity == 1)) {
			cell *c = h + 1;

			if (!strcmp("trim", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				trim = true;
			else if (!strcmp("numbers", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				numbers = true;
			else if (!strcmp("comments", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				comments = true;
			else if (!strcmp("header", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				header = true;
			else if (!strcmp("strings", C_STR(q, h)) && is_atom(c) && (c->val_off == g_true_s))
				use_strings = true;
			else if (!strcmp("strings", C_STR(q, h)) && is_atom(c) && (c->val_off == g_false_s))
				use_strings = false;
			else if (!strcmp("arity", C_STR(q, h)) && is_smallint(c))
				arity = get_smallint(c);
			else if (!strcmp("functor", C_STR(q, h)) && is_atom(c))
				functor = C_STR(q, c);
			else if (!strcmp("comment", C_STR(q, h)) && is_atom(c) && (C_STRLEN_UTF8(c) == 1))
				comment = peek_char_utf8(C_STR(q, c));
			else if (!strcmp("sep", C_STR(q, h)) && is_atom(c) && (C_STRLEN_UTF8(c) == 1))
				sep = peek_char_utf8(C_STR(q, c));
			else if (!strcmp("quote", C_STR(q, h)) && is_atom(c) && (C_STRLEN_UTF8(c) == 1))
				quote = peek_char_utf8(C_STR(q, c));
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q,p3,p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	if (!functor)
		return throw_error(q, p3, p3_ctx, "domain_error", "missing_functor");

	q->p->fp = fopen(C_STR(q, p1), "r");
	if (!q->p->fp) return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	unsigned line_nbr = 0;
	pl_idx save_hp = q->st.hp;
	frame *f = GET_CURR_FRAME();
	frame save_f = *f;
	ssize_t len;
	csv params = {.sep=sep, .quote=quote, .arity=arity, .trim=trim, .numbers=numbers, .use_strings=use_strings, .functor=functor};

	while ((len = getline(&q->p->save_line, &q->p->n_line, q->p->fp)) != -1) {
		CHECK_INTERRUPT();
		char *line = q->p->save_line;
		line_nbr++;

		if (header) {
			header = false;
			continue;
		}

		if ((comments && (line[0] == comment)) || !line[0] || (line[0] == '\r') || (line[0] == '\n'))
			continue;

		if (!do_parse_csv_line(q, &params, line, NULL, 0)) {
			//fprintf(stderr, "Error: line %u\n", line_nbr);
			free(q->p->save_line);
			q->p->save_line = NULL;
			fclose(q->p->fp);
			q->p->fp = NULL;
			return false;
		}

		*f = save_f;
		q->st.hp = save_hp;
	}

	free(q->p->save_line);
	q->p->save_line = NULL;
	fclose(q->p->fp);
	q->p->fp = NULL;

	if (!q->pl->quiet)
		printf("%% Parsed %u lines\n", line_nbr);

	return true;
}

builtins g_csv_bifs[] =
{
	{"parse_csv_line", 2, bif_parse_csv_line_2, "+atom,-list", false, false, BLAH},
	{"parse_csv_line", 3, bif_parse_csv_line_3, "+atom,-compound,+list", false, false, BLAH},
	{"parse_csv_file", 2, bif_parse_csv_file_2, "+atom,+list", false, false, BLAH},

	{0}
};
