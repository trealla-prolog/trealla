#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>

#include "base64.h"
#include "heap.h"
#include "history.h"
#include "library.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"
#include "utf8.h"

#if USE_OPENSSL
#include "openssl/sha.h"
#endif

#ifdef __wasi__
#include "wasi.h"
#endif

#ifdef _WIN32
#include <windows.h>
#define unsetenv(p1)
#define setenv(p1,p2,p3) _putenv_s(p1,p2)
#define msleep Sleep
#define localtime_r(p1,p2) localtime(p1)
#else
static void msleep(int ms)
{
	struct timespec tv;
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

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

bool do_yield_0(query *q, int msecs)
{
	q->yielded = true;
	q->tmo_msecs = get_time_in_usec() / 1000;
	q->tmo_msecs += msecs > 0 ? msecs : 1;
	check_heap_error(push_choice(q));
	return false;
}

static void make_ref(cell *tmp, pl_idx_t ctx, pl_idx_t off, unsigned var_nbr)
{
	*tmp = (cell){0};
	tmp->tag = TAG_VAR;
	tmp->nbr_cells = 1;
	tmp->flags = FLAG_REF;
	tmp->var_nbr = var_nbr;
	tmp->var_ctx = ctx;
}

void make_var(cell *tmp, pl_idx_t off, unsigned var_nbr)
{
	*tmp = (cell){0};
	tmp->tag = TAG_VAR;
	tmp->nbr_cells = 1;
	tmp->var_nbr = var_nbr;
	tmp->val_off = off;
}

void make_var2(cell *tmp, pl_idx_t off)
{
	*tmp = (cell){0};
	tmp->tag = TAG_VAR;
	tmp->nbr_cells = 1;
	tmp->val_off = off;
}

void make_float(cell *tmp, double v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_FLOAT;
	tmp->nbr_cells = 1;
	tmp->val_float = v;
}

void make_int(cell *tmp, pl_int_t v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INTEGER;
	tmp->nbr_cells = 1;
	set_smallint(tmp, v);
}

void make_uint(cell *tmp, pl_uint_t v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INTEGER;
	tmp->nbr_cells = 1;
	set_smalluint(tmp, v);
}

void make_ptr(cell *tmp, void *v)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INTEGER;
	tmp->nbr_cells = 1;
	tmp->val_uint = (size_t)v;
}

void make_struct(cell *tmp, pl_idx_t offset, void *fn, unsigned arity, pl_idx_t extra_cells)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INTERNED;
	tmp->nbr_cells = 1 + extra_cells;

	if (fn) {
		tmp->flags |= FLAG_BUILTIN;
		tmp->fn_ptr = get_fn_ptr(fn);
		assert(tmp->fn_ptr);
	}

	tmp->arity = arity;
	tmp->val_off = offset;
}

void make_end(cell *tmp)
{
	*tmp = (cell){0};
	tmp->tag = TAG_END;
	tmp->nbr_cells = 1;
}

void make_call(query *q, cell *tmp)
{
	make_end(tmp);
	cell *c = q->st.curr_cell;
	frame *f = GET_CURR_FRAME();
	tmp->val_ret = c ? c + c->nbr_cells : NULL;	// save the return instruction
	tmp->cgen = f->cgen;						// ... choice-generation
	tmp->mid = q->st.m->id;						// ... current-module
}

void make_call_return(query *q, cell *tmp, cell *c_ret)
{
	make_end(tmp);
	frame *f = GET_CURR_FRAME();
	tmp->val_ret = q->st.curr_cell;				// save the return instruction
	tmp->cgen = f->cgen;						// ... choice-generation
	tmp->mid = q->st.m->id;						// ... current-module
}

void make_atom(cell *tmp, pl_idx_t offset)
{
	*tmp = (cell){0};
	tmp->tag = TAG_INTERNED;
	tmp->nbr_cells = 1;
	tmp->val_off = offset;
}

void make_smalln(cell *tmp, const char *s, size_t n)
{
	*tmp = (cell){0};
	tmp->tag = TAG_CSTR;
	tmp->nbr_cells = 1;
	memcpy(tmp->val_chr, s, n);
	tmp->val_chr[n] = '\0';
	tmp->chr_len = n;
}

#if 0
static void init_queue(query *q)
{
	free(q->queue[0]);
	q->queue[0] = NULL;
	q->qp[0] = 0;
}
#endif

static pl_idx_t queue_used(const query *q) { return q->qp[0]; }
static cell *get_queue(query *q) { return q->queue[0]; }

static cell *pop_queue(query *q)
{
	if (!q->qp[0])
		return NULL;

	cell *c = q->queue[0] + q->popp;
	q->popp += c->nbr_cells;

	if (q->popp == q->qp[0])
		q->popp = q->qp[0] = 0;

	return c;
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
	d->nbr_cells = 1;
	SET_STR(d, s, n, 0);
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
	d->nbr_cells = 1;
	d->arity = 2;
	SET_STR(d, s, n, 0);
	return true;
}

bool make_slice(query *q, cell *d, const cell *orig, size_t off, size_t n)
{
	if (!n) {
		make_atom(d, g_empty_s);
		return true;
	}

	if (is_static(orig)) {
		*d = *orig;
		d->val_str += off;
		d->str_len = n;
		return true;
	}

	if (n < MAX_SMALL_STRING) {
		const char *s = C_STR(q, orig);

		if (is_string(orig))
			return make_stringn(d, s+off, n);

		return make_cstringn(d, s+off, n);
	}

	if (is_strbuf(orig)) {
		*d = *orig;
		d->strb_off += off;
		d->strb_len = n;
		share_cell(orig);
		return true;
	}

	const char *s = C_STR(q, orig);

	if (is_string(orig))
		return make_stringn(d, s+off, n);

	return make_cstringn(d, s+off, n);
}

static bool fn_iso_unify_with_occurs_check_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	bool save = q->flags.occurs_check;
	q->flags.occurs_check = OCCURS_CHECK_TRUE;
	bool ok = unify(q, p1, p1_ctx, p2, p2_ctx);
	q->flags.occurs_check = save;
	return ok ? true : false;
}

bool fn_iso_unify_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	bool ok = unify(q, p1, p1_ctx, p2, p2_ctx);

	if (q->cycle_error)
		return throw_error(q, p2, p2_ctx, "representation_error", "term");

	return ok ? true : false;
}

static bool fn_iso_notunify_2(query *q)
{
	if (q->retry)
		return true;

	GET_FIRST_RAW_ARG(p1,any);
	GET_NEXT_RAW_ARG(p2,any);
	cell tmp2;
	make_struct(&tmp2, g_unify_s, fn_iso_unify_2, 2, 0);
	cell *tmp = clone_to_heap(q, true, &tmp2, p1->nbr_cells+p2->nbr_cells+3);
	pl_idx_t nbr_cells = 1;
	tmp[nbr_cells].nbr_cells += p1->nbr_cells+p2->nbr_cells;
	nbr_cells++;
	safe_copy_cells(tmp+nbr_cells, p1, p1->nbr_cells);
	nbr_cells += p1->nbr_cells;
	safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	nbr_cells += p2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
	make_struct(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}


static bool fn_iso_dcgs_2(query *q)
{
	return throw_error(q, q->st.curr_cell, q->st.curr_frame, "existence_error", "procedure");
}

static bool fn_iso_repeat_0(query *q)
{
	check_heap_error(push_choice(q));
	return true;
}

static bool fn_iso_halt_0(query *q)
{
	q->halt_code = 0;
	q->halt = q->error = true;
	return false;
}

static bool fn_iso_halt_1(query *q)
{
	GET_FIRST_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	q->halt_code = get_smallint(p1);
	q->halt = q->error = true;
	return false;
}

static bool fn_iso_number_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_number(p1);
}

static bool fn_iso_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_iso_atom(p1);
}

static bool fn_iso_compound_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_compound(p1) ? 1 : 0;
}

static bool fn_iso_atomic_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_atomic(p1);
}

static bool fn_iso_var_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_var(p1);
}

static bool fn_iso_nonvar_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return !is_var(p1);
}

static bool fn_iso_ground_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return !has_vars(q, p1, p1_ctx);
}

static bool fn_iso_callable_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_callable(p1);
}

static bool fn_iso_char_code_2(query *q)
{
	GET_FIRST_ARG(p1,character_or_var);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (is_var(p2)) {
		const char *src = C_STR(q, p1);
		size_t len = len_char_utf8(src);

		if (len != C_STRLEN(q, p1))
			return throw_error(q, p1, p1_ctx, "type_error", "character");

		int ch = peek_char_utf8(src);
		cell tmp;
		make_int(&tmp, ch);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "representation_error", "character_code");

	if (is_integer(p2) && (get_smallint(p2) > MAX_CODEPOINT))
		return throw_error(q, p2, p2_ctx, "representation_error", "character_code");

	if (is_var(p1)) {
		char tmpbuf[256];
		int n = put_char_utf8(tmpbuf, get_smallint(p2));
		cell tmp;
		make_smalln(&tmp, tmpbuf, n);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	const char *src = C_STR(q, p1);
	size_t len = len_char_utf8(src);

	if (len != C_STRLEN(q, p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	int ch = peek_char_utf8(src);
	return ch == get_smallint(p2);
}

static bool fn_iso_atom_chars_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom_or_var);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_atom(p1) && !C_STRLEN(q, p1) && is_nil(p2))
		return true;

	if (is_var(p1) && is_nil(p2)) {
		cell tmp;
		make_atom(&tmp, g_empty_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (is_var(p2) && !C_STRLEN(q, p1)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (is_var(p2)) {
		cell tmp;
		check_heap_error(make_stringn(&tmp, C_STR(q, p1), C_STRLEN(q, p1)));
		bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (is_string(p2)) {
		cell tmp;
		check_heap_error(make_slice(q, &tmp, p2, 0, C_STRLEN(q, p2)));
		tmp.flags &= ~FLAG_CSTR_STRING;
		tmp.arity = 0;
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	// Verify the list

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_atom(head) && is_var(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			if (!is_atom(head) && !is_var(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			if (is_atom(head)) {
				const char *src = C_STR(q, head);
				size_t len = len_char_utf8(src);

				if (len < C_STRLEN(q, head))
					return throw_error(q, head, q->latest_ctx, "type_error", "character");
			}

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_var(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (is_string(p2) && is_var(p1)) {
		cell tmp = *p2;
		tmp.flags &= ~FLAG_CSTR_STRING;
		tmp.arity = 0;
		bool ok = unify(q, p1, p1_ctx, p2, q->st.curr_frame);
		return ok;
	}

	if (!is_var(p2) && is_var(p1)) {
		SB_alloc(pr,256);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			const char *src = C_STR(q, head);
			SB_strcatn(pr, src, len_char_utf8(src));

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		cell tmp;
		check_heap_error(make_cstring(&tmp, SB_cstr(pr)), SB_free(pr));
		SB_free(pr);
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	const char *src = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);
	bool first = true;

	while (len) {
		size_t n = len_char_utf8(src);
		cell tmp2;
		make_smalln(&tmp2, src, n);
		src += n;
		len -= n;

		if (first) {
			allocate_list(q, &tmp2);
			first = false;
		} else
			append_list(q, &tmp2);
	}

	cell *tmp = end_list(q);
	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool fn_iso_number_chars_2(query *q)
{
	GET_FIRST_ARG(p1,number_or_var);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	cell *orig_p2 = p2;

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p2))
		return throw_error(q, p2, p2_ctx, "syntax_error", "incomplete");

	// Verify the list

	pl_int_t cnt = 0;
	bool any_vars = false;

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (is_var(head))
				any_vars = true;

			if (!is_atom(head) && is_var(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			if (!is_atom(head) && !is_var(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			if (is_atom(head)) {
				const char *src = C_STR(q, head);
				size_t len = len_char_utf8(src);

				if (len < C_STRLEN(q, head))
					return throw_error(q, head, q->latest_ctx, "type_error", "character");
			}

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
			cnt++;
		}

		if (!is_nil(p2) && !is_var(p2))
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");

		if (is_var(p2))
			any_vars = true;

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (is_var(p1) && any_vars)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (!is_var(p2) && !any_vars) {
		char *tmpbuf = malloc(cnt+1+1);
		check_heap_error(tmpbuf);
		char *dst = tmpbuf;
		*dst = '\0';
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_atom(head)) {
				free(tmpbuf);
				return throw_error(q, head, q->latest_ctx, "type_error", "atom");
			}

			const char *src = C_STR(q, head);
			int ch = *src;

			if (!ch)
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			*dst++ = ch;
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2)) {
			free(tmpbuf);
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");
		}

		*dst = '\0';

		int n = q->pl->current_input;
		stream *str = &q->pl->streams[n];

		if (!str->p)
			str->p = create_parser(q->st.m);

		parser *p = str->p;
		reset(p);
		p->error = false;
		p->flags = q->st.m->flags;
		p->srcptr = tmpbuf;
		p->do_read_term = true;
		bool ok = get_token(p, true, false);
		p->do_read_term = false;

		if (q->did_throw) {
			p->srcptr = NULL;
			free(tmpbuf);
			return ok;
		}

		if (!is_number(&p->v) || *p->srcptr) {
			p->srcptr = NULL;
			free(tmpbuf);
			return throw_error(q, orig_p2, p2_ctx, "syntax_error", p->error&&p->error_desc?p->error_desc:"number");
		}

		p->srcptr = NULL;
		free(tmpbuf);
		cell tmp = p->v;
		bool ok2 = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok2;
	}

	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing_was_symbol = false;
	q->did_quote = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, p1, p1_ctx, 1, 0, 0);
	char *dst = malloc(len+10);
	check_heap_error(dst);
	print_term_to_buf(q, dst, len+1, p1, p1_ctx, 1, 0, 0);
	q->ignore_ops = false;
	q->quoted = 0;
	cell tmp;
	check_heap_error(make_string(&tmp, dst));
	free(dst);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_iso_atom_codes_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_var(p2) && is_nil(p2)) {
		cell tmp;
		make_atom(&tmp, g_empty_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (is_var(p2) && !C_STRLEN(q, p1)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	// Verify the list

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head) && is_var(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			if (!is_integer(head) && !is_var(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_var(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_var(p2) && is_var(p1)) {
		SB_alloc(pr,256);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			pl_int_t val = get_smallint(head);

			if (val < 0)
				return throw_error(q, head, q->latest_ctx, "representation_error", "character_code");

			char ch[10];
			int len;

			if (!val) {
				ch[0] = 0;
				len = 1;
			} else
				len = put_char_utf8(ch, val);

			SB_strcatn(pr, ch, len);
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

		}

		if (!is_nil(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		cell tmp;
		check_heap_error(make_cstringn(&tmp, SB_cstr(pr), SB_strlen(pr)), SB_free(pr));
		SB_free(pr);
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	const char *tmpbuf = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);
	const char *src = tmpbuf;
	cell tmp;
	len -= len_char_utf8(src);
	make_int(&tmp, get_char_utf8(&src));
	allocate_list(q, &tmp);

	while (len) {
		len -= len_char_utf8(src);
		make_int(&tmp, get_char_utf8(&src));
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool fn_string_codes_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_atom(p1) && !is_var(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (!is_var(p2) && is_nil(p2)) {
		cell tmp;
		make_atom(&tmp, g_empty_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (is_var(p2) && !C_STRLEN(q, p1)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	// Verify the list

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head) && is_var(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			if (!is_integer(head) && !is_var(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_var(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_var(p2) && is_var(p1)) {
		SB_alloc(pr,256);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			pl_int_t val = get_smallint(head);

			if (val < 0)
				return throw_error(q, head, q->latest_ctx, "representation_error", "character_code");

			char ch[10];
			int len;

			if (!val) {
				ch[0] = 0;
				len = 1;
			} else
				len = put_char_utf8(ch, val);

			SB_strcatn(pr, ch, len);
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

		}

		if (!is_nil(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		cell tmp;
		check_heap_error(make_cstringn(&tmp, SB_cstr(pr), SB_strlen(pr)), SB_free(pr));
		SB_free(pr);
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	const char *tmpbuf = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);
	const char *src = tmpbuf;
	cell tmp;
	len -= len_char_utf8(src);
	make_int(&tmp, get_char_utf8(&src));
	allocate_list(q, &tmp);

	while (len) {
		len -= len_char_utf8(src);
		make_int(&tmp, get_char_utf8(&src));
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool fn_hex_bytes_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p2)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	// Verify the list

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head) && is_var(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			if (!is_integer(head) && !is_var(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2) && !is_var(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (!is_var(p2) && is_var(p1)) {
		SB_alloc(pr,256);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			pl_int_t val = get_smallint(head);

			if ((val < 0) || (val > 255))
				return throw_error(q, head, q->latest_ctx, "representation_error", "byte");

			char ch[10];
			snprintf(ch, sizeof(ch), "%02X", (unsigned)val);
			SB_strcat(pr, ch);
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;

		}

		if (!is_nil(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "list");

		cell tmp;
		check_heap_error(make_string(&tmp, SB_cstr(pr)), SB_free(pr));
		SB_free(pr);
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	LIST_HANDLER(p1);
	bool first = true;

	while (is_list(p1)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (!is_atom(h))
			return throw_error(q, p1, p1_ctx, "type_error", "char");

		const char *src = C_STR(q, h);
		int n = peek_char_utf8(src);;
		unsigned val = 0;

		if (isdigit(n))
			val += n - '0';
		else if ((n >= 'a') && (n <= 'f'))
			val += (n - 'a') + 10;
		else if ((n >= 'A') && (n <= 'F'))
			val += (n - 'A') + 10;
		else
			return throw_error(q, p1, p1_ctx, "representation_error", "byte");

		val <<= 4;

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;

		if (!is_list(p1))
			return throw_error(q, p1, p1_ctx, "domain_error", "hex_encoding");

		h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (!is_atom(h))
			return throw_error(q, p1, p1_ctx, "type_error", "char");

		src = C_STR(q, h);
		n = peek_char_utf8(src);;

		if (isdigit(n))
			val += n - '0';
		else if ((n >= 'a') && (n <= 'f'))
			val += (n - 'a') + 10;
		else if ((n >= 'A') && (n <= 'F'))
			val += (n - 'A') + 10;
		else
			return throw_error(q, p1, p1_ctx, "representation_error", "byte");

		cell tmp;
		make_int(&tmp, (int)val);

		if (first) {
			allocate_list(q, &tmp);
			first = false;
		} else
			append_list(q, &tmp);

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	if (!is_nil(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "hex_encoding");

	if (first) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool fn_iso_number_codes_2(query *q)
{
	GET_FIRST_ARG(p1,number_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);
	cell *orig_p2 = p2;

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p2))
		return throw_error(q, p2, p2_ctx, "syntax_error", "incomplete");

	// Verify the list

	int cnt = 0;
	bool any_vars = false;

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx_t save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (is_var(head))
				any_vars = true;

			if (!cnt && !is_integer(head) && is_var(p1))
				return throw_error(q, head, q->latest_ctx, "syntax_error", "integer");

			if (!is_integer(head) && is_var(p1))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			if (!is_integer(head) && !is_var(head))
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
			cnt++;
		}

		if (!is_nil(p2) && !is_var(p2))
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");

		if (is_var(p2))
			any_vars = true;

		p2 = save_p2;
		p2_ctx = save_p2_ctx;
	}

	if (is_var(p1) && any_vars)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (!is_var(p2) && !any_vars) {
		char *tmpbuf = malloc((cnt*6)+1+1);
		check_heap_error(tmpbuf);
		char *dst = tmpbuf;
		*dst = '\0';
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head)) {
				free(tmpbuf);
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");
			}

			int val = get_smallint(head);

			if (val < 0) {
				free(tmpbuf);
				return throw_error(q, head, q->latest_ctx, "representation_error", "character_code");
			}

			dst += put_char_utf8(dst, val);

			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2)) {
			free(tmpbuf);
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");
		}

		*dst = '\0';

		int n = q->pl->current_input;
		stream *str = &q->pl->streams[n];

		if (!str->p)
			str->p = create_parser(q->st.m);

		parser *p = str->p;
		reset(p);
		p->error = false;
		p->flags = q->st.m->flags;
		p->srcptr = tmpbuf;
		p->do_read_term = true;
		bool ok = get_token(p, true, false);
		p->do_read_term = false;

		if (q->did_throw) {
			p->srcptr = NULL;
			free(tmpbuf);
			return ok;
		}

		if (!is_number(&p->v) || *p->srcptr) {
			p->srcptr = NULL;
			free(tmpbuf);
			return throw_error(q, orig_p2, p2_ctx, "syntax_error", p->error?p->error_desc:"number");
		}

		p->srcptr = NULL;
		free(tmpbuf);
		cell tmp = p->v;
		bool ok2 = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok2;
	}

	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing_was_symbol = false;
	q->did_quote = false;
	ssize_t len = print_term_to_buf(q, NULL, 0, p1, p1_ctx, 1, 0, 0);
	char *dst = malloc(len+10);
	check_heap_error(dst);
	print_term_to_buf(q, dst, len+1, p1, p1_ctx, 1, 0, 0);
	q->ignore_ops = false;
	q->quoted = 0;
	const char *src = dst;
	cell tmp;
	make_int(&tmp, *src);
	allocate_list(q, &tmp);

	while (*++src) {
		make_int(&tmp, *src);
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	free(dst);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool fn_iso_sub_atom_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer_or_var);		// before
	GET_NEXT_ARG(p3,integer_or_var);		// len
	GET_NEXT_ARG(p4,integer_or_var);		// after
	GET_NEXT_ARG(p5,atom_or_var);
	const size_t len_p1 = C_STRLEN_UTF8(p1);
	size_t before = 0, len = 0, after = 0;

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	if (is_integer(p3) && is_negative(p3))
		return throw_error(q, p3, p3_ctx, "domain_error", "not_less_than_zero");

	if (is_integer(p4) && is_negative(p4))
		return throw_error(q, p4, p4_ctx, "domain_error", "not_less_than_zero");

	bool fixed = ((is_integer(p2) ? 1: 0) + (is_integer(p3) ? 1 : 0) + (is_integer(p4) ? 1 : 0)) >= 2;

	if ((!is_var(p2) || !is_var(p4)) && !is_var(p5))
		fixed = true;

	if (!q->retry) {
		check_heap_error(push_choice(q));

		if (!is_var(p2))
			before = get_smallint(p2);

		if (!is_var(p3))
			len = get_smallint(p3);

		if (!is_var(p4))
			after = get_smallint(p4);

		if (is_var(p2) && is_integer(p3) && is_integer(p4))
			before = len_p1 - after - len;

		if (is_var(p3) && is_integer(p2) && is_integer(p4))
			len = len_p1 - before - after;
	} else {
		before = q->st.v1;
		len = q->st.v2;
	}

	if (len > (len_p1 - before)) {
		before++;
		len = 0;
	}

	if (before > len_p1) {
		drop_choice(q);
		return false;
	}

	for (size_t i = before; i <= len_p1; i++) {
		for (size_t j = len; j <= (len_p1 - i); j++) {
			CHECK_INTERRUPT();
			q->st.v1 = i;
			q->st.v2 = j + 1;
			check_heap_error(push_choice(q));
			cell tmp;
			size_t before = i;
			make_int(&tmp, before);

			if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame)) {
				retry_choice(q);
				continue;
			}

			size_t len = j;
			make_int(&tmp, len);

			if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame)) {
				retry_choice(q);
				continue;
			}

			size_t after = (len_p1 - before) - len;
			make_int(&tmp, after);

			if (!unify(q, p4, p4_ctx, &tmp, q->st.curr_frame)) {
				retry_choice(q);
				continue;
			}

			size_t ipos = offset_at_pos(C_STR(q, p1), C_STRLEN(q, p1), i);
			size_t jpos = offset_at_pos(C_STR(q, p1), C_STRLEN(q, p1), i + j);

			check_heap_error(make_slice(q, &tmp, p1, ipos, jpos - ipos));

			if (is_atom(p5) && !CMP_STR_TO_CSTRN(q, p5, C_STR(q, &tmp), C_STRLEN(q, &tmp))) {
				unshare_cell(&tmp);

				if (fixed) {
					drop_choice(q);
					drop_choice(q);
				}

				return true;
			}

			if (!unify(q, p5, p5_ctx, &tmp, q->st.curr_frame)) {
				unshare_cell(&tmp);
				retry_choice(q);
				continue;
			}

			unshare_cell(&tmp);

			if (fixed) {
				drop_choice(q);
				drop_choice(q);
			}

			return true;
		}

		len = 0;
	}

	drop_choice(q);
	return false;
}

// NOTE: this just handles the mode(-,-,+) case...

static bool do_atom_concat_3(query *q)
{
	if (!q->retry) {
		GET_FIRST_ARG(p1,var);
		GET_NEXT_ARG(p2,var);
		GET_NEXT_ARG(p3,atom);
		cell tmp;
		make_atom(&tmp, g_empty_s);
		unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unify(q, p2, p2_ctx, p3, q->st.curr_frame);

		if (C_STRLEN(q, p3))
			check_heap_error(push_choice(q));

		return true;
	}

	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	const char *s2 = C_STR(q, p2);
	size_t len = len_char_utf8(s2);
	size_t len1 = C_STRLEN(q, p1);
	size_t len2 = C_STRLEN(q, p2);
	bool done = false;

	if (!*(s2+len))
		done = true;

	GET_RAW_ARG(1,p1_raw);
	GET_RAW_ARG(2,p2_raw);
	unshare_cell(p1);
	unshare_cell(p2);
	cell tmp;
	check_heap_error(make_slice(q, &tmp, p3, 0, len1+len));
	reset_var(q, p1_raw, p1_raw_ctx, &tmp, q->st.curr_frame, true);
	unshare_cell(&tmp);
	check_heap_error(make_slice(q, &tmp, p2, len, len2-len));
	reset_var(q, p2_raw, p2_raw_ctx, &tmp, q->st.curr_frame, true);
	unshare_cell(&tmp);

	if (!done)
		check_heap_error(push_choice(q));

	return true;
}

static bool fn_iso_atom_concat_3(query *q)
{
	if (q->retry)
		return do_atom_concat_3(q);

	GET_FIRST_ARG(p1,iso_atom_or_var);
	GET_NEXT_ARG(p2,iso_atom_or_var);
	GET_NEXT_ARG(p3,iso_atom_or_var);

	if (is_var(p1) && is_var(p2))
		return do_atom_concat_3(q);

	if (is_var(p3)) {
		if (!is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		if (!is_atom(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "atom");

		SB_alloc(pr,256);
		SB_strcatn(pr, C_STR(q, p1), C_STRLEN(q, p1));
		SB_strcatn(pr, C_STR(q, p2), C_STRLEN(q, p2));
		cell tmp;
		check_heap_error(make_cstring(&tmp, SB_cstr(pr)), SB_free(pr));
		SB_free(pr);
		bool ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (is_var(p1)) {
		size_t len2 = C_STRLEN(q, p2), len3 = C_STRLEN(q, p3);
		const char *s2 = C_STR(q, p2), *s3 = C_STR(q, p3);

		if (len2 > len3)
			return false;

		if (memcmp(s3+(len3-len2), s2, len2))
			return false;

		cell tmp;
		check_heap_error(make_slice(q, &tmp, p3, 0, len3-len2));
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (is_var(p2)) {
		size_t len1 = C_STRLEN(q, p1), len3 = C_STRLEN(q, p3);
		const char *s1 = C_STR(q, p1), *s3 = C_STR(q, p3);

		if (len1 > len3)
			return false;

		if (memcmp(s3, s1, len1))
			return false;

		cell tmp;
		check_heap_error(make_slice(q, &tmp, p3, len1, len3-len1));
		bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	size_t len1 = C_STRLEN(q, p1), len2 = C_STRLEN(q, p2), len3 = C_STRLEN(q, p3);
	const char *s1 = C_STR(q, p1), *s2 = C_STR(q, p2), *s3 = C_STR(q, p3);

	if ((len1 + len2) != len3)
		return false;

	if (memcmp(s3, s1, len1))
		return false;

	if (memcmp(s3+len1, s2, len2))
		return false;

	return true;
}

static bool fn_iso_atom_length_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom);
	GET_NEXT_ARG(p2,smallint_or_var);

	if (is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	size_t len = substrlen_utf8(C_STR(q, p1), C_STRLEN(q, p1));
	cell tmp;
	make_int(&tmp, len);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static void compare_and_zero(uint64_t v1, uint64_t *v2, uint64_t *v)
{
	if (v1 != *v2) {
		*v2 = v1;
		*v = 0;
	}
}

#define MASK_FINAL 0x0000FFFFFFFFFFFF // Final 48 bits

void uuid_gen(prolog *pl, uuid *u)
{
	if (!pl->seed)
		pl->seed = (uint64_t)time(0) & MASK_FINAL;

	uint64_t now = get_time_in_usec();
	compare_and_zero(now, &pl->s_last, &pl->s_cnt);
	u->u1 = now;
	u->u2 = pl->s_cnt++;
	u->u2 <<= 48;
	u->u2 |= pl->seed;
}

static char *uuid_to_buf(const uuid *u, char *buf, size_t buflen)
{
	snprintf(buf, buflen, "%016"PRIx64"-%04"PRIx64"-%012"PRIx64"",
		 u->u1,
		 (u->u2 >> 48),
		 (u->u2 & MASK_FINAL));

	return buf;
}

static int uuid_from_buf(const char *s, uuid *u)
{
	if (!s) {
		uuid tmp = {0};
		*u = tmp;
		return 0;
	}

	uint64_t p1 = 0, p2 = 0, p3 = 0;

	if (sscanf(s, "%"PRIx64"%*c%"PRIx64"%*c%"PRIx64"", &p1, &p2, &p3) != 3) {
		uuid tmp = {0};
		*u = tmp;
		return 0;
	}

	u->u1 = p1;
	u->u2 = p2 << 48;
	u->u2 |= p3 & MASK_FINAL;
	return 1;
}

enum log_type { LOG_ASSERTA=1, LOG_ASSERTZ=2, LOG_ERASE=3 };

static void db_log(query *q, db_entry *dbe, enum log_type l)
{
	char tmpbuf[256];
	char *dst;
	q->quoted = 2;

	switch(l) {
	case LOG_ASSERTA:
		dst = print_term_to_strbuf(q, dbe->cl.cells, q->st.curr_frame, 1);
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$a_'(%s,'%s').\n", dst, tmpbuf);
		free(dst);
		break;
	case LOG_ASSERTZ:
		dst = print_term_to_strbuf(q, dbe->cl.cells, q->st.curr_frame, 1);
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$z_'(%s,'%s').\n", dst, tmpbuf);
		free(dst);
		break;
	case LOG_ERASE:
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		fprintf(q->st.m->fp, "'$e_'('%s').\n", tmpbuf);
		break;
	}

	q->quoted = 0;
}

static bool fn_iso_arg_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,compound);
	GET_NEXT_ARG(p3,any);

	if (is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	if (is_bigint(p1))
		return false;

	pl_int_t arg_nbr = get_smallint(p1);

	if ((arg_nbr == 0) || (arg_nbr > p2->arity))
		return false;

	if (is_list(p2)) {
		LIST_HANDLER(p2);

		cell *c = LIST_HEAD(p2);
		c = deref(q, c, p2_ctx);
		pl_idx_t c_ctx = q->latest_ctx;

		if (arg_nbr == 1)
			return unify(q, c, c_ctx, p3, p3_ctx);

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
		return unify(q, p2, p2_ctx, p3, p3_ctx);
	}

	p2 = p2 + 1;

	for (int i = 1; i <= arg_nbr; i++) {
		if (i == arg_nbr) {
			cell *c = deref(q, p2, p2_ctx);
			pl_idx_t c_ctx = q->latest_ctx;
			return unify(q, p3, p3_ctx, c, c_ctx);
		}

		p2 += p2->nbr_cells;
	}

	return true;
}

static bool fn_iso_univ_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (is_var(p1) && is_nil(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "non_empty_list");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_string(p1)) {
		cell tmp;
		make_atom(&tmp, g_dot_s);
		allocate_list(q, &tmp);
		LIST_HANDLER(p1);
		cell *h = LIST_HEAD(p1);
		append_list(q, h);
		cell *t = LIST_TAIL(p1);
		append_list(q, t);
		cell *l = end_list(q);
		check_heap_error(l);
		return unify(q, p2, p2_ctx, l, p1_ctx);
	}

	if (is_var(p2)) {
		cell tmp2 = *p1;
		tmp2.nbr_cells = 1;
		tmp2.arity = 0;
		CLR_OP(&tmp2);
		allocate_list(q, &tmp2);
		unsigned arity = p1->arity;
		p1++;

		while (arity--) {
			append_list(q, p1);
			p1 += p1->nbr_cells;
		}

		cell *l = end_list(q);
		check_heap_error(l);
		return unify(q, p2, p2_ctx, l, p1_ctx);
	}

	if (is_var(p1)) {
		cell *p22 = p2 + 1;
		p22 = deref(q, p22, p2_ctx);

		if (is_var(p22))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "not_sufficiently_instantiated");

		pl_idx_t save_hp = q->st.hp;
		cell *tmp = deep_clone_to_heap(q, p2, p2_ctx);
		check_heap_error(tmp);
		p2 = tmp;
		p2_ctx = q->st.curr_frame;
		unsigned arity = 0;
		check_heap_error(init_tmp_heap(q));
		cell *save_p2 = p2;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			CHECK_INTERRUPT();
			cell *h = LIST_HEAD(p2);
			cell *tmp2 = alloc_on_tmp(q, h->nbr_cells);
			check_heap_error(tmp2);
			copy_cells(tmp2, h, h->nbr_cells);
			p2 = LIST_TAIL(p2);
			arity++;
		}

		if (is_var(p2))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "list");

		if (!is_nil(p2))
			return throw_error(q, save_p2, p2_ctx, "type_error", "list");

		arity--;
		cell *tmp2 = get_tmp_heap(q, 0);
		pl_idx_t nbr_cells = tmp_heap_used(q);

		if (is_cstring(tmp2) && !is_string(tmp2)) {
			share_cell(tmp2);
			convert_to_literal(q->st.m, tmp2);
		}

		if (!is_interned(tmp2) && arity)
			return throw_error(q, tmp2, q->st.curr_frame, "type_error", "atom");

		if (tmp2->arity && arity)
			return throw_error(q, tmp2, q->st.curr_frame, "type_error", "atom");

		if (tmp2->arity)
			return throw_error(q, tmp2, q->st.curr_frame, "type_error", "atomic");

		if (arity > MAX_ARITY)
			return throw_error(q, tmp2, q->st.curr_frame, "representation_error", "max_arity");

		q->st.hp = save_hp;
		check_heap_error(tmp = alloc_on_heap(q, nbr_cells));
		safe_copy_cells(tmp, tmp2, nbr_cells);
		tmp->nbr_cells = nbr_cells;
		tmp->arity = arity;
		bool found = false;

		if (is_callable(tmp)) {
			if ((tmp->match = search_predicate(q->st.m, tmp, NULL)) != NULL) {
				tmp->flags &= ~FLAG_BUILTIN;
			} else if ((tmp->fn_ptr = get_builtin(q->pl, C_STR(q, tmp), tmp->arity, &found, NULL)), found) {
				if (tmp->fn_ptr->evaluable)
					tmp->flags |= FLAG_EVALUABLE;
				else
					tmp->flags |= FLAG_BUILTIN;
			}
		}

		unsigned specifier;

		if (search_op(q->st.m, C_STR(q, tmp), &specifier, arity == 1)) {
			if ((arity == 2) && IS_INFIX(specifier))
				SET_OP(tmp, specifier);
			else if ((arity == 1) && IS_POSTFIX(specifier))
				SET_OP(tmp, specifier);
			else if ((arity == 1) && IS_PREFIX(specifier))
				SET_OP(tmp, specifier);
		}

		return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	}

	cell tmp = *p1;
	tmp.nbr_cells = 1;
	tmp.arity = 0;
	CLR_OP(&tmp);
	allocate_list(q, &tmp);
	unsigned arity = p1->arity;
	p1++;

	while (arity--) {
		append_list(q, p1);
		p1 += p1->nbr_cells;
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, p1_ctx);
}

cell *do_term_variables(query *q, cell *p1, pl_idx_t p1_ctx)
{
	frame *f = GET_CURR_FRAME();
	q->varno = f->actual_slots;
	collect_vars(q, p1, p1_ctx);
	const unsigned cnt = q->tab_idx;
	if (!init_tmp_heap(q)) return NULL;
	cell *tmp = alloc_on_tmp(q, (cnt*2)+1);
	if (!tmp) return NULL;

	if (cnt) {
		unsigned idx = 0;

		for (unsigned i = 0, done = 0; i < cnt; i++) {
			make_atom(tmp+idx, g_dot_s);
			tmp[idx].arity = 2;
			tmp[idx].nbr_cells = ((cnt-done)*2)+1;
			idx++;
			make_ref(tmp+idx, q->pl->tabs[i].ctx, q->pl->tabs[i].val_off, q->pl->tabs[i].var_nbr);

			if (q->pl->tabs[i].is_anon)
				tmp[idx].flags |= FLAG_VAR_ANON;

			idx++;
			done++;
		}

		make_atom(tmp+idx++, g_nil_s);
		tmp[0].arity = 2;
		tmp[0].nbr_cells = idx;
	} else
		make_atom(tmp, g_nil_s);

	return tmp;
}

static bool fn_iso_term_variables_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_var(p1) && (is_atom(p1) || is_number(p1))) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	cell *tmp = do_term_variables(q, p1, p1_ctx);
	check_heap_error(tmp);
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	check_heap_error(tmp2);
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return unify(q, p2, p2_ctx, tmp2, q->st.curr_frame);
}

static cell *do_term_singletons(query *q, cell *p1, pl_idx_t p1_ctx)
{
	frame *f = GET_CURR_FRAME();
	q->varno = f->actual_slots;
	collect_vars(q, p1, p1_ctx);
	const unsigned cnt = q->tab_idx;
	unsigned cnt2 = 0;

	if (cnt) {
		for (unsigned i = 0; i < cnt; i++) {
			if (q->pl->tabs[i].cnt != 1)
				continue;

			cnt2++;
		}
	}

	if (!init_tmp_heap(q)) return NULL;
	cell *tmp = alloc_on_tmp(q, (cnt2*2)+1);
	if (!tmp) return NULL;

	if (cnt2) {
		unsigned idx = 0;

		for (unsigned i = 0, done = 0; i < cnt; i++) {
			if (q->pl->tabs[i].cnt != 1)
				continue;

			make_atom(tmp+idx, g_dot_s);
			tmp[idx].arity = 2;
			tmp[idx].nbr_cells = ((cnt2-done)*2)+1;
			idx++;
			make_ref(tmp+idx, q->pl->tabs[i].ctx, q->pl->tabs[i].val_off, q->pl->tabs[i].var_nbr);

			if (q->pl->tabs[i].is_anon)
				tmp[idx].flags |= FLAG_VAR_ANON;

			idx++;
			done++;
		}

		make_atom(tmp+idx++, g_nil_s);
		tmp[0].arity = 2;
		tmp[0].nbr_cells = idx;
	} else
		make_atom(tmp, g_nil_s);

	return tmp;		// returns on tmp_heap
}

static bool fn_term_singletons_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_var(p1) && (is_atom(p1) || is_number(p1))) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	cell *tmp = do_term_singletons(q, p1, p1_ctx);
	check_heap_error(tmp);
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	check_heap_error(tmp2);
	safe_copy_cells(tmp2, tmp, tmp->nbr_cells);
	return unify(q, p2, p2_ctx, tmp2, q->st.curr_frame);
}

static bool fn_iso_copy_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_var(p1) && is_var(p2)) {
		frame *f1 = GET_FRAME(p1_ctx);
		slot *e1 = GET_SLOT(f1, p1->var_nbr);
		frame *f2 = GET_FRAME(p2_ctx);
		slot *e2 = GET_SLOT(f2, p2->var_nbr);

		if (e1->c.attrs) {
			check_heap_error(init_tmp_heap(q));
			check_heap_error(q->vars = map_create(NULL, NULL, NULL));
			frame *f = GET_CURR_FRAME();
			q->varno = f->actual_slots;
			q->tab_idx = 0;
			cell *tmp = deep_copy_to_heap_with_replacement(q, e1->c.attrs, e1->c.attrs_ctx, false, p1, p1_ctx, p2, p2_ctx);
			check_heap_error(tmp);
			e2->c.attrs = tmp;
			e2->c.attrs_ctx = q->st.curr_frame;
		}

		return true;
	}

	if (is_atomic(p1) && is_var(p2))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (!is_var(p2) && !has_vars(q, p1, p1_ctx))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	GET_FIRST_RAW_ARG(p1_raw,any);
	cell *tmp = deep_copy_to_heap(q, p1_raw, p1_raw_ctx, true);
	check_heap_error(tmp);

	if (is_var(p1_raw) && is_var(p2)) {
		cell tmpv;
		tmpv = *p2;
		tmpv.var_nbr = q->tab0_varno;
		unify(q, p2, p2_ctx, &tmpv, q->st.curr_frame);
	}

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool fn_copy_term_nat_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_var(p1) && is_var(p2))
		return true;

	if (is_atomic(p1) && is_var(p2))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (!is_var(p2) && !has_vars(q, p1, p1_ctx))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	GET_FIRST_RAW_ARG(p1_raw,any);
	cell *tmp = deep_copy_to_heap(q, p1_raw, p1_raw_ctx, false);
	check_heap_error(tmp);

	if (is_var(p1_raw) && is_var(p2)) {
		cell tmpv;
		tmpv = *p2;
		tmpv.var_nbr = q->tab0_varno;
		unify(q, p2, p2_ctx, &tmpv, q->st.curr_frame);
	}

	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

static bool fn_iso_clause_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable_or_var);

	while (match_clause(q, p1, p1_ctx, DO_CLAUSE)) {
		if (q->did_throw) return true;
		clause *cl = &q->st.curr_dbe->cl;
		cell *body = get_body(cl->cells);
		bool ok;

		if (body)
			ok = unify(q, p2, p2_ctx, body, q->st.fp);
		else {
			cell tmp;
			make_atom(&tmp, g_true_s);
			ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		}

		if (ok) {
			bool last_match = !is_next_key(q);
			stash_me(q, cl, last_match);
			return true;
		}

		q->retry = QUERY_RETRY;
		q->tot_backtracks++;
		retry_choice(q);
	}

	return false;
}

bool do_retract(query *q, cell *p1, pl_idx_t p1_ctx, enum clause_type is_retract)
{
	if (!q->retry) {
		cell *head = deref(q, get_head(p1), p1_ctx);

		if (is_var(head))
			return throw_error(q, head, q->latest_ctx, "instantiation_error", "not_sufficiently_instantiated");

		if (!is_callable(head))
			return throw_error(q, head, q->latest_ctx, "type_error", "callable");
	}

	bool match;

	if (is_a_rule(p1) && get_logical_body(p1)) {
		match = match_rule(q, p1, p1_ctx, is_retract);
	} else {
		p1 = get_head(p1);
		match = match_clause(q, p1, p1_ctx, is_retract);
	}

	if (!match || q->did_throw)
		return match;

	db_entry *dbe = q->st.curr_dbe;
	retract_from_db(dbe);
	bool last_match = (is_retract == DO_RETRACT) && !is_next_key(q);
	stash_me(q, &dbe->cl, last_match);
	return true;
}

static bool fn_iso_retract_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	return do_retract(q, p1, p1_ctx, DO_RETRACT);
}

static bool fn_iso_retractall_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	cell *head = deref(q, get_head(p1), p1_ctx);
	predicate *pr = search_predicate(q->st.m, head, NULL);

	if (!pr) {
		bool found = false;

		if (get_builtin(q->pl, C_STR(q, head), head->arity, &found, NULL), found)
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");

		return true;
	}

	unsigned cnt = 0;

	while (do_retract(q, p1, p1_ctx, DO_RETRACTALL)) {
		if (q->did_throw) return true;
		q->retry = QUERY_RETRY;
		q->tot_backtracks++;
		retry_choice(q);
		cnt++;
	}

	//printf("*** retracted %s/%u %u of %u clauses\n", C_STR(q, &pr->key), pr->key.arity, cnt, (unsigned)pr->cnt);

	//purge_predicate_dirty_list(q, pr);

	if (pr->idx && !pr->cnt) {
		purge_predicate_dirty_list(q, pr);
		map_destroy(pr->idx2);
		map_destroy(pr->idx);
		pr->idx2 = NULL;

		pr->idx = map_create(index_cmpkey, NULL, pr->m);
		ensure(pr->idx);
		map_allow_dups(pr->idx, true);

		if (pr->key.arity > 1) {
			pr->idx2 = map_create(index_cmpkey, NULL, pr->m);
			ensure(pr->idx2);
			map_allow_dups(pr->idx2, true);
		}

		q->st.iter = NULL;
	}

	return true;
}

static bool do_abolish(query *q, cell *c_orig, cell *c, bool hard)
{
	predicate *pr = search_predicate(q->st.m, c, NULL);
	if (!pr) return true;

	if (!pr->is_dynamic)
		return throw_error(q, c_orig, q->st.curr_frame, "permission_error", "modify,static_procedure");

	for (db_entry *dbe = pr->head; dbe; dbe = dbe->next)
		retract_from_db(dbe);

	purge_predicate_dirty_list(q, pr);
	map_destroy(pr->idx2);
	map_destroy(pr->idx);
	pr->idx2 = pr->idx = NULL;
	pr->is_processed = false;
	q->st.iter = NULL;

	if (hard) {
		pr->is_abolished = true;
	} else {
		pr->idx = map_create(index_cmpkey, NULL, pr->m);
		ensure(pr->idx);
		map_allow_dups(pr->idx, true);

		if (pr->key.arity > 1) {
			pr->idx2 = map_create(index_cmpkey, NULL, pr->m);
			ensure(pr->idx2);
			map_allow_dups(pr->idx2, true);
		}
	}

	pr->head = pr->tail = NULL;
	pr->cnt = 0;
	return true;
}

static bool fn_iso_abolish_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (p1->arity != 2)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (CMP_STR_TO_CSTR(q, p1, "/") && CMP_STR_TO_CSTR(q, p1, "//"))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *p1_name = p1 + 1;
	p1_name = deref(q, p1_name, p1_ctx);

	if (!is_atom(p1_name))
		return throw_error(q, p1_name, p1_ctx, "type_error", "atom");

	cell *p1_arity = p1 + 2;
	p1_arity = deref(q, p1_arity, p1_ctx);

	if (!CMP_STR_TO_CSTR(q, p1, "//"))
		p1_arity += 2;

	if (!is_integer(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "type_error", "integer");

	if (is_negative(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "domain_error", "not_less_than_zero");

	if (get_smallint(p1_arity) > MAX_ARITY)
		return throw_error(q, p1_arity, p1_ctx, "representation_error", "max_arity");

	bool found = false;

	if (get_builtin(q->pl, C_STR(q, p1_name), get_smallint(p1_arity), &found, NULL), found)
		return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");

	cell tmp;
	tmp = *p1_name;
	tmp.arity = get_smallint(p1_arity);
	CLR_OP(&tmp);
	return do_abolish(q, p1, &tmp, true);
}

static bool fn_soft_abolish_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (p1->arity != 2)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (CMP_STR_TO_CSTR(q, p1, "/") && CMP_STR_TO_CSTR(q, p1, "//"))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *p1_name = p1 + 1;
	p1_name = deref(q, p1_name, p1_ctx);

	if (!is_atom(p1_name))
		return throw_error(q, p1_name, p1_ctx, "type_error", "atom");

	cell *p1_arity = p1 + 2;
	p1_arity = deref(q, p1_arity, p1_ctx);

	if (!CMP_STR_TO_CSTR(q, p1, "//"))
		p1_arity += 2;

	if (!is_integer(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "type_error", "integer");

	if (is_negative(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "domain_error", "not_less_than_zero");

	if (get_smallint(p1_arity) > MAX_ARITY)
		return throw_error(q, p1_arity, p1_ctx, "representation_error", "max_arity");

	bool found = false;

	if (get_builtin(q->pl, C_STR(q, p1_name), get_smallint(p1_arity), &found, NULL), found)
		return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");

	cell tmp;
	tmp = *p1_name;
	tmp.arity = get_smallint(p1_arity);
	CLR_OP(&tmp);
	return do_abolish(q, p1, &tmp, false);
}

static unsigned count_non_anons(uint8_t *mask, unsigned bit)
{
	unsigned bits = 0;

	for (unsigned i = 0; i < bit; i++) {
		if (mask[i] > 1)
			bits++;
	}

	return bits;
}

static void do_term_assign_vars(parser *p)
{
	pl_idx_t nbr_cells = p->cl->cidx;
	term_assign_vars(p, 0, true);
	memset(p->vartab.vars, 0, sizeof(p->vartab.vars));

	for (pl_idx_t i = 0; i < nbr_cells; i++) {
		cell *c = p->cl->cells+i;

		if (!is_var(c))
			continue;

		assert(c->var_nbr < MAX_VARS);
		p->vartab.vars[c->var_nbr]++;
	}

	for (pl_idx_t i = 0; i < nbr_cells; i++) {
		cell *c = p->cl->cells+i;

		if (!is_var(c))
			continue;

		unsigned var_nbr = count_non_anons(p->vartab.vars, c->var_nbr);

		char ch = 'A';
		ch += var_nbr % 26;
		unsigned n = var_nbr / 26;
		char tmpbuf[80];

		if (p->vartab.vars[c->var_nbr] == 1)
			snprintf(tmpbuf, sizeof(tmpbuf), "%s", "_");
		else if (var_nbr < 26)
			snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "%c%d", ch, n);

		c->val_off = index_from_pool(p->m->pl, tmpbuf);
		c->flags = 0;
	}
}

static bool fn_iso_asserta_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	//if (is_cyclic_term(q, p1, p1_ctx))
	//	return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp);
	cell *head = get_head(tmp);

	if (is_var(head))
		return throw_error(q, head, q->st.curr_frame, "instantiation_error", "args_not_sufficiently_instantiated");

	if (!is_interned(head) && !is_cstring(head))
		return throw_error(q, head, q->st.curr_frame, "type_error", "callable");

	bool found = false;

	if (get_builtin(q->pl, C_STR(q, head), head->arity, &found, NULL), found) {
		if (!GET_OP(head))
			return throw_error(q, head, q->st.curr_frame, "permission_error", "modify,static_procedure");
	}

	cell *tmp2, *body = get_body(tmp);

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	pl_idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->cl->allocated_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		check_heap_error(p->cl);
		p->cl->allocated_cells = nbr_cells;
	}

	p->cl->cidx = safe_copy_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_interned(h))
		return throw_error(q, h, q->st.curr_frame, "type_error", "callable");

	db_entry *dbe = asserta_to_db(q->st.m, p->cl->nbr_vars, p->cl->nbr_temporaries, p->cl->cells, 0);

	if (!dbe)
		return throw_error(q, h, q->st.curr_frame, "permission_error", "modify_static_procedure");

	p->cl->cidx = 0;
	return true;
}

static bool fn_iso_assertz_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	//if (is_cyclic_term(q, p1, p1_ctx))
	//	return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp);
	cell *head = get_head(tmp);

	if (is_var(head))
		return throw_error(q, head, q->st.curr_frame, "instantiation_error", "args_not_sufficiently_instantiated");

	if (!is_interned(head) && !is_cstring(head))
		return throw_error(q, head, q->st.curr_frame, "type_error", "callable");

	bool found = false, evaluable = false;

	if (get_builtin(q->pl, C_STR(q, head), head->arity, &found, &evaluable), found && !evaluable) {
		if (!GET_OP(head))
			return throw_error(q, head, q->st.curr_frame, "permission_error", "modify,static_procedure");
	}

	cell *tmp2, *body = get_body(tmp);

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	pl_idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->cl->allocated_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		check_heap_error(p->cl);
		p->cl->allocated_cells = nbr_cells;
	}

	p->cl->cidx = safe_copy_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_interned(h))
		return throw_error(q, h, q->st.curr_frame, "type_error", "callable");

	db_entry *dbe = assertz_to_db(q->st.m, p->cl->nbr_vars, p->cl->nbr_temporaries, p->cl->cells, 0);

	if (!dbe)
		return throw_error(q, h, q->st.curr_frame, "permission_error", "modify_static_procedure");

	p->cl->cidx = 0;
	return true;
}

static bool fn_iso_functor_3(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_var(p1)) {
		GET_NEXT_ARG(p2,any);
		GET_NEXT_ARG(p3,any);

		if (!is_atomic(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "atomic");

		if (!is_integer(p3))
			return throw_error(q, p3, p3_ctx, "type_error", "integer");

		if (is_negative(p3))
			return throw_error(q, p3, p3_ctx, "domain_error", "not_less_than_zero");

		if (is_gt(p3,MAX_ARITY/2))
			return throw_error(q, p3, p3_ctx, "representation_error", "max_arity");

		if (!is_atom(p2) && is_positive(p3))
			return throw_error(q, p2, p2_ctx, "type_error", "atom");

		unsigned arity = get_smallint(p3);
		unsigned var_nbr = 0;

		if (arity) {
			if (!(var_nbr = create_vars(q, arity)))
				return throw_error(q, p3, p3_ctx, "resource_error", "stack");
		}

		if (is_number(p2)) {
			unify(q, p1, p1_ctx, p2, p2_ctx);
		} else {
			cell *tmp = alloc_on_heap(q, 1+arity);
			check_heap_error(tmp);
			*tmp = (cell){0};
			tmp[0].tag = TAG_INTERNED;
			tmp[0].arity = arity;
			tmp[0].nbr_cells = 1 + arity;

			if (is_cstring(p2)) {
				tmp[0].val_off = index_from_pool(q->pl, C_STR(q, p2));
			} else
				tmp[0].val_off = p2->val_off;

			for (unsigned i = 1; i <= arity; i++) {
				memset(tmp+i, 0, sizeof(cell));
				tmp[i].tag = TAG_VAR;
				tmp[i].nbr_cells = 1;
				tmp[i].var_nbr = var_nbr++;
				tmp[i].val_off = g_anon_s;
				tmp[i].flags = FLAG_VAR_FRESH | FLAG_VAR_ANON;
			}

			unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
		}

		return true;
	}

	cell tmp = *p1;
	tmp.nbr_cells = 1;
	tmp.arity = 0;
	CLR_OP(&tmp);

	if (is_string(p1)) {
		tmp.tag = TAG_INTERNED;
		tmp.val_off = g_dot_s;
		tmp.flags = 0;
	}

	GET_NEXT_ARG(p2,any);

	if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) != true)
		return false;

	GET_NEXT_ARG(p3,any);
	make_int(&tmp, p1->arity);
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

static bool fn_iso_current_rule_1(query *q)
{
	GET_FIRST_ARG(p1,structure);
	int add_two = 0;

	if (!CMP_STR_TO_CSTR(q, p1, "/"))
		;
	else if (!CMP_STR_TO_CSTR(q, p1, "//"))
		add_two = 2;
	else
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *pf = deref(q, p1+1,p1_ctx);
	cell *pa = deref(q, p1+2, p1_ctx);

	if (!is_atom(pf))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (!is_integer(pa))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	const char *functor = C_STR(q, pf);
	unsigned arity = get_smallint(pa) + add_two;

	if (strchr(functor, ':')) {
		char tmpbuf1[256], tmpbuf2[256];
		tmpbuf1[0] = tmpbuf2[0] = '\0';
		sscanf(functor, "%255[^:]:%255s", tmpbuf1, tmpbuf2);
		tmpbuf1[sizeof(tmpbuf1)-1] = tmpbuf2[sizeof(tmpbuf2)-1] = '\0';
		module *m = m = find_module(q->pl, tmpbuf1);
		if (!m) return false;

		if (find_functor(m, functor, arity))
			return true;

		return false;
	}

	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = index_from_pool(q->pl, functor);
	tmp.arity = arity;

	if (search_predicate(q->st.m, &tmp, NULL))
		return true;

	bool found = false;

	if (get_builtin(q->pl, functor, arity, &found, NULL), found)
		return true;

	return false;
}

static bool search_functor(query *q, cell *p1, pl_idx_t p1_ctx, cell *p2, pl_idx_t p2_ctx)
{
	if (!q->retry)
		q->st.f_iter = map_first(q->st.m->index);

	push_choice(q);
	predicate *pr = NULL;
	check_heap_error(check_slot(q, MAX_VARS));

	while (map_next(q->st.f_iter, (void*)&pr)) {
		CHECK_INTERRUPT();

		const char *src = C_STR(q, &pr->key);

		if (src[0] == '$')
			continue;

		if (pr->is_abolished || pr->is_prebuilt)
			continue;

		if (try_me(q, MAX_VARS) != true) {
			map_done(q->st.f_iter);
			q->st.f_iter = NULL;
			return false;
		}

		cell tmpn, tmpa;
		make_atom(&tmpn, pr->key.val_off);
		make_int(&tmpa, pr->key.arity);

		if (unify(q, p1, p1_ctx, &tmpn, q->st.fp)
			&& unify(q, p2, p2_ctx, &tmpa, q->st.fp)) {
			return true;
		}

		undo_me(q);
	}

	map_done(q->st.f_iter);
	drop_choice(q);
	q->st.f_iter = NULL;
	return false;
}

static bool fn_iso_current_predicate_1(query *q)
{
	GET_FIRST_ARG(p_pi,any);

	if (is_var(p_pi)) {
		cell tmp1, tmp2;
		cell *p1 = &tmp1, *p2 = &tmp2;
		pl_idx_t p1_ctx = q->st.curr_frame;
		pl_idx_t p2_ctx = q->st.curr_frame;
		frame *f = GET_CURR_FRAME();
		unsigned var_nbr = f->actual_slots;
		make_var(&tmp1, 0, var_nbr++);
		make_var(&tmp2, 0, var_nbr++);
		create_vars(q, 2);
		bool ok = search_functor(q, p1, p1_ctx, p2, p2_ctx) ? true : false;
		cell *tmp = alloc_on_heap(q, 3);
		make_struct(tmp, g_slash_s, NULL, 2, 2);
		tmp[1] = *p1;
		tmp[2] = *p2;
		SET_OP(tmp, OP_YFX);
		return ok && unify(q, p_pi, p_pi_ctx, tmp, q->st.curr_frame);
	}

	if (p_pi->arity != 2)
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	if (CMP_STR_TO_CSTR(q, p_pi, "/"))
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	cell *p1, *p2;
	pl_idx_t p1_ctx, p2_ctx;

	p1 = p_pi + 1;
	p1 = deref(q, p1, p_pi_ctx);
	p1_ctx = q->latest_ctx;

	if (!is_atom(p1) && !is_var(p1))
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	p2 = p_pi + 2;
	p2 = deref(q, p2, p_pi_ctx);
	p2_ctx = q->latest_ctx;

	if ((!is_integer(p2) || is_negative(p2)) && !is_var(p2))
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	if (is_var(p1) || is_var(p2))
		return search_functor(q, p1, p1_ctx, p2, p2_ctx) ? true : false;

	cell tmp = (cell){0};
	tmp.tag = TAG_INTERNED;
	tmp.val_off = is_interned(p1) ? p1->val_off : index_from_pool(q->pl, C_STR(q, p1));
	tmp.arity = get_smallint(p2);
	bool is_prebuilt = false;
	bool ok = search_predicate(q->st.m, &tmp, &is_prebuilt) != NULL;
	return ok && !is_prebuilt;
}

static bool fn_cyclic_term_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_cyclic_term(q, p1, p1_ctx) ? true : false;
}

static bool fn_iso_acyclic_term_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_acyclic_term(q, p1, p1_ctx) ? true : false;
}

static bool fn_iso_current_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);

	if (!CMP_STR_TO_CSTR(q, p1, "double_quotes")) {
		cell tmp;

		if (q->st.m->flags.double_quote_atom)
			make_atom(&tmp, index_from_pool(q->pl, "atom"));
		else if (q->st.m->flags.double_quote_codes)
			make_atom(&tmp, index_from_pool(q->pl, "codes"));
		else if (q->st.m->flags.double_quote_chars)
			make_atom(&tmp, index_from_pool(q->pl, "chars"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "char_conversion")) {
		cell tmp;

		if (q->st.m->flags.char_conversion)
			make_atom(&tmp, g_on_s);
		else
			make_atom(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "unix")) {
		cell tmp;
		make_atom(&tmp, g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "dos")) {
		cell tmp;
		make_atom(&tmp, g_false_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "windows")) {
		cell tmp;
		make_atom(&tmp, g_false_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "occurs_check")) {
		cell tmp;

		if (q->st.m->flags.occurs_check == OCCURS_CHECK_TRUE)
			make_atom(&tmp, g_true_s);
		else if (q->st.m->flags.occurs_check == OCCURS_CHECK_FALSE)
			make_atom(&tmp, g_false_s);
		else
			make_atom(&tmp, index_from_pool(q->pl, "error"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "encoding")) {
		cell tmp;
		make_atom(&tmp, index_from_pool(q->pl, "UTF-8"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "strict_iso")) {
		cell tmp;

		if (!q->st.m->flags.not_strict_iso)
			make_atom(&tmp, g_on_s);
		else
			make_atom(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "debug")) {
		cell tmp;

		if (q->st.m->flags.debug)
			make_atom(&tmp, g_on_s);
		else
			make_atom(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "character_escapes")) {
		cell tmp;

		if (q->st.m->flags.character_escapes)
			make_atom(&tmp, g_true_s);
		else
			make_atom(&tmp, g_false_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "dialect")) {
		cell tmp;
		make_atom(&tmp, index_from_pool(q->pl, "trealla"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "integer_rounding_function")) {
		cell tmp;
		make_atom(&tmp, index_from_pool(q->pl, "toward_zero"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "bounded")) {
		cell tmp;
		make_atom(&tmp, g_false_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "max_arity")) {
		cell tmp;
		make_int(&tmp, MAX_ARITY);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "max_integer")) {
		return false;
	} else if (!CMP_STR_TO_CSTR(q, p1, "min_integer")) {
		return false;
	} else if (!CMP_STR_TO_CSTR(q, p1, "cpu_count")) {
		cell tmp;
		make_int(&tmp, g_cpu_count);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "version")) {
		unsigned v1 = 0;
		sscanf(g_version, "v%u", &v1);
		cell tmp;
		make_int(&tmp, v1);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "version_data")) {
		unsigned v1 = 0, v2 = 0, v3 = 0;
		sscanf(g_version, "v%u.%u.%u", &v1, &v2, &v3);
		cell *tmp = alloc_on_heap(q, 5);
		check_heap_error(tmp);
		make_atom(&tmp[0], index_from_pool(q->pl, "trealla"));
		make_int(&tmp[1], v1);
		make_int(&tmp[2], v2);
		make_int(&tmp[3], v3);
		make_atom(&tmp[4], g_nil_s);
		tmp[0].arity = 4;
		tmp[0].nbr_cells = 5;
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "version_git")) {
		cell tmp;
		make_atom(&tmp, index_from_pool(q->pl, g_version));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "argv")) {
		if (g_avc >= g_ac) {
			cell tmp;
			make_atom(&tmp, g_nil_s);
			return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		}

		int i = g_avc;
		cell tmp;
		check_heap_error(make_cstring(&tmp, g_av[i++]));
		allocate_list(q, &tmp);

		while (i < g_ac) {
			check_heap_error(make_cstring(&tmp, g_av[i++]));
			append_list(q, &tmp);
		}

		cell *l = end_list(q);
		check_heap_error(l);
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "unknown")) {
		cell tmp;
		make_atom(&tmp,
			q->st.m->flags.unknown == UNK_ERROR ? index_from_pool(q->pl, "error") :
			q->st.m->flags.unknown == UNK_WARNING ? index_from_pool(q->pl, "warning") :
			q->st.m->flags.unknown == UNK_CHANGEABLE ? index_from_pool(q->pl, "changeable") :
			index_from_pool(q->pl, "fail"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STR_TO_CSTR(q, p1, "generate_debug_info")) {
	}

	return throw_error(q, p1, p1_ctx, "domain_error", "prolog_flag");
}

static bool fn_iso_set_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (!is_atom(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (!CMP_STR_TO_CSTR(q, p1, "cpu_count") && is_integer(p2)) {
		g_cpu_count = get_smallint(p2);
		return true;
	}

	if (!is_atom(p2) && !is_integer(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "atom");

	if (!CMP_STR_TO_CSTR(q, p1, "double_quotes")) {
		if (!CMP_STR_TO_CSTR(q, p2, "atom")) {
			q->st.m->flags.double_quote_chars = q->st.m->flags.double_quote_codes = false;
			q->st.m->flags.double_quote_atom = true;
		} else if (!CMP_STR_TO_CSTR(q, p2, "codes")) {
			q->st.m->flags.double_quote_chars = q->st.m->flags.double_quote_atom = false;
			q->st.m->flags.double_quote_codes = true;
		} else if (!CMP_STR_TO_CSTR(q, p2, "chars")) {
			q->st.m->flags.double_quote_atom = q->st.m->flags.double_quote_codes = false;
			q->st.m->flags.double_quote_chars = true;
		} else {
			cell *tmp = alloc_on_heap(q, 3);
			check_heap_error(tmp);
			make_struct(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}

		q->st.m->p->flags = q->st.m->flags;
	} else if (!CMP_STR_TO_CSTR(q, p1, "character_escapes")) {
		if (!CMP_STR_TO_CSTR(q, p2, "true") || !CMP_STR_TO_CSTR(q, p2, "on"))
			q->st.m->flags.character_escapes = true;
		else if (!CMP_STR_TO_CSTR(q, p2, "false") || !CMP_STR_TO_CSTR(q, p2, "off"))
			q->st.m->flags.character_escapes = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			check_heap_error(tmp);
			make_struct(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_STR_TO_CSTR(q, p1, "char_conversion")) {
		if (!CMP_STR_TO_CSTR(q, p2, "true") || !CMP_STR_TO_CSTR(q, p2, "on"))
			q->st.m->flags.char_conversion = true;
		else if (!CMP_STR_TO_CSTR(q, p2, "false") || !CMP_STR_TO_CSTR(q, p2, "off"))
			q->st.m->flags.char_conversion = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			check_heap_error(tmp);
			make_struct(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_STR_TO_CSTR(q, p1, "occurs_check")) {
		if (!CMP_STR_TO_CSTR(q, p2, "true") || !CMP_STR_TO_CSTR(q, p2, "on"))
			q->st.m->flags.occurs_check = OCCURS_CHECK_TRUE;
		else if (!CMP_STR_TO_CSTR(q, p2, "false") || !CMP_STR_TO_CSTR(q, p2, "off"))
			q->st.m->flags.occurs_check = OCCURS_CHECK_FALSE;
		else if (!CMP_STR_TO_CSTR(q, p2, "error"))
			q->st.m->flags.occurs_check = OCCURS_CHECK_ERROR;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			check_heap_error(tmp);
			make_struct(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_STR_TO_CSTR(q, p1, "debug")) {
		if (!CMP_STR_TO_CSTR(q, p2, "true") || !CMP_STR_TO_CSTR(q, p2, "on"))
			q->st.m->flags.debug = true;
		else if (!CMP_STR_TO_CSTR(q, p2, "false") || !CMP_STR_TO_CSTR(q, p2, "off"))
			q->st.m->flags.debug = false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			check_heap_error(tmp);
			make_struct(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_STR_TO_CSTR(q, p1, "strict_iso")) {
		if (!CMP_STR_TO_CSTR(q, p2, "true") || !CMP_STR_TO_CSTR(q, p2, "on"))
			q->st.m->flags.not_strict_iso = !true;
		else if (!CMP_STR_TO_CSTR(q, p2, "false") || !CMP_STR_TO_CSTR(q, p2, "off"))
			q->st.m->flags.not_strict_iso = !false;
		else {
			cell *tmp = alloc_on_heap(q, 3);
			check_heap_error(tmp);
			make_struct(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_STR_TO_CSTR(q, p1, "unknown")) {
		if (!CMP_STR_TO_CSTR(q, p2, "fail")) {
			q->st.m->flags.unknown = UNK_FAIL;
		} else if (!CMP_STR_TO_CSTR(q, p2, "error")) {
			q->st.m->flags.unknown = UNK_ERROR;
		} else if (!CMP_STR_TO_CSTR(q, p2, "warning")) {
			q->st.m->flags.unknown = UNK_WARNING;
		} else if (!CMP_STR_TO_CSTR(q, p2, "changeable")) {
			q->st.m->flags.unknown = UNK_CHANGEABLE;
		} else {
			cell *tmp = alloc_on_heap(q, 3);
			check_heap_error(tmp);
			make_struct(tmp, g_plus_s, fn_iso_add_2, 2, 2);
			SET_OP(tmp, OP_YFX);
			tmp[1] = *p1; tmp[1].nbr_cells = 1;
			tmp[2] = *p2; tmp[2].nbr_cells = 1;
			return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
		}
	} else if (!CMP_STR_TO_CSTR(q, p1, "bounded")
		|| !CMP_STR_TO_CSTR(q, p1, "max_arity")
		|| !CMP_STR_TO_CSTR(q, p1, "max_integer")
		|| !CMP_STR_TO_CSTR(q, p1, "min_integer")
		|| !CMP_STR_TO_CSTR(q, p1, "version")
		|| !CMP_STR_TO_CSTR(q, p1, "version_data")
		|| !CMP_STR_TO_CSTR(q, p1, "version_git")
		|| !CMP_STR_TO_CSTR(q, p1, "encoding")
		|| !CMP_STR_TO_CSTR(q, p1, "unix")
		|| !CMP_STR_TO_CSTR(q, p1, "integer_rounding_function")
		|| !CMP_STR_TO_CSTR(q, p1, "dialect")
		) {
		return throw_error(q, p1, p1_ctx, "permission_error", "modify,flag");
	} else if (!CMP_STR_TO_CSTR(q, p1, "generate_debug_info")) {
	} else {
		return throw_error(q, p1, p1_ctx, "domain_error", "prolog_flag");
	}

	q->flags = q->st.m->flags;
	return true;
}

typedef struct { cell *c; pl_idx_t c_ctx; query *q; bool ascending; int arg; } basepair;

static int nodecmp(const void *ptr1, const void *ptr2)
{
	const basepair *cp1 = (const basepair*)ptr1;
	const basepair *cp2 = (const basepair*)ptr2;
	bool ascending = cp1->ascending;
	query *q = cp1->q;
	int arg = cp1->arg;
	cell *p1 = cp1->c, *p2 = cp2->c;
	pl_idx_t p1_ctx = cp1->c_ctx, p2_ctx = cp2->c_ctx;

	if ((p1->arity >= arg) && (arg > 0)) {
		p1 = p1 + 1;
		p2 = p2 + 1;

		while (--arg > 0) {
			p1 += p1->nbr_cells;
			p2 += p2->nbr_cells;
		}

		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	int ok = compare(q, p1, p1_ctx, p2, p2_ctx);

	if (ascending)
		return ok < 0 ? -1 : ok > 0 ? 1 : 0;
	else
		return ok < 0 ? 1 : ok > 0 ? -1 : 0;
}

static cell *nodesort(query *q, cell *p1, pl_idx_t p1_ctx, bool dedup, bool keysort, bool *status)
{
	pl_int_t max = PL_INT_MAX, skip = 0;
	pl_idx_t tmp_ctx = p1_ctx;
	cell tmp = {0};

	skip_max_list(q, p1, &tmp_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);
	size_t cnt = skip;
	basepair *base = malloc(sizeof(basepair)*cnt);
	LIST_HANDLER(p1);
	size_t idx = 0;

	while (is_list(p1)) {
		CHECK_INTERRUPT();
		cell *h = deref(q, LIST_HEAD(p1), p1_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (keysort) {
			cell *tmp = deref(q, h, h_ctx);
			pl_idx_t tmp_ctx = q->latest_ctx;

			if (!is_structure(tmp) || strcmp(C_STR(q, tmp), "-")) {
				*status = throw_error(q, tmp, tmp_ctx, "type_error", "pair");
				free(base);
				return NULL;
			}
		}

		base[idx].c = h;
		base[idx].c_ctx = h_ctx;
		base[idx].q = q;
		base[idx].ascending = true;
		base[idx].arg = keysort ? 1 : 0;
		idx++;
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	// On Linux qsort seems to produce a stable sort, although it's
	// not guaranteed. On BSD systems mergesort is supposed to be.
	// Note: bagof/setof are now using a Prolog sort/keysort that
	// is known to be stable.

#if __BSD__ || __FREEBSD__ || __APPLE__ || __MACH__ || __Darwin__ || __DragonFly__
	mergesort(base, cnt, sizeof(basepair), nodecmp);
#else
	qsort(base, cnt, sizeof(basepair), nodecmp);
#endif

	for (size_t i = 0; i < cnt; i++) {
		if (i > 0) {
			if (dedup && !nodecmp(&base[i], &base[i-1]))
				continue;
		}

		cell *c = deref(q, base[i].c, base[i].c_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		cell tmp;

		if (is_var(c) || is_structure(c)) {
			make_var(&tmp, c->val_off, create_vars(q, 1));
			unify(q, c, c_ctx, &tmp, q->st.curr_frame);
			c = &tmp;
		}

		if (i == 0)
			allocate_list(q, c);
		else
			append_list(q, c);
	}

	cell *l = end_list(q);
	free(base);
	return l;
}

static bool fn_iso_sort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int_t skip1 = 0, skip2 = 0;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p1)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	bool status = false;
	cell *l = nodesort(q, p1, p1_ctx, true, false, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool fn_iso_msort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int_t skip1 = 0, skip2 = 0;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p1)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	bool status = false;
	cell *l = nodesort(q, p1, p1_ctx, false, false, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool fn_iso_keysort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int_t skip1 = 0, skip2 = 0;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_iso_list(p2)) {
		LIST_HANDLER(p2);
		cell *tmp_h = LIST_HEAD(p2);
		tmp_h = deref(q, tmp_h, p2_ctx);
		pl_idx_t tmp_h_ctx = q->latest_ctx;
		LIST_TAIL(p2);

		if (!is_var(tmp_h) && (!is_structure(tmp_h) || strcmp(C_STR(q, tmp_h), "-")))
			return throw_error(q, tmp_h, tmp_h_ctx, "type_error", "pair");
	}

	if (is_nil(p1)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	bool status = false;
	cell *l = nodesort(q, p1, p1_ctx, false, true, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static cell *nodesort4(query *q, cell *p1, pl_idx_t p1_ctx, bool dedup, bool ascending, int arg, bool *status)
{
	pl_int_t max = PL_INT_MAX, skip = 0;
	pl_idx_t tmp_ctx = p1_ctx;
	cell tmp = {0};

	skip_max_list(q, p1, &tmp_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);
	size_t cnt = skip;
	basepair *base = malloc(sizeof(basepair)*cnt);
	LIST_HANDLER(p1);
	size_t idx = 0;

	while (is_list(p1)) {
		CHECK_INTERRUPT();
		cell *h = deref(q, LIST_HEAD(p1), p1_ctx);
		pl_idx_t h_ctx = q->latest_ctx;
		base[idx].c = h;
		base[idx].c_ctx = h_ctx;
		base[idx].q = q;
		base[idx].ascending = ascending;
		base[idx].arg = arg;
		idx++;
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	// On Linux qsort seems to produce a stable sort, although it's
	// not guaranteed. On BSD systems mergesort is supposed to be.
	// Note: bagof/setof are now using a Prolog sort/keysort that
	// is known to be stable.

#if __BSD__ || __FREEBSD__ || __APPLE__ || __MACH__ || __Darwin__ || __DragonFly__
	mergesort(base, cnt, sizeof(basepair), nodecmp);
#else
	qsort(base, cnt, sizeof(basepair), nodecmp);
#endif

	for (size_t i = 0; i < cnt; i++) {
		if (i > 0) {
			if (dedup && !nodecmp(&base[i], &base[i-1]))
				continue;
		}

		cell *c = deref(q, base[i].c, base[i].c_ctx);
		pl_idx_t c_ctx = q->latest_ctx;
		cell tmp;

		if (is_var(c) || is_structure(c)) {
			make_var(&tmp, c->val_off, create_vars(q, 1));
			unify(q, c, c_ctx, &tmp, q->st.curr_frame);
			c = &tmp;
		}

		if (i == 0)
			allocate_list(q, c);
		else
			append_list(q, c);
	}

	cell *l = end_list(q);
	free(base);
	return l;
}

static bool fn_sort_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,list_or_nil);
	GET_NEXT_ARG(p4,list_or_nil_or_var);
	bool is_partial = false, dedup = false, ascending = true;
	pl_int_t skip1 = 0, skip2 = 0;

	if (is_integer(p1) && is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	int arg = get_smallint(p1);
	const char *src = C_STR(q, p2);

	if (!strcmp(src, "@<")) {
		ascending = true;
		dedup = true;
	} else if (!strcmp(src, "@=<")) {
		ascending = true;
		dedup = false;
	} else if (!strcmp(src, "@>")) {
		ascending = false;
		dedup = true;
	} else if (!strcmp(src, "@>=")) {
		ascending = false;
		dedup = false;
	} else
		return throw_error(q, p2, p2_ctx, "domain_error", "order");

	if (is_iso_list(p3) && !check_list(q, p3, p3_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p3, p3_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p3, p3_ctx, "instantiation_error", "list");

	if (is_iso_list(p4) && !check_list(q, p4, p4_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p4, p4_ctx, "type_error", "list");

	if (is_iso_list(p4)) {
		LIST_HANDLER(p4);
		cell *tmp_h = LIST_HEAD(p4);
		tmp_h = deref(q, tmp_h, p4_ctx);
		pl_idx_t tmp_h_ctx = q->latest_ctx;
		LIST_TAIL(p4);

		if (!is_var(tmp_h) && (!is_structure(tmp_h) || strcmp(C_STR(q, tmp_h), "-")))
			return throw_error(q, tmp_h, tmp_h_ctx, "type_error", "pair");
	}

	if (is_nil(p3)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	bool status = false;
	cell *l = nodesort4(q, p3, p3_ctx, dedup, ascending, arg, &status);
	if (!l) return status;
	return unify(q, p4, p4_ctx, l, q->st.curr_frame);
}

cell *convert_to_list(query *q, cell *c, pl_idx_t nbr_cells)
{
	if ((!nbr_cells || !c->nbr_cells)) {
		cell *c = alloc_on_tmp(q, 1);
		if (!c) return c;
		make_atom(c, g_nil_s);
		return c;
	}

	allocate_list(q, c);
	nbr_cells -= c->nbr_cells;
	c += c->nbr_cells;

	while (nbr_cells > 0) {
		append_list(q, c);
		nbr_cells -= c->nbr_cells;
		c += c->nbr_cells;
	}

	// This function is only ever called on a queue which
	// already has a safe_copy done, so the end_list below
	// can do an unsafe copy.

	return end_list_unsafe(q);
}

static bool fn_sys_list_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell *l = convert_to_list(q, get_queue(q), queue_used(q));
	return unify(q, p1, p1_ctx, l, q->st.curr_frame);
}

static bool fn_sys_queue_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_raw_copy_to_tmp(q, p1, p1_ctx);
	check_heap_error(tmp);
	check_heap_error(alloc_on_queuen(q, 0, tmp));
	return true;
}

bool fn_sys_queuen_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,any);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_raw_copy_to_tmp(q, p2, p2_ctx);
	check_heap_error(tmp);
	check_heap_error(alloc_on_queuen(q, get_smallint(p1), tmp));
	return true;
}

static bool do_op(query *q, cell *p3, pl_idx_t p3_ctx)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);

	if (!is_atom(p3))
		return throw_error(q, p3, p3_ctx, "type_error", "atom");

	unsigned specifier;
	unsigned pri = get_smallint(p1);

	if (!CMP_STR_TO_CSTR(q, p2, "fx"))
		specifier = OP_FX;
	else if (!CMP_STR_TO_CSTR(q, p2, "fy"))
		specifier = OP_FY;
	else if (!CMP_STR_TO_CSTR(q, p2, "xf"))
		specifier = OP_XF;
	else if (!CMP_STR_TO_CSTR(q, p2, "xfx"))
		specifier = OP_XFX;
	else if (!CMP_STR_TO_CSTR(q, p2, "xfy"))
		specifier = OP_XFY;
	else if (!CMP_STR_TO_CSTR(q, p2, "yf"))
		specifier = OP_YF;
	else if (!CMP_STR_TO_CSTR(q, p2, "yfx"))
		specifier = OP_YFX;
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "operator_specifier");

	if (pri && !CMP_STR_TO_CSTR(q, p3, "|") && (!IS_INFIX(specifier) || (pri < 1001)))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_STR_TO_CSTR(q, p3, "[]"))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_STR_TO_CSTR(q, p3, "{}"))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_STR_TO_CSTR(q, p3, ","))
		return throw_error(q, p3, p3_ctx, "permission_error", "modify,operator");

	unsigned tmp_optype = 0;
	unsigned tmp_pri = search_op(q->st.m, C_STR(q, p3), &tmp_optype, false);

	if (IS_INFIX(specifier) && IS_POSTFIX(tmp_optype))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!tmp_pri && !pri)
		return true;

	tmp_pri = find_op(q->st.m, C_STR(q, p3), OP_FX);

	if (IS_POSTFIX(specifier) && (IS_INFIX(tmp_optype) || tmp_pri))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	tmp_pri = find_op(q->st.m, C_STR(q, p3), OP_FY);

	if (IS_POSTFIX(specifier) && (IS_INFIX(tmp_optype) || tmp_pri))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!set_op(q->st.m, C_STR(q, p3), specifier, pri))
		return throw_error(q, p3, p3_ctx, "resource_error", "too_many_ops");

	return true;
}

static bool fn_iso_op_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,list_or_atom);

	if (is_negative(p1) || is_gt(p1,1200))
		return throw_error(q, p1, p1_ctx, "domain_error", "operator_priority");

	LIST_HANDLER(p3);

	while (is_list(p3)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);
		bool ok = do_op(q, h, q->latest_ctx);

		if (ok != true)
			return ok;

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_var(p3))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "atom");

		if (is_nil(p3))
			return true;
	}

	if (is_atom(p3))
		return do_op(q, p3, p3_ctx);

	return true;
}

static bool fn_erase_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	uuid u;
	uuid_from_buf(C_STR(q, p1), &u);
	db_entry *dbe = erase_from_db(q->st.m, &u);
	check_heap_error(dbe);
	return true;
}

static bool fn_instance_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
	uuid u;
	uuid_from_buf(C_STR(q, p1), &u);
	db_entry *dbe = find_in_db(q->st.m, &u);
	check_heap_error(dbe);
	return unify(q, p2, p2_ctx, dbe->cl.cells, q->st.curr_frame);
}

static bool fn_clause_3(query *q)
{
	GET_FIRST_ARG(p1,callable_or_var);
	GET_NEXT_ARG(p2,callable_or_var);
	GET_NEXT_ARG(p3,atom_or_var);

	if (is_var(p1) && is_var(p2) && is_var(p3))
		return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	for (;;) {
		CHECK_INTERRUPT();
		clause *cl;

		if (!is_var(p3)) {
			uuid u;
			uuid_from_buf(C_STR(q, p3), &u);
			db_entry *dbe = find_in_db(q->st.m, &u);

			if (!dbe || (!u.u1 && !u.u2))
				break;

			q->st.curr_dbe = dbe;
			cl = &dbe->cl;
			cell *head = get_head(cl->cells);

			if (!unify(q, p1, p1_ctx, head, q->st.fp))
				break;
		} else {
			if (match_clause(q, p1, p1_ctx, DO_CLAUSE) != true)
				break;

			char tmpbuf[128];
			uuid_to_buf(&q->st.curr_dbe->u, tmpbuf, sizeof(tmpbuf));
			cell tmp;
			check_heap_error(make_cstring(&tmp, tmpbuf));
			unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
			unshare_cell(&tmp);
			cl = &q->st.curr_dbe->cl;
		}

		cell *body = get_body(cl->cells);
		bool ok;

		if (body)
			ok = unify(q, p2, p2_ctx, body, q->st.fp);
		else {
			cell tmp;
			make_atom(&tmp, g_true_s);
			ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		}

		if (ok) {
			bool last_match;

			if (is_var(p3)) {
				last_match = !is_next_key(q);
			} else {
				last_match = true;
			}

			stash_me(q, cl, last_match);
			return true;
		}

		if (!is_var(p3))
			break;

		q->retry = QUERY_RETRY;
		q->tot_backtracks++;
		retry_choice(q);
	}

	return false;
}

static bool do_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *head = deref(q, get_head(p1), p1_ctx);

	if (is_var(head))
		return throw_error(q, head, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin(q->pl, C_STR(q, head), head->arity, &found, NULL), found) {
		if (!GET_OP(head))
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");
	}

	cell *body = get_body(p1);

	if (body)
		body = deref(q, body, p1_ctx);

	if (body && !is_callable(body))
		return throw_error(q, body, q->latest_ctx, "type_error", "callable");

	cell *tmp2;

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, q->latest_ctx, "type_error", "callable");

	GET_NEXT_ARG(p2,atom_or_var);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp);
	pl_idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->cl->allocated_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		check_heap_error(p->cl);
		p->cl->allocated_cells = nbr_cells;
	}

	p->cl->cidx = safe_copy_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_interned(h))
		return throw_error(q, h, q->latest_ctx, "type_error", "callable");

	db_entry *dbe = asserta_to_db(q->st.m, p->cl->nbr_vars, p->cl->nbr_temporaries, p->cl->cells, 0);

	if (!dbe)
		return throw_error(q, h, q->st.curr_frame, "permission_error", "modify_static_procedure");

	p->cl->cidx = 0;

	if (!is_var(p2)) {
		uuid u;
		uuid_from_buf(C_STR(q, p2), &u);
		dbe->u = u;
	} else {
		uuid_gen(q->pl, &dbe->u);
		char tmpbuf[128];
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2;
		check_heap_error(make_cstring(&tmp2, tmpbuf));
		unify(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
		unshare_cell(&tmp2);
	}

	return true;
}

static bool fn_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	//if (is_cyclic_term(q, p1, p1_ctx))
	//	return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	GET_NEXT_ARG(p2,var);
	return do_asserta_2(q);
}

static bool fn_sys_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	//if (is_cyclic_term(q, p1, p1_ctx))
	//	return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	GET_NEXT_ARG(p2,atom);
	return do_asserta_2(q);
}

static bool do_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *head = deref(q, get_head(p1), p1_ctx);

	//if (is_var(head))
	//	return throw_error(q, head, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin(q->pl, C_STR(q, head), head->arity, &found, NULL), found) {
		if (!GET_OP(head))
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");
	}

	cell *body = get_body(p1);

	if (body)
		body = deref(q, body, p1_ctx);

	if (body && !is_callable(body))
		return throw_error(q, body, q->latest_ctx, "type_error", "callable");

	cell *tmp2;

	if (body && ((tmp2 = check_body_callable(q->st.m->p, body)) != NULL))
		return throw_error(q, tmp2, q->latest_ctx, "type_error", "callable");

	GET_NEXT_ARG(p2,atom_or_var);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp);
	pl_idx_t nbr_cells = tmp->nbr_cells;
	parser *p = q->st.m->p;

	if (nbr_cells > p->cl->allocated_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		check_heap_error(p->cl);
		p->cl->allocated_cells = nbr_cells;
	}

	p->cl->cidx = safe_copy_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_interned(h))
		return throw_error(q, h, q->latest_ctx, "type_error", "callable");

	db_entry *dbe = assertz_to_db(q->st.m, p->cl->nbr_vars, p->cl->nbr_temporaries, p->cl->cells, 0);

	if (!dbe)
		return throw_error(q, h, q->st.curr_frame, "permission_error", "modify_static_procedure");

	p->cl->cidx = 0;

	if (!is_var(p2)) {
		uuid u;
		uuid_from_buf(C_STR(q, p2), &u);
		dbe->u = u;
	} else {
		uuid_gen(q->pl, &dbe->u);
		char tmpbuf[128];
		uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2;
		check_heap_error(make_cstring(&tmp2, tmpbuf));
		unify(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
		unshare_cell(&tmp2);
	}

	return true;
}

static bool fn_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	//if (is_cyclic_term(q, p1, p1_ctx))
	//	return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	GET_NEXT_ARG(p2,var);
	return do_assertz_2(q);
}

static bool fn_sys_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);

	///if (is_cyclic_term(q, p1, p1_ctx))
	//	return throw_error(q, p1, q->st.curr_frame, "syntax_error", "cyclic_term");

	GET_NEXT_ARG(p2,atom);
	return do_assertz_2(q);
}

static void save_db(FILE *fp, query *q, int logging)
{
	q->listing = true;

	for (predicate *pr = q->st.m->head; pr; pr = pr->next) {
		if (pr->is_prebuilt)
			continue;

		const char *src = C_STR(q, &pr->key);

		if (src[0] == '$')
			continue;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
			if (dbe->cl.dgen_erased)
				continue;

			if (logging)
				fprintf(fp, "z_(");

			for (unsigned i = 0; i < MAX_IGNORES; i++)
				q->ignores[i] = false;

			q->print_idx = 0;
			print_term(q, fp, dbe->cl.cells, 0, 0);

			if (logging) {
				char tmpbuf[256];
				uuid_to_buf(&dbe->u, tmpbuf, sizeof(tmpbuf));
				fprintf(fp, ",'%s')", tmpbuf);
			}

			if (q->last_thing_was_symbol)
				fprintf(fp, " ");

			fprintf(fp, ".\n");
		}
	}

	q->listing = false;
}

static bool fn_listing_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	save_db(str->fp, q, 0);
	return true;
}

static void save_name(FILE *fp, query *q, pl_idx_t name, unsigned arity)
{
	module *m = q->st.curr_dbe ? q->st.curr_dbe->owner->m : q->st.m;
	q->listing = true;

	for (predicate *pr = m->head; pr; pr = pr->next) {
		if (pr->is_prebuilt && (arity == -1U))
			continue;

		if (name != pr->key.val_off)
			continue;

		if ((arity != pr->key.arity) && (arity != -1U))
			continue;

		for (db_entry *dbe = pr->head; dbe; dbe = dbe->next) {
			if (dbe->cl.dgen_erased)
				continue;

			for (unsigned i = 0; i < MAX_IGNORES; i++)
				q->ignores[i] = false;

			q->print_idx = 0;
			print_term(q, fp, dbe->cl.cells, 0, 0);
			fprintf(fp, ".\n");
		}
	}

	q->listing = false;
}

static bool fn_listing_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	pl_idx_t name = p1->val_off;
	unsigned arity = -1;

	if (p1->arity) {
		if (CMP_STR_TO_CSTR(q, p1, "/") && CMP_STR_TO_CSTR(q, p1, "//"))
			return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

		cell *p2 = p1 + 1;

		if (!is_atom(p2))
			return throw_error(q, p2, p1_ctx, "type_error", "atom");

		cell *p3 = p2 + p2->nbr_cells;

		if (!is_integer(p3))
			return throw_error(q, p3, p1_ctx, "type_error", "integer");

		name = index_from_pool(q->pl, C_STR(q, p2));
		arity = get_smallint(p3);

		if (!CMP_STR_TO_CSTR(q, p1, "//"))
			arity += 2;
	}

	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	save_name(str->fp, q, name, arity);
	return true;
}

static bool fn_help_0(query *q)
{
	bool found = false, evaluable = false;
	miter *iter = map_first(q->pl->help);
	builtins *fn;

	while (map_next(iter, (void**)&fn)) {
		if (fn->arity)
			fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		else
			fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
	}

	return true;
}

static bool fn_help_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	bool found = false, evaluable = false;

	if (!p1->arity) {
		if (!is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		const char *functor = C_STR(q, p1);
		miter *iter = map_find_key(q->pl->help, functor);
		builtins *fn;

		while (map_next_key(iter, (void**)&fn)) {
			if (fn->arity)
				fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
			else
				fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		}

		return true;
	}

	if (!is_structure(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (p1->arity != 2)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (p1->val_off != g_slash_s)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *f = p1 + 1;

	if (!is_atom(f))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *a = p1 + 2;

	if (!is_smallint(a))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	const char *functor = C_STR(q, f);
	unsigned arity = get_smallint(a);
	builtins *fn = get_help(q->pl, functor, arity, &found, &evaluable);

	if (!found || !fn)
		return throw_error(q, p1, p1_ctx, "domain_error", "existence");

	if (arity)
		fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
	else
		fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");

	return true;
}

static bool fn_help_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,atom);
	bool found = false, evaluable = false;
	const char *pr = C_STR(q, p2);
	char url[1024];

	if (!p1->arity) {
		if (!is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		const char *functor = C_STR(q, p1);
		miter *iter = map_find_key(q->pl->help, functor);
		builtins *fn;

		while (map_next_key(iter, (void**)&fn)) {
			if (fn->arity)
				fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
			else
				fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		}

		return true;
	}

	if (!is_structure(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (p1->arity != 2)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (p1->val_off != g_slash_s)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *f = p1 + 1;

	if (!is_atom(f))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *a = p1 + 2;

	if (!is_smallint(a))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	const char *functor = C_STR(q, f);
	unsigned arity = get_smallint(a);
	builtins *fn = get_help(q->pl, functor, arity, &found, &evaluable);

	if (!found || !fn)
		return throw_error(q, p1, p1_ctx, "domain_error", "existence");

	if (!strcmp(pr, "swi"))
		snprintf(url, sizeof(url), "http://swi-prolog.org/pldoc/man?predicate=%s/%u", functor, arity);
	else if (!strcmp(pr, "tau"))
		snprintf(url, sizeof(url), "http://tau-prolog.org/documentation/prolog/builtin/%s/%u", functor, arity);

	if (arity)
		fprintf(stdout, "%s/%u: %s(%s)%s%s %s\n", fn->name, arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"", url);
	else
		fprintf(stdout, "%s/%u: %s%s%s %s\n", fn->name, arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"", url);

	return true;
}

const char *dump_key(const void *k, const void *v, const void *p)
{
	query *q = (query*)p;
	cell *c = (cell*)k;
	static char tmpbuf[1024];
	print_term_to_buf(q, tmpbuf, sizeof(tmpbuf), c, q->st.curr_frame, 0, false, 0);
	return tmpbuf;
}

static bool fn_sys_dump_keys_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	const char *name = NULL;
	unsigned arity = -1;

	if (p1->arity) {
		if (CMP_STR_TO_CSTR(q, p1, "/") && CMP_STR_TO_CSTR(q, p1, "//"))
			return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

		cell *p2 = p1 + 1;

		if (!is_atom(p2))
			return throw_error(q, p2, p1_ctx, "type_error", "atom");

		cell *p3 = p2 + p2->nbr_cells;

		if (!is_integer(p3))
			return throw_error(q, p3, p1_ctx, "type_error", "integer");

		name = C_STR(q, p2);
		arity = get_smallint(p3);

		if (!CMP_STR_TO_CSTR(q, p1, "//"))
			arity += 2;
	}

	predicate *pr = find_functor(q->st.m, name, arity);

	if (!pr)
		return false;

	if (!pr->idx)
		return true;

	fprintf(stderr, "\n"); sl_dump(pr->idx, dump_key, q);
	return true;
}

static bool fn_sys_timer_0(query *q)
{
	q->time_started = get_time_in_usec();
	return true;
}

static bool fn_sys_elapsed_0(query *q)
{
	uint64_t elapsed = get_time_in_usec();
	elapsed -= q->time_started;
	fprintf(stdout, "Time elapsed %.03gs\n", (double)elapsed/1000/1000);
	return true;
}

static bool fn_trace_0(query *q)
{
	q->trace = !q->trace;
	return true;
}

static bool fn_time_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	fn_sys_timer_0(q);
	cell *tmp = clone_to_heap(q, true, p1, 2);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_elapsed_s, fn_sys_elapsed_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return true;
}

static bool fn_statistics_0(query *q)
{
	fprintf(stdout,
		"Goals %"PRIu64", Matches %"PRIu64", Max frames %u, choices %u, trails %u, slots %u, heap: %u.\nBacktracks %"PRIu64", TCOs:%"PRIu64", Recovered frames: %"PRIu64", slots: %"PRIu64"\n",
		q->tot_goals, q->tot_matches,
		q->hw_frames, q->hw_choices, q->hw_trails, q->hw_slots, q->st.hp,
		q->tot_retries, q->tot_tcos,
		q->tot_frecovs, q->tot_srecovs
		);
	return true;
}

static bool fn_statistics_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_var);

	if (!CMP_STR_TO_CSTR(q, p1, "cputime") && is_var(p2)) {
		uint64_t now = cpu_time_in_usec();
		double elapsed = now - q->time_cpu_started;
		cell tmp;
		make_float(&tmp, elapsed/1000/1000);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STR_TO_CSTR(q, p1, "gctime") && is_var(p2)) {
		cell tmp;
		make_float(&tmp, 0);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STR_TO_CSTR(q, p1, "wall") && is_var(p2)) {
		uint64_t now = get_time_in_usec();
		cell tmp;
		make_int(&tmp, now/1000);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STR_TO_CSTR(q, p1, "runtime")) {
		uint64_t now = cpu_time_in_usec();
		double elapsed = now - q->time_cpu_started;
		cell tmp;
		make_int(&tmp, elapsed/1000);
		allocate_list(q, &tmp);
		elapsed = now - q->time_cpu_last_started;
		q->time_cpu_last_started = now;
		make_int(&tmp, elapsed/1000);
		append_list(q, &tmp);
		make_atom(&tmp, g_nil_s);
		cell *l = end_list(q);
		check_heap_error(l);
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	return false;
}

static bool fn_sleep_1(query *q)
{
	if (q->retry)
		return true;

	GET_FIRST_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (q->is_task)
		return do_yield_0(q, get_smallint(p1)*1000);

	sleep((unsigned)get_smallint(p1));
	return true;
}

static bool fn_delay_1(query *q)
{
	if (q->retry)
		return true;

	GET_FIRST_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (q->is_task)
		return do_yield_0(q, get_smallint(p1));

	msleep((unsigned)get_smallint(p1));
	return true;
}

static bool fn_busy_1(query *q)
{
	GET_FIRST_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	pl_int_t elapse = get_smallint(p1);

	if (elapse < 0)
		return true;

	// Limit to 60 seconds...

	if (elapse > (60 * 1000))
		return true;

	pl_uint_t started = get_time_in_usec() / 1000;
	pl_uint_t end = started + elapse;

	while ((get_time_in_usec() / 1000)  < end) {
		CHECK_INTERRUPT();
	}

	return true;
}

static bool fn_now_0(query *q)
{
	pl_int_t secs = get_time_in_usec() / 1000 / 1000;
	q->accum.tag = TAG_INTEGER;
	set_smallint(&q->accum, secs);
	return true;
}

static bool fn_now_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	pl_int_t secs = get_time_in_usec() / 1000 / 1000;
	cell tmp;
	make_int(&tmp, secs);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_get_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	double v = ((double)get_time_in_usec()-q->get_started) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (double)v);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_cpu_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	double v = ((double)cpu_time_in_usec()-q->time_cpu_started) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (double)v);
	return unify (q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_between_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,integer_or_var);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p2))
		return throw_error(q, p2, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p3))
		return throw_error(q, p3, p3_ctx, "domain_error", "small_integer_range");

	if (!q->retry) {
		if (get_smallint(p1) > get_smallint(p2))
			return false;

		if (!is_var(p3)) {
			if (get_smallint(p3) > get_smallint(p2))
				return false;

			if (get_smallint(p3) < get_smallint(p1))
				return false;

			return true;
		}

		if (get_smallint(p1) != get_smallint(p2)) {
			q->st.cnt = get_smallint(p1);
			check_heap_error(push_choice(q));
		}

		return unify(q, p3, p3_ctx, p1, p1_ctx);
	}

	int64_t cnt = q->st.cnt;
	cell tmp;
	make_int(&tmp, ++cnt);

	if (cnt != get_smallint(p2)) {
		q->st.cnt = cnt;
		check_heap_error(push_choice(q));
	}

	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

#if 0
static bool fn_forall_2(query *q)
{
	if (q->retry)
		return true;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	pl_idx_t off = heap_used(q);
	check_heap_error(clone_to_heap(q, true, p1, 0));
	check_heap_error(clone_to_heap(q, false, p2, 1));
	cell *tmp = get_heap(q, off);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells + p2->nbr_cells;
	make_struct(tmp+nbr_cells, g_fail_s, fn_iso_fail_0, 0, 0);
	check_heap_error(push_choice(q));
	q->st.curr_cell = tmp;
	return true;
}
#endif

static bool fn_split_atom_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	GET_NEXT_ARG(p4,any);
	const char *src = C_STR(q, p1);
	int sep = peek_char_utf8(C_STR(q, p2));
	int pad = peek_char_utf8(C_STR(q, p3));
	const char *start = src, *ptr;
	cell *l = NULL;
	int nbr = 1, in_list = 0;

	if (!*start) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	// FIXME: sep & pad are not a single char...

	while ((ptr = strchr_utf8(start, sep)) != NULL) {
		while ((peek_char_utf8(start) == pad) && (pad != sep))
			get_char_utf8(&start);

		if (ptr-start) {
			cell tmp;
			check_heap_error(make_slice(q, &tmp, p1, start-src, ptr-start));

			if (nbr++ == 1)
				allocate_list(q, &tmp);
			else
				append_list(q, &tmp);

			in_list = 1;
		}

		start = ptr + 1;
	}

	if (*start) {
		while (peek_char_utf8(start) == pad)
			get_char_utf8(&start);

		cell tmp;
		check_heap_error(make_slice(q, &tmp, p1, start-src, C_STRLEN(q, p1)-(start-src)));

		if (C_STRLEN(q, p1)-(start-src)) {
			if (!in_list)
				allocate_list(q, &tmp);
			else
				append_list(q, &tmp);
		}
	}

	l = end_list(q);
	check_heap_error(l);
	return unify(q, p4, p4_ctx, l, q->st.curr_frame);
}

static bool fn_split_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);

	if (is_nil(p1) || !C_STRLEN(q, p1)) {
		cell tmp;
		make_atom(&tmp, g_nil_s);

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
			return false;

		return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	}

	const char *start = C_STR(q, p1), *ptr;
	int ch = peek_char_utf8(C_STR(q, p2));

	if ((ptr = strchr_utf8(start, ch)) != NULL) {
		cell tmp;

		if (ptr != start)
			check_heap_error(make_stringn(&tmp, start, ptr-start));
		else
			make_atom(&tmp, g_nil_s);

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame)) {
			unshare_cell(&tmp);
			return false;
		}

		unshare_cell(&tmp);
		ptr = ptr+1;

		while (isspace(*ptr))
			ptr++;

		if (*ptr)
			check_heap_error(make_stringn(&tmp, ptr, C_STRLEN(q, p1)-(ptr-start)));
		else
			make_atom(&tmp, g_nil_s);

		bool ok = unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!unify(q, p3, p3_ctx, p1, p1_ctx))
		return false;

	cell tmp;
	make_atom(&tmp, g_nil_s);
	return unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
}

static bool fn_sys_is_partial_string_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!is_iso_list(p1))
		return false;

	bool has_var, is_partial;
	scan_is_chars_list2(q, p1, p1_ctx, true, &has_var, &is_partial);
	return is_partial;
}

static bool fn_is_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	bool is_partial;
	return check_list(q, p1, p1_ctx, &is_partial, NULL);
}

static bool fn_is_partial_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_var(p1))
		return true;

	bool is_partial;

	if (check_list(q, p1, p1_ctx, &is_partial, NULL))
		return false;

	return is_partial;
}

static bool fn_is_list_or_partial_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_var(p1))
		return true;

	bool is_partial;

	if (check_list(q, p1, p1_ctx, &is_partial, NULL))
		return true;

	return is_partial;
}

static bool fn_must_be_4(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,callable);
	GET_NEXT_ARG(p4,any);

	const char *src = C_STR(q, p2);

	if (!strcmp(src, "var") && !is_var(p1))
		return throw_error2(q, p1, p1_ctx, "uninstantiation_error", "not_sufficiently_instantiated", p3);
	else if (!strcmp(src, "nonvar") && is_var(p1))
		return throw_error2(q, p1, p1_ctx, "instantiation_error", "instantiated", p3);

	if (strcmp(src, "ground") && strcmp(src, "acyclic") && is_var(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (!strcmp(src, "callable") && !is_callable(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "callable", p3);
	else if (!strcmp(src, "acyclic") && !is_acyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, p1_ctx, "domain_error", "acyclic_term");
	else if (!strcmp(src, "character") && !is_character(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "character", p3);
	else if (!strcmp(src, "chars")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
			return throw_error(q, p1, p1_ctx, "type_error", "list");

		if (has_vars(q, p1, p1_ctx))
			return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

		q->suspect = p1;

		if (!is_chars(q, p1, p1_ctx))
			return throw_error2(q, q->suspect, p1_ctx, "type_error", "character", p3);
	} else if (!strcmp(src, "boolean") && !is_boolean(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "boolean", p3);
	else if (!strcmp(src, "atom") && !is_atom(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "atom", p3);
	else if (!strcmp(src, "atomic") && !is_atomic(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "atomic", p3);
	else if (!strcmp(src, "not_less_than_zero") && !is_integer(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "integer", p3);
	else if (!strcmp(src, "not_less_than_zero") && is_negative(p1))
		return throw_error2(q, p1, p1_ctx, "domain_error", "not_less_than_zero", p3);
	else if (!strcmp(src, "integer") && !is_integer(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "integer", p3);
	else if (!strcmp(src, "float") && !is_float(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "float", p3);
	else if (!strcmp(src, "number") && !is_number(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "number", p3);
	else if (!strcmp(src, "ground")) {
		if (has_vars(q, p1, p1_ctx))
			return throw_error2(q, p1, p1_ctx, "type_error", "ground", p3);
	} else if (!strcmp(src, "compound") && !is_compound(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "compound", p3);
	else if (is_structure(p2) && (p2->arity == 1) && !strcmp(src, "list")) {
		cell *c = p2+1;
		c = deref(q, c, p2_ctx);
		pl_idx_t c_ctx = q->latest_ctx;

		if (!is_atom(c))
			return throw_error(q, c, c_ctx, "type_error", "atom");

		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL))
			return throw_error(q, p1, p1_ctx, "type_error", "list");

		cell *l = p1;
		pl_idx_t l_ctx = p1_ctx;
		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			pl_idx_t h_ctx = q->latest_ctx;
			src = C_STR(q, c);

			if (!strcmp(src, "var" ) && !is_var(h))
				return throw_error(q, h, h_ctx, "type_error", "var");
			else if (!strcmp(src, "nonvar" ) && is_var(h))
				return throw_error(q, h, h_ctx, "type_error", "nonvar");
			else if (!strcmp(src, "character" ) && !is_character(h))
				return throw_error(q, h, h_ctx, "type_error", "character");
			else if (!strcmp(src, "boolean" ) && !is_boolean(h))
				return throw_error(q, h, h_ctx, "type_error", "boolean");
			else if (!strcmp(src, "integer" ) && !is_integer(h))
				return throw_error(q, h, h_ctx, "type_error", "integer");
			else if (!strcmp(src, "float" ) && !is_float(h))
				return throw_error(q, h, h_ctx, "type_error", "float");
			else if (!strcmp(src, "number" ) && !is_number(h))
				return throw_error(q, h, h_ctx, "type_error", "number");
			else if (!strcmp(src, "not_less_than_zero" ) && !is_number(h))
				return throw_error(q, h, h_ctx, "type_error", "number");
			else if (!strcmp(src, "not_less_than_zero" ) && is_negative(h))
				return throw_error(q, h, h_ctx, "domain_error", "not_less_than_zero");
			else if (!strcmp(src, "atom" ) && !is_atom(h))
				return throw_error(q, h, h_ctx, "type_error", "atom");
			else if (!strcmp(src, "atomic" ) && !is_atomic(h))
				return throw_error(q, h, h_ctx, "type_error", "atomic");
			else if (!strcmp(src, "ground" ) && has_vars(q, h, h_ctx))
				return throw_error(q, h, h_ctx, "type_error", "ground");
			else if (!strcmp(src, "compound" ) && !is_compound(h))
				return throw_error(q, h, h_ctx, "type_error", "compound");

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}
	} else if (!strcmp(src, "list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL))
			return throw_error2(q, p1, p1_ctx, "type_error", "list", p3);
	} else if (!strcmp(src, "list_or_partial_list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
			return throw_error2(q, p1, p1_ctx, "type_error", "list", p3);
	}

	return true;
}

static bool fn_must_be_2(query *q)
{
	GET_FIRST_ARG(p2,callable);
	GET_NEXT_ARG(p1,any);

	const char *src = C_STR(q, p2);

	if (!strcmp(src, "var") && !is_var(p1))
		return throw_error(q, p1, p1_ctx, "uninstantiation_error", "not_sufficiently_instantiated");
	else if (!strcmp(src, "nonvar") && is_var(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "instantiated");

	if (strcmp(src, "ground") && strcmp(src, "acyclic") && is_var(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	if (!strcmp(src, "callable")) {
		if (!is_callable(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "callable");
	} else if (!strcmp(src, "acyclic")) {
		if (!is_acyclic_term(q, p1, p1_ctx))
			return throw_error(q, p1, p1_ctx, "domain_error", "acyclic_term");
	} else if (!strcmp(src, "character")) {
		if (!is_character(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "character");
	} else if (!strcmp(src, "chars")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
			return throw_error(q, p1, p1_ctx, "type_error", "list");

		if (has_vars(q, p1, p1_ctx))
			return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

		q->suspect = p1;

		if (!is_chars(q, p1, p1_ctx))
			return throw_error(q, q->suspect, p1_ctx, "type_error", "character");
	} else if (!strcmp(src, "boolean")) {
		if (!is_boolean(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "boolean");
	} else if (!strcmp(src, "atom")) {
		if (!is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");
	} else if (!strcmp(src, "atomic")) {
		if (!is_atomic(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atomic");
	} else if (!strcmp(src, "integer")) {
		if (!is_integer(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "integer");
	} else if (!strcmp(src, "float")) {
		if (!is_float(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "float");
	} else if (!strcmp(src, "number")) {
		if (!is_number(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "number");
	} else if (!strcmp(src, "not_less_than_zero")) {
		if (!is_number(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "number");
		if (is_negative(p1))
			return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");
	} else if (!strcmp(src, "ground")) {
		if (has_vars(q, p1, p1_ctx))
			return throw_error(q, p1, p1_ctx, "type_error", "ground");
	} else if (!strcmp(src, "compound")) {
		if (!is_compound(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "compound");
	} else if (is_structure(p2) && (p2->arity == 1) && !strcmp(src, "list")) {
		cell *c = p2+1;
		c = deref(q, c, p2_ctx);
		pl_idx_t c_ctx = q->latest_ctx;

		if (!is_atom(c))
			return throw_error(q, c, c_ctx, "type_error", "atom");

		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL))
			return throw_error(q, p1, p1_ctx, "type_error", "list");

		cell *l = p1;
		pl_idx_t l_ctx = p1_ctx;
		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			pl_idx_t h_ctx = q->latest_ctx;
			src = C_STR(q, c);

			if (!strcmp(src, "var" ) && !is_var(h))
				return throw_error(q, h, h_ctx, "type_error", "var");
			else if (!strcmp(src, "nonvar" ) && is_var(h))
				return throw_error(q, h, h_ctx, "type_error", "nonvar");
			else if (!strcmp(src, "character" ) && !is_character(h))
				return throw_error(q, h, h_ctx, "type_error", "character");
			else if (!strcmp(src, "boolean" ) && !is_boolean(h))
				return throw_error(q, h, h_ctx, "type_error", "boolean");
			else if (!strcmp(src, "integer" ) && !is_integer(h))
				return throw_error(q, h, h_ctx, "type_error", "integer");
			else if (!strcmp(src, "float" ) && !is_float(h))
				return throw_error(q, h, h_ctx, "type_error", "float");
			else if (!strcmp(src, "number" ) && !is_number(h))
				return throw_error(q, h, h_ctx, "type_error", "number");
			else if (!strcmp(src, "not_less_than_zero" ) && !is_number(h))
				return throw_error(q, h, h_ctx, "type_error", "number");
			else if (!strcmp(src, "not_less_than_zero" ) && is_negative(h))
				return throw_error(q, h, h_ctx, "domain_error", "not_less_than_zero");
			else if (!strcmp(src, "atom" ) && !is_atom(h))
				return throw_error(q, h, h_ctx, "type_error", "atom");
			else if (!strcmp(src, "atomic" ) && !is_atomic(h))
				return throw_error(q, h, h_ctx, "type_error", "atomic");
			else if (!strcmp(src, "ground" ) && has_vars(q, h, h_ctx))
				return throw_error(q, h, h_ctx, "type_error", "ground");
			else if (!strcmp(src, "compound" ) && !is_compound(h))
				return throw_error(q, h, h_ctx, "type_error", "compound");

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}
	} else if (!strcmp(src, "list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL))
			return throw_error(q, p1, p1_ctx, "type_error", "list");
	} else if (!strcmp(src, "list_or_partial_list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
			return throw_error(q, p1, p1_ctx, "type_error", "list");
	}

	return true;
}

static bool fn_can_be_4(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,callable);
	GET_NEXT_ARG(p4,any);

	if (is_var(p1))
		return true;

	const char *src = C_STR(q, p2);

	if (!strcmp(src, "callable") && !is_callable(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "callable", p3);
	else if (!strcmp(src, "character") && !is_character(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "character", p3);
	else if (!strcmp(src, "boolean") && !is_boolean(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "boolean", p3);
	else if (!strcmp(src, "atom") && !is_atom(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "atom", p3);
	else if (!strcmp(src, "atomic") && !is_atomic(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "atomic", p3);
	else if (!strcmp(src, "integer") && !is_integer(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "integer", p3);
	else if (!strcmp(src, "float") && !is_float(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "float", p3);
	else if (!strcmp(src, "number") && !is_number(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "number", p3);
	else if (!strcmp(src, "compound") && !is_compound(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "compound", p3);
	else if (!strcmp(src, "list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL))
			return throw_error2(q, p1, p1_ctx, "type_error", "list", p3);
	} else if (!strcmp(src, "list_or_partial_list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
			return throw_error2(q, p1, p1_ctx, "type_error", "list", p3);
	}

	return true;
}

static bool fn_can_be_2(query *q)
{
	GET_FIRST_ARG(p2,atom);
	GET_NEXT_ARG(p1,any);

	if (is_var(p1))
		return true;

	const char *src = C_STR(q, p2);

	if (!strcmp(src, "callable") && !is_callable(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");
	else if (!strcmp(src, "character") && !is_character(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "character");
	else if (!strcmp(src, "boolean") && !is_boolean(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "boolean");
	else if (!strcmp(src, "atom") && !is_atom(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");
	else if (!strcmp(src, "atomic") && !is_atomic(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atomic");
	else if (!strcmp(src, "integer") && !is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");
	else if (!strcmp(src, "float") && !is_float(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "float");
	else if (!strcmp(src, "number") && !is_number(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "number");
	else if (!strcmp(src, "compound") && !is_compound(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "compound");
	else if (!strcmp(src, "list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL))
			return throw_error(q, p1, p1_ctx, "type_error", "list");
	} else if (!strcmp(src, "list_or_partial_list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
			return throw_error(q, p1, p1_ctx, "type_error", "list");
	}

	return true;
}

static bool fn_sys_skip_max_list_4(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,integer_or_var);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	if (is_atomic(p3) && !is_string(p3)) {
		cell tmp;
		make_int(&tmp, 0);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame)
			&& unify(q, p3, p3_ctx, p4, p4_ctx);
	}

	pl_int_t skip=0, max = is_smallint(p2) ? get_smallint(p2) : PL_INT_MAX;
	pl_idx_t c_ctx = p3_ctx;
	cell tmp = {0};
	cell *c = skip_max_list(q, p3, &c_ctx, max, &skip, &tmp);

	if (!c) {
		c_ctx = p3_ctx;
		c = p3;
	}

	bool ok = unify(q, p4, p4_ctx, c, c_ctx);

	if (ok != true)
		return ok;

	if (!is_iso_list_or_nil(c) && !(is_cstring(c) && !strcmp(C_STR(q,c), "[]")) && !is_var(c)) {
		make_int(&tmp, -1);
		unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	make_int(&tmp, skip);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_is_stream_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_stream(p1);
}

static void push_task(module *m, query *task)
{
	task->next = m->tasks;

	if (m->tasks)
		m->tasks->prev = task;

	m->tasks = task;
}

static query *pop_task(module *m, query *task)
{
	if (task->prev)
		task->prev->next = task->next;

	if (task->next)
		task->next->prev = task->prev;

	if (task == m->tasks)
		m->tasks = task->next;

	return task->next;
}

static bool fn_wait_0(query *q)
{
	while (q->st.m->tasks) {
		CHECK_INTERRUPT();
		uint64_t now = get_time_in_usec() / 1000;
		query *task = q->st.m->tasks;
		unsigned spawn_cnt = 0;
		bool did_something = false;

		while (task) {
			CHECK_INTERRUPT();

			if (task->spawned) {
				spawn_cnt++;

				if (spawn_cnt >= g_cpu_count)
					break;
			}

			if (task->tmo_msecs) {
				if (now <= task->tmo_msecs) {
					task = task->next;
					continue;
				}

				task->tmo_msecs = 0;
			}

			if (!task->yielded || !task->st.curr_cell) {
				query *save = task;
				task = pop_task(q->st.m, task);
				destroy_query(save);
				continue;
			}

			start(task);
			task = task->next;
			did_something = true;
		}

		if (!did_something)
			msleep(1);
	}

	return true;
}

static bool fn_await_0(query *q)
{
	while (q->st.m->tasks) {
		CHECK_INTERRUPT();
		pl_uint_t now = get_time_in_usec() / 1000;
		query *task = q->st.m->tasks;
		unsigned spawn_cnt = 0;
		bool did_something = false;

		while (task) {
			CHECK_INTERRUPT();

			if (task->spawned) {
				spawn_cnt++;

				if (spawn_cnt >= g_cpu_count)
					break;
			}

			if (task->tmo_msecs) {
				if (now <= task->tmo_msecs) {
					task = task->next;
					continue;
				}

				task->tmo_msecs = 0;
			}

			if (!task->yielded || !q->st.curr_cell) {
				query *save = task;
				task = pop_task(q->st.m, task);
				destroy_query(save);
				continue;
			}

			start(task);

			if (!task->tmo_msecs && task->yielded) {
				did_something = true;
				break;
			}
		}

		if (!did_something)
			msleep(1);
		else
			break;
	}

	if (!q->st.m->tasks)
		return false;

	check_heap_error(push_choice(q));
	return true;
}

static bool fn_yield_0(query *q)
{
	if (q->retry)
		return true;

	return do_yield_0(q, 0);
}

static bool fn_task_n(query *q)
{
	pl_idx_t save_hp = q->st.hp;
	cell *p0 = deep_clone_to_heap(q, q->st.curr_cell, q->st.curr_frame);
	GET_FIRST_RAW_ARG0(p1,callable,p0);
	check_heap_error(init_tmp_heap(q));
	check_heap_error(clone_to_tmp(q, p1));
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args++ < q->st.curr_cell->arity) {
		GET_NEXT_RAW_ARG(p2,any);
		check_heap_error(append_to_tmp(q, p2));
		arity++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;
	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2, NULL)) != NULL) {
		tmp2->flags &= ~FLAG_BUILTIN;
	} else if ((tmp2->fn_ptr = get_builtin(q->pl, C_STR(q, tmp2), tmp2->arity, &found, NULL)), found) {
		tmp2->flags |= FLAG_BUILTIN;
	}

	q->st.hp = save_hp;
	cell *tmp = clone_to_heap(q, false, tmp2, 0);
	query *task = create_sub_query(q, tmp);
	task->yielded = task->spawned = true;
	push_task(q->st.m, task);
	return true;
}

static bool fn_fork_0(query *q)
{
	cell *curr_cell = q->st.curr_cell + q->st.curr_cell->nbr_cells;
	query *task = create_sub_query(q, curr_cell);
	task->yielded = true;
	push_task(q->st.m, task);
	return false;
}

static bool fn_send_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	query *dstq = q->parent ? q->parent : q;
	check_heap_error(init_tmp_heap(q));
	cell *c = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(c);

	for (pl_idx_t i = 0; i < c->nbr_cells; i++) {
		cell *c2 = c + i;
		share_cell(c2);
	}

	check_heap_error(alloc_on_queuen(dstq, 0, c));
	q->yielded = true;
	return true;
}

static bool fn_recv_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell *c = pop_queue(q);
	return unify(q, p1, p1_ctx, c, q->st.curr_frame);
}

#ifndef __wasi__
static bool fn_pid_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, getpid());
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}
#endif

static bool fn_wall_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, time(NULL));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_date_time_7(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,var);
	GET_NEXT_ARG(p5,var);
	GET_NEXT_ARG(p6,var);
	GET_NEXT_ARG(p7,var);
	struct tm tm = {0};
	time_t now = time(NULL);
	localtime_r(&now, &tm);
	cell tmp;
	make_int(&tmp, tm.tm_year+1900);
	unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mon+1);
	unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mday);
	unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_hour);
	unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_min);
	unify(q, p5, p5_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_sec);
	unify(q, p6, p6_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, 0);
	unify(q, p7, p7_ctx, &tmp, q->st.curr_frame);
	return true;
}

static bool fn_date_time_6(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,var);
	GET_NEXT_ARG(p5,var);
	GET_NEXT_ARG(p6,var);
	struct tm tm = {0};
	time_t now = time(NULL);
	localtime_r(&now, &tm);
	cell tmp;
	make_int(&tmp, tm.tm_year+1900);
	unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mon+1);
	unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_mday);
	unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_hour);
	unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_min);
	unify(q, p5, p5_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, tm.tm_sec);
	unify(q, p6, p6_ctx, &tmp, q->st.curr_frame);
	return true;
}

#ifndef __wasi__
static bool fn_shell_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	int status = system(C_STR(q, p1));
	if (status == 0)
		return true;
	else
		return false;
}

static bool fn_shell_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	int status = system(C_STR(q, p1));
	cell tmp;
	make_int(&tmp, status);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}
#endif

static bool fn_format_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,list_or_nil);

	if (is_nil(p1)) {
		if (is_nil(p2))
			return true;
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "list");
	}

	return do_format(q, NULL, 0, p1, p1_ctx, !is_nil(p2)?p2:NULL, p2_ctx);
}

static bool fn_format_3(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,list_or_nil);

	if (is_nil(p1)) {
		if (is_nil(p2))
			return true;
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "list");
	}

	return do_format(q, pstr, pstr_ctx, p1, p1_ctx, !is_nil(p2)?p2:NULL, p2_ctx);
}

#if USE_OPENSSL
static bool fn_crypto_data_hash_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p3,list_or_nil);
	bool is_sha384 = false, is_sha512 = false;
	bool is_sha256 = true;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);
		pl_idx_t h_ctx = q->latest_ctx;

		if (is_structure(h) && (h->arity == 1)) {
			cell *arg = h+1;
			arg = deref(q, arg, h_ctx);
			pl_idx_t arg_ctx = q->latest_ctx;

			if (!CMP_STR_TO_CSTR(q, h, "algorithm")) {
				if (is_var(arg)) {
					cell tmp;
					make_atom(&tmp, index_from_pool(q->pl, "sha256"));
					unify(q, arg, arg_ctx, &tmp, q->st.curr_frame);
					is_sha384 = is_sha512 = false;
					is_sha256 = true;
				} else if (!CMP_STR_TO_CSTR(q, arg, "sha256")) {
					is_sha384 = is_sha512 = false;
					is_sha256 = true;
				} else if (!CMP_STR_TO_CSTR(q, arg, "sha384")) {
					is_sha256 = is_sha512 = false;
					is_sha384 = true;
				} else if (!CMP_STR_TO_CSTR(q, arg, "sha512")) {
					is_sha384 = is_sha256 = false;
					is_sha512 = true;
				} else
					return throw_error(q, arg, arg_ctx, "domain_error", "algorithm");
			} else
				return throw_error(q, h, h_ctx, "domain_error", "hash_option");
		} else
			return throw_error(q, h, h_ctx, "domain_error", "hash_option");

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	char tmpbuf[512];
	char *dst = tmpbuf;
	*dst = '\0';
	size_t buflen = sizeof(tmpbuf);

	if (is_sha256) {
		unsigned char digest[SHA256_DIGEST_LENGTH];
		SHA256((unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest);

		for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (is_sha384) {
		unsigned char digest[SHA384_DIGEST_LENGTH];
		SHA384((unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest);

		for (int i = 0; i < SHA384_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (is_sha512) {
		unsigned char digest[SHA512_DIGEST_LENGTH];
		SHA512((unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest);

		for (int i = 0; i < SHA512_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	}

	cell tmp;
	check_heap_error(make_string(&tmp, tmpbuf));
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}
#endif

static int do_b64encode_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	const char *str = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);
	char *dstbuf = malloc((len*3)+1);	// BASE64 can increase length x3
	check_heap_error(dstbuf);
	b64_encode(str, len, &dstbuf, 0, 0);
	cell tmp;
	check_heap_error(make_string(&tmp, dstbuf), free(dstbuf));
	free(dstbuf);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static int do_b64decode_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,atom);
	const char *str = C_STR(q, p2);
	size_t len = C_STRLEN(q, p2);
	char *dstbuf = malloc(len+1);
	check_heap_error(dstbuf);
	b64_decode(str, len, &dstbuf);
	cell tmp;
	check_heap_error(make_string(&tmp, dstbuf), free(dstbuf));
	free(dstbuf);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_base64_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_list(p1)) && is_var(p2))
		return do_b64encode_2(q);
	else if (is_var(p1) && (is_atom(p2) || is_string(p2)))
		return do_b64decode_2(q);

	return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");
}

static char *url_encode(const char *src, int len, char *dstbuf)
{
	char *dst = dstbuf;

	// As per RFC3986 (2005)

	while (len-- > 0) {
		if (!isalnum(*src) && (*src != '-') && (*src != '_') && (*src != '.') && (*src != '~'))
			dst += sprintf(dst, "%%%02X", *src++);
		else
			*dst++ = *src++;
	}

	*dst = '\0';
	return dstbuf;
}

char *url_decode(const char *src, char *dstbuf)
{
	char *dst = dstbuf;

	while (*src) {
		if (*src == '%') {
			src++;
			unsigned ch = 0;
			sscanf(src, "%02X", &ch);
			src += 2;
			*dst++ = (unsigned char)ch;
		} else if (*src == '+') {
			*dst++ = ' ';
			src++;
		} else
			*dst++ = *src++;
	}

	*dst = '\0';
	return dstbuf;
}

static bool do_urlencode_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	const char *str = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);
	char *dstbuf = malloc((len*3)+1);	// URL's can increase length x3
	check_heap_error(dstbuf);
	url_encode(str, len, dstbuf);
	cell tmp;

	if (is_string(p1))
		check_heap_error(make_string(&tmp, dstbuf), free(dstbuf));
	else
		check_heap_error(make_cstring(&tmp, dstbuf), free(dstbuf));

	free(dstbuf);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool do_urldecode_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,atom);
	const char *str = C_STR(q, p2);
	size_t len = C_STRLEN(q, p2);
	char *dstbuf = malloc(len+1);
	check_heap_error(dstbuf);
	url_decode(str, dstbuf);
	cell tmp;

	if (is_string(p1))
		check_heap_error(make_string(&tmp, dstbuf), free(dstbuf));
	else
		check_heap_error(make_cstring(&tmp, dstbuf), free(dstbuf));

	free(dstbuf);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_urlenc_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_string(p1)) && is_var(p2))
		return do_urlencode_2(q);
	else if (is_var(p1) && (is_atom(p2) || is_string(p2)))
		return do_urldecode_2(q);

	return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");
}

static bool fn_atom_lower_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom);
	GET_NEXT_ARG(p2,iso_atom_or_var);
	const char *src = C_STR(q, p1);
	size_t len = substrlen_utf8(src, C_STRLEN(q, p1));
	char *tmps = malloc((len*MAX_BYTES_PER_CODEPOINT)+1);
	check_heap_error(tmps);
	char *dst = tmps;

	while (len--) {
		int ch = get_char_utf8(&src);
		ch = towlower(ch);
		dst += put_char_utf8(dst, ch);
	}

	*dst = '\0';
	cell tmp;
	check_heap_error(make_cstringn(&tmp, tmps, C_STRLEN(q, p1)), free(tmps));
	free(tmps);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_atom_upper_2(query *q)
{
	GET_FIRST_ARG(p1,iso_atom);
	GET_NEXT_ARG(p2,iso_atom_or_var);
	const char *src = C_STR(q, p1);
	size_t len = substrlen_utf8(src, C_STRLEN(q, p1));
	char *tmps = malloc((len*MAX_BYTES_PER_CODEPOINT)+1);
	check_heap_error(tmps);
	char *dst = tmps;

	while (len--) {
		int ch = get_char_utf8(&src);
		ch = towupper(ch);
		dst += put_char_utf8(dst, ch);
	}

	*dst = '\0';
	cell tmp;
	check_heap_error(make_cstringn(&tmp, tmps, C_STRLEN(q, p1)), free(tmps));
	free(tmps);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_string_lower_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *src = C_STR(q, p1);
	size_t len = substrlen_utf8(src, C_STRLEN(q, p1));
	char *tmps = malloc((len*MAX_BYTES_PER_CODEPOINT)+1);
	check_heap_error(tmps);
	char *dst = tmps;

	while (len--) {
		int ch = get_char_utf8(&src);
		ch = towlower(ch);
		dst += put_char_utf8(dst, ch);
	}

	*dst = '\0';
	cell tmp;
	check_heap_error(make_stringn(&tmp, tmps, C_STRLEN(q, p1)), free(tmps));
	free(tmps);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_string_upper_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *src = C_STR(q, p1);
	size_t len = substrlen_utf8(src, C_STRLEN(q, p1));
	char *tmps = malloc((len*MAX_BYTES_PER_CODEPOINT)+1);
	check_heap_error(tmps);
	char *dst = tmps;

	while (len--) {
		int ch = get_char_utf8(&src);
		ch = towupper(ch);
		dst += put_char_utf8(dst, ch);
	}

	*dst = '\0';
	cell tmp;
	check_heap_error(make_stringn(&tmp, tmps, C_STRLEN(q, p1)), free(tmps));
	free(tmps);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static pl_idx_t jenkins_one_at_a_time_hash(const char *key, size_t len)
{
	pl_idx_t hash = 0;

	while (len-- > 0) {
		hash += *key++;
		hash += (hash << 10);
		hash ^= (hash >> 6);
	}

	hash += (hash << 3);
	hash ^= (hash >> 11);
	hash += (hash << 15);
	return hash;
}

static bool fn_term_hash_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	if (is_var(p1))
		return true;

	cell tmp;

	if (is_smallint(p1)) {
		char tmpbuf[256];
		snprintf(tmpbuf, sizeof(tmpbuf), "%"PRId64"", (int64_t)get_smallint(p1));
		make_int(&tmp, jenkins_one_at_a_time_hash(tmpbuf, strlen(tmpbuf)));
	} else if (is_atom(p1)) {
		make_int(&tmp, jenkins_one_at_a_time_hash(C_STR(q, p1), C_STRLEN(q, p1)));
	} else {
		char *tmpbuf = print_term_to_strbuf(q, p1, p1_ctx, 1);
		make_int(&tmp, jenkins_one_at_a_time_hash(tmpbuf, strlen(tmpbuf)));
		free(tmpbuf);
	}

	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool fn_hex_chars_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");
	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");

	if (is_var(p2)) {
		char tmpbuf[256];
		char *dst = tmpbuf;

		if (is_bigint(p1)) {
			size_t len = mp_int_string_len(&p1->val_bigint->ival, 16) -1;
			dst = malloc(len+10);
			mp_int_to_string(&p1->val_bigint->ival, 16, dst, len+1);
		} else {
			snprintf(tmpbuf, sizeof(tmpbuf), "%"PRIx64"", (uint64_t)get_smallint(p1));
		}

		cell tmp;
		check_heap_error(make_string(&tmp, dst));
		if (dst != tmpbuf) free(dst);
		bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	char *src = DUP_STR(q, p2);
	const char *s = src;
	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;
	read_integer(q->p, &v2, 16, s, &s);
	free(src);
	cell tmp = {0};

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		tmp.tag = TAG_INTEGER;
		tmp.val_bigint = malloc(sizeof(bigint));
		tmp.val_bigint->refcnt = 1;
		mp_int_init_copy(&tmp.val_bigint->ival, &v2);
		tmp.flags |= FLAG_MANAGED;
	} else {
		make_int(&tmp, val);
	}

	mp_int_clear(&v2);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_octal_chars_2(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");

	if (is_var(p2)) {
		char tmpbuf[256];
		char *dst = tmpbuf;

		if (is_bigint(p1)) {
			size_t len = mp_int_string_len(&p1->val_bigint->ival, 8) -1;
			dst = malloc(len+10);
			mp_int_to_string(&p1->val_bigint->ival, 8, dst, len+1);
		} else {
			snprintf(tmpbuf, sizeof(tmpbuf), "%"PRIo64"", (uint64_t)get_smallint(p1));
		}

		cell tmp;
		check_heap_error(make_string(&tmp, dst));
		if (dst != tmpbuf) free(dst);
		bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	char *src = DUP_STR(q, p2);
	const char *s = src;
	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;
	read_integer(q->p, &v2, 16, s, &s);
	free(src);
	cell tmp = {0};

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		tmp.tag = TAG_INTEGER;
		tmp.val_bigint = malloc(sizeof(bigint));
		tmp.val_bigint->refcnt = 1;
		mp_int_init_copy(&tmp.val_bigint->ival, &v2);
		tmp.flags |= FLAG_MANAGED;
	} else {
		make_int(&tmp, val);
	}

	mp_int_clear(&v2);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_string(p1);
}

static bool fn_string_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_cstring(p1) || is_nil(p1) ||
		(scan_is_chars_list(q, p1, p1_ctx, true) > 0);
}

static bool fn_getenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *value = getenv(C_STR(q, p1));

	if (!value)
		return false;

	cell tmp;

	if (is_string(p1))
		check_heap_error(make_string(&tmp, value));
	else
		check_heap_error(make_cstring(&tmp, value));

	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_setenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_int);

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	if (is_atom(p2)) {
		setenv(C_STR(q, p1), C_STR(q, p2), 1);
	} else if (is_integer(p2)) {
		char tmpbuf[256];
		sprint_int(tmpbuf, sizeof(tmpbuf), get_smallint(p2), 10);
		setenv(C_STR(q, p1), tmpbuf, 1);
	} else
		return false;

	return true;
}

static bool fn_unsetenv_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	unsetenv(C_STR(q, p1));
	return true;
}

static bool fn_uuid_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	uuid u;
	uuid_gen(q->pl, &u);
	char tmpbuf[128];
	uuid_to_buf(&u, tmpbuf, sizeof(tmpbuf));
	cell tmp;
	check_heap_error(make_string(&tmp, tmpbuf));
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_atomic_concat_3(query *q)
{
	GET_FIRST_ARG(p1,atomic);
	GET_NEXT_ARG(p2,atomic);
	GET_NEXT_ARG(p3,atom_or_var);
	const char *src1, *src2;
	size_t len1, len2;
	char tmpbuf1[256], tmpbuf2[256];
	len1 = print_term_to_buf(q, tmpbuf1, sizeof(tmpbuf1), p1, p1_ctx, 1, false, 0);
	src1 = tmpbuf1;
	len2 = print_term_to_buf(q, tmpbuf2, sizeof(tmpbuf2), p2, p2_ctx, 1, false, 0);
	src2 = tmpbuf2;
	SB_alloc(pr, len1+len2);
	SB_strcatn(pr, src1, len1);
	SB_strcatn(pr, src2, len2);
	cell tmp;
	check_heap_error(make_cstringn(&tmp, SB_cstr(pr), SB_strlen(pr)), SB_free(pr));
	SB_free(pr);
	bool ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_atomic_list_concat_3(query *q)
{
	GET_FIRST_ARG(p1,iso_list_or_nil);
	GET_NEXT_ARG(p2,atomic);
	GET_NEXT_ARG(p3,atom_or_var);
	LIST_HANDLER(p1);
	SB_alloc(pr,256);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (is_var(h))
			return throw_error(q, h, q->latest_ctx, "instantiation_error", "atomic");

		if (!is_atomic(h))
			return throw_error(q, h, q->latest_ctx, "type_error", "atomic");

		q->parens = true;
		char *dst = print_term_to_strbuf(q, h, q->latest_ctx, 1);
		q->parens = false;
		SB_strcat(pr, dst);
		free(dst);

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;

		if (is_list(p1)) {
			q->parens = true;
			dst = print_term_to_strbuf(q, p2, p2_ctx, 1);
			q->parens = false;
			SB_strcat(pr, dst);
			free(dst);
		}
	}

	if (is_var(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "atomic_list_concat/3");

	cell tmp;
	check_heap_error(make_cstring(&tmp, SB_cstr(pr)), SB_free(pr));
	SB_free(pr);
	bool ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_replace_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	GET_NEXT_ARG(p4,var);
	size_t srclen = C_STRLEN(q, p1);
	size_t dstlen = srclen * C_STRLEN(q, p3);
	const char *src = C_STR(q, p1);
	const char *s1 = C_STR(q, p2);
	const char *s2 = C_STR(q, p3);
	size_t s1len = C_STRLEN(q, p2);
	size_t s2len = C_STRLEN(q, p3);
	SB_alloc(pr, dstlen);

	while (srclen > 0) {
		if (!strncmp(src, s1, s1len)) {
			SB_strcatn(pr, s2, s2len);
			src += s1len;
			srclen -= s1len;
		} else {
			SB_strcatn(pr, src, 1);
			src++;
			srclen--;
		}
	}

	cell tmp;

	if (SB_strlen(pr))
		check_heap_error(make_stringn(&tmp, SB_cstr(pr), SB_strlen(pr)), SB_free(pr));
	else
		make_atom(&tmp, g_nil_s);

	SB_free(pr);
	bool ok = unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static void load_properties(module *m);

static bool fn_sys_load_properties_0(query *q)
{
	load_properties(q->st.m);
	return true;
}

static void load_flags(query *q);

static bool fn_sys_load_flags_0(query *q)
{
	load_flags(q);
	return true;
}

static void load_ops(query *q);

static bool fn_sys_load_ops_0(query *q)
{
	load_ops(q);
	return true;
}

static bool fn_sys_legacy_predicate_property_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,atom_or_var);
	cell tmp;
	bool found = false;

	if (get_builtin(q->pl, C_STR(q, p1), p1->arity, &found, NULL), found) {
		make_atom(&tmp, index_from_pool(q->pl, "built_in"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "predicate_property");
	}

	predicate *pr = find_predicate(q->st.m, p1);

	if (pr && !pr->is_dynamic && !is_var(p2)) {
		make_atom(&tmp, index_from_pool(q->pl, "built_in"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr && pr->is_multifile) {
		make_atom(&tmp, index_from_pool(q->pl, "multifile"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr && pr->is_dynamic) {
		make_atom(&tmp, index_from_pool(q->pl, "dynamic"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr && !pr->is_dynamic) {
		make_atom(&tmp, index_from_pool(q->pl, "static"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr && pr->is_tabled) {
		make_atom(&tmp, index_from_pool(q->pl, "tabled"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr && pr->is_public) {
		make_atom(&tmp, index_from_pool(q->pl, "public"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr && pr->is_public) {
		make_atom(&tmp, index_from_pool(q->pl, "exported"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr) {
		make_atom(&tmp, index_from_pool(q->pl, "static"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr) {
		make_atom(&tmp, index_from_pool(q->pl, "meta_predicate"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	if (pr) {
		make_atom(&tmp, index_from_pool(q->pl, "visible"));
		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame) == true)
			return true;
	}

	return false;
}

static bool fn_char_type_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_int);
	GET_NEXT_ARG(p2,atom);
	int ch;

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_atom(p1)) {
		if (strlen_utf8(C_STR(q, p1)) != 1)
			return false;

		ch = peek_char_utf8(C_STR(q, p1));
	} else
		ch = get_smallint(p1);

	if (!CMP_STR_TO_CSTR(q, p2, "alpha"))
		return iswalpha(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "digit"))
		return iswdigit(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "xdigit"))
		return iswxdigit(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "whitespace"))
		return iswblank(ch) || iswspace(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "white"))
		return iswblank(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "space"))
		return iswspace(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "lower"))
		return iswlower(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "upper"))
		return iswupper(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "punct"))
		return iswpunct(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "cntrl"))
		return iswcntrl(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "graph"))
		return iswgraph(ch);
	else if (!CMP_STR_TO_CSTR(q, p2, "ascii"))
		return ch < 128;
	else if (!CMP_STR_TO_CSTR(q, p2, "newline"))
		return ch == 10;
	else if (!CMP_STR_TO_CSTR(q, p2, "end_of_line"))
		return (ch >= 10) && (ch <= 13);
	else if (!CMP_STR_TO_CSTR(q, p2, "end_of_file"))
		return ch == -1;
	else if (!CMP_STR_TO_CSTR(q, p2, "quote"))
		return (ch == '\'') || (ch == '"') || (ch == '`');
	else if (!CMP_STR_TO_CSTR(q, p2, "period"))
		return (ch == '.') || (ch == '!') || (ch == '?');

	return false;
}

static bool fn_abolish_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer);

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	cell tmp = *p1;
	tmp.arity = get_smallint(p2);
	CLR_OP(&tmp);
	return do_abolish(q, &tmp, &tmp, true);
}

static bool fn_sys_lt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	pl_int_t num = get_smallint(p1);

	if (num < get_smallint(p2)) {
		set_smallint(p1, num+1);
		return true;
	}

	drop_choice(q);
	return true;
}

static bool fn_limit_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	cell *tmp = clone_to_heap(q, true, p2, 4);
	pl_idx_t nbr_cells = 1 + p2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_fail_s, fn_sys_lt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_smallint(p1));
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return true;
}

static bool fn_sys_gt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	pl_int_t num = get_smallint(p1);

	if (num <= get_smallint(p2)) {
		set_smallint(p1, num+1);
		return false;
	}

	return true;
}

static bool fn_offset_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	cell *tmp = clone_to_heap(q, true, p2, 4);
	pl_idx_t nbr_cells = 1 + p2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_fail_s, fn_sys_gt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_smallint(p1));
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	return true;
}

static bool fn_sys_ne_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	pl_int_t num = get_smallint(p1);

	if (num != get_smallint(p2)) {
		set_smallint(p1, num+1);
		return false;
	}

	drop_choice(q);
	return true;
}

static bool fn_sys_incr_2(query *q)
{
	GET_FIRST_ARG(p1, integer_or_var);
	GET_NEXT_ARG(p2, integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	pl_int_t n = get_smallint(p2);
	n++;
	set_smallint(p2, n);

	if (is_integer(p1))
		return get_smallint(p1) == n;

	return unify(q, p1, p1_ctx, p2, q->st.curr_frame);
}

static bool fn_call_nth_2(query *q)
{
	if (q->retry)
		return false;

	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	if (is_integer(p2) && is_zero(p2))
		return false;

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	if (is_var(p2)) {
		cell *tmp = clone_to_heap(q, true, p1, 4);
		pl_idx_t nbr_cells = 1 + p1->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_incr_s, fn_sys_incr_2, 2, 2);
		GET_RAW_ARG(2,p2_raw);
		tmp[nbr_cells] = *p2_raw;
		tmp[nbr_cells++].nbr_cells = 1;
		make_int(tmp+nbr_cells++, 0);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_call_barrier(q));
		q->st.curr_cell = tmp;
		return true;
	}

	cell *tmp = clone_to_heap(q, true, p1, 4);
	pl_idx_t nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_ne_s, fn_sys_ne_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_smallint(p2));
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_call_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

static bool fn_sys_lengthchk_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_string(p1)) {
		cell tmp;
		make_int(&tmp, C_STRLEN(q, p1));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	LIST_HANDLER(p1);
	pl_int_t n = 0;

	while (is_iso_list(p1)) {
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
		n++;
	}

	cell tmp;
	make_int(&tmp, n);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool fn_sys_unifiable_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,list_or_nil_or_var);
	check_heap_error(push_choice(q));
	pl_idx_t before_hook_tp = q->st.tp;
	bool save_hook = q->in_hook;
	q->in_hook = true;

	if (!unify(q, p1, p1_ctx, p2, p2_ctx) && !q->cycle_error) {
		q->in_hook = save_hook;
		undo_me(q);
		drop_choice(q);
		return false;
	}

	q->in_hook = save_hook;
	bool first = true;

	// Go thru trail, getting the bindings...

	while (before_hook_tp < q->st.tp) {
		const trail *tr = q->trails + before_hook_tp;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		cell *c = deref(q, &e->c, e->c.var_ctx);

		if (is_indirect(c))
			c = c->val_ptr;

		cell *tmp = malloc(sizeof(cell)*(2+c->nbr_cells));
		check_heap_error(tmp);
		make_struct(tmp, g_unify_s, fn_iso_unify_2, 2, 1+c->nbr_cells);
		SET_OP(tmp, OP_XFX);
		cell v;
		make_var(&v, g_anon_s, tr->var_nbr);
		tmp[1] = v;
		safe_copy_cells(tmp+2, c, c->nbr_cells);

		if (first) {
			allocate_list(q, tmp);
			first = false;
		} else
			append_list(q, tmp);

		free(tmp);
		before_hook_tp++;
	}

	undo_me(q);
	drop_choice(q);

	cell *l = end_list(q);
	return unify(q, p3, p3_ctx, l, q->st.curr_frame);
}

static bool fn_sys_list_attributed_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	parser *p = q->p;
	frame *f = GET_FIRST_FRAME();
	bool first = true;

	for (unsigned i = 0; i < p->nbr_vars; i++) {
		if (!strcmp(p->vartab.var_name[i], "_"))
			continue;

		slot *e = GET_SLOT(f, i);

		if (!is_empty(&e->c))
			continue;

		if (!e->c.attrs || is_nil(e->c.attrs))
			continue;

		cell v;
		make_var(&v, index_from_pool(q->pl, p->vartab.var_name[i]), i);

		if (first) {
			allocate_list(q, &v);
			first = false;
		} else
			append_list(q, &v);
	}

	if (first) {
		cell tmp;
		make_atom(&tmp, g_nil_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	cell *l = end_list(q);
	return unify(q, p1, p1_ctx, l, 0);
}

static bool fn_sys_erase_attributes_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);
	e->c.attrs = NULL;
	return true;
}

static bool fn_sys_put_attributes_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,list_or_nil);
	frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);
	add_trail(q, p1_ctx, p1->var_nbr, e->c.attrs, e->c.attrs_ctx);
	//DUMP_TERM("$put_attr", p2, p2_ctx);
	e->c.attrs = p2;
	e->c.attrs_ctx = p2_ctx;
	return true;
}

static bool fn_sys_get_attributes_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	frame *f = GET_FRAME(p1_ctx);
	slot *e = GET_SLOT(f, p1->var_nbr);

	if (!e->c.attrs || is_nil(e->c.attrs))
		return false;

	return unify(q, p2, p2_ctx, e->c.attrs, e->c.attrs_ctx);
}

static bool fn_get_unbuffered_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	int ch = history_getch_fd(fileno(str->fp));

	if (ch == 4)
		ch = -1;

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_get_unbuffered_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	int ch = history_getch_fd(fileno(str->fp));

	if (ch == 4)
		ch = -1;

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield_0(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if ((ch == '\n') || (ch == EOF))
		str->did_getc = false;

	if (ch == -1) {
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_kv_set_3(query *q)
{
	GET_FIRST_ARG(p1,smallint_or_atom);
	GET_NEXT_ARG(p2,smallint_or_atom);
	GET_NEXT_ARG(p3,list_or_nil);
	bool do_create = false;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);

		if (is_var(h))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "read_option");

		if (is_structure(h) && (h->arity == 1)) {
			cell *n = h + 0;
			if (!CMP_STR_TO_CSTR(q, n, "create")) {
				cell *v = n + 1;
				v = deref(q, v, q->latest_ctx);

				if (is_var(v))
					return throw_error(q, p3, p3_ctx, "instantiation_error", "read_option");

				if (is_atom(v) && !CMP_STR_TO_CSTR(q, v, "true"))
					do_create = true;
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	char *key;

	if (is_integer(p1)) {
		char tmpbuf[128];
		snprintf(tmpbuf, sizeof(tmpbuf), "%"PRId64"", (int64_t)get_smallint(p1));
		key = strdup(tmpbuf);
	} else if (is_atom(p1))
		key = DUP_STR(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	check_heap_error(key);

	if (do_create) {
		if (map_get(q->pl->keyval, key, NULL)) {
			free(key);
			return false;
		}
	}

	char *val;

	if (is_integer(p2)) {
		char tmpbuf[128];
		snprintf(tmpbuf, sizeof(tmpbuf), "%"PRId64"", (int64_t)get_smallint(p2));
		val = strdup(tmpbuf);
	} else if (is_atom(p2))
		val = DUP_STR(q, p2);
	else {
		free(key);
		return throw_error(q, p2, p2_ctx, "type_error", "integer");
	}

	check_heap_error(val);
	map_set(q->pl->keyval, key, val);
	return true;
}

static bool fn_kv_get_3(query *q)
{
	GET_FIRST_ARG(p1,atomic);
	GET_NEXT_ARG(p2,atomic_or_var);
	GET_NEXT_ARG(p3,list_or_nil);
	bool do_delete = false;
	LIST_HANDLER(p3);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	while (is_list(p3)) {
		CHECK_INTERRUPT();
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);

		if (is_var(h))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "read_option");

		if (is_structure(h) && (h->arity == 1)) {
			cell *n = h + 0;
			if (!CMP_STR_TO_CSTR(q, n, "delete")) {
				cell *v = n + 1;
				v = deref(q, v, q->latest_ctx);

				if (is_var(v))
					return throw_error(q, p3, p3_ctx, "instantiation_error", "read_option");

				if (is_atom(v) && !CMP_STR_TO_CSTR(q, v, "true"))
					do_delete = true;
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	char *key;
	char tmpbuf[128];

	if (is_integer(p1)) {
		snprintf(tmpbuf, sizeof(tmpbuf), "%"PRId64"", (int64_t)get_smallint(p1));
		key = tmpbuf;
	} else if (is_atom(p1))
		key = DUP_STR(q, p1);
	else
		return throw_error(q, p2, p2_ctx, "type_error", "integer");

	check_heap_error(key);
	char *val = NULL;

	if (!map_get(q->pl->keyval, key, (void*)&val)) {
		if (key != tmpbuf) free(key);
		return false;
	}

	cell tmp;
	const char *src = val;
	int all_digs = 1;

	while (*src) {
		if (!isdigit(*src)) {
			all_digs = 0;
			break;
		}

		src++;
	}

	if (all_digs) {
		pl_int_t v = strtoll(val, NULL, 10);
		make_int(&tmp, v);
	} else
		check_heap_error(make_cstring(&tmp, val));

	if (do_delete)
		map_del(q->pl->keyval, key);

	if (key != tmpbuf) free(key);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_current_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);

	if (!q->retry) {
		if (is_atom(p1)) {
			const char *name = C_STR(q, p1);
			return find_module(q->pl, name) ? true : false;
		}

		check_heap_error(push_choice(q));
		module *m = q->current_m = q->pl->modules;
		cell tmp;
		make_atom(&tmp, index_from_pool(q->pl, m->name));
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (!q->current_m)
		return false;

	module *m = q->current_m = q->current_m->next;

	if (!m)
		return false;

	check_heap_error(push_choice(q));
	cell tmp;
	make_atom(&tmp, index_from_pool(q->pl, m->name));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_use_module_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	if (!is_atom(p1) && !is_structure(p1)) return false;
	const char *name = C_STR(q, p1);
	char dstbuf[1024*4];

	if (is_structure(p1) && !strcmp(name, "library")) {
		p1 = p1 + 1;
		if (!is_interned(p1)) return false;
		name = C_STR(q, p1);
		module *m;

		if ((m = find_module(q->pl, name)) != NULL) {
			if (m != q->st.m)
				q->st.m->used[q->st.m->idx_used++] = m;

			return true;
		}

		if (!strcmp(name, "between")
		    || !strcmp(name, "samsort")
		    || !strcmp(name, "terms")
		    || !strcmp(name, "types")
			|| !strcmp(name, "iso_ext")
		    || !strcmp(name, "files"))
			return true;

		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, name))
				continue;

			char *src = malloc(*lib->len+1);
			check_heap_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			SB(s1);
			SB_sprintf(s1, "library%c%s", PATH_SEP_CHAR, lib->name);
			m = load_text(q->st.m, src, SB_cstr(s1));
			SB_free(s1);
			free(src);

			if (m != q->st.m)
				q->st.m->used[q->st.m->idx_used++] = m;

			return true;
		}

		snprintf(dstbuf, sizeof(dstbuf), "%s%c", g_tpl_lib, PATH_SEP_CHAR);
		char *dst = dstbuf + strlen(dstbuf);
		pl_idx_t ctx = 0;
		print_term_to_buf(q, dst, sizeof(dstbuf)-strlen(g_tpl_lib), p1, ctx, 1, 0, 0);
		name = dstbuf;
	}

	if (true) {
		module *m;

		if ((m = find_module(q->pl, name)) != NULL) {
			if (m != q->st.m)
				q->st.m->used[q->st.m->idx_used++] = m;

			return true;
		}

		if (!strcmp(name, "between")
		    || !strcmp(name, "samsort")
		    || !strcmp(name, "terms")
		    || !strcmp(name, "types")
			|| !strcmp(name, "iso_ext")
		    || !strcmp(name, "files"))
			return true;

		for (library *lib = g_libs; lib->name; lib++) {
			if (strcmp(lib->name, name))
				continue;

			char *src = malloc(*lib->len+1);
			check_heap_error(src);
			memcpy(src, lib->start, *lib->len);
			src[*lib->len] = '\0';
			SB(s1);
			SB_sprintf(s1, "library/%s", lib->name);
			m = load_text(q->st.m, src, SB_cstr(s1));
			SB_free(s1);
			free(src);

			if (m != q->st.m)
				q->st.m->used[q->st.m->idx_used++] = m;

			return true;
		}
	}

	char *filename = relative_to(q->st.m->filename, name);
	module *m;

	if (!(m = load_file(q->st.m, filename, false))) {
		fprintf(stdout, "Error: module file not found: %s\n", filename);
		free(filename);
		return false;
	}

	free(filename);

	if (m != q->st.m)
		q->st.m->used[q->st.m->idx_used++] = m;

	return true;
}

static bool fn_use_module_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	return fn_use_module_1(q);
}

static bool fn_attribute_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,integer);
	module *m = q->pl->modules;
	const char *name = C_STR(q, p2);
	unsigned arity = get_smallint(p3);

	while (m) {
		if ((arity == m->arity) && !strcmp(name, m->name)) {
			cell tmp;
			make_atom(&tmp, index_from_pool(q->pl, m->orig->name));
			return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		}

		m = m->next;
	}

	cell tmp;
	make_atom(&tmp, index_from_pool(q->pl, q->st.m->name));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_prolog_load_context_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);

	if (CMP_STR_TO_CSTR(q, p1, "module"))
		return false;

	cell tmp;
	make_atom(&tmp, index_from_pool(q->pl, q->st.prev_m->name));
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool fn_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);

	if (is_var(p1)) {
		cell tmp;
		make_atom(&tmp, index_from_pool(q->pl, q->st.m->name));
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	const char *name = C_STR(q, p1);
	module *m = find_module(q->pl, name);

	if (!m) {
		if (q->p->command)
			fprintf(stdout, "Info: created module '%s'\n", name);

		m = create_module(q->pl, name);
	}

	q->st.m = m;
	return true;
}

static bool fn_using_0(query *q)
{
	module *m = q->st.m;
	fprintf(stdout, "%% %s --> [", m->name);

	for (unsigned i = 0; i < m->idx_used; i++) {
		if (i) fprintf(stdout, "%s", ",");
		fprintf(stdout, "%s", m->used[i]->name);
	}

	fprintf(stdout, "].\n");
	return true;
}

static bool fn_sys_register_term_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	check_heap_error(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->register_term = true;
	return true;
}

static bool fn_sys_alarm_1(query *q)
{
#if defined(_WIN32) || !defined(ITIMER_REAL)
	return false;
#else
	GET_FIRST_ARG(p1,number);
	int time0 = 0;

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "positive_integer");

	if (is_float(p1))
		time0 = get_float(p1) * 1000;
	else
		time0 = get_smallint(p1);

	if (time0 < 0)
		return throw_error(q, p1, p1_ctx, "domain_error", "positive_integer");

	struct itimerval it;
	memset(&it, 0, sizeof(struct itimerval));

	if (time0 == 0) {
		setitimer(ITIMER_REAL, &it, NULL);
		return true;
	}

	int ms = time0;
	int secs = ms / 1000;
	ms -= secs * 1000;

	it.it_value.tv_sec = secs;
	it.it_value.tv_usec = ms * 1000;
	setitimer(ITIMER_REAL, &it, NULL);
	return true;
#endif
}

static bool fn_sys_register_cleanup_1(query *q)
{
	if (q->retry) {
		GET_FIRST_ARG(p1,callable);
		cell *tmp = clone_to_heap(q, true, p1, 3);
		pl_idx_t nbr_cells = 1 + p1->nbr_cells;
		make_struct(tmp+nbr_cells++, g_cut_s, fn_sys_inner_cut_0, 0, 0);
		make_struct(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
		make_call(q, tmp+nbr_cells);
		q->st.curr_cell = tmp;
		return true;
	}

	check_heap_error(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->register_cleanup = true;
	return true;
}

static bool fn_sys_get_level_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, q->cp);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_abort_0(query *q)
{
	exit(0);
	return false;
}

static bool fn_sys_choice_0(query *q)
{
	if (q->retry)
		return false;

	check_heap_error(push_choice(q));
	return true;
}

static bool fn_iso_compare_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_atom(p1)) {
		if (CMP_STR_TO_CSTR(q, p1, "<")
			&& CMP_STR_TO_CSTR(q, p1, ">")
			&& CMP_STR_TO_CSTR(q, p1, "="))
			return throw_error(q, p1, p1_ctx, "domain_error", "order");
	}

	int status = compare(q, p2, p2_ctx, p3, p3_ctx);
	cell tmp;

	make_atom(&tmp, (status == 0)?g_eq_s:status<0?g_lt_s:g_gt_s);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

#ifdef __wasi__
static bool fn_sys_host_call_2(query *q) {
#ifdef WASI_IMPORTS
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,var);

	int32_t status = 0;
	char *reply = {0};
	size_t reply_len;
	size_t len;
	if (is_cstring(p1)) {
		const char *src = C_STR(q, p1);
		len = C_STRLEN(q, p1);
		status = host_call((int32_t)q, src, len, &reply, &reply_len);
	} else if ((len = scan_is_chars_list(q, p1, p1_ctx, true)) > 0) {
		char *src = chars_list_to_string(q, p1, p1_ctx, len);
		status = host_call((int32_t)q, src, len, &reply, &reply_len);
		free(src);
	} else if (is_nil(p1)) {
		;
	} else
		return throw_error(q, p1, p1_ctx, "type_error", "chars");

	switch (status) {
		case WASM_HOST_CALL_ERROR:
			return throw_error(q, p1, p1_ctx, "system_error", "wasm_host_call_failed");
		case WASM_HOST_CALL_YIELD:
			return false;
	}

	cell tmp;
	check_heap_error(make_stringn(&tmp, reply, reply_len), free(reply));
	free(reply);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
#else
	return false;
#endif
}

static bool fn_sys_host_resume_1(query *q) {
#ifdef WASI_IMPORTS
	GET_FIRST_ARG(p1,var);

	char *reply = {0};
	size_t reply_len;
	bool ok = host_resume((int32_t)q, &reply, &reply_len);
	if (!ok) {
		return false;
	}

	cell tmp;
	check_heap_error(make_stringn(&tmp, reply, reply_len), free(reply));
	free(reply);
	ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
#else
	return false;
#endif
}
#endif

static bool fn_sre_compile_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	const char *pattern = C_STR(q, p1);
	re_t reg = re_compile(pattern);
	cell tmp = {0};
	tmp.tag = TAG_BLOB;
	tmp.flags = FLAG_MANAGED;
	tmp.nbr_cells = 1;
	tmp.val_blob = malloc(sizeof(blob));
	tmp.val_blob->ptr = (void*)reg;
	tmp.val_blob->refcnt = 1;
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool fn_sre_matchp_4(query *q)
{
	GET_FIRST_ARG(p1,blob);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,atom_or_var);
	re_t re = (void*)p1->val_blob->ptr;
	const char *text = C_STR(q, p2);
	int len = 0;
	int off = re_matchp(re, text, &len);
	cell tmp1, tmp2;

	if (!len)
		make_atom(&tmp1, g_nil_s);
	else
		make_slice(q, &tmp1, p2, off, len);

	bool ok = unify(q, p3, p3_ctx, &tmp1, q->st.curr_frame);
	if (!ok) return false;

	if ((size_t)(off + len) >= C_STRLEN(q, p2))
		make_atom(&tmp2, g_nil_s);
	else
		make_slice(q, &tmp2, p2, off + len, C_STRLEN(q, p2)-(off+len));

	ok = unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	if (!ok) return false;
	unshare_cell(&tmp1);
	unshare_cell(&tmp2);
	return true;
}

static bool fn_sre_match_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,atom_or_var);
	const char *pattern = C_STR(q, p1);
	const char *text = C_STR(q, p2);
	int len = 0;
	int off = re_match(pattern, text, &len);
	cell tmp1, tmp2;

	if (!len)
		make_atom(&tmp1, g_nil_s);
	else
		make_slice(q, &tmp1, p2, off, len);

	bool ok = unify(q, p3, p3_ctx, &tmp1, q->st.curr_frame);
	if (!ok) return false;

	if ((size_t)(off + len) >= C_STRLEN(q, p2))
		make_atom(&tmp2, g_nil_s);
	else
		make_slice(q, &tmp2, p2, off + len, C_STRLEN(q, p2)-(off+len));

	ok = unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	if (!ok) return false;
	unshare_cell(&tmp1);
	unshare_cell(&tmp2);
	return true;
}

static bool fn_sre_substp_4(query *q)
{
	GET_FIRST_ARG(p1,blob);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,atom_or_var);
	re_t re = (void*)p1->val_blob->ptr;
	const char *text = C_STR(q, p2);
	int len = 0;
	int off = re_matchp(re, text, &len);
	cell tmp1, tmp2;

	if (!len)
		make_stringn(&tmp1, text, C_STRLEN(q, p2));
	else
		make_slice(q, &tmp1, p2, 0, off);

	bool ok = unify(q, p3, p3_ctx, &tmp1, q->st.curr_frame);
	if (!ok) return false;

	if ((size_t)(off + len) >= C_STRLEN(q, p2))
		make_atom(&tmp2, g_nil_s);
	else
		make_slice(q, &tmp2, p2, off + len, C_STRLEN(q, p2)-(off+len));

	ok = unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	if (!ok) return false;
	unshare_cell(&tmp1);
	unshare_cell(&tmp2);
	return true;
}

static bool fn_sre_subst_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,atom_or_var);
	const char *pattern = C_STR(q, p1);
	const char *text = C_STR(q, p2);
	int len = 0;
	int off = re_match(pattern, text, &len);
	cell tmp1, tmp2;

	if (!len)
		make_stringn(&tmp1, text, C_STRLEN(q, p2));
	else
		make_slice(q, &tmp1, p2, 0, off);

	bool ok = unify(q, p3, p3_ctx, &tmp1, q->st.curr_frame);
	if (!ok) return false;

	if ((size_t)(off + len) >= C_STRLEN(q, p2))
		make_atom(&tmp2, g_nil_s);
	else
		make_slice(q, &tmp2, p2, off + len, C_STRLEN(q, p2)-(off+len));

	ok = unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	if (!ok) return false;
	unshare_cell(&tmp1);
	unshare_cell(&tmp2);
	return true;
}

void format_property(module *m, char *tmpbuf, size_t buflen, const char *name, unsigned arity, const char *type)
{
	char *dst = tmpbuf;

	if (needs_quoting(m, name, strlen(name))) {
		char tmpbuf2[1024];
		formatted(tmpbuf2, sizeof(tmpbuf2), name, strlen(name), false);
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'('%s'", tmpbuf2);
	} else
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'(%s", name);

	if (arity) {
		dst += snprintf(dst, buflen-(dst-tmpbuf), "(");

		for (unsigned i = 0; i < arity; i++) {
			if (i > 0)
				dst += snprintf(dst, buflen-(dst-tmpbuf), ",");

			dst += snprintf(dst, buflen-(dst-tmpbuf), "_");
		}

		dst += snprintf(dst, buflen-(dst-tmpbuf), ")");
	}

	dst += snprintf(dst, buflen-(dst-tmpbuf), ", %s).\n", type);
}

static void load_properties(module *m)
{
	if (m->loaded_properties)
		return;

	m->loaded_properties = true;
	SB_alloc(pr, 1024*64);
	char tmpbuf[1024];

	format_property(m, tmpbuf, sizeof(tmpbuf), "!", 0, "control_construct"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "true", 0, "control_construct"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "fail", 0, "control_construct"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ",", 2, "control_construct"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ";", 2, "control_construct"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "->", 2, "control_construct"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "*->", 2, "control_construct"); SB_strcat(pr, tmpbuf);
	//format_property(m, tmpbuf, sizeof(tmpbuf), "call", 1, "control_construct"); SB_strcat(pr, tmpbuf);
	//format_property(m, tmpbuf, sizeof(tmpbuf), "bagof", 3, "control_construct"); SB_strcat(pr, tmpbuf);
	//format_property(m, tmpbuf, sizeof(tmpbuf), "setof", 3, "control_construct"); SB_strcat(pr, tmpbuf);
	//format_property(m, tmpbuf, sizeof(tmpbuf), "throw", 1, "control_construct"); SB_strcat(pr, tmpbuf);
	//format_property(m, tmpbuf, sizeof(tmpbuf), "findall", 3, "control_construct"); SB_strcat(pr, tmpbuf);
	//format_property(m, tmpbuf, sizeof(tmpbuf), "findall", 4, "control_construct"); SB_strcat(pr, tmpbuf);
	//format_property(m, tmpbuf, sizeof(tmpbuf), "forall", 2, "control_construct"); SB_strcat(pr, tmpbuf);

	//format_property(m, tmpbuf, sizeof(tmpbuf), "catch", 3, "meta_predicate(catch(0,?,0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ",", 2, "meta_predicate((0,0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ";", 2, "meta_predicate((0;0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "->", 2, "meta_predicate((0->0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "*->", 2, "meta_predicate((0*->0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "if", 3, "meta_predicate(if(0,0,0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "once", 1, "meta_predicate(once(0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "ignore", 1, "meta_predicate(ignore(0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "call", 1, "meta_predicate(call(0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "findall", 3, "meta_predicate(findall(?,0,-))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "|", 2, "meta_predicate((:|+))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "time", 1, "meta_predicate(time(0))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "call_nth", 2, "meta_predicate(call_nth(0,?))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "asserta", 1, "meta_predicate(asserta(:))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "asserta", 2, "meta_predicate(asserta(:,-))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "assertz", 1, "meta_predicate(assertz(:))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "assertz", 2, "meta_predicate(assertz(:,-))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retract", 1, "meta_predicate(retract(:))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retract", 2, "meta_predicate(retract(:,?))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retractall", 1, "meta_predicate(retractall(:))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "current_predicate", 1, "meta_predicate(current_predicate(:))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "predicate_property", 1, "meta_predicate(predicate_property(:,?))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "abolish", 1, "meta_predicate(abolish(:))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "clause", 2, "meta_predicate(clause(:,?))"); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "clause", 3, "meta_predicate(clause(:,?,?))"); SB_strcat(pr, tmpbuf);

	for (int i = 2; i <= 7; i++) {
		char metabuf[1024];
		char *dst2 = metabuf;
		dst2 += snprintf(dst2, sizeof(metabuf), "meta_predicate(call(%d", i-1);

		for (int j = 1; j < i; j++)
			dst2 += snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), ",?");


		snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), "))");
		format_property(m, tmpbuf, sizeof(tmpbuf), "call", i, metabuf); SB_strcat(pr, tmpbuf);
	}

	for (int i = 2; i <= 7; i++) {
		char metabuf[1024];
		char *dst2 = metabuf;
		dst2 += snprintf(dst2, sizeof(metabuf), "meta_predicate(task(%d", i-1);

		for (int j = 1; j < i; j++)
			dst2 += snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), ",?");


		snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), "))");
		format_property(m, tmpbuf, sizeof(tmpbuf), "task", i, metabuf); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_iso_bifs; ptr->name; ptr++) {
		map_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->evaluable) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); SB_strcat(pr, tmpbuf);
 	}

	for (const builtins *ptr = g_files_bifs; ptr->name; ptr++) {
		map_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->evaluable) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); SB_strcat(pr, tmpbuf);
 	}

	for (const builtins *ptr = g_evaluable_bifs; ptr->name; ptr++) {
		map_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->evaluable) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_other_bifs; ptr->name; ptr++) {
		map_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->evaluable) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_ffi_bifs; ptr->name; ptr++) {
		map_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->evaluable) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_posix_bifs; ptr->name; ptr++) {
		map_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->evaluable) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_contrib_bifs; ptr->name; ptr++) {
		map_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		if (ptr->evaluable) continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static"); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "native_code"); SB_strcat(pr, tmpbuf);
	}

	parser *p = create_parser(m);
	p->srcptr = SB_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	destroy_parser(p);
	SB_free(pr);
}

static void load_flags(query *q)
{
	cell tmp;
	make_atom(&tmp, index_from_pool(q->pl, "$current_prolog_flag"));
	tmp.arity = 2;

	if (do_abolish(q, &tmp, &tmp, false) != true)
		return;

	module *m = q->st.m;
	SB_alloc(pr, 1024);

	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "double_quotes", m->flags.double_quote_atom?"atom":m->flags.double_quote_chars?"chars":m->flags.double_quote_codes?"codes":"???");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "char_conversion", m->flags.char_conversion?"on":"off");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "occurs_check", m->flags.occurs_check==OCCURS_CHECK_TRUE?"true":m->flags.occurs_check==OCCURS_CHECK_FALSE?"false":"error");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "character_escapes", m->flags.character_escapes?"true":"false");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "strict_iso", !m->flags.not_strict_iso?"on":"off");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "debug", m->flags.debug?"on":"off");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "unknown", m->flags.unknown == UNK_ERROR?"error":m->flags.unknown == UNK_WARNING?"warning":m->flags.unknown == UNK_CHANGEABLE?"changeable":"fail");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "encoding", "'UTF-8'");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "unix", "true");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "dialect", "trealla");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "bounded", "false");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "max_arity", MAX_ARITY);
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "cpu_count", g_cpu_count);
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "integer_rounding_function", "toward_zero");

	parser *p = create_parser(m);
	p->srcptr = SB_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	destroy_parser(p);
	SB_free(pr);
}

static void load_ops(query *q)
{
	if (q->st.m->loaded_ops)
		return;

	cell tmp;
	make_atom(&tmp, index_from_pool(q->pl, "$current_op"));
	tmp.arity = 3;

	if (do_abolish(q, &tmp, &tmp, false) != true)
		return;

	q->st.m->loaded_ops = true;
	SB_alloc(pr, 1024*8);
	miter *iter = map_first(q->st.m->ops);
	op_table *ptr;

	while (map_next(iter, (void**)&ptr)) {
		char specifier[80], name[256];

		if (!ptr->specifier)
			continue;

		if (ptr->specifier == OP_FX)
			strcpy(specifier, "fx");
		else if (ptr->specifier == OP_FY)
			strcpy(specifier, "fy");
		else if (ptr->specifier == OP_YF)
			strcpy(specifier, "yf");
		else if (ptr->specifier == OP_XF)
			strcpy(specifier, "xf");
		else if (ptr->specifier == OP_YFX)
			strcpy(specifier, "yfx");
		else if (ptr->specifier == OP_XFY)
			strcpy(specifier, "xfy");
		else if (ptr->specifier == OP_XFX)
			strcpy(specifier, "xfx");

		bool quote = needs_quoting(q->st.m, ptr->name, strlen(ptr->name));

		if (quote)
			formatted(name, sizeof(name), ptr->name, strlen(ptr->name), false);
		else
			snprintf(name, sizeof(name), "%s", ptr->name);

		char tmpbuf[1024];

		if (quote)
			snprintf(tmpbuf, sizeof(tmpbuf), "'$current_op'( '%s', %s, %u).\n", name, specifier, ptr->priority);
		else
			snprintf(tmpbuf, sizeof(tmpbuf), "'$current_op'( (%s), %s, %u).\n", name, specifier, ptr->priority);

		SB_strcat(pr, tmpbuf);
	}

	map_done(iter);
	iter = map_first(q->st.m->defops);

	while (map_next(iter, (void**)&ptr)) {
		char specifier[80], name[256];

		if (!ptr->specifier)
			continue;

		if (ptr->specifier == OP_FX)
			strcpy(specifier, "fx");
		else if (ptr->specifier == OP_FY)
			strcpy(specifier, "fy");
		else if (ptr->specifier == OP_YF)
			strcpy(specifier, "yf");
		else if (ptr->specifier == OP_XF)
			strcpy(specifier, "xf");
		else if (ptr->specifier == OP_YFX)
			strcpy(specifier, "yfx");
		else if (ptr->specifier == OP_XFY)
			strcpy(specifier, "xfy");
		else if (ptr->specifier == OP_XFX)
			strcpy(specifier, "xfx");

		formatted(name, sizeof(name), ptr->name, strlen(ptr->name), false);
		char tmpbuf[1024];

		snprintf(tmpbuf, sizeof(tmpbuf), "'$current_op'('%s', %s, %u).\n",
			name, specifier, ptr->priority);
		SB_strcat(pr, tmpbuf);
	}

	//printf("%s", SB_cstr(pr));

	parser *p = create_parser(q->st.m);
	p->srcptr = SB_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	destroy_parser(p);
	SB_free(pr);
}

builtins g_iso_bifs[] =
{
	{",", 2, fn_iso_conjunction_2, "+callable,+callable", true, false, BLAH},
	{";", 2, fn_iso_disjunction_2, "+callable,+callable", true, false, BLAH},
	{"!", 0, fn_iso_cut_0, NULL, true, false, BLAH},
	{":", 2, fn_iso_invoke_2, "+module,+callable", true, false, BLAH},
	{"=..", 2, fn_iso_univ_2, "+term,?list", true, false, BLAH},
	{"->", 2, fn_iso_if_then_2, "+callable,+callable", true, false, BLAH},
	{"\\+", 1, fn_iso_negation_1, "+callable", true, false, BLAH},
	{"=", 2, fn_iso_unify_2, "+term,+term", true, false, BLAH},
	{"\\=", 2, fn_iso_notunify_2, "+term,+term", true, false, BLAH},
	{"-->", 2, fn_iso_dcgs_2, "+term,+term", true, false, BLAH},

	{"$catch", 3, fn_iso_catch_3, NULL, true, false, BLAH},
	{"$call_cleanup", 3, fn_sys_call_cleanup_3, NULL, false, false, BLAH},
	{"$block_catcher", 1, fn_sys_block_catcher_1, NULL, false, false, BLAH},
	{"$queuen", 2, fn_sys_queuen_2, NULL, false, false, BLAH},
	{"$cleanup_if_det", 0, fn_sys_cleanup_if_det_0, NULL, false, false, BLAH},
	{"$soft_inner_cut", 0, fn_sys_soft_inner_cut_0, NULL, false, false, BLAH},
	{"$inner_cut", 0, fn_sys_inner_cut_0, NULL, false, false, BLAH},
	{"$drop_barrier", 0, fn_sys_drop_barrier, NULL, false, false, BLAH},
	{"$elapsed", 0, fn_sys_elapsed_0, NULL, false, false, BLAH},
	{"$lt", 2, fn_sys_lt_2, NULL, false, false, BLAH},
	{"$gt", 2, fn_sys_gt_2, NULL, false, false, BLAH},
	{"$ne", 2, fn_sys_ne_2, NULL, false, false, BLAH},
	{"$incr", 2, fn_sys_incr_2, NULL, false, false, BLAH},

	{"call", 1, fn_iso_call_n, "+callable", true, false, BLAH},
	{"call", 2, fn_iso_call_n, "+callable,...", true, false, BLAH},
	{"call", 3, fn_iso_call_n, "+callable,...", true, false, BLAH},
	{"call", 4, fn_iso_call_n, "+callable,...", true, false, BLAH},
	{"call", 5, fn_iso_call_n, "+callable,...", true, false, BLAH},
	{"call", 6, fn_iso_call_n, "+callable,...", true, false, BLAH},
	{"call", 7, fn_iso_call_n, "+callable,...", true, false, BLAH},
	{"call", 8, fn_iso_call_n, "+callable,...", true, false, BLAH},

	{"throw", 1, fn_iso_throw_1, "+term", true, false, BLAH},
	{"once", 1, fn_iso_once_1, "+callable", true, false, BLAH},
	{"repeat", 0, fn_iso_repeat_0, NULL, true, false, BLAH},
	{"true", 0, fn_iso_true_0, NULL, true, false, BLAH},
	{"fail", 0, fn_iso_fail_0, NULL, true, false, BLAH},
	{"false", 0, fn_iso_fail_0, NULL, true, false, BLAH},
	{"atom", 1, fn_iso_atom_1, "+term", true, false, BLAH},
	{"atomic", 1, fn_iso_atomic_1, "+term", true, false, BLAH},
	{"number", 1, fn_iso_number_1, "+term", true, false, BLAH},
	{"compound", 1, fn_iso_compound_1, "+term", true, false, BLAH},
	{"var", 1, fn_iso_var_1, "+term", true, false, BLAH},
	{"nonvar", 1, fn_iso_nonvar_1, "+term", true, false, BLAH},
	{"ground", 1, fn_iso_ground_1, "+term", true, false, BLAH},
	{"callable", 1, fn_iso_callable_1, "+term", true, false, BLAH},
	{"char_code", 2, fn_iso_char_code_2, "?atom,?integer", true, false, BLAH},
	{"atom_chars", 2, fn_iso_atom_chars_2, "?number,?list", true, false, BLAH},
	{"atom_codes", 2, fn_iso_atom_codes_2, "?number,?list", true, false, BLAH},
	{"number_chars", 2, fn_iso_number_chars_2, "?number,?list", true, false, BLAH},
	{"number_codes", 2, fn_iso_number_codes_2, "?number,?list", true, false, BLAH},
	{"clause", 2, fn_iso_clause_2, "+head,?body", true, false, BLAH},
	{"arg", 3, fn_iso_arg_3, "+arg,+term,?term", true, false, BLAH},
	{"functor", 3, fn_iso_functor_3, "?term,?functor,?arity", true, false, BLAH},
	{"copy_term", 2, fn_iso_copy_term_2, "+term,?term", true, false, BLAH},
	{"term_variables", 2, fn_iso_term_variables_2, "+term,-list", true, false, BLAH},
	{"atom_length", 2, fn_iso_atom_length_2, "?list,?integer", true, false, BLAH},
	{"atom_concat", 3, fn_iso_atom_concat_3, "+atom,+atom,?atom", true, false, BLAH},
	{"sub_atom", 5, fn_iso_sub_atom_5, "+atom,?before,?length,?after,?atom", true, false, BLAH},
	{"current_rule", 1, fn_iso_current_rule_1, "-term", true, false, BLAH},
	{"sort", 2, fn_iso_sort_2, "+list,?list", true, false, BLAH},
	{"msort", 2, fn_iso_msort_2, "+list,?list", true, false, BLAH},
	{"keysort", 2, fn_iso_keysort_2, "+list,?list", true, false, BLAH},
	{"end_of_file", 0, fn_iso_halt_0, NULL, true, false, BLAH},
	{"halt", 0, fn_iso_halt_0, NULL, true, false, BLAH},
	{"halt", 1, fn_iso_halt_1, "+integer", true, false, BLAH},
	{"abolish", 1, fn_iso_abolish_1, "+predicateindicator", true, false, BLAH},
	{"asserta", 1, fn_iso_asserta_1, "+term", true, false, BLAH},
	{"assertz", 1, fn_iso_assertz_1, "+term", true, false, BLAH},
	{"retract", 1, fn_iso_retract_1, "+term", true, false, BLAH},
	{"retractall", 1, fn_iso_retractall_1, "+term", true, false, BLAH},
	{"$legacy_current_prolog_flag", 2, fn_iso_current_prolog_flag_2, "+atom,?term", true, false, BLAH},
	{"set_prolog_flag", 2, fn_iso_set_prolog_flag_2, "+atom,+term", true, false, BLAH},
	{"op", 3, fn_iso_op_3, "?priority,?type,+atom", true, false, BLAH},
	{"findall", 3, fn_iso_findall_3, "+template,+callable,-list", true, false, BLAH},
	{"current_predicate", 1, fn_iso_current_predicate_1, "+predicateindicator", true, false, BLAH},
	{"acyclic_term", 1, fn_iso_acyclic_term_1, "+term", true, false, BLAH},
	{"compare", 3, fn_iso_compare_3, "+atom,+term,+term", true, false, BLAH},
	{"unify_with_occurs_check", 2, fn_iso_unify_with_occurs_check_2, "+term,+term", true, false, BLAH},

	//{"$bagof", 3, fn_sys_bagof_3, NULL, true, false, BLAH},

	{0}
};

builtins g_other_bifs[] =
{
	{"*->", 2, fn_if_2, "+if,+then", false, false, BLAH},
	{"if", 3, fn_if_3, "+if,+then,+else", false, false, BLAH},

	{"cyclic_term", 1, fn_cyclic_term_1, "+term", false, false, BLAH},
	{"current_module", 1, fn_current_module_1, "-module", false, false, BLAH},
	{"prolog_load_context", 2, fn_prolog_load_context_2, "+atom,?term", false, false, BLAH},
	{"module", 1, fn_module_1, "?module", false, false, BLAH},
	{"attribute", 3, fn_attribute_3, "?module,+attribute,+arity", false, false, BLAH},
	{"using", 0, fn_using_0, NULL, false, false, BLAH},
	{"use_module", 1, fn_use_module_1, "+term", false, false, BLAH},
	{"use_module", 2, fn_use_module_2, "+term,+term", false, false, BLAH},

	{"sleep", 1, fn_sleep_1, "+secs", false, false, BLAH},
	{"delay", 1, fn_delay_1, "+ms", false, false, BLAH},
#ifndef __wasi__
	{"shell", 1, fn_shell_1, "+atom", false, false, BLAH},
	{"shell", 2, fn_shell_2, "+atom,-integer", false, false, BLAH},
	{"pid", 1, fn_pid_1, "-integer", false, false, BLAH},
#endif

	{"listing", 0, fn_listing_0, NULL, false, false, BLAH},
	{"listing", 1, fn_listing_1, "+predicateindicator", false, false, BLAH},
	{"time", 1, fn_time_1, "+callable", false, false, BLAH},
	{"trace", 0, fn_trace_0, NULL, false, false, BLAH},
	{"help", 2, fn_help_2, "+predicateindicator,+atom", false, false, BLAH},
	{"help", 1, fn_help_1, "+predicateindicator", false, false, BLAH},
	{"help", 0, fn_help_0, NULL, false, false, BLAH},

	// Miscellaneous...

	{"abort", 0, fn_abort_0, NULL, false, false, BLAH},
	{"sort", 4, fn_sort_4, "+integer,+atom,+list,?list", false, false, BLAH},
	{"ignore", 1, fn_ignore_1, "+callable", false, false, BLAH},
	{"soft_abolish", 1, fn_soft_abolish_1, "+term", false, false, BLAH},
	{"string_codes", 2, fn_string_codes_2, "+string,-list", false, false, BLAH},
	{"term_singletons", 2, fn_term_singletons_2, "+term,-list", false, false, BLAH},
	{"get_unbuffered_code", 1, fn_get_unbuffered_code_1, "?code", false, false, BLAH},
	{"get_unbuffered_char", 1, fn_get_unbuffered_char_1, "?char", false, false, BLAH},
	{"format", 2, fn_format_2, "+string,+list", false, false, BLAH},
	{"format", 3, fn_format_3, "+stream,+string,+list", false, false, BLAH},
	{"abolish", 2, fn_abolish_2, "+term,+list", false, false, BLAH},
	{"assert", 1, fn_iso_assertz_1, "+term", false, false, BLAH},
	{"copy_term_nat", 2, fn_copy_term_nat_2, "+term,-term", false, false, BLAH},
	{"string", 1, fn_string_1, "+term", false, false, BLAH},
	{"atomic_concat", 3, fn_atomic_concat_3, "+atomic,+atomic,?atomic", false, false, BLAH},
	{"atomic_list_concat", 3, fn_atomic_list_concat_3, "+list,+list,-atomic", false, false, BLAH},
	{"replace", 4, fn_replace_4, "+orig,+from,+to,-new", false, false, BLAH},
	{"busy", 1, fn_busy_1, "+integer", false, false, BLAH},
	{"now", 0, fn_now_0, NULL, false, false, BLAH},
	{"now", 1, fn_now_1, "-integer", false, false, BLAH},
	{"get_time", 1, fn_get_time_1, "-var", false, false, BLAH},
	{"cpu_time", 1, fn_cpu_time_1, "-var", false, false, BLAH},
	{"wall_time", 1, fn_wall_time_1, "-integer", false, false, BLAH},
	{"date_time", 6, fn_date_time_6, "-yyyy,-m,-d,-h,--m,-s", false, false, BLAH},
	{"date_time", 7, fn_date_time_7, "-yyyy,-m,-d,-h,--m,-s,-ms", false, false, BLAH},
	{"split_atom", 4, fn_split_atom_4, "+string,+sep,+pad,-list", false, false, BLAH},
	{"split_string", 4, fn_split_atom_4, "+string,+sep,+pad,-list", false, false, BLAH},
	{"split", 4, fn_split_4, "+string,+string,?left,?right", false, false, BLAH},
	{"is_list_or_partial_list", 1, fn_is_list_or_partial_list_1, "+term", false, false, BLAH},
	{"is_partial_list", 1, fn_is_partial_list_1, "+term", false, false, BLAH},
	{"is_list", 1, fn_is_list_1, "+term", false, false, BLAH},
	{"list", 1, fn_is_list_1, "+term", false, false, BLAH},
	{"is_stream", 1, fn_is_stream_1, "+term", false, false, BLAH},
	//{"forall", 2, fn_forall_2, "+term,+term", false, false, BLAH},
	{"term_hash", 2, fn_term_hash_2, "+term,?integer", false, false, BLAH},
	{"name", 2, fn_iso_atom_codes_2, "?string,?list", false, false, BLAH},
	{"base64", 3, fn_base64_3, "?string,?string,+list", false, false, BLAH},
	{"urlenc", 3, fn_urlenc_3, "?string,?string,+list", false, false, BLAH},
	{"atom_lower", 2, fn_atom_lower_2, "?atom,?atom", false, false, BLAH},
	{"atom_upper", 2, fn_atom_upper_2, "?atom,?atom", false, false, BLAH},
	{"string_lower", 2, fn_string_lower_2, "?string,?string", false, false, BLAH},
	{"string_upper", 2, fn_string_upper_2, "?string,?string", false, false, BLAH},
	{"hex_bytes", 2, fn_hex_bytes_2, "?string,?list", false, false, BLAH},
	{"hex_chars", 2, fn_hex_chars_2, "?integer,?string", false, false, BLAH},
	{"octal_chars", 2, fn_octal_chars_2, "?integer,?string", false, false, BLAH},
	{"char_type", 2, fn_char_type_2, "+char,+term", false, false, BLAH},
	{"code_type", 2, fn_char_type_2, "+code,+term", false, false, BLAH},
	{"uuid", 1, fn_uuid_1, "-string", false, false, BLAH},
	{"asserta", 2, fn_asserta_2, "+term,-ref", false, false, BLAH},
	{"assertz", 2, fn_assertz_2, "+term,-ref", false, false, BLAH},
	{"instance", 2, fn_instance_2, "+ref,?clause", false, false, BLAH},
	{"erase", 1, fn_erase_1, "+ref", false, false, BLAH},
	{"clause", 3, fn_clause_3, "?head,?body,-ref", false, false, BLAH},
	{"getenv", 2, fn_getenv_2, "+atom,-atom", false, false, BLAH},
	{"setenv", 2, fn_setenv_2, "+atom,+atom", false, false, BLAH},
	{"unsetenv", 1, fn_unsetenv_1, "+atom", false, false, BLAH},
	{"statistics", 0, fn_statistics_0, NULL, false, false, BLAH},
	{"statistics", 2, fn_statistics_2, "+string,-var", false, false, BLAH},
	{"duplicate_term", 2, fn_iso_copy_term_2, "+term,-var", false, false, BLAH},
	{"call_nth", 2, fn_call_nth_2, "+callable,+integer", false, false, BLAH},
	{"limit", 2, fn_limit_2, "+integer,+callable", false, false, BLAH},
	{"offset", 2, fn_offset_2, "+integer,+callable", false, false, BLAH},
	{"unifiable", 3, fn_sys_unifiable_3, "+term,+term,-list", false, false, BLAH},
	{"kv_set", 3, fn_kv_set_3, "+atomic,+value,+list", false, false, BLAH},
	{"kv_get", 3, fn_kv_get_3, "+atomic,-value,+list", false, false, BLAH},
	{"between", 3, fn_between_3, "+integer,+integer,-integer", false, false, BLAH},

	{"must_be", 4, fn_must_be_4, "+term,+atom,+term,?any", false, false, BLAH},
	{"can_be", 4, fn_can_be_4, "+term,+atom,+term,?any", false, false, BLAH},
	{"must_be", 2, fn_must_be_2, "+atom,+term", false, false, BLAH},
	{"can_be", 2, fn_can_be_2, "+atom,+term,", false, false, BLAH},

	{"sre_compile", 2, fn_sre_compile_2, "+pattern,-reg,", false, false, BLAH},
	{"sre_matchp", 4, fn_sre_matchp_4, "+reg,+text,-match,-rest,", false, false, BLAH},
	{"sre_match", 4, fn_sre_match_4, "+pattern,+text,-match,-rest,", false, false, BLAH},
	{"sre_substp", 4, fn_sre_substp_4, "+reg,+text,-prefix,-rest,", false, false, BLAH},
	{"sre_subst", 4, fn_sre_subst_4, "+pattern,+text,-prefix,-rest,", false, false, BLAH},

	{"$register_cleanup", 1, fn_sys_register_cleanup_1, NULL, false, false, BLAH},
	{"$register_term", 1, fn_sys_register_term_1, NULL, false, false, BLAH},
	{"$get_level", 1, fn_sys_get_level_1, "-var", false, false, BLAH},
	{"$is_partial_string", 1, fn_sys_is_partial_string_1, "+string", false, false, BLAH},
	{"$lengthchk", 2, fn_sys_lengthchk_2, NULL, false, false, BLAH},
	{"$undo_trail", 1, fn_sys_undo_trail_1, NULL, false, false, BLAH},
	{"$redo_trail", 0, fn_sys_redo_trail_0, NULL, false, false, BLAH},
	{"$legacy_predicate_property", 2, fn_sys_legacy_predicate_property_2, "+callable,?string", false, false, BLAH},
	{"$load_properties", 0, fn_sys_load_properties_0, NULL, false, false, BLAH},
	{"$load_flags", 0, fn_sys_load_flags_0, NULL, false, false, BLAH},
	{"$load_ops", 0, fn_sys_load_ops_0, NULL, false, false, BLAH},
	{"$list", 1, fn_sys_list_1, "-list", false, false, BLAH},
	{"$queue", 1, fn_sys_queue_1, "+term", false, false, BLAH},
	{"$incr", 2, fn_sys_incr_2, "?var", false, false, BLAH},
	{"$choice", 0, fn_sys_choice_0, NULL, false, false, BLAH},
	{"$alarm", 1, fn_sys_alarm_1, "+integer", false, false, BLAH},
	{"$put_attributes", 2, fn_sys_put_attributes_2, "+var,+list", false, false, BLAH},
	{"$get_attributes", 2, fn_sys_get_attributes_2, "+var,-list", false, false, BLAH},
	{"$erase_attributes", 1, fn_sys_erase_attributes_1, "+var", false, false, BLAH},
	{"$list_attributed", 1, fn_sys_list_attributed_1, "-list", false, false, BLAH},
	{"$dump_keys", 1, fn_sys_dump_keys_1, "+pi", false, false, BLAH},
	{"$skip_max_list", 4, fn_sys_skip_max_list_4, NULL, false, false, BLAH},
#ifdef __wasi__
	{"$host_call", 2, fn_sys_host_call_2, "+string,-string", false, false, BLAH},
	{"$host_resume", 1, fn_sys_host_resume_1, "-string", false, false, BLAH},
#endif

#if USE_OPENSSL
	{"crypto_data_hash", 3, fn_crypto_data_hash_3, "?string,?string,?list", false, false, BLAH},
#endif

	{"task", 1, fn_task_n, "+callable", false, false, BLAH},
	{"task", 2, fn_task_n, "+callable,+term,...", false, false, BLAH},
	{"task", 3, fn_task_n, "+callable,+term,...", false, false, BLAH},
	{"task", 4, fn_task_n, "+callable,+term,...", false, false, BLAH},
	{"task", 5, fn_task_n, "+callable,+term,...", false, false, BLAH},
	{"task", 6, fn_task_n, "+callable,+term,...", false, false, BLAH},
	{"task", 7, fn_task_n, "+callable,+term,...", false, false, BLAH},
	{"task", 8, fn_task_n, "+callable,+term,...", false, false, BLAH},

	{"wait", 0, fn_wait_0, NULL, false, false, BLAH},
	{"await", 0, fn_await_0, NULL, false, false, BLAH},
	{"yield", 0, fn_yield_0, NULL, false, false, BLAH},
	{"fork", 0, fn_fork_0, NULL, false, false, BLAH},
	{"send", 1, fn_send_1, "+term", false, false, BLAH},
	{"recv", 1, fn_recv_1, "?clause", false, false, BLAH},

	{0}
};
