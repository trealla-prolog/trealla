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

#include "bif_atts.h"

#if USE_OPENSSL
#include "openssl/sha.h"
#include "openssl/hmac.h"
#endif

#ifdef _WIN32
#include <windows.h>
#define unsetenv(p1)
#define setenv(p1,p2,p3) _putenv_s(p1,p2)
#define msleep Sleep
#define localtime_r(p1,p2) localtime(p1)
#else
#include <unistd.h>
static void msleep(int ms)
{
	struct timespec tv = {0};
	tv.tv_sec = (ms) / 1000;
	tv.tv_nsec = ((ms) % 1000) * 1000 * 1000;
	nanosleep(&tv, &tv);
}
#endif

void make_call(query *q, cell *tmp)
{
	make_end(tmp);
	const frame *f = GET_CURR_FRAME();
	cell *c = q->st.curr_instr;
	tmp->save_ret = c + c->nbr_cells;	// save next as the return instruction
	tmp->chgen = f->chgen;				// ... choice-generation
	tmp->mid = q->st.m->id;				// ... current-module
}

void make_call_redo(query *q, cell *tmp)
{
	make_end(tmp);
	const frame *f = GET_CURR_FRAME();
	tmp->save_ret = q->st.curr_instr;		// save the return instruction
	tmp->chgen = f->chgen;				// ... choice-generation
	tmp->mid = q->st.m->id;				// ... current-module
}

#if 0
static void init_queue(query *q)
{
	free(q->queue[0]);
	q->queue[0] = NULL;
	q->qp[0] = 0;
}
#endif

static pl_idx queue_used(const query *q) { return q->qp[0]; }
static cell *get_queue(query *q) { return q->queue[0]; }

bool make_slice(query *q, cell *d, const cell *orig, size_t off, size_t n)
{
	if (!n) {
		make_atom(d, g_empty_s);
		return true;
	}

	if (is_slice(orig)) {
		*d = *orig;
		d->val_str += off;
		d->str_len = n;
		return true;
	}

	if (n < MAX_SMALL_STRING) {
		const char *s = C_STR(q, orig);

		make_smalln(d, s+off, n);

		if (is_string(orig)) {
			d->flags |= FLAG_CSTR_STRING;
			d->arity = 2;
		}

		return true;
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

static bool bif_iso_findall_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,list_or_nil_or_var);

	if (!q->retry) {
		bool is_partial = false;

		// This checks for a valid list (it allows for partial but acyclic lists)...

		if (is_iso_list(p3) && !check_list(q, p3, p3_ctx, &is_partial, NULL) && !is_partial)
			return throw_error(q, p3, p3_ctx, "type_error", "list");

		if (is_compound(p1) && (!is_iso_list(p1))) {	// Why?
			create_vars(q, 16);
		}

		grab_queuen(q);

		if (q->st.qnbr == MAX_QUEUES)
			return throw_error(q, p2, p2_ctx, "resource_error", "max_queues");

		cell *tmp = prepare_call(q, true, p2, p2_ctx, 1+p1->nbr_cells+2);
		check_heap_error(tmp, drop_queuen(q));
		pl_idx nbr_cells = PREFIX_LEN + p2->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_queue_s, bif_sys_queue_1, 1, p1->nbr_cells);
		nbr_cells += copy_cells_by_ref(tmp+nbr_cells, p1, p1_ctx, p1->nbr_cells);
		make_struct(tmp+nbr_cells++, g_fail_s, bif_iso_fail_0, 0, 0);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_barrier(q), drop_queuen(q));
		q->st.curr_instr = tmp;
		return true;
	}

	if (!queuen_used(q)) {
		drop_queuen(q);
		return unify(q, p3, p3_ctx, make_nil(), q->st.curr_frame);
	}

	// Retry takes the queue

	pl_idx nbr_cells = queuen_used(q);
	cell *solns = take_queuen(q);
	drop_queuen(q);

	// Now grab matching solutions with fresh variables for each...

	check_heap_error(init_tmp_heap(q), free(solns));

	for (cell *c = solns; nbr_cells; nbr_cells -= c->nbr_cells, c += c->nbr_cells) {
		cell *tmp = alloc_on_tmp(q, 1);
		check_heap_error(tmp, free(solns));
		make_struct(tmp, g_dot_s, NULL, 2, 0);
		q->noderef = true;
		tmp = deep_copy_to_tmp(q, c, q->st.curr_frame, false);
		q->noderef = false;
		check_heap_error(tmp, free(solns));
	}

	free(solns);
	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p3, p3_ctx, l, q->st.curr_frame);
}

static bool bif_iso_unify_with_occurs_check_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	bool save = q->flags.occurs_check;
	q->flags.occurs_check = OCCURS_CHECK_TRUE;
	bool ok = unify(q, p1, p1_ctx, p2, p2_ctx);
	q->flags.occurs_check = save;
	return ok;
}

static bool bif_sys_unifiable_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,list_or_nil_or_var);
	check_heap_error(push_choice(q));
	pl_idx save_tp = q->st.tp;

	if (!unify(q, p1, p1_ctx, p2, p2_ctx) && !q->cycle_error) {
		undo_me(q);
		drop_choice(q);
		return false;
	}

	check_heap_error(init_tmp_heap(q));

	// Go thru trail, getting the bindings...

	while (save_tp < q->st.tp) {
		const trail *tr = q->trails + save_tp;
		const frame *f = GET_FRAME(tr->var_ctx);
		slot *e = GET_SLOT(f, tr->var_nbr);
		cell *c = deref(q, &e->c, e->c.var_ctx);
		pl_idx c_ctx = q->latest_ctx;
		cell *tmp = malloc(sizeof(cell)*(2+c->nbr_cells));
		check_heap_error(tmp);
		make_struct(tmp, g_unify_s, bif_iso_unify_2, 2, 1+c->nbr_cells);
		SET_OP(tmp, OP_XFX);
		cell v;
		make_ref(&v, tr->var_nbr, q->st.curr_frame);
		tmp[1] = v;
		dup_cells_by_ref(tmp+2, c, c_ctx, c->nbr_cells);
		append_list(q, tmp);
		free(tmp);
		save_tp++;
	}

	undo_me(q);
	drop_choice(q);

	cell *l = end_list(q);
	return unify(q, p3, p3_ctx, l, q->st.curr_frame);
}

bool bif_iso_unify_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	return unify(q, p1, p1_ctx, p2, p2_ctx);
}

static bool bif_iso_notunify_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	cell tmp2;
	make_struct(&tmp2, g_unify_s, bif_iso_unify_2, 2, 0);
	SET_OP(&tmp2, OP_XFX);
	cell *tmp = prepare_call(q, true, &tmp2, q->st.curr_frame, p1->nbr_cells+p2->nbr_cells+4);
	pl_idx nbr_cells = PREFIX_LEN;
	tmp[nbr_cells++].nbr_cells += p1->nbr_cells+p2->nbr_cells;
	dup_cells_by_ref(tmp+nbr_cells, p1, p1_ctx, p1->nbr_cells);
	nbr_cells += p1->nbr_cells;
	dup_cells_by_ref(tmp+nbr_cells, p2, p2_ctx, p2->nbr_cells);
	nbr_cells += p2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_struct(tmp+nbr_cells++, g_fail_s, bif_iso_fail_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->succeed_on_retry = true;
	q->st.curr_instr = tmp;
	return true;
}

static bool bif_iso_repeat_0(query *q)
{
	check_heap_error(push_choice(q));
	return true;
}

bool bif_iso_halt_0(query *q)
{
	q->halt_code = 0;
	q->halt = q->error = true;
	return false;
}

static bool bif_iso_halt_1(query *q)
{
	GET_FIRST_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	q->halt_code = get_smallint(p1);
	q->halt = q->error = true;
	return false;
}

static bool bif_iso_number_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_number(p1);
}

static bool bif_iso_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_iso_atom(p1);
}

static bool bif_iso_compound_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_structure(p1) ? 1 : 0;
}

static bool bif_iso_atomic_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_iso_atomic(p1);
}

static bool bif_iso_var_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_var(p1);
}

static bool bif_iso_nonvar_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return !is_var(p1);
}

static bool bif_iso_ground_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return !has_vars(q, p1, p1_ctx);
}

static bool bif_iso_callable_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_callable(p1);
}

static bool bif_iso_char_code_2(query *q)
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

static bool bif_iso_atom_chars_2(query *q)
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

	if (is_var(p2) && !C_STRLEN(q, p1))
		return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

	if (is_var(p2)) {
		cell tmp;
		make_stringn(&tmp, C_STR(q, p1), C_STRLEN(q, p1));
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
		pl_idx save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
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
		SB(pr);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
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
		make_cstring(&tmp, SB_cstr(pr));
		SB_free(pr);
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	const char *src = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);
	check_heap_error(init_tmp_heap(q));

	while (len) {
		size_t n = len_char_utf8(src);
		cell tmp2;
		make_smalln(&tmp2, src, n);
		src += n;
		len -= n;
		append_list(q, &tmp2);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool bif_iso_number_chars_2(query *q)
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

	pl_int cnt = 0;
	bool any_vars = false;

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
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
		SB(pr);
		SB_check(pr, cnt+1+1);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_atom(head)) {
				SB_free(pr);
				return throw_error(q, head, q->latest_ctx, "type_error", "atom");
			}

			const char *src = C_STR(q, head);
			int ch = *src;

			if (!ch)
				return throw_error(q, head, q->latest_ctx, "type_error", "character");

			SB_putchar(pr, ch);
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2)) {
			SB_free(pr);
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");
		}

		int n = q->pl->current_input;
		stream *str = &q->pl->streams[n];

		if (!str->p)
			str->p = parser_create(q->st.m);

		parser *p = str->p;
		reset(p);
		p->error = false;
		p->flags = q->st.m->flags;
		p->srcptr = SB_cstr(pr);
		p->do_read_term = true;
		bool ok = get_token(p, true, false);
		p->do_read_term = false;

		if (q->did_throw) {
			p->srcptr = NULL;
			SB_free(pr);
			return ok;
		}

		if (!is_number(&p->v) || *p->srcptr) {
			p->srcptr = NULL;
			SB_free(pr);
			return throw_error(q, orig_p2, p2_ctx, "syntax_error", p->error&&p->error_desc?p->error_desc:"number");
		}

		p->srcptr = NULL;
		SB_free(pr);
		cell tmp = p->v;
		bool ok2 = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok2;
	}

	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	print_term_to_buf(q, p1, p1_ctx, 1, false);
	q->ignore_ops = false;
	q->quoted = 0;
	cell tmp;
	make_string(&tmp, SB_cstr(q->sb));
	SB_free(q->sb);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_iso_atom_codes_2(query *q)
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

	if (is_var(p2) && !C_STRLEN(q, p1))
		return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

	// Verify the list

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
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
		SB(pr);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			pl_int val = get_smallint(head);

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
		make_cstringn(&tmp, SB_cstr(pr), SB_strlen(pr));
		SB_free(pr);
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	const char *src = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);
	cell tmp;
	len -= len_char_utf8(src);
	make_int(&tmp, get_char_utf8(&src));
	allocate_list(q, &tmp);

	while (len) {
		CHECK_INTERRUPT();
		int char_len = len_char_utf8(src);

		if (char_len > 6)
			return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

		len -= char_len;
		make_int(&tmp, get_char_utf8(&src));
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool bif_string_codes_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_cstring(p1) && !is_var(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (!is_var(p2) && is_nil(p2)) {
		cell tmp;
		make_atom(&tmp, g_empty_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (is_var(p2) && !C_STRLEN(q, p1))
		return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

	// Verify the list

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
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
		SB(pr);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			pl_int val = get_smallint(head);

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
		make_cstringn(&tmp, SB_cstr(pr), SB_strlen(pr));
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

static bool bif_hex_bytes_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil_or_var);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "not_sufficiently_instantiated");

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p2))
		return unify(q, p1, p1_ctx, make_nil(), q->st.curr_frame);

	// Verify the list

	if (!is_var(p2)) {
		cell *save_p2 = p2;
		pl_idx save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
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
		SB(pr);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			pl_int val = get_smallint(head);

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
		make_string(&tmp, SB_cstr(pr));
		SB_free(pr);
		bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (is_list(p2) && is_iso_list(p1)) {
		LIST_HANDLER(p1);
		LIST_HANDLER(p2);

		while (is_list(p1) && is_list(p2)) {
			cell *h11 = LIST_HEAD(p1);
			h11 = deref(q, h11, p1_ctx);
			pl_idx h11_ctx = q->latest_ctx;
			p1 = LIST_TAIL(p1);
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;
			cell *h12 = LIST_HEAD(p1);
			h12 = deref(q, h12, p1_ctx);
			pl_idx h12_ctx = q->latest_ctx;

			cell *h2 = LIST_HEAD(p2);
			h2 = deref(q, h2, p2_ctx);
			unsigned n = get_smalluint(h2);

			unsigned n1 = (n >> 4) & 0xF;
			int ch;
			if (n1 < 10) ch = '0' + n1;
			else { n1 -= 10; ch = 'a' + n1; }
			char tmpbuf[10];
			put_char_utf8(tmpbuf, ch);
			cell tmp;
			make_cstring(&tmp, tmpbuf);
			unify(q, h11, h11_ctx, &tmp, q->st.curr_frame);

			unsigned n2 = n & 0xF;
			if (n2 < 10) ch = '0' + n2;
			else { n2 -= 10; ch = 'a' + n2; }
			put_char_utf8(tmpbuf, ch);
			make_cstring(&tmp, tmpbuf);

			if (!unify(q, h12, h12_ctx, &tmp, q->st.curr_frame))
				return false;

			p1 = LIST_TAIL(p1);
			p1 = deref(q, p1, p1_ctx);
			p1_ctx = q->latest_ctx;
			p2 = LIST_TAIL(p2);
			p2 = deref(q, p2, p2_ctx);
			p2_ctx = q->latest_ctx;
		}
	}

	LIST_HANDLER(p1);
	check_heap_error(init_tmp_heap(q));

	while (is_list(p1)) {
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
		append_list(q, &tmp);
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	if (!is_nil(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "hex_encoding");

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool bif_iso_number_codes_2(query *q)
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
		pl_idx save_p2_ctx = p2_ctx;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
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
		SB(pr);
		SB_check(pr, (cnt*6)+1+1);
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *head = LIST_HEAD(p2);
			head = deref(q, head, p2_ctx);

			if (!is_integer(head)) {
				SB_free(pr);
				return throw_error(q, head, q->latest_ctx, "type_error", "integer");
			}

			int val = get_smallint(head);

			if (val < 0) {
				SB_free(pr);
				return throw_error(q, head, q->latest_ctx, "representation_error", "character_code");
			}

			SB_putchar(pr, val);
			cell *tail = LIST_TAIL(p2);
			p2 = deref(q, tail, p2_ctx);
			p2_ctx = q->latest_ctx;
		}

		if (!is_nil(p2)) {
			SB_free(pr);
			return throw_error(q, orig_p2, p2_ctx, "type_error", "list");
		}

		int n = q->pl->current_input;
		stream *str = &q->pl->streams[n];

		if (!str->p)
			str->p = parser_create(q->st.m);

		parser *p = str->p;
		reset(p);
		p->error = false;
		p->flags = q->st.m->flags;
		p->srcptr = SB_cstr(pr);
		p->do_read_term = true;
		bool ok = get_token(p, true, false);
		p->do_read_term = false;

		if (q->did_throw) {
			p->srcptr = NULL;
			SB_free(pr);
			return ok;
		}

		if (!is_number(&p->v) || *p->srcptr) {
			p->srcptr = NULL;
			SB_free(pr);
			return throw_error(q, orig_p2, p2_ctx, "syntax_error", p->error?p->error_desc:"number");
		}

		p->srcptr = NULL;
		SB_free(pr);
		cell tmp = p->v;
		bool ok2 = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok2;
	}

	q->ignore_ops = true;
	q->quoted = 1;
	q->last_thing = WAS_OTHER;
	q->did_quote = false;
	print_term_to_buf(q, p1, p1_ctx, 1, 0);
	q->ignore_ops = false;
	q->quoted = 0;
	const char *src = SB_cstr(q->sb);
	cell tmp;
	make_int(&tmp, *src);
	allocate_list(q, &tmp);

	while (*++src) {
		make_int(&tmp, *src);
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	SB_free(q->sb);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool do_sub_atom(query *q, cell *p1, cell *p2, pl_idx p2_ctx, cell *p3, pl_idx p3_ctx, cell *p4, pl_idx p4_ctx, cell *p5)
{
	if (!q->retry) {
		q->st.v1 = 0;
	}

	const char *src = C_STR(q, p1), *s = C_STR(q, p5);
	pl_int srclen = C_STRLEN(q, p1), before = (int)q->st.v1, len = C_STRLEN(q, p5);
	const char *src2 = src + before;
	src2 = strstr(src2, s);

	if (q->retry && !srclen)
		return false;

	if (!src2)
		return false;

	pl_int after = srclen - (src2 - src) - len;
	before = src2 - src;

	if (after < 0)
		return false;

	q->st.v1 = before + (len ? len : 1);

	if (after && strstr(src2+(len?len:1), s))
		check_heap_error(push_choice(q));

	cell tmp;
	make_int(&tmp, before);
	unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, len);
	unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	make_int(&tmp, after);
	unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	return true;
}

static bool bif_iso_sub_string_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer_or_var);		// before
	GET_NEXT_ARG(p3,integer_or_var);		// len
	GET_NEXT_ARG(p4,integer_or_var);		// after
	GET_NEXT_ARG(p5,atom_or_var);

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	if (is_integer(p3) && is_negative(p3))
		return throw_error(q, p3, p3_ctx, "domain_error", "not_less_than_zero");

	if (is_integer(p4) && is_negative(p4))
		return throw_error(q, p4, p4_ctx, "domain_error", "not_less_than_zero");

	if (!is_var(p1) && is_var(p2) && is_var(p3) && is_var(p4) && !is_var(p5)) {
		return do_sub_atom(q, p1, p2, p2_ctx, p3, p3_ctx, p4, p4_ctx, p5);
	}

	const size_t len_p1 = C_STRLEN_UTF8(p1);
	size_t before = 0, len = 0, after = 0;
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

			if (is_atom(p5) && !CMP_STRING_TO_CSTRN(q, p5, C_STR(q, &tmp), C_STRLEN(q, &tmp))) {
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

static bool bif_iso_sub_atom_5(query *q)
{
	GET_FIRST_ARG(p1,iso_atom);
	GET_NEXT_ARG(p2,integer_or_var);		// before
	GET_NEXT_ARG(p3,integer_or_var);		// len
	GET_NEXT_ARG(p4,integer_or_var);		// after
	GET_NEXT_ARG(p5,iso_atom_or_var);

	return bif_iso_sub_string_5(q);
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
	reset_var(q, p1_raw, p1_raw_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	check_heap_error(make_slice(q, &tmp, p2, len, len2-len));
	reset_var(q, p2_raw, p2_raw_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);

	if (!done)
		check_heap_error(push_choice(q));

	return true;
}

static bool bif_iso_atom_concat_3(query *q)
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

		SB(pr);
		SB_strcatn(pr, C_STR(q, p1), C_STRLEN(q, p1));
		SB_strcatn(pr, C_STR(q, p2), C_STRLEN(q, p2));
		cell tmp;
		make_cstring(&tmp, SB_cstr(pr));
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

static bool bif_iso_atom_length_2(query *q)
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
	prolog_lock(pl);

	if (!pl->seed)
		pl->seed = (uint64_t)time(0) & MASK_FINAL;

	uint64_t now = get_time_in_usec();
	compare_and_zero(now, &pl->s_last, &pl->s_cnt);
	u->u1 = now;
	u->u2 = pl->s_cnt++;
	u->u2 <<= 48;
	u->u2 |= pl->seed;
	prolog_unlock(pl);
}

char *uuid_to_buf(const uuid *u, char *buf, size_t buflen)
{
	snprintf(buf, buflen, "%016"PRIx64"-%04"PRIx64"-%012"PRIx64"",
		 u->u1,
		 (u->u2 >> 48),
		 (u->u2 & MASK_FINAL));

	return buf;
}

int uuid_from_buf(const char *s, uuid *u)
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

static bool bif_iso_arg_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,compound);
	GET_NEXT_ARG(p3,any);

	if (is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	if (is_bigint(p1))
		return false;

	pl_int arg_nbr = get_smallint(p1);

	if ((arg_nbr == 0) || (arg_nbr > p2->arity))
		return false;

	if (is_list(p2)) {
		LIST_HANDLER(p2);
		cell *c = LIST_HEAD(p2);
		c = deref(q, c, p2_ctx);
		pl_idx c_ctx = q->latest_ctx;

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
			pl_idx c_ctx = q->latest_ctx;
			return unify(q, p3, p3_ctx, c, c_ctx);
		}

		p2 += p2->nbr_cells;
	}

	return true;
}

static bool bif_iso_univ_2(query *q)
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

		cell *tmp = deep_clone_to_heap(q, p2, p2_ctx);
		check_heap_error(tmp);
		p2 = tmp;
		p2_ctx = q->st.curr_frame;
		unsigned arity = 0, save_hp = q->st.hp;
		check_heap_error(init_tmp_heap(q));
		cell *save_p2 = p2;
		LIST_HANDLER(p2);

		while (is_list(p2)) {
			cell *h = LIST_HEAD(p2);

			if (is_cstring(h) && is_string(save_p2))
				convert_to_literal(q->st.m, h);

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
		pl_idx nbr_cells = tmp_heap_used(q);

		if (is_cstring(tmp2) && !is_string(save_p2)) {
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
		dup_cells(tmp, tmp2, nbr_cells);
		tmp->nbr_cells = nbr_cells;
		tmp->arity = arity;
		bool found = false;

		if (is_callable(tmp)) {
			if ((tmp->match = search_predicate(q->st.m, tmp, NULL)) != NULL) {
				tmp->flags &= ~FLAG_BUILTIN;
			} else if ((tmp->bif_ptr = get_builtin_term(q->st.m, tmp, &found, NULL)), found) {
				if (tmp->bif_ptr->evaluable)
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

	if (is_builtin(p1)) {
		tmp.flags &= ~FLAG_BUILTIN;
		tmp.bif_ptr = NULL;
	}

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

cell *do_term_variables(query *q, cell *p1, pl_idx p1_ctx)
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
			make_ref(tmp+idx, q->pl->tabs[i].var_nbr, q->pl->tabs[i].ctx);

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

static bool bif_iso_term_variables_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_var(p1) && (is_atom(p1) || is_number(p1)))
		return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

	cell *tmp = do_term_variables(q, p1, p1_ctx);
	check_heap_error(tmp);
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	check_heap_error(tmp2);
	dup_cells(tmp2, tmp, tmp->nbr_cells);
	return unify(q, p2, p2_ctx, tmp2, q->st.curr_frame);
}

static cell *do_term_singletons(query *q, cell *p1, pl_idx p1_ctx)
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
			make_ref(tmp+idx, q->pl->tabs[i].var_nbr, q->pl->tabs[i].ctx);

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

static bool bif_term_singletons_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,iso_list_or_nil_or_var);

	// This checks for a valid list (it allows for partial but acyclic lists)...

	bool is_partial = false;

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!is_var(p1) && (is_atom(p1) || is_number(p1)))
		return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

	cell *tmp = do_term_singletons(q, p1, p1_ctx);
	check_heap_error(tmp);
	cell *tmp2 = alloc_on_heap(q, tmp->nbr_cells);
	check_heap_error(tmp2);
	dup_cells(tmp2, tmp, tmp->nbr_cells);
	return unify(q, p2, p2_ctx, tmp2, q->st.curr_frame);
}

static bool do_duplicate_term(query *q, bool copy_attrs)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_var(p1) && is_var(p2)) {
		const frame *f1 = GET_FRAME(p1_ctx);
		const frame *f2 = GET_FRAME(p2_ctx);
		slot *e1 = GET_SLOT(f1, p1->var_nbr);
		slot *e2 = GET_SLOT(f2, p2->var_nbr);

		if (e1->c.attrs && copy_attrs) {
			e2->c.attrs = deep_copy_to_heap_with_replacement(q, e1->c.attrs, e1->c.attrs_ctx, false, p1, p1_ctx, p2, p2_ctx);
			check_heap_error(e2->c.attrs);
			e2->c.attrs_ctx = q->st.curr_frame;
		}

		return true;
	}

	if (is_atomic(p1) && is_var(p2))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	if (!is_var(p2) && !has_vars(q, p1, p1_ctx))
		return unify(q, p1, p1_ctx, p2, p2_ctx);

	// You are not expected to understand this: basically we have
	// to make sure the p1 variables get copied along with the
	// deref'd values and they get linked.

	check_heap_error(init_tmp_heap(q));
	cell *tmp1 = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(tmp1);
	GET_FIRST_RAW_ARG(p1x,any);
	cell *tmp = alloc_on_heap(q, 1 + p1x->nbr_cells + tmp1->nbr_cells);
	make_struct(tmp, g_eq_s, NULL, 2, p1x->nbr_cells + tmp1->nbr_cells);
	dup_cells_by_ref(tmp+1, p1x, p1x_ctx, p1x->nbr_cells);
	dup_cells_by_ref(tmp+1+p1x->nbr_cells, tmp1, q->st.curr_frame, tmp1->nbr_cells);
	tmp = deep_copy_to_heap(q, tmp, q->st.curr_frame, copy_attrs);
	cell *tmpp1 = tmp + 1;
	cell *tmpp2 = tmpp1 + tmpp1->nbr_cells;
	unify(q, tmpp1, q->st.curr_frame, tmpp2, q->st.curr_frame);
	return unify(q, p2, p2_ctx, tmpp1, q->st.curr_frame);
}

// Do copy attributes

static bool bif_duplicate_term_2(query *q)
{
	return do_duplicate_term(q, true);
}

// Don't copy attributes (Note: SICStus & YAP don't, Scryer & SWI do)

static bool bif_iso_copy_term_2(query *q)
{
	return do_duplicate_term(q, false);
}

// Don't copy attributes

static bool bif_copy_term_nat_2(query *q)
{
	return do_duplicate_term(q, false);
}

static bool bif_iso_functor_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (is_var(p1)) {
		GET_NEXT_ARG(p3,any);

		if (!is_atomic(p2))
			return throw_error(q, p2, p2_ctx, "type_error", "atomic");

		if (!is_integer(p3))
			return throw_error(q, p3, p3_ctx, "type_error", "integer");

		if (is_negative(p3))
			return throw_error(q, p3, p3_ctx, "domain_error", "not_less_than_zero");

		if (is_gt(p3,MAX_ARITY))
			return throw_error(q, p3, p3_ctx, "representation_error", "max_arity");

		if (!is_atom(p2) && is_positive(p3))
			return throw_error(q, p2, p2_ctx, "type_error", "atom");

		unsigned arity = get_smallint(p3);

		if (!arity) {
			unify(q, p1, p1_ctx, p2, p2_ctx);
		} else {
			int var_nbr = 0;

			if ((var_nbr = create_vars(q, arity)) < 0)
				return throw_error(q, p3, p3_ctx, "resource_error", "stack");

			cell *tmp = alloc_on_heap(q, 1+arity);
			check_heap_error(tmp);
			*tmp = (cell){0};
			tmp[0].tag = TAG_INTERNED;
			tmp[0].arity = arity;
			tmp[0].nbr_cells = 1 + arity;

			if (is_cstring(p2)) {
				tmp[0].val_off = new_atom(q->pl, C_STR(q, p2));
			} else
				tmp[0].val_off = p2->val_off;

			for (unsigned i = 1; i <= arity; i++) {
				memset(tmp+i, 0, sizeof(cell));
				tmp[i].tag = TAG_VAR;
				tmp[i].nbr_cells = 1;
				tmp[i].var_nbr = var_nbr++;
				tmp[i].var_ctx = q->st.curr_frame;
				tmp[i].flags = FLAG_VAR_REF | FLAG_VAR_FRESH | FLAG_VAR_ANON;
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

	if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
		return false;

	GET_NEXT_ARG(p3,any);
	make_int(&tmp, p1->arity);
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_current_rule_1(query *q)
{
	GET_FIRST_ARG(p1,structure);
	int add_two = 0;

	if (!CMP_STRING_TO_CSTR(q, p1, "/"))
		;
	else if (!CMP_STRING_TO_CSTR(q, p1, "//"))
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
	size_t functor_len = C_STRLEN(q, pf);
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
	tmp.val_off = new_atom(q->pl, functor);
	tmp.arity = arity;

	if (search_predicate(q->st.m, &tmp, NULL))
		return true;

	bool found = false;

	if (get_builtin(q->pl, functor, functor_len, arity, &found, NULL), found)
		return true;

	return false;
}

static bool search_functor(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx)
{
	if (!q->retry)
		q->st.f_iter = sl_first(q->st.m->index);

	check_heap_error(push_choice(q));
	check_heap_error(check_slot(q, MAX_VARS));
	predicate *pr = NULL;

	while (sl_next(q->st.f_iter, (void*)&pr)) {
		const char *src = C_STR(q, &pr->key);

		if (src[0] == '$')
			continue;

		if (pr->is_abolished || pr->is_prebuilt)
			continue;

		try_me(q, MAX_VARS);
		cell tmpn, tmpa;
		make_atom(&tmpn, pr->key.val_off);
		make_int(&tmpa, pr->key.arity);

		if (unify(q, p1, p1_ctx, &tmpn, q->st.fp)
			&& unify(q, p2, p2_ctx, &tmpa, q->st.fp)) {
			return true;
		}

		undo_me(q);
	}

	sl_done(q->st.f_iter);
	drop_choice(q);
	return false;
}

static bool bif_iso_current_predicate_1(query *q)
{
	GET_FIRST_ARG(p_pi,any);

	if (!CMP_STRING_TO_CSTR(q, p_pi, ":")) {
		q->st.m = find_module(q->pl, C_STR(q, p_pi+1));
		p_pi += 2;
	}

	if (is_var(p_pi)) {
		cell tmp1, tmp2;
		cell *p1 = &tmp1, *p2 = &tmp2;
		pl_idx p1_ctx = q->st.curr_frame;
		pl_idx p2_ctx = q->st.curr_frame;
		frame *f = GET_CURR_FRAME();
		unsigned var_nbr = f->actual_slots;
		make_ref(&tmp1, var_nbr++, q->st.curr_frame);
		make_ref(&tmp2, var_nbr++, q->st.curr_frame);
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

	if (CMP_STRING_TO_CSTR(q, p_pi, "/"))
		return throw_error(q, p_pi, p_pi_ctx, "type_error", "predicate_indicator");

	cell *p1, *p2;
	pl_idx p1_ctx, p2_ctx;

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
	tmp.val_off = is_interned(p1) ? p1->val_off : new_atom(q->pl, C_STR(q, p1));
	tmp.arity = get_smallint(p2);
	predicate *pr;

	if (q->st.m == q->pl->user_m)
		pr = search_predicate(q->st.m, &tmp, NULL);
	else
		pr = find_predicate(q->st.m, &tmp);

	if (!pr)
		return false;

	return true;
}

static bool bif_cyclic_term_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_cyclic_term(q, p1, p1_ctx) ? true : false;
}

static bool bif_iso_acyclic_term_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_acyclic_term(q, p1, p1_ctx) ? true : false;
}

static bool bif_call_residue_vars_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,list_or_nil_or_var);

	bool is_partial = false;

	// This checks for a valid list (it allows for partial but acyclic lists)...

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, NULL))
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	cell *tmp = prepare_call(q, true, p1, p1_ctx, 6);
	check_heap_error(tmp);
	tmp[1].flags &= ~FLAG_TAIL_CALL;
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, new_atom(q->pl, "term_attributed_variables"), NULL, 2, 2);
	make_indirect(tmp+nbr_cells++, p1, p1_ctx);

	if (is_var(p2))
		make_ref(tmp+nbr_cells++, p2->var_nbr, p2_ctx);
	else
		make_indirect(tmp+nbr_cells++, p2, p2_ctx);

	make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	q->st.curr_instr = tmp;
	return true;
}

static bool bif_iso_current_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);

	if (!CMP_STRING_TO_CSTR(q, p1, "double_quotes")) {
		cell tmp;

		if (q->st.m->flags.double_quote_atom)
			make_atom(&tmp, new_atom(q->pl, "atom"));
		else if (q->st.m->flags.double_quote_codes)
			make_atom(&tmp, new_atom(q->pl, "codes"));
		else if (q->st.m->flags.double_quote_chars)
			make_atom(&tmp, new_atom(q->pl, "chars"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "answer_write_options")) {
		cell tmp[2];
		make_struct(tmp+0, new_atom(q->pl, "max_depth"), NULL, 1, 1);
		make_uint(tmp+1, q->pl->def_max_depth);
		allocate_list(q, tmp);
		make_struct(tmp+0, new_atom(q->pl, "quoted"), NULL, 1, 1);
		make_atom(tmp+1, q->pl->def_quoted?g_true_s:g_false_s);
		append_list(q, tmp);
		make_struct(tmp+0, new_atom(q->pl, "double_quotes"), NULL, 1, 1);
		make_atom(tmp+1, q->pl->def_double_quotes?g_true_s:g_false_s);
		append_list(q, tmp);
		return unify(q, p2, p2_ctx, end_list(q), q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "char_conversion")) {
		cell tmp;

		if (q->st.m->flags.char_conversion)
			make_atom(&tmp, g_on_s);
		else
			make_atom(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "verbose")) {
		cell tmp;
		make_atom(&tmp, q->pl->quiet ? g_false_s : g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
#if USE_THREADS
	} else if (!CMP_STRING_TO_CSTR(q, p1, "threads")) {
		cell tmp;
		make_atom(&tmp, g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "max_threads")) {
		cell tmp;
		make_int(&tmp, MAX_ACTUAL_THREADS);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "hardware_threads")) {
		cell tmp;
		make_int(&tmp, 4);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
#else
	} else if (!CMP_STRING_TO_CSTR(q, p1, "threads")) {
		cell tmp;
		make_atom(&tmp, g_false_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
#endif
	} else if (!CMP_STRING_TO_CSTR(q, p1, "unix")) {
		cell tmp;
		make_atom(&tmp, g_true_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "occurs_check")) {
		cell tmp;

		if (q->st.m->flags.occurs_check == OCCURS_CHECK_TRUE)
			make_atom(&tmp, g_true_s);
		else if (q->st.m->flags.occurs_check == OCCURS_CHECK_FALSE)
			make_atom(&tmp, g_false_s);
		else
			make_atom(&tmp, new_atom(q->pl, "error"));

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "encoding")) {
		cell tmp;
		make_atom(&tmp, new_atom(q->pl, "UTF-8"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "strict_iso")) {
		cell tmp;

		if (!q->st.m->flags.strict_iso)
			make_atom(&tmp, g_on_s);
		else
			make_atom(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "debug")) {
		cell tmp;

		if (q->st.m->flags.debug)
			make_atom(&tmp, g_on_s);
		else
			make_atom(&tmp, g_off_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "character_escapes")) {
		cell tmp;

		if (q->st.m->flags.character_escapes)
			make_atom(&tmp, g_true_s);
		else
			make_atom(&tmp, g_false_s);

		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "dialect")) {
		cell tmp;
		make_atom(&tmp, new_atom(q->pl, "trealla"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "integer_rounding_function")) {
		cell tmp;
		make_atom(&tmp, new_atom(q->pl, "toward_zero"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "bounded")) {
		cell tmp;
		make_atom(&tmp, g_false_s);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "max_arity")) {
		cell tmp;
		make_int(&tmp, MAX_ARITY);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "pid")) {
		cell tmp;
#ifndef __wasi__
		make_int(&tmp, getpid());
#else
		make_int(&tmp, -1);
#endif
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "max_integer")) {
		return false;
	} else if (!CMP_STRING_TO_CSTR(q, p1, "min_integer")) {
		return false;
	} else if (!CMP_STRING_TO_CSTR(q, p1, "cpu_count")) {
		cell tmp;
		make_int(&tmp, g_cpu_count);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "version")) {
		unsigned v1 = 0;
		sscanf(g_version, "v%u", &v1);
		cell tmp;
		make_int(&tmp, v1);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "version_data")) {
		unsigned v1 = 0, v2 = 0, v3 = 0;
		sscanf(g_version, "v%u.%u.%u", &v1, &v2, &v3);
		cell *tmp = alloc_on_heap(q, 5);
		check_heap_error(tmp);
		make_atom(&tmp[0], new_atom(q->pl, "trealla"));
		make_int(&tmp[1], v1);
		make_int(&tmp[2], v2);
		make_int(&tmp[3], v3);
		make_atom(&tmp[4], g_nil_s);
		tmp[0].arity = 4;
		tmp[0].nbr_cells = 5;
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "version_git")) {
		cell tmp;
		make_atom(&tmp, new_atom(q->pl, g_version));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "argv")) {
		if (g_avc >= g_ac)
			return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

		int i = g_avc;
		cell tmp;
		make_cstring(&tmp, g_av[i++]);
		allocate_list(q, &tmp);

		while (i < g_ac) {
			make_cstring(&tmp, g_av[i++]);
			append_list(q, &tmp);
		}

		cell *l = end_list(q);
		check_heap_error(l);
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "unknown")) {
		cell tmp;
		make_atom(&tmp,
			q->st.m->flags.unknown == UNK_ERROR ? new_atom(q->pl, "error") :
			q->st.m->flags.unknown == UNK_WARNING ? new_atom(q->pl, "warning") :
			q->st.m->flags.unknown == UNK_CHANGEABLE ? new_atom(q->pl, "changeable") :
			new_atom(q->pl, "fail"));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p1, "generate_debug_info")) {
	}

	return throw_error(q, p1, p1_ctx, "domain_error", "prolog_flag");
}

static bool answer_write_options_error(query *q, cell *c)
{
	cell *tmp = alloc_on_heap(q, 2+c->nbr_cells);
	check_heap_error(tmp);
	make_struct(tmp, g_plus_s, bif_iso_add_2, 2, 1+c->nbr_cells);
	make_atom(tmp+1, new_atom(q->pl, "answer_write_options"));
	dup_cells(tmp+2, c, c->nbr_cells);
	SET_OP(tmp, OP_YFX);
	return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
}

static bool flag_value_error(query *q, cell *p1, cell *p2)
{
	cell *tmp = alloc_on_heap(q, 2+p2->nbr_cells);
	check_heap_error(tmp);
	make_struct(tmp, g_plus_s, bif_iso_add_2, 2, 1+p2->nbr_cells);
	make_atom(tmp+1, p1->val_off);
	dup_cells(tmp+2, p2, p2->nbr_cells);
	SET_OP(tmp, OP_YFX);
	return throw_error(q, tmp, q->st.curr_frame, "domain_error", "flag_value");
}

static bool bif_iso_set_prolog_flag_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (!is_atom(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (!CMP_STRING_TO_CSTR(q, p1, "cpu_count") && is_integer(p2)) {
		g_cpu_count = get_smallint(p2);
		return true;
	}

	if (has_vars(q, p2, p2_ctx))
		return throw_error(q, p2, p2_ctx, "instantiation_error", "var");

	if (!CMP_STRING_TO_CSTR(q, p1, "double_quotes")) {
		if (!CMP_STRING_TO_CSTR(q, p2, "atom")) {
			q->st.m->flags.double_quote_chars = q->st.m->flags.double_quote_codes = false;
			q->st.m->flags.double_quote_atom = true;
		} else if (!CMP_STRING_TO_CSTR(q, p2, "codes")) {
			q->st.m->flags.double_quote_chars = q->st.m->flags.double_quote_atom = false;
			q->st.m->flags.double_quote_codes = true;
		} else if (!CMP_STRING_TO_CSTR(q, p2, "chars")) {
			q->st.m->flags.double_quote_atom = q->st.m->flags.double_quote_codes = false;
			q->st.m->flags.double_quote_chars = true;
		} else {
			return flag_value_error(q, p1, p2);
		}

		q->st.m->p->flags = q->st.m->flags;
	} else if (!CMP_STRING_TO_CSTR(q, p1, "answer_write_options")) {
		cell *l = p2;
		l = deref(q, l, p2_ctx);
		pl_idx l_ctx = q->latest_ctx;

		if (!is_list_or_nil(l))
			return answer_write_options_error(q, l);

		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			pl_idx h_ctx = q->latest_ctx;

			if (!is_compound(h))
				return answer_write_options_error(q, h);

			cell *h1 = h + 1;
			h1 = deref(q, h1, h_ctx);

			if (!CMP_STRING_TO_CSTR(q, h, "max_depth") && (h->arity == 1)) {
				if (!is_integer(h1))
					return answer_write_options_error(q, h);

				if (is_negative(h1))
					return answer_write_options_error(q, h);

				q->pl->def_max_depth = get_smallint(h1);
			} else if (!CMP_STRING_TO_CSTR(q, h, "quoted") && (h->arity == 1)) {
				if (!is_atom(h1))
					return answer_write_options_error(q, h);

				if (!CMP_STRING_TO_CSTR(q, h1, "true") || !CMP_STRING_TO_CSTR(q, h1, "on"))
					q->pl->def_quoted = true;
				else if (!CMP_STRING_TO_CSTR(q, h1, "false") || !CMP_STRING_TO_CSTR(q, h1, "off"))
					q->pl->def_quoted = false;
				else
					return answer_write_options_error(q, h);
			} else if (!CMP_STRING_TO_CSTR(q, h, "double_quotes") && (h->arity == 1)) {
				if (!is_atom(h1))
					return answer_write_options_error(q, h);

				if (!CMP_STRING_TO_CSTR(q, h1, "true") || !CMP_STRING_TO_CSTR(q, h1, "on"))
					q->pl->def_double_quotes = true;
				else if (!CMP_STRING_TO_CSTR(q, h1, "false") || !CMP_STRING_TO_CSTR(q, h1, "off"))
					q->pl->def_double_quotes = false;
				else
					return answer_write_options_error(q, h);
			} else
				return answer_write_options_error(q, h);

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}
	} else if (!CMP_STRING_TO_CSTR(q, p1, "character_escapes")) {
		if (!CMP_STRING_TO_CSTR(q, p2, "true") || !CMP_STRING_TO_CSTR(q, p2, "on"))
			q->st.m->flags.character_escapes = true;
		else if (!CMP_STRING_TO_CSTR(q, p2, "false") || !CMP_STRING_TO_CSTR(q, p2, "off"))
			q->st.m->flags.character_escapes = false;
		else {
			return flag_value_error(q, p1, p2);
		}
	} else if (!CMP_STRING_TO_CSTR(q, p1, "char_conversion")) {
		if (!CMP_STRING_TO_CSTR(q, p2, "true") || !CMP_STRING_TO_CSTR(q, p2, "on"))
			q->st.m->flags.char_conversion = true;
		else if (!CMP_STRING_TO_CSTR(q, p2, "false") || !CMP_STRING_TO_CSTR(q, p2, "off"))
			q->st.m->flags.char_conversion = false;
		else {
			return flag_value_error(q, p1, p2);
		}
	} else if (!CMP_STRING_TO_CSTR(q, p1, "occurs_check")) {
		if (!CMP_STRING_TO_CSTR(q, p2, "true") || !CMP_STRING_TO_CSTR(q, p2, "on"))
			q->st.m->flags.occurs_check = OCCURS_CHECK_TRUE;
		else if (!CMP_STRING_TO_CSTR(q, p2, "false") || !CMP_STRING_TO_CSTR(q, p2, "off"))
			q->st.m->flags.occurs_check = OCCURS_CHECK_FALSE;
		else if (!CMP_STRING_TO_CSTR(q, p2, "error"))
			q->st.m->flags.occurs_check = OCCURS_CHECK_ERROR;
		else {
			return flag_value_error(q, p1, p2);
		}
	} else if (!CMP_STRING_TO_CSTR(q, p1, "debug")) {
		if (!CMP_STRING_TO_CSTR(q, p2, "true") || !CMP_STRING_TO_CSTR(q, p2, "on"))
			q->st.m->flags.debug = true;
		else if (!CMP_STRING_TO_CSTR(q, p2, "false") || !CMP_STRING_TO_CSTR(q, p2, "off"))
			q->st.m->flags.debug = false;
		else {
			return flag_value_error(q, p1, p2);
		}
	} else if (!CMP_STRING_TO_CSTR(q, p1, "strict_iso")) {
		if (!CMP_STRING_TO_CSTR(q, p2, "true") || !CMP_STRING_TO_CSTR(q, p2, "on"))
			q->st.m->flags.strict_iso = true;
		else if (!CMP_STRING_TO_CSTR(q, p2, "false") || !CMP_STRING_TO_CSTR(q, p2, "off"))
			q->st.m->flags.strict_iso = false;
		else {
			return flag_value_error(q, p1, p2);
		}
	} else if (!CMP_STRING_TO_CSTR(q, p1, "unknown")) {
		if (!CMP_STRING_TO_CSTR(q, p2, "fail")) {
			q->st.m->flags.unknown = UNK_FAIL;
		} else if (!CMP_STRING_TO_CSTR(q, p2, "error")) {
			q->st.m->flags.unknown = UNK_ERROR;
		} else if (!CMP_STRING_TO_CSTR(q, p2, "warning")) {
			q->st.m->flags.unknown = UNK_WARNING;
		} else if (!CMP_STRING_TO_CSTR(q, p2, "changeable")) {
			q->st.m->flags.unknown = UNK_CHANGEABLE;
		} else {
			return flag_value_error(q, p1, p2);
		}
	} else if (!CMP_STRING_TO_CSTR(q, p1, "bounded")
		|| !CMP_STRING_TO_CSTR(q, p1, "max_arity")
		|| !CMP_STRING_TO_CSTR(q, p1, "max_integer")
		|| !CMP_STRING_TO_CSTR(q, p1, "min_integer")
		|| !CMP_STRING_TO_CSTR(q, p1, "version")
		|| !CMP_STRING_TO_CSTR(q, p1, "version_data")
		|| !CMP_STRING_TO_CSTR(q, p1, "version_git")
		|| !CMP_STRING_TO_CSTR(q, p1, "encoding")
		|| !CMP_STRING_TO_CSTR(q, p1, "unix")
		|| !CMP_STRING_TO_CSTR(q, p1, "threads")
#if USE_THREADS
		|| !CMP_STRING_TO_CSTR(q, p1, "hardware_threads")
		|| !CMP_STRING_TO_CSTR(q, p1, "max_threads")
#endif
		|| !CMP_STRING_TO_CSTR(q, p1, "verbose")
		|| !CMP_STRING_TO_CSTR(q, p1, "integer_rounding_function")
		|| !CMP_STRING_TO_CSTR(q, p1, "dialect")
		|| !CMP_STRING_TO_CSTR(q, p1, "pid")
		) {
		return throw_error(q, p1, p1_ctx, "permission_error", "modify,flag");
	} else if (!CMP_STRING_TO_CSTR(q, p1, "generate_debug_info")) {
	} else {
		return throw_error(q, p1, p1_ctx, "domain_error", "prolog_flag");
	}

	q->flags = q->st.m->flags;
	return true;
}

static cell *convert_to_list(query *q, cell *c, pl_idx nbr_cells)
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

static bool bif_sys_list_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell *l = convert_to_list(q, get_queue(q), queue_used(q));
	return unify(q, p1, p1_ctx, l, q->st.curr_frame);
}

bool bif_sys_queue_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	check_heap_error(init_tmp_heap(q), q->st.qnbr--);
	cell *tmp = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(tmp, q->st.qnbr--);
	check_heap_error(alloc_on_queuen(q, q->st.qnbr, tmp), q->st.qnbr--);
	return true;
}

static bool do_op(query *q, cell *p3, pl_idx p3_ctx)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);

	if (!is_atom(p3))
		return throw_error(q, p3, p3_ctx, "type_error", "atom");

	unsigned specifier, pri = get_smallint(p1);

	if (!CMP_STRING_TO_CSTR(q, p2, "fx"))
		specifier = OP_FX;
	else if (!CMP_STRING_TO_CSTR(q, p2, "fy"))
		specifier = OP_FY;
	else if (!CMP_STRING_TO_CSTR(q, p2, "xf"))
		specifier = OP_XF;
	else if (!CMP_STRING_TO_CSTR(q, p2, "xfx"))
		specifier = OP_XFX;
	else if (!CMP_STRING_TO_CSTR(q, p2, "xfy"))
		specifier = OP_XFY;
	else if (!CMP_STRING_TO_CSTR(q, p2, "yf"))
		specifier = OP_YF;
	else if (!CMP_STRING_TO_CSTR(q, p2, "yfx"))
		specifier = OP_YFX;
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "operator_specifier");

	if (pri && !CMP_STRING_TO_CSTR(q, p3, "|") && (!IS_INFIX(specifier) || (pri < 1001)))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_STRING_TO_CSTR(q, p3, "[]"))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_STRING_TO_CSTR(q, p3, "{}"))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!CMP_STRING_TO_CSTR(q, p3, ","))
		return throw_error(q, p3, p3_ctx, "permission_error", "modify,operator");

	unsigned tmp_optype = 0;
	unsigned tmp_pri = search_op(q->st.m, C_STR(q, p3), &tmp_optype, false);

	if (IS_INFIX(specifier) && IS_POSTFIX(tmp_optype))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!tmp_pri && !pri)
		return true;

	tmp_pri = find_op(q->st.m, C_STR(q, p3), OP_FX);

	if (IS_POSTFIX(specifier) && (IS_INFIX(tmp_optype)/* || tmp_pri*/))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	tmp_pri = find_op(q->st.m, C_STR(q, p3), OP_FY);

	if (IS_POSTFIX(specifier) && (IS_INFIX(tmp_optype) || tmp_pri))
		return throw_error(q, p3, p3_ctx, "permission_error", "create,operator");

	if (!set_op(q->st.m, C_STR(q, p3), specifier, pri))
		return throw_error(q, p3, p3_ctx, "resource_error", "too_many_ops");

	return true;
}

static bool bif_iso_op_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,list_or_atom);
	if (is_negative(p1) || is_gt(p1,1200))
		return throw_error(q, p1, p1_ctx, "domain_error", "operator_priority");

	q->ops_dirty = true;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
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

static bool bif_instance_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
	uuid u;
	uuid_from_buf(C_STR(q, p1), &u);
	rule *r = find_in_db(q->st.m, &u);
	check_heap_error(r);
	return unify(q, p2, p2_ctx, r->cl.cells, q->st.curr_frame);
}

static bool bif_listing_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	save_db(str->fp, q, 0);
	return true;
}

static void save_name(FILE *fp, query *q, pl_idx name, unsigned arity)
{
	module *m = q->st.r ? q->st.r->owner->m : q->st.m;
	q->listing = true;

	for (predicate *pr = (predicate*)list_front(&m->predicates);
		pr; pr = (predicate*)list_next(pr)) {
		if (pr->is_prebuilt && (arity == -1U))
			continue;

		if (name != pr->key.val_off)
			continue;

		if ((arity != pr->key.arity) && (arity != -1U))
			continue;

		for (rule *r = pr->head; r; r = r->next) {
			if (r->cl.dbgen_erased)
				continue;

			for (unsigned i = 0; i < MAX_IGNORES; i++)
				q->ignores[i] = false;

			q->print_idx = 0;
			print_term(q, fp, r->cl.cells, 0, 0);
			fprintf(fp, ".\n");
		}
	}

	q->listing = false;
}

static bool bif_listing_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	pl_idx name = p1->val_off;
	unsigned arity = -1;

	if (p1->val_off == g_colon_s) {
		p1 = p1 + 1;
		cell *cm = deref(q, p1, p1_ctx);
		module *m = find_module(q->pl, C_STR(q, cm));

		if (!m)
			return throw_error(q, cm, p1_ctx, "existence_error", "module");

		p1 += p1->nbr_cells;
	}

	if (p1->arity) {
		if (CMP_STRING_TO_CSTR(q, p1, "/") && CMP_STRING_TO_CSTR(q, p1, "//"))
			return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

		cell *p2 = p1 + 1;

		if (!is_atom(p2))
			return throw_error(q, p2, p1_ctx, "type_error", "atom");

		cell *p3 = p2 + p2->nbr_cells;

		if (!is_integer(p3))
			return throw_error(q, p3, p1_ctx, "type_error", "integer");

		name = new_atom(q->pl, C_STR(q, p2));
		arity = get_smallint(p3);

		if (!CMP_STRING_TO_CSTR(q, p1, "//"))
			arity += 2;
	}

	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	save_name(str->fp, q, name, arity);
	return true;
}

static bool bif_help_0(query *q)
{
	sliter *iter = sl_first(q->pl->help);
	builtins *fn;

	while (sl_next(iter, (void**)&fn)) {
		if (fn->arity)
			fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		else
			fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
	}

	return true;
}

static bool bif_module_info_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	const char *name = C_STR(q, p1);
	module *m = find_module(q->pl, name);

	if (!m)
		return false;

	check_heap_error(init_tmp_heap(q));

	for (predicate *pr = (predicate*)list_front(&m->predicates);
		pr; pr = (predicate*)list_next(pr)) {
		if (!pr->is_public)
			continue;

		cell tmp[3];
		make_struct(tmp+0, g_slash_s, NULL, 2, 2);
		SET_OP(tmp, OP_YFX);
		make_atom(tmp+1, pr->key.val_off);
		make_int(tmp+2, pr->key.arity);
		append_list(q, tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool bif_source_info_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,var);

	if (!is_compound(p1))
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

	cell key;
	key.val_off = f->val_off;
	key.arity = get_smalluint(a);
	predicate *pr = find_predicate(q->st.m, &key);

	if (!pr || pr->is_dynamic)
		return false;

	check_heap_error(init_tmp_heap(q));

	for (rule *r = pr->head; r; r = r->next) {
		cell tmp[8];
		make_struct(tmp+0, g_dot_s, NULL, 2, 7);
		make_struct(tmp+1, new_atom(q->pl, "filename"), NULL, 1, 1);
		make_cstring(tmp+2, r->filename);
		make_struct(tmp+3, g_dot_s, NULL, 2, 4);
		make_struct(tmp+4, new_atom(q->pl, "lines"), NULL, 2, 2);
		make_uint(tmp+5, r->line_nbr_start);
		make_uint(tmp+6, r->line_nbr_end);
		make_atom(tmp+7, g_nil_s);
		append_list(q, tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool bif_help_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	bool found = false, evaluable = false;

	if (!p1->arity) {
		if (!is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		const char *functor = C_STR(q, p1);
		sliter *iter = sl_find_key(q->pl->help, functor);
		builtins *fn;

		while (sl_next_key(iter, (void**)&fn)) {
			if (fn->help_alt) {
				if (fn->arity)
					fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help_alt ? fn->help_alt : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
			}

			if (fn->arity)
				fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
			else
				fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		}

		return true;
	}

	if (!is_compound(p1))
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
		return false;

	if (fn->help_alt) {
		if (fn->arity)
			fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help_alt ? fn->help_alt : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
	}

	if (arity)
		fprintf(stdout, "%s/%u: %s(%s)%s%s\n%s\n", fn->name, arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"", fn->desc?fn->desc:"");
	else
		fprintf(stdout, "%s/%u: %s%s%s\n%s\n", fn->name, arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"", fn->desc?fn->desc:"");

	return true;
}

static bool bif_help_2(query *q)
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
		sliter *iter = sl_find_key(q->pl->help, functor);
		builtins *fn;

		while (sl_next_key(iter, (void**)&fn)) {
			if (fn->arity)
				fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
			else
				fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		}

		return true;
	}

	if (!is_compound(p1))
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
		return false;

	if (!strcmp(pr, "swi"))
		snprintf(url, sizeof(url), "http://swi-prolog.org/pldoc/man?predicate=%s/%u", functor, arity);
	else if (!strcmp(pr, "tau"))
		snprintf(url, sizeof(url), "http://tau-prolog.org/documentation/prolog/builtin/%s/%u", functor, arity);

	if (arity)
		fprintf(stdout, "%s/%u: %s\n", fn->name, arity, url);
	else
		fprintf(stdout, "%s/%u: %s\n", fn->name, arity, url);

	return true;
}

static bool bif_module_help_1(query *q)
{
	GET_FIRST_ARG(pm,atom);
	module *m = find_module(q->pl, C_STR(q, pm));

	if (!m)
		return false;

	sliter *iter = sl_first(q->pl->help);
	builtins *fn;

	while (sl_next(iter, (void**)&fn)) {
		if (fn->m != m)
			continue;

		if (fn->arity)
			fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		else
			fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
	}

	return true;
}

static bool bif_module_help_2(query *q)
{
	GET_FIRST_ARG(pm,atom);
	GET_NEXT_ARG(p1,any);
	bool found = false, evaluable = false;
	module *m = find_module(q->pl, C_STR(q, pm));

	if (!m)
		return false;

	if (!p1->arity) {
		if (!is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		const char *functor = C_STR(q, p1);
		sliter *iter = sl_find_key(q->pl->help, functor);
		builtins *fn;

		while (sl_next_key(iter, (void**)&fn)) {
			if (fn->m != m)
				continue;

			if (fn->arity)
				fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
			else
				fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		}

		return true;
	}

	if (!is_compound(p1))
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
	builtins *fn = get_module_help(m, functor, arity, &found, &evaluable);

	if (!found || !fn)
		return false;

	if (arity)
		fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
	else
		fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");

	return true;
}

static bool bif_module_help_3(query *q)
{
	GET_FIRST_ARG(pm,atom);
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,atom);
	bool found = false, evaluable = false;
	const char *pr = C_STR(q, p2);
	char url[1024];
	module *m = find_module(q->pl, C_STR(q, pm));

	if (!m)
		return false;

	if (!p1->arity) {
		if (!is_atom(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		const char *functor = C_STR(q, p1);
		sliter *iter = sl_find_key(q->pl->help, functor);
		builtins *fn;

		while (sl_next_key(iter, (void**)&fn)) {
			if (fn->m != m)
				continue;

			if (fn->arity)
				fprintf(stdout, "%s/%u: %s(%s)%s%s\n", fn->name, fn->arity, fn->name, fn->help ? fn->help : "no args", fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
			else
				fprintf(stdout, "%s/%u: %s%s%s\n", fn->name, fn->arity, fn->name, fn->iso?" [ISO]":"", fn->evaluable?" [EVALUABLE]":"");
		}

		return true;
	}

	if (!is_compound(p1))
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
	builtins *fn = get_module_help(m, functor, arity, &found, &evaluable);

	if (!found || !fn)
		return false;

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
	return print_term_to_strbuf(q, c, q->st.curr_frame, 0);
}

static bool bif_sys_first_non_octet_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,integer_or_var);
	unsigned len = C_STRLEN_UTF8(p1);
	const char *src = C_STR(q, p1);

	for (unsigned i = 0; i < len; i++) {
		int ch = get_char_utf8(&src);

		if (ch > 255) {
			cell tmp;
			make_uint(&tmp, i);
			return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		}
	}

	return false;
}

static bool bif_sys_timer_0(query *q)
{
	q->st.timer_started = get_time_in_usec();
	q->tot_inferences = 0;
	return true;
}

static bool bif_sys_elapsed_0(query *q)
{
	q->tot_inferences--;
	uint64_t elapsed = get_time_in_usec();
	elapsed -= q->st.timer_started;
	double lips = (1.0 / ((double)elapsed/1000/1000)) * q->tot_inferences;
	fprintf(stderr, "%% Time elapsed %.3fs, %llu Inferences, %.3f MLips\n", (double)elapsed/1000/1000, (unsigned long long)q->tot_inferences, lips/1000/1000);
	if (q->is_redo) fprintf(stdout, "  ");
	return true;
}

static bool bif_time_1(query *q)
{
	if (q->retry) {
		bif_sys_elapsed_0(q);
		return false;
	}

	bif_sys_timer_0(q);
	GET_FIRST_ARG(p1,callable);
	cell *tmp = prepare_call(q, true, p1, p1_ctx, 4);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_elapsed_s, bif_sys_elapsed_0, 0, 0);
	make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	q->st.curr_instr = tmp;
	return true;
}

static bool bif_trace_0(query *q)
{
	q->trace = !q->trace;
	return true;
}

static bool do_profile(query *q)
{
	fprintf(stderr, "#functor/arity,match_attempts,matched,tcos\n");

	for (module *m = (module*)list_front(&q->pl->modules);
		m; m = (module*)list_next(m)) {
		for (predicate *pr = (predicate*)list_front(&m->predicates);
			pr; pr = (predicate*)list_next(pr)) {
			for (rule *r = pr->head; r; r = r->next) {
				if (!r->attempted)
					continue;

				fprintf(stderr, "'%s/%u',%llu,%llu,%llu\n",
					C_STR(q, &pr->key), pr->key.arity,
					(unsigned long long)r->attempted, (unsigned long long)r->matched, (unsigned long long)r->tcos);
			}
		}
	}

	return true;
}

static bool bif_statistics_0(query *q)
{
	fprintf(stdout,
		"Goals %"PRIu64", "
		"Matches %"PRIu64".\n"
		"Max frames %u, "
		"choices %u, "
		"trails %u, "
		"slots %u, "
		"heap pages %u.\n"
		"Active frames %u, "
		"choices %u, "
		"trails %u, "
		"slots %u, "
		"heap pages %u.\n"
		"Backtracks %"PRIu64", "
		"TCOs:%"PRIu64", "
		"Recovered frames: %"PRIu64", "
		"slots: %"PRIu64", "
		"Queue: %u\n",
		q->tot_inferences, q->tot_matches,
		q->hw_frames, q->hw_choices, q->hw_trails, q->hw_slots, q->hw_heap_nbr,
		q->st.fp, q->cp, q->st.tp, q->st.sp,
		q->st.heap_nbr, q->tot_retries, q->tot_tcos,
		q->tot_frecovs, q->tot_srecovs, (unsigned)q->qcnt[q->st.qnbr]
		);
	return true;
}

static bool bif_statistics_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_var);

	if (!CMP_STRING_TO_CSTR(q, p1, "cputime") && is_var(p2)) {
		uint64_t now = cpu_time_in_usec();
		double elapsed = now - q->cpu_started;
		cell tmp;
		make_float(&tmp, elapsed/1000/1000);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "gctime") && is_var(p2)) {
		cell tmp;
		make_float(&tmp, 0);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "profile") && is_var(p2)) {
		return do_profile(q);
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "wall") && is_var(p2)) {
		uint64_t now = get_time_in_usec();
		cell tmp;
		make_uint(&tmp, now/1000);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "runtime")) {
		uint64_t now = cpu_time_in_usec();
		double elapsed = now - q->cpu_started;
		cell tmp;
		make_int(&tmp, elapsed/1000);
		allocate_list(q, &tmp);
		elapsed = now - q->time_cpu_last_started;
		q->time_cpu_last_started = now;
		make_uint(&tmp, elapsed/1000);
		append_list(q, &tmp);
		cell *l = end_list(q);
		check_heap_error(l);
		return unify(q, p2, p2_ctx, l, q->st.curr_frame);
	}

	return false;
}

static bool bif_sys_msleep_1(query *q)
{
	if (q->retry)
		return true;

	GET_FIRST_ARG(p1,number);

	if (is_negative(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (q->is_task)
		return do_yield(q, get_smallint(p1));

	int ms = is_float(p1) ? (int)get_float(p1) : get_smallint(p1);

	while ((ms > 0) && !q->halt) {
		CHECK_INTERRUPT();
		msleep(ms > 10 ? 10 : ms);
		ms -= 10;
	}

	return true;
}

static bool bif_busy_1(query *q)
{
	GET_FIRST_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	pl_int elapse = get_smallint(p1);

	if (elapse < 0)
		return true;

	// Limit to 60 seconds...

	if (elapse > (60 * 1000))
		return true;

	pl_uint started = get_time_in_usec() / 1000;
	pl_uint end = started + elapse;

	while ((get_time_in_usec() / 1000)  < end) {
		CHECK_INTERRUPT();
	}

	return true;
}

static bool bif_now_0(query *q)
{
	pl_int secs = get_time_in_usec() / 1000 / 1000;
	q->accum.tag = TAG_INTEGER;
	set_smallint(&q->accum, secs);
	return true;
}

static bool bif_now_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	pl_int secs = get_time_in_usec() / 1000 / 1000;
	cell tmp;
	make_int(&tmp, secs);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_get_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	double v = ((double)get_time_in_usec()-q->get_started) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (double)v);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_cpu_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	double v = ((double)cpu_time_in_usec()-q->cpu_started) / 1000 / 1000;
	cell tmp;
	make_float(&tmp, (pl_flt)v);
	return unify (q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

bool bif_sys_set_if_var_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	if (!is_var(p1))
		return true;

	return unify(q, p1, p1_ctx, p2, p2_ctx);
}

static bool bif_between_3(query *q)
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

static bool bif_split_string_4(query *q)
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
	int nbr = 1;

	if (!*start)
		return unify(q, p4, p4_ctx, make_nil(), q->st.curr_frame);

	check_heap_error(init_tmp_heap(q));

	// FIXME: sep & pad are not a single char...

	while ((ptr = strchr_utf8(start, sep)) != NULL) {
		while ((peek_char_utf8(start) == pad) && (pad != sep))
			get_char_utf8(&start);

		if (ptr-start) {
			cell tmp;
			check_heap_error(make_slice(q, &tmp, p1, start-src, ptr-start));
			append_list(q, &tmp);
		}

		start = ptr + 1;
	}

	if (*start) {
		while (peek_char_utf8(start) == pad)
			get_char_utf8(&start);

		cell tmp;
		check_heap_error(make_slice(q, &tmp, p1, start-src, C_STRLEN(q, p1)-(start-src)));

		if (C_STRLEN(q, p1)-(start-src))
			append_list(q, &tmp);
	}

	l = end_list(q);
	check_heap_error(l);
	return unify(q, p4, p4_ctx, l, q->st.curr_frame);
}

static bool bif_split_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);

	if (is_nil(p1) || !C_STRLEN(q, p1)) {
		if (!unify(q, p3, p3_ctx, make_nil(), q->st.curr_frame))
			return false;

		return unify(q, p4, p4_ctx, make_nil(), q->st.curr_frame);
	}

	const char *start = C_STR(q, p1), *ptr;
	int ch = peek_char_utf8(C_STR(q, p2));

	if ((ptr = strchr_utf8(start, ch)) != NULL) {
		cell tmp;

		if (ptr != start)
			make_stringn(&tmp, start, ptr-start);
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
			make_stringn(&tmp, ptr, C_STRLEN(q, p1)-(ptr-start));
		else
			make_atom(&tmp, g_nil_s);

		bool ok = unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!unify(q, p3, p3_ctx, p1, p1_ctx))
		return false;

	return unify(q, p4, p4_ctx, make_nil(), q->st.curr_frame);
}

static bool bif_sys_is_partial_string_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!is_iso_list(p1))
		return false;

	bool has_var, is_partial;
	scan_is_chars_list2(q, p1, p1_ctx, true, &has_var, &is_partial);
	return is_partial;
}

static bool bif_is_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	bool is_partial;
	return check_list(q, p1, p1_ctx, &is_partial, NULL);
}

static bool bif_is_partial_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_var(p1))
		return true;

	bool is_partial;

	if (check_list(q, p1, p1_ctx, &is_partial, NULL))
		return false;

	return is_partial;
}

static bool bif_is_list_or_partial_list_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (is_var(p1))
		return true;

	bool is_partial;

	if (check_list(q, p1, p1_ctx, &is_partial, NULL))
		return true;

	return is_partial;
}

static bool bif_must_be_4(query *q)
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
			return throw_error2(q, p1, p1_ctx, "instantiation_error", "ground", p3);
	} else if (!strcmp(src, "compound") && !is_structure(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "compound", p3);
	else if (is_compound(p2) && (p2->arity == 1) && !strcmp(src, "list")) {
		cell *c = p2+1;
		c = deref(q, c, p2_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (!is_atom(c))
			return throw_error(q, c, c_ctx, "type_error", "atom");

		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL))
			return throw_error(q, p1, p1_ctx, "type_error", "list");

		cell *l = p1;
		pl_idx l_ctx = p1_ctx;
		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			pl_idx h_ctx = q->latest_ctx;
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
				return throw_error(q, h, h_ctx, "instantiation_error", "ground");
			else if (!strcmp(src, "compound" ) && !is_structure(h))
				return throw_error(q, h, h_ctx, "type_error", "compound");

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}
	} else if (!strcmp(src, "list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL)) {
			if (is_partial)
				return throw_error(q, p1, p1_ctx, "instantiation_error", "list");
			else
				return throw_error(q, p1, p1_ctx, "type_error", "list");
		}
	} else if (!strcmp(src, "list_or_partial_list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
			return throw_error2(q, p1, p1_ctx, "type_error", "list", p3);
	}

	return true;
}

static bool do_must_be_2(query *q, cell *p2, pl_idx p2_ctx, cell *p1, pl_idx p1_ctx)
{
	const char *src = C_STR(q, p2);

	if (!strcmp(src, "var") && !is_var(p1))
		return throw_error(q, p1, p1_ctx, "uninstantiation_error", "not_sufficiently_instantiated");
	else if (!strcmp(src, "nonvar") && is_var(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "instantiated");

	if (strcmp(src, "var") && strcmp(src, "ground") && strcmp(src, "acyclic") && is_var(p1))
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
			return throw_error(q, p1, p1_ctx, "instantiation_error", "ground");
	} else if (!strcmp(src, "compound")) {
		if (!is_structure(p1))
			return throw_error(q, p1, p1_ctx, "type_error", "compound");
	} else if (is_compound(p2) && (p2->arity == 1) && !strcmp(src, "list")) {
		cell *c = p2+1;
		c = deref(q, c, p2_ctx);
		pl_idx c_ctx = q->latest_ctx;
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL))
			return throw_error(q, p1, p1_ctx, "type_error", "list");

		cell *l = p1;
		pl_idx l_ctx = p1_ctx;
		LIST_HANDLER(l);

		while (is_iso_list(l)) {
			cell *h = LIST_HEAD(l);
			h = deref(q, h, l_ctx);
			pl_idx h_ctx = q->latest_ctx;

			if (!do_must_be_2(q, c, c_ctx, h, h_ctx))
				return false;

			l = LIST_TAIL(l);
			l = deref(q, l, l_ctx);
			l_ctx = q->latest_ctx;
		}
	} else if (!strcmp(src, "list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL)) {
			if (is_partial)
				return throw_error(q, p1, p1_ctx, "instantiation_error", "list");
			else
				return throw_error(q, p1, p1_ctx, "type_error", "list");
		}
	} else if (!strcmp(src, "list_or_partial_list")) {
		bool is_partial;

		if (!check_list(q, p1, p1_ctx, &is_partial, NULL) && !is_partial)
			return throw_error(q, p1, p1_ctx, "type_error", "list");
	}

	return true;
}

static bool bif_must_be_2(query *q)
{
	GET_FIRST_ARG(p2,callable);
	GET_NEXT_ARG(p1,any);
	return do_must_be_2(q, p2, p2_ctx, p1, p1_ctx);
}

static bool bif_can_be_4(query *q)
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
	else if (!strcmp(src, "compound") && !is_structure(p1))
		return throw_error2(q, p1, p1_ctx, "type_error", "compound", p3);
	else if (!strcmp(src, "term") && is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, p1_ctx, "type_error", "term");
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

static bool bif_can_be_2(query *q)
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
	else if (!strcmp(src, "compound") && !is_structure(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "compound");
	else if (!strcmp(src, "term") && is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, p1_ctx, "type_error", "term");
	else if (!strcmp(src, "term") && is_cyclic_term(q, p1, p1_ctx))
		return throw_error(q, p1, p1_ctx, "type_error", "term");
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

static bool bif_sys_skip_max_list_4(query *q)
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

	pl_int skip=0, max = is_smallint(p2) ? get_smallint(p2) : PL_INT_MAX;
	pl_idx c_ctx = p3_ctx;
	cell tmp = {0};
	cell *c = skip_max_list(q, p3, &c_ctx, max, &skip, &tmp);

	if (!c) {
		c_ctx = p3_ctx;
		c = p3;
	}

	bool ok = unify(q, p4, p4_ctx, c, c_ctx);

	if (ok != true)
		return ok;


	unshare_cell(&tmp);

	if (!is_iso_list_or_nil(c) && !(is_cstring(c) && !strcmp(C_STR(q,c), "[]")) && !is_var(c)) {
		make_int(&tmp, -1);
		unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	make_int(&tmp, skip);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_wall_time_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, time(NULL));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_date_time_7(query *q)
{
	GET_FIRST_ARG(p1,var);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,var);
	GET_NEXT_ARG(p5,var);
	GET_NEXT_ARG(p6,var);
	GET_NEXT_ARG(p7,var);
	struct timeval cur_time;
	gettimeofday(&cur_time, NULL);
	struct tm tm = {0};
	localtime_r((const time_t*)&cur_time.tv_sec, &tm);
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
	make_int(&tmp, cur_time.tv_usec/1000);
	unify(q, p7, p7_ctx, &tmp, q->st.curr_frame);
	return true;
}

static bool bif_date_time_6(query *q)
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
static bool bif_shell_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	int status = system(C_STR(q, p1));
	if (status == 0)
		return true;
	else
		return false;
}

static bool bif_shell_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	int status = system(C_STR(q, p1));
	cell tmp;
	make_int(&tmp, status);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}
#else
static bool bif_shell_1(query *q)
{
	return false;
}

static bool bif_shell_2(query *q)
{
	return false;
}
#endif

// FIXME: not truly crypto strength

static bool bif_crypto_n_random_bytes_2(query *q)
{
	static bool s_seed = false;

	if (!s_seed) {
		srand(time(NULL));
		s_seed = true;
	}

	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,list_or_var);
	int n = get_smallint(p1);

	if (n < 1)
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	check_heap_error(init_tmp_heap(q));

	while (n--) {
		int i = rand() % 256;
		cell tmp;
		make_int(&tmp, i);
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

#if USE_OPENSSL
static bool bif_crypto_data_hash_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p3,list_or_nil);
	enum {is_sha256, is_sha384, is_sha512} algo;
	algo = is_sha256;
	char *key = NULL;
	int keylen = 0;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		h = deref(q, h, p3_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (is_compound(h) && (h->arity == 1)) {
			cell *arg = h+1;
			arg = deref(q, arg, h_ctx);
			pl_idx arg_ctx = q->latest_ctx;

			if (!CMP_STRING_TO_CSTR(q, h, "algorithm")) {
				if (is_var(arg)) {
					cell tmp;
					make_atom(&tmp, new_atom(q->pl, "sha256"));
					unify(q, arg, arg_ctx, &tmp, q->st.curr_frame);
					algo = is_sha256;
				} else if (!CMP_STRING_TO_CSTR(q, arg, "sha256")) {
					algo = is_sha256;
				} else if (!CMP_STRING_TO_CSTR(q, arg, "sha384")) {
					algo = is_sha384;
				} else if (!CMP_STRING_TO_CSTR(q, arg, "sha512")) {
					algo = is_sha512;
				} else
					return throw_error(q, arg, arg_ctx, "domain_error", "algorithm");
			} else if (!CMP_STRING_TO_CSTR(q, h, "hmac") && is_iso_list(arg)
				&& (keylen = scan_is_chars_list(q, arg, 0, true)) > 0) {
				key = chars_list_to_string(q, arg, 0, keylen);
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

	if (key && (algo == is_sha256)) {
		unsigned char digest[SHA256_DIGEST_LENGTH];
		unsigned digest_len = 0;
		HMAC(EVP_sha256(), key, keylen, (unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest, &digest_len);

		for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (key && (algo == is_sha384)) {
		unsigned char digest[SHA384_DIGEST_LENGTH];
		unsigned digest_len = 0;
		HMAC(EVP_sha384(), key, keylen, (unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest, &digest_len);

		for (int i = 0; i < SHA384_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (key && (algo == is_sha512)) {
		unsigned char digest[SHA512_DIGEST_LENGTH];
		unsigned digest_len = 0;
		HMAC(EVP_sha512(), key, keylen, (unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest, &digest_len);

		for (int i = 0; i < SHA512_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (algo == is_sha256) {
		unsigned char digest[SHA256_DIGEST_LENGTH];
		SHA256((unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest);

		for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (algo == is_sha384) {
		unsigned char digest[SHA384_DIGEST_LENGTH];
		SHA384((unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest);

		for (int i = 0; i < SHA384_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	} else if (algo == is_sha512) {
		unsigned char digest[SHA512_DIGEST_LENGTH];
		SHA512((unsigned char*)C_STR(q, p1), C_STRLEN(q, p1), digest);

		for (int i = 0; i < SHA512_DIGEST_LENGTH; i++) {
			size_t len = snprintf(dst, buflen, "%02x", digest[i]);
			dst += len;
			buflen -= len;
		}
	}

	if (key)
		free(key);

	cell tmp;
	make_string(&tmp, tmpbuf);
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
	make_string(&tmp, dstbuf);
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
	make_string(&tmp, dstbuf);
	free(dstbuf);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_base64_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_list(p1)) && is_var(p2))
		return do_b64encode_2(q);
	else if (is_var(p1) && (is_atom(p2) || is_string(p2)))
		return do_b64decode_2(q);

	return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");
}

char *url_encode(const char *src, int len, char *dstbuf)
{
	char *dst = dstbuf;

	// As per RFC3986 (2005)

	while (len-- > 0) {
		if (*src == ' ') {
			*dst++ = '+';
			src++;
		} else if (!isalnum(*src) && (*src != '-') && (*src != '_') && (*src != '.') && (*src != '~')) {
			const unsigned char* src2 = (unsigned char*)src;
			dst += sprintf(dst, "%%%02X", *src2);
			src++;
		} else
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
		make_string(&tmp, dstbuf);
	else
		make_cstring(&tmp, dstbuf);

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
		make_string(&tmp, dstbuf);
	else
		make_cstring(&tmp, dstbuf);

	free(dstbuf);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_urlenc_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if ((is_atom(p1) || is_string(p1)) && is_var(p2))
		return do_urlencode_2(q);
	else if (is_var(p1) && (is_atom(p2) || is_string(p2)))
		return do_urldecode_2(q);

	return throw_error(q, p1, p1_ctx, "instantiation_error", "atom");
}

static bool bif_atom_lower_2(query *q)
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
	make_cstringn(&tmp, tmps, C_STRLEN(q, p1));
	free(tmps);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_atom_upper_2(query *q)
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
	make_cstringn(&tmp, tmps, C_STRLEN(q, p1));
	free(tmps);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_string_lower_2(query *q)
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
	make_stringn(&tmp, tmps, C_STRLEN(q, p1));
	free(tmps);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_string_upper_2(query *q)
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
	make_stringn(&tmp, tmps, C_STRLEN(q, p1));
	free(tmps);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static pl_idx jenkins_one_at_a_time_hash(const char *key, size_t len)
{
	pl_idx hash = 0;

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

static bool bif_term_hash_2(query *q)
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

static bool bif_hex_chars_2(query *q)
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
			check_heap_error(dst);
			mp_int_to_string(&p1->val_bigint->ival, 16, dst, len+1);
		} else {
			snprintf(tmpbuf, sizeof(tmpbuf), "%"PRIx64"", (uint64_t)get_smallint(p1));
		}

		cell tmp;
		make_string(&tmp, dst);
		if (dst != tmpbuf) free(dst);
		bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	char *src = DUP_STRING(q, p2);
	const char *s = src;
	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;
	read_integer(q->p, &v2, 16, &s);
	free(src);
	cell tmp = {0};

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		tmp.tag = TAG_INTEGER;
		tmp.val_bigint = malloc(sizeof(bigint));
		check_heap_error(tmp.val_bigint);
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

static bool bif_octal_chars_2(query *q)
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
			check_heap_error(dst);
			mp_int_to_string(&p1->val_bigint->ival, 8, dst, len+1);
		} else {
			snprintf(tmpbuf, sizeof(tmpbuf), "%"PRIo64"", (uint64_t)get_smallint(p1));
		}

		cell tmp;
		make_string(&tmp, dst);
		if (dst != tmpbuf) free(dst);
		bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	char *src = DUP_STRING(q, p2);
	const char *s = src;
	mpz_t v2;
	mp_int_init(&v2);
	mp_small val;
	read_integer(q->p, &v2, 16, &s);
	free(src);
	cell tmp = {0};

	if (mp_int_to_int(&v2, &val) == MP_RANGE) {
		tmp.tag = TAG_INTEGER;
		tmp.val_bigint = malloc(sizeof(bigint));
		check_heap_error(tmp.val_bigint);
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

static bool bif_atom_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_string(p1);
}

static bool bif_getenv_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	const char *value = getenv(C_STR(q, p1));

	if (!value)
		return false;

	cell tmp;

	if (is_string(p1))
		make_string(&tmp, value);
	else
		make_cstring(&tmp, value);

	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_setenv_2(query *q)
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

static bool bif_unsetenv_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	unsetenv(C_STR(q, p1));
	return true;
}

static bool bif_uuid_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	uuid u;
	uuid_gen(q->pl, &u);
	char tmpbuf[128];
	uuid_to_buf(&u, tmpbuf, sizeof(tmpbuf));
	cell tmp;
	make_string(&tmp, tmpbuf);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_atomic_concat_3(query *q)
{
	GET_FIRST_ARG(p1,atomic);
	GET_NEXT_ARG(p2,atomic);
	GET_NEXT_ARG(p3,atom_or_var);
	char *src1 = print_term_to_strbuf(q, p1, p1_ctx, 1);
	char *src2 = print_term_to_strbuf(q, p2, p2_ctx, 1);
	SB(pr);
	SB_strcat(pr, src1);
	SB_strcat(pr, src2);
	free(src1);
	free(src2);
	cell tmp;
	make_cstringn(&tmp, SB_cstr(pr), SB_strlen(pr));
	SB_free(pr);
	bool ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_atomic_list_concat_3(query *q)
{
	GET_FIRST_ARG(p1,iso_list_or_nil);
	GET_NEXT_ARG(p2,atomic);
	GET_NEXT_ARG(p3,atom_or_var);
	LIST_HANDLER(p1);
	SB(pr);

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
	make_cstring(&tmp, SB_cstr(pr));
	SB_free(pr);
	bool ok = unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_replace_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom);
	GET_NEXT_ARG(p4,var);
	size_t srclen = C_STRLEN(q, p1);
	const char *src = C_STR(q, p1);
	const char *s1 = C_STR(q, p2);
	const char *s2 = C_STR(q, p3);
	size_t s1len = C_STRLEN(q, p2);
	size_t s2len = C_STRLEN(q, p3);
	SB(pr);

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
		make_stringn(&tmp, SB_cstr(pr), SB_strlen(pr));
	else
		make_atom(&tmp, g_nil_s);

	SB_free(pr);
	bool ok = unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static void load_properties(module *m);

static bool bif_sys_load_properties_0(query *q)
{
	load_properties(q->st.m);
	return true;
}

static void load_flags(query *q);

static bool bif_sys_load_flags_0(query *q)
{
	load_flags(q);
	return true;
}

static void load_ops(query *q);

static bool bif_sys_load_ops_0(query *q)
{
	load_ops(q);
	return true;
}

static bool bif_sys_ops_dirty_0(query *q)
{
	bool ok = q->ops_dirty;
	q->ops_dirty = false;
	return ok;
}

static void do_template(char *tmpbuf, const char *name, unsigned arity, const char *help, bool function, bool quote)
{
	SB(t);

	if (quote) {
		SB_sprintf(t, "template('%s'", name);
	} else {
		SB_sprintf(t, "template(%s", name);
	}

	if (arity)
		SB_strcat(t, "(");

	char tmpbuf1[256], tmpbuf2[256], tmpbuf3[256];
	const char *src = help + (function?1:0);

	for (unsigned i = 0; i < arity; i++) {
		sscanf(src, "%255[^,],%s255", tmpbuf1, tmpbuf2);
		tmpbuf1[sizeof(tmpbuf1)-1] = tmpbuf2[sizeof(tmpbuf2)-1] = 0;

		if (i > 0)
			SB_strcat(t, ",");

		SB_strcat(t, tmpbuf1);
		memcpy(tmpbuf3, tmpbuf2, sizeof(tmpbuf3));
		src = tmpbuf3 + (function?1:0);
	}

	if (arity)
		SB_strcat(t, ")");

	if (function) {
		SB_sprintf(t, ",%s", src);
	}

	strcpy(tmpbuf, SB_cstr(t));
	SB_free(t);
}

static bool bif_sys_legacy_predicate_property_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,atom_or_var);
	cell tmp;
	bool found = false, evaluable = false;

	if (get_builtin_term(q->st.m, p1, &found, &evaluable), found) {
		if (evaluable)
			return false;

		make_atom(&tmp, new_atom(q->pl, "built_in"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;

		make_atom(&tmp, new_atom(q->pl, "static"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;

		make_atom(&tmp, new_atom(q->pl, "dynamic"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return false;

		return throw_error(q, p2, p2_ctx, "domain_error", "predicate_property");
	}

	predicate *pr = find_predicate(q->st.m, p1);

	if (!pr)
		return false;

	if (!pr->is_dynamic && !is_var(p2)) {
		make_atom(&tmp, new_atom(q->pl, "built_in"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;
	}

	if (!pr->is_dynamic) {
		make_atom(&tmp, new_atom(q->pl, "static"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;
	}

	if (pr->is_dynamic) {
		make_atom(&tmp, new_atom(q->pl, "dynamic"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;
	}

	if (pr->is_multifile) {
		make_atom(&tmp, new_atom(q->pl, "multifile"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;
	}

	if (pr->is_public) {
		make_atom(&tmp, new_atom(q->pl, "public"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;
	}

	if (pr->is_public) {
		make_atom(&tmp, new_atom(q->pl, "exported"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;
	}

	make_atom(&tmp, new_atom(q->pl, "static"));

	if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
		return true;

	make_atom(&tmp, new_atom(q->pl, "meta_predicate"));

	if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
		return true;

	make_atom(&tmp, new_atom(q->pl, "visible"));

	if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
		return true;

	return false;
}

static bool bif_sys_legacy_evaluable_property_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,atom_or_var);
	cell tmp;
	bool found = false, evaluable = false;

	if (CMP_STRING_TO_CSTR(q, p2, "built_in")
		&& CMP_STRING_TO_CSTR(q, p2, "static")
		&& CMP_STRING_TO_CSTR(q, p2, "dynamic")
		&& CMP_STRING_TO_CSTR(q, p2, "foreign")
		)
		return throw_error(q, p2, p2_ctx, "domain_error", "evaluable_property");

	if (get_builtin_term(q->st.m, p1, &found, &evaluable), found) {
		if (!evaluable)
			return false;

		make_atom(&tmp, new_atom(q->pl, "built_in"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;

		make_atom(&tmp, new_atom(q->pl, "static"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return true;

		make_atom(&tmp, new_atom(q->pl, "dynamic"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return false;

		make_atom(&tmp, new_atom(q->pl, "foreign"));

		if (unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
			return false;

		return throw_error(q, p2, p2_ctx, "domain_error", "evaluable_property");
	}

	return false;
}

static bool bif_char_type_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_int);
	GET_NEXT_ARG(p2,atom_or_compound);
	int ch;

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_atom(p1)) {
		if (strlen_utf8(C_STR(q, p1)) != 1)
			return false;

		ch = peek_char_utf8(C_STR(q, p1));
	} else
		ch = get_smallint(p1);

	if (!CMP_STRING_TO_CSTR(q, p2, "alpha"))
		return iswalpha(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "alphabetic"))
		return iswalpha(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "alnum"))
		return iswalpha(ch) || iswdigit(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "alphanumeric"))
		return iswalpha(ch) || iswdigit(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "prolog"))
		return iswalpha(ch) || iswdigit(ch) || iswgraph(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "hexadecimal_digit")) {
		return isxdigit(ch);
	} else if (!CMP_STRING_TO_CSTR(q, p2, "octal_digit")) {
		static const char *s_hex = "01234567";
		return isdigit(ch) && strchr(s_hex, ch);
	} else if (!CMP_STRING_TO_CSTR(q, p2, "decimal_digit"))
		return isdigit(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "numeric"))
		return iswdigit(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "whitespace"))
		return iswblank(ch) || iswspace(ch) || (ch == 0x85) || (ch == 0xA0) || (ch == 0x2007) || (ch == 0x202f);
	else if (!CMP_STRING_TO_CSTR(q, p2, "lower") && !p2->arity)
		return iswlower(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "upper") && !p2->arity)
		return iswupper(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "lower") && p2->arity) {
		cell *arg21 = deref(q, p2+1, p2_ctx);
		pl_idx arg21_ctx = q->latest_ctx;
		char tmpbuf[20];
		put_char_utf8(tmpbuf, tolower(ch));
		cell tmp;
		make_string(&tmp, tmpbuf);
		return unify(q, arg21, arg21_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p2, "upper") && p2->arity) {
		cell *arg21 = deref(q, p2+1, p2_ctx);
		pl_idx arg21_ctx = q->latest_ctx;
		char tmpbuf[20];
		put_char_utf8(tmpbuf, toupper(ch));
		cell tmp;
		make_string(&tmp, tmpbuf);
		return unify(q, arg21, arg21_ctx, &tmp, q->st.curr_frame);
	} else if (!CMP_STRING_TO_CSTR(q, p2, "graphic"))
		return iswgraph(ch) && !iswalnum(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "graphic_token"))	// ???
		return iswgraph(ch) && !iswalnum(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "ascii_graphic"))	// ???
		return iswgraph(ch) && iswalnum(ch) && (ch < 128);
	else if (!CMP_STRING_TO_CSTR(q, p2, "ascii"))
		return ch < 128;
	else if (!CMP_STRING_TO_CSTR(q, p2, "ascii_punctuation"))
		return iswpunct(ch);
	else if (!CMP_STRING_TO_CSTR(q, p2, "octet"))
		return ch < 256;
	else if (!CMP_STRING_TO_CSTR(q, p2, "layout"))
		return iswspace(ch) || (ch == '\t') || (ch == '\v') || (ch == '\f') || (ch == '\r') || (ch == '\n');
	else if (!CMP_STRING_TO_CSTR(q, p2, "exponent"))
		return (ch == 'e') || (ch == 'E');
	else if (!CMP_STRING_TO_CSTR(q, p2, "sign"))
		return (ch == '-') || (ch == '+');
	else if (!CMP_STRING_TO_CSTR(q, p2, "meta"))
		return (ch == '\'') || (ch == '"') || (ch == '`') || (ch == '\\');
	else if (!CMP_STRING_TO_CSTR(q, p2, "solo"))
		return (ch == '|') || (ch == '!') || (ch == '%')
			|| (ch == '[') || (ch == ']')
			|| (ch == '{') || (ch == '}')
			|| (ch == '(') || (ch == ')')
			;

	return false;
}

static bool bif_sys_lt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	pl_int num = get_smallint(p1);

	if (num < get_smallint(p2)) {
		set_smallint(p1, num+1);
		return true;
	}

	drop_choice(q);
	return true;
}

static bool bif_limit_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	cell *tmp = prepare_call(q, true, p2, p2_ctx, 4);
	pl_idx nbr_cells = PREFIX_LEN + p2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_fail_s, bif_sys_lt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_smallint(p1));
	make_call(q, tmp+nbr_cells);
	q->st.curr_instr = tmp;
	return true;
}

static bool bif_sys_gt_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	pl_int num = get_smallint(p1);

	if (num <= get_smallint(p2)) {
		set_smallint(p1, num+1);
		return false;
	}

	return true;
}

static bool bif_offset_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,callable);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	cell *tmp = prepare_call(q, true, p2, p2_ctx, 4);
	pl_idx nbr_cells = PREFIX_LEN + p2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_fail_s, bif_sys_gt_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_smallint(p1));
	make_call(q, tmp+nbr_cells);
	q->st.curr_instr = tmp;
	return true;
}

static bool bif_sys_ne_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	pl_int num = get_smallint(p1);

	if (num != get_smallint(p2)) {
		set_smallint(p1, num+1);
		return false;
	}

	drop_choice(q);
	return true;
}

static bool bif_sys_incr_2(query *q)
{
	GET_FIRST_ARG(p1, integer_or_var);
	GET_NEXT_ARG(p2, integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	pl_int n = get_smallint(p2);
	n++;
	set_smallint(p2, n);

	if (is_integer(p1))
		return get_smallint(p1) == n;

	return unify(q, p1, p1_ctx, p2, q->st.curr_frame);
}

static bool bif_call_nth_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_bigint(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "small_integer_range");

	if (is_integer(p2) && is_zero(p2))
		return false;

	if (is_integer(p2) && is_negative(p2))
		return throw_error(q, p2, p2_ctx, "domain_error", "not_less_than_zero");

	if (is_var(p2)) {
		cell *tmp = prepare_call(q, true, p1, p1_ctx, 4);
		pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_incr_s, bif_sys_incr_2, 2, 2);
		GET_RAW_ARG(2,p2_raw);
		tmp[nbr_cells] = *p2_raw;
		tmp[nbr_cells++].nbr_cells = 1;
		make_int(tmp+nbr_cells++, 0);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_barrier(q));
		choice *ch = GET_CURR_CHOICE();
		ch->fail_on_retry = true;
		q->st.curr_instr = tmp;
		return true;
	}

	cell *tmp = prepare_call(q, true, p1, p1_ctx, 7);
	pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_ne_s, bif_sys_ne_2, 2, 2);
	make_int(tmp+nbr_cells++, 1);
	make_int(tmp+nbr_cells++, get_smallint(p2));
	make_struct(tmp+nbr_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
	make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	q->st.curr_instr = tmp;
	return true;
}

static bool bif_string_length_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list_or_nil);
	GET_NEXT_ARG(p2,integer_or_var);

	if (is_interned(p1) && !CMP_STRING_TO_CSTR(q, p1, "[]")) {
		cell tmp;
		make_int(&tmp, 0);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	if (is_atom(p1)) {
		cell tmp;
		make_int(&tmp, C_STRLEN_UTF8(p1));
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	size_t tmp_len;

	if (q->st.m->flags.double_quote_chars
		&& !is_cyclic_term(q, p1, p1_ctx)
		&& (tmp_len = scan_is_chars_list(q, p1, p1_ctx, false)) > 0) {
		cell tmp;
		make_int(&tmp, tmp_len);
		return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	}

	return throw_error(q, p1, p1_ctx, "type_error", "chars");
}

static bool bif_get_unbuffered_code_1(query *q)
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
		return do_yield(q, 1);
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

static bool bif_get_unbuffered_char_1(query *q)
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
		return do_yield(q, 1);
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

// module:goal

bool bif_iso_invoke_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,callable);

	if (is_atom(p1)) {
		module *m = find_module(q->pl, C_STR(q, p1));

		if (!m)
			m = module_create(q->pl, C_STR(q, p1));

		q->st.m = m;
	}

	cell *tmp = prepare_call(q, true, p2, p2_ctx, 1);
	check_heap_error(tmp);
	pl_idx nbr_cells = PREFIX_LEN;

	if (!is_builtin(p2) /*&& !tmp[nbr_cells].match*/)
		tmp[nbr_cells].match = find_predicate(q->st.m, p2);

	nbr_cells += p2->nbr_cells;
	make_call(q, tmp+nbr_cells);
	q->st.curr_instr = tmp;
	q->st.curr_frame = p2_ctx;
	return true;
}

static bool bif_current_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);

	if (!q->retry) {
		if (is_atom(p1)) {
			const char *name = C_STR(q, p1);
			return find_module(q->pl, name) ? true : false;
		}

		check_heap_error(push_choice(q));

		module *m = (module*)list_front(&q->pl->modules);
		cell tmp;
		make_atom(&tmp, new_atom(q->pl, m->name));
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	if (!q->current_m)
		return false;

	module *m = (module*)list_next(q->current_m);

	if (!m)
		return false;

	check_heap_error(push_choice(q));
	cell tmp;
	make_atom(&tmp, new_atom(q->pl, m->name));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_use_module_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	if (!is_atom(p1) && !is_compound(p1)) return false;
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_clone_to_tmp(q, q->st.curr_instr, q->st.curr_frame);
	return do_use_module_1(q->st.m, tmp);
}

static bool bif_use_module_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_clone_to_tmp(q, q->st.curr_instr, q->st.curr_frame);

	if (!do_use_module_1(q->st.m, tmp))
		return false;

	return do_use_module_2(q->st.m, tmp);
}

static bool bif_multifile_1(query *q)
{
	GET_FIRST_ARG(p1,compound);

	if (p1->val_off == g_colon_s) {
		const char *mod = C_STR(q, p1+1);
		p1 += 2;
		const char *name = C_STR(q, p1+1);
		unsigned arity = get_smalluint(p1+2);

		if (!is_multifile_in_db(q->pl, mod, name, arity)) {
			//fprintf(stdout, "Error: not multifile %s:%s/%u\n", mod, name, arity);
			//return true;
		}
	} else if (p1->val_off == g_slash_s) {
		const char *name = C_STR(q, p1+1);
		unsigned arity = get_smalluint(p1+2);
		set_multifile_in_db(q->st.m, name, arity);
	} else
		return false;

	return true;
}

static bool bif_prolog_load_context_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);

	if (CMP_STRING_TO_CSTR(q, p1, "module"))
		return false;

	cell tmp;
	make_atom(&tmp, new_atom(q->pl, q->st.m->name));
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool bif_strip_module_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p3,any);

	if (p1->val_off == g_colon_s) {
		cell *cm = deref(q, p1+1, p1_ctx);
		pl_idx cm_ctx = q->latest_ctx;

		if (!unify(q, p2, p2_ctx, cm, cm_ctx))
			return false;

		cell *ct = deref(q, p1+2, p1_ctx);
		pl_idx ct_ctx = q->latest_ctx;
		return unify(q, p3, p3_ctx, ct, ct_ctx);
	}

#if 0
	cell tmp;
	make_atom(&tmp, new_atom(q->pl, q->st.m->name));

	if (!unify(q, p2, p2_ctx, &tmp, q->st.curr_frame))
		return false;
#endif

	return unify(q, p3, p3_ctx, p1, p1_ctx);
}

static bool bif_module_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);

	if (is_var(p1)) {
		cell tmp;
		make_atom(&tmp, new_atom(q->pl, q->st.m->name));
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	const char *name = C_STR(q, p1);
	module *m = find_module(q->pl, name);

	if (!m) {
		if (q->p->command)
			fprintf(stdout, "Info: created module '%s'\n", name);

		m = module_create(q->pl, name);
	}

	q->st.m = m;
	return true;
}

static bool bif_modules_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	check_heap_error(init_tmp_heap(q));

	for (module *m = (module*)list_front(&q->pl->modules);
		m; m = (module*)list_next(m)) {
		if (m->orig)
			continue;

		cell tmp;
		make_atom(&tmp,  new_atom(q->pl, m->name));
		append_list(q, &tmp);
	}

	cell *l = end_list(q);
	check_heap_error(l);
	return unify(q, p1, p1_ctx, l, q->st.curr_frame);
}

static bool bif_using_0(query *q)
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

static bool bif_sys_alarm_1(query *q)
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

	struct itimerval it = {0};

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

static bool bif_sys_register_cleanup_1(query *q)
{
	if (q->retry) {
		GET_FIRST_ARG(p1,callable);
		cell *tmp = prepare_call(q, true, p1, p1_ctx, 5);
		pl_idx nbr_cells = PREFIX_LEN + p1->nbr_cells;
		make_struct(tmp+nbr_cells++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_uint(tmp+nbr_cells++, q->cp);
		make_struct(tmp+nbr_cells++, g_fail_s, bif_iso_fail_0, 0, 0);
		make_call(q, tmp+nbr_cells);
		q->st.curr_instr = tmp;
		return true;
	}

	check_heap_error(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->register_cleanup = true;
	return true;
}

static bool bif_sys_det_length_rundown_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_var);
	GET_NEXT_ARG(p2,integer);
	int var_nbr;
	unsigned n = get_smalluint(p2);

	if ((var_nbr = create_vars(q, n)) < 0)
		return throw_error(q, p2, p2_ctx, "resource_error", "stack");

	cell *l;
	check_heap_error(l = alloc_on_heap(q, n*2+1));
	cell *save_l = l;

	while (n) {
		l->tag = TAG_INTERNED;
		l->val_off = g_dot_s;
		l->nbr_cells = n*2+1;
		l->arity = 2;
		l++;
		make_ref(l, var_nbr++, q->st.curr_frame);
		l->flags = FLAG_VAR_ANON;
		l++;
		n--;
	}

	make_atom(l, g_nil_s);
	GET_FIRST_ARG(xp1,list_or_var);
	return unify(q, xp1, xp1_ctx, save_l, q->st.curr_frame);
}

static bool bif_sys_memberchk_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	GET_NEXT_ARG(p3,var);
	LIST_HANDLER(p2);
	push_choice(q);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (unify(q, p1, p1_ctx, h, h_ctx)) {
			drop_choice(q);
			unify(q, p3, p3_ctx, make_nil(), q->st.curr_frame);
			return true;
		}

		if (!is_string(p2))
			undo_me(q);

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	drop_choice(q);

	if (is_nil(p2))
		return false;

	unify(q, p3, p3_ctx, p2, p2_ctx);
	return true;
}

static bool bif_sys_get_level_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell tmp;
	make_int(&tmp, q->cp);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_sys_dump_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,atom);
	GET_FIRST_RAW_ARG(p1x,any);
	bool deref = p2->val_off == g_true_s;
	p1 = deref ? p1 : p1x;
	cell *tmp = p1;

	for (unsigned i = 0; i <p1->nbr_cells; i++, tmp++) {
		printf("[%02u] tag=%10s, nbr_cells=%u, arity=%u",
			i,
			(
				(tmp->tag == TAG_VAR && is_ref(tmp))? "var_ref" :
				tmp->tag == TAG_VAR ? "var" :
				tmp->tag == TAG_INTERNED ? "interned" :
				tmp->tag == TAG_CSTR ? "cstr" :
				tmp->tag == TAG_INTEGER ? "integer" :
				tmp->tag == TAG_DOUBLE ? "float" :
				tmp->tag == TAG_RATIONAL ? "rational" :
				tmp->tag == TAG_INDIRECT ? "indirect" :
				tmp->tag == TAG_BLOB ? "blob" :
				tmp->tag == TAG_DBID ? "dbid" :
				tmp->tag == TAG_KVID ? "kvid" :
				"other"
			),
			tmp->nbr_cells, tmp->arity);

		if ((tmp->tag == TAG_INTEGER) && !is_managed(tmp))
			printf(", %lld", (long long)tmp->val_int);

		if (tmp->tag == TAG_INTERNED)
			printf(", '%s'", C_STR(q, tmp));

		if (is_var(tmp))
			printf(", local=%d, temp=%d", is_local(tmp), is_temporary(tmp));

		if (is_ref(tmp))
			printf(", slot=%u, ctx=%u", tmp->var_nbr, tmp->var_ctx);
		else if (is_var(tmp))
			printf(", slot=%u, %s", tmp->var_nbr, C_STR(q, tmp));

		printf("\n");
	}

	return true;
}

static bool bif_abort_0(query *q)
{
	return throw_error(q, q->st.curr_instr, q->st.curr_frame, "$aborted", "abort_error");
}

static bool bif_sys_choice_0(query *q)
{
	check_heap_error(push_choice(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	return true;
}

static bool bif_iso_compare_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	if (is_atom(p1)) {
		if (CMP_STRING_TO_CSTR(q, p1, "<")
			&& CMP_STRING_TO_CSTR(q, p1, ">")
			&& CMP_STRING_TO_CSTR(q, p1, "="))
			return throw_error(q, p1, p1_ctx, "domain_error", "order");
	}

	int status = compare(q, p2, p2_ctx, p3, p3_ctx);
	cell tmp;

	make_atom(&tmp, (status == 0)?g_eq_s:status<0?g_lt_s:g_gt_s);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

bool bif_sys_counter_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	pl_uint n = 0;

	if (is_smallint(p1))
		n = get_smalluint(p1);

	cell tmp;
	make_uint(&tmp, n+1);
	GET_RAW_ARG(1, p1_raw);
	reset_var(q, p1_raw, p1_raw_ctx, &tmp, q->st.curr_frame);
	return true;
}

void format_property(module *m, char *tmpbuf, size_t buflen, const char *name, unsigned arity, const char *type, bool function)
{
	tmpbuf[0] = '\0';
	char *dst = tmpbuf;

	if (needs_quoting(m, name, strlen(name))) {
		char *dst2 = formatted(name, strlen(name), false, false);
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'(%s, '%s'", function?"function":"predicate", dst2);
		free(dst2);
	} else
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'(%s, %s", function?"function":"predicate", name);

	if (arity) {
		dst += snprintf(dst, buflen-(dst-tmpbuf), "(");

		for (unsigned i = 0; i < arity; i++) {
			if (i > 0)
				dst += snprintf(dst, buflen-(dst-tmpbuf), ",");

			dst += snprintf(dst, buflen-(dst-tmpbuf), "_");
		}

		dst += snprintf(dst, buflen-(dst-tmpbuf), ")");
	}

	dst += snprintf(dst, buflen-(dst-tmpbuf), ", (%s)).\n", type);
}

void format_template(module *m, char *tmpbuf, size_t buflen, const char *name, unsigned arity, const builtins *ptr, bool function, bool alt)
{
	tmpbuf[0] = '\0';

	if (!ptr->help || !*ptr->help)
		return;

	if (alt && (!ptr->help_alt || !*ptr->help_alt))
		return;

	char *dst = tmpbuf;
	bool quote = needs_quoting(m, name, strlen(name));

	if (quote) {
		char *dst2 = formatted(name, strlen(name), false, false);
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'(%s, '%s'", function?"function":"predicate", dst2);
		free(dst2);
	} else
		dst += snprintf(dst, buflen-(dst-tmpbuf), "'$predicate_property'(%s, %s", function?"function":"predicate", name);

	if (arity) {
		dst += snprintf(dst, buflen-(dst-tmpbuf), "(");

		for (unsigned i = 0; i < arity; i++) {
			if (i > 0)
				dst += snprintf(dst, buflen-(dst-tmpbuf), ",");

			dst += snprintf(dst, buflen-(dst-tmpbuf), "_");
		}

		dst += snprintf(dst, buflen-(dst-tmpbuf), ")");
	}

	char tmpbuf2[256];
	do_template(tmpbuf2, name, ptr->arity, alt?ptr->help_alt:ptr->help, function, quote);
	dst += snprintf(dst, buflen-(dst-tmpbuf), ", (%s))).\n", tmpbuf2);
}

static void load_properties(module *m)
{
	if (m->loaded_properties)
		return;

	m->loaded_properties = true;
	SB_alloc(pr, 1024*64);
	char tmpbuf[1024];

	format_property(m, tmpbuf, sizeof(tmpbuf), "!", 0, "choice_construct", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "true", 0, "choice_construct", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "fail", 0, "choice_construct", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ",", 2, "choice_construct", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ";", 2, "choice_construct", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "->", 2, "choice_construct", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "*->", 2, "choice_construct", false); SB_strcat(pr, tmpbuf);

	format_property(m, tmpbuf, sizeof(tmpbuf), "\\+", 1, "meta_predicate((\\+0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "catch", 3, "meta_predicate(catch(0,?,0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "reset", 3, "meta_predicate(reset(0,?,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "", 2, "meta_predicate((0,0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ",", 2, "meta_predicate((0,0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), ";", 2, "meta_predicate((0;0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "->", 2, "meta_predicate((0->0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "*->", 2, "meta_predicate((0*->0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "if", 3, "meta_predicate(if(0,0,0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "once", 1, "meta_predicate(once(0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "ignore", 1, "meta_predicate(ignore(0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "call", 1, "meta_predicate(call(0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "task", 1, "meta_predicate(task(0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "findall", 3, "meta_predicate(findall(?,0,-))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "engine_create", 4, "meta_predicate(engine_create(?,0,?,+))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "|", 2, "meta_predicate((:|+))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "time", 1, "meta_predicate(time(0))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "call_nth", 2, "meta_predicate(call_nth(0,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "asserta", 1, "meta_predicate(asserta(:))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "asserta", 2, "meta_predicate(asserta(:,-))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "assertz", 1, "meta_predicate(assertz(:))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "assertz", 2, "meta_predicate(assertz(:,-))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retract", 1, "meta_predicate(retract(:))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retract", 2, "meta_predicate(retract(:,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "retractall", 1, "meta_predicate(retractall(:))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "current_predicate", 1, "meta_predicate(current_predicate(:))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "predicate_property", 2, "meta_predicate(predicate_property(:,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "abolish", 1, "meta_predicate(abolish(:))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "clause", 2, "meta_predicate(clause(:,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "clause", 3, "meta_predicate(clause(:,?,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "call_residue_vars", 2, "meta_predicate(call_residue_vars(0,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "bb_b_put", 2, "meta_predicate(bb_b_put(:,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "bb_put", 2, "meta_predicate(bb_put(:,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "bb_get", 2, "meta_predicate(bb_get(:,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "bb_update", 3, "meta_predicate(bb_update(:,?,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "bb_delete", 2, "meta_predicate(bb_delete(:,?))", false); SB_strcat(pr, tmpbuf);

#if USE_THREADS
	format_property(m, tmpbuf, sizeof(tmpbuf), "thread_create", 3, "meta_predicate(thread_create(0,-,?))", false); SB_strcat(pr, tmpbuf);
	format_property(m, tmpbuf, sizeof(tmpbuf), "thread_signal", 2, "meta_predicate(thread_signal(+,0))", false); SB_strcat(pr, tmpbuf);
#endif

	for (int i = 2; i <= 7; i++) {
		char metabuf[1024];
		char *dst2 = metabuf;
		dst2 += snprintf(dst2, sizeof(metabuf), "meta_predicate(call(%d", i-1);

		for (int j = 1; j < i; j++)
			dst2 += snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), ",?");


		snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), "))");
		format_property(m, tmpbuf, sizeof(tmpbuf), "call", i, metabuf, false); SB_strcat(pr, tmpbuf);
	}

	for (int i = 2; i <= 7; i++) {
		char metabuf[1024];
		char *dst2 = metabuf;
		dst2 += snprintf(dst2, sizeof(metabuf), "meta_predicate(task(%d", i-1);

		for (int j = 1; j < i; j++)
			dst2 += snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), ",?");


		snprintf(dst2, sizeof(metabuf)-(dst2-metabuf), "))");
		format_property(m, tmpbuf, sizeof(tmpbuf), "task", i, metabuf, false); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_atts_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_bboard_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_contrib_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_csv_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_database_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_evaluable_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_ffi_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_iso_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
 	}

	for (const builtins *ptr = g_maps_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
 	}

	for (const builtins *ptr = g_other_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_posix_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_sort_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_sregex_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_streams_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
 	}

	for (const builtins *ptr = g_tasks_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
	}

	for (const builtins *ptr = g_threads_bifs; ptr->name; ptr++) {
		sl_app(m->pl->biftab, ptr->name, ptr);
		if (ptr->name[0] == '$') continue;
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "built_in", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "static", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf);
		if (ptr->iso) { format_property(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, "iso", ptr->evaluable?true:false); SB_strcat(pr, tmpbuf); }
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, false); SB_strcat(pr, tmpbuf);
		format_template(m, tmpbuf, sizeof(tmpbuf), ptr->name, ptr->arity, ptr, ptr->evaluable?true:false, true); SB_strcat(pr, tmpbuf);
 	}

	parser *p = parser_create(m);
	p->srcptr = SB_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	parser_destroy(p);
	SB_free(pr);
}

static void load_flags(query *q)
{
	cell tmp;
	make_atom(&tmp, new_atom(q->pl, "$current_prolog_flag"));
	tmp.arity = 2;

	if (do_abolish(q, &tmp, &tmp, false) != true)
		return;

	module *m = q->st.m;
	SB_alloc(pr, 1024*8);

	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "double_quotes", m->flags.double_quote_atom?"atom":m->flags.double_quote_chars?"chars":m->flags.double_quote_codes?"codes":"???");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "char_conversion", m->flags.char_conversion?"on":"off");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "occurs_check", m->flags.occurs_check==OCCURS_CHECK_TRUE?"true":m->flags.occurs_check==OCCURS_CHECK_FALSE?"false":"error");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "character_escapes", m->flags.character_escapes?"true":"false");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "strict_iso", m->flags.strict_iso?"on":"off");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "debug", m->flags.debug?"on":"off");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "unknown", m->flags.unknown == UNK_ERROR?"error":m->flags.unknown == UNK_WARNING?"warning":m->flags.unknown == UNK_CHANGEABLE?"changeable":"fail");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "encoding", "'UTF-8'");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "unix", "true");
#if USE_THREADS
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "threads", "true");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "hardware_threads", 4);
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "max_threads", MAX_ACTUAL_THREADS);
#else
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "threads", "false");
#endif
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "verbose", q->pl->quiet?"false":"true");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "dialect", "trealla");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "bounded", "false");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "max_arity", MAX_ARITY);
#ifndef __wasi__
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "pid", getpid());
#else
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "pid", 42);
#endif
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %u).\n", "cpu_count", g_cpu_count);
	SB_sprintf(pr, "'$current_prolog_flag'(%s, %s).\n", "integer_rounding_function", "toward_zero");
	SB_sprintf(pr, "'$current_prolog_flag'(%s, [max_depth(%u),quoted(%s),double_quotes(%s)]).\n", "answer_write_options", (unsigned)q->pl->def_max_depth, q->pl->def_quoted?"true":"false", q->pl->def_double_quotes?"true":"false");

	parser *p = parser_create(m);
	p->srcptr = SB_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	parser_destroy(p);
	SB_free(pr);
}

static void load_ops(query *q)
{
	SB_alloc(pr, 1024*8);
	sliter *iter = sl_first(q->st.m->ops);
	op_table *ptr;

	while (sl_next(iter, (void**)&ptr)) {
		char specifier[80], name[1024];

		if (!ptr->priority || !ptr->specifier)
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

		if (quote) {
			char *dst2 = formatted(ptr->name, strlen(ptr->name), false, false);
			snprintf(name, sizeof(name), "%s", dst2);
			free(dst2);
		} else
			snprintf(name, sizeof(name), "%s", ptr->name);

		if (quote) {
			SB_sprintf(pr, "'$op'( '%s', %s, %u).\n", name, specifier, ptr->priority);
		} else {
			SB_sprintf(pr, "'$op'( (%s), %s, %u).\n", name, specifier, ptr->priority);
		}
	}

	sl_done(iter);
	iter = sl_first(q->st.m->defops);

	while (sl_next(iter, (void**)&ptr)) {
		char specifier[80];

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


		char *dst2 = formatted(ptr->name, strlen(ptr->name), false, false);
		SB_sprintf(pr, "'$op'('%s', %s, %u).\n", dst2, specifier, ptr->priority);
		free(dst2);
	}

	parser *p = parser_create(q->st.m);
	p->srcptr = SB_cstr(pr);
	p->consulting = true;
	tokenize(p, false, false);
	parser_destroy(p);
	//printf("*** %s load_ops %s\n", q->st.m->name, SB_cstr(pr));
	SB_free(pr);
}

builtins g_iso_bifs[] =
{
	{"true", 0, bif_iso_true_0, NULL, true, false, BLAH},
	{"fail", 0, bif_iso_fail_0, NULL, true, false, BLAH},
	{"false", 0, bif_iso_fail_0, NULL, true, false, BLAH},

	{",", 2, bif_iso_conjunction_2, ":callable,:callable", true, false, BLAH},
	{";", 2, bif_iso_disjunction_2, ":callable,:callable", true, false, BLAH},
	{"!", 0, bif_iso_cut_0, NULL, true, false, BLAH},
	{":", 2, bif_iso_invoke_2, "+atom,:callable", true, false, BLAH},
	{"=..", 2, bif_iso_univ_2, "+term,?list", true, false, BLAH},
	{"->", 2, bif_iso_if_then_2, ":callable,:callable", true, false, BLAH},
	{"\\+", 1, bif_iso_negation_1, ":callable", true, false, BLAH},
	{"=", 2, bif_iso_unify_2, "+term,+term", true, false, BLAH},
	{"\\=", 2, bif_iso_notunify_2, "+term,+term", true, false, BLAH},

	{"call", 1, bif_iso_call_1, ":callable", true, false, BLAH},
	{"call", 2, bif_iso_call_n, ":callable,?term", true, false, BLAH},
	{"call", 3, bif_iso_call_n, ":callable,?term,term", true, false, BLAH},
	{"call", 4, bif_iso_call_n, ":callable,?term,?term,?term", true, false, BLAH},
	{"call", 5, bif_iso_call_n, ":callable,?term,?term,?term,?term", true, false, BLAH},
	{"call", 6, bif_iso_call_n, ":callable,?term,?term,?term,?term,?term", true, false, BLAH},
	{"call", 7, bif_iso_call_n, ":callable,?term,?term,?term,?term,?term,?term", true, false, BLAH},
	{"call", 8, bif_iso_call_n, ":callable,?term,?term,?term,?term,?term,?term,?term", true, false, BLAH},

	{"$catch", 3, bif_iso_catch_3, ":callable,?term,:callable", true, false, BLAH},
	{"throw", 1, bif_iso_throw_1, "+term", true, false, BLAH},
	{"once", 1, bif_iso_once_1, ":callable", true, false, BLAH},
	{"repeat", 0, bif_iso_repeat_0, NULL, true, false, BLAH},
	{"atom", 1, bif_iso_atom_1, "+term", true, false, BLAH},
	{"atomic", 1, bif_iso_atomic_1, "+term", true, false, BLAH},
	{"number", 1, bif_iso_number_1, "+term", true, false, BLAH},
	{"compound", 1, bif_iso_compound_1, "+term", true, false, BLAH},
	{"var", 1, bif_iso_var_1, "+term", true, false, BLAH},
	{"nonvar", 1, bif_iso_nonvar_1, "+term", true, false, BLAH},
	{"ground", 1, bif_iso_ground_1, "+term", true, false, BLAH},
	{"callable", 1, bif_iso_callable_1, "+term", true, false, BLAH},
	{"char_code", 2, bif_iso_char_code_2, "?atom,?integer", true, false, BLAH},
	{"atom_chars", 2, bif_iso_atom_chars_2, "?number,?list", true, false, BLAH},
	{"atom_codes", 2, bif_iso_atom_codes_2, "?number,?list", true, false, BLAH},
	{"number_chars", 2, bif_iso_number_chars_2, "?number,?list", true, false, BLAH},
	{"number_codes", 2, bif_iso_number_codes_2, "?number,?list", true, false, BLAH},
	{"arg", 3, bif_iso_arg_3, "+integer,+term,?term", true, false, BLAH},
	{"functor", 3, bif_iso_functor_3, "?term,?atom,?integer", true, false, BLAH},
	{"copy_term", 2, bif_iso_copy_term_2, "+term,?term", true, false, BLAH},
	{"term_variables", 2, bif_iso_term_variables_2, "+term,-list", true, false, BLAH},
	{"atom_length", 2, bif_iso_atom_length_2, "?list,?integer", true, false, BLAH},
	{"atom_concat", 3, bif_iso_atom_concat_3, "+atom,+atom,?atom", true, false, BLAH},
	{"sub_atom", 5, bif_iso_sub_atom_5, "+atom,?before,?length,?after,?atom", true, false, BLAH},
	{"sub_string", 5, bif_iso_sub_string_5, "+character_list,?before,?length,?after,?character_list", true, false, BLAH},
	{"current_rule", 1, bif_iso_current_rule_1, "-term", true, false, BLAH},
	{"end_of_file", 0, bif_iso_halt_0, NULL, true, false, BLAH},
	{"halt", 0, bif_iso_halt_0, NULL, true, false, BLAH},
	{"halt", 1, bif_iso_halt_1, "+integer", true, false, BLAH},
	{"$legacy_current_prolog_flag", 2, bif_iso_current_prolog_flag_2, "+atom,?term", true, false, BLAH},
	{"set_prolog_flag", 2, bif_iso_set_prolog_flag_2, "+atom,+term", true, false, BLAH},
	{"op", 3, bif_iso_op_3, "?integer,?atom,+atom", true, false, BLAH},
	{"findall", 3, bif_iso_findall_3, "+term,:callable,-list", true, false, BLAH},
	{"current_predicate", 1, bif_iso_current_predicate_1, "+predicate_indicator", true, false, BLAH},
	{"acyclic_term", 1, bif_iso_acyclic_term_1, "+term", true, false, BLAH},
	{"compare", 3, bif_iso_compare_3, "+atom,+term,+term", true, false, BLAH},
	{"unify_with_occurs_check", 2, bif_iso_unify_with_occurs_check_2, "+term,+term", true, false, BLAH},

	{0}
};

builtins g_other_bifs[] =
{
	{"*->", 2, bif_if_2, ":callable,:callable", false, false, BLAH},
	{"if", 3, bif_if_3, ":callable,:callable,:callable", false, false, BLAH},

	{"shell", 1, bif_shell_1, "+atom", false, false, BLAH},
	{"shell", 2, bif_shell_2, "+atom,-integer", false, false, BLAH},
	{"listing", 0, bif_listing_0, NULL, false, false, BLAH},
	{"listing", 1, bif_listing_1, "+predicate_indicator", false, false, BLAH},
	{"time", 1, bif_time_1, ":callable", false, false, BLAH},
	{"trace", 0, bif_trace_0, NULL, false, false, BLAH},
	{"statistics", 0, bif_statistics_0, NULL, false, false, BLAH},
	{"statistics", 2, bif_statistics_2, "+atom,-term", false, false, BLAH},

	{"current_module", 1, bif_current_module_1, "-atom", false, false, BLAH},
	{"prolog_load_context", 2, bif_prolog_load_context_2, "+atom,?term", false, false, BLAH},
	{"strip_module", 3, bif_strip_module_3, "+callable,?atom,?callable", false, false, BLAH},
	{"module", 1, bif_module_1, "?atom", false, false, BLAH},
	{"modules", 1, bif_modules_1, "-list", false, false, BLAH},
	{"using", 0, bif_using_0, NULL, false, false, BLAH},
	{"use_module", 1, bif_use_module_1, "+term", false, false, BLAH},
	{"use_module", 2, bif_use_module_2, "+term,+list", false, false, BLAH},
	{"module_info", 2, bif_module_info_2, "+atom,-list", false, false, BLAH},
	{"source_info", 2, bif_source_info_2, "+predicate_indicator,-list", false, false, BLAH},
	{"multifile", 1, bif_multifile_1, "+term", false, false, BLAH},

	{"help", 2, bif_help_2, "+predicate_indicator,+atom", false, false, BLAH},
	{"help", 1, bif_help_1, "+predicate_indicator", false, false, BLAH},
	{"help", 0, bif_help_0, NULL, false, false, BLAH},
	{"module_help", 3, bif_module_help_3, "+atom,+predicate_indicator,+atom", false, false, BLAH},
	{"module_help", 2, bif_module_help_2, "+atom,+predicate_indicator", false, false, BLAH},
	{"module_help", 1, bif_module_help_1, "+atom", false, false, BLAH},

	{"abort", 0, bif_abort_0, NULL, false, false, BLAH},
	{"sort", 4, bif_sort_4, "+integer,+atom,+list,?list", false, false, BLAH},
	{"ignore", 1, bif_ignore_1, ":callable", false, false, BLAH},
	{"string_codes", 2, bif_string_codes_2, "+string,-list", false, false, BLAH},
	{"term_singletons", 2, bif_term_singletons_2, "+term,-list", false, false, BLAH},
	{"get_unbuffered_code", 1, bif_get_unbuffered_code_1, "?integer", false, false, BLAH},
	{"get_unbuffered_char", 1, bif_get_unbuffered_char_1, "?character", false, false, BLAH},
	{"string", 1, bif_atom_1, "+term,+term", false, false, BLAH},
	{"atomic_concat", 3, bif_atomic_concat_3, "+atomic,+atomic,?atomic", false, false, BLAH},
	{"atomic_list_concat", 3, bif_atomic_list_concat_3, "+list,+list,-atomic", false, false, BLAH},
	{"replace", 4, bif_replace_4, "+string,+integer,+integer,-string", false, false, BLAH},
	{"busy", 1, bif_busy_1, "+integer", false, false, BLAH},
	{"now", 0, bif_now_0, NULL, false, false, BLAH},
	{"now", 1, bif_now_1, "-integer", false, false, BLAH},
	{"get_time", 1, bif_get_time_1, "-integer", false, false, BLAH},
	{"cpu_time", 1, bif_cpu_time_1, "-integer", false, false, BLAH},
	{"wall_time", 1, bif_wall_time_1, "-integer", false, false, BLAH},
	{"date_time", 6, bif_date_time_6, "-integer,-integer,-integer,-integer,-integer,-integer", false, false, BLAH},
	{"date_time", 7, bif_date_time_7, "-integer,-integer,-integer,-integer,-integer,-integer,-integer", false, false, BLAH},
	{"split_string", 4, bif_split_string_4, "+string,+atom,+atom,-list", false, false, BLAH},
	{"split", 4, bif_split_4, "+string,+string,?string,?string", false, false, BLAH},
	{"is_list_or_partial_list", 1, bif_is_list_or_partial_list_1, "+term", false, false, BLAH},
	{"is_partial_list", 1, bif_is_partial_list_1, "+term", false, false, BLAH},
	{"is_list", 1, bif_is_list_1, "+term", false, false, BLAH},
	{"list", 1, bif_is_list_1, "+term", false, false, BLAH},
	{"term_hash", 2, bif_term_hash_2, "+term,?integer", false, false, BLAH},
	{"base64", 3, bif_base64_3, "?string,?string,+list", false, false, BLAH},
	{"urlenc", 3, bif_urlenc_3, "?string,?string,+list", false, false, BLAH},
	{"atom_lower", 2, bif_atom_lower_2, "?atom,?atom", false, false, BLAH},
	{"atom_upper", 2, bif_atom_upper_2, "?atom,?atom", false, false, BLAH},
	{"string_lower", 2, bif_string_lower_2, "?string,?string", false, false, BLAH},
	{"string_upper", 2, bif_string_upper_2, "?string,?string", false, false, BLAH},
	{"hex_bytes", 2, bif_hex_bytes_2, "?string,?list", false, false, BLAH},
	{"hex_chars", 2, bif_hex_chars_2, "?integer,?string", false, false, BLAH},
	{"octal_chars", 2, bif_octal_chars_2, "?integer,?string", false, false, BLAH},
	{"$char_type", 2, bif_char_type_2, "+character,+term", false, false, BLAH},
	{"$code_type", 2, bif_char_type_2, "+integer,+term", false, false, BLAH},
	{"uuid", 1, bif_uuid_1, "-string", false, false, BLAH},
	{"instance", 2, bif_instance_2, "+string,?term", false, false, BLAH},
	{"getenv", 2, bif_getenv_2, "+atom,-atom", false, false, BLAH},
	{"setenv", 2, bif_setenv_2, "+atom,+atom", false, false, BLAH},
	{"unsetenv", 1, bif_unsetenv_1, "+atom", false, false, BLAH},
	{"duplicate_term", 2, bif_duplicate_term_2, "+term,-term", false, false, BLAH},
	{"copy_term_nat", 2, bif_copy_term_nat_2, "+term,-term", false, false, BLAH},
	{"call_nth", 2, bif_call_nth_2, ":callable,+integer", false, false, BLAH},
	{"limit", 2, bif_limit_2, "+integer,:callable", false, false, BLAH},
	{"offset", 2, bif_offset_2, "+integer,+callable", false, false, BLAH},
	{"unifiable", 3, bif_sys_unifiable_3, "+term,+term,-list", false, false, BLAH},
	{"between", 3, bif_between_3, "+integer,+integer,-integer", false, false, BLAH},
	{"string_length", 2, bif_string_length_2, "+string,?integer", false, false, BLAH},
	{"crypto_n_random_bytes", 2, bif_crypto_n_random_bytes_2, "+integer,-codes", false, false, BLAH},
	{"cyclic_term", 1, bif_cyclic_term_1, "+term", false, false, BLAH},
	{"call_residue_vars", 2, bif_call_residue_vars_2, ":callable,-list", false, false, BLAH},
	{"reset", 3, bif_reset_3, ":callable,?term,-term", false, false, BLAH},
	{"shift", 1, bif_shift_1, "+term", false, false, BLAH},

	{"$must_be", 4, bif_must_be_4, "+term,+atom,+term,?any", false, false, BLAH},
	{"$can_be", 4, bif_can_be_4, "+term,+atom,+term,?any", false, false, BLAH},
	{"$must_be", 2, bif_must_be_2, "+atom,+term", false, false, BLAH},
	{"$can_be", 2, bif_can_be_2, "+atom,+term,", false, false, BLAH},

	{"$set_if_var", 2, bif_sys_set_if_var_2, "?term,+term", false, false, BLAH},
	{"$msleep", 1, bif_sys_msleep_1, "+number", false, false, BLAH},
	{"$det_length_rundown", 2, bif_sys_det_length_rundown_2, "?list,+integer", false, false, BLAH},
	{"$memberchk", 3, bif_sys_memberchk_3, "?term,?list,-term", false, false, BLAH},
	{"$countall", 2, bif_sys_countall_2, "@callable,-integer", false, false, BLAH},
	{"$register_cleanup", 1, bif_sys_register_cleanup_1, NULL, false, false, BLAH},
	{"$get_level", 1, bif_sys_get_level_1, "?integer", false, false, BLAH},
	{"$is_partial_string", 1, bif_sys_is_partial_string_1, "+string", false, false, BLAH},
	{"$undo_trail", 2, bif_sys_undo_trail_2, "-list,-blob", false, false, BLAH},
	{"$redo_trail", 1, bif_sys_redo_trail_1, "+blob", false, false, BLAH},
	{"$counter", 1, bif_sys_counter_1, NULL, false, false, BLAH},
	{"$legacy_predicate_property", 2, bif_sys_legacy_predicate_property_2, "+callable,?string", false, false, BLAH},
	{"$legacy_evaluable_property", 2, bif_sys_legacy_evaluable_property_2, "+callable,?string", false, false, BLAH},
	{"$load_properties", 0, bif_sys_load_properties_0, NULL, false, false, BLAH},
	{"$load_flags", 0, bif_sys_load_flags_0, NULL, false, false, BLAH},
	{"$load_ops", 0, bif_sys_load_ops_0, NULL, false, false, BLAH},
	{"$ops_dirty", 0, bif_sys_ops_dirty_0, NULL, false, false, BLAH},
	{"$list", 1, bif_sys_list_1, "-list", false, false, BLAH},
	{"$queue", 1, bif_sys_queue_1, "+term", false, false, BLAH},
	{"$incr", 2, bif_sys_incr_2, "@integer,+integer", false, false, BLAH},
	{"$choice", 0, bif_sys_choice_0, NULL, false, false, BLAH},
	{"$alarm", 1, bif_sys_alarm_1, "+integer", false, false, BLAH},
	{"$first_non_octet", 2, bif_sys_first_non_octet_2, "+chars,-integer", false, false, BLAH},
	{"$skip_max_list", 4, bif_sys_skip_max_list_4, "?integer,?integer?,?term,?term", false, false, BLAH},
	{"$dump_term", 2, bif_sys_dump_term_2, "+term, +bool", false, false, BLAH},

#if USE_OPENSSL
	{"crypto_data_hash", 3, bif_crypto_data_hash_3, "?string,?string,?list", false, false, BLAH},
#endif

	{"$call_cleanup", 3, bif_sys_call_cleanup_3, NULL, false, false, BLAH},
	{"$block_catcher", 1, bif_sys_block_catcher_1, NULL, false, false, BLAH},
	{"$cleanup_if_det", 1, bif_sys_cleanup_if_det_1, NULL, false, false, BLAH},
	{"$drop_barrier", 1, bif_sys_drop_barrier_1, NULL, false, false, BLAH},
	{"$timer", 0, bif_sys_timer_0, NULL, false, false, BLAH},
	{"$elapsed", 0, bif_sys_elapsed_0, NULL, false, false, BLAH},
	{"$lt", 2, bif_sys_lt_2, NULL, false, false, BLAH},
	{"$gt", 2, bif_sys_gt_2, NULL, false, false, BLAH},
	{"$ne", 2, bif_sys_ne_2, NULL, false, false, BLAH},

	{0}
};
