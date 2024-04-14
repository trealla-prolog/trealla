#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "sort_r.h"

#include "heap.h"
#include "query.h"

typedef struct { query *q; cell *c; pl_idx c_ctx; int8_t arg; bool ascending:1; } basepair;

static int nodecmp(const void *ptr1, const void *ptr2)
{
	const basepair *cp1 = (const basepair*)ptr1;
	const basepair *cp2 = (const basepair*)ptr2;
	bool ascending = cp1->ascending;
	query *q = cp1->q;
	int arg = cp1->arg;
	cell *p1 = cp1->c, *p2 = cp2->c;
	pl_idx p1_ctx = cp1->c_ctx, p2_ctx = cp2->c_ctx;

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

static cell *nodesort(query *q, cell *p1, pl_idx p1_ctx, bool dedup, bool keysort, bool *status)
{
	pl_int max = PL_INT_MAX, skip = 0;
	pl_idx tmp_ctx = p1_ctx;
	cell tmp = {0};

	skip_max_list(q, p1, &tmp_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);
	size_t cnt = skip;
	basepair *base = malloc(sizeof(basepair)*cnt);
	check_error(base);
	LIST_HANDLER(p1);
	size_t idx = 0;

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (keysort) {
			if (!is_compound(h) || strcmp(C_STR(q, h), "-")) {
				*status = throw_error(q, h, h_ctx, "type_error", "pair");
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

	sort_r(base, cnt, sizeof(basepair), (void*)nodecmp, NULL);

	for (size_t i = 0; i < cnt; i++) {
		if (i > 0) {
			if (dedup && !nodecmp(&base[i], &base[i-1]))
				continue;
		}

		cell *c = deref(q, base[i].c, base[i].c_ctx);
		pl_idx c_ctx = q->latest_ctx;
		cell tmp;

		if (is_compound(c) && !is_iso_list(c)) {
			make_ref(&tmp, create_vars(q, 1), q->st.curr_frame);
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

static bool bif_iso_sort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int skip1 = 0, skip2 = 0;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p1))
		return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

	if (!is_list_or_nil(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (!is_list_or_nil_or_var(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	if (is_string(p1))
		p1 = string_to_chars_list(q, p1, p1_ctx);

	if (is_string(p2))
		p2 = string_to_chars_list(q, p2, p2_ctx);

	p1 = deep_clone_to_heap(q, p1, p1_ctx);
	check_heap_error(p1);
	p1_ctx = q->st.curr_frame;

	bool status = false;
	cell *l = nodesort(q, p1, p1_ctx, true, false, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool bif_iso_msort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int skip1 = 0, skip2 = 0;

	if (is_iso_list(p1) && !check_list(q, p1, p1_ctx, &is_partial, &skip1) && !is_partial)
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (is_partial)
		return throw_error(q, p1, p1_ctx, "instantiation_error", "list");

	if (is_iso_list(p2) && !check_list(q, p2, p2_ctx, &is_partial, &skip2) && !is_partial)
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (is_nil(p1))
		return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

	if (!is_list_or_nil(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	if (!is_list_or_nil_or_var(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	if (is_string(p1))
		p1 = string_to_chars_list(q, p1, p1_ctx);

	if (is_string(p2))
		p2 = string_to_chars_list(q, p2, p2_ctx);

	p1 = deep_clone_to_heap(q, p1, p1_ctx);
	check_heap_error(p1);
	p1_ctx = q->st.curr_frame;

	bool status = false;
	cell *l = nodesort(q, p1, p1_ctx, false, false, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static bool bif_iso_keysort_2(query *q)
{
	GET_FIRST_ARG(p1,list_or_nil);
	GET_NEXT_ARG(p2,list_or_nil_or_var);
	bool is_partial = false;
	pl_int skip1 = 0, skip2 = 0;

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
		pl_idx tmp_h_ctx = q->latest_ctx;
		LIST_TAIL(p2);

		if (!is_var(tmp_h) && (!is_compound(tmp_h) || strcmp(C_STR(q, tmp_h), "-")))
			return throw_error(q, tmp_h, tmp_h_ctx, "type_error", "pair");
	}

	if (is_nil(p1))
		return unify(q, p2, p2_ctx, make_nil(), q->st.curr_frame);

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	p1 = deep_clone_to_heap(q, p1, p1_ctx);
	check_heap_error(p1);
	p1_ctx = q->st.curr_frame;

	bool status = false;
	cell *l = nodesort(q, p1, p1_ctx, false, true, &status);
	if (!l) return status;
	return unify(q, p2, p2_ctx, l, q->st.curr_frame);
}

static cell *nodesort4(query *q, cell *p1, pl_idx p1_ctx, bool dedup, bool ascending, int arg, bool *status)
{
	pl_int max = PL_INT_MAX, skip = 0;
	pl_idx tmp_ctx = p1_ctx;
	cell tmp = {0};

	skip_max_list(q, p1, &tmp_ctx, max, &skip, &tmp);
	unshare_cell(&tmp);
	size_t cnt = skip;
	basepair *base = malloc(sizeof(basepair)*cnt);
	check_error(base);
	LIST_HANDLER(p1);
	size_t idx = 0;

	while (is_list(p1)) {
		cell *h = deref(q, LIST_HEAD(p1), p1_ctx);
		pl_idx h_ctx = q->latest_ctx;
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

	sort_r(base, cnt, sizeof(basepair), (void*)nodecmp, NULL);

	for (size_t i = 0; i < cnt; i++) {
		if (i > 0) {
			if (dedup && !nodecmp(&base[i], &base[i-1]))
				continue;
		}

		cell *c = deref(q, base[i].c, base[i].c_ctx);
		pl_idx c_ctx = q->latest_ctx;
		cell tmp;

		if (is_var(c) || is_compound(c)) {
			make_ref(&tmp, create_vars(q, 1), q->st.curr_frame);
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

static bool bif_sort_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,list_or_nil);
	GET_NEXT_ARG(p4,list_or_nil_or_var);
	bool is_partial = false, dedup = false, ascending = true;
	pl_int skip1 = 0, skip2 = 0;

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
		pl_idx tmp_h_ctx = q->latest_ctx;
		LIST_TAIL(p4);

		if (!is_var(tmp_h) && (!is_compound(tmp_h) || strcmp(C_STR(q, tmp_h), "-")))
			return throw_error(q, tmp_h, tmp_h_ctx, "type_error", "pair");
	}

	if (is_nil(p3))
		return unify(q, p4, p4_ctx, make_nil(), q->st.curr_frame);

	if (skip1 && skip2 && (skip2 > skip1))
		return false;

	bool status = false;
	cell *l = nodesort4(q, p3, p3_ctx, dedup, ascending, arg, &status);
	if (!l) return status;
	return unify(q, p4, p4_ctx, l, q->st.curr_frame);
}

builtins g_sort_bifs[] =
{
	{"sort", 2, bif_iso_sort_2, "+list,?list", true, false, BLAH},
	{"msort", 2, bif_iso_msort_2, "+list,?list", true, false, BLAH},
	{"keysort", 2, bif_iso_keysort_2, "+list,?list", true, false, BLAH},

	{"sort", 4, bif_sort_4, "+integer,+atom,+list,?list", false, false, BLAH},

	{0}
};

