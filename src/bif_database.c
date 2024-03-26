#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>

#include "heap.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

static bool module_context(query *q, cell **p1, pl_idx p1_ctx)
{
	if (!is_var(*p1)) {
		if ((*p1)->val_off == g_colon_s) {
			*p1 = *p1 + 1;
			cell *cm = deref(q, *p1, p1_ctx);
			module *m = find_module(q->pl, C_STR(q, cm));

			if (!m) {
				return throw_error(q, cm, p1_ctx, "existence_error", "module");
			}

			*p1 += (*p1)->nbr_cells;
		}
	}

	return true;
}

static bool bif_clause_3(query *q)
{
	GET_FIRST_ARG(p1,callable_or_var);
	GET_NEXT_ARG(p2,callable_or_var);
	GET_NEXT_ARG(p3,atom_or_var);

	if (!module_context(q, &p1, p1_ctx))
		return false;

	if (is_var(p1) && is_var(p2) && is_var(p3))
		return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	for (;;) {
		clause *cl;

		if (!is_var(p3)) {
			uuid u;
			uuid_from_buf(C_STR(q, p3), &u);
			rule *r = find_in_db(q->st.m, &u);

			if (!r || (!u.u1 && !u.u2))
				break;

			q->st.curr_rule = r;
			cl = &r->cl;
			cell *head = get_head(cl->cells);

			if (!unify(q, p1, p1_ctx, head, q->st.fp))
				break;
		} else {
			if (match_clause(q, p1, p1_ctx, DO_CLAUSE) != true)
				break;

			char tmpbuf[128];
			uuid_to_buf(&q->st.curr_rule->u, tmpbuf, sizeof(tmpbuf));
			cell tmp;
			make_cstring(&tmp, tmpbuf);
			unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
			unshare_cell(&tmp);
			cl = &q->st.curr_rule->cl;
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
				last_match = !has_next_key(q);
			} else {
				last_match = true;
			}

			stash_frame(q, cl, last_match);
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

static void db_log(query *q, rule *r, enum log_type l)
{
	FILE *fp = q->pl->logfp;

	if (!fp)
		return;

	char tmpbuf[256];
	char *dst;
	q->quoted = 2;

	switch(l) {
	case LOG_ASSERTA:
		dst = print_term_to_strbuf(q, r->cl.cells, q->st.curr_frame, 1);
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		fprintf(fp, "%s:'$a_'((%s),'%s').\n", q->st.m->name, dst, tmpbuf);
		free(dst);
		break;
	case LOG_ASSERTZ:
		dst = print_term_to_strbuf(q, r->cl.cells, q->st.curr_frame, 1);
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		fprintf(fp, "%s:'$z_'((%s),'%s').\n", q->st.m->name, dst, tmpbuf);
		free(dst);
		break;
	case LOG_ERASE:
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		fprintf(fp, "%s:'$e_'('%s').\n", q->st.m->name, tmpbuf);
		break;
	}

	q->quoted = 0;
}

static bool bif_iso_clause_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable_or_var);

	if (!module_context(q, &p1, p1_ctx))
		return false;

	while (match_clause(q, p1, p1_ctx, DO_CLAUSE)) {
		if (q->did_throw) return true;
		clause *cl = &q->st.curr_rule->cl;
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
			bool last_match = !has_next_key(q);
			stash_frame(q, cl, last_match);
			return true;
		}

		q->retry = QUERY_RETRY;
		q->tot_backtracks++;
		retry_choice(q);
	}

	return false;
}

static bool bif_sys_clause_2(query *q)
{
	q->access_private = true;
	bool ok = bif_iso_clause_2(q);
	q->access_private = false;
	return ok;
}

static bool bif_sys_clause_3(query *q)
{
	q->access_private = true;
	bool ok = bif_clause_3(q);
	q->access_private = false;
	return ok;
}

static void predicate_purge_dirty_list(predicate *pr)
{
	unsigned cnt = 0;
	rule *r;

	while ((r = (rule*)list_pop_front(&pr->dirty)) != NULL) {
		list_delink(pr, r);
		clear_clause(&r->cl);
		free(r);
		cnt++;
	}

#if 0
	if (cnt)
		printf("*** predicate_purge_dirty_list %u\n", cnt);
#endif
}

bool do_retract(query *q, cell *p1, pl_idx p1_ctx, enum clause_type is_retract)
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

	rule *r = q->st.curr_rule;
	db_log(q, r, LOG_ERASE);
	retract_from_db(r->owner->m, r);
	bool last_match = (is_retract == DO_RETRACT) && !has_next_key(q);
	stash_frame(q, &r->cl, last_match);
	return true;
}

static bool bif_iso_retract_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (!module_context(q, &p1, p1_ctx))
		return false;

	prolog_lock(q->pl);
	bool ok = do_retract(q, p1, p1_ctx, DO_RETRACT);
	prolog_unlock(q->pl);
	return ok;
}

static bool bif_iso_retractall_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (!module_context(q, &p1, p1_ctx))
		return false;

	cell *head = deref(q, get_head(p1), p1_ctx);
	predicate *pr = search_predicate(q->st.m, head, NULL);

	if (!pr) {
		bool found = false;

		if (get_builtin_term(q->st.m, head, &found, NULL), found)
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");

		return true;
	}

	prolog_lock(q->pl);

	while (do_retract(q, p1, p1_ctx, DO_RETRACTALL)) {
		if (q->did_throw) {
			prolog_unlock(q->pl);
			return true;
		}

		q->retry = QUERY_RETRY;
		q->tot_backtracks++;
		retry_choice(q);
	}

	if (!pr->refcnt)
		predicate_purge_dirty_list(pr);

	if (pr->idx && !pr->cnt) {
		sl_destroy(pr->idx2);
		sl_destroy(pr->idx);
		pr->idx = pr->idx2 = NULL;
	}

	prolog_unlock(q->pl);
	return true;
}

bool do_abolish(query *q, cell *c_orig, cell *c_pi, bool hard)
{
	predicate *pr = search_predicate(q->st.m, c_pi, NULL);
	if (!pr) return true;

	if (!pr->is_dynamic)
		return throw_error(q, c_orig, q->st.curr_frame, "permission_error", "modify,static_procedure");

	for (rule *r = pr->head; r; r = r->next)
		retract_from_db(r->owner->m, r);

	if (pr->idx && !pr->cnt) {
		predicate_purge_dirty_list(pr);
	} else {
		rule *r;

		while ((r = (rule*)list_pop_front(&pr->dirty)) != NULL)
			list_push_back(&q->dirty, r);
	}

	sl_destroy(pr->idx2);
	sl_destroy(pr->idx);
	pr->idx = pr->idx2 = NULL;
	pr->is_processed = false;
	pr->head = pr->tail = NULL;
	pr->cnt = 0;

	if (hard)
		pr->is_abolished = true;

	return true;
}

static bool bif_iso_abolish_1(query *q)
{
	GET_FIRST_ARG(p1,callable);

	if (!is_var(p1)) {
		if (p1->val_off == g_colon_s) {
			p1 = p1 + 1;
			p1 += p1->nbr_cells;
		}
	}

	if (p1->arity != 2)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (CMP_STRING_TO_CSTR(q, p1, "/") && CMP_STRING_TO_CSTR(q, p1, "//"))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *p1_name = p1 + 1;
	p1_name = deref(q, p1_name, p1_ctx);

	if (!is_atom(p1_name))
		return throw_error(q, p1_name, p1_ctx, "type_error", "atom");

	cell *p1_arity = p1 + 2;
	p1_arity = deref(q, p1_arity, p1_ctx);

	if (!CMP_STRING_TO_CSTR(q, p1, "//"))
		p1_arity += 2;

	if (!is_integer(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "type_error", "integer");

	if (is_negative(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "domain_error", "not_less_than_zero");

	if (get_smallint(p1_arity) > MAX_ARITY)
		return throw_error(q, p1_arity, p1_ctx, "representation_error", "max_arity");

	bool found = false;

	if (get_builtin(q->pl, C_STR(q, p1_name), C_STRLEN(q, p1_name), get_smallint(p1_arity), &found, NULL), found) {
		prolog_unlock(q->pl);
		return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");
	}

	cell tmp;
	tmp = *p1_name;
	tmp.arity = get_smallint(p1_arity);
	CLR_OP(&tmp);

	prolog_lock(q->pl);
	bool ok = do_abolish(q, p1, &tmp, true);
	prolog_unlock(q->pl);
	return ok;
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
	pl_idx nbr_cells = p->cl->cidx;
	clause_assign_vars(p, 0, true);
	memset(p->vartab.vars, 0, sizeof(p->vartab.vars));

	for (pl_idx i = 0; i < nbr_cells; i++) {
		cell *c = p->cl->cells+i;

		if (!is_var(c))
			continue;

		assert(c->var_nbr < MAX_VARS);
		p->vartab.vars[c->var_nbr]++;
	}

	for (pl_idx i = 0; i < nbr_cells; i++) {
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

		c->val_off = new_atom(p->m->pl, tmpbuf);
		c->flags = 0;
	}
}

static bool bif_iso_asserta_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp);
	cell *head = get_head(tmp);

	if (is_var(head))
		return throw_error(q, head, q->st.curr_frame, "instantiation_error", "args_not_sufficiently_instantiated");

	if (!is_interned(head) && !is_cstring(head))
		return throw_error(q, head, q->st.curr_frame, "type_error", "callable");

	bool found = false;

	if (get_builtin_term(q->st.m, head, &found, NULL), found) {
		if (!GET_OP(head)) {
			return throw_error(q, head, q->st.curr_frame, "permission_error", "modify,static_procedure");
		}
	}

	cell *tmp2, *body = get_body(tmp);

	if (body && ((tmp2 = check_body_callable(body)) != NULL)) {
		prolog_unlock(q->pl);
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");
	}

	pl_idx nbr_cells = tmp->nbr_cells;
	parser *p = parser_create(q->st.m);

	if (nbr_cells > p->cl->nbr_allocated_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		check_heap_error(p->cl, prolog_unlock(q->pl));
		p->cl->nbr_allocated_cells = nbr_cells;
	}

	p->cl->cidx = dup_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_interned(h)) {
		return throw_error(q, h, q->st.curr_frame, "type_error", "callable");
	}

	prolog_lock(q->pl);
	rule *r = asserta_to_db(q->st.m, p->cl->nbr_vars, p->cl->cells, 0);
	prolog_unlock(q->pl);

	p->cl->cidx = 0;
	parser_destroy(p);

	if (!r)
		return throw_error(q, h, q->st.curr_frame, "permission_error", "modify,static_procedure");

	db_log(q, r, LOG_ASSERTA);
	return true;
}

static bool bif_iso_assertz_1(query *q)
{
	GET_FIRST_ARG(p1,callable);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp);
	cell *head = get_head(tmp);

	if (is_var(head))
		return throw_error(q, head, q->st.curr_frame, "instantiation_error", "args_not_sufficiently_instantiated");

	if (!is_interned(head) && !is_cstring(head))
		return throw_error(q, head, q->st.curr_frame, "type_error", "callable");

	bool found = false, evaluable = false;

	if (get_builtin_term(q->st.m, head, &found, &evaluable), found && !evaluable) {
		if (!GET_OP(head)) {
			return throw_error(q, head, q->st.curr_frame, "permission_error", "modify,static_procedure");
		}
	}

	cell *tmp2, *body = get_body(tmp);

	if (body && ((tmp2 = check_body_callable(body)) != NULL)) {
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");
	}

	pl_idx nbr_cells = tmp->nbr_cells;
	parser *p = parser_create(q->st.m);

	if (nbr_cells > p->cl->nbr_allocated_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		check_heap_error(p->cl, prolog_unlock(q->pl));
		p->cl->nbr_allocated_cells = nbr_cells;
	}

	p->cl->cidx = dup_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_interned(h)) {
		return throw_error(q, h, q->st.curr_frame, "type_error", "callable");
	}

	prolog_lock(q->pl);
	rule *r = assertz_to_db(q->st.m, p->cl->nbr_vars, p->cl->cells, false);
	prolog_unlock(q->pl);

	p->cl->cidx = 0;
	parser_destroy(p);

	if (!r)
		return throw_error(q, h, q->st.curr_frame, "permission_error", "modify,static_procedure");

	db_log(q, r, LOG_ASSERTZ);
	return true;
}

static bool do_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *head = deref(q, get_head(p1), p1_ctx);

	if (is_var(head))
		return throw_error(q, head, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin_term(q->st.m, head, &found, NULL), found) {
		if (!GET_OP(head)) {
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");
		}
	}

	cell *body = get_body(p1);

	if (body)
		body = deref(q, body, p1_ctx);

	if (body && !is_callable(body)) {
		return throw_error(q, body, q->latest_ctx, "type_error", "callable");
	}

	cell *tmp2;

	if (body && ((tmp2 = check_body_callable(body)) != NULL)) {
		return throw_error(q, tmp2, q->latest_ctx, "type_error", "callable");
	}

	GET_NEXT_ARG(p2,atom_or_var);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp);

	pl_idx nbr_cells = tmp->nbr_cells;
	parser *p = parser_create(q->st.m);

	if (nbr_cells > p->cl->nbr_allocated_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		check_heap_error(p->cl, prolog_unlock(q->pl));
		p->cl->nbr_allocated_cells = nbr_cells;
	}

	p->cl->cidx = dup_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_interned(h)) {
		return throw_error(q, h, q->latest_ctx, "type_error", "callable");
	}

	prolog_lock(q->pl);
	rule *r = asserta_to_db(q->st.m, p->cl->nbr_vars, p->cl->cells, 0);
	prolog_unlock(q->pl);

	p->cl->cidx = 0;
	parser_destroy(p);

	if (!r)
		return throw_error(q, h, q->st.curr_frame, "permission_error", "modify,static_procedure");

	if (!is_var(p2)) {
		uuid u;
		uuid_from_buf(C_STR(q, p2), &u);
		r->u = u;
	} else {
		uuid_gen(q->pl, &r->u);
		char tmpbuf[128];
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2;
		make_cstring(&tmp2, tmpbuf);
		unify(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
		unshare_cell(&tmp2);
	}

	db_log(q, r, LOG_ASSERTA);
	return true;
}

static bool bif_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,var);
	return do_asserta_2(q);
}

static bool bif_sys_asserta_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,atom);
	return do_asserta_2(q);
}

static bool do_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	cell *head = deref(q, get_head(p1), p1_ctx);

	if (is_var(head))
		return throw_error(q, head, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

	bool found = false;

	if (get_builtin_term(q->st.m, head, &found, NULL), found) {
		if (!GET_OP(head)) {
			return throw_error(q, head, q->latest_ctx, "permission_error", "modify,static_procedure");
		}
	}

	cell *body = get_body(p1);

	if (body)
		body = deref(q, body, p1_ctx);

	if (body && !is_callable(body)) {
		return throw_error(q, body, q->latest_ctx, "type_error", "callable");
	}

	cell *tmp2;

	if (body && ((tmp2 = check_body_callable(body)) != NULL)) {
		return throw_error(q, tmp2, q->latest_ctx, "type_error", "callable");
	}

	GET_NEXT_ARG(p2,atom_or_var);
	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_copy_to_tmp(q, p1, p1_ctx, false);
	check_heap_error(tmp);

	pl_idx nbr_cells = tmp->nbr_cells;
	parser *p = parser_create(q->st.m);

	if (nbr_cells > p->cl->nbr_allocated_cells) {
		p->cl = realloc(p->cl, sizeof(clause)+(sizeof(cell)*(nbr_cells+1)));
		check_heap_error(p->cl, prolog_unlock(q->pl));
		p->cl->nbr_allocated_cells = nbr_cells;
	}

	p->cl->cidx = dup_cells(p->cl->cells, tmp, nbr_cells);
	do_term_assign_vars(p);
	term_to_body(p);
	cell *h = get_head(p->cl->cells);

	if (is_cstring(h))
		convert_to_literal(q->st.m, h);

	if (!is_interned(h)) {
		return throw_error(q, h, q->latest_ctx, "type_error", "callable");
	}

	prolog_lock(q->pl);
	rule *r = assertz_to_db(q->st.m, p->cl->nbr_vars, p->cl->cells, false);
	prolog_unlock(q->pl);

	p->cl->cidx = 0;
	parser_destroy(p);

	if (!r)
		return throw_error(q, h, q->st.curr_frame, "permission_error", "modify,static_procedure");

	if (!is_var(p2)) {
		uuid u;
		uuid_from_buf(C_STR(q, p2), &u);
		r->u = u;
	} else {
		uuid_gen(q->pl, &r->u);
		char tmpbuf[128];
		uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
		cell tmp2;
		make_cstring(&tmp2, tmpbuf);
		unify(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
		unshare_cell(&tmp2);
	}

	db_log(q, r, LOG_ASSERTZ);
	return true;
}

static bool bif_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,var);
	return do_assertz_2(q);
}

static bool bif_sys_assertz_2(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	GET_NEXT_ARG(p2,atom);
	return do_assertz_2(q);
}

void save_db(FILE *fp, query *q, int logging)
{
	q->listing = true;
	q->double_quotes = true;

	for (predicate *pr = (predicate*)list_front(&q->st.m->predicates);
		pr; pr = (predicate*)list_next(pr)) {
		if (pr->is_prebuilt)
			continue;

		const char *src = C_STR(q, &pr->key);

		if (src[0] == '$')
			continue;

		for (rule *r = pr->head; r; r = r->next) {
			if (r->cl.dbgen_retracted)
				continue;

			if (logging)
				fprintf(fp, "'$z_'(");

			for (unsigned i = 0; i < MAX_IGNORES; i++)
				q->ignores[i] = false;

			q->print_idx = 0;
			print_term(q, fp, r->cl.cells, 0, 0);

			if (logging) {
				char tmpbuf[256];
				uuid_to_buf(&r->u, tmpbuf, sizeof(tmpbuf));
				fprintf(fp, ",'%s')", tmpbuf);
			}

			if (q->last_thing == WAS_SYMBOL)
				fprintf(fp, " ");

			fprintf(fp, ".\n");
		}
	}

	q->double_quotes = false;
	q->listing = false;
}

static bool bif_abolish_2(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,iso_list_or_nil);

	if (!is_var(p1)) {
		if (p1->val_off == g_colon_s) {
			p1 = p1 + 1;
			p1 += p1->nbr_cells;
		}
	}

	if (p1->arity != 2)
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	if (CMP_STRING_TO_CSTR(q, p1, "/") && CMP_STRING_TO_CSTR(q, p1, "//"))
		return throw_error(q, p1, p1_ctx, "type_error", "predicate_indicator");

	cell *p1_name = p1 + 1;
	p1_name = deref(q, p1_name, p1_ctx);

	if (!is_atom(p1_name))
		return throw_error(q, p1_name, p1_ctx, "type_error", "atom");

	cell *p1_arity = p1 + 2;
	p1_arity = deref(q, p1_arity, p1_ctx);

	if (!CMP_STRING_TO_CSTR(q, p1, "//"))
		p1_arity += 2;

	if (!is_integer(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "type_error", "integer");

	if (is_negative(p1_arity))
		return throw_error(q, p1_arity, p1_ctx, "domain_error", "not_less_than_zero");

	if (get_smallint(p1_arity) > MAX_ARITY)
		return throw_error(q, p1_arity, p1_ctx, "representation_error", "max_arity");

	bool force = false, tree = false;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_compound(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);

			if (!CMP_STRING_TO_CSTR(q, c, "force")) {
				if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "true")) {
					force = true;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "false")) {
					force = false;
				}
			} else if (!CMP_STRING_TO_CSTR(q, c, "tree") && false) {
				if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "true")) {
					tree = force;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "false")) {
					tree = false;
				}
			} else
				return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;

		if (is_var(p2))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	if (!force) {
		bool found = false;

		if (get_builtin(q->pl, C_STR(q, p1_name), C_STRLEN(q, p1_name), get_smallint(p1_arity), &found, NULL), found) {
			return throw_error(q, p1, p1_ctx, "permission_error", "modify,static_procedure");
		}
	}

	cell tmp;
	tmp = *p1_name;
	tmp.arity = get_smallint(p1_arity);
	CLR_OP(&tmp);

	bool ok = do_abolish(q, p1, &tmp, true);
	return ok;
}

bool do_erase(module* m, const char *str)
{
	uuid u;
	uuid_from_buf(str, &u);
	erase_from_db(m, &u);
	return true;
}

static bool bif_erase_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	return do_erase(q->st.m, C_STR(q, p1));
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

static bool bif_sys_retract_on_backtrack_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	int var_nbr;

	if ((var_nbr = create_vars(q, 1)) < 0)
		return false;

	blob *b = calloc(1, sizeof(blob));
	b->ptr = (void*)q->st.m;
	b->ptr2 = (void*)strdup(C_STR(q, p1));
	cell c, v;
	make_ref(&c, var_nbr, q->st.curr_frame);
	make_dbref(&v, b);
	return unify(q, &c, q->st.curr_frame, &v, q->st.curr_frame);
}

builtins g_database_bifs[] =
{
	{"abolish", 1, bif_iso_abolish_1, "+predicate_indicator", true, false, BLAH},
	{"asserta", 1, bif_iso_asserta_1, "+term", true, false, BLAH},
	{"assertz", 1, bif_iso_assertz_1, "+term", true, false, BLAH},
	{"retract", 1, bif_iso_retract_1, "+term", true, false, BLAH},
	{"retractall", 1, bif_iso_retractall_1, "+term", true, false, BLAH},
	{"clause", 2, bif_iso_clause_2, "+term,?term", true, false, BLAH},

	{"assert", 1, bif_iso_assertz_1, "+term", false, false, BLAH},
	{"asserta", 2, bif_asserta_2, "+term,-string", false, false, BLAH},
	{"assertz", 2, bif_assertz_2, "+term,-string", false, false, BLAH},
	{"erase", 1, bif_erase_1, "+string", false, false, BLAH},
	{"clause", 3, bif_clause_3, "?term,?term,-string", false, false, BLAH},
	{"abolish", 2, bif_abolish_2, "+term,+list", false, false, BLAH},
	{"instance", 2, bif_instance_2, "+string,?term", false, false, BLAH},

	{"$clause", 2, bif_sys_clause_2, "?term,?term", false, false, BLAH},
	{"$clause", 3, bif_sys_clause_3, "?term,?term,-string", false, false, BLAH},
	{"$retract_on_backtrack", 1, bif_sys_retract_on_backtrack_1, "+string", false, false, BLAH},

	{"$a_", 2, bif_sys_asserta_2, "+term,+atom", true, false, BLAH},
	{"$z_", 2, bif_sys_assertz_2, "+term,+atom", true, false, BLAH},
	{"$e_", 1, bif_erase_1, "+atom", true, false, BLAH},

	{0}
};

