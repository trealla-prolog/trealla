#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heap.h"
#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

bool fn_iso_true_0(query *q)
{
	q->tot_goals--;
	return true;
}

bool fn_iso_fail_0(query *q)
{
	q->tot_goals--;
	return false;
}

bool fn_sys_drop_barrier_1(query *q)
{
	GET_FIRST_ARG(p1,integer)
	q->tot_goals--;
	drop_barrier(q, get_smallint(p1));
	return true;
}

void do_cleanup(query *q, cell *c, pl_idx c_ctx)
{
	ensure(init_tmp_heap(q));
	cell *tmp2 = deep_clone_to_tmp(q, c, c_ctx);
	ensure(tmp2);
	cell *tmp = clone_to_heap(q, true, tmp2, q->st.curr_frame, 2);
	ensure(tmp);
	pl_idx nbr_cells = 1 + tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_prune_s, fn_sys_prune_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
}

bool fn_sys_cleanup_if_det_1(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,integer);
	const frame *f = GET_CURR_FRAME();
	choice *ch = GET_CURR_CHOICE();

	if ((q->cp-1) != get_smallint(p1))
		return true;

	drop_choice(q);
	ch = GET_CURR_CHOICE();

	if (!ch->register_cleanup)
		return true;

	if (ch->fail_on_retry)
		return true;

	drop_choice(q);
	ch->fail_on_retry = true;
	cell *c = deref(q, ch->st.curr_cell, ch->st.curr_frame);
	pl_idx c_ctx = q->latest_ctx;
	c = deref(q, FIRST_ARG(c), c_ctx);
	c_ctx = q->latest_ctx;
	do_cleanup(q, c, c_ctx);
	return true;
}

// module:goal

bool fn_iso_invoke_2(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,callable);
	module *m = find_module(q->pl, C_STR(q, p1));

	if (!m)
		m = module_create(q->pl, C_STR(q, p1));

	cell *tmp = clone_to_heap(q, true, p2, q->st.curr_frame, 1);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1;

	if (!is_builtin(p2) /*&& !tmp[nbr_cells].match*/)
		tmp[nbr_cells].match = find_predicate(m, p2);

	nbr_cells += p2->nbr_cells;
	make_call(q, tmp+nbr_cells);
	q->st.curr_cell = tmp;
	q->st.curr_frame = p2_ctx;
	q->st.m = q->save_m = m;
	return true;
}

bool fn_call_0(query *q, cell *p1, pl_idx p1_ctx)
{
	q->tot_goals--;

	if (!is_callable(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "callable");

	cell *tmp = clone_to_heap(q, false, p1, p1_ctx, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = 0 + tmp->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, fn_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	q->st.curr_cell = tmp;
	return true;
}

bool fn_iso_call_n(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,callable);

	if ((p1->val_off == g_colon_s) && (p1->arity == 2)) {
		cell *pm = p1 + 1;
		pm = deref(q, pm, p1_ctx);

		if (!is_atom(pm) && !is_var(pm))
			return throw_error(q, pm, p1_ctx, "type_error", "callable");

		module *m = find_module(q->pl, C_STR(q, pm));
		if (m) q->st.m = m;
		p1 += 2;
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	check_heap_error(init_tmp_heap(q));
	check_heap_error(deep_clone_to_tmp(q, p1, p1_ctx));
	unsigned arity = p1->arity;
	unsigned args = 1;

	while (args < q->st.curr_cell->arity) {
		GET_NEXT_ARG(p2,any);
		check_heap_error(deep_clone_to_tmp(q, p2, p2_ctx));
		arity++;
		args++;
	}

	cell *tmp2 = get_tmp_heap(q, 0);
	tmp2->nbr_cells = tmp_heap_used(q);
	tmp2->arity = arity;

	if (is_cstring(tmp2)) {
		share_cell(tmp2);
		convert_to_literal(q->st.m, tmp2);
	}

	bool found = false;

	if ((tmp2->match = search_predicate(q->st.m, tmp2, NULL)) != NULL) {
		tmp2->flags &= ~FLAG_BUILTIN;
	} else if ((tmp2->fn_ptr = get_builtin_term(q->st.m, tmp2, &found, NULL)), found) {
		tmp2->flags |= FLAG_BUILTIN;
	} else {
		tmp2->flags &= ~FLAG_BUILTIN;
	}

	if (arity <= 2) {
		const char *functor = C_STR(q, tmp2);
		unsigned specifier;

		if (search_op(q->st.m, functor, &specifier, false))
			SET_OP(tmp2, specifier);
	}

	if (check_body_callable(tmp2) != NULL)
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, tmp2, q->st.curr_frame, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1+tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, fn_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	q->st.curr_cell = tmp;
	return true;
}

bool fn_iso_call_1(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,callable);
	check_heap_error(init_tmp_heap(q));
	cell *tmp2 = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(tmp2);
	bool found = false;

	if (!tmp2->match) {
		if ((tmp2->fn_ptr = get_builtin_term(q->st.m, tmp2, &found, NULL)), found)
			tmp2->flags |= FLAG_BUILTIN;
	}

	if (check_body_callable(tmp2) != NULL)
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, tmp2, q->st.curr_frame, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1+tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, fn_sys_drop_barrier_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	q->st.curr_cell = tmp;
	return true;
}

// goal, !

bool fn_iso_once_1(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,callable);
	check_heap_error(init_tmp_heap(q));
	cell *tmp2 = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(tmp2);
	bool found = false;

	if (!tmp2->match) {
		if ((tmp2->fn_ptr = get_builtin_term(q->st.m, tmp2, &found, NULL)), found)
			tmp2->flags |= FLAG_BUILTIN;
	}

	if (check_body_callable(tmp2) != NULL)
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, tmp2, q->st.curr_frame, 2);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1+tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_prune_s, fn_sys_prune_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	q->st.curr_cell = tmp;
	return true;
}

// if -> ! ; true

bool fn_ignore_1(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,callable);
	check_heap_error(init_tmp_heap(q));
	cell *tmp2 = deep_clone_to_tmp(q, p1, p1_ctx);
	check_heap_error(tmp2);
	bool found = false;

	if (!tmp2->match) {
		if ((tmp2->fn_ptr = get_builtin_term(q->st.m, tmp2, &found, NULL)), found)
			tmp2->flags |= FLAG_BUILTIN;
	}

	if (check_body_callable(tmp2) != NULL)
		return throw_error(q, tmp2, q->st.curr_frame, "type_error", "callable");

	cell *tmp = clone_to_heap(q, true, tmp2, q->st.curr_frame, 2);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1+tmp2->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_prune_s, fn_sys_prune_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->succeed_on_retry = true;
	q->st.curr_cell = tmp;
	return true;
}

// if -> then

bool fn_iso_if_then_2(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	const frame *f = GET_CURR_FRAME();
	cell *tmp = clone_to_heap(q, true, p1, p1_ctx, 1+p2->nbr_cells+1);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_prune_s, fn_sys_prune_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	q->st.curr_cell = tmp;
	return true;
}

// if *-> then

bool fn_if_2(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	const frame *f = GET_CURR_FRAME();
	cell *tmp = clone_to_heap(q, true, p1, p1_ctx, 2+p2->nbr_cells+1);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_soft_prune_s, fn_sys_soft_prune_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->fail_on_retry = true;
	q->st.curr_cell = tmp;
	return true;
}

// if -> then ; else

static bool do_if_then_else(query *q, cell *p1, cell *p2, cell *p3)
{
	q->tot_goals--;

	if (q->retry) {
		q->retry = QUERY_SKIP;
		q->st.curr_cell = p3;
		return true;
	}

	const frame *f = GET_CURR_FRAME();
	cell *tmp = clone_to_heap(q, true, p1, q->st.curr_frame, 1+p2->nbr_cells+1);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_prune_s, fn_sys_prune_0, 0, 0);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

// if *-> then ; else

static bool do_if_else(query *q, cell *p1, cell *p2, cell *p3)
{
	q->tot_goals--;

	if (q->retry) {
		q->retry = QUERY_SKIP;
		q->st.curr_cell = p3;
		return true;
	}

	cell *tmp = clone_to_heap(q, true, p1, q->st.curr_frame, 2+p2->nbr_cells+1);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_soft_prune_s, fn_sys_soft_prune_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	nbr_cells += safe_copy_cells(tmp+nbr_cells, p2, p2->nbr_cells);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	q->st.curr_cell = tmp;
	return true;
}

// if_(if,then,else)

bool fn_if_3(query *q)
{
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,callable);
	return do_if_else(q, p1, p2, p3);
}

// goal , goal

bool fn_iso_conjunction_2(query *q)
{
	q->tot_goals--;
	q->retry = QUERY_SKIP;
	q->st.curr_cell++;
	return true;
}

// goal ; goal

bool fn_iso_disjunction_2(query *q)
{
	cell *c = q->st.curr_cell+1;

	if (is_callable(c)) {
		if (is_cstring(c) && !CMP_STR_TO_CSTR(q, c, "[]"))
			return throw_error(q, c, q->st.curr_frame, "type_error", "callable");

		if (c->fn_ptr && (c->fn_ptr->fn == fn_iso_if_then_2)) {
			cell *p1 = q->st.curr_cell + 2;
			cell *p2 = p1 + p1->nbr_cells;
			cell *p3 = p2 + p2->nbr_cells;
			return do_if_then_else(q, p1, p2, p3);
		}

		if (c->fn_ptr && (c->fn_ptr->fn == fn_if_2)) {
			cell *p1 = q->st.curr_cell + 2;
			cell *p2 = p1 + p1->nbr_cells;
			cell *p3 = p2 + p2->nbr_cells;
			return do_if_else(q, p1, p2, p3);
		}
	}

	q->tot_goals--;
	GET_FIRST_ARG(p1,callable);
	GET_NEXT_ARG(p2,any);

	if (!is_callable(p2))
		return throw_error(q, q->st.curr_cell, q->st.curr_frame, "type_error", "callable");

	if (q->retry) {
		q->retry = QUERY_SKIP;
		q->st.curr_cell = p2;
		return true;
	}

	cell *tmp = clone_to_heap(q, true, p1, p1_ctx, 1);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1 + p1->nbr_cells;
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_choice(q));
	q->st.curr_cell = tmp;
	return true;
}

// if -> !, fail ; true

bool fn_iso_negation_1(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,callable);
	cell *tmp = clone_to_heap(q, true, p1, p1_ctx, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1 + p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_prune_s, fn_sys_prune_0, 0, 0);
	make_struct(tmp+nbr_cells++, g_fail_s, fn_iso_fail_0, 0, 0);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_barrier(q));
	choice *ch = GET_CURR_CHOICE();
	ch->succeed_on_retry = true;
	q->st.curr_cell = tmp;
	return true;
}

bool fn_iso_cut_0(query *q)
{
	q->tot_goals--;
	cut(q);
	return true;
}

bool fn_sys_prune_0(query *q)
{
	q->tot_goals--;
	prune(q, false, 0);
	return true;
}

bool fn_sys_soft_prune_1(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,integer);
	prune(q, true, get_smallint(p1));
	return true;
}

bool fn_sys_block_catcher_1(query *q)
{
	q->tot_goals--;
	GET_FIRST_ARG(p1,integer);
	pl_idx cp = get_smalluint(p1);
	choice *ch = GET_CHOICE(cp);

	if (!ch->catchme_retry)
		return false;

	if (q->retry) {
		ch->block_catcher = false;
		return false;
	}

	if (drop_barrier(q, cp))
		return true;

	ch->block_catcher = true;
	check_heap_error(push_choice(q));
	return true;
}

bool fn_iso_catch_3(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (q->retry && q->ball) {
		GET_NEXT_ARG(p2,any);
		return unify(q, p2, p2_ctx, q->ball, q->st.curr_frame);
	}

	// Second time through? Try the recover goal...

	if (q->retry == QUERY_EXCEPTION) {
		GET_NEXT_ARG(p2,any);
		GET_NEXT_ARG(p3,callable);
		q->retry = QUERY_OK;
		cell *tmp = clone_to_heap(q, true, p3, p3_ctx, 3);
		check_heap_error(tmp);
		pl_idx nbr_cells = 1+p3->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_drop_barrier_s, fn_sys_drop_barrier_1, 1, 1);
		make_uint(tmp+nbr_cells++, q->cp);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_catcher(q, QUERY_EXCEPTION));
		q->st.curr_cell = tmp;
		return true;
	}

	if (q->retry)
		return false;

	// First time through? Try the primary goal...

	cell *tmp = clone_to_heap(q, true, p1, p1_ctx, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1+p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_block_catcher_s, fn_sys_block_catcher_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_catcher(q, QUERY_RETRY));
	q->st.curr_cell = tmp;
	return true;
}

bool fn_sys_call_cleanup_3(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (q->retry && q->ball) {
		GET_NEXT_ARG(p2,any);
		cell *tmp = deep_clone_to_heap(q, q->ball, q->st.curr_frame);
		check_heap_error(tmp);
		return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
	}

	// Second time through? Try the recover goal...

	if (q->retry == QUERY_EXCEPTION) {
		GET_NEXT_ARG(p2,any);
		GET_NEXT_ARG(p3,callable);
		q->retry = QUERY_OK;
		cell *tmp = clone_to_heap(q, true, p3, p3_ctx, 3);
		check_heap_error(tmp);
		pl_idx nbr_cells = 1+p3->nbr_cells;
		make_struct(tmp+nbr_cells++, g_sys_cleanup_if_det_s, fn_sys_cleanup_if_det_1, 1, 1);
		make_uint(tmp+nbr_cells++, q->cp);
		make_call(q, tmp+nbr_cells);
		check_heap_error(push_catcher(q, QUERY_EXCEPTION));
		q->st.curr_cell = tmp;
		return true;
	}

	if (q->retry)
		return false;

	// First time through? Try the primary goal...

	cell *tmp = clone_to_heap(q, true, p1, p1_ctx, 3);
	check_heap_error(tmp);
	pl_idx nbr_cells = 1+p1->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_cleanup_if_det_s, fn_sys_cleanup_if_det_1, 1, 1);
	make_uint(tmp+nbr_cells++, q->cp);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_catcher(q, QUERY_RETRY));
	q->st.curr_cell = tmp;
	return true;
}

static cell *parse_to_heap(query *q, const char *src)
{
	SB(s);
	SB_sprintf(s, "%s.", src);
	parser *p2 = parser_create(q->st.m);
	check_error(p2);
	frame *f = GET_CURR_FRAME();
	p2->read_term_slots = f->actual_slots;
	p2->skip = true;
	p2->srcptr = SB_cstr(s);
	tokenize(p2, false, false);
	xref_rule(p2->m, p2->cl, NULL);
	p2->read_term_slots = 0;
	SB_free(s);

	if (p2->nbr_vars) {
		if (!create_vars(q, p2->nbr_vars)) {
			parser_destroy(p2);
			return false;
		}
	}

	cell *tmp = deep_clone_to_heap(q, p2->cl->cells, q->st.curr_frame);
	check_error(tmp, parser_destroy(p2));
	parser_destroy(p2);
	return tmp;
}

bool find_exception_handler(query *q, char *ball)
{
	while (retry_choice(q)) {
		const choice *ch = GET_CHOICE(q->cp);

		if (ch->block_catcher)
			continue;

		if (!ch->catchme_retry)
			continue;

		q->ball = parse_to_heap(q, ball);
		q->retry = QUERY_EXCEPTION;

		if (fn_iso_catch_3(q) != true) {
			q->ball = NULL;
			continue;
		}

		q->ball = NULL;
		return true;
	}

	cell *e = parse_to_heap(q, ball);
	pl_idx e_ctx = q->st.curr_frame;

	if (!strcmp(C_STR(q, e+1), "$aborted")) {
		fprintf(stdout, "%% Execution aborted\n");
		q->pl->did_dump_vars = true;
		q->ball = NULL;
		q->error = true;
		return false;
	}

	if (!q->is_redo)
		fprintf(stdout, "   ");
	else
		fprintf(stdout, "  ");

	if (q->run_init)
		fprintf(stdout, "\rWarning: Initialization goal exception: ");

	//if (!q->run_init/*!is_interned(e) || strcmp(C_STR(q, e), "error")*/)
	//	fprintf(stdout, "throw(");

	if (is_cyclic_term(q, e, e_ctx)) {
		q->quoted = 1;
		print_term(q, stdout, e, e_ctx, 0);
	} else {
		q->quoted = 1;
		print_term(q, stdout, e, e_ctx, 1);
	}

	//if (!q->run_init/*!is_interned(e) || strcmp(C_STR(q, e), "error")*/)
	//	fprintf(stdout, ")");

	fprintf(stdout, ".\n");
	q->quoted = 0;
	q->pl->did_dump_vars = true;
	q->ball = NULL;
	//q->error = true;
	q->abort = true;
	return false;
}

bool fn_iso_throw_1(query *q)
{
	GET_FIRST_ARG(p1,nonvar);
	q->parens = q->numbervars = true;
	q->quoted = true;
	char *ball = print_term_to_strbuf(q, p1, p1_ctx, 1);
	clear_write_options(q);

	if (!find_exception_handler(q, ball)) {
		free(ball);
		return false;
	}

	free(ball);
	return fn_iso_catch_3(q);
}

bool throw_error3(query *q, cell *c, pl_idx c_ctx, const char *err_type, const char *expected, cell *goal)
{
	if (g_tpl_interrupt)
		return false;

	q->did_throw = true;
	q->quoted = 0;

	if (!strncmp(expected, "iso_", 4))
		expected += 4;

	char tmpbuf[1024];
	snprintf(tmpbuf, sizeof(tmpbuf), "%s", expected);
	tmpbuf[sizeof(tmpbuf)-1] = '\0';
	char *ptr;

	if (!strcmp(err_type, "type_error")
		&& ((ptr = strstr(tmpbuf, "_or")) != NULL))
		*ptr = '\0';

	if (!strcmp(err_type, "type_error") && !strcmp(expected, "stream"))
		err_type = "existence_error";

	expected = tmpbuf;
	char functor[1024];
	functor[0] = '\0';

	if (!strcmp(expected, "smallint"))
		expected = "integer";

	if (!is_var(c)) {
		char *tmpbuf = DUP_STR(q, goal);
		snprintf(functor, sizeof(functor), "%s", tmpbuf);
		functor[sizeof(functor)-1] = '\0';
		free(tmpbuf);
	}

	int extra = 0;
	const char *eptr = expected;

	while ((eptr = strchr(eptr, ',')) != NULL) {
		extra += 1;
		eptr++;
	}

	bool is_abolish = false;

	if (!strcmp(C_STR(q, q->st.curr_cell), "abolish"))
		is_abolish = true;

	bool is_builtin = false, evaluable = false;
	get_builtin_term(q->st.m, c, &is_builtin, &evaluable);
	bool is_op = search_op(q->st.m, C_STR(q, c), NULL, true) > 0;

	cell *tmp;

	if (is_var(c)) {
		err_type = "instantiation_error";
		//printf("error(%s,%s).\n", err_type, expected);
		tmp = alloc_on_heap(q, 3);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 2);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, err_type));
		make_atom(tmp+nbr_cells, index_from_pool(q->pl, expected));
	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "var")) {
		err_type = "uninstantiation_error";
		//printf("error(%s(%s),(%s)/%u).\n", err_type, C_STR(q, c), functor, goal->arity);
		tmp = alloc_on_heap(q, 6+(c->nbr_cells-1));
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 5+(c->nbr_cells-1));
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 1, 1+(c->nbr_cells-1));
		safe_copy_cells(tmp+nbr_cells, c, c->nbr_cells);
		nbr_cells += c->nbr_cells;
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "type_error") && !strcmp(expected, "evaluable")) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, C_STR(q, c), c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 8);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2, 4);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "permission_error") && is_structure(c) && CMP_STR_TO_CSTR(q, c, "/") && is_var(FIRST_ARG(c))) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9+extra);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 8+extra);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2+extra, 4+extra);

		if (!extra)
			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
		}

		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "permission_error") && (is_builtin || (is_op && c->arity)) && !is_abolish) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9+extra);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 8+extra);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2+extra, 4+extra);

		if (!extra)
			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
		}

		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		tmp[nbr_cells] = *c;
		share_cell(c);
		if (is_callable(c)) { tmp[nbr_cells].arity = 0; tmp[nbr_cells].nbr_cells = 1; CLR_OP(tmp+nbr_cells); }
		nbr_cells++;
		make_int(tmp+nbr_cells++, c->arity);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "instantiation_error")) {
		//printf("error(%s,(%s)/%u).\n", err_type, functor, goal->arity);
		tmp = alloc_on_heap(q, 5);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 4);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, err_type));
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "existence_error") && !strcmp(expected, "procedure") && is_callable(c)) {
		//printf("error(%s(%s,(%s)/%u),(%s)/%u).\n", err_type, expected, tmpbuf, c->arity, functor, goal->arity);
		tmp = alloc_on_heap(q, 9);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 8);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2, 4);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, C_STR(q, c)));
		make_int(tmp+nbr_cells++, c->arity);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else if (!strcmp(err_type, "representation_error")
		|| !strcmp(err_type, "evaluation_error")
		|| !strcmp(err_type, "syntax_error")
		|| !strcmp(err_type, "resource_error")) {
		//printf("error(%s(%s),(%s)/%u).\n", err_type, expected, functor, goal->arity);
		tmp = alloc_on_heap(q, 6);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 5);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 1, 1);
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	} else {
		//printf("error(%s(%s,(%s)),(%s)/%u).\n", err_type, expected, C_STR(q, c), functor, goal->arity);
		tmp = alloc_on_heap(q, 7+(c->nbr_cells-1)+extra);
		check_heap_error(tmp);
		pl_idx nbr_cells = 0;
		make_struct(tmp+nbr_cells++, g_error_s, NULL, 2, 6+(c->nbr_cells-1)+extra);
		make_struct(tmp+nbr_cells++, index_from_pool(q->pl, err_type), NULL, 2+extra, 2+(c->nbr_cells-1)+extra);

		if (!extra) {
			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, expected));
		} else {
			char tmpbuf[1024*8];
			strcpy(tmpbuf, expected);
			const char *ptr = tmpbuf;
			char *ptr2 = strchr(ptr, ',');
			if (*ptr2) *ptr2++ = '\0';

			while (ptr2) {
				make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
				ptr = ptr2;
				ptr2 = strchr(ptr, ',');
			}

			make_atom(tmp+nbr_cells++, index_from_pool(q->pl, ptr));
		}

		nbr_cells += safe_copy_cells(tmp+nbr_cells, c, c->nbr_cells);
		make_struct(tmp+nbr_cells, g_slash_s, NULL, 2, 2);
		SET_OP(tmp+nbr_cells, OP_YFX); nbr_cells++;
		make_atom(tmp+nbr_cells++, index_from_pool(q->pl, functor));
		make_int(tmp+nbr_cells, !is_string(goal)?goal->arity:0);
	}

	q->parens = q->numbervars = true;
	q->quoted = true;
	char *ball = print_term_to_strbuf(q, tmp, c_ctx, 1);
	clear_write_options(q);

	if (find_exception_handler(q, ball)) {
		free(ball);
		return fn_iso_catch_3(q);
	}

	free(ball);
	return false;
}

bool throw_error2(query *q, cell *c, pl_idx c_ctx, const char *err_type, const char *expected, cell *goal)
{
	cell tmp;
	tmp = goal[1];
	tmp.arity = get_smallint(&goal[2]);
	return throw_error3(q, c, c_ctx, err_type, expected, &tmp);
}

bool throw_error(query *q, cell *c, pl_idx c_ctx, const char *err_type, const char *expected)
{
	if ((q->st.m->flags.syntax_error == UNK_FAIL) && !strcmp(err_type, "syntax_error"))
		return false;

	return throw_error3(q, c, c_ctx, err_type, expected, q->st.curr_cell);
}

