#include <ctype.h>
#include <stdlib.h>

#include "module.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

static void copy_term(cell **dst, cell **src)
{
	unsigned n = copy_cells(*dst, *src, (*src)->num_cells);
	*dst += n;
	*src += n;
}

static void compile_term(predicate *pr, clause *cl, cell **dst, cell **src)
{
	if (((*src)->val_off == g_conjunction_s) && ((*src)->arity == 2)) {
		*src += 1;
		compile_term(pr, cl, dst, src);		// LHS
		compile_term(pr, cl, dst, src);		// RHS
		return;
	}

	cell *c = (*src) + 1;

	// T1 -> T2 ; T3

	if (((*src)->val_off == g_disjunction_s) && ((*src)->arity == 2)
		&& !is_var((*src)+1)
		&& is_callable(c) && c->bif_ptr && (c->arity == 2)
		&& (c->bif_ptr->fn == bif_iso_if_then_2)) {
		*src += 2;
		unsigned var_num = cl->num_vars++;
		cell *save_dst1 = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_num);
		make_uint((*dst)++, 0);										// Dummy value1
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		compile_term(pr, cl, dst, src);								// Arg2
		cell *save_dst2 = *dst;
		make_instr((*dst)++, g_sys_jump_s, bif_sys_jump_1, 1, 1);
		make_uint((*dst)++, 0);										// Dummy value2
		make_uint(save_dst1+2, *dst - save_dst1);					// Real value1
		compile_term(pr, cl, dst, src);								// Arg3
		make_uint(save_dst2+1, *dst - save_dst2);					// Real value2
		make_instr((*dst)++, g_true_s, bif_iso_true_0, 0, 0);		// Landing
		return;
	}

	// T1 *-> T2 ; T3

	if (((*src)->val_off == g_disjunction_s) && ((*src)->arity == 2)
		&& is_callable(c) && c->bif_ptr && (c->arity == 2)
		&& (c->bif_ptr->fn == bif_soft_if_then_2)) {
		*src += 2;
		unsigned var_num = cl->num_vars++;
		cell *save_dst1 = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_num);
		make_uint((*dst)++, 0);										// Dummy value1
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_sys_cut_s, bif_sys_cut_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		compile_term(pr, cl, dst, src);								// Arg2
		cell *save_dst2 = *dst;
		make_instr((*dst)++, g_sys_jump_s, bif_sys_jump_1, 1, 1);
		make_uint((*dst)++, 0);										// Dummy value2
		make_uint(save_dst1+2, *dst - save_dst1);					// Real value1
		compile_term(pr, cl, dst, src);								// Arg3
		make_uint(save_dst2+1, *dst - save_dst2);					// Real value2
		make_instr((*dst)++, g_true_s, bif_iso_true_0, 0, 0);		// Landing
		return;
	}

	// T1 ; T2

	if (((*src)->val_off == g_disjunction_s) && ((*src)->arity == 2)) {
		*src += 1;
		cell *save_dst1 = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_1, 1, 1);
		make_uint((*dst)++, 0);										// Dummy value1
		compile_term(pr, cl, dst, src);								// LHS
		cell *save_dst2 = *dst;
		make_instr((*dst)++, g_sys_jump_s, bif_sys_jump_1, 1, 1);
		make_uint((*dst)++, 0);										// Dummy value1
		make_uint(save_dst1+1, *dst - save_dst1);					// Real value1
		compile_term(pr, cl, dst, src);								// RHS
		make_uint(save_dst2+1, *dst - save_dst2);					// Real value2
		make_instr((*dst)++, g_true_s, bif_iso_true_0, 0, 0);		// Landing
		return;
	}

	// T1 -> T2

	if (((*src)->val_off == g_if_then_s) && ((*src)->arity == 2) && !is_var((*src)+1)) {
		unsigned var_num = cl->num_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		compile_term(pr, cl, dst, src);								// Arg2
		return;
	}

	// T1 *-> T2

	if (((*src)->val_off == g_soft_cut_s) && ((*src)->arity == 2) && !is_var((*src)+1)) {
		unsigned var_num = cl->num_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		compile_term(pr, cl, dst, src);								// Arg2
		return;
	}

	if (((*src)->val_off == g_if_s) && ((*src)->arity == 3) && !is_var((*src)+1)) {
		*src += 1;
		unsigned var_num = cl->num_vars++;
		cell *save_dst1 = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_num);
		make_uint((*dst)++, 0);										// Dummy value1
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		compile_term(pr, cl, dst, src);								// Arg2
		cell *save_dst2 = *dst;
		make_instr((*dst)++, g_sys_jump_s, bif_sys_jump_1, 1, 1);
		make_uint((*dst)++, 0);										// Dummy value2
		make_uint(save_dst1+2, *dst - save_dst1);					// Real value1
		compile_term(pr, cl, dst, src);								// Arg3
		make_uint(save_dst2+1, *dst - save_dst2);					// Real value2
		make_instr((*dst)++, g_true_s, bif_iso_true_0, 0, 0);		// Landing
		return;
	}

	if (((*src)->val_off == g_call_s) && ((*src)->arity == 1) && !is_var((*src)+1)) {
		unsigned var_num = cl->num_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		make_instr((*dst)++, g_sys_call_check_s, bif_sys_call_check_1, 1, (*src)->num_cells);
		*dst += copy_cells(*dst, *src, (*src)->num_cells);
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		return;
	}

	if (((*src)->val_off == g_once_s) && ((*src)->arity == 1) && !is_var((*src)+1)) {
		unsigned var_num = cl->num_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		make_instr((*dst)++, g_sys_call_check_s, bif_sys_call_check_1, 1, (*src)->num_cells);
		*dst += copy_cells(*dst, *src, (*src)->num_cells);
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		return;
	}

	if (((*src)->val_off == g_ignore_s) && ((*src)->arity == 1) && !is_var((*src)+1)) {
		unsigned var_num = cl->num_vars++;
		*src += 1;
		cell *save_dst = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_num);
		make_uint((*dst)++, 0);										// Dummy value
		make_instr((*dst)++, g_sys_call_check_s, bif_sys_call_check_1, 1, (*src)->num_cells);
		*dst += copy_cells(*dst, *src, (*src)->num_cells);
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		make_uint(save_dst+2, *dst - save_dst);						// Real value
		make_instr((*dst)++, g_true_s, bif_iso_true_0, 0, 0);		// Landing
		return;
	}

	if (((*src)->val_off == g_negation_s) && ((*src)->arity == 1) && !is_var((*src)+1)) {
		unsigned var_num = cl->num_vars++;
		*src += 1;
		cell *save_dst = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_num);
		make_uint((*dst)++, 0);										// Dummy value
		copy_term(dst, src);										// Not compile_term
		make_instr((*dst)++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		make_instr((*dst)++, g_fail_s, bif_iso_fail_0, 0, 0);
		make_uint(save_dst+2, *dst - save_dst);						// Real value
		make_instr((*dst)++, g_true_s, bif_iso_true_0, 0, 0);		// Landing
		return;
	}

	if (((*src)->val_off == g_reset_s) && ((*src)->arity == 3) && !is_var((*src)+1)) {
		unsigned var_num = cl->num_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		compile_term(pr, cl, dst, src);								// Arg1
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		*src += (*src)->num_cells;									// Arg2
		make_instr((*dst)++, g_sys_set_if_var_s, bif_sys_set_if_var_2, 2, (*src)->num_cells+1);
		compile_term(pr, cl, dst, src);								// Arg3
		make_atom((*dst)++, g_none_s);
		return;
	}

	if (((*src)->val_off == g_notunify_s) && ((*src)->arity == 2)) {
		unsigned var_num = cl->num_vars++;
		*src += 1;
		cell *save_dst = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_num);
		make_uint((*dst)++, 0);										// Dummy value
		cell *save_dst1 = *dst;
		make_instr((*dst), g_unify_s, bif_iso_unify_2, 2, 0);
		SET_OP(*dst, OP_XFX); (*dst)++;
		copy_term(dst, src);										// Arg1
		copy_term(dst, src);										// Arg2
		save_dst1->num_cells = *dst - save_dst1;					// Real value
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num);
		make_instr((*dst)++, g_fail_s, bif_iso_fail_0, 0, 0);
		make_uint(save_dst+2, *dst - save_dst);						// Real value
		make_instr((*dst)++, g_true_s, bif_iso_true_0, 0, 0);		// Landing
		return;
	}

	if (((*src)->val_off == g_colon_s) && ((*src)->arity == 2) && !is_var((*src)+1)
		) {
		unsigned var_num1 = cl->num_vars++;
		unsigned var_num2 = cl->num_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_module_s, bif_sys_module_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num1);
		make_instr((*dst)++, g_sys_module_s, bif_sys_module_1, 1, (*src)->num_cells);
		copy_term(dst, src);										// Arg1
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num2);
		make_instr((*dst)++, g_sys_call_check_s, bif_sys_call_check_1, 1, (*src)->num_cells);
		*dst += copy_cells(*dst, *src, (*src)->num_cells);			// Arg2
		copy_term(dst, src);										// Arg2
		make_instr((*dst)++, g_sys_module_s, bif_sys_module_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num1);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num2);
		return;
	}

	if (((*src)->val_off == g_maplist_s) && ((*src)->arity == 2) && !is_var((*src)+1)) {
		unsigned var_numL1 = cl->num_vars++;
		unsigned var_num1 = cl->num_vars++;
		unsigned var_numH1 = cl->num_vars++;
		unsigned var_numT1 = cl->num_vars++;
		*src += 1;
		cell *f = *src;
		cell *arg1 = f + f->num_cells;

		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num1);

		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 1+arg1->num_cells); // L1=L
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		*dst += copy_cells(*dst, arg1, arg1->num_cells);			// L

		cell *save_dst0 = *dst;
		make_instr((*dst)++, g_sys_loop_s, bif_iso_true_0, 0, 0);	// LOOP

		cell *save_dst3 = *dst;
		make_instr((*dst)++, g_sys_jump_if_nil_s, bif_sys_jump_if_nil_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		make_uint((*dst)++, 0);										// Dummy value

		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 1+3);		// L1=[H1|T1]
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		make_instr((*dst)++, g_dot_s, NULL, 2, 2);
		make_var((*dst)++, g_anon_s, var_numH1);					// H1
		make_var((*dst)++, g_anon_s, var_numT1);					// T1

		cell *save_dst = *dst;
		copy_term(dst, src);										// Functor
		save_dst->arity++;
		save_dst->num_cells++;
		make_var((*dst)++, g_anon_s, var_numH1);					// H1

		bool found;

		if ((save_dst->match = search_predicate(pr->m, save_dst, NULL)) != NULL)
			save_dst->flags &= ~FLAG_INTERNED_BUILTIN;
		 else if ((save_dst->bif_ptr = get_builtin_term(pr->m, save_dst, &found, NULL)), found)
			save_dst->flags |= FLAG_INTERNED_BUILTIN;
		else
			save_dst->flags &= ~FLAG_INTERNED_BUILTIN;

		*src += (*src)->num_cells;

		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 2);		// L1=T1
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		make_var((*dst)++, g_anon_s, var_numT1);					// T1
		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numH1);					// H1
		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numT1);					// T1

		make_instr((*dst)++, g_sys_jump_s, bif_sys_jump_1, 1, 1);
		make_int((*dst), -(ssize_t)((*dst)-save_dst0));				// jump to LOOP
		(*dst)++;

		make_uint(save_dst3+2, *dst - save_dst3);					// Real value
		make_instr((*dst)++, g_sys_end_s, bif_iso_true_0, 0, 0);	// Landing
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num1);
		return;
	}

#if 0
	if (((*src)->val_off == g_maplist_s) && ((*src)->arity == 3) && !is_var((*src)+1)) {
		unsigned var_numL1 = cl->num_vars++;
		unsigned var_numL2 = cl->num_vars++;
		unsigned var_num1 = cl->num_vars++;
		unsigned var_numH1 = cl->num_vars++;
		unsigned var_numH2 = cl->num_vars++;
		unsigned var_numT1 = cl->num_vars++;
		unsigned var_numT2 = cl->num_vars++;
		*src += 1;
		cell *f = *src;
		cell *arg1 = f + f->num_cells;
		cell *arg2 = arg1 + arg1->num_cells;

		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num1);

		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 1+arg1->num_cells); // L1=L
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		*dst += copy_cells(*dst, arg1, arg1->num_cells);			// L

		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 1+arg2->num_cells); // L2=L
		make_var((*dst)++, g_anon_s, var_numL2);					// L2
		*dst += copy_cells(*dst, arg2, arg2->num_cells);			// L

		cell *save_dst0 = *dst;
		make_instr((*dst)++, g_sys_loop_s, bif_iso_true_0, 0, 0);	// LOOP

		cell *save_dst31 = *dst;
		make_instr((*dst)++, g_sys_jump_if_nil_s, bif_sys_jump_if_nil_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		make_uint((*dst)++, 0);										// Dummy value

		cell *save_dst32 = *dst;
		make_instr((*dst)++, g_sys_jump_if_nil_s, bif_sys_jump_if_nil_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_numL2);					// L1
		make_uint((*dst)++, 0);										// Dummy value
\
		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 1+3);		// L1=[H1|T1]
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		make_instr((*dst)++, g_dot_s, NULL, 2, 2);
		make_var((*dst)++, g_anon_s, var_numH1);					// H1
		make_var((*dst)++, g_anon_s, var_numT1);					// T1

		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 1+3);		// L2=[H2|T2]
		make_var((*dst)++, g_anon_s, var_numL2);					// L2
		make_instr((*dst)++, g_dot_s, NULL, 2, 2);
		make_var((*dst)++, g_anon_s, var_numH2);					// H2
		make_var((*dst)++, g_anon_s, var_numT2);					// T2

		cell *save_dst = *dst;
		copy_term(dst, src);										// Functor
		save_dst->arity++;
		save_dst->num_cells++;
		make_var((*dst)++, g_anon_s, var_numH1);					// H1
		save_dst->arity++;
		save_dst->num_cells++;
		make_var((*dst)++, g_anon_s, var_numH2);					// H2

		bool found;

		if ((save_dst->match = search_predicate(pr->m, save_dst, NULL)) != NULL)
			save_dst->flags &= ~FLAG_INTERNED_BUILTIN;
		 else if ((save_dst->bif_ptr = get_builtin_term(pr->m, save_dst, &found, NULL)), found)
			save_dst->flags |= FLAG_INTERNED_BUILTIN;
		else
			save_dst->flags &= ~FLAG_INTERNED_BUILTIN;

		*src += (*src)->num_cells;
		*src += (*src)->num_cells;

		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 2);		// L1=T1
		make_var((*dst)++, g_anon_s, var_numL1);					// L1
		make_var((*dst)++, g_anon_s, var_numT1);					// T1
		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numH1);					// H1
		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numT1);					// T1

		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numL2);					// L2
		make_instr((*dst)++, g_eq_s, bif_iso_unify_2, 2, 2);		// L2=T2
		make_var((*dst)++, g_anon_s, var_numL2);					// L2
		make_var((*dst)++, g_anon_s, var_numT2);					// T2
		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numH2);					// H2
		make_instr((*dst)++, g_sys_undo_s, bif_sys_undo_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_numT2);					// T2

		make_instr((*dst)++, g_sys_jump_s, bif_sys_jump_1, 1, 1);
		make_int((*dst), -(ssize_t)((*dst)-save_dst0));				// jump to LOOP
		(*dst)++;

		make_uint(save_dst31+2, *dst - save_dst31);					// Real value
		make_uint(save_dst32+2, *dst - save_dst32);					// Real value
		make_instr((*dst)++, g_sys_end_s, bif_iso_true_0, 0, 0);	// Landing
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_num1);
		return;
	}
#endif

	copy_term(dst, src);
}

void compile_clause(predicate *pr, clause *cl, cell *body)
{
	pl_idx num_cells = cl->cidx - (body - cl->cells);
	cl->alt = malloc(sizeof(cell) * num_cells*100+1024);
	cell *dst = cl->alt, *src = body;
	compile_term(pr, cl, &dst, &src);
	assert(src->tag == TAG_END);
	dst += copy_cells(dst, src, 1);
	cl->alt = realloc(cl->alt, sizeof(cell)*((dst-cl->alt)));
}
