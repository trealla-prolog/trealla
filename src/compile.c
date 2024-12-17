#include <ctype.h>
#include <stdlib.h>

#include "parser.h"
#include "prolog.h"
#include "query.h"

static void compile_term(clause *cl, cell **dst, cell **src)
{
	if (((*src)->val_off == g_conjunction_s) && ((*src)->arity == 2)) {
		*src += 1;
		compile_term(cl, dst, src);		// LHS
		compile_term(cl, dst, src);		// RHS
		return;
	}

#if 0
	cell *c = (*src) + 1;

	if (((*src)->val_off == g_disjunction_s) && ((*src)->arity == 2)
		&& is_callable(c) && c->bif_ptr
		&& (c->bif_ptr->fn == bif_iso_if_then_2)) {
#if 0
		cell *p1 = c + 1;
		cell *p2 = p1 + p1->nbr_cells;
		cell *p3 = p2 + p2->nbr_cells;
		return do_if_then_else(q, p1, p2, p3);
#else
		pl_idx n = copy_cells(*dst, *src, (*src)->nbr_cells);
		*dst += n;
		*src += n;
		return;
#endif
	}

	if (((*src)->val_off == g_disjunction_s) && ((*src)->arity == 2)
		&& is_callable(c) && c->bif_ptr
		&& (c->bif_ptr->fn == bif_if_2)) {
#if 0
		cell *p1 = c + 1;
		cell *p2 = p1 + p1->nbr_cells;
		cell *p3 = p2 + p2->nbr_cells;
		return soft_do_if_then_else(q, p1, p2, p3);
#else
		pl_idx n = copy_cells(*dst, *src, (*src)->nbr_cells);
		*dst += n;
		*src += n;
		return;
#endif
	}
#endif

#if 0
	if (((*src)->val_off == g_disjunction_s) && ((*src)->arity == 2)) {
		*src += 1;
		cell *save_dst1 = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_1, 1, 1);
		make_uint((*dst)++, 0);										// Dummy value
		compile_term(cl, dst, src);		// LHS
		cell *save_dst2 = *dst;
		make_instr((*dst)++, g_sys_jump_s, bif_sys_jump_1, 1, 1);
		make_uint((*dst)++, 0);										// Dummy value
		cell *save_dst3 = *dst;
		make_uint(save_dst1+1, *dst - save_dst1);					// Real value
		compile_term(cl, dst, src);		// RHS
		make_uint(save_dst2+1, *dst - save_dst3);					// Real value
		return;
	}
#endif

	if (((*src)->val_off == g_once_s) && ((*src)->arity == 1) && !is_var((*src)+1)) {
		unsigned var_nbr = cl->nbr_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		make_instr((*dst)++, g_sys_call_check_s, bif_sys_call_check_1, 1, (*src)->nbr_cells);
		*dst += copy_cells(*dst, *src, (*src)->nbr_cells);
		compile_term(cl, dst, src);
		make_instr((*dst)++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		return;
	}

	if (((*src)->val_off == g_ignore_s) && ((*src)->arity == 1) && !is_var((*src)+1)) {
		unsigned var_nbr = cl->nbr_vars++;
		*src += 1;
		cell *save_dst = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_nbr);
		make_uint((*dst)++, 0);										// Dummy value
		make_instr((*dst)++, g_sys_call_check_s, bif_sys_call_check_1, 1, (*src)->nbr_cells);
		*dst += copy_cells(*dst, *src, (*src)->nbr_cells);
		compile_term(cl, dst, src);
		make_instr((*dst)++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		make_uint(save_dst+2, *dst - save_dst);						// Real value
		return;
	}

	if (((*src)->val_off == g_negation_s) && ((*src)->arity == 1) && !is_var((*src)+1)) {
		unsigned var_nbr = cl->nbr_vars++;
		*src += 1;
		cell *save_dst = *dst;
		make_instr((*dst)++, g_sys_succeed_on_retry_s, bif_sys_succeed_on_retry_2, 2, 2);
		make_var((*dst)++, g_anon_s, var_nbr);
		make_uint((*dst)++, 0);										// Dummy value
		compile_term(cl, dst, src);
		make_instr((*dst)++, g_cut_s, bif_iso_cut_0, 0, 0);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		make_instr((*dst)++, g_fail_s, bif_iso_fail_0, 0, 0);
		make_uint(save_dst+2, *dst - save_dst);						// Real value
		make_instr((*dst)++, g_true_s, bif_iso_true_0, 0, 0);		// Why????
		return;
	}

	if (((*src)->val_off == g_call_s) && ((*src)->arity == 1) && !is_var((*src)+1)) {
		unsigned var_nbr = cl->nbr_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		make_instr((*dst)++, g_sys_call_check_s, bif_sys_call_check_1, 1, (*src)->nbr_cells);
		*dst += copy_cells(*dst, *src, (*src)->nbr_cells);
		compile_term(cl, dst, src);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		return;
	}

	if (((*src)->val_off == g_reset_s) && ((*src)->arity == 3) && !is_var((*src)+1)) {
		unsigned var_nbr = cl->nbr_vars++;
		*src += 1;
		make_instr((*dst)++, g_sys_get_level_s, bif_sys_get_level_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		compile_term(cl, dst, src);		// arg1
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		*src += (*src)->nbr_cells;		// arg2
		make_instr((*dst)++, g_sys_set_if_var_s, bif_sys_set_if_var_2, 2, (*src)->nbr_cells+1);
		compile_term(cl, dst, src);		// arg3
		make_atom((*dst)++, g_none_s);
		return;
	}

	pl_idx n = copy_cells(*dst, *src, (*src)->nbr_cells);
	*dst += n;
	*src += n;
}

void compile_clause(clause *cl, cell *body)
{
	pl_idx nbr_cells = cl->cidx - (body - cl->cells);
	cl->alt = malloc(sizeof(cell) * nbr_cells*100);
	cell *dst = cl->alt, *src = body;
	compile_term(cl, &dst, &src);
	assert(src->tag == TAG_END);
	copy_cells(dst, src, 1);
}
