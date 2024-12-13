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

	if (((*src)->val_off == g_once_s) && ((*src)->arity == 1)) {
		unsigned var_nbr = cl->nbr_vars++;
		*src += 1;
		cell *save_dst = *dst;
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

	if (((*src)->val_off == g_ignore_s) && ((*src)->arity == 1)) {
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

#if 0
	if (((*src)->val_off == g_negation_s) && ((*src)->arity == 1)) {
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
		return;
	}
#endif

#if 0
	if (((*src)->val_off == g_call_s) && ((*src)->arity == 1)) {
		unsigned var_nbr = cl->nbr_vars++;
		*src += 1;
		cell *save_dst = *dst;
		make_instr((*dst)++, g_sys_fail_on_retry_s, bif_sys_fail_on_retry_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		make_instr((*dst)++, g_sys_call_check_s, bif_sys_call_check_1, 1, (*src)->nbr_cells);
		*dst += copy_cells(*dst, *src, (*src)->nbr_cells);
		compile_term(cl, dst, src);
		make_instr((*dst)++, g_sys_drop_barrier_s, bif_sys_drop_barrier_1, 1, 1);
		make_var((*dst)++, g_anon_s, var_nbr);
		return;
	}
#endif

	pl_idx n = copy_cells(*dst, *src, (*src)->nbr_cells);
	*dst += n;
	*src += n;
}

void compile_clause(clause *cl, cell *body)
{
	pl_idx nbr_cells = cl->cidx - (body - cl->cells);
	cl->alt = malloc(sizeof(cell) * nbr_cells*10);
	cell *dst = cl->alt, *src = body;
	compile_term(cl, &dst, &src);
	assert(src->tag == TAG_END);
	copy_cells(dst, src, 1);
}
