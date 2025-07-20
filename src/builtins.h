#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "internal.h"
#include "cdebug.h"
#include "heap.h"

#define MAX_FFI 1000

#if USE_FFI
bool wrap_ffi_function(query *q, builtins *bif_ptr);
bool wrap_ffi_predicate(query *q, builtins *bif_ptr);
#endif

#define is_callable_or_var(c) (is_callable(c) || is_var(c))
#define is_list_or_nil(c) (is_list(c) || is_nil(c))
#define is_list_or_atom_or_var(c) (is_list(c) || is_atom(c) || is_var(c))
#define is_list_or_nil_or_var(c) (is_list_or_nil(c) || is_var(c))
#define is_list_or_var(c) (is_list(c) || is_var(c))
#define is_nil_or_var(c) (is_nil(c) || is_var(c))
#define is_compound_or_atom(c) (is_compound(c) || is_atom(c))
#define is_compound_or_var(c) (is_compound(c) || is_var(c))
#define is_atomic_or_var(c) (is_atomic(c) || is_var(c))
#define is_atom_or_var(c) (is_atom(c) || is_var(c))
#define is_atom_or_integer(c) (is_atom(c) || is_integer(c))
#define is_atom_or_list_or_nil(c) (is_atom(c) || is_list_or_nil(c))
#define is_atom_or_compound(c) (is_atom(c) || is_compound(c))
#define is_number_or_var(c) (is_number(c) || is_var(c))
#define is_float_or_var(c) (is_float(c) || is_var(c))
#define is_integer_or_var(c) (is_integer(c) || is_var(c))
#define is_integer_or_atom(c) (is_integer(c) || is_atom(c))
#define is_smallint_or_var(c) (is_smallint(c) || is_var(c))
#define is_smallint_or_atom(c) (is_smallint(c) || is_atom(c))
#define is_stream(c) (get_stream(q,c) >= 0)
#define is_stream_or_var(c) (is_stream(c) || is_var(c))
#define is_stream_or_compound(c) (is_stream(c) || is_compound(c))
#define is_list_or_atom(c) (is_atom(c) || is_iso_list(c))
#define is_atom_or_list(c) (is_atom(c) || is_iso_list(c))
#define is_atom_or_list_or_var(c) (is_atom(c) || is_iso_list(c) || is_var(c))
#define is_character(c) (is_iso_atom(c) && ((strlen_utf8(C_STR(q, c)) <= 1) || !CMP_STRING_TO_CSTR(q, c, "end_of_file")))
#define is_character_or_var(c) (is_in_character(c) || is_var(c))
#define is_in_character(c) (is_atom(c) && ((strlen_utf8(C_STR(q, c)) <= 1) || !CMP_STRING_TO_CSTR(q, c, "end_of_file")))
#define is_in_character_or_var(c) (is_in_character(c) || is_var(c))
#define is_in_byte(c) (is_integer(c) && (get_smallint(c) >= -1) && (get_smallint(c) < 256))
#define is_in_byte_or_var(c) (is_in_byte(c) || is_var(c))
#define is_byte(c) (is_integer(c) && (get_smallint(c) >= 0) && (get_smallint(c) < 256))
#define is_chars(q,c,ctx) (is_nil(c) || is_string(c) || scan_is_chars_list(q, c, ctx, false) || (is_cstring(c) && !CMP_STRING_TO_CSTR(q, c, "[]")))
#define is_sregex(c) (is_blob(c) && ((c)->flags & FLAG_BLOB_SREGEX))
#define is_string_or_var(c) (is_string(c) || is_var(c))
#define is_source_sink(c) (is_atom(c) || is_compound(c))
#define is_any(c) true

#define is_iso_list_or_nil(c) (is_iso_list(c) || is_nil(c))
#define is_iso_list_or_nil_or_var(c) (is_iso_list_or_nil(c) || is_var(c))
#define is_iso_list_or_var(c) (is_iso_list(c) || is_var(c))
#define is_iso_atom_or_var(c) (is_iso_atom(c) || is_var(c))
#define is_iso_atomic_or_var(c) (is_iso_atom(c) || is_number(c) || is_var(c))

#define GET_SOURCE_SINK(p1, p1_ctx, filename) { \
	if (is_iso_list(p1)) { \
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true); \
		if (!len) \
			return throw_error(q, p1, p1_ctx, "type_error", "source_sink"); \
		filename = chars_list_to_string(q, p1, p1_ctx); \
	} else \
		filename = DUP_STRING(q, p1); \
}

bool call_builtin(query *q, cell *c, pl_idx c_ctx);
bool call_userfun(query *q, cell *c, pl_idx c_ctx);

#define eval(q,c)														\
	is_evaluable(c) || is_builtin(c) ? (call_builtin(q,c,c##_ctx), q->accum) : \
	is_callable(c) ? (call_userfun(q, c, c##_ctx), q->accum) : *c;		\
	q->accum.flags = 0;													\
	if (q->did_throw)													\
		return true; 												\
	if (is_var(c))													\
		return throw_error(q, c, q->st.curr_frame, "instantiation_error", "number"); \
	if (is_builtin(c) && c->bif_ptr && (c->bif_ptr->fn != bif_iso_float_1) && (c->bif_ptr->fn != bif_iso_integer_1)) \
		return throw_error(q, c, q->st.curr_frame, "type_error", "evaluable");

bool bif_iso_add_2(query *q);
bool bif_iso_float_1(query *q);
bool bif_iso_integer_1(query *q);

bool do_yield(query *q, int msecs);
bool do_yield_then(query *q, bool status);
void do_yield_at(query *q, unsigned int time_in_ms);

inline static void init_queuen(query *q)
{
	free(q->queue[q->st.qnum]);
	q->queue[q->st.qnum] = NULL;
	q->qp[q->st.qnum] = 0;
	q->qcnt[q->st.qnum] = 0;
}

inline static void grab_queuen(query *q)
{
	q->st.qnum++;
	init_queuen(q);
}

inline static void drop_queuen(query *q)
{
	init_queuen(q);
	q->st.qnum--;
}

inline static pl_idx queuen_used(const query *q)
{
	return q->qp[q->st.qnum];
}

inline static cell *get_queuen(query *q)
{
	return q->queue[q->st.qnum];
}

inline static cell *take_queuen(query *q)
{
	cell *save = q->queue[q->st.qnum];
	q->queue[q->st.qnum] = NULL;
	q->qp[q->st.qnum] = 0;
	return save;
}

#define GET_CHOICE(i) (q->choices+(i))
#define GET_CURR_CHOICE() GET_CHOICE(q->cp-1)
#define GET_PREV_CHOICE() GET_CHOICE(q->cp-2)

#define GET_FRAME(i) (q->frames+(i))
#define GET_CURR_FRAME() GET_FRAME(q->st.curr_frame)
#define GET_NEW_FRAME() GET_FRAME(q->st.fp)

#define GET_SLOT(f,var_num) get_slot(q, f, var_num)

inline static slot *get_slot(const query *q, const frame *f, unsigned var_num)
{
	if (var_num < f->initial_slots)
		return q->slots + f->base + var_num;
	else
		return q->slots + f->op + (var_num - f->initial_slots);
}

inline static cell *deref(query *q, cell *c, pl_idx c_ctx)
{
	if (!is_var(c)) {
		if (is_indirect(c)) {
			q->latest_ctx = c->var_ctx;
			return c->val_ptr;
		}

		q->latest_ctx = c_ctx;
		return c;
	}

	if (is_ref(c))
		c_ctx = c->var_ctx;

	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_num);

	while (is_var(&e->c)) {
		c_ctx = e->c.var_ctx;
		c = &e->c;

		if (is_ref(c))
			c_ctx = c->var_ctx;

		f = GET_FRAME(c_ctx);
		e = GET_SLOT(f, c->var_num);
	}

	if (is_indirect(&e->c)) {
		q->latest_ctx = e->c.var_ctx;
		return e->c.val_ptr;
	}

	q->latest_ctx = c_ctx;

	if (is_empty(&e->c))
		return c;

	return &e->c;
}

#define FIRST_ARG(c) ((c)+1)
#define NEXT_ARG(c) ((c)+(c)->num_cells)

#define GET_RAW_ARG(n,p) \
	cell *p = get_raw_arg(q,n); \
	pl_idx p##_ctx = q->latest_ctx

#define GET_FIRST_ARG(p,vt) \
	cell *p = get_first_arg(q); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->did_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_ARG0(p,vt,p0) \
	cell *p = get_first_arg0(q,p0); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->did_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_RAW_ARG(p,vt) \
	cell *p = get_first_raw_arg(q); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->did_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_RAW_ARG0(p,vt,p0) \
	cell *p = get_first_raw_arg0(q,p0); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->did_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_NEXT_ARG(p,vt) \
	cell *p = get_next_arg(q); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->did_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_NEXT_RAW_ARG(p,vt) \
	cell *p = get_next_raw_arg(q); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->did_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

inline static cell *get_first_arg(query *q)
{
	q->last_arg = q->st.instr + 1;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_first_arg0(query *q, cell *p0)
{
	q->last_arg = p0 + 1;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_first_raw_arg(query *q)
{
	q->last_arg = q->st.instr + 1;
	q->latest_ctx = q->st.curr_frame;
	return q->last_arg;
}

inline static cell *get_first_raw_arg0(query *q, cell *p0)
{
	q->last_arg = p0 + 1;
	q->latest_ctx = q->st.curr_frame;
	return q->last_arg;
}

inline static cell *get_next_arg(query *q)
{
	q->last_arg += q->last_arg->num_cells;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_next_raw_arg(query *q)
{
	q->last_arg += q->last_arg->num_cells;
	q->latest_ctx = q->st.curr_frame;
	return q->last_arg;
}

inline static cell *get_raw_arg(query *q, int n)
{
	cell *c = q->st.instr + 1;

	for (int i = 1; i < n; i++)
		c += c->num_cells;

	q->latest_ctx = q->st.curr_frame;
	return c;
}

#define check_memory(expr, ...) \
	CHECK_SENTINEL(expr, 0, __VA_ARGS__; \
	return throw_error(q, q->st.instr, q->st.curr_frame, "resource_error", "memory"))

// This one leaves original state if a cycle detected...

#define DEREF_CHECKED(any, both, svg, ee, evgen, cc, cc_ctx, qvgen)	\
	if (is_var(cc)) {												\
		pl_idx tmp_cc_ctx = cc_ctx;									\
		any = true;													\
																	\
		if (is_ref(cc))												\
			tmp_cc_ctx = cc->var_ctx;								\
																	\
		const frame *f = GET_FRAME(tmp_cc_ctx);						\
		ee = GET_SLOT(f, cc->var_num);								\
		svg = evgen;												\
																	\
		if (evgen == qvgen) {										\
			both++;													\
		} else {													\
			cc = deref(q, cc, tmp_cc_ctx);							\
			cc_ctx = q->latest_ctx;									\
			evgen = qvgen;											\
		}															\
	}

// This one always derefs...

#define DEREF_VAR(any, both, svg, ee, evgen, cc, cc_ctx, qvgen)		\
	if (is_var(cc)) {												\
		pl_idx tmp_cc_ctx = cc_ctx;									\
		any = true;													\
																	\
		if (is_ref(cc))												\
			tmp_cc_ctx = cc->var_ctx;								\
																	\
		const frame *f = GET_FRAME(tmp_cc_ctx);						\
		ee = GET_SLOT(f, cc->var_num);								\
		svg = evgen;												\
																	\
		if (evgen == qvgen) {										\
			both++;													\
		} else {													\
			evgen = qvgen;											\
		}															\
																	\
		cc = deref(q, cc, tmp_cc_ctx);								\
		cc_ctx = q->latest_ctx;										\
	}

#define RESTORE_VAR(cc, cc_ctx, p, p_ctx, qvgen)					\
	if (is_var(cc)) {												\
		if (is_ref(cc))												\
			cc_ctx = cc->var_ctx;									\
																	\
		const frame *f = GET_FRAME(cc_ctx);							\
		slot *e = GET_SLOT(f, cc->var_num);							\
		e->vgen = 0;												\
		p = deref(q, cc, cc_ctx);									\
		p_ctx = q->latest_ctx;										\
	}

#define RESTORE_VAR2(cc, cc_ctx, p, p_ctx, qvgen)					\
	if (is_var(cc)) {												\
		if (is_ref(cc))												\
			cc_ctx = cc->var_ctx;									\
																	\
		const frame *f = GET_FRAME(cc_ctx);							\
		slot *e = GET_SLOT(f, cc->var_num);							\
		e->vgen2 = 0;												\
		p = deref(q, cc, cc_ctx);									\
		p_ctx = q->latest_ctx;										\
	}

#define RESTORE_VAR_CHECKED(any, cc, cc_ctx, p, p_ctx, qvgen)		\
	if (is_var(cc)) {												\
		if (is_ref(cc))												\
			cc_ctx = cc->var_ctx;									\
																	\
		const frame *f = GET_FRAME(cc_ctx);							\
		slot *e = GET_SLOT(f, cc->var_num);							\
		if (e->vgen == qvgen) any = true;							\
		e->vgen = 0;												\
		p = deref(q, cc, cc_ctx);									\
		p_ctx = q->latest_ctx;										\
	}


#define START_FUNCTION(q) { \
	errno = 0; \
	if (!q->eval) { \
		if (!q->st.m->flags.unknown) \
			return throw_error(q, q->st.instr, q->st.curr_frame, "existence_error", "procedure"); \
		else \
			return false; \
	} \
}


bool bif_iso_halt_0(query *q);
bool bif_iso_close_1(query *q);
bool bif_iso_true_0(query *q);
bool bif_iso_fail_0(query *q);
bool bif_iso_call_1(query *q);
bool bif_iso_conjunction_2(query *q);
bool bif_iso_qualify_2(query *q);
bool bif_iso_cut_0(query *q);
bool bif_iso_unify_2(query *q);
bool bif_iso_if_then_2(query *q);
bool bif_soft_if_then_2(query *q);
bool bif_sys_call_check_1(query *q);
bool bif_sys_succeed_on_retry_1(query *q);
bool bif_sys_succeed_on_retry_2(query *q);
bool bif_sys_fail_on_retry_1(query *q);
bool bif_sys_drop_barrier_1(query *q);
bool bif_sys_reset_handler_1(query *q);
bool bif_sys_call_cleanup_3(query *q);
bool bif_sys_queue_1(query *q);
bool bif_sys_get_level_1(query *q);
bool bif_sys_set_if_var_2(query *q);
bool bif_sys_jump_1(query *q);
bool bif_sys_jump_if_nil_2(query *q);
bool bif_sys_cut_1(query *q);
bool bif_parse_csv_file_2(query *q);
bool bif_parse_csv_line_3(query *q);
bool bif_parse_csv_line_2(query *q);
bool bif_sre_compile_2(query *q);
bool bif_sre_matchp_4(query *q);
bool bif_sre_match_4(query *q);
bool bif_sre_substp_4(query *q);
bool bif_sre_subst_4(query *q);
bool bif_call_0(query *q, cell *p1, pl_idx p1_ctx);
bool bif_statistics_0(query *q);
bool bif_sys_module_1(query *q);
bool bif_sys_undo_1(query *q);
bool bif_sys_create_var_1(query *q);
bool bif_sys_match_1(query *q);
