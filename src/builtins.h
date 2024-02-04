#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "internal.h"

#define MAX_FFI 1000

void clr_accum(cell *p);

#if USE_FFI
bool wrap_ffi_function(query *q, builtins *bif_ptr);
bool wrap_ffi_predicate(query *q, builtins *bif_ptr);
#endif

#define is_callable_or_var(c) (is_interned(c) || is_cstring(c) || is_var(c))
#define is_list_or_nil(c) (is_list(c) || is_nil(c))
#define is_list_or_atom_or_var(c) (is_list(c) || is_atom(c) || is_var(c))
#define is_list_or_nil_or_var(c) (is_list_or_nil(c) || is_var(c))
#define is_list_or_var(c) (is_list(c) || is_var(c))
#define is_nil_or_var(c) (is_nil(c) || is_var(c))
#define is_compound_or_var(c) (is_compound(c) || is_var(c))
#define is_atomic_or_var(c) (is_atomic(c) || is_var(c))
#define is_atom_or_var(c) (is_atom(c) || is_var(c))
#define is_atom_or_int(c) (is_atom(c) || is_integer(c))
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
#define is_any(c) true

#define is_iso_list_or_nil(c) (is_iso_list(c) || is_nil(c))
#define is_iso_list_or_nil_or_var(c) (is_iso_list_or_nil(c) || is_var(c))
#define is_iso_list_or_var(c) (is_iso_list(c) || is_var(c))
#define is_iso_atom_or_var(c) (is_iso_atom(c) || is_var(c))
#define is_iso_atomic_or_var(c) (is_iso_atom(c) || is_number(c) || is_var(c))

#if USE_FFI
bool bif_sys_dlopen_3(query *q);
bool bif_sys_dlsym_3(query *q);
bool bif_sys_dlclose_1(query *q);
bool bif_sys_ffi_register_function_4(query *q);
bool bif_sys_ffi_register_predicate_4(query *q);
#endif

bool bif_iso_add_2(query *q);
bool bif_iso_float_1(query *q);
bool bif_iso_integer_1(query *q);

bool do_yield(query *q, int msecs);
void do_yield_at(query *q, unsigned int time_in_ms);

inline static void make_indirect(cell *tmp, cell *v, pl_idx v_ctx)
{
	tmp->tag = TAG_INDIRECT;
	tmp->nbr_cells = 1;
	tmp->arity = 0;
	tmp->flags = 0;
	tmp->val_ptr = v;
	tmp->var_ctx = v_ctx;
}

inline static void init_queuen(query *q)
{
	free(q->queue[q->st.qnbr]);
	q->queue[q->st.qnbr] = NULL;
	q->qp[q->st.qnbr] = 0;
	q->qcnt[q->st.qnbr] = 0;
}

inline static void grab_queuen(query *q)
{
	q->st.qnbr++;
	init_queuen(q);
}

inline static void drop_queuen(query *q)
{
	init_queuen(q);
	q->st.qnbr--;
}

inline static pl_idx queuen_used(const query *q)
{
	return q->qp[q->st.qnbr];
}

inline static cell *get_queuen(query *q)
{
	return q->queue[q->st.qnbr];
}

inline static cell *take_queuen(query *q)
{
	cell *save = q->queue[q->st.qnbr];
	q->queue[q->st.qnbr] = NULL;
	q->qp[q->st.qnbr] = 0;
	return save;
}

inline static cell *get_var(query *q, cell *c, pl_idx c_ctx)
{
	if (is_ref(c))
		c_ctx = c->var_ctx;

	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);

	while (is_var(&e->c)) {
		c_ctx = e->c.var_ctx;
		c = &e->c;
		f = GET_FRAME(c_ctx);
		e = GET_SLOT(f, c->var_nbr);
	}

	if (is_indirect(&e->c)) {
		q->latest_ctx = e->c.var_ctx;
		return e->c.val_ptr;
	}

	q->latest_ctx = c_ctx;

	if (!is_empty(&e->c))
		return &e->c;

	return c;
}

#define deref(q,c,c_ctx) \
	is_indirect(c) ? (q->latest_ctx = (c)->var_ctx, (c)->val_ptr) : \
	!is_var(c) ? (q->latest_ctx = (c_ctx), (c)) : \
	get_var(q, c, c_ctx)

#define GET_RAW_ARG(n,p) \
	cell *p = get_raw_arg(q,n); \
	pl_idx p##_ctx = q->latest_ctx

#define GET_FIRST_ARG(p,vt) \
	cell *p = get_first_arg(q); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->in_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_ARG0(p,vt,p0) \
	cell *p = get_first_arg0(q,p0); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->in_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_RAW_ARG(p,vt) \
	cell *p = get_first_raw_arg(q); \
	pl_idx p##_ctx = q->st.curr_frame; \
	if (!is_##vt(p)) { return q->in_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_RAW_ARG0(p,vt,p0) \
	cell *p = get_first_raw_arg0(q,p0); \
	pl_idx p##_ctx = q->st.curr_frame; \
	if (!is_##vt(p)) { return q->in_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_NEXT_ARG(p,vt) \
	cell *p = get_next_arg(q); \
	pl_idx p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return q->in_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_NEXT_RAW_ARG(p,vt) \
	cell *p = get_next_raw_arg(q); \
	pl_idx p##_ctx = q->st.curr_frame; \
	if (!is_##vt(p)) { return q->in_throw ? false : throw_error(q, p, p##_ctx, "type_error", #vt); }

inline static cell *get_first_arg(query *q)
{
	q->last_arg = q->st.curr_instr + 1;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_first_arg0(query *q, cell *p0)
{
	q->last_arg = p0 + 1;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_first_raw_arg(query *q)
{
	q->last_arg = q->st.curr_instr + 1;
	return q->last_arg;
}

inline static cell *get_first_raw_arg0(query *q, cell *p0)
{
	q->last_arg = p0 + 1;
	return q->last_arg;
}

inline static cell *get_next_arg(query *q)
{
	q->last_arg += q->last_arg->nbr_cells;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_next_raw_arg(query *q)
{
	q->last_arg += q->last_arg->nbr_cells;
	return q->last_arg;
}

inline static cell *get_raw_arg(const query *q, int n)
{
	cell *c = q->st.curr_instr + 1;

	for (int i = 1; i < n; i++)
		c += c->nbr_cells;

	return c;
}

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

#define check_heap_error(expr, ...) \
	CHECK_SENTINEL(expr, 0, __VA_ARGS__; \
	return throw_error(q, q->st.curr_instr, q->st.curr_frame, "resource_error", "memory"))

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
		ee = GET_SLOT(f, cc->var_nbr);								\
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
		ee = GET_SLOT(f, cc->var_nbr);								\
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
		slot *e = GET_SLOT(f, cc->var_nbr);							\
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
		slot *e = GET_SLOT(f, cc->var_nbr);							\
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
		slot *e = GET_SLOT(f, cc->var_nbr);							\
		if (e->vgen == qvgen) any = true;							\
		e->vgen = 0;												\
		p = deref(q, cc, cc_ctx);									\
		p_ctx = q->latest_ctx;										\
	}


inline static bool START_FUNCTION(query *q)
{
	extern bool throw_error(query *q, cell *c, pl_idx c_ctx, const char *err_type, const char *expected);
	errno = 0;

	if (q->eval)
		return true;

	if (!q->st.m->flags.unknown)
		return throw_error(q, q->st.curr_instr, q->st.curr_frame, "existence_error", "procedure");

	return false;
}

