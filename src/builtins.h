#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "internal.h"

#define MAX_FFI 1000

#define CHECK_CALC()								\
	clr_accum(&q->accum);							\
	errno = 0;										\
													\
	if (!q->eval) {									\
		if (q->st.m->flags.unknown == 0)			\
			return false;							\
		return throw_error(q, q->st.curr_cell, 		\
			q->st.curr_frame, 						\
			"existence_error", "procedure");		\
	}

void clr_accum(cell *p);

#if USE_FFI
bool wrap_ffi_function(query *q, builtins *fn_ptr);
bool wrap_ffi_predicate(query *q, builtins *fn_ptr);
#endif

#define is_callable_or_var(c) (is_interned(c) || is_cstring(c) || is_var(c))
#define is_list_or_nil(c) (is_list(c) || is_nil(c))
#define is_list_or_atom_or_var(c) (is_list(c) || is_atom(c) || is_var(c))
#define is_list_or_nil_or_var(c) (is_list_or_nil(c) || is_var(c))
#define is_list_or_var(c) (is_list(c) || is_var(c))
#define is_structure_or_var(c) (is_structure(c) || is_var(c))
#define is_atomic_or_var(c) (is_atomic(c) || is_var(c))
#define is_atom_or_var(c) (is_atom(c) || is_var(c))
#define is_atom_or_int(c) (is_atom(c) || is_integer(c))
#define is_atom_or_list_or_nil(c) (is_atom(c) || is_list_or_nil(c))
#define is_atom_or_structure(c) (is_atom(c) || is_structure(c))
#define is_number_or_var(c) (is_number(c) || is_var(c))
#define is_float_or_var(c) (is_float(c) || is_var(c))
#define is_integer_or_var(c) (is_integer(c) || is_var(c))
#define is_integer_or_atom(c) (is_integer(c) || is_atom(c))
#define is_smallint_or_var(c) (is_smallint(c) || is_var(c))
#define is_smallint_or_atom(c) (is_smallint(c) || is_atom(c))
#define is_stream(c) (get_stream(q,c) >= 0)
#define is_stream_or_var(c) (is_stream(c) || is_var(c))
#define is_stream_or_structure(c) (is_stream(c) || is_structure(c))
#define is_list_or_atom(c) (is_atom(c) || is_iso_list(c))
#define is_atom_or_list(c) (is_atom(c) || is_iso_list(c))
#define is_atom_or_list_or_var(c) (is_atom(c) || is_iso_list(c) || is_var(c))
#define is_character(c) (is_iso_atom(c) && ((strlen_utf8(C_STR(q, c)) <= 1) || !CMP_STR_TO_CSTR(q, c, "end_of_file")))
#define is_character_or_var(c) (is_in_character(c) || is_var(c))
#define is_in_character(c) (is_atom(c) && ((strlen_utf8(C_STR(q, c)) <= 1) || !CMP_STR_TO_CSTR(q, c, "end_of_file")))
#define is_in_character_or_var(c) (is_in_character(c) || is_var(c))
#define is_in_byte(c) (is_integer(c) && (get_smallint(c) >= -1) && (get_smallint(c) < 256))
#define is_in_byte_or_var(c) (is_in_byte(c) || is_var(c))
#define is_byte(c) (is_integer(c) && (get_smallint(c) >= 0) && (get_smallint(c) < 256))
#define is_chars(q,c,ctx) (is_nil(c) || is_string(c) || scan_is_chars_list(q, c, ctx, false) || (is_cstring(c) && !CMP_STR_TO_CSTR(q, c, "[]")))
#define is_sregex(c) (is_blob(c) && ((c)->flags & FLAG_BLOB_SRE))
#define is_any(c) true

#define is_iso_list_or_nil(c) (is_iso_list(c) || is_nil(c))
#define is_iso_list_or_nil_or_var(c) (is_iso_list_or_nil(c) || is_var(c))
#define is_iso_list_or_var(c) (is_iso_list(c) || is_var(c))
#define is_iso_atom_or_var(c) (is_iso_atom(c) || is_var(c))
#define is_iso_atomic_or_var(c) (is_iso_atom(c) || is_number(c) || is_var(c))

void make_uint(cell *tmp, pl_uint_t v);
void make_int(cell *tmp, pl_int_t v);
void make_float(cell *tmp, pl_flt_t v);
void make_ptr(cell *tmp, void *v);
void make_struct(cell *tmp, pl_idx_t offset, void *fn, unsigned arity, pl_idx_t extra_cells);
void make_var(cell *tmp, pl_idx_t off, unsigned var_nbr);
void make_var2(cell *tmp, pl_idx_t off);
void make_call(query *q, cell *tmp);
void make_call_return(query *q, cell *tmp, cell *ret);
void make_end(cell *tmp);

void make_atom(cell *tmp, pl_idx_t offset);
void make_smalln(cell *tmp, const char *s, size_t n);

bool make_cstringn(cell *d, const char *s, size_t n);
bool make_stringn(cell *d, const char *s, size_t n);

#if USE_FFI
bool fn_sys_dlopen_3(query *q);
bool fn_sys_dlsym_3(query *q);
bool fn_sys_dlclose_1(query *q);
bool fn_sys_ffi_register_function_4(query *q);
bool fn_sys_ffi_register_predicate_4(query *q);
#endif

bool fn_iso_add_2(query *q);
bool fn_iso_float_1(query *q);
bool fn_iso_integer_1(query *q);

inline static void make_indirect(cell *tmp, cell *v, pl_idx_t v_ctx)
{
	tmp->tag = TAG_PTR;
	tmp->nbr_cells = 1;
	tmp->arity = 0;
	tmp->flags = 0;
	tmp->val_ptr = v;
	tmp->var_ctx = v_ctx;
}

inline static cell *get_var(query *q, cell *c, pl_idx_t c_ctx)
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

bool check_trail(query *q);

inline static void add_trail(query *q, pl_idx_t c_ctx, unsigned c_var_nbr, cell *attrs, pl_idx_t attrs_ctx)
{
	if (q->st.tp >= q->trails_size) {
		if (!check_trail(q)) {
			q->error = false;
			return;
		}
	}

	trail *tr = q->trails + q->st.tp++;
	tr->var_ctx = c_ctx;
	tr->var_nbr = c_var_nbr;
	tr->attrs = attrs;
	tr->attrs_ctx = attrs_ctx;
}

inline static void set_var(query *q, const cell *c, pl_idx_t c_ctx, cell *v, pl_idx_t v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);
	cell *c_attrs = is_empty(&e->c) ? e->c.attrs : NULL, *v_attrs = NULL;
	pl_idx_t c_attrs_ctx = c_attrs ? e->c.attrs_ctx : 0;

	if (c_attrs || (q->cp && (c_ctx < q->st.fp)))
		add_trail(q, c_ctx, c->var_nbr, c_attrs, c_attrs_ctx);

	if (c_attrs && is_var(v)) {
		const frame *vf = GET_FRAME(v_ctx);
		const slot *ve = GET_SLOT(vf, v->var_nbr);
		v_attrs = is_empty(&ve->c) ? ve->c.attrs : NULL;
	}

	// If 'c' is an attvar and either 'v' is an attvar or nonvar then run the hook
	// If 'c' is an attvar and 'v' is a plain var then copy attributes to 'v'
	// If 'c' is a plain var and 'v' is an attvar then copy attributes to 'c'

	if (c_attrs && (v_attrs || is_nonvar(v))) {
		q->run_hook = true;
	} else if (c_attrs && !v_attrs && is_var(v)) {
		const frame *vf = GET_FRAME(v_ctx);
		slot *ve = GET_SLOT(vf, v->var_nbr);
		add_trail(q, v_ctx, v->var_nbr, NULL, 0);
		ve->c.attrs = c_attrs;
		ve->c.attrs_ctx = c_attrs_ctx;
	}

	if (is_structure(v)) {
		q->no_tco = true;
		make_indirect(&e->c, v, v_ctx);
	} else if (is_var(v)) {
		e->c = *v;
		e->c.flags |= FLAG_VAR_REF;
		e->c.var_ctx = v_ctx;
	} else {
		share_cell(v);
		e->c = *v;
	}
}

inline static void reset_var(query *q, const cell *c, pl_idx_t c_ctx, cell *v, pl_idx_t v_ctx)
{
	const frame *f = GET_FRAME(c_ctx);
	slot *e = GET_SLOT(f, c->var_nbr);

	if (q->cp && (c_ctx < q->st.fp))
		add_trail(q, c_ctx, c->var_nbr, NULL, 0);

	if (is_structure(v)) {
		make_indirect(&e->c, v, v_ctx);
	} else if (is_var(v)) {
		e->c = *v;
		e->c.flags |= FLAG_VAR_REF;
		e->c.var_ctx = v_ctx;
	} else {
		share_cell(v);
		e->c = *v;
	}
}

#define deref(q,c,c_ctx) \
	is_indirect(c) ? q->latest_ctx = (c)->var_ctx, (c)->val_ptr : \
	!is_var(c) ? q->latest_ctx = (c_ctx), (c) : \
	get_var(q, c, c_ctx)

#define GET_RAW_ARG(n,p) \
	cell *p = get_raw_arg(q,n); \
	pl_idx_t p##_ctx = q->latest_ctx

#define GET_FIRST_ARG(p,vt) \
	cell *p = get_first_arg(q); \
	pl_idx_t p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_ARG0(p,vt,p0) \
	cell *p = get_first_arg0(q,p0); \
	pl_idx_t p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_RAW_ARG(p,vt) \
	cell *p = get_first_raw_arg(q); \
	pl_idx_t p##_ctx = q->st.curr_frame; \
	if (!is_##vt(p)) { return throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_FIRST_RAW_ARG0(p,vt,p0) \
	cell *p = get_first_raw_arg0(q,p0); \
	pl_idx_t p##_ctx = q->st.curr_frame; \
	if (!is_##vt(p)) { return throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_NEXT_ARG(p,vt) \
	cell *p = get_next_arg(q); \
	pl_idx_t p##_ctx = q->latest_ctx; \
	if (!is_##vt(p)) { return throw_error(q, p, p##_ctx, "type_error", #vt); }

#define GET_NEXT_RAW_ARG(p,vt) \
	cell *p = get_next_raw_arg(q); \
	pl_idx_t p##_ctx = q->st.curr_frame; \
	if (!is_##vt(p)) { return throw_error(q, p, p##_ctx, "type_error", #vt); }

inline static cell *get_first_arg(query *q)
{
	q->last_arg = q->st.curr_cell + 1;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_first_arg0(query *q, cell *p0)
{
	q->last_arg = p0 + 1;
	return deref(q, q->last_arg, q->st.curr_frame);
}

inline static cell *get_first_raw_arg(query *q)
{
	q->last_arg = q->st.curr_cell + 1;
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
	cell *c = q->st.curr_cell + 1;

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
	if (is_builtin(c) && c->fn_ptr && (c->fn_ptr->fn != fn_iso_float_1) && (c->fn_ptr->fn != fn_iso_integer_1)) \
		return throw_error(q, c, q->st.curr_frame, "type_error", "evaluable");

#define check_heap_error(expr, ...) \
	CHECK_SENTINEL(expr, 0, __VA_ARGS__; \
	return false /*throw_error(q, q->st.curr_cell, q->st.curr_frame, "resource_error", "memory")*/)
