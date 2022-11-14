#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <fenv.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "heap.h"
#include "module.h"
#include "prolog.h"
#include "query.h"

#define SET_ACCUM() {											\
	if (errno == ENOMEM)										\
		return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory"); \
	q->accum.tag = TAG_INTEGER;										\
	q->accum.flags = FLAG_MANAGED;								\
	q->accum.val_bigint = malloc(sizeof(bigint));				\
	if (errno == ENOMEM)										\
		return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory"); \
	q->accum.val_bigint->refcnt = 0;							\
	if (mp_int_init_copy(&q->accum.val_bigint->ival, &q->tmp_ival) == MP_MEMORY) {\
		return throw_error(q, &q->accum, q->st.curr_frame, "resource_error", "memory"); \
	} \
	if (errno == ENOMEM)										\
		return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory"); \
}

void clr_accum(cell *p)
{
	if (is_bigint(p) && !p->val_bigint->refcnt) {
		mp_int_clear(&p->val_bigint->ival);
		free(p->val_bigint);
	}

	p->tag = TAG_INTEGER;
	p->nbr_cells = 1;
	p->arity = 0;
	p->flags = 0;
	p->val_int = 0;
}

#define CLEANUP __attribute__((cleanup (clr_accum)))

#if defined(__SIZEOF_INT128__)

#define ON_OVERFLOW(op,v1,v2)									\
	__int128_t tmp = (__int128_t)v1 op v2;						\
	if ((tmp > INT64_MAX) || (tmp < INT64_MIN))

#else

#define ON_OVERFLOW(op,v1,v2)									\
	if ((v1) >= INT32_MAX ||									\
		(v1) <= INT32_MIN ||									\
		(v2) >= INT32_MAX ||									\
		(v2) <= INT32_MIN)
#endif

#define DO_OP2(op,op2,p1,p2) \
	if (is_smallint(&p1) && is_smallint(&p2)) { \
		ON_OVERFLOW(op, p1.val_int, p2.val_int) { \
			mpz_t tmp; \
			mp_int_init_value(&tmp, p1.val_int); \
			mp_int_##op2##_value(&tmp, p2.val_int, &q->tmp_ival); \
			mp_int_clear(&tmp); \
			SET_ACCUM(); \
		} else { \
			q->accum.val_int = p1.val_int op p2.val_int; \
			q->accum.tag = TAG_INTEGER; \
		} \
	} else if (is_bigint(&p1)) { \
		if (is_bigint(&p2)) { \
			mp_int_##op2(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival); \
			SET_ACCUM(); \
		} else if (is_smallint(&p2)) { \
			mp_int_##op2##_value(&p1.val_bigint->ival, p2.val_int, &q->tmp_ival); \
			SET_ACCUM(); \
		} else if (is_float(&p2)) { \
			double d = BIGINT_TO_DOUBLE(&p1.val_bigint->ival); \
			q->accum.val_float = d op p2.val_float; \
			if (isinf(q->accum.val_float)) return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow"); \
			q->accum.tag = TAG_FLOAT; \
			q->accum.flags = 0; \
		} \
	} else if (is_bigint(&p2)) { \
		if (is_smallint(&p1)) { \
			mpz_t tmp; \
			mp_int_init_value(&tmp, p1.val_int); \
			mp_int_##op2(&tmp, &p2.val_bigint->ival, &q->tmp_ival); \
			mp_int_clear(&tmp); \
			SET_ACCUM(); \
		} else if (is_float(&p1)) { \
			double d = BIGINT_TO_DOUBLE(&p2.val_bigint->ival); \
			q->accum.val_float = p1.val_float op d; \
			if (isinf(q->accum.val_float)) return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow"); \
			q->accum.tag = TAG_FLOAT; \
			q->accum.flags = 0; \
		} \
	} else if (is_smallint(&p1) && is_float(&p2)) { \
		q->accum.val_float = (double)p1.val_int op p2.val_float; \
		if (isinf(q->accum.val_float)) return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow"); \
		q->accum.tag = TAG_FLOAT; \
	} else if (is_float(&p1) && is_float(&p2)) { \
		q->accum.val_float = p1.val_float op p2.val_float; \
		if (isinf(q->accum.val_float)) return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow"); \
		q->accum.tag = TAG_FLOAT; \
	} else if (is_float(&p1) && is_smallint(&p2)) { \
		q->accum.val_float = p1.val_float op p2.val_int; \
		if (isinf(q->accum.val_float)) return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow"); \
		q->accum.tag = TAG_FLOAT; \
	} else if (is_var(&p1) || is_var(&p2)) { \
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated"); \
	} else { \
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable"); \
	}

static double BIGINT_TO_DOUBLE(mpz_t *v)
{
	double d;
	mp_int_to_double(v, &d);
	return d;
}

static mp_result mp_int_divx(mp_int a, mp_int b, mp_int q)
{
	return mp_int_div(a, b, q, NULL);
}

static mp_result mp_int_divx_value(mp_int a, mp_small b, mp_int q)
{
	return mp_int_div_value(a, b, q, NULL);
}

bool call_builtin(query *q, cell *c, pl_idx_t c_ctx)
{
	cell *save = q->st.curr_cell;
	pl_idx_t save_ctx = q->st.curr_frame;
	bool save_calc = q->eval;
	q->st.curr_cell = c;
	q->st.curr_frame = c_ctx;
	q->eval = true;

#if USE_FFI
	if (c->fn_ptr && c->fn_ptr->ffi)
		wrapper_for_function(q, c->fn_ptr);
	else
#endif
	if (!c->fn_ptr->evaluable && (c->val_off != g_float_s))
		return throw_error(q, &q->accum, q->st.curr_frame, "type_error", "evaluable");
	else
		c->fn_ptr->fn(q);

	q->eval = save_calc;

	if (!q->did_throw) {
		q->st.curr_cell = save;
		q->st.curr_frame = save_ctx;
	}

	return true;
}

bool call_userfun(query *q, cell *c, pl_idx_t c_ctx)
{
	if (q->retry)
		return false;

	if (is_string(c))
		return throw_error(q, c, c_ctx, "type_error", "evaluable");

	if (!c->match)
		c->match = search_predicate(q->st.m, c, NULL);

	if (!c->match)
		return throw_error(q, c, c_ctx, "type_error", "evaluable");

	// Currently user-defined functions are disabled...

	return throw_error(q, c, c_ctx, "type_error", "evaluable");

	cell *save = q->st.curr_cell;
	pl_idx_t save_ctx = q->st.curr_frame;
	cell *tmp = clone_to_heap(q, true, c, 2);
	pl_idx_t nbr_cells = 1 + c->nbr_cells;
	make_struct(tmp+nbr_cells++, g_sys_drop_barrier, fn_sys_drop_barrier, 0, 0);
	make_call(q, tmp+nbr_cells);
	check_heap_error(push_call_barrier(q));
	q->st.curr_cell = tmp;
	bool ok = start(q);
	q->error = false;

	if (!q->did_throw) {
		q->st.curr_cell = save;
		q->st.curr_frame = save_ctx;
	}

	return ok;
}

static bool fn_return_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum = p1;
	inner_cut(q, false);
	drop_choice(q);
	q->error = true;
	return true;
}

static bool fn_iso_is_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p2 = eval(q, p2_tmp);
	p2.nbr_cells = 1;

	if (is_var(p1) && is_number(&p2)) {
		bool ok = unify(q, p1, p1_ctx, &p2, q->st.curr_frame);
		clr_accum(&q->accum);
		return ok;
	}

	if (is_smallint(p1) && is_smallint(&p2))
		return (p1->val_int == p2.val_int);

	if (is_bigint(p1) && is_bigint(&p2))
		return !mp_int_compare(&p1->val_bigint->ival, &p2.val_bigint->ival);

	if (is_bigint(p1) && is_smallint(&p2))
		return !mp_int_compare_value(&p1->val_bigint->ival, p2.val_int);

	if (is_bigint(&p2) && is_smallint(p1))
		return !mp_int_compare_value(&p2.val_bigint->ival, p1->val_int);

	if (is_float(p1) && is_float(&p2))
		return p1->val_float == p2.val_float;

	if (is_atom(p1) && is_number(&p2) && !strcmp(C_STR(q, p1), "nan"))
		return is_float(&p2)? isnan(p2.val_float) : 0;

	if (is_atom(p1) && is_number(&p2) && !strcmp(C_STR(q, p1), "inf"))
		return is_float(&p2) ? isinf(p2.val_float) : 0;

	return false;
}

bool fn_iso_float_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->eval) {
		CLEANUP cell p1 = eval(q, p1_tmp);

		if (is_float(&p1)) {
			q->accum.val_float = p1.val_float;
			q->accum.tag = TAG_FLOAT;
			return true;
		}

		if (is_bigint(&p1)) {
			q->accum.val_float = BIGINT_TO_DOUBLE(&p1.val_bigint->ival);

			if (isinf(q->accum.val_float))
				return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

			q->accum.tag = TAG_FLOAT;
			return true;
		}

		if (is_smallint(&p1)) {
			q->accum.val_float = (double)p1.val_int;
			q->accum.tag = TAG_FLOAT;
			return true;
		}

		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer_or_float");
	}

	return is_float(p1_tmp);
}

bool fn_iso_integer_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);

	if (q->eval) {
		CLEANUP cell p1 = eval(q, p1_tmp);

		if (is_integer(&p1)) {
			share_cell(&p1);
			q->accum = p1;
			return true;
		}

		if (is_float(&p1) && (p1.val_float < (double)PL_INT_MAX) && (p1.val_float > (double)PL_INT_MIN)) {
			q->accum.val_int = (pl_int_t)p1.val_float;
			q->accum.tag = TAG_INTEGER;
			return true;
		}

		if (is_float(&p1)) {
			mp_int_set_double(&q->tmp_ival, p1.val_float);
			SET_ACCUM();
			return true;
		}

		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer_or_float");
	}

	return is_integer(p1_tmp);
}

static bool fn_iso_abs_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum.tag = p1.tag;

	if (is_smallint(&p1))
		q->accum.val_int = llabs((long long)p1.val_int);
	else if (is_bigint(&p1)) {
		mp_int_abs(&p1.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_float(&p1))
		q->accum.val_float = fabs(p1.val_float);
	else
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");

	return true;
}

static bool fn_iso_sign_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum.tag = p1.tag;

	if (is_smallint(&p1))
		q->accum.val_int = p1.val_int < 0 ? -1 : p1.val_int > 0  ? 1 : 0;
	else if (is_bigint(&p1))
		q->accum.val_int = mp_int_compare_zero(&p1.val_bigint->ival);
	else if (is_float(&p1))
		q->accum.val_float = p1.val_float < 0 ? -1 : p1.val_float > 0  ? 1 : 0;
	else
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");

	return true;
}

static bool fn_iso_positive_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum = p1;
	return true;
}

static bool fn_iso_negative_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	q->accum.tag = p1.tag;

	if (is_smallint(&p1))
		q->accum.val_int = -p1.val_int;
	else if (is_bigint(&p1)) {
		mp_int_neg(&p1.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_float(&p1))
		q->accum.val_float = -p1.val_float;
	else if (is_var(&p1))
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	else
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");

	return true;
}

static bool fn_iso_epsilon_0(query *q)
{
	CHECK_CALC();
	q->accum.val_float = DBL_EPSILON;
	q->accum.tag = TAG_FLOAT;
	return true;
}

static bool fn_iso_pi_0(query *q)
{
	CHECK_CALC();
	q->accum.val_float = M_PI;
	q->accum.tag = TAG_FLOAT;
	return true;
}

static bool fn_iso_e_0(query *q)
{
	CHECK_CALC();
	q->accum.val_float = M_E;
	q->accum.tag = TAG_FLOAT;
	return true;
}

bool fn_iso_add_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);
	DO_OP2(+, add, p1, p2);
	return true;
}

static bool fn_iso_sub_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);
	DO_OP2(-, sub, p1, p2);
	return true;
}

static bool fn_iso_mul_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);
	DO_OP2(*, mul, p1, p2);
	return true;
}

static bool fn_iso_exp_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_bigint(&p1)) {
		if (mp_int_compare_zero(&p1.val_bigint->ival) <= 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = exp(BIGINT_TO_DOUBLE(&p1.val_bigint->ival));

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;

	} else if (is_smallint(&p1)) {
		q->accum.val_float = exp((double)p1.val_int);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;

	} else if (is_float(&p1)) {
		q->accum.val_float = exp(p1.val_float);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_sqrt_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_bigint(&p1)) {
		if (mp_int_compare_zero(&p1.val_bigint->ival) < 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = sqrt(BIGINT_TO_DOUBLE(&p1.val_bigint->ival));
		q->accum.tag = TAG_FLOAT;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");
	} else if (is_smallint(&p1)) {
		if (p1.val_int < 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = sqrt((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		if (p1.val_float == -1)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = sqrt(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_log_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_bigint(&p1)) {
		if (mp_int_compare_zero(&p1.val_bigint->ival) <= 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = log(BIGINT_TO_DOUBLE(&p1.val_bigint->ival));
		q->accum.tag = TAG_FLOAT;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");
	} else if (is_smallint(&p1)) {
		if (p1.val_int <= 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = log((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		if (p1.val_float <= 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = log(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_popcount_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");

	if (is_bigint(&p1)) {
		mp_usmall count = 0;

		if (mp_int_popcount(&p1.val_bigint->ival, &count) != MP_OK)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_zero");

		q->accum.val_int = count;
	} else {
		if (p1.val_int < 0)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_zero");

		uint64_t n = p1.val_int;
		uint64_t count = 0;

		while (n > 0) {
			n = n & (n - 1);
			count++;
		}

		q->accum.val_int = count;
	}

	q->accum.tag = TAG_INTEGER;
	return true;
}

static bool fn_lsb_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");

	if (is_bigint(&p1)) {
		mp_usmall count = 0;

		if (mp_int_lsb(&p1.val_bigint->ival, &count) != MP_OK)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_one");

		q->accum.val_int = count;
	} else {
		if (p1.val_int < 1)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_one");

		uint64_t n = p1.val_int;
    	uint64_t lsb = 0;
    	while ((n & 1) == 0) {
    	    ++lsb;
    	    n = n >> 1;
    	}

		q->accum.val_int = lsb;
	}

	q->accum.tag = TAG_INTEGER;
	return true;
}

static bool fn_msb_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");

	if (is_bigint(&p1)) {
		mp_usmall count = 0;

		if (mp_int_msb(&p1.val_bigint->ival, &count) != MP_OK)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_one");

		q->accum.val_int = count;
	} else {
		if (p1.val_int < 1)
			return throw_error(q, &p1, q->st.curr_frame, "domain_error", "not_less_than_one");

		uint64_t n = p1.val_int;
    	uint64_t msb = -1;
    	while (n != 0) {
    	    msb++;
    	    n = n >> 1;
    	}

		q->accum.val_int = msb;
	}

	q->accum.tag = TAG_INTEGER;
	return true;
}

static bool fn_iso_truncate_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_float(&p1)) {
		q->accum.val_int = (pl_int_t)p1.val_float;

#ifdef FE_INVALID
		if (fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW)) {
			mp_int_set_double(&q->tmp_ival, p1.val_float);
			SET_ACCUM();
		} else
#endif
			q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_round_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_float(&p1)) {
		double f = fabs(p1.val_float);

#ifdef FE_UPWARD
		if ((f - floor(f)) > 0.5)
			fesetround(FE_TONEAREST);
		else
			fesetround(FE_UPWARD);
#else
		fesetround(FE_TONEAREST);
#endif

		q->accum.val_int = llrint(p1.val_float);

#ifdef FE_INVALID
		if (fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW)) {
			mp_int_set_double(&q->tmp_ival, p1.val_float);
			SET_ACCUM();
		} else
#endif
			q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_ceiling_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_float(&p1)) {
		q->accum.val_int = (pl_int_t)ceil(p1.val_float);

#ifdef FE_INVALID
		if (fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW)) {
			mp_int_set_double(&q->tmp_ival, p1.val_float);
			SET_ACCUM();
		} else
#endif
			q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_float_integer_part_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_float(&p1)) {
		q->accum.val_float = (pl_int_t)p1.val_float;
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_float_fractional_part_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_float(&p1)) {
		q->accum.val_float = p1.val_float - (pl_int_t)p1.val_float;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_floor_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_float(&p1)) {
		q->accum.val_int = (pl_int_t)floor(p1.val_float);

#ifdef FE_INVALID
		if (fetestexcept(FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW)) {
			mp_int_set_double(&q->tmp_ival, p1.val_float);
			SET_ACCUM();
		} else
#endif
			q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_sin_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = sin((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = sin(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_cos_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = cos((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = cos(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_tan_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = tan((double)p1.val_int);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = tan(p1.val_float);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_asin_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = asin((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = asin(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_acos_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = acos((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = acos(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_atan_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = atan((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = atan(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_atan2_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2)) {
		if ((p1.val_int == 0) && (p2.val_int == 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = atan2((double)p1.val_int, (double)p2.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_smallint(&p1) && is_float(&p2)) {
		if ((p1.val_int == 0) && (p2.val_float == 0.0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = atan2((double)p1.val_int, p2.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		if ((p1.val_float == 0.0) && (p2.val_int == 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = atan2(p1.val_float, p2.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_smallint(&p2)) {
		if ((p1.val_float == 0.0) && (p2.val_int == 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = atan2(p1.val_float, (double)p2.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_sinh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = sinh((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = sinh(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_cosh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = cosh((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = cosh(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_tanh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = tanh((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = tanh(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_asinh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = asinh((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = asinh(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_acosh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = acosh((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = acosh(p1.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_atanh_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_float = atanh((double)p1.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1)) {
		q->accum.val_float = atanh(p1.val_float);
		if (isinf(q->accum.val_float)) return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "undefined");
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isinf(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_copysign_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum = p1;

		if (p2.val_int < 0)
			q->accum.val_int = -llabs((long long)p1.val_int);

		q->accum.tag = TAG_INTEGER;
	} else if (is_smallint(&p1) && is_float(&p2)) {
		q->accum = p1;

		if (p2.val_float < 0.0)
			q->accum.val_int = -llabs((long long)p1.val_int);

		q->accum.tag = TAG_INTEGER;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_float = copysign(p1.val_float, p2.val_float);
		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_smallint(&p2)) {
		q->accum.val_float = copysign(p1.val_float, p2.val_int);
		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static bool fn_iso_pow_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_smallint(&p2)) {
		if ((mp_int_compare_zero(&p1.val_bigint->ival) == 0) && (p2.val_int < 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = pow(BIGINT_TO_DOUBLE(&p1.val_bigint->ival), (double)p2.val_int);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
		return true;
	}

	if (is_smallint(&p1) && is_smallint(&p2)) {
		if ((p1.val_int == 0) && (p2.val_int < 0))
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = pow((double)p1.val_int, (double)p2.val_int);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_smallint(&p1) && is_float(&p2)) {
		if ((p1.val_int == 0) && (p2.val_float < 0.0))
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = pow((double)p1.val_int, p2.val_float);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		if ((p1.val_float == 0.0) && (p2.val_float < 0.0))
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = pow(p1.val_float, p2.val_float);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_smallint(&p2)) {
		if ((p1.val_float == 0.0) && (p2.val_int < 0))
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");

		q->accum.val_float = pow(p1.val_float, p2.val_int);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_powi_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		if (is_negative(&p2))
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "greater_zero");

		if (mp_int_expt_full(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival) != MP_OK)
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");

		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		if (p2.val_int < 0)
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "greater_zero");

		if (p2.val_int > (INT32_MAX/2))
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");

		if (mp_int_expt(&p1.val_bigint->ival, p2.val_int, &q->tmp_ival) != MP_OK)
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");

		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		if (is_negative(&p2))
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "greater_zero");

		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);

		if (mp_int_expt_full(&tmp, &p2.val_bigint->ival, &q->tmp_ival) != MP_OK) {
			mp_int_clear(&tmp);
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");
		}

		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if ((p1.val_int == 0) && (p2.val_int < 0))
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

		if ((p1.val_int != 1) && (p2.val_int < 0))
			return throw_error(q, &p1, q->st.curr_frame, "type_error", "float");

		if (p2.val_int < 0) {
			q->accum.val_int = pow(p1.val_int, p2.val_int);
			q->accum.tag = TAG_INTEGER;
			return true;
		}

		if (p2.val_int > (INT32_MAX/2))
			return throw_error(q, &p2, q->st.curr_frame, "resource_error", "memory");

		if (mp_int_expt_value(p1.val_int, p2.val_int, &q->tmp_ival) != MP_OK)
			return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory");

		if (errno == ENOMEM)
			return throw_error(q, &p1, q->st.curr_frame, "resource_error", "memory");

		if (mp_int_compare_value(&q->tmp_ival, MP_SMALL_MAX) > 0) {
			SET_ACCUM();
			return true;
		}

		mp_small i;
		mp_int_to_int(&q->tmp_ival, &i);
		q->accum.val_int = i;
		q->accum.tag = TAG_INTEGER;
	} else if (is_smallint(&p1) && is_float(&p2)) {
		q->accum.val_float = pow(p1.val_int, p2.val_float);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_float = pow(p1.val_float, p2.val_float);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_smallint(&p2)) {
		q->accum.val_float = pow(p1.val_float, p2.val_int);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_divide_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		q->accum.val_float = BIGINT_TO_DOUBLE(&p1.val_bigint->ival);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		double d = BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

		if (isinf(d))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		if (d == 0.0)
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float /= d;
		q->accum.tag = TAG_FLOAT;
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		q->accum.val_float = BIGINT_TO_DOUBLE(&p1.val_bigint->ival);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float /= p2.val_int;
		q->accum.tag = TAG_FLOAT;
	} else if (is_bigint(&p1) && is_float(&p2)) {
		if (p2.val_float == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float = BIGINT_TO_DOUBLE(&p1.val_bigint->ival) / p2.val_float;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		q->accum.val_float = p1.val_int;
		double d = BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

		if (isinf(d))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		if (d == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float /= d;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_bigint(&p2) && is_float(&p1)) {
		double d = BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

		if (isinf(d))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		if (d == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float = p1.val_float / d;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float = (double)p1.val_int / p2.val_int;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_smallint(&p1) && is_float(&p2)) {
		if (p2.val_float == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float = (double)p1.val_int / p2.val_float;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		if (p2.val_float == 0.0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float = p1.val_float / p2.val_float;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_float = p1.val_float / p2.val_int;

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_float(&q->accum) && isnan(q->accum.val_float))
		return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");

	return true;
}

static bool fn_iso_divint_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_integer(&p1) && is_integer(&p2)) {
		if (is_bigint(&p2) && mp_int_compare_zero(&p2.val_bigint->ival) == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		if (is_smallint(&p2) && get_smallint(&p2) == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		DO_OP2(/, divx, p1, p2);
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_mod_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_int = p1.val_int % p2.val_int;

		if (p2.val_int < 0)
			q->accum.val_int *= -1;

		if (p1.val_int < 0)
			q->accum.val_int *= -1;

		q->accum.tag = TAG_INTEGER;
	} else if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_mod(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);

		if (mp_int_compare_zero(&p2.val_bigint->ival))
			mp_int_neg(&q->tmp_ival, &q->tmp_ival);

		if (mp_int_compare_zero(&p1.val_bigint->ival))
			mp_int_neg(&q->tmp_ival, &q->tmp_ival);

		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mp_small n;
		mp_int_mod_value(&p1.val_bigint->ival, p2.val_int, &n);
		q->accum.val_int = n;
		q->accum.tag = TAG_INTEGER;
	} else if (is_smallint(&p1) && is_bigint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_mod(&tmp, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
		mp_int_clear(&tmp);
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_div_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mpz_t tmp3, tmp4;
		mp_int_init(&tmp3);
		mp_int_init(&tmp4);
		mp_int_mod(&p1.val_bigint->ival, &p2.val_bigint->ival, &tmp3);
		mp_int_sub(&p1.val_bigint->ival, &tmp3, &tmp4);
		mp_int_div(&tmp4, &p2.val_bigint->ival, &q->tmp_ival, NULL);
		SET_ACCUM();
		mp_int_clear(&tmp3);
		mp_int_clear(&tmp4);
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp2, tmp3, tmp4;
		mp_int_init_value(&tmp2, p2.val_int);
		mp_int_init(&tmp3);
		mp_int_init(&tmp4);
		mp_int_mod(&p1.val_bigint->ival, &tmp2, &tmp3);
		mp_int_sub(&p1.val_bigint->ival, &tmp3, &tmp4);
		mp_int_div(&tmp4, &tmp2, &q->tmp_ival, NULL);
		SET_ACCUM();
		mp_int_clear(&tmp2);
		mp_int_clear(&tmp3);
		mp_int_clear(&tmp4);
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp1, tmp3, tmp4;
		mp_int_init_value(&tmp1, p1.val_int);
		mp_int_init(&tmp3);
		mp_int_init(&tmp4);
		mp_int_mod(&tmp1, &p2.val_bigint->ival, &tmp3);
		mp_int_sub(&tmp1, &tmp3, &tmp4);
		mp_int_div(&tmp4, &p2.val_bigint->ival, &q->tmp_ival, NULL);
		SET_ACCUM();
		mp_int_clear(&tmp1);
		mp_int_clear(&tmp3);
		mp_int_clear(&tmp4);
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

        q->accum.val_int = p1.val_int % p2.val_int;

		if ((q->accum.val_int != 0) && ((q->accum.val_int ^ p2.val_int) < 0))
			q->accum.val_int = q->accum.val_int + p2.val_int;

		q->accum.val_int = (p1.val_int - q->accum.val_int) / p2.val_int;
		q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_rem_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p2.val_int == 0)
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");

		q->accum.val_int = p1.val_int % p2.val_int;
		q->accum.tag = TAG_INTEGER;
	} if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_mod(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_mod(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_bigint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_mod(&tmp, &p2.val_bigint->ival, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_max_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

 	if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p1.val_int >= p2.val_int)
			q->accum = p1;
		else
			q->accum = p2;

		q->accum.tag = TAG_INTEGER;
	} else if (is_bigint(&p1)) {
		if (is_bigint(&p2)) {
			if (mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) >= 0) {
				mp_int_init_copy(&q->tmp_ival, &p1.val_bigint->ival);
			} else {
				mp_int_init_copy(&q->tmp_ival, &p2.val_bigint->ival);
			}
		} else if (is_smallint(&p2)) {
			if (mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) >= 0) {
				mp_int_init_copy(&q->tmp_ival, &p1.val_bigint->ival);
			} else {
				mp_int_set_value(&q->tmp_ival, p2.val_int);
			}
		} else
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");

		SET_ACCUM();
	} else if (is_bigint(&p2)) {
		if (is_smallint(&p1)) {
			if (mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) >= 0) {
				mp_int_init_copy(&q->tmp_ival, &p2.val_bigint->ival);
			} else {
				mp_int_set_value(&q->tmp_ival, p1.val_int);
			}
		} else
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");

		SET_ACCUM();
	} else if (is_smallint(&p1) && is_float(&p2)) {
		double f1 = (double)p1.val_int;

		if (f1 > p2.val_float)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_smallint(&p2) && is_float(&p1)) {
		double f2 = (double)p2.val_int;

		if (f2 > p1.val_float)
			q->accum = p2;
		else
			q->accum = p1;
	} else if (is_float(&p1) && is_float(&p2)) {
		if (p1.val_float > p2.val_float)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_smallint(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_min_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2)) {
		if (p1.val_int <= p2.val_int)
			q->accum = p1;
		else
			q->accum = p2;

		q->accum.tag = TAG_INTEGER;
	} if (is_bigint(&p1)) {
		if (is_bigint(&p2)) {
			if (mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) <= 0) {
				mp_int_init_copy(&q->tmp_ival, &p1.val_bigint->ival);
			} else {
				mp_int_clear(&q->tmp_ival);
				mp_int_init_copy(&q->tmp_ival, &p2.val_bigint->ival);
			}
		} else if (is_smallint(&p2)) {
			if (mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) <= 0) {
				mp_int_clear(&q->tmp_ival);
				mp_int_init_copy(&q->tmp_ival, &p1.val_bigint->ival);
			} else {
				mp_int_set_value(&q->tmp_ival, p2.val_int);
			}
		} else
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");

		SET_ACCUM();
	} else if (is_bigint(&p2)) {
		if (is_smallint(&p1)) {
			if (mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) <= 0) {
				mp_int_clear(&q->tmp_ival);
				mp_int_init_copy(&q->tmp_ival, &p2.val_bigint->ival);
			} else {
				mp_int_set_value(&q->tmp_ival, p1.val_int);
			}
		} else
			return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");

		SET_ACCUM();
	} else if (is_smallint(&p1) && is_float(&p2)) {
		double f1 = (double)p1.val_int;

		if (f1 < p2.val_float)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_smallint(&p2) && is_float(&p1)) {
		double f2 = (double)p2.val_int;

		if (f2 < p1.val_float)
			q->accum = p2;
		else
			q->accum = p1;
	} else if (is_float(&p1) && is_float(&p2)) {
		if (p1.val_float < p2.val_float)
			q->accum = p1;
		else
			q->accum = p2;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_smallint(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_smallint(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_xor_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_xor(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_xor(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_xor(&p2.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int ^ p2.val_int;
		q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_or_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_or(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_or(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_or(&p2.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int | p2.val_int;
		q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_and_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_and(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_and(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_and(&p2.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int & p2.val_int;
		q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_shl_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_smallint(&p2)) {
		mp_int_mul_pow2(&p1.val_bigint->ival, p2.val_int, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int << p2.val_int;

		if ((q->accum.val_int >= 0) && (p2.val_int < 64)) {
			q->accum.tag = TAG_INTEGER;
			return true;
		}

		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_mul_pow2(&tmp, p2.val_int, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_shr_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_smallint(&p2)) {
		mp_int_div_pow2(&p1.val_bigint->ival, p2.val_int, &q->tmp_ival, NULL);
		SET_ACCUM();
	} if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = p1.val_int >> p2.val_int;
		q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_neg_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_smallint(&p1)) {
		q->accum.val_int = ~p1.val_int;
		q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_iso_seq_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res == 0;
}

static bool fn_iso_sne_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res != 0;
}

static bool fn_iso_slt_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res < 0;
}

static bool fn_iso_sle_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res <= 0;
}

static bool fn_iso_sgt_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res > 0;
}

static bool fn_iso_sge_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	int res = compare(q, p1, p1_ctx, p2, p2_ctx);
	return res >= 0;
}

static bool fn_iso_neq_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int == p2.val_int;
	else if (is_smallint(&p1) && is_bigint(&p2))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) == 0;
	else if (is_smallint(&p1) && is_float(&p2))
		return p1.val_int == p2.val_float;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) == 0;
	else if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) == 0;
	else if (is_bigint(&p1) && is_float(&p2))
		return BIGINT_TO_DOUBLE(&p1.val_bigint->ival) == p2.val_float;
	else if (is_float(&p1) && is_smallint(&p2))
		return p1.val_float == p2.val_int;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_float == p2.val_float;
	else if (is_float(&p1) && is_bigint(&p2))
		return p1.val_float == BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static bool fn_iso_nne_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int != p2.val_int;
	else if (is_smallint(&p1) && is_bigint(&p2))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) != 0;
	else if (is_smallint(&p1) && is_float(&p2))
		return p1.val_int != p2.val_float;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) != 0;
	else if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) != 0;
	else if (is_bigint(&p1) && is_float(&p2))
		return BIGINT_TO_DOUBLE(&p1.val_bigint->ival) != p2.val_float;
	else if (is_float(&p1) && is_smallint(&p2))
		return p1.val_float != p2.val_int;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_float != p2.val_float;
	else if (is_float(&p1) && is_bigint(&p2))
		return p1.val_float != BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static bool fn_iso_nge_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int >= p2.val_int;
	else if (is_smallint(&p1) && is_bigint(&p2))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) < 0;
	else if (is_smallint(&p1) && is_float(&p2))
		return p1.val_int >= p2.val_float;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) >= 0;
	else if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) >= 0;
	else if (is_bigint(&p1) && is_float(&p2))
		return BIGINT_TO_DOUBLE(&p1.val_bigint->ival) >= p2.val_float;
	else if (is_float(&p1) && is_smallint(&p2))
		return p1.val_float >= p2.val_int;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_float >= p2.val_float;
	else if (is_float(&p1) && is_bigint(&p2))
		return p1.val_float >= BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static bool fn_iso_ngt_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int > p2.val_int;
	else if (is_smallint(&p1) && is_bigint(&p2))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) <= 0;
	else if (is_smallint(&p1) && is_float(&p2))
		return p1.val_int > p2.val_float;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) > 0;
	else if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) > 0;
	else if (is_bigint(&p1) && is_float(&p2))
		return BIGINT_TO_DOUBLE(&p1.val_bigint->ival) > p2.val_float;
	else if (is_float(&p1) && is_smallint(&p2))
		return p1.val_float > p2.val_int;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_float > p2.val_float;
	else if (is_float(&p1) && is_bigint(&p2))
		return p1.val_float > BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static bool fn_iso_nle_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int <= p2.val_int;
	else if (is_smallint(&p1) && is_bigint(&p2))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) > 0;
	else if (is_smallint(&p1) && is_float(&p2))
		return p1.val_int <= p2.val_float;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) <= 0;
	else if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) <= 0;
	else if (is_bigint(&p1) && is_float(&p2))
		return BIGINT_TO_DOUBLE(&p1.val_bigint->ival) <= p2.val_float;
	else if (is_float(&p1) && is_smallint(&p2))
		return p1.val_float <= p2.val_int;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_float <= p2.val_float;
	else if (is_float(&p1) && is_bigint(&p2))
		return p1.val_float <= BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static bool fn_iso_nlt_2(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_smallint(&p1) && is_smallint(&p2))
		return p1.val_int < p2.val_int;
	else if (is_smallint(&p1) && is_bigint(&p2))
		return mp_int_compare_value(&p2.val_bigint->ival, p1.val_int) >= 0;
	else if (is_smallint(&p1) && is_float(&p2))
		return p1.val_int < p2.val_float;
	else if (is_bigint(&p1) && is_smallint(&p2))
		return mp_int_compare_value(&p1.val_bigint->ival, p2.val_int) < 0;
	else if (is_bigint(&p1) && is_bigint(&p2))
		return mp_int_compare(&p1.val_bigint->ival, &p2.val_bigint->ival) < 0;
	else if (is_bigint(&p1) && is_float(&p2))
		return BIGINT_TO_DOUBLE(&p1.val_bigint->ival) < p2.val_float;
	else if (is_float(&p1) && is_smallint(&p2))
		return p1.val_float < p2.val_int;
	else if (is_float(&p1) && is_float(&p2))
		return p1.val_float < p2.val_float;
	else if (is_float(&p1) && is_bigint(&p2))
		return p1.val_float < BIGINT_TO_DOUBLE(&p2.val_bigint->ival);

	return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
}

static bool fn_log_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (is_var(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (! is_integer(&p1) && ! is_float(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	} else if (! is_integer(&p2) && ! is_float(&p2)){
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "evaluable");
	}

	if (is_smallint(&p1)) {
		if (p1.val_int == 0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p1.val_int < 0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");
		}
	} else if (is_float(&p1)) {
		if (p1.val_float == 0.0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p1.val_float < 0.0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");
		}
	}

	if (is_smallint(&p2)) {
		if (p2.val_int == 0) {
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p2.val_int < 0) {
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");
		}
	} else if (is_float(&p2)) {
		if (p2.val_float == 0.0) {
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p2.val_float < 0.0) {
			return throw_error(q, &p2, q->st.curr_frame, "evaluation_error", "undefined");
		}
	}

	if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_float = log(p2.val_int) / log(p1.val_int);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_smallint(&p1) && is_float(&p2)) {
		q->accum.val_float = log(p2.val_float) / log(p1.val_int);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_smallint(&p2)) {
		q->accum.val_float = log(p2.val_int) / log(p1.val_float);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	} else if (is_float(&p1) && is_float(&p2)) {
		q->accum.val_float = log(p2.val_float) / log(p1.val_float);

		if (isinf(q->accum.val_float))
			return throw_error(q, &q->accum, q->st.curr_frame, "evaluation_error", "float_overflow");

		q->accum.tag = TAG_FLOAT;
	}

	return true;
}

static bool fn_log10_1(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);

	if (is_var(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else 	if (is_smallint(&p1)) {
		if (p1.val_int == 0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p1.val_int < 0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");
		} else {
			q->accum.val_float = log10(p1.val_int);
			q->accum.tag = TAG_FLOAT;
		}
	} else if (is_float(&p1)) {
		if (p1.val_float == 0.0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "zero_divisor");
		} else if (p1.val_float < 0.0) {
			return throw_error(q, &p1, q->st.curr_frame, "evaluation_error", "undefined");
		} else {
			q->accum.val_float = log10(p1.val_float);
			q->accum.tag = TAG_FLOAT;
		}
	} else {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "evaluable");
	}

	return true;
}

static pl_uint_t g_seed = 0;
#define random_M 0x7FFFFFFFL

static double rnd(void)
{
	g_seed = ((g_seed * 2743) + 5923) & random_M;
	return((double)g_seed / (double)random_M);
}

static bool fn_set_seed_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	g_seed = p1->val_int;
	return true;
}

static bool fn_get_seed_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, g_seed);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_random_between_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,integer_or_var);

	if (!is_smallint(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (!is_smallint(p2))
		return throw_error(q, p2, p2_ctx, "type_error", "integer");

	cell tmp;
	pl_int_t r = rnd() * ((int64_t)RAND_MAX+1);
	make_int(&tmp, get_smallint(p1) + (r % get_smallint(p2)));
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

static bool fn_random_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_float(&tmp, rnd());
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_random_integer_0(query *q)
{
	CHECK_CALC();
	q->accum.tag = TAG_INTEGER;
	q->accum.val_int = rnd() * ((int64_t)RAND_MAX+1);
	return true;
}

static bool fn_random_float_0(query *q)
{
	CHECK_CALC();
	q->accum.tag = TAG_FLOAT;
	q->accum.val_float = rnd();
	return true;
}

static bool fn_rand_0(query *q)
{
	CHECK_CALC();
	q->accum.tag = TAG_INTEGER;
	q->accum.val_int = rnd() * ((int64_t)RAND_MAX+1);
	return true;
}

static bool fn_rand_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_int(&tmp, rnd() * ((int64_t)RAND_MAX+1));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool fn_sys_set_prob_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	double p;

	if (is_float(p1))
		p = p1->val_float;
	else if (is_smallint(p1))
		p = p1->val_int;
	else if (is_structure(p1) && (p1->arity == 2) && !strcmp(C_STR(q, p1), "/")) {
		cell *c1 = p1+1;

		if (!is_smallint(c1))
			return throw_error(q, p1, p1_ctx, "type_error", "integer");

		cell *c2 = p1+2;

		if (!is_smallint(c2))
			return throw_error(q, p1, p1_ctx, "type_error", "integer");

		p = (double)c1->val_int / c2->val_int;
	} else
		return throw_error(q, p1, p1_ctx, "type_error", "number");

	if (p < 0.0)
		return throw_error(q, p1, p1_ctx, "domain_error", "not_less_than_zero");

	if (p > 1.0)
		return throw_error(q, p1, p1_ctx, "domain_error", "range_error");

	q->st.prob *= p;
	return true;
}

static bool fn_sys_get_prob_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	cell tmp;
	make_float(&tmp, q->st.prob);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	q->st.prob = 1.0;
	return ok;
}

static pl_int_t gcd(pl_int_t num, pl_int_t remainder)
{
	if (remainder == 0)
		return num;

	return gcd(remainder, num % remainder);
}

static bool fn_gcd_2(query *q)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1_tmp,any);
	GET_NEXT_ARG(p2_tmp,any);
	CLEANUP cell p1 = eval(q, p1_tmp);
	CLEANUP cell p2 = eval(q, p2_tmp);

	if (is_bigint(&p1) && is_bigint(&p2)) {
		mp_int_gcd(&p1.val_bigint->ival, &p2.val_bigint->ival, &q->tmp_ival);
		SET_ACCUM();
	} else if (is_bigint(&p1) && is_smallint(&p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2.val_int);
		mp_int_gcd(&p1.val_bigint->ival, &tmp, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_bigint(&p2) && is_smallint(&p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1.val_int);
		mp_int_gcd(&tmp, &p2.val_bigint->ival, &q->tmp_ival);
		mp_int_clear(&tmp);
		SET_ACCUM();
	} else if (is_smallint(&p1) && is_smallint(&p2)) {
		q->accum.val_int = gcd(p1.val_int, p2.val_int);
		q->accum.tag = TAG_INTEGER;
	} else if (is_var(&p1) || is_var(&p2)) {
		return throw_error(q, &p1, q->st.curr_frame, "instantiation_error", "not_sufficiently_instantiated");
	} else if (!is_integer(&p1)) {
		return throw_error(q, &p1, q->st.curr_frame, "type_error", "integer");
	} else if (!is_integer(&p2)) {
		return throw_error(q, &p2, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

static bool fn_divmod_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,var);

	if (is_bigint(p1) && is_bigint(p2)) {
		mpz_t tmp1, tmp2;
		mp_int_init(&tmp1);
		mp_int_init(&tmp2);
		mp_int_div(&p1->val_bigint->ival, &p2->val_bigint->ival, &tmp1, &tmp2);
		q->accum.tag = TAG_INTEGER;
		q->accum.flags = FLAG_MANAGED;
		q->accum.val_bigint = malloc(sizeof(bigint));
		q->accum.val_bigint->refcnt = 0;
		mp_int_init_copy(&q->accum.val_bigint->ival, &tmp1);
		mp_int_clear(&tmp1);
        unify(q, p3, p3_ctx, &q->accum, q->st.curr_frame);
		q->accum.tag = TAG_INTEGER;
		q->accum.flags = FLAG_MANAGED;
		q->accum.val_bigint = malloc(sizeof(bigint));
		q->accum.val_bigint->refcnt = 0;
		mp_int_init_copy(&q->accum.val_bigint->ival, &tmp2);
		mp_int_clear(&tmp2);
        unify(q, p4, p4_ctx, &q->accum, q->st.curr_frame);
	} else if (is_bigint(p1) && is_smallint(p2)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p2->val_int);
		mpz_t tmp1, tmp2;
		mp_int_init(&tmp1);
		mp_int_init(&tmp2);
		mp_int_div(&p1->val_bigint->ival, &tmp, &tmp1, &tmp2);
		mp_int_clear(&tmp);
		q->accum.tag = TAG_INTEGER;
		q->accum.flags = FLAG_MANAGED;
		q->accum.val_bigint = malloc(sizeof(bigint));
		q->accum.val_bigint->refcnt = 0;
		mp_int_init_copy(&q->accum.val_bigint->ival, &tmp1);
		mp_int_clear(&tmp1);
        unify(q, p3, p3_ctx, &q->accum, q->st.curr_frame);
		q->accum.tag = TAG_INTEGER;
		q->accum.flags = FLAG_MANAGED;
		q->accum.val_bigint = malloc(sizeof(bigint));
		q->accum.val_bigint->refcnt = 0;
		mp_int_init_copy(&q->accum.val_bigint->ival, &tmp2);
		mp_int_clear(&tmp2);
        unify(q, p4, p4_ctx, &q->accum, q->st.curr_frame);
	} else if (is_bigint(p2) && is_smallint(p1)) {
		mpz_t tmp;
		mp_int_init_value(&tmp, p1->val_int);
		mpz_t tmp1, tmp2;
		mp_int_init(&tmp1);
		mp_int_init(&tmp2);
		mp_int_div(&tmp, &p2->val_bigint->ival, &tmp1, &tmp2);
		mp_int_clear(&tmp);
		q->accum.tag = TAG_INTEGER;
		q->accum.flags = FLAG_MANAGED;
		q->accum.val_bigint = malloc(sizeof(bigint));
		q->accum.val_bigint->refcnt = 0;
		mp_int_init_copy(&q->accum.val_bigint->ival, &tmp1);
		mp_int_clear(&tmp1);
        unify(q, p3, p3_ctx, &q->accum, q->st.curr_frame);
		q->accum.tag = TAG_INTEGER;
		q->accum.flags = FLAG_MANAGED;
		q->accum.val_bigint = malloc(sizeof(bigint));
		q->accum.val_bigint->refcnt = 0;
		mp_int_init_copy(&q->accum.val_bigint->ival, &tmp2);
		mp_int_clear(&tmp2);
        unify(q, p4, p4_ctx, &q->accum, q->st.curr_frame);
	} else if (is_smallint(p1) && is_smallint(p2)) {
		if (p2->val_int == 0)
			return throw_error(q, p2, q->st.curr_frame, "evaluation_error", "zero_divisor");

		cell tmp;
        q->accum.val_int = p1->val_int / p2->val_int;
        make_int(&tmp, q->accum.val_int);
        unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
        q->accum.val_int = p1->val_int % p2->val_int;
        make_int(&tmp, q->accum.val_int);
        unify(q, p4, p4_ctx, &tmp, q->st.curr_frame);
	} else {
		return throw_error(q, p1, q->st.curr_frame, "type_error", "integer");
	}

	return true;
}

builtins g_evaluable_bifs[] =
{
	// Predicate...

	{"=:=", 2, fn_iso_neq_2, "+number,+number", true, false, BLAH},
	{"=\\=", 2, fn_iso_nne_2, "+number,+number", true, false, BLAH},
	{">", 2, fn_iso_ngt_2, "+number,+number", true, false, BLAH},
	{">=", 2, fn_iso_nge_2, "+number,+number", true, false, BLAH},
	{"=<", 2, fn_iso_nle_2, "+number,+number", true, false, BLAH},
	{"<", 2, fn_iso_nlt_2, "+number,+number", true, false, BLAH},

	{"==", 2, fn_iso_seq_2, "+term,+term", true, false, BLAH},
	{"\\==", 2, fn_iso_sne_2, "+term,+term", true, false, BLAH},
	{"@>", 2, fn_iso_sgt_2, "+term,+term", true, false, BLAH},
	{"@>=", 2, fn_iso_sge_2, "+term,+term", true, false, BLAH},
	{"@=<", 2, fn_iso_sle_2, "+term,+term", true, false, BLAH},
	{"@<", 2, fn_iso_slt_2, "+term,+term", true, false, BLAH},

	{"is", 2, fn_iso_is_2, "?number,+number", true, false, BLAH},
	{"float", 1, fn_iso_float_1, "+number", true, false, BLAH},
	{"integer", 1, fn_iso_integer_1, "+number", true, false, BLAH},
	{"setrand", 1, fn_set_seed_1, "+integer", false, false, BLAH},
	{"srandom", 1, fn_set_seed_1, "+integer", false, false, BLAH},
	{"set_seed", 1, fn_set_seed_1, "+integer", false, false, BLAH},
	{"get_seed", 1, fn_get_seed_1, "-integer", false, false, BLAH},
	{"rand", 1, fn_rand_1, "?integer", false, false, BLAH},
	{"random", 1, fn_random_1, "?integer", false, false, BLAH},
	{"random_between", 3, fn_random_between_3, "?integer,?integer,-integer", false, false, BLAH},

	{"$set_prob", 1, fn_sys_set_prob_1, "+real", false, false, BLAH},
	{"$get_prob", 1, fn_sys_get_prob_1, "-real", false, false, BLAH},

	// Functions...

	{"return", 1, fn_return_1, "+number,-number", false, true, BLAH},
	{"+", 1, fn_iso_positive_1, "+number,-number", true, true, BLAH},
	{"-", 1, fn_iso_negative_1, "+number,-number", true, true, BLAH},
	{"abs", 1, fn_iso_abs_1, "+number,-number", true, true, BLAH},
	{"sign", 1, fn_iso_sign_1, "+number,-integer", true, true, BLAH},
	{"epsilon", 0, fn_iso_epsilon_0, "-float", true, true, BLAH},
	{"pi", 0, fn_iso_pi_0, "-float", true, true, BLAH},
	{"e", 0, fn_iso_e_0, "-float", true, true, BLAH},
	{"+", 2, fn_iso_add_2, "+number,+number,-number", true, true, BLAH},
	{"-", 2, fn_iso_sub_2, "+number,+number,-number", true, true, BLAH},
	{"*", 2, fn_iso_mul_2, "+number,+number,-float", true, true, BLAH},
	{"/", 2, fn_iso_divide_2, "+number,+number,-float", true, true, BLAH},
	{"//", 2, fn_iso_divint_2, "+integer,+integer,-integer", true, true, BLAH},
	{"div", 2, fn_iso_div_2, "+integer,+integer,-integer", true, true, BLAH},
	{"mod", 2, fn_iso_mod_2, "+integer,+integer,-integer", true, true, BLAH},
	{"rem", 2, fn_iso_rem_2, "+integer,+integer,-integer", true, true, BLAH},
	{"max", 2, fn_iso_max_2, "+number,+number,-number", true, true, BLAH},
	{"min", 2, fn_iso_min_2, "+number,+number,-number", true, true, BLAH},
	{"xor", 2, fn_iso_xor_2, "+integer,+integer,-integer", true, true, BLAH},
	{"/\\", 2, fn_iso_and_2, "+integer,+integer,-integer", true, true, BLAH},
	{"\\/", 2, fn_iso_or_2, "+integer,+integer,-integer", true, true, BLAH},
	{"<<", 2, fn_iso_shl_2, "+integer,-integer", true, true, BLAH},
	{">>", 2, fn_iso_shr_2, "+integer,-integer", true, true, BLAH},
	{"\\", 1, fn_iso_neg_1, "+integer,-integer", true, true, BLAH},
	{"**", 2, fn_iso_pow_2, "+number,+number,-float", true, true, BLAH},
	{"^", 2, fn_iso_powi_2, "+number,+number,-integer", true, true, BLAH},
	{"exp", 1, fn_iso_exp_1, "+number,-float", true, true, BLAH},
	{"sqrt", 1, fn_iso_sqrt_1, "+number,-float", true, true, BLAH},
	{"log", 1, fn_iso_log_1, "+number,-float", true, true, BLAH},

	{"sin", 1, fn_iso_sin_1, "+number,-float", true, true, BLAH},
	{"cos", 1, fn_iso_cos_1, "+number,-float", true, true, BLAH},
	{"tan", 1, fn_iso_tan_1, "+number,-float", true, true, BLAH},
	{"asin", 1, fn_iso_asin_1, "+number,-float", true, true, BLAH},
	{"acos", 1, fn_iso_acos_1, "+number,-float", true, true, BLAH},
	{"atan", 1, fn_iso_atan_1, "+number,-float", true, true, BLAH},

	{"sinh", 1, fn_sinh_1, "+number,-float", false, true, BLAH},
	{"cosh", 1, fn_cosh_1, "+number,-float", false, true, BLAH},
	{"tanh", 1, fn_tanh_1, "+number,-float", false, true, BLAH},
	{"asinh", 1, fn_asinh_1, "+number,-float", false, true, BLAH},
	{"acosh", 1, fn_acosh_1, "+number,-float", false, true, BLAH},
	{"atanh", 1, fn_atanh_1, "+number,-float", false, true, BLAH},

	{"atan2", 2, fn_iso_atan2_2, "+number,+number,-float", true, true, BLAH},
	{"copysign", 2, fn_iso_copysign_2, "+number,-number", true, true, BLAH},
	{"truncate", 1, fn_iso_truncate_1, "+float,-integer", true, true, BLAH},
	{"round", 1, fn_iso_round_1, "+float,-integer", true, true, BLAH},
	{"ceiling", 1, fn_iso_ceiling_1, "+float,-integer", true, true, BLAH},
	{"floor", 1, fn_iso_floor_1, "+float,-integer", true, true, BLAH},
	{"float_integer_part", 1, fn_iso_float_integer_part_1, "+float,-integer", true, true, BLAH},
	{"float_fractional_part", 1, fn_iso_float_fractional_part_1, "+float,-float", true, true, BLAH},

	{"divmod", 4, fn_divmod_4, "+integer,+integer,-integer,-integer", false, false, BLAH},
	{"log", 2, fn_log_2, "+number,+number,-float", false, true, BLAH},
	{"log10", 1, fn_log10_1, "+number,-float", false, true, BLAH},
	{"random_integer", 0, fn_random_integer_0, "-integer", false, true, BLAH},
	{"random_float", 0, fn_random_float_0, "-float", false, true, BLAH},
	{"rand", 0, fn_rand_0, "-integer", false, true, BLAH},
	{"gcd", 2, fn_gcd_2, "+integer,+integer,-integer", false, true, BLAH},
	{"popcount", 1, fn_popcount_1, "+integer,-integer", false, true, BLAH},
	{"lsb", 1, fn_lsb_1, "+integer,-integer", false, true, BLAH},
	{"msb", 1, fn_msb_1, "+integer,-integer", false, true, BLAH},

	{0}
};
