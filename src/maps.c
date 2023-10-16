#include <stdlib.h>
#include <stdio.h>
#include <float.h>

#include "heap.h"
#include "module.h"
#include "prolog.h"
#include "query.h"

#ifndef DBL_DECIMAL_DIG
#define DBL_DECIMAL_DIG DBL_DIG
#endif

bool fn_map_create_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	int n = new_stream(q->pl);
	GET_NEXT_ARG(p4,list_or_nil);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STR_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			sl_set(str->alias, DUP_STR(q, name), NULL);
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_var(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	str->keyval = sl_create((void*)fake_strcmp, (void*)keyvalfree, NULL);
	check_heap_error(str->keyval);
	str->is_map = true;

	cell tmp ;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

bool fn_map_set_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,atomic);
	GET_NEXT_ARG(p2,atomic);
	char *key;

	if (is_integer(p1)) {
		char tmpbuf[128];
		snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long unsigned)get_smallint(p1));
		key = strdup(tmpbuf);
	} else if (is_atom(p1))
		key = DUP_STR(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	check_heap_error(key);
	char *val = NULL;

	if (is_integer(p2)) {
		char tmpbuf[128];
		snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long unsigned)get_smallint(p2));
		val = strdup(tmpbuf);
	} else if (is_float(p2)) {
		char tmpbuf[128];
		snprintf(tmpbuf, sizeof(tmpbuf), "%.*lg", DBL_DECIMAL_DIG, get_float(p2));
		val = strdup(tmpbuf);
	} else if (is_atom(p2))
		val = DUP_STR(q, p2);
	else {
		free(key);
		return throw_error(q, p2, p2_ctx, "type_error", "integer");
	}

	check_heap_error(val);
	sl_set(str->keyval, key, val);
	return true;
}

bool fn_map_get_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,atomic);
	GET_NEXT_ARG(p2,atomic_or_var);
	char *key;
	char tmpbuf[128];

	if (is_integer(p1)) {
		snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long unsigned)get_smallint(p1));
		key = tmpbuf;
	} else if (is_atom(p1))
		key = DUP_STR(q, p1);
	else
		return throw_error(q, p2, p2_ctx, "type_error", "integer");

	check_heap_error(key);
	char *val = NULL;

	if (!sl_get(str->keyval, key, (void*)&val)) {
		if (key != tmpbuf) free(key);
		return false;
	}

	cell tmp;
	const char *src = val;
	int all_digs = 1, floaties = 0;

	if (*src == '-')
		src++;

	while (*src) {
		if ((*src == '.') || (*src == 'e') || (*src == 'E')
			|| (*src == '+') || (*src == '-'))
			floaties++;
		else if (!isdigit(*src)) {
			all_digs = 0;
			break;
		}

		src++;
	}

	if (all_digs && !floaties) {
		pl_int v = strtoll(val, NULL, 10);
		make_int(&tmp, v);
	} else if (all_digs && floaties) {
		pl_flt v = strtod(val, NULL);
		make_float(&tmp, v);
	} else
		make_cstring(&tmp, val);

	if (key != tmpbuf) free(key);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

bool fn_map_del_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,atomic);
	char *key;

	if (is_integer(p1)) {
		char tmpbuf[128];
		snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long unsigned)get_smallint(p1));
		key = strdup(tmpbuf);
	} else if (is_atom(p1))
		key = DUP_STR(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	check_heap_error(key);
	sl_del(str->keyval, key);
	return true;
}

bool fn_map_list_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,list_or_var);
	sliter *iter = sl_first(str->keyval);
	char *val = NULL;
	check_heap_error(init_tmp_heap(q));

	while (sl_next(iter, (void**)&val)) {
		void *key = sl_key(iter);
		cell tmpk, tmpv;
		const char *src = key;
		int all_digs = 1;

		while (*src) {
			if (!isdigit(*src)) {
				all_digs = 0;
				break;
			}

			src++;
		}

		if (all_digs) {
			pl_int v = strtoll(key, NULL, 10);
			make_int(&tmpk, v);
		} else
			make_cstring(&tmpk, key);

		src = val;
		src = val;
		all_digs = 1; int floaties = 0;

		if (*src == '-')
			src++;

		while (*src) {
			if ((*src == '.') || (*src == 'e') || (*src == 'E')
				|| (*src == '+') || (*src == '-'))
				floaties++;
			else if (!isdigit(*src)) {
				all_digs = 0;
				break;
			}

			src++;
		}

		if (all_digs && !floaties) {
			pl_int v = strtoll(val, NULL, 10);
			make_int(&tmpv, v);
		} else if (all_digs && floaties) {
			pl_flt v = strtod(val, NULL);
			make_float(&tmpv, v);
		} else
			make_cstring(&tmpv, val);

		cell tmp2[3];
		make_struct(tmp2+0, g_colon_s, NULL, 2, 2);
		tmp2[1] = tmpk;
		tmp2[2] = tmpv;
		SET_OP(tmp2, OP_YFX);
		append_list(q, tmp2);
	}

	cell *tmp = end_list(q);
	sl_done(iter);
	return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
}

bool fn_map_count_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,var);
	cell tmp;
	make_int(&tmp, sl_count(str->keyval));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

bool fn_map_close_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	return fn_iso_close_1(q);
}

typedef union {
	struct { int32_t row; int32_t col; };
	uint64_t k;
} mat_key;

bool fn_mat_create_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	int n = new_stream(q->pl);
	GET_NEXT_ARG(p4,list_or_nil);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	LIST_HANDLER(p4);
	unsigned identity = 0;
	str->is_integer = true;
	str->is_sparse = true;

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STR_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			sl_set(str->alias, DUP_STR(q, name), NULL);
		} else if (!CMP_STR_TO_CSTR(q, c, "integer")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (!strcmp(C_STR(q, name), "true"))
				str->is_integer = true;
			else
				str->is_integer = false;
		} else if (!CMP_STR_TO_CSTR(q, c, "sparse")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (!strcmp(C_STR(q, name), "true"))
				str->is_sparse = true;
			else
				str->is_sparse = false;
		} else if (!CMP_STR_TO_CSTR(q, c, "double")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (!strcmp(C_STR(q, name), "true"))
				str->is_integer = false;
			else
				str->is_integer = true;
		} else if (!CMP_STR_TO_CSTR(q, c, "identity")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_smallint(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			str->rows = str->cols = identity = get_smallint(name);
		} else if (!CMP_STR_TO_CSTR(q, c, "rows")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_smallint(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			str->rows = get_smallint(name);

			if (str->cols == 0)
				str->cols = str->rows;
		} else if (!CMP_STR_TO_CSTR(q, c, "cols")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_smallint(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			str->cols = get_smallint(name);

			if (str->rows == 0)
				str->rows = str->cols;
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_var(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	str->keyval = sl_create(NULL, NULL, NULL);
	check_heap_error(str->keyval);
	str->is_map = true;

	if (!str->is_sparse) {
		for (unsigned i = 0; i < str->rows; i++) {
			for (unsigned j = 0; j < str->cols; j++) {
				union {
					int64_t vi;
					double vf;
					void *v;
				} val;

				mat_key key;
				key.row = i;
				key.col = j;

				if (str->is_integer)
					val.vi = 1;
				 else
					val.vf = 1.0;

				sl_set(str->keyval, (void*)key.k, val.v);
			}
		}
	}

	if (identity) {
		union {
			int64_t vi;
			double vf;
			void *v;
		} val;

		for (unsigned i = 0; i < identity; i++) {
			mat_key key;
			key.row = i;
			key.col = i;

			if (str->is_integer)
				val.vi = 1;
			else
				val.vf = 1.0;

			sl_set(str->keyval, (void*)key.k, val.v);
		}
	}

	cell tmp ;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

bool fn_mat_set_4(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,number);
	mat_key key;
	key.row = get_smallint(p1);
	key.col = get_smallint(p2);
	union {
		int64_t vi;
		double vf;
		void *v;
	} val;

	if (str->is_integer) {
		int64_t v = is_integer(p3) ? get_smallint(p3) : get_float(p3);
		val.vi = v;
	} else {
		double v = is_integer(p3) ? get_smallint(p3) : get_float(p3);
		val.vf = v;
	}

	sl_set(str->keyval, (void*)key.k, val.v);
	return true;
}

bool fn_mat_get_4(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,number);
	GET_NEXT_ARG(p2,number);
	GET_NEXT_ARG(p3,number_or_var);
	mat_key key;
	key.row = get_smallint(p1);
	key.col = get_smallint(p2);
	union {
		int64_t vi;
		double vf;
	} val;

	if (!sl_get(str->keyval, (void*)key.k, (void*)&val)) {
		if (str->is_integer)
			val.vi = 0;
		else
			val.vf = 0.0;
	}

	cell tmp;

	if (str->is_integer) {
		int64_t v = val.vi;
		make_int(&tmp, v);
	} else {
		double v = val.vf;
		make_float(&tmp, v);
	}

	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

bool fn_mat_del_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,number);
	GET_NEXT_ARG(p2,number);
	mat_key key;
	key.row = get_smallint(p1);
	key.col = get_smallint(p2);
	sl_del(str->keyval, (void*)key.k);
	return true;
}

bool fn_mat_list_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,var);
	cell tmp1[2];
	make_struct(tmp1, new_atom(q->pl, "rows"), NULL, 1, 1);
	make_int(tmp1+1, str->rows);
	allocate_list(q, tmp1);
	make_struct(tmp1, new_atom(q->pl, "cols"), NULL, 1, 1);
	make_int(tmp1+1, str->cols);
	append_list(q, tmp1);
	make_struct(tmp1, new_atom(q->pl, "sparse"), NULL, 1, 1);
	make_atom(tmp1+1, str->is_sparse?g_true_s:g_false_s);
	append_list(q, tmp1);
	make_struct(tmp1, new_atom(q->pl, "integer"), NULL, 1, 1);
	make_atom(tmp1+1, str->is_integer?g_true_s:g_false_s);
	append_list(q, tmp1);
	cell *tmp = end_list(q);

	if (!unify(q, p1, p1_ctx, tmp, q->st.curr_frame))
		return false;

	GET_NEXT_ARG(p2,var);
	sliter *iter = sl_first(str->keyval);
	union {
		int64_t vi;
		double vf;
	} val;

	check_heap_error(init_tmp_heap(q));

	while (sl_next(iter, (void**)&val)) {
		void *key = sl_key(iter);
		cell tmpv;

		if (str->is_integer) {
			int64_t v = val.vi;
			make_int(&tmpv, v);
		} else {
			double v = val.vf;
			make_float(&tmpv, v);
		}

		if (is_zero(&tmpv))
			continue;

		mat_key tmpk;
		memcpy(&tmpk, &key, sizeof(size_t));
		cell tmp2[5];
		make_struct(tmp2+0, g_colon_s, NULL, 2, 4);
		make_struct(tmp2+1, g_conjunction_s, NULL, 2, 2);
		make_int(tmp2+2, tmpk.row);
		make_int(tmp2+3, tmpk.col);
		tmp2[4] = tmpv;
		SET_OP(tmp2+0, OP_YFX);
		SET_OP(tmp2+1, OP_XFY);
		append_list(q, tmp2);
	}

	tmp = end_list(q);
	sl_done(iter);
	return unify(q, p2, p2_ctx, tmp, q->st.curr_frame);
}

bool fn_mat_max_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,number);
	GET_NEXT_ARG(p2,var);
	sliter *iter = sl_first(str->keyval);
	bool first = true;
	union {
		int64_t vi;
		double vf;
	} val;

	struct {
		int64_t vi;
		double vf;
	} max = {INT64_MIN, DBL_MIN};

	while (sl_next(iter, (void**)&val)) {
		if (str->is_integer) {
			int64_t v = val.vi;

			if (v > max.vi)
				max.vi = v;
		} else {
			double v = val.vf;

			if (v > max.vf)
				max.vf = v;
		}
	}

	cell tmp;

	if (str->is_integer)
		make_int(&tmp, max.vi);
	else
		make_float(&tmp, max.vf);

	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

bool fn_mat_min_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,number);
	GET_NEXT_ARG(p2,var);
	sliter *iter = sl_first(str->keyval);
	bool first = true;
	union {
		int64_t vi;
		double vf;
	} val;

	struct {
		int64_t vi;
		double vf;
	} min = {INT64_MAX, DBL_MAX};

	while (sl_next(iter, (void**)&val)) {
		if (str->is_integer) {
			int64_t v = val.vi;

			if (v < min.vi)
				min.vi = v;
		} else {
			double v = val.vf;

			if (v < min.vf)
				min.vf = v;
		}
	}

	cell tmp;

	if (str->is_integer)
		make_int(&tmp, min.vi);
	else
		make_float(&tmp, min.vf);

	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

bool fn_mat_avg_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,number);
	GET_NEXT_ARG(p2,var);
	sliter *iter = sl_first(str->keyval);
	bool first = true;
	unsigned cnt = 0;

	union {
		int64_t vi;
		double vf;
	} val;

	struct {
		int64_t vi;
		double vf;
	} avg = {0, 0.0};

	while (sl_next(iter, (void**)&val)) {
		if (str->is_integer) {
			int64_t v = val.vi;
			avg.vi += v;
		} else {
			double v = val.vf;
			avg.vf += v;
		}

		cnt++;
	}

	cell tmp;

	if (str->is_integer)
		make_int(&tmp, avg.vi/cnt);
	else
		make_float(&tmp, avg.vf/cnt);

	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

bool fn_mat_mult_scal_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,number);
	sliter *iter = sl_first(str->keyval);
	bool first = true;
	union {
		int64_t *vi;
		double *vf;
	} val;

	while (sl_next_mutable(iter, (void**)&val)) {
		if (str->is_integer) {
			int64_t v = *(val.vi);

			if (is_smallint(p1)) {
				v *= get_smallint(p1);
				*(val.vi) = v;
			} else {
				v *= get_float(p1);
				*(val.vi) = v;
			}
		} else {
			double v = *(val.vf);

			if (is_smallint(p1)) {
				v *= get_smallint(p1);
				*(val.vf) = v;
			} else {
				v *= get_float(p1);
				*(val.vf) = v;
			}
		}
	}

	return true;
}

bool fn_mat_div_scal_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,number);
	sliter *iter = sl_first(str->keyval);
	bool first = true;
	union {
		int64_t *vi;
		double *vf;
	} val;

	while (sl_next_mutable(iter, (void**)&val)) {
		if (str->is_integer) {
			int64_t v = *(val.vi);

			if (is_smallint(p1)) {
				v /= get_smallint(p1);
				*(val.vi) = v;
			} else {
				v /= get_float(p1);
				*(val.vi) = v;
			}
		} else {
			double v = *(val.vf);

			if (is_smallint(p1)) {
				v /= get_smallint(p1);
				*(val.vf) = v;
			} else {
				v /= get_float(p1);
				*(val.vf) = v;
			}
		}
	}

	return true;
}
