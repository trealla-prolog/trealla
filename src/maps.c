#include <stdlib.h>
#include <stdio.h>
#include <float.h>

#include "heap.h"
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
