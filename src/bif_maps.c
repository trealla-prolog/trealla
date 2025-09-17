#include <stdlib.h>
#include <stdio.h>
#include <float.h>

#include "prolog.h"
#include "query.h"

#ifndef DBL_DECIMAL_DIG
#define DBL_DECIMAL_DIG DBL_DIG
#endif

static bool bif_map_create_2(query *q)
{
	GET_FIRST_ARG(p1,var);
	int n = new_stream(q->pl);
	GET_NEXT_ARG(p4,list_or_nil);

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	bool is_alias = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			sl_app(str->alias, DUP_STRING(q, name), NULL);
			cell tmp;
			make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

			if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
				return false;

			is_alias = true;
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_var(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	str->keyval = sl_create((void*)fake_strcmp, (void*)fake_free, NULL);
	checked(str->keyval);
	str->is_map = true;

	if (!is_alias) {
		cell tmp ;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_MAP;

		if (!unify(q, p1, p1_ctx, &tmp, q->st.curr_frame))
			return false;
	}

	return true;
}

static bool bif_map_set_3(query *q)
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
		key = DUP_STRING(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	checked(key);
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
		val = DUP_STRING(q, p2);
	else {
		free(key);
		return throw_error(q, p2, p2_ctx, "type_error", "integer");
	}

	checked(val);
	sl_app(str->keyval, key, val);
	return true;
}

static bool bif_map_get_3(query *q)
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
		key = DUP_STRING(q, p1);
	else
		return throw_error(q, p2, p2_ctx, "type_error", "integer");

	checked(key);
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

static bool bif_map_del_2(query *q)
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
		key = DUP_STRING(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	checked(key);
	sl_del(str->keyval, key);
	return true;
}

static bool bif_map_list_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	GET_NEXT_ARG(p1,list_or_var);
	sliter *iter = sl_first(str->keyval);
	char *val = NULL;
	checked(init_tmp_heap(q));

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
		make_instr(tmp2+0, g_colon_s, NULL, 2, 2);
		tmp2[1] = tmpk;
		tmp2[2] = tmpv;
		SET_OP(tmp2, OP_YFX);
		append_list(q, tmp2);
	}

	cell *tmp = end_list(q);
	sl_done(iter);
	return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
}

static bool bif_map_count_2(query *q)
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

static bool bif_map_close_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_map)
		return throw_error(q, pstr, pstr_ctx, "type_error", "not_a_map");

	return bif_iso_close_1(q);
}

static bool bif_engine_create_4(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,callable);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,list_or_nil);

	int n = new_stream(q->pl);

	if (n < 0)
		return throw_error(q, q->st.instr, q->st.curr_frame, "resource_error", "too_many_streams");

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	bool is_alias = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			sl_app(str->alias, DUP_STRING(q, name), NULL);
			cell tmp;
			make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

			if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
				return false;

			is_alias = true;
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_var(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	if (is_atom(p3)) {
		if (get_named_stream(q->pl, C_STR(q, p3), C_STRLEN(q, p3)) >= 0)
			return throw_error(q, q->st.instr, q->st.curr_frame, "permission_error", "open,source_sink");

		sl_app(str->alias, DUP_STRING(q, p3), NULL);
	} else if (!is_alias) {
		cell tmp2;
		make_int(&tmp2, n);
		tmp2.flags |= FLAG_INT_STREAM | FLAG_INT_MAP;
		unify(q, p3, p3_ctx, &tmp2, q->st.curr_frame);
	}

	str->first_time = str->is_engine = true;
	str->curr_yield = NULL;

	str->engine = query_create(q->st.m);
	str->engine->curr_engine = n;
	str->engine->is_engine = true;
	str->engine->trace = q->trace;

	cell *p0 = copy_term_to_heap(q, q->st.instr, q->st.curr_frame, false);
	unify(q, q->st.instr, q->st.curr_frame, p0, q->st.curr_frame);
	checked(p0);

	q = str->engine;		// Operating in engine now

	GET_FIRST_ARG0(xp1,any,p0);
	GET_NEXT_ARG(xp2,callable);

	cell *tmp = prepare_call(q, CALL_NOSKIP, xp2, xp2_ctx, 1);
	pl_idx num_cells = xp2->num_cells;
	make_call(q, tmp+num_cells);
	checked(push_barrier(q));
	q->st.instr = tmp;
	str->pattern = clone_term_to_heap(q, xp1, xp1_ctx);
	return true;
}

static bool bif_engine_next_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_engine)
		return throw_error(q, pstr, pstr_ctx, "existence_error", "not_an_engine");

	bool was_first_time = str->first_time;

	if (str->first_time) {
		str->first_time = false;
		execute(str->engine, str->engine->st.instr, MAX_ARITY);
	}

	if (str->curr_yield) {
		cell *tmp = copy_term_to_heap(q, str->curr_yield, 0, false);
		str->curr_yield = NULL;
		return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	}

	if (!was_first_time) {
		if (!query_redo(str->engine))
			return false;
	}

	cell *tmp = copy_term_to_heap(str->engine, str->pattern, 0, false);
	return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
}

static bool bif_engine_yield_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!q->is_engine)
		return throw_error(q, q->st.instr, q->st.curr_frame, "permission_error", "not_an_engine");

	stream *str = &q->pl->streams[q->curr_engine];

	if (q->retry && str->curr_yield)
		return do_yield(q, 0);
	else if (q->retry)
		return true;

	str->curr_yield = clone_term_to_heap(q, p1, p1_ctx);
	return do_yield(q, 0);
}

static bool bif_engine_post_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_engine)
		return throw_error(q, pstr, pstr_ctx, "existence_error", "not_an_engine");

	str->curr_yield = clone_term_to_heap(q, p1, p1_ctx);
	return true;
}

static bool bif_engine_fetch_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!q->is_engine)
		return throw_error(q, q->st.instr, q->st.curr_frame, "existence_error", "not_an_engine");

	stream *str = &q->pl->streams[q->curr_engine];

	if (!str->curr_yield)
		return throw_error(q, q->st.instr, q->st.curr_frame, "existence_error", "no_data");

	cell *tmp = copy_term_to_heap(q, str->curr_yield, 0, false);
	str->curr_yield = NULL;
	return unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
}

static bool bif_engine_self_1(query *q)
{
	GET_FIRST_ARG(p1,any);

	if (!q->is_engine)
		return false;

	cell tmp2;
	make_int(&tmp2, q->curr_engine);
	tmp2.flags |= FLAG_INT_STREAM | FLAG_INT_MAP;
	return unify(q, p1, p1_ctx, &tmp2, q->st.curr_frame);
}

static bool bif_is_engine_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = get_stream(q, p1);

	if (n < 0)
		return false;

	stream *str = &q->pl->streams[n];
	return str->is_engine;
}

static bool bif_engine_destroy_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!str->is_engine)
		return throw_error(q, pstr, pstr_ctx, "existence_error", "not_an_engine");

	return bif_iso_close_1(q);
}

builtins g_maps_bifs[] =
{
	{"map_create", 2, bif_map_create_2, "--stream,+list", false, false, BLAH},
	{"map_set", 3, bif_map_set_3, "+stream,+atomic,+atomic", false, false, BLAH},
	{"map_get", 3, bif_map_get_3, "+stream,+atomic,-atomic", false, false, BLAH},
	{"map_del", 2, bif_map_del_2, "+stream,+atomic", false, false, BLAH},
	{"map_list", 2, bif_map_list_2, "+stream,-list", false, false, BLAH},
	{"map_count", 2, bif_map_count_2, "+stream,-integer", false, false, BLAH},
	{"map_close", 1, bif_map_close_1, "+stream", false, false, BLAH},

	{"engine_create", 4, bif_engine_create_4, "+term,:callable,?stream,+list", false, false, BLAH},
	{"engine_next", 2, bif_engine_next_2, "+stream,-term", false, false, BLAH},
	{"is_engine", 1, bif_is_engine_1, "+term", false, false, BLAH},
	{"engine_self", 1, bif_engine_self_1, "--stream", false, false, BLAH},
	{"engine_yield", 1, bif_engine_yield_1, "+term", false, false, BLAH},
	{"engine_post", 2, bif_engine_post_2, "+stream,+term", false, false, BLAH},
	{"engine_fetch", 1, bif_engine_fetch_1, "-term", false, false, BLAH},
	{"engine_destroy", 1, bif_engine_destroy_1, "+stream", false, false, BLAH},

	{0}
};
