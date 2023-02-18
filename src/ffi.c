#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if USE_FFI
#include <dlfcn.h>
#include <ffi.h>
#endif

#include "prolog.h"
#include "module.h"
#include "query.h"

// These are pseudo tags just used here...

enum {
	TAG_INT8=TAG_END+1,
	TAG_INT16,
	TAG_INT32,
	TAG_INT64,
	TAG_UINT8,
	TAG_UINT16,
	TAG_UINT32,
	TAG_UINT64,
	TAG_USHORT,
	TAG_SHORT,
	TAG_UINT,
	TAG_INT,
	TAG_ULONG,
	TAG_LONG,
	TAG_FLOAT32,
	TAG_CCSTR,
	TAG_STRUCT
};

#define MARK_OUT(t) (((unsigned)(t) << 2) | 1)

union result_ {
	float f32;
	double f64;
	uint8_t u8;
	uint16_t u16;
	uint32_t u32;
	uint64_t u64;
	int8_t i8;
	int16_t i16;
	int32_t i32;
	int64_t i64;
	unsigned short ushort;
	signed short sshort;
	unsigned int uint;
	signed int sint;
	unsigned long ulong;
	signed long slong;
	char *s;
	void *p;
};

#if USE_FFI
void *do_dlopen(const char *filename, int flag)
{
#if __APPLE__
	char *filename2 = malloc((strlen(filename)-2)+5+1);
	const char *ptr = strstr(filename, ".so");

	if (ptr) {
		const char *src = filename;
		char *dst = filename2;

		while (src != ptr)
			*dst++ = *src++;

		strcpy(dst, ".dylib");
		dst += strlen(".dylib");
		src += strlen(".so");

		while (*src)
			*dst++ = *src++;
	}
#else
	char *filename2 = strdup(filename);
#endif

	void *handle = dlopen(filename2, !flag ? RTLD_LAZY | RTLD_GLOBAL : flag);
	free(filename2);
	return handle;
}

USE_RESULT bool fn_sys_dlopen_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,integer);
	GET_NEXT_ARG(p3,var);
	void *handle = do_dlopen(C_STR(q, p1), get_smallint(p2));
	if (!handle) return false;
	cell tmp;
	make_uint(&tmp, (pl_int_t)(size_t)handle);
	tmp.flags |= FLAG_INT_HANDLE | FLAG_HANDLE_DLL;
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

USE_RESULT bool fn_sys_dlsym_3(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,var);
	size_t handle = get_smalluint(p1);
	const char *symbol = C_STR(q, p2);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	void *ptr = dlsym((void*)handle, symbol);
	if (!ptr) return false;
	cell tmp;
	make_uint(&tmp, (pl_int_t)(size_t)ptr);
	tmp.flags |= FLAG_INT_HANDLE | FLAG_INT_OCTAL;
	return unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
}

int do_dlclose(void *handle)
{
	return dlclose(handle);
}

USE_RESULT bool fn_sys_dlclose_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	size_t handle = get_smalluint(p1);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	return do_dlclose((void*)handle) ? false : true;
}

USE_RESULT bool fn_sys_register_function_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,iso_list);
	GET_NEXT_ARG(p4,atom);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	size_t handle = get_smalluint(p1);
	const char *symbol = C_STR(q, p2);
	void *func = dlsym((void*)handle, symbol);
	if (!func) return false;

	uint8_t arg_types[MAX_ARITY], ret_type = 0;
	LIST_HANDLER(l);
	cell *l = p3;
	pl_idx_t l_ctx = p3_ctx;
	int idx = 0;

	while (is_iso_list(l) && (idx < MAX_ARITY)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);

		if (is_interned(h)) {
			const char *src = C_STR(q, h);

			if (!strcmp(src, "uint8"))
				arg_types[idx++] = TAG_UINT8;
			else if (!strcmp(src, "uint16"))
				arg_types[idx++] = TAG_UINT16;
			else if (!strcmp(src, "uint32"))
				arg_types[idx++] = TAG_UINT32;
			else if (!strcmp(src, "uint64"))
				arg_types[idx++] = TAG_UINT64;
			else if (!strcmp(src, "uint"))
				arg_types[idx++] = TAG_UINT;
			else if (!strcmp(src, "sint8"))
				arg_types[idx++] = TAG_INT8;
			else if (!strcmp(src, "sint16"))
				arg_types[idx++] = TAG_INT16;
			else if (!strcmp(src, "sint32"))
				arg_types[idx++] = TAG_INT32;
			else if (!strcmp(src, "sint64"))
				arg_types[idx++] = TAG_INT64;
			else if (!strcmp(src, "sint"))
				arg_types[idx++] = TAG_INT;
			else if (!strcmp(src, "ushort"))
				arg_types[idx++] = TAG_USHORT;
			else if (!strcmp(src, "sshort"))
				arg_types[idx++] = TAG_SHORT;
			else if (!strcmp(src, "ulong"))
				arg_types[idx++] = TAG_ULONG;
			else if (!strcmp(src, "slong"))
				arg_types[idx++] = TAG_LONG;
			else if (!strcmp(src, "float"))
				arg_types[idx++] = TAG_FLOAT32;
			else if (!strcmp(src, "double"))
				arg_types[idx++] = TAG_FLOAT;
			else if (!strcmp(src, "ptr"))
				arg_types[idx++] = TAG_PTR;
			else if (!strcmp(src, "cstr"))
				arg_types[idx++] = TAG_CSTR;
			else if (!strcmp(src, "ccstr"))
				arg_types[idx++] = TAG_CCSTR;
			else {
				builtins *ptr = NULL;

				if (map_get(q->pl->biftab, src, (void*)&ptr))
					arg_types[idx++] = TAG_STRUCT;
				else
					printf("invalid arg_type: %s\n", src);
			}
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	const char *src = C_STR(q, p4);

	if (!strcmp(src, "uint8"))
		ret_type = TAG_UINT8;
	else if (!strcmp(src, "uint16"))
		ret_type = TAG_UINT16;
	else if (!strcmp(src, "uint32"))
		ret_type = TAG_UINT32;
	else if (!strcmp(src, "uint64"))
		ret_type = TAG_UINT64;
	else if (!strcmp(src, "uint"))
		ret_type = TAG_UINT;
	else if (!strcmp(src, "sint8"))
		ret_type = TAG_INT8;
	else if (!strcmp(src, "sint16"))
		ret_type = TAG_INT16;
	else if (!strcmp(src, "sint32"))
		ret_type = TAG_INT32;
	else if (!strcmp(src, "sint64"))
		ret_type = TAG_INT64;
	else if (!strcmp(src, "sint"))
		ret_type = TAG_INT;
	else if (!strcmp(src, "float"))
		ret_type = TAG_FLOAT32;
	else if (!strcmp(src, "double"))
		ret_type = TAG_FLOAT;
	else if (!strcmp(src, "ptr"))
		ret_type = TAG_PTR;
	else if (!strcmp(src, "cstr"))
		ret_type = TAG_CSTR;
	else if (!strcmp(src, "ccstr"))
		ret_type = TAG_CCSTR;
	else
		printf("invalid ret_type: %s\n", src);

	register_ffi(q->pl, symbol, idx, (void*)func, arg_types, ret_type, true);
	return true;
}

bool do_register_struct(module *m, query *q, void *handle, const char *symbol, cell *l, pl_idx_t l_ctx, const char *ret)
{
	uint8_t arg_types[MAX_ARITY], ret_type = 0;
	bool arg_vars[MAX_ARITY];
	LIST_HANDLER(l);
	int idx = 0;

	while (is_iso_list(l) && (idx < MAX_ARITY)) {
		cell *h = LIST_HEAD(l);
		h = q ? deref(q, h, l_ctx) : h;

		if (is_interned(h)) {
			const char *src = C_STR(m, h);

			if (!strcmp(src, "uint8"))
				arg_types[idx++] = TAG_UINT8;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint8"))
				arg_types[idx++] = MARK_OUT(TAG_UINT8);
			else if (!strcmp(src, "uint16"))
				arg_types[idx++] = TAG_UINT16;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint16"))
				arg_types[idx++] = MARK_OUT(TAG_UINT16);
			else if (!strcmp(src, "uint32"))
				arg_types[idx++] = TAG_UINT32;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint32"))
				arg_types[idx++] = MARK_OUT(TAG_UINT32);
			else if (!strcmp(src, "uint64"))
				arg_types[idx++] = TAG_UINT64;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint64"))
				arg_types[idx++] = MARK_OUT(TAG_UINT64);
			else if (!strcmp(src, "uint"))
				arg_types[idx++] = TAG_UINT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint"))
				arg_types[idx++] = MARK_OUT(TAG_UINT);
			else if (!strcmp(src, "ushort"))
				arg_types[idx++] = TAG_USHORT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "ushort"))
				arg_types[idx++] = MARK_OUT(TAG_USHORT);
			else if (!strcmp(src, "ulong"))
				arg_types[idx++] = TAG_ULONG;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "ulong"))
				arg_types[idx++] = MARK_OUT(TAG_ULONG);
			else if (!strcmp(src, "sint8"))
				arg_types[idx++] = TAG_INT8;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint8"))
				arg_types[idx++] = MARK_OUT(TAG_INT8);
			else if (!strcmp(src, "sint16"))
				arg_types[idx++] = TAG_INT16;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint16"))
				arg_types[idx++] = MARK_OUT(TAG_INT16);
			else if (!strcmp(src, "sint32"))
				arg_types[idx++] = TAG_INT32;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint32"))
				arg_types[idx++] = MARK_OUT(TAG_INT32);
			else if (!strcmp(src, "sint64"))
				arg_types[idx++] = TAG_INT64;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint64"))
				arg_types[idx++] = MARK_OUT(TAG_INT64);
			else if (!strcmp(src, "sint"))
				arg_types[idx++] = TAG_INT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint"))
				arg_types[idx++] = MARK_OUT(TAG_INT);
			else if (!strcmp(src, "sshort"))
				arg_types[idx++] = TAG_INT16;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sshort"))
				arg_types[idx++] = MARK_OUT(TAG_INT16);
			else if (!strcmp(src, "slong"))
				arg_types[idx++] = TAG_LONG;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "slong"))
				arg_types[idx++] = MARK_OUT(TAG_LONG);
			else if (!strcmp(src, "float"))
				arg_types[idx++] = TAG_FLOAT32;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "float"))
				arg_types[idx++] = MARK_OUT(TAG_FLOAT32);
			else if (!strcmp(src, "double"))
				arg_types[idx++] = TAG_FLOAT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "double"))
				arg_types[idx++] = MARK_OUT(TAG_FLOAT);
			else if (!strcmp(src, "ptr"))
				arg_types[idx++] = TAG_PTR;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "ptr"))
				arg_types[idx++] = MARK_OUT(TAG_PTR);
			else if (!strcmp(src, "cstr"))
				arg_types[idx++] = TAG_CSTR;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "cstr"))
				arg_types[idx++] = MARK_OUT(TAG_CSTR);
			else if (!strcmp(src, "ccstr"))
				arg_types[idx++] = TAG_CCSTR;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "ccstr"))
				arg_types[idx++] = MARK_OUT(TAG_CCSTR);
			else if (!strcmp(src, "bool"))
				arg_types[idx++] = TAG_INT64;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "bool"))
				arg_types[idx++] = MARK_OUT(TAG_INT64);
			else
				printf("invalid arg_type: %s\n", src);
		}

		l = LIST_TAIL(l);
		l = q ? deref(q, l, l_ctx) : l;
		l_ctx = q ? q->latest_ctx : 0;
	}

	register_struct(m->pl, symbol, idx, NULL, arg_types);
	return true;
}

bool do_register_predicate(module *m, query *q, void *handle, const char *symbol, cell *l, pl_idx_t l_ctx, const char *ret)
{
	void *func = dlsym(handle, symbol);
	if (!func) return false;

	uint8_t arg_types[MAX_ARITY], ret_type = 0;
	bool arg_vars[MAX_ARITY];
	LIST_HANDLER(l);
	int idx = 0;

	while (is_iso_list(l) && (idx < MAX_ARITY)) {
		cell *h = LIST_HEAD(l);
		h = q ? deref(q, h, l_ctx) : h;

		if (is_interned(h)) {
			const char *src = C_STR(m, h);

			if (!strcmp(src, "uint8"))
				arg_types[idx++] = TAG_UINT8;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint8"))
				arg_types[idx++] = MARK_OUT(TAG_UINT8);
			else if (!strcmp(src, "uint16"))
				arg_types[idx++] = TAG_UINT16;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint16"))
				arg_types[idx++] = MARK_OUT(TAG_UINT16);
			else if (!strcmp(src, "uint32"))
				arg_types[idx++] = TAG_UINT32;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint32"))
				arg_types[idx++] = MARK_OUT(TAG_UINT32);
			else if (!strcmp(src, "uint64"))
				arg_types[idx++] = TAG_UINT64;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint64"))
				arg_types[idx++] = MARK_OUT(TAG_UINT64);
			else if (!strcmp(src, "uint"))
				arg_types[idx++] = TAG_UINT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "uint"))
				arg_types[idx++] = MARK_OUT(TAG_UINT);
			else if (!strcmp(src, "ushort"))
				arg_types[idx++] = TAG_USHORT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "ushort"))
				arg_types[idx++] = MARK_OUT(TAG_USHORT);
			else if (!strcmp(src, "ulong"))
				arg_types[idx++] = TAG_ULONG;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "ulong"))
				arg_types[idx++] = MARK_OUT(TAG_ULONG);
			else if (!strcmp(src, "sint8"))
				arg_types[idx++] = TAG_INT8;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint8"))
				arg_types[idx++] = MARK_OUT(TAG_INT8);
			else if (!strcmp(src, "sint16"))
				arg_types[idx++] = TAG_INT16;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint16"))
				arg_types[idx++] = MARK_OUT(TAG_INT16);
			else if (!strcmp(src, "sint32"))
				arg_types[idx++] = TAG_INT32;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint32"))
				arg_types[idx++] = MARK_OUT(TAG_INT32);
			else if (!strcmp(src, "sint64"))
				arg_types[idx++] = TAG_INT64;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint64"))
				arg_types[idx++] = MARK_OUT(TAG_INT64);
			else if (!strcmp(src, "sint"))
				arg_types[idx++] = TAG_INT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sint"))
				arg_types[idx++] = MARK_OUT(TAG_INT);
			else if (!strcmp(src, "sshort"))
				arg_types[idx++] = TAG_SHORT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "sshort"))
				arg_types[idx++] = MARK_OUT(TAG_SHORT);
			else if (!strcmp(src, "slong"))
				arg_types[idx++] = TAG_LONG;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "slong"))
				arg_types[idx++] = MARK_OUT(TAG_LONG);
			else if (!strcmp(src, "float"))
				arg_types[idx++] = TAG_FLOAT32;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "float"))
				arg_types[idx++] = MARK_OUT(TAG_FLOAT32);
			else if (!strcmp(src, "double"))
				arg_types[idx++] = TAG_FLOAT;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "double"))
				arg_types[idx++] = MARK_OUT(TAG_FLOAT);
			else if (!strcmp(src, "ptr"))
				arg_types[idx++] = TAG_PTR;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "ptr"))
				arg_types[idx++] = MARK_OUT(TAG_PTR);
			else if (!strcmp(src, "cstr"))
				arg_types[idx++] = TAG_CSTR;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "cstr"))
				arg_types[idx++] = MARK_OUT(TAG_CSTR);
			else if (!strcmp(src, "ccstr"))
				arg_types[idx++] = TAG_CCSTR;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "ccstr"))
				arg_types[idx++] = MARK_OUT(TAG_CCSTR);
			else if (!strcmp(src, "bool"))
				arg_types[idx++] = TAG_INT64;
			else if (!strcmp(src, "-") && !strcmp(C_STR(m, h+1), "bool"))
				arg_types[idx++] = MARK_OUT(TAG_INT64);
			else {
				builtins *ptr = NULL;

				if (map_get(m->pl->biftab, src, (void*)&ptr))
					arg_types[idx++] = TAG_STRUCT;
				else
					printf("invalid arg_type: %s\n", src);
			}
		}

		l = LIST_TAIL(l);
		l = q ? deref(q, l, l_ctx) : l;
		l_ctx = q ? q->latest_ctx : 0;
	}

	const char *src = ret;

	if (!strcmp(src, "uint8")) {
		arg_types[idx++] = MARK_OUT(TAG_UINT8);
		ret_type = TAG_INT8;
	} else if (!strcmp(src, "uint16")) {
		arg_types[idx++] = MARK_OUT(TAG_UINT16);
		ret_type = TAG_INT16;
	} else if (!strcmp(src, "uint32")) {
		arg_types[idx++] = MARK_OUT(TAG_UINT32);
		ret_type = TAG_INT32;
	} else if (!strcmp(src, "uint64")) {
		arg_types[idx++] = MARK_OUT(TAG_UINT64);
		ret_type = TAG_INT64;
	} else if (!strcmp(src, "uint")) {
		arg_types[idx++] = MARK_OUT(TAG_UINT);
		ret_type = TAG_UINT;
	} else if (!strcmp(src, "ushort")) {
		arg_types[idx++] = MARK_OUT(TAG_USHORT);
		ret_type = TAG_USHORT;
	} else if (!strcmp(src, "ulong")) {
		arg_types[idx++] = MARK_OUT(TAG_ULONG);
		ret_type = TAG_ULONG;
	} else if (!strcmp(src, "sint8")) {
		arg_types[idx++] = MARK_OUT(TAG_INT8);
		ret_type = TAG_INT8;
	} else if (!strcmp(src, "sint16")) {
		arg_types[idx++] = MARK_OUT(TAG_INT16);
		ret_type = TAG_INT16;
	} else if (!strcmp(src, "sint32")) {
		arg_types[idx++] = MARK_OUT(TAG_INT32);
		ret_type = TAG_INT32;
	} else if (!strcmp(src, "sint64")) {
		arg_types[idx++] = MARK_OUT(TAG_INT64);
		ret_type = TAG_INT64;
	} else if (!strcmp(src, "sint")) {
		arg_types[idx++] = MARK_OUT(TAG_INT);
		ret_type = TAG_INT;
	} else if (!strcmp(src, "sshort")) {
		arg_types[idx++] = MARK_OUT(TAG_SHORT);
		ret_type = TAG_SHORT;
	} else if (!strcmp(src, "slong")) {
		arg_types[idx++] = MARK_OUT(TAG_LONG);
		ret_type = TAG_LONG;
	} else if (!strcmp(src, "float")) {
		arg_types[idx++] = MARK_OUT(TAG_FLOAT32);
		ret_type = TAG_FLOAT32;
	} else if (!strcmp(src, "double")) {
		arg_types[idx++] = MARK_OUT(TAG_FLOAT);
		ret_type = TAG_FLOAT;
	} else if (!strcmp(src, "ptr")) {
		arg_types[idx++] = MARK_OUT(TAG_PTR);
		ret_type = TAG_FLOAT;
	} else if (!strcmp(src, "cstr")) {
		arg_types[idx++] = MARK_OUT(TAG_CSTR);
		ret_type = TAG_CSTR;
	} else if (!strcmp(src, "ccstr")) {
		arg_types[idx++] = MARK_OUT(TAG_CCSTR);
		ret_type = TAG_CCSTR;
	} else if (!strcmp(src, "bool")) {
		arg_types[idx++] = MARK_OUT(TAG_INT64);
		ret_type = TAG_INT64;
	} else if (!strcmp(src, "void")) {
		arg_types[idx++] = MARK_OUT(TAG_VOID);
		ret_type = TAG_VOID;
	} else {
		printf("invalid ret_type: %s\n", src);
	}

	register_ffi(m->pl, symbol, idx, (void*)func, arg_types, ret_type, false);
	return true;
}

USE_RESULT bool fn_sys_register_predicate_4(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,iso_list);
	GET_NEXT_ARG(p4,atom);

	if (!(p1->flags & FLAG_INT_HANDLE) && !(p1->flags & FLAG_HANDLE_DLL))
		return throw_error(q, p1, p1_ctx, "existence_error", "handle");

	return do_register_predicate(q->st.m, q, (void*)(size_t)get_smallint(p1), C_STR(q, p2), p3, p3_ctx, C_STR(q, p4));
}

bool wrapper_for_function(query *q, builtins *ptr)
{
	CHECK_CALC();
	GET_FIRST_ARG(p1, any);
	cell *c = p1;
	pl_idx_t c_ctx = p1_ctx;

	ffi_cif cif;
	ffi_type *arg_types[MAX_ARITY];
	ffi_status status;
	void *arg_values[MAX_ARITY];

	for (unsigned i = 0; i < ptr->arity; i++) {
		if ((ptr->types[i] == TAG_UINT8) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_UINT16) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_UINT32) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_UINT64) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_UINT) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_USHORT) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_ULONG) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT8) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT16) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT32) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT64) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_SHORT) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_LONG) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_PTR) && is_smallint(c))
			;
		else if (ptr->types[i] != c->tag)
			return throw_error(q, c, c_ctx, "type_error",
			ptr->types[i] == TAG_UINT8 ? "integer" :
			ptr->types[i] == TAG_UINT16 ? "integer" :
			ptr->types[i] == TAG_UINT32 ? "integer" :
			ptr->types[i] == TAG_UINT64 ? "integer" :
			ptr->types[i] == TAG_UINT ? "integer" :
			ptr->types[i] == TAG_INT8 ? "integer" :
			ptr->types[i] == TAG_INT16 ? "integer" :
			ptr->types[i] == TAG_INT32 ? "integer" :
			ptr->types[i] == TAG_INT64 ? "integer" :
			ptr->types[i] == TAG_INT ? "integer" :
			ptr->types[i] == TAG_FLOAT32 ? "float" :
			ptr->types[i] == TAG_FLOAT ? "float" :
			ptr->types[i] == TAG_PTR ? "stream" :
			ptr->types[i] == TAG_CSTR ? "atom" :
			ptr->types[i] == TAG_CCSTR ? "atom" :
			ptr->types[i] == TAG_VAR ? "var" :
			"invalid"
			);

		const char *src = C_STR(q, c);

		if (ptr->types[i] == TAG_UINT8)
			arg_types[i] = &ffi_type_uint8;
		else if (ptr->types[i] == TAG_UINT16)
			arg_types[i] = &ffi_type_uint16;
		else if (ptr->types[i] == TAG_UINT32)
			arg_types[i] = &ffi_type_uint32;
		else if (ptr->types[i] == TAG_UINT64)
			arg_types[i] = &ffi_type_uint64;
		else if (ptr->types[i] == TAG_UINT)
			arg_types[i] = &ffi_type_uint;
		else if (ptr->types[i] == TAG_USHORT)
			arg_types[i] = &ffi_type_ushort;
		else if (ptr->types[i] == TAG_ULONG)
			arg_types[i] = &ffi_type_ulong;
		else if (ptr->types[i] == TAG_INT8)
			arg_types[i] = &ffi_type_sint8;
		else if (ptr->types[i] == TAG_INT16)
			arg_types[i] = &ffi_type_sint16;
		else if (ptr->types[i] == TAG_INT32)
			arg_types[i] = &ffi_type_sint32;
		else if (ptr->types[i] == TAG_INT64)
			arg_types[i] = &ffi_type_sint64;
		else if (ptr->types[i] == TAG_INT)
			arg_types[i] = &ffi_type_sint;
		else if (ptr->types[i] == TAG_SHORT)
			arg_types[i] = &ffi_type_sshort;
		else if (ptr->types[i] == TAG_LONG)
			arg_types[i] = &ffi_type_slong;
		else if (ptr->types[i] == TAG_FLOAT32)
			arg_types[i] = &ffi_type_float;
		else if (ptr->types[i] == TAG_FLOAT)
			arg_types[i] = &ffi_type_double;
		else if (ptr->types[i] == TAG_PTR)
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_CSTR)
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_CCSTR)
			arg_types[i] = &ffi_type_pointer;
		else
			arg_types[i] = &ffi_type_void;

		if (ptr->types[i] == TAG_UINT8)
			arg_values[i] = &c->val_uint8;
		else if (ptr->types[i] == TAG_UINT16)
			arg_values[i] = &c->val_uint16;
		else if (ptr->types[i] == TAG_UINT32)
			arg_values[i] = &c->val_uint32;
		else if (ptr->types[i] == TAG_UINT64)
			arg_values[i] = &c->val_uint64;
		else if (ptr->types[i] == TAG_UINT)
			arg_values[i] = &c->val_uint;
		else if (ptr->types[i] == TAG_USHORT)
			arg_values[i] = &c->val_ushort;
		else if (ptr->types[i] == TAG_ULONG)
			arg_values[i] = &c->val_ulong;
		else if (ptr->types[i] == TAG_INT8)
			arg_values[i] = &c->val_int8;
		else if (ptr->types[i] == TAG_INT16)
			arg_values[i] = &c->val_int16;
		else if (ptr->types[i] == TAG_INT32)
			arg_values[i] = &c->val_int32;
		else if (ptr->types[i] == TAG_INT64)
			arg_values[i] = &c->val_int64;
		else if (ptr->types[i] == TAG_INT)
			arg_values[i] = &c->val_int;
		else if (ptr->types[i] == TAG_FLOAT32)
			arg_values[i] = &c->val_float32;
		else if (ptr->types[i] == TAG_FLOAT)
			arg_values[i] = &c->val_float;
		else if (ptr->types[i] == TAG_PTR)
			arg_values[i] = &c->val_ptr;
		else if (ptr->types[i] == TAG_CSTR)
			arg_values[i] = C_STR(q, c);
		else if (ptr->types[i] == TAG_CCSTR)
			arg_values[i] = C_STR(q, c);
		else
			arg_values[i] = NULL;

		GET_NEXT_ARG(p2, any);
		c = p2;
		c_ctx = p2_ctx;
	}

	ffi_type *ret_type = NULL;

	if (ptr->ret_type == TAG_UINT8)
		ret_type = &ffi_type_uint8;
	else if (ptr->ret_type == TAG_UINT16)
		ret_type = &ffi_type_uint16;
	else if (ptr->ret_type == TAG_UINT32)
		ret_type = &ffi_type_uint32;
	else if (ptr->ret_type == TAG_UINT64)
		ret_type = &ffi_type_uint64;
	else if (ptr->ret_type == TAG_UINT)
		ret_type = &ffi_type_uint;
	else if (ptr->ret_type == TAG_USHORT)
		ret_type = &ffi_type_ushort;
	else if (ptr->ret_type == TAG_ULONG)
		ret_type = &ffi_type_ulong;
	else if (ptr->ret_type == TAG_INT8)
		ret_type = &ffi_type_sint8;
	else if (ptr->ret_type == TAG_INT16)
		ret_type = &ffi_type_sint16;
	else if (ptr->ret_type == TAG_INT32)
		ret_type = &ffi_type_sint32;
	else if (ptr->ret_type == TAG_INT64)
		ret_type = &ffi_type_sint64;
	else if (ptr->ret_type == TAG_INT)
		ret_type = &ffi_type_sint;
	else if (ptr->ret_type == TAG_SHORT)
		ret_type = &ffi_type_sshort;
	else if (ptr->ret_type == TAG_LONG)
		ret_type = &ffi_type_slong;
	else if (ptr->ret_type == TAG_FLOAT32)
		ret_type = &ffi_type_float;
	else if (ptr->ret_type == TAG_FLOAT)
		ret_type = &ffi_type_double;
	else if (ptr->ret_type == TAG_PTR)
		ret_type = &ffi_type_pointer;
	else if (ptr->ret_type == TAG_CSTR)
		ret_type = &ffi_type_pointer;
	else if (ptr->ret_type == TAG_CCSTR)
		ret_type = &ffi_type_pointer;
	else
		return false;

	if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, ptr->arity, ret_type, arg_types) != FFI_OK)
		return false;

	union result_ result;
	ffi_call(&cif, FFI_FN(ptr->fn), &result, arg_values);

	cell tmp;

	if (ptr->ret_type == TAG_UINT8)
		make_int(&tmp, result.u8);
	else if (ptr->ret_type == TAG_UINT16)
		make_int(&tmp, result.u16);
	else if (ptr->ret_type == TAG_UINT32)
		make_int(&tmp, result.u32);
	else if (ptr->ret_type == TAG_UINT64)
		make_int(&tmp, result.u64);
	else if (ptr->ret_type == TAG_UINT)
		make_int(&tmp, result.uint);
	else if (ptr->ret_type == TAG_USHORT)
		make_int(&tmp, result.ushort);
	else if (ptr->ret_type == TAG_ULONG)
		make_int(&tmp, result.ulong);
	else if (ptr->ret_type == TAG_INT8)
		make_int(&tmp, result.i8);
	else if (ptr->ret_type == TAG_INT16)
		make_int(&tmp, result.i16);
	else if (ptr->ret_type == TAG_INT32)
		make_int(&tmp, result.i32);
	else if (ptr->ret_type == TAG_INT64)
		make_int(&tmp, result.i64);
	else if (ptr->ret_type == TAG_INT)
		make_int(&tmp, result.sint);
	else if (ptr->ret_type == TAG_SHORT)
		make_int(&tmp, result.sshort);
	else if (ptr->ret_type == TAG_LONG)
		make_int(&tmp, result.slong);
	else if (ptr->ret_type == TAG_FLOAT32)
		make_float(&tmp, result.f32);
	else if (ptr->ret_type == TAG_FLOAT)
		make_float(&tmp, result.f64);
	else if (ptr->ret_type == TAG_PTR)
		make_cstring(&tmp, result.p);
	else if (ptr->ret_type == TAG_CSTR)
		make_cstring(&tmp, result.p);
	else if (ptr->ret_type == TAG_CCSTR)
		make_cstring(&tmp, result.p);
	else
		return false;

	q->accum = tmp;
	return true;
}

bool wrapper_for_predicate(query *q, builtins *ptr)
{
	GET_FIRST_ARG(p1, any);
	cell *c = p1;
	pl_idx_t c_ctx = p1_ctx;

	ffi_cif cif;
	ffi_type *arg_types[MAX_ARITY];
	ffi_status status;
	void *arg_values[MAX_ARITY];
	void *s_args[MAX_ARITY];
	cell cells[MAX_ARITY];
	unsigned arity = ptr->arity - 1;

	if (ptr->ret_type == TAG_VOID)
		arity++;

	for (unsigned i = 0; i < arity; i++) {
		if ((ptr->types[i] == TAG_UINT8) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_UINT16) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_UINT32) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_UINT64) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_UINT) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_USHORT) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_ULONG) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT8) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT16) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT32) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT64) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_INT) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_SHORT) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_LONG) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_PTR) && is_smallint(c))
			;
		else if ((ptr->types[i] == TAG_FLOAT32) && is_float(c))
			;
		else if ((ptr->types[i] == TAG_FLOAT) && is_float(c))
			;
		else if ((ptr->types[i] == TAG_STRUCT) && is_iso_list(c))
			;
		else if ((ptr->types[i] != c->tag) && !is_var(c))
			return throw_error(q, c, c_ctx, "type_error",
			ptr->types[i] == TAG_UINT8 ? "integer" :
			ptr->types[i] == TAG_UINT16 ? "integer" :
			ptr->types[i] == TAG_UINT32 ? "integer" :
			ptr->types[i] == TAG_UINT64 ? "integer" :
			ptr->types[i] == TAG_UINT ? "integer" :
			ptr->types[i] == TAG_USHORT ? "integer" :
			ptr->types[i] == TAG_ULONG ? "integer" :
			ptr->types[i] == TAG_INT8 ? "integer" :
			ptr->types[i] == TAG_INT16 ? "integer" :
			ptr->types[i] == TAG_INT32 ? "integer" :
			ptr->types[i] == TAG_INT64 ? "integer" :
			ptr->types[i] == TAG_INT ? "integer" :
			ptr->types[i] == TAG_SHORT ? "integer" :
			ptr->types[i] == TAG_LONG ? "integer" :
			ptr->types[i] == TAG_FLOAT32 ? "float" :
			ptr->types[i] == TAG_FLOAT ? "float" :
			ptr->types[i] == TAG_CSTR ? "atom" :
			ptr->types[i] == TAG_CCSTR ? "atom" :
			ptr->types[i] == TAG_PTR ? "stream" :
			ptr->types[i] == TAG_VAR ? "var" :
			"invalid"
			);

		const char *src = C_STR(q, c);

		if (ptr->types[i] == TAG_UINT8)
			arg_types[i] = &ffi_type_uint8;
		else if (ptr->types[i] == MARK_OUT(TAG_UINT8))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_UINT16)
			arg_types[i] = &ffi_type_uint16;
		else if (ptr->types[i] == MARK_OUT(TAG_UINT16))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_UINT32)
			arg_types[i] = &ffi_type_uint32;
		else if (ptr->types[i] == MARK_OUT(TAG_UINT32))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_UINT64)
			arg_types[i] = &ffi_type_uint64;
		else if (ptr->types[i] == MARK_OUT(TAG_UINT64))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_UINT)
			arg_types[i] = &ffi_type_uint;
		else if (ptr->types[i] == MARK_OUT(TAG_UINT))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_USHORT)
			arg_types[i] = &ffi_type_ushort;
		else if (ptr->types[i] == MARK_OUT(TAG_USHORT))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_ULONG)
			arg_types[i] = &ffi_type_ulong;
		else if (ptr->types[i] == MARK_OUT(TAG_ULONG))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_INT8)
			arg_types[i] = &ffi_type_sint8;
		else if (ptr->types[i] == MARK_OUT(TAG_INT8))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_INT16)
			arg_types[i] = &ffi_type_sint16;
		else if (ptr->types[i] == MARK_OUT(TAG_INT16))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_INT32)
			arg_types[i] = &ffi_type_sint32;
		else if (ptr->types[i] == MARK_OUT(TAG_INT32))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_INT64)
			arg_types[i] = &ffi_type_sint32;
		else if (ptr->types[i] == MARK_OUT(TAG_INT64))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_INT)
			arg_types[i] = &ffi_type_sint32;
		else if (ptr->types[i] == MARK_OUT(TAG_INT))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_SHORT)
			arg_types[i] = &ffi_type_sshort;
		else if (ptr->types[i] == MARK_OUT(TAG_SHORT))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_LONG)
			arg_types[i] = &ffi_type_slong;
		else if (ptr->types[i] == MARK_OUT(TAG_LONG))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_FLOAT32)
			arg_types[i] = &ffi_type_float;
		else if (ptr->types[i] == MARK_OUT(TAG_FLOAT32))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_FLOAT)
			arg_types[i] = &ffi_type_double;
		else if (ptr->types[i] == MARK_OUT(TAG_FLOAT))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_PTR)
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == MARK_OUT(TAG_PTR))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_CSTR)
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == MARK_OUT(TAG_CSTR))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_CCSTR)
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == MARK_OUT(TAG_CCSTR))
			arg_types[i] = &ffi_type_pointer;
		else if (ptr->types[i] == TAG_STRUCT)
			arg_types[i] = &ffi_type_pointer;
		else
			arg_types[i] = &ffi_type_void;

		if (ptr->types[i] == TAG_UINT8) {
			arg_values[i] = &c->val_uint8;
		} else if (ptr->types[i] == MARK_OUT(TAG_UINT8)) {
			s_args[i] = &cells[i].val_uint8;
			arg_values[i] = &cells[i].val_uint8;
		} else if (ptr->types[i] == TAG_UINT16) {
			arg_values[i] = &c->val_uint16;
		} else if (ptr->types[i] == MARK_OUT(TAG_UINT16)) {
			s_args[i] = &cells[i].val_uint16;
			arg_values[i] = &cells[i].val_uint16;
		} else if (ptr->types[i] == TAG_UINT32) {
			arg_values[i] = &c->val_uint32;
		} else if (ptr->types[i] == MARK_OUT(TAG_UINT32)) {
			s_args[i] = &cells[i].val_uint32;
			arg_values[i] = &cells[i].val_uint32;
		} else if (ptr->types[i] == TAG_UINT64) {
			arg_values[i] = &c->val_uint64;
		} else if (ptr->types[i] == MARK_OUT(TAG_UINT64)) {
			s_args[i] = &cells[i].val_uint64;
			arg_values[i] = &cells[i].val_uint64;
		} else if (ptr->types[i] == TAG_UINT) {
			arg_values[i] = &c->val_uint;
		} else if (ptr->types[i] == MARK_OUT(TAG_UINT)) {
			s_args[i] = &cells[i].val_uint;
			arg_values[i] = &cells[i].val_uint;
		} else if (ptr->types[i] == TAG_USHORT) {
			arg_values[i] = &c->val_ushort;
		} else if (ptr->types[i] == MARK_OUT(TAG_USHORT)) {
			s_args[i] = &cells[i].val_ushort;
			arg_values[i] = &cells[i].val_ushort;
		} else if (ptr->types[i] == TAG_ULONG) {
			arg_values[i] = &c->val_ulong;
		} else if (ptr->types[i] == MARK_OUT(TAG_ULONG)) {
			s_args[i] = &cells[i].val_ulong;
			arg_values[i] = &cells[i].val_ulong;
		} else if (ptr->types[i] == TAG_INT8) {
			arg_values[i] = &c->val_int8;
		} else if (ptr->types[i] == MARK_OUT(TAG_INT8)) {
			s_args[i] = &cells[i].val_int8;
			arg_values[i] = &cells[i].val_int8;
		} else if (ptr->types[i] == TAG_INT16) {
			arg_values[i] = &c->val_int16;
		} else if (ptr->types[i] == MARK_OUT(TAG_INT16)) {
			s_args[i] = &cells[i].val_int16;
			arg_values[i] = &cells[i].val_int16;
		} else if (ptr->types[i] == TAG_INT32) {
			arg_values[i] = &c->val_int32;
		} else if (ptr->types[i] == MARK_OUT(TAG_INT32)) {
			s_args[i] = &cells[i].val_int32;
			arg_values[i] = &cells[i].val_int32;
		} else if (ptr->types[i] == TAG_INT64) {
			arg_values[i] = &c->val_int64;
		} else if (ptr->types[i] == MARK_OUT(TAG_INT64)) {
			s_args[i] = &cells[i].val_int64;
			arg_values[i] = &cells[i].val_int64;
		} else if (ptr->types[i] == TAG_INT) {
			arg_values[i] = &c->val_int;
		} else if (ptr->types[i] == MARK_OUT(TAG_INT)) {
			s_args[i] = &cells[i].val_int;
			arg_values[i] = &cells[i].val_int;
		} else if (ptr->types[i] == TAG_SHORT) {
			arg_values[i] = &c->val_sshort;
		} else if (ptr->types[i] == MARK_OUT(TAG_SHORT)) {
			s_args[i] = &cells[i].val_sshort;
			arg_values[i] = &cells[i].val_sshort;
		} else if (ptr->types[i] == TAG_LONG) {
			arg_values[i] = &c->val_slong;
		} else if (ptr->types[i] == MARK_OUT(TAG_LONG)) {
			s_args[i] = &cells[i].val_slong;
			arg_values[i] = &cells[i].val_slong;
		} else if (ptr->types[i] == TAG_FLOAT32) {
			arg_values[i] = &c->val_float32;
		} else if (ptr->types[i] == MARK_OUT(TAG_FLOAT32)) {
			s_args[i] = &cells[i].val_float32;
			arg_values[i] = &s_args[i];
		} else if (ptr->types[i] == TAG_FLOAT) {
			arg_values[i] = &c->val_float;
		} else if (ptr->types[i] == MARK_OUT(TAG_FLOAT)) {
			s_args[i] = &cells[i].val_float;
			arg_values[i] = &s_args[i];
		} else if (ptr->types[i] == TAG_PTR) {
			arg_values[i] = &c->val_ptr;
		} else if (ptr->types[i] == MARK_OUT(TAG_PTR)) {
			s_args[i] = &cells[i].val_ptr;
			arg_values[i] = &s_args[i];
		} else if (ptr->types[i] == TAG_CSTR) {
			cells[i].val_str = C_STR(q, c);
			s_args[i] = &cells[i].val_str;
			arg_values[i] = &cells[i].val_str;
		} else if (ptr->types[i] == MARK_OUT(TAG_CSTR)) {
			cells[i].val_str = C_STR(q, c);
			s_args[i] = &cells[i].val_str;
			arg_values[i] = &s_args[i];
		} else if (ptr->types[i] == TAG_CCSTR) {
			cells[i].val_str = C_STR(q, c);
			s_args[i] = &cells[i].val_str;
			arg_values[i] = &cells[i].val_str;
		} else if (ptr->types[i] == MARK_OUT(TAG_CCSTR)) {
			cells[i].val_str = C_STR(q, c);
			s_args[i] = &cells[i].val_str;
			arg_values[i] = &s_args[i];
		} else if (ptr->types[i] == TAG_STRUCT) {
			// pack the struct here
			arg_values[i] = c;
		} else
			arg_values[i] = NULL;

		GET_NEXT_ARG(p2, any);
		c = p2;
		c_ctx = p2_ctx;
	}

	ffi_type *ret_type = NULL;

	if (ptr->ret_type == TAG_UINT8)
		ret_type = &ffi_type_uint8;
	else if (ptr->ret_type == TAG_UINT16)
		ret_type = &ffi_type_uint16;
	else if (ptr->ret_type == TAG_UINT32)
		ret_type = &ffi_type_uint32;
	else if (ptr->ret_type == TAG_UINT64)
		ret_type = &ffi_type_uint64;
	else if (ptr->ret_type == TAG_UINT)
		ret_type = &ffi_type_uint;
	else if (ptr->ret_type == TAG_USHORT)
		ret_type = &ffi_type_ushort;
	else if (ptr->ret_type == TAG_ULONG)
		ret_type = &ffi_type_ulong;
	else if (ptr->ret_type == TAG_INT8)
		ret_type = &ffi_type_sint8;
	else if (ptr->ret_type == TAG_INT16)
		ret_type = &ffi_type_sint16;
	else if (ptr->ret_type == TAG_INT32)
		ret_type = &ffi_type_sint32;
	else if (ptr->ret_type == TAG_INT64)
		ret_type = &ffi_type_sint64;
	else if (ptr->ret_type == TAG_INT)
		ret_type = &ffi_type_sint;
	else if (ptr->ret_type == TAG_SHORT)
		ret_type = &ffi_type_sshort;
	else if (ptr->ret_type == TAG_LONG)
		ret_type = &ffi_type_slong;
	else if (ptr->ret_type == TAG_FLOAT32)
		ret_type = &ffi_type_float;
	else if (ptr->ret_type == TAG_FLOAT)
		ret_type = &ffi_type_double;
	else if (ptr->ret_type == TAG_PTR)
		ret_type = &ffi_type_pointer;
	else if (ptr->ret_type == TAG_CSTR)
		ret_type = &ffi_type_pointer;
	else if (ptr->ret_type == TAG_CCSTR)
		ret_type = &ffi_type_pointer;
	else if (ptr->ret_type == TAG_VOID)
		ret_type = &ffi_type_void;
	else
		return false;

	if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arity, ret_type, arg_types) != FFI_OK)
		return false;

	union result_ result;
	ffi_call(&cif, FFI_FN(ptr->fn), &result, arg_values);

	GET_FIRST_ARG(p11, any);
	c = p11;
	c_ctx = p11_ctx;

	for (unsigned i = 0; i < arity; i++) {
		if (is_var(c)) {
			cell tmp;

			if (ptr->types[i] == MARK_OUT(TAG_UINT8)) {
				make_int(&tmp, cells[i].val_uint8);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_UINT16)) {
				make_int(&tmp, cells[i].val_uint16);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_UINT32)) {
				make_int(&tmp, cells[i].val_uint32);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_UINT64)) {
				make_int(&tmp, cells[i].val_uint64);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_UINT)) {
				make_int(&tmp, cells[i].val_uint);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_USHORT)) {
				make_int(&tmp, cells[i].val_ushort);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_ULONG)) {
				make_int(&tmp, cells[i].val_ulong);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_INT8)) {
				make_int(&tmp, cells[i].val_int8);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_INT16)) {
				make_int(&tmp, cells[i].val_int16);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_INT32)) {
				make_int(&tmp, cells[i].val_int32);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_INT64)) {
				make_int(&tmp, cells[i].val_int64);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_INT)) {
				make_int(&tmp, cells[i].val_int);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_SHORT)) {
				make_int(&tmp, cells[i].val_sshort);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_LONG)) {
				make_int(&tmp, cells[i].val_slong);
				bool ok = unify (q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_FLOAT32)) {
				make_float(&tmp, cells[i].val_float32);
				bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_FLOAT)) {
				make_float(&tmp, cells[i].val_float);
				bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_PTR)) {
				make_ptr(&tmp, cells[i].val_ptr);
				bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_CSTR)) {
				check_heap_error(make_cstring(&tmp, cells[i].val_str));
				bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			} else if (ptr->types[i] == MARK_OUT(TAG_CCSTR)) {
				check_heap_error(make_cstring(&tmp, cells[i].val_str));
				bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
				unshare_cell(&tmp);
				if (ok != true) return ok;
			}
		}

		GET_NEXT_ARG(p2, any);
		c = p2;
		c_ctx = p2_ctx;
	}

	cell tmp;

	if (ptr->ret_type == TAG_UINT8) {
		make_int(&tmp, result.u8);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_UINT16) {
		make_int(&tmp, result.u16);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_UINT32) {
		make_int(&tmp, result.u32);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_UINT64) {
		make_int(&tmp, result.u64);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_UINT) {
		make_int(&tmp, result.uint);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_USHORT) {
		make_int(&tmp, result.ushort);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_ULONG) {
		make_int(&tmp, result.ulong);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_INT8) {
		make_int(&tmp, result.i8);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_INT16) {
		make_int(&tmp, result.i16);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_INT32) {
		make_int(&tmp, result.i32);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_INT64) {
		make_int(&tmp, result.i64);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_INT) {
		make_int(&tmp, result.sint);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_SHORT) {
		make_int(&tmp, result.sshort);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_LONG) {
		make_int(&tmp, result.slong);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_FLOAT32) {
		make_float(&tmp, result.f32);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_FLOAT) {
		make_float(&tmp, result.f64);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_PTR) {
		make_ptr(&tmp, result.p);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_CSTR) {
		check_heap_error(make_cstring(&tmp, result.s));
		free(result.s);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	} else if (ptr->ret_type == TAG_CCSTR) {
		check_heap_error(make_cstring(&tmp, result.s));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		if (ok != true) return ok;
	}

	return true;
}

static bool fn_foreign_struct_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);

	return do_foreign_struct(q->st.m, q->st.curr_cell);
}

static bool fn_use_foreign_module_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);

	return do_use_foreign_module(q->st.m, q->st.curr_cell);
}
#endif

builtins g_ffi_bifs[MAX_FFI] =
{
#if USE_FFI
	{"$dlopen", 3, fn_sys_dlopen_3, "+filename,+flag,-handle", false, false, BLAH},
	{"$dlsym", 3, fn_sys_dlsym_3, "+handle,+symbol,-function", false, false, BLAH},
	{"$dlclose", 1, fn_sys_dlclose_1, "+handle", false, false, BLAH},
	{"$register_function", 4, fn_sys_register_function_4, "+handle, +symbol, +arglist,+result", false, false, BLAH},
	{"$register_predicate", 4, fn_sys_register_predicate_4, "+handle, +symbol, +arglist,+result", false, false, BLAH},

	{"foreign_struct", 2, fn_foreign_struct_2, "+atom,+list", false, false, BLAH},
	{"use_foreign_module", 2, fn_use_foreign_module_2, "+atom,+list", false, false, BLAH},
#endif

	{0}
};
