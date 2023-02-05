#ifdef WASI_TARGET_SPIN

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include "cdebug.h"
#include "trealla.h"
#include "prolog.h"
#include "spin-http.h"
#include "stringbuf.h"
#include "map.h"
#include "internal.h"

#include "spin.h"

const char* SPIN_METHODS[] = {
	[SPIN_HTTP_METHOD_GET] 		= "get",
	[SPIN_HTTP_METHOD_POST] 	= "post",
	[SPIN_HTTP_METHOD_PUT] 		= "put",
	[SPIN_HTTP_METHOD_DELETE]	= "delete",
	[SPIN_HTTP_METHOD_PATCH]	= "patch",
	[SPIN_HTTP_METHOD_HEAD]		= "head",
	[SPIN_HTTP_METHOD_OPTIONS]	= "options"
};

bool spin_http_method_lookup(const char *name, uint8_t *id)
{
	for (uint8_t i = 0; i < SPIN_HTTP_METHODS_MAX; i++) {
		if (!strcmp(name, SPIN_METHODS[i])) {
			*id = i;
			return true;
		}
	}
	return false;
}

#define RESIZE_TMPBUF(size) 				\
		if (size > tmpbuf_len) {			\
			tmpbuf = realloc(tmpbuf, size);	\
			if (!tmpbuf) abort();			\
			tmpbuf_len = size; 				\
		}

// The Spin HTTP component handler.
// Using the pre-initialized global interpreter, it asserts a bunch of HTTP-related info
// and then calls spin:http_handle_request/2.
// The response body is read from a memory stream with the alias "http_body".
// The response headers are read from a map stream with the alias "http_headers".
extern void spin_http_handle_http_request(spin_http_request_t *request, spin_http_response_t *response) {
	pl_global_init();
	prolog* pl = pl_global();

	// s will be the query sent to Prolog
	SB(s);

	SB_strcat(s, "spin:assertz(current_http_uri(\"");
	SB_strcatn(s, request->uri.ptr, request->uri.len);
	SB_strcat(s, "\")), ");

	if (request->method >= SPIN_HTTP_METHODS_MAX) {
BADMETHOD:
		SB_free(s);
		fprintf(stderr, "Unhandled method: %d\n", request->method);
		response->status = 405;
		return;
	}
	const char *method = SPIN_METHODS[request->method];
	if (!method) goto BADMETHOD;

	char *tmpbuf = malloc(1024);
	size_t tmpbuf_len = 1024;

	for (size_t i = 0; i < request->params.len; i++) {
		spin_http_tuple2_string_string_t param = request->params.ptr[i];

		size_t fmt_len = param.f0.len*3;
		RESIZE_TMPBUF(fmt_len);
		size_t len = formatted(tmpbuf, tmpbuf_len,
			param.f0.ptr, param.f0.len, true, false);
		SB_strcat(s, "spin:assertz(current_http_param(\"");
		SB_strcatn(s, tmpbuf, len);
		SB_strcat(s, "\",\"");

		fmt_len = param.f1.len*3;
		RESIZE_TMPBUF(fmt_len);
		len = formatted(tmpbuf, tmpbuf_len,
			param.f1.ptr, param.f1.len, true, false);
		SB_strcatn(s, tmpbuf, len);
		SB_strcat(s, "\")), ");
	}

	for (size_t i = 0; i < request->headers.len; i++) {
		spin_http_tuple2_string_string_t header = request->headers.ptr[i];

		size_t fmt_len = header.f0.len*3;
		RESIZE_TMPBUF(fmt_len);
		size_t len = formatted(tmpbuf, tmpbuf_len,
			header.f0.ptr, header.f0.len, true, false);
		SB_strcat(s, "spin:assertz(current_http_header(\"");
		SB_strcatn(s, tmpbuf, len);
		SB_strcat(s, "\",\"");

		fmt_len = header.f1.len*3;
		RESIZE_TMPBUF(fmt_len);
		len = formatted(tmpbuf, tmpbuf_len,
			header.f1.ptr, header.f1.len, true, false);
		SB_strcatn(s, tmpbuf, len);
		SB_strcat(s, "\")), ");
	}

	if (request->body.is_some && request->body.val.len > 0) {
		size_t body_len = request->body.val.len*3;
		RESIZE_TMPBUF(body_len);
		size_t len = formatted(tmpbuf, tmpbuf_len,
			(const char *)request->body.val.ptr, request->body.val.len,
			true, false);
		SB_strcat(s, "spin:assertz(current_http_body(\"");
		SB_strcatn(s, tmpbuf, len);
		SB_strcat(s, "\")), ");
	}
	free(tmpbuf);

	SB_strcat(s, "spin:http_handle_request(\"");
	SB_strcatn(s, request->uri.ptr, request->uri.len);
	SB_strcat(s, "\",");
	SB_strcat(s, method);
	SB_strcat(s, ").");

	bool ok = pl_eval(pl, SB_cstr(s));
	if (!ok || !get_status(pl)) {
		fprintf(stderr, "Error: query failed (ok = %d, status = %d): %s\n",
			ok, get_status(pl), SB_cstr(s));
		SB_free(s);

		int n = pl->current_error;
		stream *str = &pl->streams[n];
		const char *src = SB_cstr(str->sb);
		size_t len = SB_strlen(str->sb);
		char* body = (len > 0) ? strdup(src) : 
			strdup("Internal server error: query failed.\n");
		int32_t body_length = strlen(body);
		response->status = 500;
		response->body.is_some = true;
		response->body.val.ptr = (uint8_t *)body;
		response->body.val.len = body_length;
		return;
	}
	SB_free(s);

	int status = 0;
	const char *body_string;
	size_t body_len = 0;
	
	int n = get_named_stream(pl, "http_body", strlen("http_body"));
	stream *str = &pl->streams[n];
	if (str->is_memory) {
		body_string = SB_cstr(str->sb);
		body_len = SB_strlen(str->sb);
	}

	n = get_named_stream(pl, "http_headers", strlen("http_headers"));
	str = &pl->streams[n];
	spin_http_headers_t response_headers = {0};
	if (str->is_map) {
		map *m = str->keyval;

		const char *sch;
		if (map_get(m, "status", (const void **)&sch)) {
			status = atoi(sch);
			map_del(m, "status");
		}

		size_t count = map_count(m);
		response_headers.len = count;
		spin_http_tuple2_string_string_t *headers = calloc(count,
			sizeof(spin_http_tuple2_string_string_t));
		response_headers.ptr = headers;
		if (count > 0)
			response->headers.is_some = true;

		miter *iter = map_first(m);
		const char *value;
		size_t i = 0;
		while (map_next(iter, (void **)&value)) {
			spin_http_tuple2_string_string_t *header = &headers[i++];
			const char *key = map_key(iter);
			spin_http_string_t header_name, header_value;
			spin_http_string_dup(&header_name, key);
			spin_http_string_dup(&header_value, value);
			header->f0 = header_name;
			header->f1 = header_value;
		}
		map_done(iter);
	}
	response->headers.val = response_headers;

	if (!status)
		status = 500;
	response->status = status;

	if (body_len > 0) {
		uint8_t *body = malloc(body_len);
		memcpy(body, body_string, body_len);
		response->body.is_some = true;
		response->body.val.ptr = body;
		response->body.val.len = body_len;
	}
}

#undef RESIZE_TMPBUF

#endif
