#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <float.h>
#include <errno.h>

#include "trealla.h"
#include "internal.h"
#include "query.h"

#ifdef WASI_TARGET_SPIN
#include "spin.h"
#include "wasi-outbound-http.h"

static bool fn_sys_wasi_outbound_http_5(query *q) {
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,stream);
	stream *req_str = &q->pl->streams[get_stream(q, p2)];
	GET_NEXT_ARG(p3,stream);
	stream *req_hdr_str = &q->pl->streams[get_stream(q, p3)];
	GET_NEXT_ARG(p4,stream);
	stream *resp_str = &q->pl->streams[get_stream(q, p4)];
	GET_NEXT_ARG(p5,stream);
	stream *resp_hdr_str = &q->pl->streams[get_stream(q, p5)];

	if (!is_map_stream(req_str))
		return throw_error(q, p2, q->st.curr_frame, "type_error", "not_a_map");
	if (!is_map_stream(req_hdr_str))
		return throw_error(q, p3, q->st.curr_frame, "type_error", "not_a_map");
	if (!is_map_stream(resp_str))
		return throw_error(q, p4, q->st.curr_frame, "type_error", "not_a_map");
	if (!is_map_stream(resp_hdr_str))
		return throw_error(q, p5, q->st.curr_frame, "type_error", "not_a_map");

	__attribute__((cleanup(wasi_outbound_http_request_free)))
		wasi_outbound_http_request_t request = {0};
	__attribute__((cleanup(wasi_outbound_http_response_free)))
		wasi_outbound_http_response_t response = {0};

	// Request URL
	char *url;
	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		url = chars_list_to_string(q, p1, p1_ctx, len);
	} else {
		url = DUP_STR(q, p1);
	}
	wasi_outbound_http_string_set(&request.uri, url);

	// Request method
	const char *tmpstr;
	if (map_get(req_str->keyval, "method", (const void **)&tmpstr)) {
		if (!spin_http_method_for(tmpstr, &request.method))
			return throw_error(q, p2, q->st.curr_frame, "domain_error", "http_method");
	}

	// Request body
	if (map_get(req_str->keyval, "body", (const void **)&tmpstr)) {
		request.body.is_some = true;
		request.body.val.len = strlen(tmpstr);
		request.body.val.ptr = (uint8_t *)strdup(tmpstr);
	}

	// Request headers
	size_t req_hdr_ct = map_count(req_hdr_str->keyval);
	if (req_hdr_ct > 0) {
		request.headers.len = req_hdr_ct;
		wasi_outbound_http_tuple2_string_string_t *headers = calloc(req_hdr_ct,
			sizeof(wasi_outbound_http_tuple2_string_string_t));
		request.headers.ptr = headers;

		miter *iter = map_first(req_hdr_str->keyval);
		const char *value;
		size_t i = 0;
		while (map_next(iter, (void **)&value)) {
			wasi_outbound_http_tuple2_string_string_t *header = &headers[i++];
			const char *key = map_key(iter);
			wasi_outbound_http_string_t header_name, header_value;
			wasi_outbound_http_string_dup(&header_name, key);
			wasi_outbound_http_string_dup(&header_value, value);
			header->f0 = header_name;
			header->f1 = header_value;
		}
		map_done(iter);
	}

	wasi_outbound_http_http_error_t code = wasi_outbound_http_request(&request, &response);
	switch (code) {
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_SUCCESS:
	case 255:
		// 255 is success (WASI_OUTBOUND_HTTP_HTTP_ERROR_SUCCESS is unused?)
		// see: https://discord.com/channels/926888690310053918/950022897160839248/1026482038028648558
		break;
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_DESTINATION_NOT_ALLOWED:
		return throw_error(q, p1, p1_ctx, "spin_error", "destination_not_allowed");
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_INVALID_URL:
		return throw_error(q, p1, p1_ctx, "spin_error", "invalid_url");
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_REQUEST_ERROR:
		return throw_error(q, p1, p1_ctx, "spin_error", "request_error");
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_RUNTIME_ERROR:
		return throw_error(q, p1, p1_ctx, "spin_error", "runtime_error");
	case WASI_OUTBOUND_HTTP_HTTP_ERROR_TOO_MANY_REQUESTS:
		return throw_error(q, p1, p1_ctx, "spin_error", "too_many_requests");
	default:
		return throw_error(q, p1, p1_ctx, "spin_error", "unknown_error");
	}

	// Response status
	char tmpbuf[8];
	snprintf(tmpbuf, sizeof(tmpbuf), "%d", response.status);
	map_set(resp_str->keyval, strdup("status"), strdup(tmpbuf));

	// Response body
	if (response.body.is_some) {
		char *body = malloc(response.body.val.len) + 1;
		body[response.body.val.len] = 0;
		memcpy(body, response.body.val.ptr, response.body.val.len);
		map_set(resp_str->keyval, strdup("body"), body);
	}

	// Response headers
	if (response.headers.is_some) {
		char *k, *v;
		for (size_t i = 0; i < response.headers.val.len; i++) {
			wasi_outbound_http_tuple2_string_string_t *header = &response.headers.val.ptr[i];

			k = malloc(header->f0.len + 1);
			k[header->f0.len] = 0;
			memcpy(k, header->f0.ptr, header->f0.len);

			v = malloc(header->f1.len + 1);
			v[header->f1.len] = 0;
			memcpy(v, header->f1.ptr, header->f1.len);

			map_set(resp_hdr_str->keyval, k, v);
		}
	}

	return true;
}
#endif

builtins g_contrib_bifs[] =
{
#ifdef WASI_TARGET_SPIN
	{"$wasi_outbound_http", 5, fn_sys_wasi_outbound_http_5, "+string,+map,+map,-map,-map", false, false, BLAH},
#endif
	{0}
};

