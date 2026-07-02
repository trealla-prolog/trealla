#define _XOPEN_SOURCE
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "network.h"
#include "prolog.h"
#include "query.h"

static void parse_host(const char *src, char hostname[1024], char path[4096], unsigned *port, int *ssl, int *domain)
{
	if (!strncmp(src, "https://", 8)) {
		src += 8;
		*ssl = 1;
		*port = 443;
	} else if (!strncmp(src, "http://", 7)) {
		src += 7;
		*ssl = 0;
		*port = 80;
	} else if (!strncmp(src, "unix://", 7)) {
		src += 7;
		*domain = 1;
	}

	if (*src == ':')
		sscanf(src, ":%u/%4095s", port, path);
	else
		sscanf(src, "%1023[^/]%4095s", hostname, path);

	hostname[1023] = '\0';
	path[4095] = '\0';

	if (path[0] == '/')
		memmove(path, path+1, strlen(path+1)+1);
}

static bool bif_sys_server_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,list_or_nil);
	char hostname[1024], path[4096];
	char *keyfile = "privkey.pem", *certfile = "fullchain.pem";
	int udp = 0, nodelay = 1, ssl = 0, domain = 0, level = 0;
	unsigned port = 80;
	snprintf(hostname, sizeof(hostname), "localhost");
	path[0] = '\0';
	char *filename = NULL;

	if (is_var(p1)) {
		port = 0;
		filename = strdup(":0");
	} else if (is_compound(p1) && (p1->arity == 2)) {
		cell *p11 = deref(q, p1+1, p1_ctx);
		cell *p12 = deref(q, p1+2, p1_ctx);
		char tmpbuf[1024];

		if (is_atom(p11) && is_smallint(p12))
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%u", C_STR(q, p11), (unsigned)get_smalluint(p12));
		else if (is_atom(p11) && is_var(p12)) {
			p1 = deref(q, p12, p1_ctx);
			p1_ctx = q->latest_ctx;
			snprintf(tmpbuf, sizeof(tmpbuf), "%s:%u", C_STR(q, p11), port=0);
		} else
			return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

		filename = strdup(tmpbuf);
	} else if (is_atom(p1))
		filename = DUP_STRING(q, p1);
	else if (!is_iso_list(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");
	else {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx);
	}

	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_compound(c) && (c->arity == 1)) {
			if (!CMP_STRING_TO_CSTR(q, c, "udp")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					udp = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "nodelay")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					nodelay = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "ssl")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					ssl = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "keyfile")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					keyfile = C_STR(q, c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "certfile")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					certfile = C_STR(q, c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "hostname")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					slicecpy(hostname, sizeof(hostname), C_STR(q, c), C_STRLEN(q, c));
			} else if (!CMP_STRING_TO_CSTR(q, c, "scheme")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c)) {
					ssl = !CMP_STRING_TO_CSTR(q, c, "https") ? 1 : 0;
					port = 443;
				}
			} else if (!CMP_STRING_TO_CSTR(q, c, "port")) {
				c = deref(q, c + 1, c_ctx);

				if (is_integer(c))
					port = get_smallint(c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "level")) {
				c = deref(q, c + 1, c_ctx);

				if (is_integer(c))
					level = (int)get_smallint(c);
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	const char *url = filename;
	parse_host(url, hostname, path, &port, &ssl, &domain);
	TPL_free(filename);
	int fd = tpl_server(hostname, port, udp, ssl?keyfile:NULL, ssl?certfile:NULL);

	if (fd == -1)
		return throw_error(q, p1, p1_ctx, "existence_error", "server_failed");

	int n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	if (port == 0) {
		port = get_local_port(fd);
		cell tmp;
		make_int(&tmp, port);
		unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	}

	stream *str = &q->pl->streams[n];
	CHECKED(str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL));
	sl_app(str->alias, strdup(hostname), NULL);
	CHECKED(str->filename = DUP_STRING(q, p1));
	CHECKED(str->mode = strdup("update"));
	str->is_socket = true;
	str->nodelay = nodelay;
	str->udp = udp;
	str->ssl = ssl;
	str->level = level;
	str->fp = fdopen(fd, "r");
	str->fp_out = str->fp;

	if (str->fp == NULL) {
		str->is_active = false;
		close(fd);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

	if (!str->ssl && q->is_task)
		tpl_set_nonblocking(str);

	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM;
	unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
	return true;
}

static bool bif_sys_accept_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,var);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	int fd = tpl_accept(str);

	if (fd == -1) {
		if (q->is_task)
			return do_yield(q, 1);

		return false;
	}

	n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str2 = &q->pl->streams[n];
	sl_app(str2->alias, strdup(str->filename), NULL);
	CHECKED(str2->filename = strdup(str->filename));
	CHECKED(str2->mode = strdup("update"));
	str2->is_socket = true;
	str2->nodelay = str->nodelay;
	str2->udp = str->udp;
	str2->ssl = str->ssl;
	str2->fp = fdopen(fd, "r+");

	if (str2->fp == NULL) {
		str2->is_active = false;
		close(fd);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

#ifndef __wasi__
	int fd2 = dup(fd);
	str2->fp_out = fdopen(fd2, "r+");

	if (str2->fp_out == NULL) {
		close(fd2);
		fclose(str2->fp);
		str2->is_active = false;
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}
#else
	str2->fp_out = str2->fp;
#endif

	if (str->ssl) {
		str2->sslptr = tpl_enable_ssl(fd, str->filename, 1, str->level, NULL);

		if (!str2->sslptr) {
			close(fd);
			str2->is_active = false;
			return false;
		}
	}

	if (!str->ssl && q->is_task) {
		tpl_set_nonblocking(str2);
		CHECKED(push_choice(q));
	}

	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM;
	unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
	return true;
}

static bool do_parse_parts(query *q, cell *p1, pl_ctx p1_ctx, cell *p2, pl_ctx p2_ctx, bool full)
{
	char protocol[256], host[1024], path[8192], search[8192], fragment[8192];
	protocol[0] = host[0] = path[0] = search[0] = fragment[0] = '\0';
	int port = 0;
	LIST_HANDLER(p2);

	while (is_iso_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_ctx h_ctx = q->latest_ctx;
		cell *c = deref(q, h+1, h_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (!strcmp(C_STR(q, h), "protocol")) {
			if (!is_atom(c))
				return throw_error(q, c, c_ctx, "type_error", "atom");

			snprintf(protocol, sizeof(protocol), "%s", C_STR(q, c));
		} else if (!strcmp(C_STR(q, h), "host")) {
			if (!is_atom(c))
				return throw_error(q, c, c_ctx, "type_error", "atom");

			snprintf(host, sizeof(host), "%s", C_STR(q, c));
		} else if (!strcmp(C_STR(q, h), "port")) {
			if (!is_smallint(c))
				return throw_error(q, c, c_ctx, "type_error", "integer");

			port = get_smallint(c);
		} else if (!strcmp(C_STR(q, h), "path")) {
			if (!is_atom(c))
				return throw_error(q, c, c_ctx, "type_error", "atom");

			snprintf(path, sizeof(path), "%s", C_STR(q, c));
		} else if (!strcmp(C_STR(q, h), "search")) {
			cell *h1 = h + 1;
			h1 = deref(q, h1, h_ctx);
			pl_ctx h1_ctx = q->latest_ctx;

			if (!is_iso_list(h1))
				return throw_error(q, h1, h_ctx, "type_error", "list");

			char *dst = search;
			LIST_HANDLER(h1);

			while (is_iso_list(h1)) {
				cell *c = LIST_HEAD(h1);
				c = deref(q, c, h1_ctx);

				if (!is_compound(c) || (c->val_off != g_eq_s) || (c->arity != 2))
					return throw_error(q, c, h1_ctx, "type_error", "compound");

				if (!is_atom(c+1))
					return throw_error(q, c+1, h1_ctx, "type_error", "atom");

				if (!is_atomic(c+2))
					return throw_error(q, c+2, h1_ctx, "type_error", "atom");

				size_t len1 = C_STRLEN(q, c+1);
				char *dstbuf1 = TPL_malloc(len1+1);
				CHECKED(dstbuf1);
				url_encode(C_STR(q, c+1), len1, dstbuf1, len1+1);
				dst += snprintf(dst, sizeof(search), "%s", dstbuf1);
				TPL_free(dstbuf1);

				if (is_atom(c+2)) {
					size_t len2 = C_STRLEN(q, c+2);
					char *dstbuf2 = TPL_malloc(len2+1);
					CHECKED(dstbuf2);
					url_encode(C_STR(q, c+2), len2, dstbuf2, len2+1);
					dst += snprintf(dst, sizeof(search), "=%s", dstbuf2);
					TPL_free(dstbuf2);
				} else if (is_smallint(c+2)) {
					char tmpbuf[256];
					snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long)get_smallint(c+2));
					size_t len2 = strlen(tmpbuf);
					char *dstbuf2 = TPL_malloc(len2+1);
					CHECKED(dstbuf2);
					url_encode(tmpbuf, len2, dstbuf2, len2+1);
					dst += snprintf(dst, sizeof(search), "=%s", dstbuf2);
					TPL_free(dstbuf2);
				} else {
					char tmpbuf[256];
					snprintf(tmpbuf, sizeof(tmpbuf), "%.17g", get_float(c+2));
					size_t len2 = strlen(tmpbuf);
					char *dstbuf2 = TPL_malloc(len2+1);
					CHECKED(dstbuf2);
					url_encode(tmpbuf, len2, dstbuf2, len2+1);
					dst += snprintf(dst, sizeof(search), "=%s", dstbuf2);
					TPL_free(dstbuf2);
				}

				h1 = LIST_TAIL(h1);
				h1 = deref(q, h1, h1_ctx);
				h1_ctx = q->latest_ctx;

				if (!is_nil(h1))
					dst += snprintf(dst, sizeof(search), "&");
			}
		} else if (!strcmp(C_STR(q, h), "fragment")) {
			if (!is_atom(c))
				return throw_error(q, c, c_ctx, "type_error", "atom");

			snprintf(fragment, sizeof(fragment), "%s", C_STR(q, c));
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	SB(pr);
	SB_sprintf(pr, "%s://%s", protocol, host);
	if (port) SB_sprintf(pr, ":%d", port);
	if (path[0]) SB_sprintf(pr, "%s", path);
	if (search[0]) SB_sprintf(pr, "?%s", search);
	if (fragment[0]) SB_sprintf(pr, "#%s", fragment);
	cell tmp;
	make_cstring(&tmp,  SB_cstr(pr));
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

static bool do_parse_url(query *q, cell *p1, pl_ctx p1_ctx, cell *p2, pl_ctx p2_ctx, bool full)
{
	const char *src = C_STR(q, p1);
	char protocol[256], host[1024], path[8192], search[8192], fragment[8192];
	protocol[0] = host[0] = path[0] = search[0] = fragment[0] = '\0';

	if (full)
		sscanf(src, "%255[^:\r\n]://%1023[^/\r\n]%8191[^?\r\n]?%8191[^#\r\n]#%8191[^\r\n]", protocol, host, path, search, fragment);
	else
		sscanf(src, "%8191[^?\r\n]?%8191[^#\r\n]#%8191[^\r\n]", path, search, fragment);

	protocol[255] = host[1023] = path[8191] = search[8191] = fragment[8191] = '\0';
	char *dstbuf;
	size_t len;
	cell tmp[3];

	if (search[0]) {
		allocate_list(q, tmp);
		char key[8192], search2[8192];
		key[0] = search2[0] = '\0';
		char *src2 = search, *dst2 = key;
		bool first = true;

		while (*src2) {
			if (*src2 == '=') {
				src2++;
				dst2 = search2;
				*dst2 = '\0';
			} else if (*src2 == '&') {
				make_instr(tmp, new_atom(q->pl, "="), NULL, 2, 2);
				SET_OP(tmp, OP_YFX);

				len = strlen(key);
				dstbuf = TPL_malloc(len+1);
				CHECKED(dstbuf);
				url_decode(key, dstbuf);
				make_cstring(tmp+1, dstbuf);
				TPL_free(dstbuf);

				len = strlen(search2);
				dstbuf = TPL_malloc(len+1);
				CHECKED(dstbuf);
				url_decode(search2, dstbuf);
				make_cstring(tmp+2, dstbuf);
				TPL_free(dstbuf);

				if (first) {
					allocate_list(q, tmp);
					first = false;
				} else
					append_list(q, tmp);

				src2++;
				dst2 = key;
				*dst2 = '\0';
			}

			*dst2++ = *src2++;
			*dst2 = '\0';
		}

		make_instr(tmp, new_atom(q->pl, "="), NULL, 2, 2);
		SET_OP(tmp, OP_YFX);

		len = strlen(key);
		dstbuf = TPL_malloc(len+1);
		CHECKED(dstbuf);
		url_decode(key, dstbuf);
		make_cstring(tmp+1, dstbuf);
		TPL_free(dstbuf);

		len = strlen(search2);
		dstbuf = TPL_malloc(len+1);
		CHECKED(dstbuf);
		url_decode(search2, dstbuf);
		make_cstring(tmp+2, dstbuf);
		TPL_free(dstbuf);
		append_list(q, tmp);

		cell *l = end_list(q);
		cell *tmp2 = alloc_heap(q, 1 + l->num_cells);
		make_instr(tmp2, new_atom(q->pl, "search"), NULL, 1, l->num_cells);
		dup_cells(tmp2+1, l, l->num_cells);
		allocate_list(q, tmp2);
	}

	if (protocol[0]) {
		make_instr(tmp, new_atom(q->pl, "protocol"), NULL, 1, 1);
		make_cstring(tmp+1, protocol);

		if (search[0])
			append_list(q, tmp);
		else
			allocate_list(q, tmp);
	}

	if (host[0]) {
		char host2[256]; host2[0] = '\0';
		int port = 0;
		sscanf(host, "%255[^:]:%d", host2, &port);
		host2[255] = '\0';

		make_instr(tmp, new_atom(q->pl, "host"), NULL, 1, 1);
		make_cstring(tmp+1, host2);
		append_list(q, tmp);

		if (port) {
			make_instr(tmp, new_atom(q->pl, "port"), NULL, 1, 1);
			make_int(tmp+1, port);
			append_list(q, tmp);
		}
	}

	if (!path[0])
		strcpy(path, "/");

	if (path[0]) {
		len = strlen(path);
		dstbuf = TPL_malloc(len+1);
		CHECKED(dstbuf);
		url_decode(path, dstbuf);
		src = dstbuf;
		make_instr(tmp, new_atom(q->pl, "path"), NULL, 1, 1);
		make_cstring(tmp+1, path);
		append_list(q, tmp);
		TPL_free(dstbuf);
	}

	if (fragment[0]) {
		len = strlen(fragment);
		dstbuf = TPL_malloc(len+1);
		CHECKED(dstbuf);
		url_decode(path, dstbuf);
		src = dstbuf;
		make_instr(tmp, new_atom(q->pl, "fragment"), NULL, 1, 1);
		make_cstring(tmp+1, fragment);
		append_list(q, tmp);
		TPL_free(dstbuf);
	}

	return unify(q, p2, p2_ctx, end_list(q), q->st.cur_ctx);
}

static bool bif_sys_parse_url_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,iso_list_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error2(q, p1, p1_ctx, "uninstantiation_error", "not_sufficiently_instantiated", p2);

	if (is_var(p2))
		return do_parse_url(q, p1, p1_ctx, p2, p2_ctx, true);
	else
		return do_parse_parts(q, p1, p1_ctx, p2, p2_ctx, true);
}

static bool bif_sys_http_location_2(query *q)
{
	GET_FIRST_ARG(p1,iso_list_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error2(q, p1, p1_ctx, "uninstantiation_error", "not_sufficiently_instantiated", p2);

	if (is_var(p1))
		return do_parse_url(q, p2, p2_ctx, p1, p1_ctx, false);
	else
		return do_parse_parts(q, p2, p2_ctx, p1, p1_ctx, false);
}

static bool bif_sys_client_5(query *q)
{
	GET_FIRST_ARG(p1,source_sink);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,var);
	GET_NEXT_ARG(p5,list_or_nil);
	char hostname[1024], path[1024*4];
	char *certfile = NULL;
	int udp = 0, nodelay = 1, ssl = 0, domain = 0, level = 0;
	hostname[0] = path[0] = '\0';
	unsigned port = 80;
	char *filename = NULL;

	if (is_atom(p1))
		filename = DUP_STRING(q, p1);
	else if (!is_list(p1)) {
		char host[1024];
		snprintf(host, sizeof(host), "%s", C_STR(q, deref(q, p1+1, p1_ctx)));
		port = (int)get_smallint(deref(q, p1+2, p1_ctx));
		filename = strdup(host);
	}

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx);
	}

	LIST_HANDLER(p5);

	while (is_iso_list(p5)) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);
		pl_ctx c_ctx = q->latest_ctx;

		if (is_compound(c) && (c->arity == 1)) {
			if (!CMP_STRING_TO_CSTR(q, c, "udp")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					udp = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "nodelay")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					nodelay = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "ssl")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					ssl = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "certfile")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c))
					certfile = C_STR(q, c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "scheme")) {
				c = deref(q, c + 1, c_ctx);

				if (is_atom(c)) {
					ssl = !CMP_STRING_TO_CSTR(q, c, "https") ? 1 : 0;
					if (ssl) port = 443;
				}
			} else if (!CMP_STRING_TO_CSTR(q, c, "port")) {
				c = deref(q, c + 1, c_ctx);

				if (is_integer(c))
					port = (int)get_smallint(c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "level")) {
				c = deref(q, c + 1, c_ctx);

				if (is_integer(c))
					level = (int)get_smallint(c);
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	const char *url = filename;
	parse_host(url, hostname, path, &port, &ssl, &domain);
	TPL_free(filename);

	int fd = tpl_connect(hostname, port, udp, nodelay);

	if (fd == -1)
		return throw_error(q, p1, p1_ctx, "resource_error", "could_not_connect");

	int n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str = &q->pl->streams[n];
	CHECKED(str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL));
	sl_app(str->alias, DUP_STRING(q, p1), NULL);
	CHECKED(str->filename = DUP_STRING(q, p1));
	CHECKED(str->mode = strdup("update"));
	str->is_socket = true;
	str->nodelay = nodelay;
	str->udp = udp;
	str->ssl = ssl;
	str->level = level;
	str->fp = fdopen(fd, "r+");

	if (!str->filename || !str->mode) {
		sl_destroy(str->alias);
		TPL_free(str->filename);
		TPL_free(str->mode);
		str->is_active = false;
		return false;
	}

	if (str->fp == NULL) {
		str->is_active = false;
		close(fd);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

#ifndef __wasi__
	int fd2 = dup(fd);
	str->fp_out = fdopen(fd2, "r+");

	if (str->fp_out == NULL) {
		close(fd2);
		fclose(str->fp);
		str->is_active = false;
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}
#else
	str->fp_out = str->fp;
#endif

	if (str->ssl) {
		str->sslptr = tpl_enable_ssl(fd, hostname, 0, str->level, certfile);
		CHECKED(str->sslptr);
	}

	if (!str->ssl && q->is_task)
		tpl_set_nonblocking(str);

	cell tmp;
	make_string(&tmp, hostname);
	unify(q, p2, p2_ctx, &tmp, q->st.cur_ctx);
	unshare_cell(&tmp);
	make_string(&tmp, path);
	unify(q, p3, p3_ctx, &tmp, q->st.cur_ctx);
	unshare_cell(&tmp);
	cell tmp2;
	make_int(&tmp2, n);
	tmp2.flags |= FLAG_INT_STREAM;
	unify(q, p4, p4_ctx, &tmp2, q->st.cur_ctx);
	return true;
}

static bool bif_sys_current_host_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	char buffer[256];
	const char *host = get_local_hostname(buffer, sizeof(buffer));
	cell tmp;
	make_cstring(&tmp, host);
	return unify(q, p1, p1_ctx, &tmp, q->st.cur_ctx);
}

builtins g_net_bifs[] =
{
	{"$http_location", 2, bif_sys_http_location_2, "?list,?atom", false, false, BLAH},
	{"$parse_url", 2, bif_sys_parse_url_2, "?atom,?list", false, false, BLAH},
	{"$server", 3, bif_sys_server_3, "+source_sink,--stream,+list", false, false, BLAH},
	{"$accept", 2, bif_sys_accept_2, "+stream,--stream", false, false, BLAH},
	{"$client", 5, bif_sys_client_5, "+source_sink,-atom,-atom,-stream,+list", false, false, BLAH},
	{"$current_host", 1, bif_sys_current_host_1, "-atom", false, false, BLAH},

	{0}
};

