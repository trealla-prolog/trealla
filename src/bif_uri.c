#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "prolog.h"
#include "query.h"

// RFC-3986 syntax primitives: split a URI into its five components and
// put one back together again. Nothing here percent-decodes anything -
// a component comes back exactly as it appeared in the URI. Decoding
// belongs to the caller, which is the only party that knows WHICH
// component it is holding and therefore which character set applies.
//
// Text going in may be an atom, a string, or a chars/codes list. Text
// coming out is always a cstring, matching what SWI's library(uri)
// hands back.

static char *get_text(query *q, cell *c, pl_ctx c_ctx)
{
	// Nil is the empty chars list, not the atom '[]'. Checked first
	// because is_atom() says yes to it and DUP_STRING would then hand
	// back the two-character text "[]".

	if (is_nil(c))
		return slicedup("", 0);

	if (is_iso_list(c)) {
		if (!scan_is_chars_list(q, c, c_ctx, true))
			return NULL;

		return chars_list_to_string(q, c, c_ctx);
	}

	if (!is_atom(c))
		return NULL;

	return DUP_STRING(q, c);
}

static bool unify_text(query *q, cell *p, pl_ctx p_ctx, const char *s, size_t n)
{
	cell tmp;

	if (!make_cstringn(&tmp, s, n))
		return throw_error(q, q->st.instr, q->st.cur_ctx, "resource_error", "memory");

	bool ok = unify(q, p, p_ctx, &tmp, q->st.cur_ctx);
	unshare_cell(&tmp);
	return ok;
}

// An absent component is reported by leaving the argument unbound, so
// that a missing query string stays distinguishable from an empty one
// ("http://x/" vs "http://x/?"). A caller that passed something bound
// there is asserting the component is present, so that has to fail.

static bool unify_absent(cell *p)
{
	return is_var(p);
}

typedef struct {
	const char *scheme, *auth, *path, *search, *frag;
	size_t scheme_len, auth_len, path_len, search_len, frag_len;
	bool has_scheme, has_auth, has_search, has_frag;
} uri_parts;

// RFC-3986 appendix B, minus the regex.

static void uri_split(const char *src, uri_parts *u)
{
	memset(u, 0, sizeof(*u));
	const char *p = src, *s;

	// A scheme only counts if the ':' arrives before any '/', '?' or
	// '#' AND the run in front of it is ALPHA *( ALPHA / DIGIT / "+" /
	// "-" / "." ). That leading-letter rule is what keeps "80:foo" and
	// "/tmp/a:b" from being read as scheme-ful URIs.

	if (isalpha((unsigned char)*p)) {
		s = p + 1;

		while (isalnum((unsigned char)*s) || (*s == '+') || (*s == '-') || (*s == '.'))
			s++;

		if (*s == ':') {
			u->scheme = src;
			u->scheme_len = s - src;
			u->has_scheme = true;
			p = s + 1;
		}
	}

	if ((p[0] == '/') && (p[1] == '/')) {
		p += 2;

		for (s = p; *s && (*s != '/') && (*s != '?') && (*s != '#'); s++)
			;

		u->auth = p;
		u->auth_len = s - p;
		u->has_auth = true;
		p = s;
	}

	// Path is always present, possibly empty - unlike the other four
	// it has no "missing" state.

	for (s = p; *s && (*s != '?') && (*s != '#'); s++)
		;

	u->path = p;
	u->path_len = s - p;
	p = s;

	if (*p == '?') {
		p++;

		for (s = p; *s && (*s != '#'); s++)
			;

		u->search = p;
		u->search_len = s - p;
		u->has_search = true;
		p = s;
	}

	if (*p == '#') {
		p++;
		u->frag = p;
		u->frag_len = strlen(p);
		u->has_frag = true;
	}
}

// '$uri_parse'(+Uri, ?Scheme, ?Auth, ?Path, ?Search, ?Fragment)

static bool bif_sys_uri_parse_6(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);
	GET_NEXT_ARG(p5,any);
	GET_NEXT_ARG(p6,any);

	char *src = get_text(q, p1, p1_ctx);

	if (!src)
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	uri_parts u;
	uri_split(src, &u);

	bool ok =
		(u.has_scheme ? unify_text(q, p2, p2_ctx, u.scheme, u.scheme_len) : unify_absent(p2))
		&& (u.has_auth ? unify_text(q, p3, p3_ctx, u.auth, u.auth_len) : unify_absent(p3))
		&& unify_text(q, p4, p4_ctx, u.path, u.path_len)
		&& (u.has_search ? unify_text(q, p5, p5_ctx, u.search, u.search_len) : unify_absent(p5))
		&& (u.has_frag ? unify_text(q, p6, p6_ctx, u.frag, u.frag_len) : unify_absent(p6));

	TPL_free(src);
	return ok;
}

// '$uri_build'(-Uri, ?Scheme, ?Auth, ?Path, ?Search, ?Fragment)
//
// RFC-3986 section 5.3. An unbound argument means "component absent",
// which is not the same as an empty one.

static bool bif_sys_uri_build_6(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);
	GET_NEXT_ARG(p5,any);
	GET_NEXT_ARG(p6,any);

	cell *args[5] = {p2, p3, p4, p5, p6};
	pl_ctx ctxs[5] = {p2_ctx, p3_ctx, p4_ctx, p5_ctx, p6_ctx};
	char *part[5] = {NULL, NULL, NULL, NULL, NULL};

	for (int i = 0; i < 5; i++) {
		if (is_var(args[i]))
			continue;

		if (!(part[i] = get_text(q, args[i], ctxs[i]))) {
			cell *bad = args[i];
			pl_ctx bad_ctx = ctxs[i];

			while (i-- > 0)
				TPL_free(part[i]);

			return throw_error(q, bad, bad_ctx, "type_error", "atom");
		}
	}

	SB(pr);

	if (part[0]) {
		SB_strcat(pr, part[0]);
		SB_strcat(pr, ":");
	}

	if (part[1]) {
		SB_strcat(pr, "//");
		SB_strcat(pr, part[1]);
	}

	if (part[2])
		SB_strcat(pr, part[2]);

	if (part[3]) {
		SB_strcat(pr, "?");
		SB_strcat(pr, part[3]);
	}

	if (part[4]) {
		SB_strcat(pr, "#");
		SB_strcat(pr, part[4]);
	}

	bool ok = unify_text(q, p1, p1_ctx, SB_cstr(pr), SB_strlen(pr));
	SB_free(pr);

	for (int i = 0; i < 5; i++)
		TPL_free(part[i]);

	return ok;
}

// '$uri_authority_parse'(+Auth, ?User, ?Password, ?Host, ?Port)
//
// authority = [ userinfo "@" ] host [ ":" port ], userinfo = user [ ":"
// password ]. Port comes back as an integer, the IPv6 host without its
// brackets - both as SWI reports them.

static bool bif_sys_uri_authority_parse_5(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);
	GET_NEXT_ARG(p5,any);

	char *src = get_text(q, p1, p1_ctx);

	if (!src)
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	const char *user = NULL, *pass = NULL, *host = src;
	size_t user_len = 0, pass_len = 0, host_len = 0;
	bool has_user = false, has_pass = false, has_port = false;
	pl_int port = 0;

	// Split on the LAST '@'. A '@' inside userinfo is required to be
	// escaped, but in the wild it frequently isn't (unescaped e-mail
	// addresses as usernames), and splitting on the last one is what
	// browsers do - it can't misread a valid authority, since a host
	// may never contain '@' at all.

	const char *at = strrchr(src, '@');

	if (at) {
		const char *colon = memchr(src, ':', at - src);
		has_user = true;

		if (colon) {
			user = src;
			user_len = colon - src;
			pass = colon + 1;
			pass_len = at - (colon + 1);
			has_pass = true;
		} else {
			user = src;
			user_len = at - src;
		}

		host = at + 1;
	}

	host_len = strlen(host);

	if (*host == '[') {
		// IPv6 literal. The port, if any, follows the ']' - looking for
		// a bare last ':' first would land inside the address.

		const char *end = strchr(host, ']');

		if (end) {
			const char *rest = end + 1;
			host++;
			host_len = end - host;

			if (*rest == ':')
				rest++;
			else
				rest = NULL;

			if (rest && *rest) {
				char *endp;
				unsigned long n = strtoul(rest, &endp, 10);

				if (!*endp) {
					port = (pl_int)n;
					has_port = true;
				}
			}
		}
	} else {
		const char *colon = strrchr(host, ':');

		// An all-digit tail is a port; anything else is part of the
		// host (a bracketless IPv6 address, say) and is left alone.

		if (colon && colon[1]) {
			char *endp;
			unsigned long n = strtoul(colon + 1, &endp, 10);

			if (!*endp) {
				port = (pl_int)n;
				has_port = true;
				host_len = colon - host;
			}
		} else if (colon && !colon[1])
			host_len = colon - host;
	}

	cell tmp;

	bool ok =
		(has_user ? unify_text(q, p2, p2_ctx, user, user_len) : unify_absent(p2))
		&& (has_pass ? unify_text(q, p3, p3_ctx, pass, pass_len) : unify_absent(p3))
		&& unify_text(q, p4, p4_ctx, host, host_len);

	if (ok) {
		if (has_port) {
			make_int(&tmp, port);
			ok = unify(q, p5, p5_ctx, &tmp, q->st.cur_ctx);
		} else
			ok = unify_absent(p5);
	}

	TPL_free(src);
	return ok;
}

// Hex digits, ':' and '.' only, with at least one ':' - enough to tell
// an address apart from a host name, without re-validating RFC-4291.

static bool is_ipv6_literal(const char *s)
{
	bool colon = false;

	for (const char *p = s; *p; p++) {
		if (*p == ':')
			colon = true;
		else if (!isxdigit((unsigned char)*p) && (*p != '.'))
			return false;
	}

	return colon;
}

// '$uri_authority_build'(-Auth, ?User, ?Password, ?Host, ?Port)

static bool bif_sys_uri_authority_build_5(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	GET_NEXT_ARG(p4,any);
	GET_NEXT_ARG(p5,integer_or_var);

	cell *args[3] = {p2, p3, p4};
	pl_ctx ctxs[3] = {p2_ctx, p3_ctx, p4_ctx};
	char *part[3] = {NULL, NULL, NULL};

	for (int i = 0; i < 3; i++) {
		if (is_var(args[i]))
			continue;

		if (!(part[i] = get_text(q, args[i], ctxs[i]))) {
			cell *bad = args[i];
			pl_ctx bad_ctx = ctxs[i];

			while (i-- > 0)
				TPL_free(part[i]);

			return throw_error(q, bad, bad_ctx, "type_error", "atom");
		}
	}

	SB(pr);

	if (part[0]) {
		SB_strcat(pr, part[0]);

		if (part[1]) {
			SB_strcat(pr, ":");
			SB_strcat(pr, part[1]);
		}

		SB_strcat(pr, "@");
	}

	if (part[2]) {
		// An IPv6 literal has to go back in brackets or the port
		// separator below becomes ambiguous. Testing for ':' alone is
		// not enough: '$uri_authority_parse' hands back a malformed
		// "host:notaport" whole (the tail isn't a port, so it never
		// split), and bracketing that would invent an IPv6 address
		// nobody wrote.

		if (is_ipv6_literal(part[2])) {
			SB_strcat(pr, "[");
			SB_strcat(pr, part[2]);
			SB_strcat(pr, "]");
		} else
			SB_strcat(pr, part[2]);
	}

	if (is_integer(p5))
		SB_sprintf(pr, ":%lld", (long long)get_smallint(p5));

	bool ok = unify_text(q, p1, p1_ctx, SB_cstr(pr), SB_strlen(pr));
	SB_free(pr);

	for (int i = 0; i < 3; i++)
		TPL_free(part[i]);

	return ok;
}

builtins g_uri_bifs[] =
{
	{"$uri_parse", 6, bif_sys_uri_parse_6, "+atom,?atom,?atom,?atom,?atom,?atom", false, false, BLAH},
	{"$uri_build", 6, bif_sys_uri_build_6, "-atom,?atom,?atom,?atom,?atom,?atom", false, false, BLAH},
	{"$uri_authority_parse", 5, bif_sys_uri_authority_parse_5, "+atom,?atom,?atom,?atom,?integer", false, false, BLAH},
	{"$uri_authority_build", 5, bif_sys_uri_authority_build_5, "-atom,?atom,?atom,?atom,?integer", false, false, BLAH},

	{0}
};
