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

// RFC-3986 section 5.3. A component is absent when its has_ flag is
// clear, which is not the same as being empty: an absent query means no
// '?' at all, an empty one means a '?' with nothing after it.

static char *uri_recompose(const uri_parts *u)
{
	SB(pr);

	if (u->has_scheme) {
		SB_strcatn(pr, u->scheme, u->scheme_len);
		SB_strcat(pr, ":");
	}

	if (u->has_auth) {
		SB_strcat(pr, "//");
		SB_strcatn(pr, u->auth, u->auth_len);
	}

	if (u->path_len)
		SB_strcatn(pr, u->path, u->path_len);

	if (u->has_search) {
		SB_strcat(pr, "?");
		SB_strcatn(pr, u->search, u->search_len);
	}

	if (u->has_frag) {
		SB_strcat(pr, "#");
		SB_strcatn(pr, u->frag, u->frag_len);
	}

	char *out = slicedup(SB_cstr(pr), SB_strlen(pr));
	SB_free(pr);
	return out;
}

// '$uri_build'(-Uri, ?Scheme, ?Auth, ?Path, ?Search, ?Fragment)
//
// An unbound argument means "component absent".

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

	uri_parts u;
	memset(&u, 0, sizeof(u));
	u.has_scheme = part[0] != NULL;
	u.scheme = part[0];
	u.scheme_len = part[0] ? strlen(part[0]) : 0;
	u.has_auth = part[1] != NULL;
	u.auth = part[1];
	u.auth_len = part[1] ? strlen(part[1]) : 0;
	u.path = part[2];
	u.path_len = part[2] ? strlen(part[2]) : 0;
	u.has_search = part[3] != NULL;
	u.search = part[3];
	u.search_len = part[3] ? strlen(part[3]) : 0;
	u.has_frag = part[4] != NULL;
	u.frag = part[4];
	u.frag_len = part[4] ? strlen(part[4]) : 0;

	char *out = uri_recompose(&u);
	bool ok = unify_text(q, p1, p1_ctx, out, strlen(out));
	TPL_free(out);

	for (int i = 0; i < 5; i++)
		TPL_free(part[i]);

	return ok;
}

// '$uri_authority_parse'(+Auth, ?User, ?Password, ?Host, ?Port)
//
// authority = [ userinfo "@" ] host [ ":" port ], userinfo = user [ ":"
// password ]. Port comes back as an integer, the IPv6 host without its
// brackets - both as SWI reports them.

// A port is *DIGIT and has to fit the 16 bits everything downstream
// assumes. A numeric tail that qualifies on neither count stays part of
// the host rather than being silently truncated: strtoul() was letting
// "x:99999999999999999999" saturate and come back as port -1.

static bool parse_port(const char *s, pl_int *port)
{
	unsigned long n = 0;

	if (!*s)
		return false;

	for (const char *p = s; *p; p++) {
		if (!isdigit((unsigned char)*p))
			return false;

		n = (n * 10) + (*p - '0');

		if (n > 65535)
			return false;
	}

	*port = (pl_int)n;
	return true;
}

typedef struct {
	const char *user, *pass, *host;
	size_t user_len, pass_len, host_len;
	bool has_user, has_pass, has_port;
	pl_int port;
} auth_parts;

static void auth_split(const char *src, auth_parts *a)
{
	memset(a, 0, sizeof(*a));
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

			if (rest && *rest)
				has_port = parse_port(rest, &port);
		}
	} else {
		const char *colon = strrchr(host, ':');

		// An all-digit tail is a port; anything else is part of the
		// host (a bracketless IPv6 address, say) and is left alone.

		if (colon && colon[1]) {
			if (parse_port(colon + 1, &port)) {
				has_port = true;
				host_len = colon - host;
			}
		} else if (colon && !colon[1])
			host_len = colon - host;
	}

	a->user = user; a->user_len = user_len; a->has_user = has_user;
	a->pass = pass; a->pass_len = pass_len; a->has_pass = has_pass;
	a->host = host; a->host_len = host_len;
	a->port = port; a->has_port = has_port;
}

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

	auth_parts a;
	auth_split(src, &a);
	cell tmp;

	bool ok =
		(a.has_user ? unify_text(q, p2, p2_ctx, a.user, a.user_len) : unify_absent(p2))
		&& (a.has_pass ? unify_text(q, p3, p3_ctx, a.pass, a.pass_len) : unify_absent(p3))
		&& unify_text(q, p4, p4_ctx, a.host, a.host_len);

	if (ok) {
		if (a.has_port) {
			make_int(&tmp, a.port);
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

// RFC-3986 section 5.2.4. The output can never be longer than the
// input, so one allocation up front is enough.

static void pop_segment(char *out, char **o)
{
	while ((*o > out) && (*(*o - 1) != '/'))
		(*o)--;

	if (*o > out)
		(*o)--;
}

static char *remove_dot_segments(const char *in, size_t n)
{
	char *out = TPL_malloc(n + 2);

	if (!out)
		return NULL;

	char *o = out;
	const char *p = in, *end = in + n;

	while (p < end) {
		size_t left = end - p;

		if ((left >= 3) && !memcmp(p, "../", 3))
			p += 3;
		else if ((left >= 2) && !memcmp(p, "./", 2))
			p += 2;
		else if ((left >= 3) && !memcmp(p, "/./", 3))
			p += 2;
		else if ((left == 2) && !memcmp(p, "/.", 2)) {
			*o++ = '/';
			p = end;
		} else if ((left >= 4) && !memcmp(p, "/../", 4)) {
			p += 3;
			pop_segment(out, &o);
		} else if ((left == 3) && !memcmp(p, "/..", 3)) {
			pop_segment(out, &o);
			*o++ = '/';
			p = end;
		} else if ((left == 1) && (*p == '.'))
			p++;
		else if ((left == 2) && !memcmp(p, "..", 2))
			p += 2;
		else {
			// Move one whole segment across, leading '/' included.

			if (*p == '/')
				*o++ = *p++;

			while ((p < end) && (*p != '/'))
				*o++ = *p++;
		}
	}

	*o = '\0';
	return out;
}

// RFC-3986 section 5.3's merge(). Everything up to and including the
// base's last '/', then the reference.

static char *merge_paths(const uri_parts *base, const char *ref, size_t ref_len)
{
	SB(pr);

	if (base->has_auth && !base->path_len) {
		SB_strcat(pr, "/");
	} else {
		size_t keep = 0;

		for (size_t i = base->path_len; i > 0; i--) {
			if (base->path[i-1] == '/') {
				keep = i;
				break;
			}
		}

		SB_strcatn(pr, base->path, keep);
	}

	SB_strcatn(pr, ref, ref_len);
	char *out = slicedup(SB_cstr(pr), SB_strlen(pr));
	SB_free(pr);
	return out;
}

// '$uri_resolve'(+Ref, +Base, -Uri)
//
// RFC-3986 section 5.2.2, the strict flavour: a reference carrying its
// own scheme is already absolute, even when that scheme matches the
// base's. (The non-strict variant folds "http:g" against a http base
// into "http://a/b/c/g"; section 5.4.2 lists it as the one place the
// two disagree.)

static bool bif_sys_uri_resolve_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);

	char *ref_str = get_text(q, p1, p1_ctx);

	if (!ref_str)
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	char *base_str = get_text(q, p2, p2_ctx);

	if (!base_str) {
		TPL_free(ref_str);
		return throw_error(q, p2, p2_ctx, "type_error", "atom");
	}

	uri_parts r, b, t;
	uri_split(ref_str, &r);
	uri_split(base_str, &b);
	memset(&t, 0, sizeof(t));

	char *owned = NULL;			// whichever path we had to build

	if (r.has_scheme) {
		t.has_scheme = true;
		t.scheme = r.scheme;
		t.scheme_len = r.scheme_len;
		t.has_auth = r.has_auth;
		t.auth = r.auth;
		t.auth_len = r.auth_len;
		owned = remove_dot_segments(r.path, r.path_len);
		t.has_search = r.has_search;
		t.search = r.search;
		t.search_len = r.search_len;
	} else {
		t.has_scheme = b.has_scheme;
		t.scheme = b.scheme;
		t.scheme_len = b.scheme_len;

		if (r.has_auth) {
			t.has_auth = true;
			t.auth = r.auth;
			t.auth_len = r.auth_len;
			owned = remove_dot_segments(r.path, r.path_len);
			t.has_search = r.has_search;
			t.search = r.search;
			t.search_len = r.search_len;
		} else {
			t.has_auth = b.has_auth;
			t.auth = b.auth;
			t.auth_len = b.auth_len;

			if (!r.path_len) {
				// An empty reference path keeps the base's path, and
				// only falls back to the base's query when the
				// reference brought none of its own.

				owned = slicedup(b.path, b.path_len);

				if (r.has_search) {
					t.has_search = true;
					t.search = r.search;
					t.search_len = r.search_len;
				} else {
					t.has_search = b.has_search;
					t.search = b.search;
					t.search_len = b.search_len;
				}
			} else {
				if (r.path[0] == '/')
					owned = remove_dot_segments(r.path, r.path_len);
				else {
					char *merged = merge_paths(&b, r.path, r.path_len);

					if (merged) {
						owned = remove_dot_segments(merged, strlen(merged));
						TPL_free(merged);
					}
				}

				t.has_search = r.has_search;
				t.search = r.search;
				t.search_len = r.search_len;
			}
		}
	}

	t.has_frag = r.has_frag;
	t.frag = r.frag;
	t.frag_len = r.frag_len;

	bool ok = false;

	if (owned) {
		t.path = owned;
		t.path_len = strlen(owned);
		char *out = uri_recompose(&t);

		if (out) {
			ok = unify_text(q, p3, p3_ctx, out, strlen(out));
			TPL_free(out);
		}

		TPL_free(owned);
	}

	TPL_free(ref_str);
	TPL_free(base_str);
	return ok;
}

static int hexval(int ch)
{
	if ((ch >= '0') && (ch <= '9')) return ch - '0';
	if ((ch >= 'a') && (ch <= 'f')) return ch - 'a' + 10;
	return ch - 'A' + 10;
}

static bool is_unreserved(int ch)
{
	if ((ch < 0) || (ch > 127))
		return false;

	return isalnum(ch) || (ch == '-') || (ch == '.') || (ch == '_') || (ch == '~');
}

// RFC-3986 section 6.2.2.2: an escape standing for an unreserved
// character is decoded, and every escape that has to stay gets its hex
// digits uppercased. Reserved characters are left encoded - decoding
// %2F in a path would change what the path means.

static char *pct_normalize(const char *s, size_t n)
{
	char *out = TPL_malloc(n + 1);

	if (!out)
		return NULL;

	char *o = out;

	for (size_t i = 0; i < n; ) {
		if ((s[i] == '%') && (i + 2 < n)
			&& isxdigit((unsigned char)s[i+1]) && isxdigit((unsigned char)s[i+2])) {
			int ch = (hexval((unsigned char)s[i+1]) << 4) | hexval((unsigned char)s[i+2]);

			if (is_unreserved(ch))
				*o++ = (char)ch;
			else {
				*o++ = '%';
				*o++ = toupper((unsigned char)s[i+1]);
				*o++ = toupper((unsigned char)s[i+2]);
			}

			i += 3;
		} else
			*o++ = s[i++];
	}

	*o = '\0';
	return out;
}

static const struct {
	const char *scheme;
	pl_int port;
} s_default_ports[] = {
	{"ftp", 21}, {"http", 80}, {"https", 443}, {"ws", 80}, {"wss", 443}, {NULL, 0}
};

static bool is_default_port(const char *scheme, pl_int port)
{
	if (!scheme)
		return false;

	for (int i = 0; s_default_ports[i].scheme; i++) {
		if (!strcmp(s_default_ports[i].scheme, scheme))
			return s_default_ports[i].port == port;
	}

	return false;
}

// The authority is normalized piecewise rather than as one string: only
// the host is case-insensitive, and only the port can be dropped.

static char *normalize_authority(const char *auth, size_t auth_len, const char *scheme, bool pct)
{
	char *src = slicedup(auth, auth_len);

	if (!src)
		return NULL;

	auth_parts a;
	auth_split(src, &a);
	SB(pr);

	if (a.has_user) {
		char *u = pct ? pct_normalize(a.user, a.user_len) : slicedup(a.user, a.user_len);
		SB_strcat(pr, u ? u : "");
		TPL_free(u);

		if (a.has_pass) {
			SB_strcat(pr, ":");
			char *w = pct ? pct_normalize(a.pass, a.pass_len) : slicedup(a.pass, a.pass_len);
			SB_strcat(pr, w ? w : "");
			TPL_free(w);
		}

		SB_strcat(pr, "@");
	}

	char *h = pct ? pct_normalize(a.host, a.host_len) : slicedup(a.host, a.host_len);

	if (h) {
		for (char *c = h; *c; c++)
			*c = tolower((unsigned char)*c);

		if (is_ipv6_literal(h)) {
			SB_strcat(pr, "[");
			SB_strcat(pr, h);
			SB_strcat(pr, "]");
		} else
			SB_strcat(pr, h);

		TPL_free(h);
	}

	if (a.has_port && !is_default_port(scheme, a.port))
		SB_sprintf(pr, ":%lld", (long long)a.port);

	char *out = slicedup(SB_cstr(pr), SB_strlen(pr));
	SB_free(pr);
	TPL_free(src);
	return out;
}

// '$uri_normalize'(+Uri, +Mode, -Normalized)
//
// RFC-3986 section 6.2.2 (case, %-encoding and path segments) plus the
// two scheme-based rules from 6.2.3 that are safe without knowing the
// scheme's own rules: a default port is dropped, and an empty path
// under an authority becomes "/".
//
// Mode 'iri' skips the %-encoding half of 6.2.2 and leaves every escape
// exactly as it was found, hex case included. An IRI is allowed to hold
// characters a URI has to escape, so re-spelling its escapes is not
// this predicate's business.

static bool bif_sys_uri_normalize_3(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(pm,atom);
	GET_NEXT_ARG(p2,any);

	bool pct = CMP_STRING_TO_CSTR(q, pm, "iri") != 0;

	if (pct && CMP_STRING_TO_CSTR(q, pm, "uri"))
		return throw_error(q, pm, pm_ctx, "domain_error", "uri_normalize_mode");

	char *src = get_text(q, p1, p1_ctx);

	if (!src)
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	uri_parts u, t;
	uri_split(src, &u);
	memset(&t, 0, sizeof(t));

	char *scheme = NULL, *auth = NULL, *path = NULL, *dots = NULL;
	char *search = NULL, *frag = NULL;

	if (u.has_scheme) {
		scheme = slicedup(u.scheme, u.scheme_len);

		if (scheme) {
			for (char *c = scheme; *c; c++)
				*c = tolower((unsigned char)*c);
		}

		t.has_scheme = true;
		t.scheme = scheme;
		t.scheme_len = scheme ? strlen(scheme) : 0;
	}

	if (u.has_auth) {
		auth = normalize_authority(u.auth, u.auth_len, scheme, pct);
		t.has_auth = true;
		t.auth = auth;
		t.auth_len = auth ? strlen(auth) : 0;
	}

	path = pct ? pct_normalize(u.path, u.path_len) : slicedup(u.path, u.path_len);

	if (path)
		dots = remove_dot_segments(path, strlen(path));

	if (dots && !*dots && t.has_auth) {
		TPL_free(dots);
		dots = slicedup("/", 1);
	}

	t.path = dots;
	t.path_len = dots ? strlen(dots) : 0;

	if (u.has_search) {
		search = pct ? pct_normalize(u.search, u.search_len) : slicedup(u.search, u.search_len);
		t.has_search = true;
		t.search = search;
		t.search_len = search ? strlen(search) : 0;
	}

	if (u.has_frag) {
		frag = pct ? pct_normalize(u.frag, u.frag_len) : slicedup(u.frag, u.frag_len);
		t.has_frag = true;
		t.frag = frag;
		t.frag_len = frag ? strlen(frag) : 0;
	}

	bool ok = false;
	char *out = uri_recompose(&t);

	if (out) {
		ok = unify_text(q, p2, p2_ctx, out, strlen(out));
		TPL_free(out);
	}

	TPL_free(scheme);
	TPL_free(auth);
	TPL_free(path);
	TPL_free(dots);
	TPL_free(search);
	TPL_free(frag);
	TPL_free(src);
	return ok;
}

// Which characters have to be escaped, per component. These sets were
// read off what SWI's uri_encoded/3 actually escapes rather than off
// the grammar: RFC-3986 permits sub-delims in a query, but a
// query_value holding a raw '&' or '=' would break the very name=value
// split it exists to feed.

typedef enum {
	COMP_QUERY_VALUE = 0,
	COMP_FRAGMENT,
	COMP_PATH,
	COMP_SEGMENT,
	COMP_AUTH,
	COMP_ANY
} uri_comp;

// Unsafe in every component: escaping these can never change what a
// URI means, only how it reads.

static const char *s_esc_always = " \"#%<>[\\]^`{|}";

static const char *s_esc_extra[] = {
	"&+:;=",		// query_value
	"",				// fragment
	":?",			// path
	"/:?",			// segment
	":@/?#",		// authority - not reachable from '$uri_encode', used
					// when transcoding, where decoding an escaped ':'
					// or '@' would move the userinfo/port boundary
	""				// the everywhere-unsafe set on its own
};

static bool must_escape(int ch, uri_comp comp)
{
	// Controls, DEL, and every byte of a non-ASCII sequence.

	if ((ch < 0x20) || (ch >= 0x7f))
		return true;

	return strchr(s_esc_always, ch) || strchr(s_esc_extra[comp], ch);
}

static void put_escape(char **o, unsigned char ch)
{
	static const char *hex = "0123456789ABCDEF";
	*(*o)++ = '%';
	*(*o)++ = hex[ch >> 4];
	*(*o)++ = hex[ch & 0x0f];
}

// Escaping can triple the length, so one allocation up front.

static char *pct_encode(const char *s, size_t n, uri_comp comp)
{
	char *out = TPL_malloc((n * 3) + 1);

	if (!out)
		return NULL;

	char *o = out;

	for (size_t i = 0; i < n; i++) {
		unsigned char ch = (unsigned char)s[i];

		if (must_escape(ch, comp))
			put_escape(&o, ch);
		else
			*o++ = (char)ch;
	}

	*o = '\0';
	return out;
}

// Decoding only ever shrinks. A malformed escape is passed through
// verbatim rather than being treated as an error - that is what SWI
// does, and a URI holding a bare '%' is common enough in the wild.

static char *pct_decode(const char *s, size_t n, bool plus_is_space, size_t *out_len)
{
	char *out = TPL_malloc(n + 1);

	if (!out)
		return NULL;

	char *o = out;

	for (size_t i = 0; i < n; ) {
		if ((s[i] == '%') && (i + 2 < n)
			&& isxdigit((unsigned char)s[i+1]) && isxdigit((unsigned char)s[i+2])) {
			*o++ = (char)((hexval((unsigned char)s[i+1]) << 4) | hexval((unsigned char)s[i+2]));
			i += 3;
		} else if (plus_is_space && (s[i] == '+')) {
			// Only a query_value reads '+' as a space, and only when
			// decoding - encoding always produces %20. That asymmetry
			// is the HTML form-encoding legacy, and it stops here.

			*o++ = ' ';
			i++;
		} else
			*o++ = s[i++];
	}

	// A decoded query value is arbitrary user data and may hold a
	// NUL, so the length is reported rather than left to strlen():
	// "%00" would otherwise take the rest of the value with it.

	*o = '\0';
	*out_len = o - out;
	return out;
}

// How many bytes of well-formed UTF-8 start at s, or 0 if what is there
// is not a valid sequence. Bounded by end, so a sequence truncated at
// the tail reports 0 rather than reading past the buffer.

static size_t utf8_seq_len(const unsigned char *s, const unsigned char *end)
{
	size_t need;
	unsigned int cp;

	if (s >= end)
		return 0;

	if (s[0] < 0x80)
		return 1;
	else if ((s[0] & 0xe0) == 0xc0) { need = 2; cp = s[0] & 0x1f; }
	else if ((s[0] & 0xf0) == 0xe0) { need = 3; cp = s[0] & 0x0f; }
	else if ((s[0] & 0xf8) == 0xf0) { need = 4; cp = s[0] & 0x07; }
	else return 0;

	if ((size_t)(end - s) < need)
		return 0;

	for (size_t i = 1; i < need; i++) {
		if ((s[i] & 0xc0) != 0x80)
			return 0;

		cp = (cp << 6) | (s[i] & 0x3f);
	}

	// Overlong forms, surrogates and out-of-range code points all have
	// valid-looking byte patterns but no valid meaning.

	if (cp > 0x10ffff)
		return 0;

	if ((cp >= 0xd800) && (cp <= 0xdfff))
		return 0;

	if ((need == 2) && (cp < 0x80))
		return 0;

	if ((need == 3) && (cp < 0x800))
		return 0;

	if ((need == 4) && (cp < 0x10000))
		return 0;

	return need;
}

// Re-encode a raw byte buffer as well-formed UTF-8. Bytes already
// forming a valid sequence are copied across untouched; a byte that
// does not is read as Latin-1 and re-encoded as the code point of the
// same value.
//
// This has to be total. Percent-decoding can yield any byte at all, and
// handing ill-formed UTF-8 to make_cstringn() builds an atom that hangs
// the printer rather than printing badly - "%FF" was enough to do it.
// Reading a stray byte as Latin-1 is also what SWI does with it.

static char *utf8_sanitize(const char *s, size_t n, size_t *out_len)
{
	char *out = TPL_malloc((n * 2) + 1);

	if (!out)
		return NULL;

	char *o = out;
	const unsigned char *p = (const unsigned char *)s, *end = p + n;

	while (p < end) {
		size_t seq = utf8_seq_len(p, end);

		if (seq) {
			memcpy(o, p, seq);
			o += seq;
			p += seq;
		} else {
			unsigned char ch = *p++;
			*o++ = (char)(0xc0 | (ch >> 6));
			*o++ = (char)(0x80 | (ch & 0x3f));
		}
	}

	*o = '\0';
	*out_len = o - out;
	return out;
}

static bool get_component(query *q, cell *c, pl_ctx c_ctx, uri_comp *comp)
{
	if (!is_atom(c))
		return false;

	if (!CMP_STRING_TO_CSTR(q, c, "query_value")) *comp = COMP_QUERY_VALUE;
	else if (!CMP_STRING_TO_CSTR(q, c, "fragment")) *comp = COMP_FRAGMENT;
	else if (!CMP_STRING_TO_CSTR(q, c, "path")) *comp = COMP_PATH;
	else if (!CMP_STRING_TO_CSTR(q, c, "segment")) *comp = COMP_SEGMENT;
	else return false;

	return true;
}

// '$uri_encode'(+Component, +Value, -Encoded)

static bool bif_sys_uri_encode_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	uri_comp comp;

	if (!get_component(q, p1, p1_ctx, &comp))
		return throw_error(q, p1, p1_ctx, "domain_error", "uri_component");

	char *src = get_text(q, p2, p2_ctx);

	if (!src)
		return throw_error(q, p2, p2_ctx, "type_error", "atom");

	bool ok = false;
	char *out = pct_encode(src, strlen(src), comp);

	if (out) {
		ok = unify_text(q, p3, p3_ctx, out, strlen(out));
		TPL_free(out);
	}

	TPL_free(src);
	return ok;
}

// '$uri_decode'(+Component, +Encoded, -Value)

static bool bif_sys_uri_decode_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,any);
	GET_NEXT_ARG(p3,any);
	uri_comp comp;

	if (!get_component(q, p1, p1_ctx, &comp))
		return throw_error(q, p1, p1_ctx, "domain_error", "uri_component");

	char *src = get_text(q, p2, p2_ctx);

	if (!src)
		return throw_error(q, p2, p2_ctx, "type_error", "atom");

	bool ok = false;
	size_t raw_len = 0;
	char *out = pct_decode(src, strlen(src), comp == COMP_QUERY_VALUE, &raw_len);

	if (out) {
		size_t len = 0;
		char *clean = utf8_sanitize(out, raw_len, &len);

		if (clean) {
			ok = unify_text(q, p3, p3_ctx, clean, len);
			TPL_free(clean);
		}

		TPL_free(out);
	}

	TPL_free(src);
	return ok;
}

// Move one component between its URI and IRI spellings.
//
// The two questions get two different character sets, which looks
// asymmetric until you see what each one protects. An ESCAPE is decoded
// only when its character is safe in THIS component - decoding %3F in a
// path would invent a query. A LITERAL is escaped only when it is
// unsafe in EVERY component - escaping the '&' already sitting in a
// query would break the name=value split that is there on purpose.

static char *transcode(const char *s, size_t n, uri_comp comp, bool to_ascii)
{
	char *out = TPL_malloc((n * 3) + 1);

	if (!out)
		return NULL;

	char *o = out;

	for (size_t i = 0; i < n; ) {
		if ((s[i] == '%') && (i + 2 < n)
			&& isxdigit((unsigned char)s[i+1]) && isxdigit((unsigned char)s[i+2])) {
			unsigned char ch = (unsigned char)((hexval((unsigned char)s[i+1]) << 4)
				| hexval((unsigned char)s[i+2]));

			if (ch >= 0x80) {
				// A byte of a UTF-8 sequence: it stays escaped in the
				// URI spelling and becomes raw text in the IRI one.

				if (to_ascii) {
					*o++ = '%';
					*o++ = toupper((unsigned char)s[i+1]);
					*o++ = toupper((unsigned char)s[i+2]);
				} else
					*o++ = (char)ch;
			} else if (must_escape(ch, comp)) {
				*o++ = '%';
				*o++ = toupper((unsigned char)s[i+1]);
				*o++ = toupper((unsigned char)s[i+2]);
			} else
				*o++ = (char)ch;

			i += 3;
		} else {
			unsigned char ch = (unsigned char)s[i++];

			if (ch >= 0x80) {
				if (to_ascii)
					put_escape(&o, ch);
				else
					*o++ = (char)ch;
			} else if (must_escape(ch, COMP_ANY))
				put_escape(&o, ch);
			else
				*o++ = (char)ch;
		}
	}

	*o = '\0';
	return out;
}

static bool do_uri_iri(query *q, bool to_ascii)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,any);

	char *src = get_text(q, p1, p1_ctx);

	if (!src)
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	uri_parts u, t;
	uri_split(src, &u);
	memset(&t, 0, sizeof(t));

	// The scheme is ASCII by grammar and has nothing to transcode.

	t.has_scheme = u.has_scheme;
	t.scheme = u.scheme;
	t.scheme_len = u.scheme_len;

	char *auth = NULL, *path = NULL, *search = NULL, *frag = NULL;

	if (u.has_auth) {
		auth = transcode(u.auth, u.auth_len, COMP_AUTH, to_ascii);
		t.has_auth = true;
		t.auth = auth;
		t.auth_len = auth ? strlen(auth) : 0;
	}

	path = transcode(u.path, u.path_len, COMP_PATH, to_ascii);
	t.path = path;
	t.path_len = path ? strlen(path) : 0;

	if (u.has_search) {
		search = transcode(u.search, u.search_len, COMP_QUERY_VALUE, to_ascii);
		t.has_search = true;
		t.search = search;
		t.search_len = search ? strlen(search) : 0;
	}

	if (u.has_frag) {
		frag = transcode(u.frag, u.frag_len, COMP_FRAGMENT, to_ascii);
		t.has_frag = true;
		t.frag = frag;
		t.frag_len = frag ? strlen(frag) : 0;
	}

	bool ok = false;
	char *out = uri_recompose(&t);

	if (out) {
		if (to_ascii)
			ok = unify_text(q, p2, p2_ctx, out, strlen(out));
		else {
			// Only this direction puts decoded bytes back into the
			// text, so only this direction can go ill-formed.

			size_t len = 0;
			char *clean = utf8_sanitize(out, strlen(out), &len);

			if (clean) {
				ok = unify_text(q, p2, p2_ctx, clean, len);
				TPL_free(clean);
			}
		}

		TPL_free(out);
	}

	TPL_free(auth);
	TPL_free(path);
	TPL_free(search);
	TPL_free(frag);
	TPL_free(src);
	return ok;
}

// '$iri_uri'(+IRI, -URI) - everything non-ASCII becomes UTF-8 escapes.

static bool bif_sys_iri_uri_2(query *q)
{
	return do_uri_iri(q, true);
}

// '$uri_iri'(+URI, -IRI) - UTF-8 escapes become the characters they
// stand for, wherever that is safe.

static bool bif_sys_uri_iri_2(query *q)
{
	return do_uri_iri(q, false);
}

builtins g_uri_bifs[] =
{
	{"$uri_parse", 6, bif_sys_uri_parse_6, "+atom,?atom,?atom,?atom,?atom,?atom", false, false, BLAH},
	{"$uri_build", 6, bif_sys_uri_build_6, "-atom,?atom,?atom,?atom,?atom,?atom", false, false, BLAH},
	{"$uri_authority_parse", 5, bif_sys_uri_authority_parse_5, "+atom,?atom,?atom,?atom,?integer", false, false, BLAH},
	{"$uri_authority_build", 5, bif_sys_uri_authority_build_5, "-atom,?atom,?atom,?atom,?integer", false, false, BLAH},
	{"$uri_resolve", 3, bif_sys_uri_resolve_3, "+atom,+atom,-atom", false, false, BLAH},
	{"$uri_normalize", 3, bif_sys_uri_normalize_3, "+atom,+atom,-atom", false, false, BLAH},
	{"$uri_encode", 3, bif_sys_uri_encode_3, "+atom,+atom,-atom", false, false, BLAH},
	{"$uri_decode", 3, bif_sys_uri_decode_3, "+atom,+atom,-atom", false, false, BLAH},
	{"$iri_uri", 2, bif_sys_iri_uri_2, "+atom,-atom", false, false, BLAH},
	{"$uri_iri", 2, bif_sys_uri_iri_2, "+atom,-atom", false, false, BLAH},

	{0}
};
