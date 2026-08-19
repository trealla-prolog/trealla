% RFC-3986 primitives: '$uri_parse'/6, '$uri_build'/6,
% '$uri_authority_parse'/5, '$uri_authority_build'/5,
% '$uri_resolve'/3, '$uri_normalize'/3, '$uri_encode'/3,
% '$uri_decode'/3, '$iri_uri'/2, '$uri_iri'/2.

:- initialization(main).

% An absent component comes back unbound. Print it as a fixed atom so
% the output doesn't depend on variable numbering.

o(V, '-') :- var(V), !.
o(V, V).

p(U) :-
	'$uri_parse'(U, S, A, P, Q, F),
	o(S,S1), o(A,A1), o(P,P1), o(Q,Q1), o(F,F1),
	format("~q | ~q ~q ~q ~q ~q~n", [U,S1,A1,P1,Q1,F1]).

a(A) :-
	'$uri_authority_parse'(A, U, Pw, H, Pt),
	o(U,U1), o(Pw,W1), o(H,H1), o(Pt,T1),
	format("~q | ~q ~q ~q ~q~n", [A,U1,W1,H1,T1]).

rt(U) :-
	'$uri_parse'(U, S, A, P, Q, F),
	'$uri_build'(U2, S, A, P, Q, F),
	(	U == U2
	->	format("ok ~q~n", [U])
	;	format("FAILED ~q -> ~q~n", [U,U2])
	).

art(A) :-
	'$uri_authority_parse'(A, U, Pw, H, Pt),
	'$uri_authority_build'(A2, U, Pw, H, Pt),
	(	A == A2
	->	format("ok ~q~n", [A])
	;	format("FAILED ~q -> ~q~n", [A,A2])
	).

% Every reference in RFC-3986 sections 5.4.1 and 5.4.2, resolved
% against the base the RFC uses. Silent when they all agree.

base('http://a/b/c/d;p?q').

r(Ref, Want) :-
	base(B),
	'$uri_resolve'(Ref, B, Got),
	(	Got == Want
	->	true
	;	format("FAILED ~q + ~q -> ~q, wanted ~q~n", [Ref,B,Got,Want])
	).

n(U) :-
	'$uri_normalize'(U, uri, N),
	'$uri_normalize'(U, iri, I),
	format("~q | ~q | ~q~n", [U,N,I]).

rfc5401 :-
	r('g:h', 'g:h'), r(g, 'http://a/b/c/g'), r('./g', 'http://a/b/c/g'),
	r('g/', 'http://a/b/c/g/'), r('/g', 'http://a/g'), r('//g', 'http://g'),
	r('?y', 'http://a/b/c/d;p?y'), r('g?y', 'http://a/b/c/g?y'),
	r('#s', 'http://a/b/c/d;p?q#s'), r('g#s', 'http://a/b/c/g#s'),
	r('g?y#s', 'http://a/b/c/g?y#s'), r(';x', 'http://a/b/c/;x'),
	r('g;x', 'http://a/b/c/g;x'), r('g;x?y#s', 'http://a/b/c/g;x?y#s'),
	r('', 'http://a/b/c/d;p?q'), r('.', 'http://a/b/c/'),
	r('./', 'http://a/b/c/'), r('..', 'http://a/b/'), r('../', 'http://a/b/'),
	r('../g', 'http://a/b/g'), r('../..', 'http://a/'),
	r('../../', 'http://a/'), r('../../g', 'http://a/g').

rfc5402 :-
	r('../../../g', 'http://a/g'), r('../../../../g', 'http://a/g'),
	r('/./g', 'http://a/g'), r('/../g', 'http://a/g'),
	r('g.', 'http://a/b/c/g.'), r('.g', 'http://a/b/c/.g'),
	r('g..', 'http://a/b/c/g..'), r('..g', 'http://a/b/c/..g'),
	r('./../g', 'http://a/b/g'), r('./g/.', 'http://a/b/c/g/'),
	r('g/./h', 'http://a/b/c/g/h'), r('g/../h', 'http://a/b/c/h'),
	r('g;x=1/./y', 'http://a/b/c/g;x=1/y'), r('g;x=1/../y', 'http://a/b/c/y'),
	r('g?y/./x', 'http://a/b/c/g?y/./x'),
	r('g?y/../x', 'http://a/b/c/g?y/../x'),
	r('g#s/./x', 'http://a/b/c/g#s/./x'),
	r('g#s/../x', 'http://a/b/c/g#s/../x'),
	% Strict resolution: a reference with its own scheme is already
	% absolute, even when it repeats the base's scheme.
	r('http:g', 'http:g').

e(C, V) :-
	'$uri_encode'(C, V, E),
	format("enc ~w ~q | ~q~n", [C,V,E]).

d(C, V) :-
	'$uri_decode'(C, V, D),
	format("dec ~w ~q | ~q~n", [C,V,D]).

i2u(I) :-
	'$iri_uri'(I, U),
	format("iri->uri ~q | ~q~n", [I,U]).

u2i(U) :-
	'$uri_iri'(U, I),
	format("uri->iri ~q | ~q~n", [U,I]).

% The exact set of printable ASCII each component escapes. Compared
% character by character against SWI's uri_encoded/3, which these four
% lines reproduce exactly.

charset(C) :-
	findall(Code,
		(	between(32, 126, Code),
			char_code(Ch, Code),
			'$uri_encode'(C, Ch, E),
			atom_length(E, L),
			L > 1
		), Escaped),
	format("~w escapes ~w~n", [C,Escaped]).

main :-
	p('http://www.example.com/path?q=1#frag'),
	p('http://user:pw@[::1]:8080/a/b'),
	p('/relative/path'),
	p('relative/path'),
	p('mailto:bob@example.com'),
	p('urn:isbn:0451450523'),
	p('http://x/'),
	p('http://x/?'),
	p('http://x'),
	p('//host/path'),
	p('a:b'),
	p('/tmp/a:b'),
	p('80:nope'),
	p('file:///etc/hosts'),
	p('http://x/p?a=1&b=2#'),
	p('HTTP://X/Y%2fZ'),
	p(''),
	p("chars://list/ok"),
	nl,
	a('www.example.com'),
	a('www.example.com:8080'),
	a('bob@example.com'),
	a('bob:secret@example.com:443'),
	a('[::1]'),
	a('[::1]:8080'),
	a('[2001:db8::1]'),
	a('bob@sub@host:99'),
	a('host:notaport'),
	a(''),
	nl,
	rt('http://www.example.com/path?q=1#frag'),
	rt('http://user:pw@[::1]:8080/a/b'),
	rt('/relative/path'),
	rt('mailto:bob@example.com'),
	rt('urn:isbn:0451450523'),
	rt('http://x/?'),
	rt('http://x'),
	rt('//host/path'),
	rt('file:///etc/hosts'),
	rt('http://x/p?a=1&b=2#'),
	rt(''),
	nl,
	art('www.example.com'),
	art('www.example.com:8080'),
	art('bob@example.com'),
	art('bob:secret@example.com:443'),
	art('[::1]:8080'),
	art('[2001:db8::1]'),
	art('host:notaport'),
	nl,
	rfc5401,
	rfc5402,
	write('RFC 5.4 vectors agree'), nl,
	nl,
	n('HTTP://www.EXAMPLE.com/'),
	n('http://www.example.com:80/x'),
	n('https://www.example.com:443/x'),
	n('http://www.example.com:8080/x'),
	n('http://example.com'),
	n('http://example.com/%7Euser/%2Fpath/%aa'),
	n('http://example.com/a/./b/../c'),
	n('eXaMpLe://a/./b/../c?Q%7e#F%7e'),
	n('http://USER:PW@HOST:80/'),
	n('/a/b/../c'),
	n('http://[::1]:80/'),
	n('http://[2001:DB8::1]/'),
	n('urn:ISBN:0451450523'),
	n('http://x/%2e%2e/y'),
	n('http://x/%c3%a9'),
	% A port that is not *DIGIT, or will not fit 16 bits, is not a
	% port - it stays part of the host instead of being truncated.
	n('http://x:99999999999999999999/'),
	n('http://x:70000/'),
	n(''),
	n('%'), n('%4'), n('%zz'),
	n('/../../../..'),
	n('http://'),
	n('http://[]:80'),
	nl,
	charset(query_value),
	charset(fragment),
	charset(path),
	charset(segment),
	nl,
	e(path, 'a b'), e(path, 'a/b:c?d#e'), e(path, 'é'), e(path, '日本'),
	e(path, '~-._!$@'), e(segment, 'a/b:c?d#e'), e(fragment, 'a&b=c+d;e'),
	e(query_value, 'a&b=c+d;e'), e(query_value, 'a b'), e(query_value, ''),
	nl,
	% Only a query_value reads '+' as a space, and only when decoding.
	d(query_value, 'a+b'), d(path, 'a+b'), d(fragment, 'a+b'),
	d(segment, 'a+b'),
	d(path, 'a%20b'), d(path, '%C3%A9'), d(path, 'a%2Fb'),
	% Malformed escapes pass through as themselves.
	d(path, '%ZZ'), d(path, '%'), d(path, '%4'), d(path, ''),
	% Percent-decoding can produce any byte at all. A byte that is not
	% part of well-formed UTF-8 is read as Latin-1 rather than left to
	% become an atom the printer cannot handle.
	d(path, '%FF'), d(path, '%C3'), d(path, 'a%00b'),
	nl,
	i2u('http://x/é'), i2u('http://x/a b'), i2u('http://x/%20'),
	i2u('http://x/%2F'), i2u('http://x/?q=a b&r=c'), i2u('http://x/#f g'),
	i2u('http://x/?q=%23b'), i2u('http://x/p%3Fq'), i2u('http://x/?a=1&b=2'),
	i2u('http://user:pw@x/p'), i2u('http://x/日本/x?a=é'),
	nl,
	u2i('http://x/%C3%A9'), u2i('http://x/%2F'), u2i('http://x/%20'),
	u2i('http://x/a+b'), u2i('http://x/?q=%23b'), u2i('http://x/p%3Fq'),
	u2i('http://x/%E6%97%A5%E6%9C%AC'), u2i('http://x/?a=1&b=2'),
	u2i('http://x/%FF').
