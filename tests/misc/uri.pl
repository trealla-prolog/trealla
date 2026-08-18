% RFC-3986 primitives: '$uri_parse'/6, '$uri_build'/6,
% '$uri_authority_parse'/5, '$uri_authority_build'/5,
% '$uri_resolve'/3, '$uri_normalize'/2.

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
	'$uri_normalize'(U, N),
	format("~q | ~q~n", [U,N]).

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
	n('http://[]:80').
