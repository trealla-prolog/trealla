% RFC-3986 syntax primitives: '$uri_parse'/6, '$uri_build'/6,
% '$uri_authority_parse'/5, '$uri_authority_build'/5.

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
	art('host:notaport').
