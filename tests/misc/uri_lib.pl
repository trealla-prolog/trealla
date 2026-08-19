% library(uri), after SWI-Prolog's. Every case below was run against
% SWI as well; the output agrees with it everywhere except the one
% marked default-port case, where we additionally apply RFC-3986
% section 6.2.3 and SWI does not.

:- initialization(main).
:- use_module(library(uri)).


% Unbound components print as _ so the expected output does not depend
% on variable numbering.

canon(T, '_') :- var(T), !.
canon(T, T) :- atomic(T), !.
canon(T, C) :- T =.. [F|As], canon_list(As, Bs), C =.. [F|Bs].

canon_list([], []).
canon_list([H|T], [H2|T2]) :- canon(H, H2), canon_list(T, T2).

w(Fmt, Args) :- canon(Args, Args2), format(Fmt, Args2).

t(L,G) :- ( catch(G,E,(w("~w ERR ~q~n",[L,E]),true)) -> true ; w("~w FAIL~n",[L]) ).
c(U) :- t(c,(uri_components(U,C),w("comp ~q | ~q~n",[U,C]))).
b(C) :- t(b,(uri_components(U,C),w("bld ~q | ~q~n",[C,U]))).
f(F,U) :- t(f,(uri_components(U,C),(uri_data(F,C,V)->w("data ~w ~q | ~q~n",[F,U,V]);w("data ~w ~q | <fail>~n",[F,U])))).
qp(S) :- t(qp,(uri_query_components(S,Q),w("qparse ~q | ~q~n",[S,Q]))).
qb(L) :- t(qb,(uri_query_components(S,L),w("qbuild ~q | ~q~n",[L,S]))).
ac(A) :- t(ac,(uri_authority_components(A,C),w("auth ~q | ~q~n",[A,C]))).
ab(C) :- t(ab,(uri_authority_components(A,C),w("authb ~q | ~q~n",[C,A]))).
g(U) :- ( uri_is_global(U) -> w("global ~q | yes~n",[U]) ; w("global ~q | no~n",[U]) ).
fn(U) :- ( uri_file_name(U,F) -> w("u2f ~q | ~q~n",[U,F]) ; w("u2f ~q | <fail>~n",[U]) ).
nf(F) :- t(nf,(uri_file_name(U,F),w("f2u ~q | ~q~n",[F,U]))).
nz(U) :- t(nz,(uri_normalized(U,N),w("norm ~q | ~q~n",[U,N]))).
nz3(U,B) :- t(nz3,(uri_normalized(U,B,N),w("norm3 ~q ~q | ~q~n",[U,B,N]))).
iz(U) :- t(iz,(iri_normalized(U,N),w("inorm ~q | ~q~n",[U,N]))).
zi(U) :- t(zi,(uri_normalized_iri(U,N),w("normi ~q | ~q~n",[U,N]))).
rs(U,B) :- t(rs,(uri_resolve(U,B,N),w("res ~q ~q | ~q~n",[U,B,N]))).
ed(A,U) :- t(ed,(uri_edit(A,U,N),w("edit ~q ~q | ~q~n",[A,U,N]))).
en(C,V) :- t(en,(uri_encoded(C,V,E),w("enc ~w ~q | ~q~n",[C,V,E]))).
de(C,E) :- t(de,(uri_encoded(C,V,E),w("dec ~w ~q | ~q~n",[C,E,V]))).
ii(U) :- t(ii,(uri_iri(U,I),w("u2i ~q | ~q~n",[U,I]))).

main :-
    c('http://user:pw@host:8080/p/q?a=1#f'), c('urn:isbn:0451450523'),
    c('mailto:bob@x.com'), c('/rel/path'), c('file:///etc/hosts'), c('http://h'),
    b(uri_components(http,'h:80','/p','a=1',frag)),
    b(uri_components(http,h,_,_,_)),
    b(urn_components(urn,isbn,'0451',_,_)),
    forall(member(F,[scheme,authority,path,search,fragment,nid,nss]),
           f(F,'http://h/p?q#f')),
    forall(member(F,[scheme,authority,path,search,fragment,nid,nss]),
           f(F,'urn:isbn:0451')),
    qp('a=1&b=2'), qp('a&b=1'), qp(''), qp('a=1&a=2'), qp('a=b=c'), qp('=1'),
    qp('a='), qp('a=1&b=x+y&c=%26'),
    qb([a=1,b='x y',c='&=']), qb([a(1),b-2,c=3]), qb([]),
    ac('u:p@h:80'), ac('h'), ac('[::1]:8080'),
    ab(uri_authority(u,p,h,80)), ab(uri_authority(_,_,h,_)),
    g('http://x/'), g('/rel'), g('a:b'), g('c:/x'), g('urn:x:y'),
    fn('file:///etc/hosts'), fn('http://x/y'), fn('file:///a%20b/c'),
    nf('/etc/hosts'), nf('/a b/c'),
    nz('HTTP://X/a/../b'), nz('http://x:80/'), nz('http://X/%c3%a9/%7e/a/../b'),
    nz3(g,'http://a/b/c/d'),
    iz('HTTP://X/%c3%a9/%7e/a/../b'), zi('HTTP://X/%c3%a9/%7e/a/../b'),
    rs(g,'http://a/b/c/d;p?q'), rs('../x','http://a/b/c/d'), rs('http:g','http://a/b/c/d;p?q'),
    ed(path('/new'),'http://h/old?q'), ed([host(h2),port(99)],'http://h/p'),
    ed(path(rel),'http://h/a/b'), ed(fragment(_),'http://h/p#f'),
    ed(search([a=1]),'http://h/p'), ed(scheme(https),'http://h/p'),
    ed(user(bob),'http://h/p'),
    en(path,'a b'), en(query_value,'a&b'), de(path,'a%20b'), de(query_value,'a+b'),
    ii('http://x/%C3%A9').
