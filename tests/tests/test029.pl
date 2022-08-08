:-initialization(main).

foo(a,b,c).
foo(a,b,d).
foo(b,c,e).
foo(b,c,f).
foo(c,c,g).
foo(d,e,g).

main :- between(1,10,_),bagof(C, foo(_,_,C), Cs), write(Cs), nl, fail.
main.
