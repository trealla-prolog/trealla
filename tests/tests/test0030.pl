:-initialization(main).

foo(a,b,c).
foo(a,b,d).
foo(b,c,e).
foo(b,c,f).
foo(c,c,g).
foo(d,e,g).

main :- bagof(C, A^B^foo(A,B,C), Cs), write(Cs), nl, fail.
main.
