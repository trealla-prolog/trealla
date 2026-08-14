:-initialization(main).

foo(a,b,c).
foo(a,b,d).
foo(b,c,e).
foo(b,c,f).
foo(c,c,g).
foo(d,e,g).

main :- findall(Cs, bagof(C, foo(_,_,C), Cs), L), write(L), nl.
