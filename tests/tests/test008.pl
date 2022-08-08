:-initialization(main).

g(a). g(b).

main :- call(g(X)), write(X), nl, fail.
main.
