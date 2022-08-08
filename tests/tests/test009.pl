:-initialization(main).

g(a). g(b).

main :- once(g(X)), write(X), nl, fail.
main.
