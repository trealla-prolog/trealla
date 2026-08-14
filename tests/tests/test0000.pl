:-initialization(main).

f(1). f(2). f(3).
g(a). g(b).

main :- f(X), write(X), nl, g(Y), write('\t'), write(Y), nl, fail.
main.
