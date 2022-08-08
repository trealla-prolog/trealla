:-initialization(main).

f(1). f(2). f(3).

main :- f(X), write(X), nl, fail.
main.
