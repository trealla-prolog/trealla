:- initialization(main).

a(1).
a(2).

test :- a(X), (write(X)->nl,!;xyz), fail.
test :- write(oops), nl.

main :- test.
main.
