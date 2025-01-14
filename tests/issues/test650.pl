:- initialization(main).

a(1).
a(2).

test :- a(X), (writeln(X)->!;xyz), fail.
test :- writeln(oops).

main :- test.
main.
