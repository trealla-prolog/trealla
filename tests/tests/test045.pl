:- initialization(main).

ok(N) :- write(ok), write(N).
ok(N) :- write(again), write(N).

main :- (true -> ok(1) ; write(nok1)), nl, fail.
main :- (false -> write(nok2) ; ok(2)), nl, fail.
main.
