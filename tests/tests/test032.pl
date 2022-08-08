:-initialization(main).

equal(3,1+2).
equal(24,6*4).
equal(1,5 mod 2).

main :- forall(equal(Left,Right), Left =:= Right), write(ok), nl.
