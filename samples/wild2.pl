% wild2 is populated (some clauses carry a var in arg2), then the
% predicate is abolished. If wild2 is neither destroyed nor nulled, the
% side list outlives the clauses it points at.
:- dynamic(e/2).

fill :- between(1,700,I),
        ( 0 is I mod 90 -> assertz(e(k, f(a,_,_))) ; assertz(e(k, f(a,I,I))) ),
        fail ; true.

round :- fill,
         ( e(k, f(a,_,_)) -> true ; true ),   % force an idx2+wild2 lookup
         abolish(e/2).

main :- between(1,30,_), round, fail ; true.
main2 :- main, fill, ( e(k, f(a,_,_)) -> write(ok) ; write(no) ), nl.
