:- initialization(main).

main :-
     member(X,[true,\+true]),
     (   X ->
         write(here1),
         X
     ; \+X ->
         write(here2),
         (\+X; write(here3))
     ),
     write({X}), fail.
main :- nl.
