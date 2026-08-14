:- initialization(main).

main :-
     member(X,[true,false]),
     ( member(X,[false,true,false]) *->
         write(here1)
     ; write(here2)
     ),
     write({X}), fail.
main :- nl.
