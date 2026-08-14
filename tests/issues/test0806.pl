:- initialization(main).

:- use_module(library(dcgs)).

pr(R) :-
   (  phrase(("0"|"1"),"0")
   -> false
   ;  R=bad
   ).

main :-
	pr(R), write(R), nl.
