:- initialization(main).
:- use_module(library(dif)).

ti(G=Rs) :-
   ti(EsG, EsRG, 3),
   ( G = EsG ; G = EsRG ),
   findall(R,(call_residue_vars(G,Vs),length(Vs,R)),Rs).

ti(EsG,(A,B,EDif),N) :-
   N>0,
   EDif = dif(_,_),
   EsG = (EDif,A,B),
   f(EsG).

f((dif(A,B),B=[]*[],A=[]*_)).

main :-
	findall(G-Rs, ti(G=Rs), L),
	write(L), nl.

