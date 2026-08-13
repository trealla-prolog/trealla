:- use_module(library(dif)).

:- initialization(main).

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
	term_variables(L, [A,B,C,D,E,F]),
	write_term(L, [variable_names(['A'=A, 'B'=B, 'C'=C, 'D'=D, 'E'=E, 'F'=F])]), nl.
