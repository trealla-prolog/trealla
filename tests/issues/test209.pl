:-initialization(main).

ti(G_0, R_0) :-
   G_0 = (A_0,B_0,C_0),
   R_0 = (B_0,C_0,A_0),
   A_0 = unify_with_occurs_check(_,_),
   skel(B_0),
   skel(C_0),
   g(G_0).

g((unify_with_occurs_check(A,B),unify_with_occurs_check(A,[B|C]),unify_with_occurs_check(C,[B|_]))).

skel(unify_with_occurs_check(_,[_|_])).


main :-
	ti(G_0,_),G_0.

main.
	ti(_,R_0),R_0.

main.
