:-initialization(main).

primes(Limit,Ps) :-
    integers(2,Limit,Is),
    sift(Is,Ps).

integers(Low,High,[Low|Rest]) :-
    Low =< High,
    !,
    M is Low+1,
    integers(M,High,Rest).
integers(_,_,[]).

sift([],[]) :- !.
sift([I|Is],[I|Ps]) :-
    remove(I,Is,New),
    sift(New,Ps).

remove(_,[],[]) :- !.
remove(P,[I|Is],Nis) :-
    0 is I mod P,
    !,
    remove(P,Is,Nis).
remove(P,[I|Is],[I|Nis]) :-
    X is I mod P,
    X \= 0,
    remove(P,Is,Nis).

main :-
    primes(100, X),
    write(X), nl.
