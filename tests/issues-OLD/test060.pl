:-initialization(main).

fill(0, []).
fill(Len, [L|T]) :-
    succ(L, Len),
    !,
    fill(L, T).

main :-
    findall(p(A),
        (   between(1, 10, I),
            fill(I, A)
        ),
        C
    ),
    writeq(C), nl.
