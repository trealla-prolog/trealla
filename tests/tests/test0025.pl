:-initialization(main).

main :- findall(integer(I),between(1,10,I),L), write(L), nl.
