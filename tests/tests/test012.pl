:-initialization(main).

sum(I,I,T,T) :- !.
sum(I,X,Tmp,T) :- NewTmp is Tmp+I, NewI is I+1, sum(NewI,X,NewTmp,T).

main :- sum(1,10000,0,T), write(T), nl.
