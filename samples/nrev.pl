nrev([],[]).
nrev([X|Rest],Ans) :- nrev(Rest,L), app(L,[X],Ans).

app([],L,L).
app([X|L1],L2,[X|L3]) :- app(L1,L2,L3).
