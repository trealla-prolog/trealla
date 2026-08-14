:-initialization(main).

xmember(X, X) :- var(X), !, fail.
xmember(X, [X|_]).
xmember(X, [_|T]) :- xmember(X,T).

main :- xmember(X,[a,[b,b],c]), write(X), nl, fail.
main.
