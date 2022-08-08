inform(X,Y) :- write(X), write(Y), nl.
move(0,_,_,_) :- !.
move(N,A,B,C) :- M is N-1, move(M,A,C,B), inform(A,B), move(M,C,B,A).
hanoi(N) :- move(N,' left ',' centre ', ' right ').

moveq(0,_,_,_) :- !.
moveq(N,A,B,C) :- M is N-1, moveq(M,A,C,B), moveq(M,C,B,A).
hanoiq(N) :- moveq(N,' left ',' centre ', ' right ').

test :- hanoiq(21).
