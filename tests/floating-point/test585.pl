% Solving polynomial equations of degree 4
% See http://alain.colmerauer.free.fr/alcol/ArchivesPublications/Equation4/Equation4.pdf

% Liste des racines dun polynome
'https://josd.github.io/eye/ns#roots'(P,L) :-
    findall(Z,racine(P,Z),L).

% Racine dun polynome
racine([A,B],Z) :-
    est(Z,moins(div(B,A))).
racine([A,B,C],Z) :-
    est(P,div(B,fois([2,0],A))),
    est(Q,div(C,A)),
    est(Z,add(moins(P),fois(racarreeun,racine(2,moins(carre(P),Q))))).
racine([A,B,C,D],Zp) :-
    est(T,div(B,fois([-3,0],A))),
    est(P,div(add(fois([3,0],fois(A,carre(T))),add(fois([2,0],fois(B,T)),C)),A)),
    est(Q,div(add(fois(A,cube(T)),add(fois(B,carre(T)),add(fois(C,T),D))),A)),
    solutionCardan(P,Q,Z),
    est(Zp,add(Z,T)).
racine([A,B,C,D,E],Zp) :-
    est(T,div(B,fois([-4,0],A))),
    est(P,div(add(fois([6,0],fois(A,carre(T))),add(fois([3,0],fois(B,T)),C)),A)),
    est(Q,div(add(fois(fois([4,0],A),cube(T)),add(fois(fois([3,0],B),carre(T)),add(fois(fois([2,0],C),T),D))),A)),
    est(R,div(add(fois(A,pquatre(T)),add(fois(B,cube(T)),add(fois(C,carre(T)),add(fois(D,T),E)))),A)),
    solutionLagrange(P,Q,R,Z),
    est(Zp,add(Z,T)).

% Polynome a partir de ses racines
polynome(L,P) :-
    polynome(L,[[1,0]],P).

polynome([],P0,P0).
polynome([X|L],P0,P4) :-
    conc(P0,[[0,0]],P1),
    est(Xp,moins(X)),
    foisl(Xp,P0,P2),
    addll(P1,[[0,0]|P2],P3),
    polynome(L,P3,P4).

conc([],L,L).
conc([E|L],Lp,[E|Lpp]) :-
    conc(L,Lp,Lpp).

foisl(_,[],[]).
foisl(X,[Y|L],[Z|Lp]) :-
    est(Z,fois(X,Y)),
    foisl(X,L,Lp).

addl(_,[],[]).
addl(X,[Y|L],[Z|Lp]) :-
    est(Z,addl(X,Y)),
    addl(X,L,Lp).

addll([],[],[]).
addll([X|L],[Y|Lp],[Z|Lpp]) :-
    est(Z,add(X,Y)),
    addll(L,Lp,Lpp).

% Solution de lequation du troisieme degre selon Cardan
solutionCardan(P,Q,Z) :-
    nul(P),
    est(Z,fois(racubiqueun,racine(3,moins(Q)))).
solutionCardan(Pp,Qp,Z) :-
    nonnul(Pp),
    est(P,div(Pp,[3,0])),
    est(Q,div(Qp,[2,0])),
    est(Raccubique,fois(racubiqueun,racine(3,moins(racine(2,add(carre(Q),cube(P))),Q)))),
    est(Z,moins(Raccubique,div(P,Raccubique))).

% Solutions de lequation du quatrieme degre selon Lagrange
solutionLagrange(P,Q,R,Z) :-
    est(A,[1,0]),
    est(B,fois([2,0],P)),
    est(C,moins(carre(P),fois([4,0],R))),
    est(D,moins(carre(Q))),
    'https://josd.github.io/eye/ns#roots'([A,B,C,D],[Y1,Y2,Y3]),
    est(Y1p,racine(2,Y1)),
    est(Y2p,racine(2,Y2)),
    est(Y3p,racine(2,Y3)),
    est(U1,div(add(Y1p,add(Y2p,Y3p)),[2,0])),
    est(U2,div(moins(Y1p,add(Y2p,Y3p)),[2,0])),
    est(U3,div(moins(Y3p,add(Y1p,Y2p)),[2,0])),
    est(U4,div(moins(Y2p,add(Y1p,Y3p)),[2,0])),
    est(V1,fois(U1,fois(U2,U3))),
    est(V2,fois(U1,fois(U2,U4))),
    est(V3,fois(U1,fois(U3,U4))),
    est(V4,fois(U2,fois(U3,U4))),
    epsilon(E,moins(add(V1,add(V2,add(V3,V4)))),Q),
    dans(U,[U1,U2,U3,U4]),
    est(Z,fois(E,U)).

epsilon([1,0],_,Q) :-
    nul(Q).
epsilon(E,S,Q) :-
    nonnul(Q),
    est(E,div(S,Q)).

dans(U,[U|_]).
dans(U,[_|L]) :-
    dans(U,L).

% Valeurs de lenchainement des operations sur les complexes
est(Z,Z) :-
    Z = [_,_].
est(Z,T) :-
    T =.. [F],
    atom(F),
    Tp =.. [F,Z],
    Tp.
est(Z,T) :-
    T =.. [F,X],
    est(Xp,X),
    Tp =.. [F,Xp,Z],
    Tp.
est(Z,T) :-
    T =.. [F,X,Y],
    F \== racine,
    F \== .,
    est(Xp,X),
    est(Yp,Y),
    Tp =.. [F,Xp,Yp,Z],
    Tp.
est(Z,racine(N,X)) :-
    est(Xp,X),
    racine(N,Xp,Z).

% Operations sur les complexes
moins([X1,X2],[Y1,Y2]) :-
    Y1 is -X1,
    Y2 is -X2.

moins([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1-Y1,
    Z2 is X2-Y2.

add([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1+Y1,
    Z2 is X2+Y2.

fois([X1,X2],[Y1,Y2],[Z1,Z2]) :-
    Z1 is X1*Y1-X2*Y2,
    Z2 is X1*Y2+X2*Y1.

invers([X1,X2],[Y1,Y2]) :-
    Y1 is X1/(X1**2+X2**2),
    Y2 is -X2/(X1**2+X2**2).

div(X,Y,Z) :-
    invers(Y,Yp),
    fois(X,Yp,Z).

carre(X,Y) :-
    fois(X,X,Y).

cube(X,Y) :-
    carre(X,Xp),
    fois(X,Xp,Y).

pquatre(X,Y) :-
    carre(X,Xp),
    carre(Xp,Y).

racarreeun([1,0]).
racarreeun([-1,0]).

racubiqueun([1,0]).
racubiqueun([X,Y]) :-
    X is -1/2,
    Y is sqrt(3)/2.
racubiqueun([X,Y]) :-
    X is -1/2,
    Y is -sqrt(3)/2.

racine(_,X,[0,0]) :-
    nul(X).
racine(N,X,Y) :-
    nonnul(X),
    polaire(X,[R,T]),
    root(N,R,Rp),
    Tp is T/N,
    cartesien([Rp,Tp],Y).

root(N,X,Y) :-
    Y is exp(log(X)/N).

polaire([X,Y],[R,Tp]) :-
    R is sqrt(X**2+Y**2),
    T is acos(abs(X)/R),
    cadran(X,Y,T,Tp).

cadran(X,Y,T,Tp) :-
    X >= 0,
    Y >= 0,
    Tp = T.
cadran(X,Y,T,Tp) :-
    X < 0,
    Y >= 0,
    Tp is pi-T.
cadran(X,Y,T,Tp) :-
    X < 0,
    Y < 0,
    Tp is T+pi.
cadran(X,Y,T,Tp) :-
    X >= 0,
    Y < 0,
    Tp is 2*pi-T.

cartesien([R,T],[X1,X2]) :-
    X1 is R*cos(T),
    X2 is R*sin(T).

% Problemes de zero
nul([X,Y]) :-
    nulreel(X),
    nulreel(Y),
    !.

nonnul(Z) :-
    nul(Z),
    !,
    fail.
nonnul(_).

nulreel(0) :-
    !.
nulreel(0.0) :-
    !.
nulreel(-0.0).

% query
query('https://josd.github.io/eye/ns#roots'([[1,0],[-10,0],[35,0],[-50,0],[24,0]],_ANSWER)).
query('https://josd.github.io/eye/ns#roots'([[1,0],[-9,-5],[14,33],[24,-44],[-26,0]],_ANSWER)).

run :-
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.

:- initialization(run).
