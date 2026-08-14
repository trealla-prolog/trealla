:- initialization(main).

% http://jens-otten.de/tutorial_cade19/
% https://www.philipzucker.com/javascript-automated-proving/

% -----------------------------------------------------------------
% leanseq.pl - A sequent calculus prover implemented in Prolog
% -----------------------------------------------------------------

% operator definitions (TPTP syntax)

:- op( 500, fy, ~).     % negation
:- op(1000, xfy, &).    % conjunction
:- op(1100, xfy, '|').  % disjunction
:- op(1110, xfy, =>).   % implication
:- op( 500, fy, !).     % universal quantifier:  ![X]:
:- op( 500, fy, ?).     % existential quantifier:  ?[X]:
:- op( 500, xfy, :).

% -----------------------------------------------------------------
prove0(F, P) :- prove([] > [F], P).
% -----------------------------------------------------------------

% axiom
prove(G > D, ax(G > D, A)) :- member(A,G), member(A,D), !.

% conjunction
prove(G > D, land(G > D, P) ) :- select1( (A & B) ,G,G1), !,
				prove([A , B | G1] > D, P).

prove(G > D, rand(G > D, P1,P2)) :- select1( (A & B) ,D,D1), !,
				prove(G > [A|D1], P1), prove(G > [B|D1], P2).

% disjunction
prove(G > D, lor(G > D, P1,P2)) :- select1((A | B),G,G1), !,
				prove([A|G1] > D, P1), prove([B|G1] > D, P2).

prove(G > D, ror(G > D, P)) :- select1( (A | B),D,D1), !,
				prove(G > [A,B|D1], P ).

% implication
prove(G > D, limpl(G > D, P1,P2)) :- select1((A => B),G,G1), !,
				prove(G1 > [A|D], P1), prove([B|G1] > D, P2).

prove(G > D, rimpl(G > D, P)) :- select1((A => B),D,D1), !,
				prove([A|G] > [B|D1], P).

% negation
prove(G > D, lneg(G > D, P)) :- select1( ~A,G,G1), !,
				prove(G1 > [A|D], P).

prove(G > D, rneg(G > D, P)) :- select1(~A ,D,D1), !,
				prove([A|G] > D1, P).

% -----------------------------------------------------------------
select1(X,L,L1) :- append(L2,[X|L3],L), append(L2,L3,L1).
% -----------------------------------------------------------------

/*
 Sample theorems to prove:

	((a => b) => a) => a
	a | ~ a
	(~(~a) => a) & (a => ~(~a))
	((a & b) => ~((~a | ~b))) & (~((~a | ~b)) => (a & b))
	a & b | a & ~b | ~a & b | ~a & ~c
	(~b => f) & ((b & f) => ~i) & ((i | ~b) => ~f) => b
	(~b => f) & ((b & f) => ~i) & ((i | ~b) => ~f) => (i & f)
*/

:- set_prolog_flag(double_quotes, codes).  % for presentation

main :-
	prove0((a | ~a), Proof),
	write(Proof), nl.

