% Explanation-based learning uses an explicitly represented domain theory
% to construct an explanation of a training example, usually a proof that
% the example logically follows from the theory. By generalizing from the
% explanation of the instance, rather than from the instance itself,
% explanation-based learning filters noise, selects relevant aspects of
% experience, and organizes training data into a coherent structure.

:- initialization(main).
:- discontiguous(cup/1).

:- dynamic(cup/1).
:- dynamic(holds_liquid/1).
:- dynamic(liftable/1).
:- dynamic(light/1).
:- dynamic(small/1).
:- dynamic(part/2).
:- dynamic(owns/2).
:- dynamic(points_up/1).
:- dynamic(concave/1).
:- dynamic(color/2).
:- dynamic(made_of/2).

main :-
	ebl(cup(obj1), cup(_), Rule),
	write(Rule), nl.

% domain theory
cup(X) :-
	liftable(X),
	holds_liquid(X).

holds_liquid(Z) :-
	part(Z, W),
	concave(W),
	points_up(W).

liftable(Y) :-
	light(Y),
	part(Y, handle).

light(A):-
	small(A).
light(A):-
	made_of(A, feathers).

% training example
cup(obj1).
small(obj1).

part(obj1, bottom).
part(obj1, bowl).
part(obj1, handle).

owns(bob, obj1).

points_up(bowl).

concave(bowl).

color(obj1, red).

made_of(obj2, feathers).

% operational criteria
operational(small(_)).
operational(part(_, _)).
operational(owns(_, _)).
operational(points_up(_)).
operational(concave(_)).

% explanation-based learning
ebl(Goal, Gen_goal, (Gen_goal :- Premise)) :-
	ebl(Goal, Gen_goal, _, Gen_proof),
	extract_support(Gen_proof, Premise).

ebl((A, B), (GenA, GenB), (AProof, BProof), (GenAProof, GenBProof)) :-
    !,
	ebl(A, GenA, AProof, GenAProof),
	ebl(B, GenB, BProof, GenBProof).
ebl(A, GenA, A, GenA) :-
	clause(A, true).
ebl(A, GenA, (A :- Proof), (GenA :- GenProof)) :-
	clause(GenA, GenB),
	write(clause(GenA, GenB)), nl,
	copy_term(GenA-GenB, A-B),
	B \= true,
	ebl(B, GenB, Proof, GenProof).

extract_support(Proof, Proof) :-
	operational(Proof).
extract_support((A :- _), A) :-
	operational(A).
extract_support((AProof, BProof), (A, B)) :-
	extract_support(AProof, A),
	extract_support(BProof, B).
extract_support((_ :- Proof), B) :-
	extract_support(Proof, B).
