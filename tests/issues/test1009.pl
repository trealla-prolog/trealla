:- initialization(main).

:- use_module(library(dcgs)).

map(_G__1, []) --> [].
map(G__1, [E|Es]) --> call(G__1, E), map(G__1, Es).

left([], E0, R, [], 0, [E0|R]).
left([E|L], E0, R, L, E, [E0|R]).

right(L, E0, [], [E0|L], 0, []).
right(L, E0, [E|R], [E0|L], E, R).


term_expansion(term_expansion(T,Ts,[]), term_expansion(T,Ts)).
term_expansion((term_expansion(T,Ts,[]):-G), (term_expansion(T,Ts):-G)).

term_expansion(bb_transition(N0, E0, E1, M, N)) -->
    {   H =.. [N0,E0,L0,R0,N,E,L,R],
        G =.. [M,L0,E1,R0,L,E,R]
    },
    [(H:-G)].

% bb(Name0, Read, Write, Move, Name).
%% BB(5): 47176870 steps
%bb_transition(a, 0, 1, right, b).
%bb_transition(a, 1, 1,  left, c).
%bb_transition(b, 0, 1, right, c).
%bb_transition(b, 1, 1, right, b).
%bb_transition(c, 0, 1, right, d).
%bb_transition(c, 1, 0,  left, e).
%bb_transition(d, 0, 1,  left, a).
%bb_transition(d, 1, 1,  left, d).
%bb_transition(e, 0, 1, right, halt).
%bb_transition(e, 1, 0,  left, a).
% BB(4): 107 steps
bb_transition(a, 0, 1, right, b).
bb_transition(a, 1, 1,  left, b).
bb_transition(b, 0, 1,  left, a).
bb_transition(b, 1, 0,  left, c).
bb_transition(c, 0, 1, right, halt).
bb_transition(c, 1, 1,  left, d).
bb_transition(d, 0, 1, right, d).
bb_transition(d, 1, 0, right, a).

bb_step0(N0) -->
    { G =.. [N0,E0,L0,R0,N,E,L,R] },
    [(bb_step(N0,bb_state(N0,L0,E0,R0),bb_state(N,L,E,R)):-G)].

term_expansion(bb_step(Ls)) -->
    [bb_step(halt,bb_state(halt,L,E,R),bb_state(L,E,R))],
    map(bb_step0, Ls).

bb_step([a,b,c,d]).

term_expansion(bb_state(N0,L0,E0,R0)) -->
    { bb_step(N0, bb_state(N0,L0,E0,R0), S) },
    [S].

term_expansion(bb_start) --> [bb_state(a,[],0,[])].

bb_start.

main :-
	bb_state(L, E, R),
	write([L,E,R]), nl.

