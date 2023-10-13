:- pragma(apply, [once(true)]).

maplist(_, []).
maplist(Cont, [E1|E1s]) :-
    call(Cont, E1),
    maplist(Cont, E1s).

maplist(_, [], []).
maplist(Cont, [E1|E1s], [E2|E2s]) :-
    call(Cont, E1, E2),
    maplist(Cont, E1s, E2s).

maplist(_, [], [], []).
maplist(Cont, [E1|E1s], [E2|E2s], [E3|E3s]) :-
    call(Cont, E1, E2, E3),
    maplist(Cont, E1s, E2s, E3s).

maplist(_, [], [], [], []).
maplist(Cont, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s]) :-
    call(Cont, E1, E2, E3, E4),
    maplist(Cont, E1s, E2s, E3s, E4s).

maplist(_, [], [], [], [], []).
maplist(Cont, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s]) :-
    call(Cont, E1, E2, E3, E4, E5),
    maplist(Cont, E1s, E2s, E3s, E4s, E5s).

maplist(_, [], [], [], [], [], []).
maplist(Cont, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s]) :-
    call(Cont, E1, E2, E3, E4, E5, E6),
    maplist(Cont, E1s, E2s, E3s, E4s, E5s, E6s).

maplist(_, [], [], [], [], [], [], []).
maplist(Cont, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s], [E7|E7s]) :-
    call(Cont, E1, E2, E3, E4, E5, E6, E7),
    maplist(Cont, E1s, E2s, E3s, E4s, E5s, E6s, E7s).

maplist(_, [], [], [], [], [], [], [], []).
maplist(Cont, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s], [E7|E7s], [E8|E8s]) :-
    call(Cont, E1, E2, E3, E4, E5, E6, E7, E8),
    maplist(Cont, E1s, E2s, E3s, E4s, E5s, E6s, E7s, E8s).

:- meta_predicate(maplist(1, ?)).
:- meta_predicate(maplist(2, ?, ?)).
:- meta_predicate(maplist(3, ?, ?, ?)).
:- meta_predicate(maplist(4, ?, ?, ?, ?)).
:- meta_predicate(maplist(5, ?, ?, ?, ?, ?)).
:- meta_predicate(maplist(6, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(maplist(7, ?, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(maplist(8, ?, ?, ?, ?, ?, ?, ?, ?)).

:- help(maplist(:callable,+list), [iso(false)]).
:- help(maplist(:callable,+list,+list), [iso(false)]).
:- help(maplist(:callable,+list,+list,+list), [iso(false)]).
:- help(maplist(:callable,+list,+list,+list,+list), [iso(false)]).
:- help(maplist(:callable,+list,+list,+list,+list,+list), [iso(false)]).
:- help(maplist(:callable,+list,+list,+list,+list,+list,+list), [iso(false)]).
:- help(maplist(:callable,+list,+list,+list,+list,+list,+list,+list), [iso(false)]).

tasklist(G, L) :-
	tasklist_(L, G).

tasklist_([], _) :- wait.
tasklist_([E|T], G) :-
	task(G, E),
	tasklist_(T, G).

tasklist(G, L1, L2) :-
	tasklist_(L1, L2, G).

tasklist_([], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], G) :-
	task(G, E1, E2),
	tasklist_(T1, T2, G).

tasklist(G, L1, L2, L3) :-
	tasklist_(L1, L2, L3, G).

tasklist_([], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], G) :-
	task(G, E1, E2, E3),
	tasklist_(T1, T2, T3, G).

tasklist(G, L1, L2, L3, L4) :-
	tasklist_(L1, L2, L3, L4, G).

tasklist_([], [], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], G) :-
	task(G, E1, E2, E3, E4),
	tasklist_(T1, T2, T3, T4, G).

tasklist(G, L1, L2, L3, L4, L5) :-
	tasklist_(L1, L2, L3, L4, L5, G).

tasklist_([], [], [], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], G) :-
	task(G, E1, E2, E3, E4, E5),
	tasklist_(T1, T2, T3, T4, T5, G).

tasklist(G, L1, L2, L3, L4, L5, L6) :-
	tasklist_(L1, L2, L3, L4, L5, L6, G).

tasklist_([], [], [], [], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], [E6|T6], G) :-
	task(G, E1, E2, E3, E4, E5, E6),
	tasklist_(T1, T2, T3, T4, T5, T6, G).

tasklist(G, L1, L2, L3, L4, L5, L6, L7) :-
	tasklist_(L1, L2, L3, L4, L5, L6, L7, G).

tasklist_([], [], [], [], [], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], [E6|T6], [E7|T7], G) :-
	task(G, E1, E2, E3, E4, E5, E6, E7),
	tasklist_(T1, T2, T3, T4, T5, T6, T7, G).

:- meta_predicate(tasklist(1, ?)).
:- meta_predicate(tasklist(2, ?, ?)).
:- meta_predicate(tasklist(3, ?, ?, ?)).
:- meta_predicate(tasklist(4, ?, ?, ?, ?)).
:- meta_predicate(tasklist(5, ?, ?, ?, ?, ?)).
:- meta_predicate(tasklist(6, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(tasklist(7, ?, ?, ?, ?, ?, ?, ?)).

:- help(tasklist(:callable,+list), [iso(false)]).
:- help(tasklist(:callable,+list,+list), [iso(false)]).
:- help(tasklist(:callable,+list,+list,+list), [iso(false)]).
:- help(tasklist(:callable,+list,+list,+list,+list), [iso(false)]).
:- help(tasklist(:callable,+list,+list,+list,+list,+list), [iso(false)]).
:- help(tasklist(:callable,+list,+list,+list,+list,+list,+list), [iso(false)]).
:- help(tasklist(:callable,+list,+list,+list,+list,+list,+list,+list), [iso(false)]).


foldl(G, L, V0, V) :-
	foldl_(L, G, V0, V).

foldl_([], _, V, V).
foldl_([H|T], G, V0, V) :-
	call(G, H, V0, V1),
	foldl_(T, G, V1, V).

foldl(G, L1, L2, V0, V) :-
	foldl_(L1, L2, G, V0, V).

foldl_([], [], _, V, V).
foldl_([H1|T1], [H2|T2], G, V0, V) :-
	call(G, H1, H2, V0, V1),
	foldl_(T1, T2, G, V1, V).

foldl(G, L1, L2, L3, V0, V) :-
	foldl_(L1, L2, L3, G, V0, V).

foldl_([], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], G, V0, V) :-
	call(G, H1, H2, H3, V0, V1),
	foldl_(T1, T2, T3, G, V1, V).

foldl(G, L1, L2, L3, L4, V0, V) :-
	foldl_(L1, L2, L3, L4, G, V0, V).

foldl_([], [], [], [], _, V, V).
foldl_([H1|T1], [H2|T2], [H3|T3], [H4|T4], G, V0, V) :-
	call(G, H1, H2, H3, H4, V0, V1),
	foldl_(T1, T2, T3, T4, G, V1, V).

:- help(foldl(:callable,+list,+var,-var), [iso(false)]).
:- help(foldl(:callable,+list,+list,+var,-var), [iso(false)]).
:- help(foldl(:callable,+list,+list,+list,+var,-var), [iso(false)]).
:- help(foldl(:callable,+list,+list,+list,+list,+var,-var), [iso(false)]).


include(G, L, Included) :-
	include_(L, G, Included).

	include_([], _, []).
	include_([X1|Xs1], P, Included) :-
		(   call(P, X1) -> Included = [X1|Included1]
		;   Included = Included1
		),
		include_(Xs1, P, Included1).

:- help(include(:callable,?list), [iso(false)]).


exclude(G, L, Included) :-
	exclude_(L, G, Included).

exclude_([], _, []).
exclude_([X1|Xs1], P, Included) :-
	(   call(P, X1) -> Included = Included1
	;   Included = [X1|Included1]
	),
	exclude_(Xs1, P, Included1).

:- help(exclude(:callable,?list), [iso(false)]).


partition([X|L], Y, [X|L1], L2) :-
	X @< Y, !,
	partition(L, Y, L1, L2).
partition([X|L], Y, L1, [X|L2]) :-
	partition(L, Y, L1, L2).
partition([], _, [], []).

:- help(partition(:callable,?list,?list), [iso(false)]).
