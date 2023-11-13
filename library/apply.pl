:- pragma(apply, [once(true)]).

maplist(G, L) :-
	maplist_(L, G).

maplist_([], _).
maplist_([E|T], G) :-
	call(G, E),
	maplist_(T, G).

maplist(G, L1, L2) :-
	maplist_(L1, L2, G).

maplist_([], [], _).
maplist_([E1|T1], [E2|T2], G) :-
	call(G, E1, E2),
	maplist_(T1, T2, G).

maplist(G, L1, L2, L3) :-
	maplist_(L1, L2, L3, G).

maplist_([], [], [], _).
maplist_([E1|T1], [E2|T2], [E3|T3], G) :-
	call(G, E1, E2, E3),
	maplist_(T1, T2, T3, G).

maplist(G, L1, L2, L3, L4) :-
	maplist_(L1, L2, L3, L4, G).

maplist_([], [], [], [], _).
maplist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], G) :-
	call(G, E1, E2, E3, E4),
	maplist_(T1, T2, T3, T4, G).

maplist(G, L1, L2, L3, L4, L5) :-
	maplist_(L1, L2, L3, L4, L5, G).

maplist_([], [], [], [], [], _).
maplist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], G) :-
	call(G, E1, E2, E3, E4, E5),
	maplist_(T1, T2, T3, T4, T5, G).

maplist(G, L1, L2, L3, L4, L5, L6) :-
	maplist_(L1, L2, L3, L4, L5, L6, G).

maplist_([], [], [], [], [], [], _).
maplist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], [E6|T6], G) :-
	call(G, E1, E2, E3, E4, E5, E6),
	maplist_(T1, T2, T3, T4, T5, T6, G).

maplist(G, L1, L2, L3, L4, L5, L6, L7) :-
	maplist_(L1, L2, L3, L4, L5, L6, L7, G).

maplist_([], [], [], [], [], [], [], _).
maplist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], [E6|T6], [E7|T7], G) :-
	call(G, E1, E2, E3, E4, E5, E6, E7),
	maplist_(T1, T2, T3, T4, T5, T6, T7, G).

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
