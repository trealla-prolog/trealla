:- module(lists, [
		member/2, memberchk/2,
		select/3, selectchk/3,
		append/2, append/3,
		subtract/3, union/3, intersection/3, is_set/1,
		nth1/3, nth0/3, nth1/4, nth0/4,
		last/2, same_length/2, transpose/2,
		sum_list/2, max_list/2, min_list/2,	% SWI
		list_sum/2, list_max/2, list_min/2,	% Modern
		list_to_set/2, length/2, reverse/2,
		exclude/3, include/3, permutation/2,
		foldl/4, foldl/5, foldl/6,
		flatten/2, list_to_conjunction/2,
		maplist/2, maplist/3, maplist/4, maplist/5, maplist/6, maplist/7, maplist/8,
		tasklist/2, tasklist/3, tasklist/4, tasklist/5, tasklist/6, tasklist/7, tasklist/8
	]).

list_to_conjunction(List0, T) :-
	reverse(List0, List),
	toconjunction_(List, true, T).

toconjunction_([], In, In).
toconjunction_([H|T], true, Out) :- !,
	Out2 = H,
	toconjunction_(T, Out2, Out).
toconjunction_([H|T], In, Out) :-
	Out2 = (H, In),
	toconjunction_(T, Out2, Out).

conjunction_to_list(T, List) :-
	tolist_(T, List).

tolist_((T1,T2), [T1|Rest]) :- !,
	tolist_(T2, Rest).
tolist_(T, [T|[]]).

:- help(list_to_conjunction(?list,?list), [iso(false), desc('Does as it says.')]).

flatten(List, FlatList) :-
	flatten_(List, [], FlatList0),
	!,
	FlatList = FlatList0.

flatten_(Var, Tl, [Var|Tl]) :-
	var(Var),
	!.
flatten_([], Tl, Tl) :- !.
flatten_([Hd|Tl], Tail, List) :-
	!,
	flatten_(Hd, FlatHeadTail, List),
	flatten_(Tl, Tail, FlatHeadTail).
flatten_(NonList, Tl, [NonList|Tl]).

:- help(flatten(?list,?list), [iso(false), desc('Does as it says.')]).

reverse(Xs, Ys) :-
	(	nonvar(Xs) -> reverse_(Xs, Ys, [], Xs)
	;	reverse_(Ys, Xs, [], Ys)
	).

reverse_([], [], YsRev, YsRev).
reverse_([_|Xs], [Y1|Ys], YsPreludeRev, Xss) :-
	reverse_(Xs, Ys, [Y1|YsPreludeRev], Xss).

:- help(reverse(?list,?list), [iso(false), desc('Reverse one list to make another.')]).

append([], []).
append([L0|Ls0], Ls) :-
	append(L0, Rest, Ls),
	append(Ls0, Rest).

:- help(append(?list,?list), [iso(false), desc('The concatention of a list of lists to make a new one.')]).

append([], R, R).
append([X|L], R, [X|S]) :- append(L, R, S).

:- help(append(?term,?term,?term), [iso(false), desc('The concatenation of two lists to make a third.')]).

memberchk(E, List) :-
	'$memberchk'(E, List, Tail),
	(   nonvar(Tail) ->  true
	;   Tail = [_|_], memberchk(E, Tail)
	).

:- help(memberchk(?term,?term), [iso(false), desc('Is element a member of the list.')]).

member(El, [H|T]) :-
	member_(T, El, H).

member_(_, El, El).
member_([H|T], El, _) :-
	member_(T, El, H).

:- help(member(?term,?term), [iso(false), desc('Is element a member of the list.')]).

selectchk(X, L, Rest) :- select(X, L, Rest), !.

:- help(selectchk(+term,?term,?term), [iso(false), desc('Deterministically remove element from list to make a new one.')]).

select(X, [Head|Tail], Rest) :-
	select3_(Tail, Head, X, Rest).

select3_(Tail, Head, Head, Tail).
select3_([Head2|Tail], Head, X, [Head|Rest]) :-
	select3_(Tail, Head2, X, Rest).

:- help(select(+term,+term, ?term), [iso(false), desc('Remove element from a list to make a new one.')]).

subtract([], _, []) :- !.
subtract([H|T], L2, L3) :- memberchk(H, L2), !, subtract(T, L2, L3).
subtract([H|T1], L2, [H|T3]) :- subtract(T1, L2, T3).

:- help(subtract(+list,+list,-list), [iso(false), desc('Delete all elements from set to make a new one.')]).

union([], L, L).
union([H|T], Y, Z):- member(H, Y), !, union(T, Y, Z).
union([H|T], Y, [H|Z]):- union(T, Y, Z).

:- help(union(+list,+list,-list), [iso(false), desc('The union of two sets to produce a third.')]).

intersection([], _, []).
intersection([H|T], Y, [H|Z]) :- member(H, Y), !, intersection(T, Y, Z).
intersection([_|T], Y, Z) :- intersection(T, Y, Z).

:- help(intersection(+list,+list,-list), [iso(false), desc('The intersection of two sets to produce a third.')]).

%% nth0(?N, ?Ls, ?E).
%
% Succeeds if in the N position of the list Ls, we found the element E. The elements start counting from zero.
%
% ```
% ?- nth0(2, [1,2,3,4], 3).
%    true.
% ```
nth0(N, Es0, E) :-
   nonvar(N),
   '$skip_max_list'(Skip, N, Es0,Es1),
   !,
   (  Skip == N
   -> Es1 = [E|_]
   ;  ( var(Es1) ; Es1 = [_|_] ) % a partial or infinite list
   -> R is N-Skip,
      skipn(R,Es1,Es2),
      Es2 = [E|_]
   ).
nth0(N, Es0, E) :-
   can_be(not_less_than_zero, N),
   Es0 = [E0|Es1],
   nth0_el(0,N, E0,E, Es1).

skipn(N0, Es0,Es) :-
   N0>0,
   N1 is N0-1,
   Es0 = [_|Es1],
   skipn(N1, Es1,Es).
skipn(0, Es,Es).

nth0_el(N0,N, E0,E, Es0) :-
   Es0 == [],
   !, % indexing
   N0 = N,
   E0 = E.
nth0_el(N,N, E,E, _).
nth0_el(N0,N, _,E, [E0|Es0]) :-
   N1 is N0+1,
   nth0_el(N1,N, E0,E, Es0).

%% nth1(?N, ?Ls, ?E).
%
% Succeeds if in the N position of the list Ls, we found the element E. The elements start counting from one.
%
% ```
% ?- nth1(2, [1,2,3,4], 2).
%    true.
% ```
nth1(N, Es0, E) :-
   N \== 0,
   nth0(N, [_|Es0], E),
   N \== 0.

skipn(N0, Es0,Es, Xs0,Xs) :-
   N0>0,
   N1 is N0-1,
   Es0 = [E|Es1],
   Xs0 = [E|Xs1],
   skipn(N1, Es1,Es, Xs1,Xs).
skipn(0, Es,Es, Xs,Xs).

%% nth0(?N, ?Ls, ?E, ?Rs).
%
% Succeeds if in the N position of the list Ls, we found the element E and the rest of the list is Rs. The elements start counting from zero.
%
% ```
% ?- nth0(2, [1,2,3,4], 3, [1,2,4]).
%    true.
% ```
nth0(N, Es0, E, Es) :-
   integer(N),
   N >= 0,
   !,
   skipn(N, Es0,Es1, Es,Es2),
   Es1 = [E|Es2].
nth0(N, Es0, E, Es) :-
   can_be(not_less_than_zero, N),
   Es0 = [E0|Es1],
   nth0_elx(0,N, E0,E, Es1, Es).

nth0_elx(N0,N, E0,E, Es0, Es) :-
   Es0 == [],
   !,
   N0 = N,
   E0 = E,
   Es0 = Es.
nth0_elx(N,N, E,E, Es, Es).
nth0_elx(N0,N, E0,E, [E1|Es0], [E0|Es]) :-
   N1 is N0+1,
   nth0_elx(N1,N, E1,E, Es0, Es).

% p.p.8.5

%% nth1(?N, ?Ls, ?E, ?Rs).
%
% Succeeds if in the N position of the list Ls, we found the element E and the rest of the list is Rs. The elements start counting from one.
%
% ```
% ?- nth1(2, [1,2,3,4], 2, [1,3,4]).
%    true.
% ```
nth1(N, Es0, E, Es) :-
   N \== 0,
   nth0(N, [_|Es0], E, [_|Es]),
   N \== 0.



:- help(nth0(?integer,?term,?term), [iso(false), desc('Indexed element (from 0) into list.')]).
:- help(nth0(?integer,?term,?term,?term), [iso(false), desc('Indexed element (from 0) into list with remainder.')]).
:- help(nth1(?integer,?term,?term), [iso(false), desc('Indexed element (from 1) into list.')]).
:- help(nth1(?integer,+term,?term,?term), [iso(false), desc('Indexed element (from 1) into list with remainder.')]).

last([H|T], Last) :- last_(T, H, Last).

last_([], Last, Last).
last_([H|T], _, Last) :- last_(T, H, Last).

:- help(last(+list,-term), [iso(false)]).

same_length([], []).
same_length([_|As], [_|Bs]) :- same_length(As, Bs).

:- help(same_length(?list,?list), [iso(false), desc('Are two list the same length.')]).

list_sum(Xs, Sum) :-
	list_sum_(Xs, 0, Sum).

sum_list(Xs, Sum) :-
	list_sum_(Xs, 0, Sum).

:- help(sum_list(+list,?integer), [iso(false), deprecated(true), desc('Add all values of a list.')]).

list_sum_([], Sum0, Sum) :-
	Sum = Sum0.
list_sum_([H|T], Sum0, Sum) :-
	Sum1 is Sum0 + H,
	list_sum_(T, Sum1, Sum).

:- help(list_sum(+list,?integer), [iso(false), desc('Add all values of a list.')]).

list_max([H|T], Max) :-
	list_max_(T, H, Max).
list_max([], _) :- fail.

max_list([H|T], Max) :-
	list_max_(T, H, Max).
max_list([], _) :- fail.

:- help(max_list(+list,?integer), [iso(false), deprecated(true), desc('Highest value in list.')]).

list_max_([], Max0, Max) :-
	Max = Max0.
list_max_([H|T], Max0, Max) :-
	Max1 is max(H, Max0),
	list_max_(T, Max1, Max).

:- help(list_max(+list,?integer), [iso(false), desc('Highest value in list.')]).

list_min([H|T], Min) :-
	list_min_(T, H, Min).
list_min([], _) :- fail.

min_list([H|T], Min) :-
	list_min_(T, H, Min).
min_list([], _) :- fail.

:- help(min_list(+list,?integer), [iso(false), deprecated(true), desc('Lowest value in list.')]).

list_min_([], Min0, Min) :-
	Min = Min0.
list_min_([H|T], Min0, Min) :-
	Min1 is min(H, Min0),
	list_min_(T, Min1, Min).

:- help(list_min(+list,?integer), [iso(false), desc('Lowest value in list.')]).

list_to_set(Ls0, Ls) :-
		maplist(with_var, Ls0, LVs0),
		keysort(LVs0, LVs),
		same_elements(LVs),
		pick_firsts(LVs0, Ls).

pick_firsts([], []).
pick_firsts([E-V|EVs], Fs0) :-
		(   V == visited ->
			Fs0 = Fs
		;   V = visited,
			Fs0 = [E|Fs]
		),
		pick_firsts(EVs, Fs).

with_var(E, E-_).

same_elements([]).
same_elements([EV|EVs]) :-
		foldl(unify_same, EVs, EV, _).

unify_same(E-V, Prev-Var, E-V) :-
		(   Prev == E ->
			Var = V
		;   true
		).

is_set(Set) :-
	'$skip_list'(Len, Set, Tail),
	Tail == [],
	sort(Set, Sorted),
	length(Sorted, Len)
  .
:- help(is_set(+list), [iso(false), desc('Is it a set.')]).

length(Xs0, N) :-
   '$skip_max_list'(M, N, Xs0,Xs),
   !,
   (  Xs == [] -> N = M
   ;  nonvar(Xs) -> var(N), Xs = [_|_], resource_error(finite_memory,length/2)
   ;  nonvar(N) -> R is N-M, length_rundown(Xs, R)
   ;  N == Xs -> failingvarskip(Xs), resource_error(finite_memory,length/2)
   ;  length_addendum(Xs, N, M)
   ).
length(_, N) :-
   integer(N), !,
   domain_error(not_less_than_zero, N, length/2).
length(_, N) :-
   type_error(integer, N, length/2).

length_rundown(Xs, 0) :- !, Xs = [].
length_rundown(Vs, N) :-
    '$unattributed_var'(Vs), % unconstrained
    !,
    '$det_length_rundown'(Vs, N).
length_rundown([_|Xs], N) :- % force unification
    N1 is N-1,
    length(Xs, N1). % maybe some new info on Xs

failingvarskip(Xs) :-
    '$unattributed_var'(Xs), % unconstrained
    !.
failingvarskip([_|Xs0]) :- % force unification
    '$skip_max_list'(_, _, Xs0,Xs),
    (  nonvar(Xs) -> Xs = [_|_]
	 ;  failingvarskip(Xs)
    ).

length_addendum([], N, N).
length_addendum([_|Xs], N, M) :-
    M1 is M + 1,
    length_addendum(Xs, N, M1).

:- help(length(?term,?integer), [iso(false), desc('Number of elements in list.')]).

list_first_rest([L|Ls], L, Ls).

transpose_(_, Fs, Lists0, Lists) :-
        maplist(list_first_rest, Lists0, Fs, Lists).

lists_transpose_([], []).
lists_transpose_([L|Ls], Ts) :-
        maplist(same_length(L), Ls),
        foldl(transpose_, L, Ts, [L|Ls], _).

transpose(Ls, Ts) :-
        lists_transpose_(Ls, Ts).

:- help(transpose(?list,?list), [iso(false), desc('Transpose list of lists.')]).

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

:- help(foldl(:callable,+list,+var,-var), [iso(false)]).
:- help(foldl(:callable,+list,+list,+var,-var), [iso(false)]).
:- help(foldl(:callable,+list,+list,+list,+var,-var), [iso(false)]).
:- meta_predicate(foldl(3, ?, ?, ?)).
:- meta_predicate(foldl(4, ?, ?, ?, ?)).
:- meta_predicate(foldl(5, ?, ?, ?, ?, ?)).
:- meta_predicate(foldl(6, ?, ?, ?, ?, ?, ?)).


include(G, L, Included) :-
	include_(L, G, Included).

include_([], _, []).
include_([X1|Xs1], P, Included) :-
	(   call(P, X1) -> Included = [X1|Included1]
	;   Included = Included1
	),
	include_(Xs1, P, Included1).

:- help(include(:callable,?list), [iso(false)]).
:- meta_predicate(include(2, ?, ?)).

exclude(G, L, Included) :-
	exclude_(L, G, Included).

exclude_([], _, []).
exclude_([X1|Xs1], P, Included) :-
	(   call(P, X1) -> Included = Included1
	;   Included = [X1|Included1]
	),
	exclude_(Xs1, P, Included1).

:- help(exclude(:callable,?list), [iso(false)]).
:- meta_predicate(exclude(2, ?, ?)).

permutation(Xs, Ys) :-
    '$skip_max_list'(Xlen, _, Xs, XTail),
    '$skip_max_list'(Ylen, _, Ys, YTail),
    (   XTail == [], YTail == []            % both proper lists
    ->  Xlen == Ylen
    ;   var(XTail), YTail == []             % partial, proper
    ->  length(Xs, Ylen)
    ;   XTail == [], var(YTail)             % proper, partial
    ->  length(Ys, Xlen)
    ;   var(XTail), var(YTail)              % partial, partial
    ->  length(Xs, Len),
        length(Ys, Len)
    ;   must_be(list, Xs),                  % either is not a list
        must_be(list, Ys)
    ),
    perm_(Xs, Ys).

perm_([], []).
perm_(List, [First|Perm]) :-
    select(First, List, Rest),
    perm_(Rest, Perm).

:- help(permutation(?list,?list), [iso(false)]).

:- use_module(library(gensym)).

user:goal_expansion(maplist(G, L1), Goal) :-
	nonvar(G), !,
	gensym(maplist_, U),
	Goal =.. [U,L1],
	G1 =.. [U,[]],
	user:'$assertz'(G1),
	G2a =.. [U,[E1|T1]],
	G2b =.. [U,T1],
	user:'$assertz'((G2a :- call(G, E1), G2b)),
	true.
user:goal_expansion(maplist(G, L1), maplist(G, L1)).

user:goal_expansion(maplist(G, L1, L2), Goal) :-
	nonvar(G), !,
	gensym(maplist_, U),
	Goal =.. [U,L1,L2],
	G1 =.. [U,[],[]],
	user:'$assertz'(G1),
	G2a =.. [U,[E1|T1],[E2|T2]],
	G2b =.. [U,T1,T2],
	user:'$assertz'((G2a :- call(G, E1, E2), G2b)),
	true.
user:goal_expansion(maplist(G, L1, L2), maplist(G, L1, L2)).

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
	call_task(G, E),
	tasklist_(T, G).

tasklist(G, L1, L2) :-
	tasklist_(L1, L2, G).

tasklist_([], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], G) :-
	call_task(G, E1, E2),
	tasklist_(T1, T2, G).

tasklist(G, L1, L2, L3) :-
	tasklist_(L1, L2, L3, G).

tasklist_([], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], G) :-
	call_task(G, E1, E2, E3),
	tasklist_(T1, T2, T3, G).

tasklist(G, L1, L2, L3, L4) :-
	tasklist_(L1, L2, L3, L4, G).

tasklist_([], [], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], G) :-
	call_task(G, E1, E2, E3, E4),
	tasklist_(T1, T2, T3, T4, G).

tasklist(G, L1, L2, L3, L4, L5) :-
	tasklist_(L1, L2, L3, L4, L5, G).

tasklist_([], [], [], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], G) :-
	call_task(G, E1, E2, E3, E4, E5),
	tasklist_(T1, T2, T3, T4, T5, G).

tasklist(G, L1, L2, L3, L4, L5, L6) :-
	tasklist_(L1, L2, L3, L4, L5, L6, G).

tasklist_([], [], [], [], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], [E6|T6], G) :-
	call_task(G, E1, E2, E3, E4, E5, E6),
	tasklist_(T1, T2, T3, T4, T5, T6, G).

tasklist(G, L1, L2, L3, L4, L5, L6, L7) :-
	tasklist_(L1, L2, L3, L4, L5, L6, L7, G).

tasklist_([], [], [], [], [], [], [], _) :- wait.
tasklist_([E1|T1], [E2|T2], [E3|T3], [E4|T4], [E5|T5], [E6|T6], [E7|T7], G) :-
	call_task(G, E1, E2, E3, E4, E5, E6, E7),
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
