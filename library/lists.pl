:- module(lists, [
		member/2, memberchk/2,
		select/3, selectchk/3,
		append/2, append/3,
		subtract/3, union/3, intersection/3, is_set/1,
		nth1/3, nth0/3, nth1/4, nth0/4,
		last/2, flatten/2, same_length/2, transpose/2,
		sum_list/2, prod_list/2, max_list/2, min_list/2,	% SWI
		list_sum/2, list_prod/2, list_max/2, list_min/2,	% Modern
		list_to_conjunction/2, conjunction_to_list/2,
		list_to_set/2, numlist/3, length/2, reverse/2,
		partition/4, exclude/3, include/3,
		foldl/4, foldl/5, foldl/6, foldl/7
	]).

/*  Author:        Mark Thom, Jan Wielemaker, and Richard O'Keefe
	Copyright (c)  2018-2021, Mark Thom
	Copyright (c)  2002-2020, University of Amsterdam
							  VU University Amsterdam
							  SWI-Prolog Solutions b.v.
	All rights reserved.
	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions
	are met:
	1. Redistributions of source code must retain the above copyright
	   notice, this list of conditions and the following disclaimer.
	2. Redistributions in binary form must reproduce the above copyright
	   notice, this list of conditions and the following disclaimer in
	   the documentation and/or other materials provided with the
	   distribution.
	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
	"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
	LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
	FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
	COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
	INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
	BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
	CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
	LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
	ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
	POSSIBILITY OF SUCH DAMAGE.
*/

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

nth1_orig(N, Es, E) :-
	can_be(integer, N),
	can_be(list_or_partial_list, Es),
	(   integer(N) ->
		must_be(N, not_less_than_zero, nth1/3, _),
		N1 is N - 1,
		nth0_index(N1, Es, E)
	;   nth0_search(N1, Es, E),
		N is N1 + 1
	).

nth0_orig(N, Es, E) :-
	can_be(integer, N),
	can_be(list_or_partial_list, Es),
	(   integer(N) ->
		must_be(N, not_less_than_zero, nth0/3, _),
		nth0_index(N, Es, E)
	;   nth0_search(N, Es, E)
	).

nth0_index(0, [E|_], E) :- !.
nth0_index(N, [_|Es], E) :-
	N > 0,
	N1 is N - 1,
	nth0_index(N1, Es, E).

nth0_search(N, Es, E) :-
	nth0_search(0, N, Es, E).

nth0_search(N, N, [E|_], E).
nth0_search(N0, N, [_|Es], E) :-
	N1 is N0 + 1,
	nth0_search(N1, N, Es, E).

nth1(N, Es0, E) :-
	nonvar(N),
	N > 0,
	N1 is N - 1,
	'$skip_max_list'(N1, N1, Es0, Es),
	!,
	Es = [E|_].
nth1(N, Es, E) :-
	nth1_orig(N, Es, E).

:- help(nth1(?integer,?term,?term), [iso(false), desc('Indexed element (from 1) into list.')]).

nth0(N, Es0, E) :-
	nonvar(N),
	'$skip_max_list'(N, N, Es0, Es),
	!,
	Es = [E|_].
nth0(N, Es, E) :-
	nth0_orig(N, Es, E).

:- help(nth0(?integer,?term,?term), [iso(false), desc('Indexed element (from 0) into list.')]).

nth1(Nth, List, Element, Rest) :-
	nth(Element, List, 1, Nth, Rest).

:- help(nth1(?integer,+term,?term,?term), [iso(false)]).

nth0(Nth, List, Element, Rest) :-
	nth(Element, List, 0, Nth, Rest).

:- help(nth0(?integer,?term,?term,?term), [iso(false), desc('Indexed element (from 0) into list with remainder.')]).

nth(Element, List, Acc, Nth, Rest) :-
	(	integer(Nth),
		Nth >= Acc,
		nth_aux(NthElement, List, Acc, Nth, Rest) ->
		Element = NthElement
	;	var(Nth),
		nth_aux(Element, List, Acc, Nth, Rest)
	).

nth_aux(Element, [Element| Rest], Position, Position, Rest).
nth_aux(Element, [Head| Tail], Position0, Position, [Head| Rest]) :-
	Position1 is Position0 + 1,
	nth_aux(Element, Tail, Position1, Position, Rest).

last([X|Xs], Last) :- last_(Xs, X, Last).

last_([], Last, Last).
last_([X|Xs], _, Last) :- last_(Xs, X, Last).

:- help(last(+list,-term), [iso(false)]).

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

:- help(flatten(+list,-list), [iso(false)]).

same_length([], []).
same_length([_|As], [_|Bs]) :- same_length(As, Bs).

:- help(same_length(?list,?list), [iso(false), desc('Are two list the same length.')]).

list_sum(Xs, Sum) :-
	list_sum_(Xs, 0, Sum).

sum_list(Xs, Sum) :-
	list_sum_(Xs, 0, Sum).

list_sum_([], Sum0, Sum) :-
	Sum = Sum0.
list_sum_([X|Xs], Sum0, Sum) :-
	Sum1 is Sum0 + X,
	list_sum_(Xs, Sum1, Sum).

:- help(list_sum(+list,?integer), [iso(false), desc('Add all values of a list.')]).
:- help(sum_list(+list,?integer), [iso(false), desc('Add all values of a list.')]).

list_prod(Xs, Prod) :-
	list_prod_(Xs, 1, Prod).

prod_list(Xs, Prod) :-
	list_prod_(Xs, 1, Prod).

list_prod_([], Prod0, Prod) :-
	Prod = Prod0.
list_prod_([X|Xs], Prod0, Prod) :-
	Prod1 is Prod0 * X,
	list_prod_(Xs, Prod1, Prod).

:- help(list_prod(+list,?integer), [iso(false), desc('Multiplay all values of a list.')]).
:- help(prod_list(+list,?integer), [iso(false), desc('Multiplay all values of a list.')]).

list_max([H|T], Max) :-
	list_max_(T, H, Max).
list_max([], _) :- fail.

max_list([H|T], Max) :-
	list_max_(T, H, Max).
max_list([], _) :- fail.

list_max_([], Max0, Max) :-
	Max = Max0.
list_max_([H|T], Max0, Max) :-
	Max1 is max(H, Max0),
	list_max_(T, Max1, Max).

:- help(list_max(+list,?integer), [iso(false), desc('Highest value in list.')]).
:- help(max_list(+list,?integer), [iso(false), desc('Highest value in list.')]).

list_min([H|T], Min) :-
	list_min_(T, H, Min).
list_min([], _) :- fail.

min_list([H|T], Min) :-
	list_min_(T, H, Min).
min_list([], _) :- fail.

list_min_([], Min0, Min) :-
	Min = Min0.
list_min_([H|T], Min0, Min) :-
	Min1 is min(H, Min0),
	list_min_(T, Min1, Min).

:- help(list_min(+list,?integer), [iso(false), desc('Lowest value in list.')]).
:- help(min_list(+list,?integer), [iso(false), desc('Lowest value in list.')]).

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

list_to_set(Ls0, Ls) :-
		maplist(lists:with_var, Ls0, LVs0),
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
		foldl(lists:unify_same, EVs, EV, _).

unify_same(E-V, Prev-Var, E-V) :-
		(   Prev == E ->
			Var = V
		;   true
		).

numlist(L, U, Ns) :-
	must_be(L, integer, numlist/3, _),
	must_be(U, integer, numlist/3, _),
	L =< U,
	numlist_(L, U, Ns).

numlist_(U, U, List) :-
	!,
	List = [U].
numlist_(L, U, [L|Ns]) :-
	L2 is L+1,
	numlist_(L2, U, Ns).

:- help(numlist(+integer,+integer,?list), [iso(false), desc('Produce list of numbers from start to finish.')]).

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
length_rundown([_|Xs], N) :-
	N1 is N-1,
	length_rundown(Xs, N1).

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

transpose(Ls, Ts) :-
        lists_transpose(Ls, Ts).

:- help(transpose(?list,?list), [iso(false), desc('Transpose list of lists.')]).

lists_transpose([], []).
lists_transpose([L|Ls], Ts) :-
        maplist(lists:same_length(L), Ls),
        foldl(lists:transpose_, L, Ts, [L|Ls], _).

transpose_(_, Fs, Lists0, Lists) :-
        maplist(lists:list_first_rest, Lists0, Fs, Lists).

list_first_rest([L|Ls], L, Ls).


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
