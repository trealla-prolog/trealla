:- module(lists, [
		member/2, memberchk/2,
		select/3, selectchk/3,
		append/2, append/3,
		subtract/3, union/3, intersection/3, is_set/1,
		nth1/3, nth0/3, nth1/4, nth0/4,
		last/2, flatten/2, same_length/2,
		sum_list/2, prod_list/2, max_list/2, min_list/2,
		list_to_conjunction/2, conjunction_to_list/2,
		list_to_set/2,
		numlist/3,
		length/2, reverse/2
	]).

/*  Author:        Andrew Davison, Mark Thom, Jan Wielemaker, and Richard O'Keefe
    Copyright (c)  2022,      Andrew Davison
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
    (	nonvar(Xs)
	->	reverse_(Xs, Ys, [], Xs)
    ;	reverse_(Ys, Xs, [], Ys)
    ).

reverse_([], [], YsRev, YsRev).
reverse_([_|Xs], [Y1|Ys], YsPreludeRev, Xss) :-
    reverse_(Xs, Ys, [Y1|YsPreludeRev], Xss).

:- help(reverse(?list,?list), [iso(false),desc('Reverse one list to make another.')]).

append([], []).
append([L0|Ls0], Ls) :-
    append(L0, Rest, Ls),
    append(Ls0, Rest).

:- help(append(+list,?list), [iso(false),desc('The concatention of a list of lists to make one list.')]).

append([], R, R).
append([X|L], R, [X|S]) :- append(L, R, S).

:- help(append(?list,?list,?list), [iso(false),desc('The concatenation of to two lists to make a third.')]).

memberchk(X, Xs) :- member(X, Xs), !.

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

:- help(member(?term,?list), [iso(false),desc('Is element a member of the list.')]).

selectchk(X, L, Rest) :- select(X, L, Rest), !.

:- help(selectchk(+term,+list,-rest), [iso(false),desc('Deterministically remove element from list to make another list.')]).

select(X, [X|T], T).
select(X, [H|T], [H|Rest]) :- select(X, T, Rest).

:- help(select(+term,+list), [iso(false),desc('Remove element from a list to make another list.')]).

subtract([], _, []) :- !.
subtract([H|T], L2, L3) :- memberchk(H, L2), !, subtract(T, L2, L3).
subtract([H|T1], L2, [H|T3]) :- subtract(T1, L2, T3).

:- help(subtract(+list,+list,-list), [iso(false),desc('Delete all elements from set to make another.')]).

union([], L, L).
union([H|T], Y, Z):- member(H, Y), !, union(T, Y, Z).
union([H|T], Y, [H|Z]):- union(T, Y, Z).

:- help(union(+list,+list,-list), [iso(false),desc('The union of two sets to produce a third.')]).

intersection([], _, []).
intersection([H|T], Y, [H|Z]) :- member(H, Y), !, intersection(T, Y, Z).
intersection([_|T], Y, Z) :- intersection(T, Y, Z).

:- help(intersection(+list,+list,-list), [iso(false),desc('The intersection of two sets to produce a third.')]).

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
	'$skip_max_list'(N1, N1, Es0,Es),
	!,
	Es = [E|_].
nth1(N, Es, E) :-
	nth1_orig(N, Es, E).

:- help(nth1(+integer,+list,-term), [iso(false),desc('Indexed element (from 1) into list.')]).

nth0(N, Es0, E) :-
	nonvar(N),
	'$skip_max_list'(N, N, Es0,Es),
	!,
	Es = [E|_].
nth0(N, Es, E) :-
	nth0_orig(N, Es, E).

:- help(nth0(+integer,+list,-term), [iso(false),desc('Indexed element (from 0) into list.')]).

nth1(Nth, List, Element, Rest) :-
	nth(Element, List, 1, Nth, Rest).

:- help(nth1(+integer,+list,-term,-list), [iso(false)]).

nth0(Nth, List, Element, Rest) :-
	nth(Element, List, 0, Nth, Rest).

:- help(nth0(+integer,+list,-term,-list), [iso(false),desc('Indexed element (from 0) into list with remainder.')]).

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

:- help(same_length(?list,?list), [iso(false),desc('Are two list the same length.')]).

sum_list(Xs, Sum) :-
	sum_list_(Xs, 0, Sum).

sum_list_([], Sum0, Sum) :-
	Sum = Sum0.
sum_list_([X|Xs], Sum0, Sum) :-
	Sum1 is Sum0 + X,
	sum_list_(Xs, Sum1, Sum).

:- help(sum_list(+list,?integer), [iso(false),desc('Add all values of a list.')]).

prod_list(Xs, Prod) :-
	prod_list_(Xs, 1, Prod).

prod_list_([], Prod0, Prod) :-
	Prod = Prod0.
prod_list_([X|Xs], Prod0, Prod) :-
	Prod1 is Prod0 * X,
	prod_list_(Xs, Prod1, Prod).

:- help(prod_list(+list,?integer), [iso(false),desc('Multiplay all values of a list.')]).

max_list([H|T], Max) :-
	max_list_(T, H, Max).
max_list([], _) :- fail.

max_list_([], Max0, Max) :-
	Max = Max0.
max_list_([H|T], Max0, Max) :-
	Max1 is max(H, Max0),
	max_list_(T, Max1, Max).

:- help(max_list(+list,?integer), [iso(false),desc('Highest value in list.')]).

min_list([H|T], Min) :-
	min_list_(T, H, Min).
min_list([], _) :- fail.

min_list_([], Min0, Min) :-
	Min = Min0.
min_list_([H|T], Min0, Min) :-
	Min1 is min(H, Min0),
	min_list_(T, Min1, Min).

:- help(min_list(+list,?integer), [iso(false),desc('Lowest value in list.')]).

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

:- help(numlist(+integer,+integer,?list), [iso(false),desc('Produce list of numbers from start to finish.')]).

is_set(Set) :-
	'$skip_list'(Len, Set, Tail),
	Tail == [],
	sort(Set, Sorted),
	length(Sorted, Len)
  .
:- help(is_set(+list), [iso(false),desc('Is it a set.')]).

length(Xs0, N) :-
   '$skip_max_list'(M, N, Xs0, Xs),
   !,
   (  Xs == [] -> N = M
   ;  nonvar(Xs) -> var(N), Xs = [_|_], throw(error(resource_error(finite_memory),length/2))
   ;  nonvar(N) -> R is N-M, length_rundown(Xs, R)
   ;  N == Xs -> throw(error(resource_error(finite_memory),length/2))
   ;  length_addendum(Xs, N, M)
   ).
length(_, N) :-
   integer(N), !,
   domain_error(not_less_than_zero, N, length/2).
length(_, N) :-
   type_error(integer, N, length/2).

length_addendum([], N, N).
length_addendum([_|Xs], N, M) :-
    M1 is M + 1,
    length_addendum(Xs, N, M1).

length_rundown(Xs, 0) :- !, Xs = [].
length_rundown([_|Xs], N) :-
    N1 is N-1,
    length_rundown(Xs, N1).

:- help(length(?list,?integer), [iso(true),desc('Number of elements in list.')]).
