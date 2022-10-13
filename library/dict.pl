:- module(dict, [get/4, get/3, set/4, app/4, del/3, del/4, lst/2, match/3]).

get([], _, D, D) :- !.
get([N:V|_], N, V, _) :- !.
get([_|T], N, V, D) :-
	get(T, N, V, D).

get([], _, _) :- !,
	fail.
get([N:V|_], N, V) :- !.
get([_|T], N, V) :-
	get(T, N, V).

set([], N, V, [N:V]) :- !.
set(D, N, V, D2) :-
	del(D, N, D3),
	D2=[N:V|D3].

app([], N, V, [N:V]) :- !.
app(D, N, V, D2) :-
	D2=[N:V|D].

del([], _, []) :- !.
del([N:_|T], N, T) :- !.
del([H|T], N, [H|D]) :-
	del(T, N, D).

del([], _, _, []) :- !.
del([N:V|T], N, V, T) :- !.
del([H|T], N, V, [H|D]) :-
	del(T, N, V, D).

lst0([], L, L) :- !.
lst0([_:V|T], L1, L) :-
	lst0(T, [V|L1], L).

lst(D, L) :-
	lst0(D, [], L).

match([], _, L, L) :- !.
match([H:V|T], Template, L1, L) :-
	copy_term(Template, Template2),
	(	H = Template ->
		match(T, Template2, [V|L1], L)
	;	match(T, Template2, L1, L)
	).

match(D, Template, L) :-
	match(D, Template, [], L).
