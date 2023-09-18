:- module(dict, [
	d_get/4, d_get/3,
	d_set/4, d_app/4,
	d_del/3, d_del/4,
	d_lst/2,
	d_match/3
	]).

d_get([], _, D, D) :- !.
d_get([N:V|_], N, V, _) :- !.
d_get([_|T], N, V, D) :-
	d_get(T, N, V, D).

d_get([], _, _) :- !,
	fail.
d_get([N:V|_], N, V) :- !.
d_get([_|T], N, V) :-
	d_get(T, N, V).

d_set([], N, V, [N:V]) :- !.
d_set(D, N, V, D2) :-
	d_del(D, N, D3),
	D2=[N:V|D3].

d_app([], N, V, [N:V]) :- !.
d_app(D, N, V, D2) :-
	D2=[N:V|D].

d_del([], _, []) :- !.
d_del([N:_|T], N, T) :- !.
d_del([H|T], N, [H|D]) :-
	d_del(T, N, D).

d_del([], _, _, []) :- !.
d_del([N:V|T], N, V, T) :- !.
d_del([H|T], N, V, [H|D]) :-
	d_del(T, N, V, D).

d_lst_([], L, L) :- !.
d_lst_([_:V|T], L1, L) :-
	d_lst_(T, [V|L1], L).

d_lst(D, L) :-
	d_lst_(D, [], L).

d_match([], _, L, L) :- !.
d_match([H:V|T], Template, L1, L) :-
	copy_term(Template, Template2),
	(	H = Template -> d_match(T, Template2, [V|L1], L)
	;	d_match(T, Template2, L1, L)
	).

d_match(D, Template, L) :-
	d_match(D, Template, [], L).
