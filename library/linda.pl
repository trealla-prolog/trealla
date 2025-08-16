:- module(linda, [
	linda_eval/1, linda_eval/2,
	out/1,
	inp/1, rdp/1,
	in/1, rd/1,
	bagof_inp/3,
	bagof_rdp/3
	]).

:- dynamic(linda/1).

linda_call_(H, G) :-
	G,
	H = G,
	out(H).

linda_eval(Goal) :-
	copy_term(Goal, Goal2),
	call_task(linda_call_(_, Goal2)).

linda_eval(Head, Goal) :-
	copy_term(Goal, Goal2),
	call_task(linda_call_(Head, Goal2)).

out(Tuple) :-
	assertz('$linda'(Tuple)),
	yield.

inp(Tuple) :-
	retract('$linda'(Tuple)).

rdp(Tuple) :-
	'$linda'(Tuple).

in(Tuple) :-
	once(retract('$linda'(Tuple))).
in(Tuple) :-
	yield,
	in(Tuple).

rd(Tuple) :-
	'$linda'(Tuple).
rd(Tuple) :-
	yield,
	rd(Tuple).

turn_(Free^Generator, Functor, Goal) :-
	!,
	turn_(Generator, Functor, Goal2),
	Goal = Free^Goal2.

turn_(Generator, Functor, Goal) :-
	Goal =.. [Functor,Generator].

bagof_inp(Template, Tuple, Bag) :-
	turn_(Tuple, inp, Goal),
	bagof(Template, Goal, Bag).

bagof_rdp(Template, Tuple, Bag) :-
	turn_(Tuple, rdp, Goal),
	bagof(Template, Goal, Bag).
