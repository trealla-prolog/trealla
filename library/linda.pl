:- module(linda, [
	linda_eval/1, linda_eval/2,
	out/1,
	in_noblock/1, rd_noblock/1,
	in/1, rd/1,
	bagof_in_noblock/3,
	bagof_rd_noblock/3
	]).

:- dynamic(linda/1).

linda_call_(H, G) :-
	G,
	H = G,
	out(H).

linda_eval(Goal) :-
	copy_term(Goal, Goal2),
	task(linda_call_(_, Goal2)).

linda_eval(Head, Goal) :-
	copy_term(Goal, Goal2),
	task(linda_call_(Head, Goal2)).

out(Tuple) :-
	assertz('$linda'(Tuple)),
	yield.

in_noblock(Tuple) :-
	retract('$linda'(Tuple)).

rd_noblock(Tuple) :-
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

bagof_in_noblock(Template, Tuple, Bag) :-
	turn_(Tuple, in_noblock, Goal),
	bagof(Template, Goal, Bag).

bagof_rd_noblock(Template, Tuple, Bag) :-
	turn_(Tuple, rd_noblock, Goal),
	bagof(Template, Goal, Bag).
