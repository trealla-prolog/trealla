:- module(linda, [
	linda_eval/1,
	out/1,
	in_noblock/1, rd_noblock/1,
	in/1, rd/1
	]).

:- dynamic(linda/1).

linda_eval(Goal) :-
	task(Goal).

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
