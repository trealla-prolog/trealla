:- initialization(main).

% A tail call reuses its caller's frame and winds the heap back to that
% frame's base. That is only sound while nothing outside the frame
% points into what it built. A body goal binding an *older* frame's
% variable to a compound of this clause does exactly that: the binding
% is an indirect carrying this frame's context, so reusing the frame
% makes it read the next iteration's slots.
%
% p/1 below binds V - a variable of the list main built - to g(S), where
% S is p/1's own. Answering [h(g(bar),foo),...] means the first
% element's binding followed the frame into the second iteration.
%
% set_var() did notice, but only in q->no_recov, which unify() clears on
% entry - and head unification of the very call that reuses the frame is
% one such unify(). The frame carries the pin instead.

p([]).
p([I|T]) :- I = h(V,S), V = g(S), p(T).

% The same through the THEN branch of an if-then-else, which is also a
% tail position.

q([]).
q([I|T]) :- ( I = h(V,S) -> V = g(S) ; true ), q(T).

% ... and a shape where the value binds nothing of the caller's, which
% must still be tail recursive - the pin is not meant to be catching
% ordinary accumulator loops.

count(0) :- !,
	statistics(frames, F),
	( F < 100 -> write(count_constant) ; write(count_grew) ), nl.
count(N) :- M is N-1, count(M).

main :-
	L1 = [h(_,foo), h(_,bar)], p(L1), write(L1), nl,
	L2 = [h(_,foo), h(_,bar)], q(L2), write(L2), nl,
	count(200000).
