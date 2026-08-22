% call_with_time_limit/2 is what library(quads) uses to stop a query
% that does not terminate, and it is a mechanism that can go missing
% quietly: if the platform's timer cannot be armed, nothing throws -
% the limit simply never fires and the goal runs forever. That showed
% up only as a 30-second hang of the whole quads suite on NetBSD, and
% then only while quads ran under `make test`.
%
% So pin the mechanism itself here: cheap, single-threaded, and on
% every platform the suite runs on. tests/misc/timeout.pl covers the
% threaded case, which is not portable enough for this suite.

:- initialization(main).

:- use_module(library(iso_ext)).

check(Name, Goal) :-
	(	catch(call(Goal), E, (write(Name), write(' THREW '), writeq(E), nl, fail))
	->	write(Name), write(' ok'), nl
	;	write(Name), write(' FAILED'), nl
	).

timed_out(E) :- nonvar(E), E = error(B, _), nonvar(B), functor(B, time_limit_exceeded, _).

% a goal that would never end is cut short

fires :-
	catch(call_with_time_limit(0.5, (repeat, fail)), E, true),
	timed_out(E).

% one that ends well inside the limit is left alone

completes :-
	catch(call_with_time_limit(30.0, X is 1 + 1), E, true),
	var(E),
	X == 2.

% and one that merely fails inside the limit just fails

fails :-
	\+ catch(call_with_time_limit(30.0, fail), _, true).

% the limit is cancelled when the goal is done, so a later goal is not
% cut short by a stale alarm

cancelled :-
	catch(call_with_time_limit(0.5, true), E1, true),
	var(E1),
	catch(call_with_time_limit(30.0, ( between(1, 200000, _), fail ; true )), E2, true),
	var(E2).

% nested limits: the inner one fires and the outer one does not

nested :-
	catch(
		call_with_time_limit(30.0,
			(	catch(call_with_time_limit(0.5, (repeat, fail)), Inner, true),
				timed_out(Inner)
			)),
		Outer, true),
	var(Outer).

main :-
	check(fires, fires),
	check(completes, completes),
	check(fails, fails),
	check(cancelled, cancelled),
	check(nested, nested).
