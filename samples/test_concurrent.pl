:- use_module(library(concurrent)).

test1(C) :-
	future(X, slow_predicate(X), F1),
	future(X, fast_predicate(X), F2),
	future_all([F1,F2], F),
	await(F, [A,B]),
	C is 2 * A + B.

test2(C) :-
	future(X, slow_predicate(X), F1),
	future(X, fast_predicate(X), F2),
	await(F1, A),
	await(F2, B),
	C is 2 * A + B.

test3(C) :-
	future(X, slow_predicate(X), F1),
	future(X, fast_predicate(X), F2),
	future_any([F1,F2], F),
	await(F, V),
	C = V.

test4(C) :-
	future(X, slow_predicate(X), F1),
	future(X, fast_predicate(X), F2),
	future_any([F1,F2], F),
	await(F, V),
	(future_done(F1) -> writeln(doneF1); writeln(notdoneF1)),
	(future_done(F2) -> writeln(doneF2); writeln(notdoneF2)),
	C = V.

slow_predicate(X) :- delay(900), X = 41.
fast_predicate(X) :- delay(100), X = 1.

:- use_module(library(http)).

test9(C) :-
	future(Status, geturl("www.google.com", Status), F1),
	future(Status, geturl("www.bing.com", Status), F2),
	future(Status, geturl("www.duckduckgo.com", Status), F3),
	future_all([F1,F2,F3], F),
	await(F, StatusCodes),
	C = StatusCodes.

geturl(Url, Status) :-
	http_get(Url, _Data, [status_code(Code),final_url(Location)]),
	!,
	format("Job [~s] ~d ==> ~s done~n", [Url,Code,Location]),
	Status = Code.
