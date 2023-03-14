:- use_module('samples/concurrent.pl').

test1(C) :-
    future(X, slow_predicate(X), F1),
    future(Y, other_slow_thing(Y), F2),
    future_all([F1,F2], F),
    await(F, [A,B]),
    C is 2 * A + B.

test2(C) :-
    future(X, slow_predicate(X), F1),
    future(Y, other_slow_thing(Y), F2),
    await(F1, A),
    await(F2, B),
    C is 2 * A + B.

test3(C) :-
    future(X, slow_predicate(X), F1),
    future(Y, other_slow_thing(Y), F2),
    future_any([F1,F2], F),
    await(F, V),
    C = V.

slow_predicate(X) :- delay(900), X = 41.
other_slow_thing(X) :- delay(500), X = 1.


:- use_module(library(format)).
:- use_module(library(http)).
:- use_module(library(pio)).

test4(C) :-
	future(Status1, geturl("www.google.com", Status1), F1),
	future(Status2, geturl("www.bing.com", Status2), F2),
	future(Status3, geturl("www.duckduckgo.com", Status3), F3),
    future_all([F1,F2,F3], F),
    await(F, StatusCodes),
    C = StatusCodes.

geturl(Url,Code) :-
	http_get(Url,_Data,[status_code(Code),final_url(Location)]), !,
	format("Job [~w] ~w ==> ~w done~n",[Url,Code,Location]).

