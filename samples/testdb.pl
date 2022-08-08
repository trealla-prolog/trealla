:-use_module(library(auth)).

writeln(T) :- writeq(T), nl.

test1a :-
	auth:adduser(user1,pass1),
	writeln('OK').
test1a :-
	writeln('OOPS already exists').

test1b :-
	auth:deluser(user1),
	writeln('OK').
test1b :-
	writeln('OOPS not exists').

test2a :-
	auth:adduser(user2,pass1),
	writeln('OK').
test2a :-
	writeln('OOPS already exists').

test2b :-
	auth:deluser(user2),
	writeln('OK').
test2b :-
	writeln('OOPS not exists').

test99 :-
	auth:save,
	auth:listusers(L),
	writeln(L),
	writeln('OK').
