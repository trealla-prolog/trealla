:- use_module(library(format)).
:- use_module(library(dict)).
:- use_module(library(http)).
:- use_module(library(pio)).

test1 :-
	\+ \+ true,
	writeln(ok1).
test1 :-
	writeln(failed).

task2 :- writeln('OK about to throw'), throw(error(p1,p2)).

test2 :-
	( catch(task2,E,(format("OK caught: ~w~n", [E]),fail)) ->
		writeln('OOPS no error') ; writeln('OK was error')
	),
	writeln('OK done').

task3 :- writeln('OK here').

test3 :-
	( catch(task3,E,(format("wrong, caught: ~w~n", [E]),fail)) ->
		writeln('OK no error') ; writeln('OOPS was error')
	),
	writeln('OK done').

test4a :-
	( call(writeln('OK here')) ->
		writeln('OK no error') ; writeln('OOPS was error')
	),
	writeln('OK done (3rd line)').

test4b :-
	( call(writeln, 'OK here') ->
		writeln('OK no error') ; writeln('OOPS was error')
	),
	writeln('OK done (3rd line)').

test4c :-
	( call(writeln, 'OK here') -> writeln('OK no error') ),
	writeln('OK done (3rd line)').

test5a :- throw(error(p1,p2)).
test5b :- _ is abs(abc).

test6a :- S='Aa...Bb...Cc...Dd', writeln(S).

test6b :- Orig='Aa...Bb...Cc...Dd', string_lower(Orig,S), writeln(S).

test6c :- Orig='Aa...Bb...Cc...Dd', writeln(Orig), fail.
test6c :- writeln(ok).

test6d :- Orig='Aa...Bb...Cc...Dd', sys_queue(Orig), string_lower(Orig,S), sys_queue(S), fail.
test6d :- sys_list(L),writeln(L).

test6e :- Orig='Aa...Bb...Cc...Dd', atom_concat(Orig,Orig,S2), writeln(S2).

test7 :-
	http_get("www.duckduckgo.com",_Data,[status_code(Code),headers(Hdrs)]),
	format("Response=~w~n", [Code]),
	writeln(Hdrs),
	%write(_Data), nl,
	true.

test8 :-
	http_get("http://www.bing.com",_Data,[status_code(Code),headers(Hdrs)]),
	format("Response=~w~n", [Code]),
	writeln(Hdrs),
	%write(_Data), nl,
	true.

test9 :-
	http_get("https://www.google.com",_Data,[status_code(Code),headers(Hdrs),final_url(Location)]),
	format("Response=~w, location=~s~n", [Code,Location]),
	writeln(Hdrs),
	%write(_Data), nl,
	true.

task10(C) :-
	getline(C,L),
	format("GOT: ~w\n", [L]),
	task10(C).

test10a :-
	fork,
	server('',S,[ssl(false),hostname(localhost),port(8080)]),
	accept(S,C),
		fork,
		task10(C).
test10a :-
	wait.

test10b :-
	client('localhost:8080',_,_,S,[]),
	between(1,1000000,I),
		(format(S,"[~w] Hello, world~n",[I]) ->
			delay(1) ; (writeln(disconnected), !)),
		fail.

handle(C,'GET','/',Ver,Hdrs) :-
	format(C,"HTTP/~w 200 Ok\r\nConnection: close\r\nContent-Type: text/html\r\n\r\n",[Ver]),
	format(C, "<html><body><h1>Home</h1><h2>~w</h2></body></html>\n",[Hdrs]).
handle(C,_,_,Ver,_) :-
	format(C,"HTTP/~w 500 Server Error\r\nConnection: close\r\nContent-Type: text/html\r\n\r\n",[Ver]),
	bwrite(C,"<html><body><h1>500 Server Error</h1></body></html>\n").

test10c :-
	fork,
	server(':8080',S,[]),
	accept(S,C),
		fork,
		http_request(C,Method,Path,Ver,Hdrs),
		handle(C,Method,Path,Ver,Hdrs),
		close(C).
test10c :-
	wait.

task11(C) :-
	repeat,
		getline(C,L),
		write('GOT: '), writeln(L),
		fail.
task11(_).

test11a :-
	fork,
	server(':8080',S,[udp(true)]),
	task11(S).
test11a :-
	wait.

test11b :-
	client('localhost:8080',_,_,S,[udp(true)]),
	between(1,1000000,I),
		format(S,"[~w] Hello, world~n",[I]),
		delay(100),
		fail.

test12a :-
	JsonData = '[{"foo":1,"bar":2}, {"bar":3,"foo":4}]',
	read_term_from_atom(JsonData, Data, [double_quotes(atom)]),
	findall(X, (member({F1:A,F2:B},Data), (F1=foo -> X = A ; (F2=foo -> X = B))), L),
	writeln(L).

test12b :-
	JsonData = '[{"foo":1,"bar":2}, {"bar":3,"foo":4}]',
	read_term_from_atom(JsonData, Data, [double_quotes(atom)]),
	bagof(X, (member({F1:A,F2:B},Data), (F1=foo -> X = A ; (F2=foo -> X = B))), L),
	writeln(L),
	fail.
test12b.

test13.
test13 :- test13.

sum14(I,I,T,T) :- !.
sum14(I,X,Tmp,T) :- NewTmp is Tmp+I, NewI is I+1, sum14(NewI,X,NewTmp,T).

test14 :-
	sum14(1,100000,0,T),
	write(T), nl.

integers(Low,High,[Low|Rest]) :-
	Low =< High,
	!,
	M is Low+1,
	integers(M,High,Rest).
integers(_,_,[]).

test15a :- integers(1, 100000, L), L=[H|_], write(H), nl.
test15b:- integers(1, 100000, L), L=[_|T], write(T), nl.
test15c:- integers(1, 1000000, L), write_term(L,[max_depth(0)]), nl.

:- dynamic(p/2).
:- dynamic(p/3).

test16 :-
	assertz(p(Z, h(Z, W), f(W))), write('ok14\n'),
	p(f(f(a)), h(f(f(a)), f(a)), f(f(a))), write('ok15\n').
test16 :- write(failed), nl.

f(a,1).
f(a,2).
f(a,3).
f(b,10).
f(b,20).
f(b,30).

test17 :-
	findall(X,f(a,X),Bag,Tail),
	write(Bag), nl,
	findall(X,f(b,X),Tail,_NewTail),
	write(Bag), nl.

test18a :- assertz(f18(123),R), assertz(f18(456)), erase(R), listing(f18).
test18b :- assertz(f18(123),_), clause(f18(_),_,_).

task50(T) :-
	between(1,1000000,_),
		format("Task ... ~w",[T]), nl,
		sleep(T),
		fail.

test50 :- between(1,4,I), fork, task50(I).
test50 :- wait.

task51(T) :- Ms is random(1000), delay(Ms), send(T).

test51 :- between(1,10,I), fork, task51(I).
test51 :- wait, sys_list(L), writeln(L).

test52 :- between(1,10,_), N is random(1000), sys_queue(N), fail.
test52 :- sys_list(L), sort(L,L2),
	write_term_to_atom(S,L2,[]), writeln(S), nl,
	read_term_from_atom(S,S2,[]), write_term(S2,[]), nl.

task53(T) :- between(1,10,_), R is random(1000), delay(R), send({T,R}), fail.
task53(T) :- format("Task ~w done~n",[T]).

test53 :- between(1,4,I), fork, task53(I).
test53 :-
	forall(await, (recv(Msg), format("Got: ~w~n",[Msg]))),
	writeln('Finished').

geturl(Url) :-
	http_get(Url,_Data,[status_code(Code),final_url(Location)]), !,
	format("Job [~w] ~w ==> ~w done~n",[Url,Code,Location]).

test54 :-
	L = ["www.google.com","www.bing.com","www.duckduckgo.com"],
	maplist(geturl,L),
	writeln('Finished').

test55 :-
	L = ["www.google.com","www.bing.com","www.duckduckgo.com"],
	maplist(task(geturl),L),
	wait, writeln('Finished').

test56 :-
	L = ["www.google.com","www.bing.com","www.duckduckgo.com"],
	tasklist(geturl,L),
	writeln('Finished').

test57 :-
	L = ["www.google.com","www.bing.com","www.duckduckgo.com"],
	maplist(writeln,L),
	writeln('Finished').

test61(0).
test61(N) :- N > 0, M is N - 1, test61(M).

test62(N) :- N > 0, !, M is N - 1, test62(M).
test62(0).

test63(N) :- ( (N > 0 -> M is N - 1, test63(M)) ; true ).

pr(1,a).
pr(2,b).
pr(3,c).

task64(G) :- G.

test64 :-
	task64(findall(X, pr(X,_), S)),
	writeln(S).

test65 :-
	task64(bagof(X, pr(X,_), S)),
	writeln(S),
	fail.
test65.

test66 :-
	call_nth(between(1,1000000000,I),12345678), writeln(I), fail.
test66.

test67 :-
	call_nth(between(1,1000000000,I),_), writeln(I), sleep(1), fail.

test68 :-
	between(1,3,I), test66, writeln(I), fail.
test68.

test69 :-
	limit(5, offset(5, between(1,20,I))), writeln(I), fail.
test69.

task70(_).

test70 :- freeze(X,task70(X)), frozen(X,Y), writeln(Y), writeln('OK done').

test80 :-
	between(1,10,I),
		write(I), nl,
		(I is 5 -> !; true),
		fail.
test80 :- write(oops), nl.

test81 :- \+ \+ fail, writeln(nok).
test81 :- writeln(ok).

test82 :- \+ \+ true, writeln(ok).
test82 :- writeln(nok).

test83 :- \+ \+ (!, true), writeln(ok).
test83 :- writeln(nok).

test90 :- D=[a:1,b:2,c:3], d_lst(D,L), writeln(L).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

test91 :-
	open("README.md", read, Str, [mmap(Ms)]),
	length(Ms,N1), format("Ms length=~w~n", [N1]),
	phrase(list(Ls), Ms, []),
	length(Ls,N2), format("Ls length=~w~n", [N2]),
	close(Str).

test92 :-
	phrase_from_file(list(Ls), "README.md"),
	length(Ls,N), format("Ls length=~w~n", [N]).

