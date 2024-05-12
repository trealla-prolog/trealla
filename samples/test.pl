:- use_module(library(format)).
:- use_module(library(dict)).
:- use_module(library(http)).
:- use_module(library(pio)).

geturl(Url) :-
	http_get(Url,_Data,[status_code(Code),final_url(Location)]), !,
	format("Job [~w] ~w ==> ~w done~n",[Url,Code,Location]).

test54 :-
	L = ["www.google.com","www.bing.com","www.duckduckgo.com"],
	maplist(geturl,L),
	writeln('Finished').

test55 :-
	L = ["www.google.com","www.bing.com","www.duckduckgo.com"],
	maplist(call_task(geturl),L),
	wait, writeln('Finished').

test56 :-
	L = ["www.google.com","www.bing.com","www.duckduckgo.com"],
	tasklist(geturl,L),
	writeln('Finished').

test57 :-
	L = ["www.google.com","www.bing.com","www.duckduckgo.com"],
	maplist(writeln,L),
	writeln('Finished').

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

