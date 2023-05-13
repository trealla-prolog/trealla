:-initialization(main).

leak2 :-
    findall(_, inner, _).

leak3 :-
    findall(_, inner2, _).

% calling inner/0 from the toplevel is fine
inner :-
    do_something("abcdefg")
    ; do_something("fooobarbaz").

% calling inner2/0 from the toplevel seems to leak "qux" (but not the other strings)?
inner2 :-
    do_something_else("abcdefg")
    ; do_something_else("fooobarbaz").

do_something(_).

do_something_else(X) :- X \= "qux".

main :-
	leak2.
