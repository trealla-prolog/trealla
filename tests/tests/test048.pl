:- initialization(main).
:- use_module(library(lists)).

foo([bar(a), bar(b), bar(c), bar(d)]).

main :- foo(Bars), member(bar(Bar), Bars), write(Bar), nl, fail.
main.
