:- initialization(main).

last_element([], Out) :- Out = nil.
last_element([Arg|Rest], Out) :- write(Arg), last_element(Rest, Out).

foo(A, B, Out) :- last_element([A|B], Out).
bar(A, B) :- foo(A, [B], _Out).

main :- bar(a, b), nl.
