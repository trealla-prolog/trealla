:-initialization(main).

:-set_prolog_flag(double_quotes,atom).
main :- S="a b c", write(S), nl.
