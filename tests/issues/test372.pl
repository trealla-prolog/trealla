:-initialization(main).
:-set_prolog_flag(strict_iso,off).

answer(41) :- 1 =:= 2, fail.
answer(42).

main :- H = answer(X), clause(H, Body), writeln((H :- Body)), fail.
main.
