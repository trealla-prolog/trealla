:-initialization(main).
:-set_prolog_flag(public_clauses,on).

answer(41) :- abc, fail.
answer(42).

main :- H = answer(_), clause(H, Body), write_term((H :- Body), [nl(true), fullstop(true)]), fail.
main :- current_prolog_flag(public_clauses, Flag), writeln(Flag), fail.
main.
