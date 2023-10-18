:-initialization(main).
:-public(answer/1).

answer(41) :- abc, fail.
answer(42).

not_public(_).

main :- H = answer(_), clause(H, Body), write_term((H :- Body), [nl(true), fullstop(true)]), fail.
main :- predicate_property(answer(_), (public)), writeln(ok), fail.
main :- catch(clause(not_public(_), _), Error, writeln(Error)), fail.
main.
