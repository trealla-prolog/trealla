:-initialization(main).

main :- sub_atom(abc,B,L,A,S),writeq([B,L,A,S]), nl, fail.
main :- nl, sub_atom(abc,B,2,A,S),writeq([B,2,A,S]), nl, fail.
main :- nl, sub_atom(abc,0,2,A,S),writeq([0,2,A,S]), nl, fail.
main :- nl, sub_atom(abc,0,L,A,S),writeq([0,L,A,S]), nl, fail.
main :- nl, sub_atom(abc,B,L,0,S),writeq([B,L,0,S]), nl, fail.
main :- nl, sub_atom(abc,B,2,0,S),writeq([B,2,0,S]), nl, fail.
main.
