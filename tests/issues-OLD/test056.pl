:-initialization(main).

main :-
	number_chars(0, Cs),
	crypto_data_hash(Cs, S, [algorithm(sha256)]),
	writeq(S), nl.
