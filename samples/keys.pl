main1 :-
	write('Set'), nl,
	between(1,1000000,I),
		assertz(dummy(I,I)),
		fail.

main1 :-
	write('Get'), nl,
	between(1,1000000,I),
		dummy(I,I),
		fail.

/*
main1 :-
	write('Del'), nl,
	between(1,1000000,I),
		retract(dummy(I,I)),
		fail.
*/

main1 :-
	write('Done'), nl.

main2 :-
	write('Set'), nl,
	between(1,1000000,I),
		recordz(I,I),
		fail.

main2 :-
	write('Get'), nl,
	between(1,1000000,I),
		recorded(I,I),
		fail.

/*
main2 :-
	write('Del'), nl,
	between(1,1000000,I),
		recorded(I,I,R),
		erase(R),
		fail.
*/

main2 :-
	write('Done'), nl.

main3 :-
	write('Set'), nl,
	between(1,1000000,I),
		kv_set(I,I,[]),
		fail.

main3 :-
	write('Get'), nl,
	between(1,1000000,I),
		kv_get(I,I,[]),
		fail.

/*
main3 :-
	write('Del'), nl,
	between(1,1000000,I),
		kv_get(I,I,[delete(true)]),
		fail.
*/

main3 :-
	write('Done'), nl.

main4 :-
	write('Set'), nl,
	between(1,1000000,I),
		create_key_value(I,I),
		fail.

main4 :-
	write('Get'), nl,
	between(1,1000000,I),
		read_key_value(I,I),
		fail.

/*
main4 :-
	write('Del'), nl,
	between(1,1000000,I),
		delete_key_value(I,I]),
		fail.
*/

main4 :-
	write('Done'), nl.
