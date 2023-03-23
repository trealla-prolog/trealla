:-initialization(main).

main :-
	call_cleanup(between(1,5,_), throw(error)), (between(1,5,_) -> !).
