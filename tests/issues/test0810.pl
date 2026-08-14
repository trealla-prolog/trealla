:- initialization(main).

main :-
	catch(number_chars(N,"'-\\\n3"), Err1, writeln(err1)),
	catch(number_chars(N,"'\\\n-3"), Err1, writeln(err2)),
	writeln(done).

