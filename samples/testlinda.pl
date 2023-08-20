:- initialization(main).
:- use_module(library(linda)).

main :-
    linda_eval(consumer('C1')),
    linda_eval(consumer('C2')),
    linda_eval(producer('P')),
    wait,
    in(producer(V0)),		% verify it finished normally
    writeq(done(V0)), nl,
    halt.

producer(N) :-
    between(1, 10, I),
		writeq(['producer',N,' ',I]), nl,
		out({msg:I}),
		delay(250),
		fail.
producer(_) :-
	forall(rd_noblock({msg:_}), delay(1)),
	end_wait.

consumer(N) :-
	in({msg:I}),
	writeq(['consumer',N,'got=',I]), nl,
	random(R),
	Ms is floor(R*1000) // 10 + 10,
	delay(Ms),
	fail.
