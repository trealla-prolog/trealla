:- initialization(main).
:- use_module(library(linda)).

main :-
    linda_eval(consumer('A')),
    linda_eval(consumer('B')),
    linda_eval(producer('P')),
    wait,
    in(producer(V0)),		% verify it finished
    writeq(done(V0)), nl,
    in(consumer(V1)),		% verify it finished
    writeq(done(V1)), nl,
    in(consumer(V2)),		% verify it finished
    writeq(done(V2)), nl,
    halt.

producer(N) :-
    between(1, 10, I),
		writeq(['producer',N,' ',I]), nl,
		out({msg:I}),
		delay(250),
		fail.
producer(_) :-
	end_wait.

consumer(N) :-
	in({msg:I}),
	writeq(['consumer',N,'got=',I]), nl,
	random(R),
	Ms is floor(R*1000),
	delay(Ms),
	fail.
