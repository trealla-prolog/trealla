:- initialization(main).
:- use_module(library(linda)).

main :-
	now(Seed),
	srandom(Seed),
    linda_eval(consumer('C1')),
    linda_eval(consumer('C2')),
    linda_eval(producer('P')),
    wait,
    in(producer(V0)),		% verify it finished normally
    writeq(done(V0)), nl,
    halt.

producer(_) :-
    between(1, 10, I),
		out({msg:I}),
		random(R),
		Secs is (floor(R*1000) // 2 + 1) / 1000,
		sleep(Secs),
		fail.
producer(_) :-
	forall(rd_noblock({msg:_}), sleep(0.001)),
	end_wait.

consumer(N) :-
	in({msg:I}),
	writeq(['consumer',N,'got=',I]), nl,
	random(R),
	Secs is (floor(R*1000) // 10 + 1) / 1000,
	sleep(Secs),
	fail.
