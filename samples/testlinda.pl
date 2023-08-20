:- initialization(main).
:- use_module(library(linda)).

main :-
    linda_eval(consumer('A')),
    linda_eval(consumer('B')),
    linda_eval(producer),
    wait,
    in(producer),			% verify it finished
    in(consumer(_)),		% verify it finished
    in(consumer(_)),		% verify it finished
    write(done), nl,
    halt.

producer :-
    between(1, 10, I),
		writeq(['producer', I]), nl,
		out({msg:I}),
		delay(250),
		fail.
producer :-
	delay(1000),
	end_wait.

consumer(N) :-
	in({msg:I}),
	writeq(['consumer',N,'got=',I]), nl,
	random(R),
	Ms is floor(R*1000),
	delay(Ms),
	fail.
