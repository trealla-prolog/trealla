:- initialization(main).
:- use_module(library(linda)).

main :-
    linda_eval(consumer('A')),
    linda_eval(consumer('B')),
    linda_eval(producer),
    wait,
    halt.

producer :-
    between(1, 10, I),
		write(['producer', I]), nl,
		out({msg:I}),
		delay(250),
		fail.
producer :-
	delay(1000),
	end_wait.

consumer(N) :-
	in({msg:I}),
	write(['consumer',N,'got=',I]), nl,
	random(R),
	Ms is floor(R*1000),
	delay(Ms),
	fail.
