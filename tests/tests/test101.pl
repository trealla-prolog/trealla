:- initialization(main).

test1(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([Fs,Cs,Ts,Ss]), nl, fail.
test1(N) :- N1 is N-1, test1(N1).

f :- true.
test2(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([Fs,Cs,Ts,Ss]), nl, fail.
test2(N) :- f, N1 is N-1, test2(N1).

f(1) :- true.
test3(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([Fs,Cs,Ts,Ss]), nl, fail.
test3(N) :- f(_), N1 is N-1, test3(N1).

% TODO

f(_, _) :- true.
test4(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([Fs,Cs,Ts,Ss]), nl, fail.
test4(N) :- f(N, _), N1 is N-1, test4(N1).

f(_, g(_), g(g(_))) :- true.
test5(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([Fs,Cs,Ts,Ss]), nl, fail.
test5(N) :- f(I, g(N), g(g(_))), N1 is N-1, test5(N1).

statistics.

main :-
	write(test1), write(': '), test1(1000000);
	write(test2), write(': '), test2(1000000);
	write(test3), write(': '), test3(1000000);
	write(test4), write(': '), test4(1000000);
	write(test5), write(': '), test5(1000000);
	true.

