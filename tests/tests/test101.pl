:- initialization(main).

test1(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([f,Fs,c,Cs,t,Ts,s,Ss]), nl, fail.
test1(N) :- N1 is N-1, test1(N1).

f :- true.
test2(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([f,Fs,c,Cs,t,Ts,s,Ss]), nl, fail.
test2(N) :- f, N1 is N-1, test2(N1).

f(_) :- true.
test3(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([f,Fs,c,Cs,t,Ts,s,Ss]), nl, fail.
test3(N) :- f(_), N1 is N-1, test3(N1).

f(X, X) :- true.
test4(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([f,Fs,c,Cs,t,Ts,s,Ss]), nl, fail.
test4(N) :- f(_, _), N1 is N-1, test4(N1).

f(_, g(_), g(g(_))) :- true.
test5(0) :- !, statistics(frames, Fs), statistics(choices, Cs), statistics(trails, Ts), statistics(slots, Ss), write([f,Fs,c,Cs,t,Ts,s,Ss]), nl, fail.
test5(N) :- f(I, g(N), g(g(_))), N1 is N-1, test5(N1).

statistics.

main :-
	write(test1), write(': '), test1(100000);
	write(test2), write(': '), test2(100000);
	write(test3), write(': '), test3(100000);
	write(test4), write(': '), test4(100000);
	write(test5), write(': '), test5(100000);
	true.

