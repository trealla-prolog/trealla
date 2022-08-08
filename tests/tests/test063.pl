:- dynamic(p/3).
:- dynamic(p/2).
:- dynamic(q/2).
:- dynamic(r/2).
:- dynamic(r/1).
:- dynamic(h/1).

p(X, Y) :- q(X, Z), r(Z, Y).
q(q, s).
r(s, t).

main :-
	\+ \+ findall([X,Y], p(X, Y), [[q, t]]), write('ok1\n'),
	p(q, t), write('ok2\n'),
	\+ p(t, q), write('ok3\n'),
	\+ \+ findall(T, p(q, T), [t]), write('ok4\n'),
	\+ p(t, t), write('ok5\n'),
	\+ \+ retract((p(X,Y) :- q(X,Z), r(Z, Y))), write('ok6\n'),
	retract(q(_,_)), write('ok7\n'),
	\+ \+ assertz((p(X,_) :- q(f(f(X)), _), r(_, _))), write('ok8\n'),
	\+ \+ assertz(q(f(f(X)), r)), write('ok9\n'),
	p(_,_), write('ok10\n'),
	retract(q(_,_)), write('ok11\n'),
	assertz(q(f(f(x)), r)), write('ok12\n'),
	\+ \+ findall(X, p(X,_), [x]), write('ok13\n'),
	\+ \+ retract((p(X,_) :- q(f(f(X)), _), r(_, _))), write('ok14\n'),
	retract(q(_,_)), write('ok15\n'),
	\+ \+ assertz((p(X, Y) :- q(X, Y), r(X, Y))), write('ok16\n'),
	assertz(q(s, t)), write('ok17\n'),
	retract(r(_,_)), write('ok18\n'),
	\+ \+ assertz((r(X, Y) :- r(a))), write('ok19\n'),
	assertz(r(a)), write('ok20\n'),
	\+ \+ findall([X,Y], p(X, Y), [[s,t]]), write('ok21\n'),
	\+ p(t, _), write('ok22\n'),
	\+ \+ findall(T, p(s, T), [t]), write('ok23\n'),
	\+ \+ findall(S, p(S, t), [s]), write('ok24\n'),
	\+ \+ assertz((p(f(f(a), g(b), X), g(b), h) :- q(X, _))), write('ok25\n'),
	retract(q(_,_)), write('ok26\n'),
	assertz(q(_,_)), write('ok27\n'),
	\+ \+ findall([X,Y,Z], p(f(X, Y, Z), g(b), h), [[f(a), g(b), _]]), write('ok28\n'),
	\+ p(f(X, g(_), Z), g(Z), X), write('ok29\n'),
	\+ \+ findall([X,Y,Z], p(f(X, g(Y), Z), g(Z), h), [[f(a), b, b]]), write('ok30\n'),
	\+ \+ findall([X,Y,Z], p(Z, Y, X), [[h, g(b), f(f(a),g(b),_)]]), write('ok31\n'),
	\+ \+ findall([X,Y,Z], p(f(X, Y, Z), Y, h), [[f(a), g(b), _]]), write('ok32\n'),
	\+ \+ retract((p(X, Y) :- q(X, Y), r(X, Y))), write('ok33\n'),
	\+ \+ retract((p(f(f(a), g(b), X), g(b), h) :- q(X, _))), write('ok34\n'),
	\+ \+ assertz((p(_, f(_, Y, _)) :- h(Y))), write('ok35\n'),
	assertz(h(y)), write('ok36\n'),
	\+ \+ findall(Y, p(_, f(_, Y, _)), [y]), write('ok37\n'),
	p(_, f(_, y, _)), write('ok38\n'),
	\+ p(_, f(_, z, _)), write('ok39\n'),
	\+ \+ retract((p(_, f(_, Y, _)) :- h(Y))), write('ok40\n'),
	cleanup, write('ok41\n'),
	write(done), nl.

cleanup :-
	abolish(p/3),
	abolish(p/2),
	abolish(q/2),
	abolish(r/2),
	abolish(r/1),
	abolish(h/1).

:- initialization(main).
