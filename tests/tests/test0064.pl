:- dynamic(p/2).
:- dynamic(p/3).

p(Z, Z).
clouds(are, nice).
p(Z, h(Z, W), f(W)).

main :-
	findall(Z, p(Z, Z), [Z]), write('ok1\n'),
	var(Z),
	findall(Z, p(Z, z), [z]), write('ok2\n'),
	findall(Z, p(Z, w), [w]), write('ok3\n'),
	\+ p(z, w), write('ok4\n'),
	p(w, w), write('ok5\n'),
	\+ clouds(Z, Z), write('ok6\n'),
	findall(Z, clouds(are, Z), [nice]), write('ok7\n'),
	\+ p(z, h(z, z), f(w)), write('ok8\n'),
	p(z, h(z, w), f(w)), write('ok9\n'),
	findall(W, p(z, h(z, W), f(w)), [w]), write('ok10\n'),
	findall(Z, p(Z, h(Z, w), f(Z)), [w]), write('ok11\n'),
	\+ p(z, h(Z, w), f(Z)), write('ok12\n'),
	retract(p(_,_,_)), write('ok13\n'),
	assertz(p(Z, h(Z, W), f(W))), write('ok14\n'),
	p(f(f(a)), h(f(f(a)), f(a)), f(f(a))), write('ok15\n'),
	retract(p(Z, h(Z, W), f(W))), write('ok16\n').

:- initialization(main).
