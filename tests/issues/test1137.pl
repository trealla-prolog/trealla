% Issue #1137: write_term/2 with ignore_ops(true) went through the
% canonical writer, which also forced quoted(true).

:- initialization(main).

main :-
	show(write_term(., [ignore_ops(true)])),
	show(write_term('.', [ignore_ops(true)])),
	show(write_term('.', [ignore_ops(true), quoted(true)])),
	show(write_term('.', [ignore_ops(true), quoted(false)])),
	show(write_canonical(.)),
	show(write_term('a b', [ignore_ops(true)])),
	show(write_term('a b', [ignore_ops(true), quoted(true)])),
	show(write_term(1+2*3, [ignore_ops(true)])),
	show(write_term([a,b|c], [ignore_ops(true)])),
	show(write_term({a}, [ignore_ops(true)])).

show(Goal) :-
	call(Goal),
	nl.
