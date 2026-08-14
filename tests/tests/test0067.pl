main :-
	( call(writeq, 'OK here') ->
		(nl, writeq('OK no error'), nl) ; (nl, writeq('OOPS was error'), nl)
	),
	writeq('OK done (3rd line)'), nl.

:- initialization(main).
