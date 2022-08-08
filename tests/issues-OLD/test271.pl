a(1).
a(2).
a(_) :- throw(e).

from_generator(Goal, Value, Optional) :-
	catch(Goal, Error, true),
	(	var(Error) ->
		Optional = optional(Value)
	;	Optional = empty,
		!
	).
from_generator(_, _, empty).

:-initialization(main).

main :-
	findall(Optional, from_generator(a(X), X, Optional), Optionals),
	write(Optionals), nl.

