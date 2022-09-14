:- initialization(main).
:- use_module(library(lists)).

main :-
	JsonData = '[{"foo": 1, "bar": 2}, {"bar": 3, "foo": 4}]',
	read_term_from_atom(Data, [double_quotes(atom)], JsonData),
	findall(X, (member({F1:A, F2:B},Data), (F1=foo -> X = A ; (F2=foo -> X = B))), L),
	writeq(L), nl.
