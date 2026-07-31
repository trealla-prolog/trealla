:-initialization(main).

% Issue #1091 / ISO Cor.3: when several variable_names/1 elements
% apply to the same (aliased) variable, the leftmost is used.

main :-
	Z = Y, Y = X, T = (X, Y, Z),
	write_term(T, [quoted(true), variable_names(['X'=X, 'Y'=Y, 'Z'=Z])]), nl,
	write_term(T, [quoted(true), variable_names(['Z'=Z, 'Y'=Y, 'X'=X])]), nl,
	write_term(T, [quoted(true), variable_names(['Y'=Y, 'X'=X, 'Z'=Z])]), nl,
	halt.
