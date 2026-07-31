:- initialization(main).

% Issue #1090: improper variable_names/1 list must throw once.
% Missing return after check_list failure caused a second throw (and
% for Name=Var|non_list, a write after the error).

once_domain_error(Goal) :-
	findall(E,
		catch(call(Goal), error(E, _), true),
		[E]),
	E = domain_error(write_option, _).

main :-
	once_domain_error(write_term(_,[variable_names([a|non_list])])),
	write(atom_tail-ok), nl,
	once_domain_error(write_term(_,[variable_names([_='T'|non_list])])),
	write(eq_tail-ok), nl,
	once_domain_error(write_term(hello,[variable_names(['T'=_|non_list])])),
	write(name_eq_tail-ok), nl,
	% No write-after-error: hello must not appear before the ok lines above.
	halt.
