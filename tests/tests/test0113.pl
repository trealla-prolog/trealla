% atomic_list_concat/3 in split mode: with the list unbound (or only
% partly bound) and the atom given, the atom is split on the separator.

:- initialization(main).

show(G) :-
	(  catch(G, E, (writeq(caught(E)), nl, fail))
	-> true
	;  writeln(fail)
	).

split(Sep, Atom) :-
	show(( atomic_list_concat(L, Sep, Atom), writeq(L), nl )).

main :-
	split('-', 'a-b-c'),
	split(-, 'a-b-c'),
	split('::', 'a::b::c'),
	split('-', 'abc'),
	split('-', ''),
	split('-', 'a--b'),
	split('-', '-a-'),
	split('é', 'aébéc'),
	split('-', 'a-bbbbbbbbbbbbbbbbbbbbbbbbbbbb-c'),

	% pieces are atoms, never numbers or strings
	show(( atomic_list_concat(L1, '-', '1-2'), maplist(atom, L1), writeln(all_atoms) )),

	% a partial list, or one with unbound elements, splits and unifies
	show(( atomic_list_concat([a|T], '-', 'a-b-c'), writeq(T), nl )),
	show(( atomic_list_concat([a,X], '-', 'a-b'), writeq(X), nl )),
	show(( atomic_list_concat([x,_], '-', 'a-b') )),
	show(( atomic_list_concat([_,_], '-', 'a-b-c') )),

	% split is semi-deterministic
	findall(L2, atomic_list_concat(L2, '-', 'a-b'), L2s),
	writeq(L2s), nl,

	% and round-trips through the concat mode
	show(( atomic_list_concat(L3, '-', 'a-b-c'), atomic_list_concat(L3, '-', A3), writeq(A3), nl )),

	% concat mode is unchanged
	show(( atomic_list_concat([a,b,c], '-', A4), writeq(A4), nl )),
	show(( atomic_list_concat([], '-', A5), writeq(A5), nl )),

	% atomic_list_concat/2 has no separator to split on, so a list that
	% isn't fully there is an error there, not a request to split
	show(( atomic_list_concat([a,b,c], A6), writeq(A6), nl )),
	show(( atomic_list_concat([_,bar], foobar) )),
	show(( atomic_list_concat([foo,bar|_], foobar) )),
	show(( atomic_list_concat(_, foobar) )),
	show(( atomic_list_concat([foo,bar], a(1)) )),

	% errors
	show(( atomic_list_concat([foo,bar], '_', a(1)) )),
	show(( atomic_list_concat([foo,bar], a(1), _) )),
	show(( atomic_list_concat(_, '', 'abc') )),
	show(( atomic_list_concat(_, '-', _) )),
	show(( atomic_list_concat(_, _, 'a-b') )),
	show(( atomic_list_concat([a|_], '-', _) )),
	show(( atomic_list_concat(foo, '-', _) )),
	show(( atomic_list_concat([a|b], '-', _) )).
