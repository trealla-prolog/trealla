:- initialization(main).

% Issue #1117: transient compounds use a 32-bit arity. Predicates stored
% in the database deliberately retain the 8-bit arity limit.

parser_roundtrip :-
	functor(T, a_long_functor_name, 256),
	write_term_to_chars(T, [], Chars),
	read_term_from_chars(Chars, U, []),
	functor(U, a_long_functor_name, 256).

univ_roundtrip :-
	length(Args, 256),
	L = [a_long_functor_name|Args],
	T =.. L,
	T =.. L2,
	length(L2, 257),
	functor(T, a_long_functor_name, 256).

copy_variant :-
	functor(T, a_long_functor_name, 256),
	copy_term(T, U),
	variant(T, U).

database_limit :-
	functor(T, a_long_functor_name, 256),
	catch(assertz(T), E, true),
	E = error(representation_error(max_arity),_).

syntax_error :-
	catch(read(_), E, true),
	E = error(syntax_error(args),_).

constructor ?- functor(_, a_long_functor_name, 256).
   true.

parser_roundtrip ?- parser_roundtrip.
   true.

univ_roundtrip ?- univ_roundtrip.
   true.

copy_variant ?- copy_variant.
   true.

database_limit ?- database_limit.
   true.

max_arity_flag ?- current_prolog_flag(max_arity, unbounded).
   true.

syntax_error ?- syntax_error.
   inputs("t(1,,2)."), peeks("\n"), true.

main :- use_module(library(quads)), run_quads.
