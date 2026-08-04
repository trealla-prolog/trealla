:- use_module(library(yall)).
:- initialization(main).

% https://github.com/trealla-prolog/trealla/issues/1097
% Text input must enforce UTF-8: ISO 13211-1 8.12.1.3 i says an entity
% input from the stream that is not a character raises
% representation_error(character).

bad_file(F) :-
	open(F, write, S, [type(binary)]),
	put_byte(S, 0'a),
	put_byte(S, 0xff),
	close(S).

try(F, G) :-
	open(F, read, S),
	get_char(S, a),
	catch(call(G, S), E, true),
	(	var(E)
	->	write(no_error)
	;	write(E)
	),
	nl,
	close(S).

main :-
	F = 'tests/issues/test1097.tmp',
	bad_file(F),
	try(F, [S]>>get_char(S, _)),
	try(F, [S]>>get_code(S, _)),
	try(F, [S]>>peek_char(S, _)),
	try(F, [S]>>peek_code(S, _)),
	delete_file(F).
