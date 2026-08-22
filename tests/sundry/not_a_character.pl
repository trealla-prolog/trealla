:- initialization(main).

% An octet that cannot be part of a UTF-8 sequence is not a character,
% and ISO 13211-1 8.12.1.3 i makes reading one a representation error.
% get_char/2 and peek_char/2 said so already; read/2 reported a syntax
% error, and a character left behind in the line read/2 had buffered
% was decoded silently and then reported as the end of the file
% (issue #1099).
%
% Each check states what it expects, so not_a_character.expected is a
% list of "ok" lines and a regression reads as "FAILED got ...".

tmpfile('/tmp/tpl_not_a_character.txt').

% Write Text, then the octet 0xff behind it.

write_sentinel_file(Text) :-
	tmpfile(F),
	open(F, write, S, []),
	write(S, Text),
	close(S),
	open(F, append, B, [type(binary)]),
	put_byte(B, 0xff),
	close(B).

check(Name, Goal, Expected) :-
	tmpfile(F),
	open(F, read, S, []),
	(	catch(call(Goal, S, Got), E, Got = threw(E))
	->	true
	;	Got = failed
	),
	catch(close(S), _, true),
	(	Got == Expected
	->	write(Name), write(' ok'), nl
	;	write(Name), write(' FAILED got '), writeq(Got),
		write(' wanted '), writeq(Expected), nl
	).

rep_err(G, threw(error(representation_error(character), G))).

% --------------------------------------------------------------------

get_one(S, C) :- get_char(S, C).
peek_one(S, C) :- peek_char(S, C).
read_one(S, T) :- read(S, T).

% read/1, then what the harness looks for behind the term it read

read_then_get(S, C) :- read(S, _), get_char(S, C).
read_then_get2(S, C) :- read(S, _), get_char(S, _), get_char(S, C).

% a peek does not consume, so peeking twice throws twice

peek_twice(S, C) :-
	catch(peek_char(S, _), _, true),
	peek_char(S, C).

main :-
	% nothing but the sentinel
	write_sentinel_file(''),
	rep_err(get_char/2, GetErr),
	check(get_char, get_one, GetErr),
	rep_err(peek_char/2, PeekErr),
	check(peek_char, peek_one, PeekErr),
	rep_err(read/2, ReadErr),
	check(read, read_one, ReadErr),

	% a term, a space, then the sentinel: read/2 takes the whole line
	% into the parser's buffer, and the space and the sentinel must
	% still be found behind it
	write_sentinel_file('1. '),
	check(read_then_get, read_then_get, ' '),
	check(read_then_get2, read_then_get2, GetErr),

	% peeking is idempotent
	write_sentinel_file(''),
	check(peek_twice, peek_twice, PeekErr),

	% valid multi-byte text is unaffected
	write_sentinel_file('héllo. '),
	check(read_accented, read_one, 'héllo'),

	tmpfile(F),
	catch(delete_file(F), _, true).
