:- initialization(main).

% unget_char/1,2 take a character, not its code - the argument is
% checked as in_character - but the code point was read out of the
% cell with get_smallint(), which for an atom is its offset in the
% symbol table. unget_char(S, a) therefore pushed back U+6101,
% wherever the atom 'a' happened to sit.
%
% The /2 clause also fetched a character out of the parser's line
% buffer into the same one-character slot before overwriting it, so
% ungetting after read/1 swallowed whatever the term was followed by.

tmpfile('/tmp/tpl_unget_char.txt').

make_file(Text) :-
	tmpfile(F),
	open(F, write, S, []),
	write(S, Text),
	close(S).

check(Name, Goal, Expected) :-
	tmpfile(F),
	open(F, read, S, []),
	(	catch(call(Goal, S, Got), error(E, _), Got = threw(E))
	->	true
	;	Got = failed
	),
	catch(close(S), _, true),
	(	Got == Expected
	->	write(Name), write(' ok'), nl
	;	write(Name), write(' FAILED got '), writeq(Got),
		write(' wanted '), writeq(Expected), nl
	).

% --------------------------------------------------------------------

% what goes back comes back, and the stream carries on behind it

roundtrip(S, [A,B,C]) :-
	get_char(S, A), unget_char(S, A),
	get_char(S, B), get_char(S, C).

% a character the stream never held is put back just the same

arbitrary(S, [A,B]) :-
	unget_char(S, 'Z'), get_char(S, A), get_char(S, B).

% the character need not be one byte

multibyte(S, [A,B,C]) :-
	get_char(S, _), get_char(S, A), unget_char(S, A),
	get_char(S, B), get_char(S, C).

% read/1 takes the whole line into the parser's buffer; ungetting must
% not consume what is left of it

after_read(S, [T,A,B,C]) :-
	read(S, T), unget_char(S, 'Z'),
	get_char(S, A), get_char(S, B), get_char(S, C).

% end_of_file is admitted by in_character, and the get_char/2 that
% reported it consumed nothing, so putting it back restores nothing

unget_eof(S, A) :-
	unget_char(S, end_of_file), get_char(S, A).

at_real_eof(S, [E,F]) :-
	get_char(S, _), get_char(S, _), get_char(S, _), get_char(S, E),
	unget_char(S, end_of_file), get_char(S, F).

% the empty atom passes the type check but is no character

empty(S, _) :- unget_char(S, '').

% the current-input clause behaves the same

current(S, [A,B,C]) :-
	current_input(Old),
	set_input(S),
	get_char(A), unget_char(A), get_char(B), get_char(C),
	set_input(Old).

main :-
	make_file('abc'),
	check(roundtrip, roundtrip, [a,a,b]),
	check(arbitrary, arbitrary, ['Z',a]),
	check(unget_eof, unget_eof, a),
	check(at_real_eof, at_real_eof, [end_of_file,end_of_file]),
	check(empty, empty, threw(type_error(in_character, ''))),
	check(current_input, current, [a,a,b]),

	make_file('héllo'),
	check(multibyte, multibyte, ['é','é',l]),

	make_file('1. abc'),
	check(after_read, after_read, [1,'Z',' ',a]),

	tmpfile(F),
	catch(delete_file(F), _, true).
