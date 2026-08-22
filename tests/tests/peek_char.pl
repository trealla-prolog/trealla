:- initialization(main).

% A peek must not consume what it reports on, including when what it
% reports is that the entity input is not a character. An ill-formed
% sequence used to be swallowed by the peek that raised the error, so
% the following get_char/2 saw whatever came after it.
%
% The 0xff byte is the useful case: it can never begin a UTF-8
% sequence, so it is the standard way to mark "nothing may be read
% past here" in a test input.

write_bytes([], _).
write_bytes([B|Bs], S) :- put_byte(S, B), write_bytes(Bs, S).

make_file(File, Bytes) :-
	open(File, write, S, [type(binary)]),
	write_bytes(Bytes, S),
	close(S).

show(Label, Goal) :-
	(	catch(Goal, error(E, _), (write(Label-threw(E)), nl, fail))
	->	true
	;	true
	).

probe(File, Bytes) :-
	make_file(File, Bytes),
	open(File, read, S, []),
	show(peek_1, (peek_char(S, A), write(peek(A)), nl)),
	show(peek_2, (peek_char(S, B), write(peek(B)), nl)),
	show(get_1,  (get_char(S, C),  write(get(C)),  nl)),
	show(get_2,  (get_char(S, D),  write(get(D)),  nl)),
	close(S),
	nl.

main :-
	File = 'tmp.peek',
	probe(File, [0'a, 0'b]),			% well formed
	probe(File, [0xc3, 0xa9, 0'z]),		% well formed, two octets
	probe(File, [0'a, 0xff]),			% the sentinel behind a character
	probe(File, [0xff, 0'a]),			% the sentinel first
	probe(File, [0'a, 0xc3]),			% truncated sequence at eof
	( catch(delete_file(File), _, true) -> true ; true ).
