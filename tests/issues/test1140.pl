% Issue #1140: an overflowing float literal raised a syntax error.
%
% 9.9e999 is syntactically perfect prolog, so 8.16.7.3 e cannot apply.
% What it exceeds is an implementation defined limit, which 8.12.2 f
% covers as a representation error. stc#74 names it max_float.
%
% The parser sets error_type alongside error_desc (as the invalid-UTF8
% case from #1099 already did); read_term honoured error_type already,
% number_chars/2 and number_codes/2 hardcoded syntax_error.
%
% https://github.com/trealla-prolog/trealla/issues/1140

:- initialization(main).

t(N, G) :-
	(  catch(G, E, (format("~w: ~q~n", [N,E]), fail))
	-> format("~w: no error~n", [N])
	;  true
	).

r(N, Text) :-
	t(N, (	read_term_from_atom(Text, T, []),
		format("~w: read ~q~n", [N,T])
	)).

main :-
	t(chars_pos,   number_chars(_, ['9','.','9','e','9','9','9'])),
	t(chars_neg,   number_chars(_, ['-','9','.','9','e','9','9','9'])),
	t(codes_pos,   number_codes(_, [0'9,0'.,0'9,0'e,0'9,0'9,0'9])),
	t(atom_number, atom_number('9.9e999', _)),
	r(read_term,   '9.9e999'),
	r(read_nested, 'f(9.9e999)'),

	% Neighbouring cases that must keep their current answers.

	t(underflow,   (number_chars(N1, ['9','.','9','e','-','9','9','9']),
			format("underflow: ~w~n", [N1]))),
	t(in_range,    (number_chars(N2, ['1','.','5','e','3']),
			format("in_range: ~w~n", [N2]))),
	t(bad_syntax,  number_chars(_, ['9','.','9','e','e'])),
	t(not_a_num,   number_chars(_, [a,b,c])).
