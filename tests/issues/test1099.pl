:- initialization(main).

% An answer may describe what the query reads: inputs/1 the characters
% it consumes, peeks/1 the single character it looks at without
% consuming, waits that it is left asking for a character none is
% there to supply.
%
% The query is run against a file holding those characters followed by
% the byte 0xff, which can never begin a UTF-8 sequence. Reading too
% much reaches the sentinel and raises a representation error; reading
% too little leaves characters in the stream, which the harness looks
% for afterwards. 'waits' is the case where reaching the sentinel is
% the described outcome.

% consumes exactly what it says

?- get_char(C).
   inputs("a"), C = a.

% reads nothing at all

?- true.
   inputs("").

% peeks without consuming

?- peek_char(C).
   peeks("a"), C = a.

% consumes, then looks one character past

?- get_char(A), peek_char(B).
   inputs("x"), peeks("y"), A = x, B = y.

% read/1 consumes the end token and peeks one past it, so that two
% dots would not be mistaken for one

?- read(G).
   inputs("writeq(a)."), peeks("\n"), G = writeq(a).

% is left asking for a character

?- get_char(_).
   inputs(""), waits.

% reads less than described: b is left in the stream

?- get_char(C).
   inputs("ab"), C = a.

% reads more than described: reaches the sentinel

?- get_char(C), get_char(_).
   inputs("a"), C = a.

% claims a peek but consumes

?- get_char(C).
   peeks("a"), C = a.

% claims to wait but does not

?- true.
   inputs(""), waits.

% peeks/1 is one character

?- get_char(C).
   peeks("ab"), C = a.

% inputs/1 is characters, not a DCG body: an answer says exactly what
% was read, and a nonterminal may stand for more than one string

?- get_char(C).
   inputs(seq([a])), C = a.

% a query is run once against one input

?- get_char(C).
   inputs("a"), inputs("b"), C = a.

% a peek leaves the character, so every later read returns it and the
% query can never be left waiting

?- peek_char(C).
   peeks("c"), waits.

% run_quads names the file as it was consulted, so the report would
% otherwise depend on the path this was invoked with. Keep the base
% name only.

strip_dirs(Cs, Out) :- strip_dirs(Cs, [], Out).

strip_dirs([], W, Out) :- reverse(W, Out).
strip_dirs([C|Cs], W, Out) :-
	(	C == (/)
	->	strip_dirs(Cs, [], Out)
	;	C == ' '
	->	reverse([C|W], Pre), append(Pre, Out0, Out), strip_dirs(Cs, [], Out0)
	;	C == '\n'
	->	reverse([C|W], Pre), append(Pre, Out0, Out), strip_dirs(Cs, [], Out0)
	;	strip_dirs(Cs, [C|W], Out)
	).

main :-
	use_module(library(quads)),
	with_output_to(chars(Cs), run_quads),
	strip_dirs(Cs, Out),
	atom_chars(A, Out),
	write(A).
