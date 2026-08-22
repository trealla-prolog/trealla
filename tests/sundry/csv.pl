:- initialization(main).

% CSV: parse_csv_line/2,3 and write_csv_file/3.
%
% Each check states its own expected value, so csv.expected is just a
% list of "ok" lines and a regression shows up as "FAILED got ...".
% The two write checks additionally print the exact bytes written, so
% a quoting change is pinned in csv.expected rather than hidden behind
% a round-trip that might compensate for its own bug.

:- use_module(library(lists)).

% In the current directory, not /tmp: Windows and WASI have no such
% path, and the runner already works from the repo root. Deleted by
% cleanup/0 below.

tmpfile('tmp.csv_test.csv').

% --------------------------------------------------------------- util

% parse_csv_line yields each field as a char list (or a number under
% numbers(true)); normalise to atoms so the expectations read plainly.
norm(F, A) :- is_list(F), !, atom_chars(A, F).
norm(F, F).

fields(Text, Fs) :-
	atom_codes(Text, Cs),
	parse_csv_line(Cs, Raw),
	maplist(norm, Raw, Fs).

fields(Text, Opts, Fs) :-
	atom_codes(Text, Cs),
	parse_csv_line(Cs, Raw, Opts),
	maplist(norm, Raw, Fs).

chk(Name, Got, Want) :-
	(	Got == Want
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q want ~q~n", [Name, Got, Want])
	).

chk_err(Name, Goal, Want) :-
	catch((call(Goal), Got = no_error), error(E,_), Got = E),
	(	Got == Want
	->	format("~w: ok~n", [Name])
	;	format("~w: FAILED got ~q want ~q~n", [Name, Got, Want])
	).

str_atom(S, A) :- string_codes(S, Cs), atom_codes(A, Cs).

read_lines(S, Ls) :-
	read_line_to_string(S, L),
	(	L == end_of_file
	->	Ls = []
	;	Ls = [L|T], read_lines(S, T)
	).

file_lines(Lines) :-
	tmpfile(F), open(F, read, S, []), read_lines(S, Lines), close(S).

% ------------------------------------------------------------ parsing

test_plain :-
	fields('a,b,c', Fs),
	chk(plain, Fs, [a,b,c]).

test_empty_fields :-
	fields(',,', Fs),
	chk(empty_fields, Fs, ['','','']).

test_quoted_plain :-
	fields('"x","y","z"', Fs),
	chk(quoted_plain, Fs, [x,y,z]).

% A quoted field may contain the separator - that is the whole point of
% quoting, and getting it wrong silently splits one field into two.
test_quoted_sep :-
	fields('"a,b",c', Fs),
	chk(quoted_sep, Fs, ['a,b', c]).

test_quoted_sep_last :-
	fields('c,"a,b"', Fs),
	chk(quoted_sep_last, Fs, [c, 'a,b']).

% RFC 4180: "" inside a quoted field is a literal quote and the field
% STAYS quoted. Before this was fixed the parser left quoted state after
% the escaped quote, so the following separator was taken literally and
% `"a""b",c` came back as the single field `a"b,c`.
test_escaped_quote :-
	fields('"a""b",c', Fs),
	chk(escaped_quote, Fs, ['a"b', c]).

test_escaped_quote_only :-
	fields('""""', Fs),
	chk(escaped_quote_only, Fs, ['"']).

test_mixed_quoting :-
	fields('"a",b,"c,d",e', Fs),
	chk(mixed_quoting, Fs, [a, b, 'c,d', e]).

% ------------------------------------------------------------ options

test_opt_sep :-
	fields('a;"b;c";d', [sep(';')], Fs),
	chk(opt_sep, Fs, [a, 'b;c', d]).

test_opt_numbers :-
	fields('1,2,x', [numbers(true)], Fs),
	chk(opt_numbers, Fs, [1, 2, x]).

test_opt_trim :-
	fields('  a  ,  b  ', [trim(true)], Fs),
	chk(opt_trim, Fs, [a, b]).

test_opt_functor :-
	atom_codes('a,b', Cs),
	parse_csv_line(Cs, Row, [functor(row)]),
	Row =.. [F|Raw],
	maplist(norm, Raw, Fs),
	chk(opt_functor, F-Fs, row-[a,b]).

% arity/1 pins the expected column count; a mismatch must be reported
% rather than silently accepted.
test_opt_arity_ok :-
	atom_codes('a,b', Cs),
	parse_csv_line(Cs, Row, [functor(row), arity(2)]),
	functor(Row, F, N),
	chk(opt_arity_ok, F/N, row/2).

test_opt_arity_bad :-
	atom_codes('a,b,c', Cs),
	chk_err(opt_arity_bad,
		parse_csv_line(Cs, _, [functor(row), arity(2)]),
		domain_error(row_arity, 2)).

% ------------------------------------------------------------ writing

% The exact bytes matter: a field containing the separator or a quote
% must come back out quoted, with embedded quotes doubled. Printing the
% line pins that in csv.expected.
test_write_quoting :-
	tmpfile(F),
	write_csv_file(F, [[a,'b,c'],[d,'say "hi"']], []),
	file_lines(Lines),
	maplist(str_atom, Lines, As),
	chk(write_quoting, As, ['a,"b,c"', 'd,"say ""hi"""']).

test_write_plain :-
	tmpfile(F),
	write_csv_file(F, [[a,b],[1,2]], []),
	file_lines(Lines),
	maplist(str_atom, Lines, As),
	chk(write_plain, As, ['a,b', '1,2']).

% ---------------------------------------------------------- round trip

roundtrip(Rows) :-
	tmpfile(F),
	write_csv_file(F, Rows, []),
	file_lines(Lines),
	maplist(line_fields, Lines, Back),
	Back = Rows.

line_fields(Line, Fs) :-
	string_codes(Line, Cs),
	parse_csv_line(Cs, Raw),
	maplist(norm, Raw, Fs).

test_roundtrip :-
	Cases = [ [[a,b],[c,d]],
	          [[a,'b,c'],[d,e]],
	          [[a,'say "hi"'],[b,c]],
	          [['x,y','a"b']],
	          [[a,'',b]],
	          [['"']],
	          [[',']] ],
	(	forall(member(Rows, Cases), roundtrip(Rows))
	->	format("roundtrip: ok~n")
	;	member(Bad, Cases), \+ roundtrip(Bad),
		format("roundtrip: FAILED on ~q~n", [Bad])
	).

% -------------------------------------------------------------- errors

test_err_write_bad_row :-
	tmpfile(F),
	chk_err(err_write_bad_row,
		write_csv_file(F, [notalist], []),
		type_error(list, notalist)).

% ------------------------------------------------------------------ go

cleanup :- tmpfile(F), ( catch(delete_file(F), _, true) -> true ; true ).

% Each check runs independently: a failing or throwing test must not
% stop the ones after it, or the first regression hides every later
% one. (Seen for real - on a pre-fix build this file used to abort at
% escaped_quote and never reached the write or round-trip checks.)

run(T) :-
	(	catch(T, E, (format("~w: EXCEPTION ~q~n", [T, E]), true))
	->	true
	;	format("~w: FAILED (goal failed)~n", [T])
	).

main :-
	forall(member(T, [test_plain,
	                  test_empty_fields,
	                  test_quoted_plain,
	                  test_quoted_sep,
	                  test_quoted_sep_last,
	                  test_escaped_quote,
	                  test_escaped_quote_only,
	                  test_mixed_quoting,
	                  test_opt_sep,
	                  test_opt_numbers,
	                  test_opt_trim,
	                  test_opt_functor,
	                  test_opt_arity_ok,
	                  test_opt_arity_bad,
	                  test_write_quoting,
	                  test_write_plain,
	                  test_roundtrip,
	                  test_err_write_bad_row]),
	       run(T)),
	cleanup.
