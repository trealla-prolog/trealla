:- initialization(main).

% with_output_to/2 nests. '$capture_output' used to be a toggle on the
% stream, so a capture started inside another turned capturing off and
% freed the outer buffer with it: both came back empty and the text
% went to the real output. The captures now share the buffer, each
% remembering how much of it was already there, and taking one
% truncates the buffer back so the capture around it never sees it.

check(Name, Goal, Expected) :-
	(	catch(call(Goal, Got), error(E, _), Got = threw(E))
	->	true
	;	Got = failed
	),
	(	Got == Expected
	->	write(Name), write(' ok'), nl
	;	write(Name), write(' FAILED got '), writeq(Got),
		write(' wanted '), writeq(Expected), nl
	).

% the inner text belongs to the inner capture and to no other

nested(Out-In) :-
	with_output_to(chars(Out),
		(	write(a),
			with_output_to(chars(In), write(b)),
			write(c)
		)).

% and it holds however deep they go

deep([A,B,C]) :-
	with_output_to(chars(A),
		(	write(1),
			with_output_to(chars(B),
				(	write(2),
					with_output_to(chars(C), write(3)),
					write(4)
				)),
			write(5)
		)).

% the atom sink nests the same way

atoms(X-Y) :-
	with_output_to(atom(X),
		(	write(a),
			with_output_to(atom(Y), write(b)),
			write(c)
		)).

% two captures in sequence inside one, neither leaking into the other

siblings([A,B,C]) :-
	with_output_to(chars(A),
		(	with_output_to(chars(B), write(x)),
			write(m),
			with_output_to(chars(C), write(y))
		)).

% a goal that throws out of an inner capture still gives the stream
% back: the outer capture's cleanup runs, and writing works after

after_throw(done) :-
	catch(
		with_output_to(chars(_),
			(	write(x),
				with_output_to(chars(_), throw(oops))
			)),
		oops, true),
	with_output_to(chars(Cs), write(ok)),
	Cs == [o,k].

% an unnested capture is what it always was

plain(X) :- with_output_to(chars(X), write(hello)).

empty(X) :- with_output_to(chars(X), true).

main :-
	check(nested, nested, [a,c]-[b]),
	check(deep, deep, [['1','5'],['2','4'],['3']]),
	check(atoms, atoms, ac-b),
	check(siblings, siblings, [[m],[x],[y]]),
	check(after_throw, after_throw, done),
	check(plain, plain, [h,e,l,l,o]),
	check(empty, empty, []).
