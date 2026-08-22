% The empty_args flag: f() reads as '()'(f), and writes back as f().
%
% ISO has no zero-arity compound and this does not add one - f() is
% read into an ordinary compound of arity 1, so functor/3, =.. and the
% writer need no special cases. The flag exists because the Janus
% interface (SWI and XSB both) spells every zero-argument method call
% f(); see docs/janus-design.md section 2a.
%
% Off by default, which is what keeps the reader ISO-conformant.

:- initialization(main).

show(Label, Goal) :-
	(   catch(Goal, E, (format("~w: raised ~p~n", [Label, E]), fail))
	->  true
	;   format("~w: failed~n", [Label])
	).

roundtrip(Label, T) :-
	format(atom(W), "~q", [T]),
	(   catch(read_term_from_atom(W, Back, []), _, fail)
	->  ( Back == T -> R = 'reads back' ; R = 'DIFFERS' )
	;   R = 'unreadable'
	),
	format("~w: ~w  ~w~n", [Label, W, R]).

main :-
	current_prolog_flag(empty_args, Default),
	format("default: ~w~n", [Default]),

	% Off: the syntax is rejected, and the term still prints readably.
	show('off, f() rejected',
	     (   catch(read_term_from_atom('foo()', _, []), _, fail)
	     ->  write('off, f() rejected: PARSED')
	     ;   write('off, f() rejected: syntax error')
	     )), nl,
	roundtrip('off, term prints as', '()'(foo)),

	set_prolog_flag(empty_args, true),
	current_prolog_flag(empty_args, On),
	format("after set: ~w~n", [On]),

	% On: f() reads as an ordinary compound, distinct from the atom.
	read_term_from_atom('foo()', T, []),
	format("foo() reads as ~q~n", [T]),
	( T == '()'(foo) -> write('same as ''()''(foo)') ; write('NOT ''()''(foo)') ), nl,
	( T == foo -> write('same as the atom: WRONG') ; write('distinct from the atom foo') ), nl,
	( compound(T) -> write('compound: yes') ; write('compound: no') ), nl,
	functor(T, N, A), format("functor ~q/~w~n", [N, A]),

	% and writes back as itself
	roundtrip('on, plain', '()'(foo)),
	roundtrip('on, quoted name', '()'('hello world')),
	roundtrip('on, nested', f(a, '()'(close), b)),
	roundtrip('on, non-atom arg', '()'(1)),

	% things that must NOT change
	roundtrip('on, ordinary compound', foo(bar)),
	roundtrip('on, atom', foo),
	roundtrip('on, list', [a,b]),
	roundtrip('on, curly', {a}),
	show('bare () still rejected',
	     (   catch(read_term_from_atom('x = ()', _, []), _, fail)
	     ->  write('bare () still rejected: PARSED')
	     ;   write('bare () still rejected: syntax error')
	     )), nl.
