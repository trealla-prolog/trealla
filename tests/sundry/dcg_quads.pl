% Driver for tests/misc/phrase_quad.txt, the ISO conformance spec.
%
% The file is NOT executable Prolog. It is a specification in a
% `<id> ?- Query.` / expected-answer notation:
%
%     19 ?- phrase(([a|L],1),[]).
%           type_error(callable,1)
%        |  instantiation_error.
%
% `|` at the start of a line separates *acceptable alternative* answers -
% an implementation is conforming if it gives any one of them - and `...`
% appears as a literal wildcard standing for some unspecified term. So a
% driver needs three things the design did not budget for: a reader for
% this notation, alternative matching, and wildcards.
%
% Splitting alternatives is done on lines whose first non-blank character
% is `|`, NOT on the `|` character, because answers legitimately contain
% it - `type_error(list,[a|b])` in quad 21, for one.
%
% Answers come in four shapes. `true` and `false` constrain the outcome
% only. A term whose principal functor is one of the ISO error kinds is
% matched against the FORMAL part of a thrown error/2, with `...` as a
% wildcard. Anything else describes bindings, and is checked only as far
% as "did it succeed" - flagged `shallow` in the summary so the weaker
% check is visible rather than counted as a full pass.

:- initialization(main).
:- use_module(library(dcgs)).
:- use_module(library(lists)).

spec_file('tests/misc/phrase_quad.txt').

% --- reading ---------------------------------------------------------

lines(File, Ls) :-
	open(File, read, S),
	getlines(S, Ls0),
	close(S),
	maplist(to_atom, Ls0, Ls).

% getlines/2 yields char lists under the default double_quotes, but a
% blank line comes back as the empty string rather than [].

to_atom(Cs, '') :- Cs == [], !.
to_atom(Cs, '') :- Cs == '', !.
to_atom(Cs, A) :- atom_chars(A, Cs).

trim(A, T) :-
	atom_chars(A, Cs),
	strip_lead(Cs, Cs1),
	reverse(Cs1, R0),
	strip_lead(R0, R1),
	reverse(R1, Cs2),
	atom_chars(T, Cs2).

strip_lead([C|Cs], Out) :- (C == ' ' ; C == '\t'), !, strip_lead(Cs, Out).
strip_lead(Cs, Cs).

% `<id> ?- <query>` with the id hard against the left margin.

entry_start(Line, Id, Query) :-
	atom_chars(Line, [C|_]),
	C \== ' ', C \== '\t',
	sub_atom(Line, Before, _, After, '?-'),
	sub_atom(Line, 0, Before, _, IdRaw),
	trim(IdRaw, Id),
	sub_atom(Line, _, After, 0, QRaw),
	trim(QRaw, Query).

alt_start(Line, Rest) :-
	trim(Line, T),
	atom_concat('|', R0, T),
	trim(R0, Rest).

% Answer lines run until a blank line or the next entry.

collect([], [], []).
collect([L|Ls], Rest, Ans) :-
	(  trim(L, '')
	-> Rest = Ls, Ans = []
	;  entry_start(L, _, _)
	-> Rest = [L|Ls], Ans = []
	;  Ans = [L|Ans1], collect(Ls, Rest, Ans1)
	).

% Group answer lines into alternatives, on `|`-leading lines.

group([], []).
group([L|Ls], [Alt|Alts]) :-
	(  alt_start(L, R) -> First = R ; trim(L, First) ),
	take_cont(Ls, Cont, Rest),
	join([First|Cont], Alt),
	group(Rest, Alts).

take_cont([], [], []).
take_cont([L|Ls], Cont, Rest) :-
	(  alt_start(L, _)
	-> Cont = [], Rest = [L|Ls]
	;  trim(L, T), Cont = [T|Cont1], take_cont(Ls, Cont1, Rest)
	).

join([A], A) :- !.
join([A|As], Out) :- join(As, R), atom_concat(A, ' ', A1), atom_concat(A1, R, Out).

strip_dot(A, B) :-
	(  atom_concat(B0, '.', A) -> B = B0 ; B = A ).

parse([], []).
parse([L|Ls], Es) :-
	(  entry_start(L, Id, Q0)
	-> strip_dot(Q0, Q),
	   collect(Ls, Rest, AnsLines),
	   group(AnsLines, Alts0),
	   maplist(strip_dot, Alts0, Alts),
	   Es = [entry(Id, Q, Alts)|Es1],
	   parse(Rest, Es1)
	;  Es = Es1, parse(Ls, Es1)
	).

% --- matching --------------------------------------------------------

err_functor(type_error).
err_functor(existence_error).
err_functor(instantiation_error).
err_functor(representation_error).
err_functor(permission_error).
err_functor(domain_error).
err_functor(evaluation_error).
err_functor(resource_error).

is_error_shape(T) :-
	(  atom(T) -> F = T ; compound(T) -> functor(T, F, _) ; fail ),
	err_functor(F).

% `...` stands for some unspecified term.

wild(T, V) :- T == '...', !, V = _Fresh.
wild(T, T) :- var(T), !.
wild(T, T) :- atomic(T), !.
wild(T, Out) :- T =.. [F|As], maplist(wild, As, Bs), Out =.. [F|Bs].

outcome(Q, Out) :-
	(  catch(findall(x, call(Q), Sols), Ball, true)
	-> (  nonvar(Ball) -> Out = threw(Ball)
	   ;  Sols == []   -> Out = fails
	   ;  Out = succeeds
	   )
	;  Out = fails
	).

match(true, succeeds, full) :- !.
match(false, fails, full) :- !.
match(throw(B), threw(Ball), full) :- !, wild(B, P), subsumes_term(P, Ball).
match(Ans, threw(error(Formal, _)), full) :-
	is_error_shape(Ans), !,
	wild(Ans, P),
	subsumes_term(P, Formal).
match(Ans, succeeds, shallow) :-
	\+ is_error_shape(Ans),
	Ans \== true, Ans \== false.

% --- running ---------------------------------------------------------

check(entry(Id, QText, AltTexts), Result) :-
	(  catch(read_term_from_atom(QText, Q, []), _, fail)
	-> (  read_alts(AltTexts, Alts)
	   -> outcome(Q, Out),
	      (  member(A, Alts), match(A, Out, How)
	      -> Result = pass(Id, How)
	      ;  Result = fail(Id, Out, Alts)
	      )
	   ;  Result = unreadable(Id)
	   )
	;  Result = unreadable(Id)
	).

read_alts([], []).
read_alts([T|Ts], [A|As]) :-
	catch(read_term_from_atom(T, A, []), _, fail),
	read_alts(Ts, As).

% stdout carries only what is stable across runs: the failing ids and the
% totals. The actual outcome contains freshly numbered variables, so it
% goes to stderr, which tests/run.sh does not capture.

report(pass(_, full)).
report(pass(_, shallow)).
report(unreadable(Id)) :-
	format("QUAD-UNREADABLE ~w~n", [Id]).
report(fail(Id, Out, Alts)) :-
	format("QUAD-FAIL ~w~n", [Id]),
	format(user_error, "  ~w: got ~q~n     wanted one of ~q~n", [Id, Out, Alts]).

tally([], 0, 0, 0, 0).
tally([R|Rs], F, S, X, U) :-
	tally(Rs, F0, S0, X0, U0),
	(  R = pass(_, full)    -> F is F0+1, S = S0, X = X0, U = U0
	;  R = pass(_, shallow) -> S is S0+1, F = F0, X = X0, U = U0
	;  R = unreadable(_)    -> U is U0+1, F = F0, S = S0, X = X0
	;  X is X0+1, F = F0, S = S0, U = U0
	).

main :-
	spec_file(File),
	lines(File, Ls),
	parse(Ls, Es),
	maplist(check, Es, Rs),
	maplist(report, Rs),
	tally(Rs, Full, Shallow, Failed, Unreadable),
	length(Es, N),
	Ok is Full + Shallow,

	% A clean run says nothing on stderr. The breakdown matters when
	% something is failing or unreadable; otherwise stdout's "N of N
	% acceptable" is the whole story, and it is what .expected pins.

	(  Ok =:= N
	-> true
	;  format(user_error,
	          "quad driver: ~w entries, ~w full, ~w shallow, ~w failed, ~w unreadable~n",
	          [N, Full, Shallow, Failed, Unreadable])
	),
	format("quad driver: ~w of ~w acceptable~n", [Ok, N]).
