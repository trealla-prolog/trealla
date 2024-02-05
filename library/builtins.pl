:- pragma(builtins, [once(true)]).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).

expand_term((H --> B), Out) :-
	dcg_translate((H --> B), Out), !.

dcg_translate(TermIn, Term) :-
	nonvar(TermIn),
	dcg_rule(TermIn, Term).

predicate_property(P, A) :-
	nonvar(P), atom(A), !,
	'$must_be'(P, callable, predicate_property/2, _),
	'$legacy_predicate_property'(P, A).
predicate_property(P, A) :-
	'$load_properties',
	(	var(A) ->
		true
	; 	(Controls = [
			built_in,choice_construct,
			discontiguous,private,static,
			dynamic,foreign,tabled,multifile,
			meta_predicate(_),iso,visible,
			template(_)
			],
			memberchk(A, Controls) ->
				true
			;	throw(error(domain_error(predicate_property, A), P))
		)
	),
	'$must_be'(P, callable, predicate_property/2, _),
	(	P = (M:P2) ->
		M:'$predicate_property'(predicate, P2, A)
	;	'$predicate_property'(predicate, P, A)
	).

:- help(predicate_property(+callable,+term), [iso(true)]).

evaluable_property(P, A) :-
	nonvar(P), atom(A), !,
	'$must_be'(P, callable, evaluable_property/2, _),
	'$legacy_evaluable_property'(P, A).
evaluable_property(P, A) :-
	'$load_properties',
	(	var(A) ->
		true
	; 	(Controls = [iso,built_in,static,dynamic,template(_),template(_,_)],
		memberchk(A, Controls) ->
			true
		;	(
			'$must_be'(A, callable, evaluable_property/2, _),
			throw(error(domain_error(evaluable_property, A), P))
			)
		)
	),
	'$must_be'(P, callable, evaluable_property/2, _),
	(	P = (M:P2) ->
		M:'$predicate_property'(function, P2, A)
	;	'$predicate_property'(function, P, A)
	).

:- help(evaluable_property(+callable,+term), [iso(true)]).

current_prolog_flag(P, A) :-
	nonvar(P), !,
	'$legacy_current_prolog_flag'(P, A).
current_prolog_flag(P, A) :-
	'$load_flags',
	'$current_prolog_flag'(P, A).

:- help(current_prolog_flag(+callable,+term), [iso(true)]).

subsumes_term(G, S) :-
	\+ \+ (
	 term_variables(S, V1),
	 G = S,
	 term_variables(V1, V2),
	 V2 == V1
	).

:- help(subsumes_term(+term,+term), [iso(true)]).

% definition taken from the SWI-Prolog documentation
variant(Term1, Term2) :-
	% avoid trouble in any shared variables
	copy_term(Term1, Term1Copy),
	copy_term(Term2, Term2Copy),
	% ground and compare the term copies
	numbervars(Term1Copy, 0, N),
	numbervars(Term2Copy, 0, N),
	Term1Copy == Term2Copy.

:- help(variant(+term,+term), [iso(false)]).

catch(G, E, C) :-
	'$catch'(call(G), E, call(C)).

:- meta_predicate(call_det(0,?)).

call_det(G, Det) :-
	'$get_level'(L1),
	G,
	'$get_level'(L2),
	(L1 = L2 -> Det = true; Det = false).

:- help(call_det(:callable,?boolean), [iso(false)]).

:- meta_predicate(findall(?,0,-,?)).

findall(T, G, B, Tail) :-
	'$can_be'(B, list, findall/4, _),
	'$can_be'(Tail, list, findall/4, _),
	findall(T, G, B0),
	append(B0, Tail, B), !.

:- help(findall(+term,:callable,-list,+list), [iso(false)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Derived from code by R.A. O'Keefe

:- meta_predicate(setof(-,0,?)).

setof(Template, Generator, Set) :-
	( 	var(Set) ->
		true
	; 	'$must_be'(Set, list_or_partial_list, setof/3, _)
	),
	bagof_(Template, Generator, Bag),
	is_list_or_partial_list(Set),
	sort(Bag, Set).

:- help(setof(+term,+callable,?list), [iso(true)]).

:- meta_predicate(bagof(-,0,?)).

bagof(Template, Generator, Bag) :-
	(var(Bag) -> true; '$must_be'(Bag, list_or_partial_list, bagof/3, _)),
	bagof_(Template, Generator, Bag).

:- help(bagof(+term,:callable,?list), [iso(true)]).

bagof_(Template, Generator, Bag) :-
	acyclic_term(Generator),
	free_variables_(Generator, Template, [], Vars, 1),
	Vars \== [],
	!,
	Key =.. [(.)|Vars],
	functor(Key, (.), N),
	findall(Key-Template, Generator, Recorded),
	replace_instance_(Recorded, Key, N, _, OmniumGatherum),
	keysort(OmniumGatherum, Gamut), !,
	concordant_subset_(Gamut, Key, Answer),
	Bag = Answer.
bagof_(Template, Generator, Bag) :-
	findall(Template, Generator, Bag0),
	Bag0 \== [],
	Bag = Bag0.

_^Goal :- Goal.

replace_instance_([], _, _, _, []) :- !.
replace_instance_([NewKey-Term|Xs], Key, NVars, Vars, [NewKey-Term|NewBag]) :-
	replace_key_variables_(NVars, Key, Vars, NewKey), !,
	replace_instance_(Xs, Key, NVars, Vars, NewBag).


%   Original R.A. O'Keefe comment:
%   There is a bug in the compiled version of arg in Dec-10 Prolog,
%   hence the rather strange code.  Only two calls on arg are needed
%   in Dec-10 interpreted Prolog or C-Prolog.

replace_key_variables_(0, _, _, _) :- !.
replace_key_variables_(N, OldKey, Vars0, NewKey) :-
	arg(N, NewKey, Arg),
	nonvar(Arg), !,
	replace_variables_(Arg, Vars0, Vars1),
	M is N-1,
	replace_key_variables_(M, OldKey, Vars1, NewKey).
replace_key_variables_(N, OldKey, Vars, NewKey) :-
	%arg(N, OldKey, OldVar),
	arg(N, NewKey, _OldVar),
	M is N-1,
	replace_key_variables_(M, OldKey, Vars, NewKey).

replace_variables_(Term, [Var|Vars], Vars) :-
	var(Term), !,
	Term = Var.
replace_variables_(Term, Vars, Vars) :-
	atomic(Term), !.
replace_variables_(Term, Vars0, Vars) :-
	functor(Term, _, Arity),
	replace_variables_term_(Arity, Term, Vars0, Vars).

replace_variables_term_(0, _, Vars, Vars) :- !.
replace_variables_term_(N, Term, Vars0, Vars) :-
	arg(N, Term, Arg),
	(	cyclic_term(Arg) ->
		N1 is N-1,
		replace_variables_term_(N1, Term, Vars0, Vars)
	;	replace_variables_(Arg, Vars0, Vars1),
		N1 is N-1,
		replace_variables_term_(N1, Term, Vars1, Vars)
	).

/*
%   concordant_subset_([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.
*/

concordant_subset_([Key-Val|Rest], Clavis, Answer) :-
	concordant_subset_(Rest, Key, List, More),
	concordant_subset_(More, Key, [Val|List], Clavis, Answer).

/*
%   concordant_subset_(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.
*/

concordant_subset_([Key-Val|Rest], Clavis, List, More) :-
	subsumes_term(Key, Clavis),
	subsumes_term(Clavis, Key),
	!,
	Key = Clavis,
	List = [Val|Rest2],
	concordant_subset_(Rest, Clavis, Rest2, More).
concordant_subset_(More, _, [], More).

/*
%   concordant_subset_/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choicepoint when this is
%   the last possible subset.
*/

concordant_subset_([],   Key, Subset, Key, Subset) :- !.
concordant_subset_(_,    Key, Subset, Key, Subset).
concordant_subset_(More, _,   _,   Clavis, Answer) :-
	concordant_subset_(More, Clavis, Answer).

% 0 disables use of explicit_binding_, 1 enables them
% setof stuff still uses 1, that's closer to it's usual implementation
free_variables_(A,B,C,D) :- free_variables_(A,B,C,D,0).

% ---extracted from: not.pl --------------------%

%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: "suspicious" negation

%   In order to handle variables properly, we have to find all the
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
% a)  they occur in the template
% b)  they are bound by X^P, setof, or bagof
%   free_variables_(Generator, Template, OldList, NewList,CheckBindings=0,1)
%   finds this set, using OldList as an accumulator.

free_variables_(Term, Bound, VarList, [Term|VarList],_) :-
	var(Term),
	term_is_free_of_(Bound, Term),
	list_is_free_of_(VarList, Term),
	!.
free_variables_(Term, _, VarList, VarList,_) :-
	var(Term),
	!.
free_variables_(Term, Bound, OldList, NewList, 1) :-
	explicit_binding_(Term, Bound, NewTerm, NewBound),
	!,
	free_variables_(NewTerm, NewBound, OldList, NewList, 1).
free_variables_(Term, Bound, OldList, NewList, _) :-
	functor(Term, _, N),
	free_variables_(N, Term, Bound, OldList, NewList, 0).

free_variables_(0,    _,     _, VarList, VarList, _) :- !.
free_variables_(N, Term, Bound, OldList, NewList, B) :-
	arg(N, Term, Argument),
	(	cyclic_term(Argument) ->
		M is N-1, !,
		free_variables_(M, Term, Bound, OldList, NewList, B)
	;	free_variables_(Argument, Bound, OldList, MidList, B),
		M is N-1, !,
		free_variables_(M, Term, Bound, MidList, NewList, B)
	).

%   explicit_binding_ checks for goals known to existentially quantify
%   one or more variables.  In particular "not" is quite common.

explicit_binding_(\+(_),     Bound, fail, Bound ).
explicit_binding_(not(_),    Bound, fail, Bound ).
explicit_binding_(Term^Goal, Bound, Goal, Bound+Vars) :-
	term_variables(Term, Vars).
explicit_binding_(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var).
explicit_binding_(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var).

term_is_free_of_(Term, Var) :-
	var(Term), !,
	Term \== Var.
term_is_free_of_(Term, Var) :-
	functor(Term, _, N),
	term_is_free_of_(N, Term, Var).

term_is_free_of_(0, _, _) :- !.
term_is_free_of_(N, Term, Var) :-
	arg(N, Term, Argument),
	term_is_free_of_(Argument, Var),
	M is N-1, !,
	term_is_free_of_(M, Term, Var).

list_is_free_of_([], _).
list_is_free_of_([Head|Tail], Var) :-
	Head \== Var,
	list_is_free_of_(Tail, Var).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edinburgh...

get0(C) :- get_code(C).
get0(S, C) :- get_code(S, C).
display(T) :- write_canonical(T).
display(S, T) :- write_canonical(S, T).
put(C) :- put_code(C).
put(S,C) :- put_code(S, C).
see(F) :- open(F, read, S), set_input(S).
tell(F) :- open(F, write, S), set_output(S).
append(F) :- open(F, append, S), set_output(S).
file_exists(F) :- exists_file(F).
directory_exists(F) :- exists_directory(F).

:- help(get0(?integer), [iso(false),deprecated(true)]).
:- help(get0(+stream,?integer), [iso(false),deprecated(true)]).
:- help(get0(+term), [iso(false),deprecated(true)]).
:- help(get0(+stream,+term), [iso(false),deprecated(true)]).
:- help(put(+integer), [iso(false),deprecated(true)]).
:- help(put(+stream,+integer), [iso(false),deprecated(true)]).
:- help(see(+filename), [iso(false),deprecated(true)]).
:- help(tell(+filename), [iso(false),deprecated(true)]).
:- help(append(+filename), [iso(false),deprecated(true)]).
:- help(file_exists(+filename), [iso(false),deprecated(true)]).
:- help(directory_exists(+filename), [iso(false),deprecated(true)]).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate(not(0)).

not(X) :- X, !, fail.
not(_).

:- help(not(:callable), [iso(false),deprecated(true)]).

current_key(K) :- var(K), '$record_global_key'(K,_).
recorda(K, V) :- nonvar(K), nonvar(V), asserta('$record_global_key'(K,V)).
recordz(K, V) :- nonvar(K), nonvar(V), assertz('$record_global_key'(K,V)).
recorded(K, V) :- nonvar(K), '$record_global_key'(K,V).
recorda(K, V, R) :- nonvar(K), nonvar(V), asserta('$record_global_key'(K,V), R).
recordz(K, V, R) :- nonvar(K), nonvar(V), assertz('$record_global_key'(K,V), R).
recorded(K, V, R) :- nonvar(K), clause('$record_global_key'(K,V), _, R).

:- help(current_key(-term), [iso(false)]).
:- help(recorda(+term,+term), [iso(false)]).
:- help(recorda(+term,+term,-ref), [iso(false)]).
:- help(recordz(+term,+term), [iso(false)]).
:- help(recordz(+term,+term,-ref), [iso(false)]).
:- help(recorded(+term,?term), [iso(false)]).
:- help(recorded(+term,?term,-ref), [iso(false)]).

:- meta_predicate(call_with_time_limit(+,0)).

call_with_time_limit(Time, Goal) :-
	Time0 is truncate(Time * 1000),
	'$alarm'(Time0),
	(	catch(once(Goal), E, ('$alarm'(0), throw(E))) ->
		'$alarm'(0)
	;	('$alarm'(0), fail)
	).

:- help(call_with_time_limit(+millisecs,:callable), [iso(false)]).

:- meta_predicate(time_out(0,+,-)).

time_out(Goal, Time, Result) :-
	'$alarm'(Time),
	(	catch(once(Goal), E, ('$alarm'(0), throw(E))) ->
		('$alarm'(0), Result = success)
	;	('$alarm'(0), fail)
	).

:- help(time_out(:callable,+integer,?atom), [iso(false)]).

print(T) :- bwrite(user_output, T), nl.
print(S, T) :- bwrite(S, T), nl.

:- help(print(+term), [iso(false)]).
:- help(print(+stream,+term), [iso(false)]).

writeln(T) :- write(T), nl.

:- help(writeln(+term), [iso(false)]).

format(F) :- format(F, []).

:- help(format(+term), [iso(false)]).

open(F, M, S) :- open(F, M, S, []).

:- help(open(+atom,+atom,--stream), [iso(true)]).

:- meta_predicate(engine_create(?,0,?)).

engine_create(T, G, S) :- engine_create(T, G, S, []).

:- help(engine_create(+term,+callable,?stream), [iso(false)]).

engine_post(E, T, R) :-
	engine_post(E, T),
	engine_next(E, R).

current_engine(E) :-
	stream_property(E, engine(true)).

samsort(L, R) :- msort(L, R).

:- help(samsort(+list,?list), [iso(false)]).

atomic_list_concat(L, Atom) :- atomic_list_concat(L, '', Atom).

:- help(atomic_list_concat(+list,+atomic), [iso(false)]).

partial_string(S, P) :- append(S, _, P).
partial_string(S, P, V) :- append(S, V, P).

chars_base64(Plain, Base64, Opts) :- base64(Plain, Base64, Opts).

:- help(chars_base64(+atom,?atom,+list), [iso(false)]).

chars_urlenc(Plain, Url, Opts) :- urlenc(Plain, Url, Opts).

:- help(chars_urlenc(+atom,?atom,+list), [iso(false)]).

term_to_atom(T, S) :- write_term_to_chars(T, [], S).

:- help(term_to_atom(+term,?atom), [iso(false)]).

absolute_file_name(R, A) :- absolute_file_name(R, A, []).

:- help(absolute_filename(+atom,?atom), [iso(false)]).

client(Url, S) :- client(Url, _, _, S, []).

:- help(client(+atom,-atom,-atom,--stream), [iso(false)]).

client(Url, Host, Path, S) :- client(Url, Host, Path, S, []).

:- help(client(+atom,-atom,-atom,--stream), [iso(false)]).

server(Host, S) :- server(Host, S, []).

:- help(server(+atom,--stream), [iso(false)]).

load_files(Files) :- load_files(Files,[]).

:- help(load_files(+list), [iso(false)]).

consult(Files) :- load_files(Files,[]).

:- help(consult(+list), [iso(false)]).

reconsult(Files) :- load_files(Files,[]).

:- help(reconsult(+list), [iso(false)]).

deconsult(Files) :- unload_files(Files).

:- help(deconsult(+list), [iso(false)]).

?=(X, Y) :- \+ unifiable(X, Y, [_|_]).

:- help('?='(+term,+term), [iso(false)]).

atom_number(A, N) :- atom_codes(A,Codes), number_codes(N, Codes).

:- help(atom_number(+atom,-number), [iso(false)]).

rational_numerator_denominator(R, N, D) :-
	N is numerator(R),
	D is denominator(R).

:- help(rational_numerator_denominator(+rational,-integr,-integer), [iso(false)]).

'$skip_list'(Skip, Xs0, Xs) :- '$skip_max_list'(Skip,_, Xs0, Xs).

:- help('$skip_list'(+p1,?p2,?p3,-p4), [iso(false)]).

term_hash(Term, _Opts, Hash) :- term_hash(Term, Hash).

:- help(term_hash(+term,+list,-integer), [iso(false)]).

read_term_from_chars_(T, Cs, Rest) :-
	'$read_term_from_chars'(T, [], Cs, Rest).

read_term_from_chars_(T, Opts, Cs, Rest) :-
	'$read_term_from_chars'(T, Opts, Cs, Rest).

:- help(read_term_from_chars_(?term,+chars,-chars), [iso(false)]).
:- help(read_term_from_chars_(?term,+list,+chars,-chars), [iso(false)]).

read_from_atom(A, T) :- read_term_from_atom(A, T, []).
read_from_chars(Cs, T) :- read_term_from_chars(Cs, T, []).

:- help(read_term_from_atom(+atom,?term), [iso(false)]).
:- help(read_term_from_chars(+chars,?term), [iso(false)]).

with_output_to(chars(Cs), Goal) :-
	setup_call_cleanup(
		'$capture_output',
		once(Goal),
		'$capture_output_to_chars'(Cs)
	), !.

with_output_to(string(Cs), Goal) :-
	setup_call_cleanup(
		'$capture_output',
		once(Goal),
		'$capture_output_to_chars'(Cs)
	), !.

with_output_to(atom(Cs), Goal) :-
	setup_call_cleanup(
		'$capture_output',
		once(Goal),
		'$capture_output_to_atom'(Cs)
	), !.

sl_create(S) :- sl_create(S,[]).

:- help(sl_create(--stream), [iso(false)]).

iso_dif(X, Y) :-
	X \== Y,
	(	X \= Y ->
		true
	;	throw(error(instantiation_error,iso_dif/2))
	).

numbervars(Term, N0, N) :-
   '$must_be'(N0, integer, numbervars/3, _),
   '$can_be'(N, integer, numbervars/3, _),
   term_variables(Term, Vars),
   numberlist_(Vars, N0, N).

:- help(numbervars(+term,+integer,?integer), [iso(false)]).

numberlist_([], N, N).
numberlist_(['$VAR'(N0)|Vars], N0, N) :-
   N1 is N0+1,
   numberlist_(Vars, N1, N).

:- help(numberlist(+list,?integer,?integer), [iso(false)]).

read_line_to_codes(Stream, Codes) :-
	read_line_to_string(Stream, String),
	string_codes(String, Codes).

:- help(read_line_to_codes(+stream,?list), [iso(false)]).

sleep(0) :- !.
sleep(Secs) :-
	'$must_be'(Secs, integer, sleep/1, _),
	(Secs < 0 ->
		throw(error(domain_error(not_less_than_zero, Secs), sleep/1))
	;	true
	),
	delay(1000),
	Secs2 is Secs - 1,
	sleep(Secs2).

:- help(sleep(+integer), [iso(false)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

% This is preliminary:

:- dynamic('$pl_thread_alias'/3).

pl_thread_option_([], _, _, _, _, _) :- !.
pl_thread_option_([X|_], _, _, _, _, _) :- var(X), !, instantiation_error(thread_option).
pl_thread_option_([at_exit(AtExit)|Rest], _, _, _, _, AtExit) :-
	!,
	pl_thread_option_(Rest, _, _, _, _, _).
pl_thread_option_([alias(Alias)|Rest], Alias, _, _, _, _) :-
	!,
	pl_thread_option_(Rest, _, _, _, _, _).
pl_thread_option_([detached(Detached)|Rest], _, _, _, Detached, _) :-
	!,
	pl_thread_option_(Rest, _, _, _, _, _).
pl_thread_option_([cpu(Cpu)|Rest], _, Cpu, _, _, _) :-
	!,
	pl_thread_option_(Rest, _, _, _, _, _).
pl_thread_option_([priority(Priority)|Rest], _, _, Priority, _, _) :-
	!,
	pl_thread_option_(Rest, _, _, _, _, _).
pl_thread_option_([Name|_], _, _, _, _, _) :-
	throw(error(domain_error(thread_option, Name), pl_thread/2)).

pl_thread(Id, Filename) :-
	'$pl_thread'(Id, Filename).

pl_thread(Id, Filename, Options) :-
	pl_thread_option_(Options, Alias, _Cpu, _Priority, _Detached, _AtExit),
	'$pl_thread'(Id, Filename),
	(atom(Alias) -> retractall('$pl_thread_alias'(_, Alias, _)) ; true),
	(atom(Alias) -> assertz('$pl_thread_alias'(Id, Alias, thread)) ; true),
	%(integer(Cpu) -> '$pl_thread_pin_cpu'(Id, Cpu) ; true),
	%(integer(Priority) -> '$pl_thread_set_priority'(Id, Priority) ; true),
	true.

:- meta_predicate(thread_create(0,-)).
:- meta_predicate(thread_create(0,-,?)).
:- meta_predicate(thread_signal(+,0)).

thread_create(Goal, Id) :-
	%format("*** thread_create(~w,~w)~n", [Goal,Id]),
	'$must_be'(Goal, callable, thread_create/3, _),
	'$thread_create'(Goal, Id, false).

thread_create(Goal, Id, Options) :-
	%format("*** thread_create(~w,~w,~w)~n", [Goal,Id,Options]),
	'$must_be'(Goal, callable, thread_create/3, _),
	'$must_be'(Options, list, thread_create/3, _),
	pl_thread_option_(Options, Alias, _Cpu, _Priority, Detached, AtExit),
	(atom(Alias) ->
		('$pl_thread_alias'(_, Alias, _) ->
			throw(error(permission_error(create,thread,alias(Alias))))
			; true
		),
		Goal1 = Goal,
		(	(nonvar(Detached), Detached = true) ->
			Goal2 = (Goal1, retractall('$pl_thread_alias'(_, Alias, _)))
		;	Goal2 = Goal1
		),
		Goal0 = (Goal2, halt)
	;
		Goal0 = (Goal, halt)
	),
	(atom(Alias) -> retractall('$pl_thread_alias'(_, Alias, _)) ; true),
	( nonvar(AtExit) ->
		'$thread_create'(Goal0, Id0, Detached, (AtExit, halt))
	;	'$thread_create'(Goal0, Id0, Detached, _)
	),
	( atom(Alias) ->
		(assertz('$pl_thread_alias'(Id0, Alias, thread)), Id = Alias)
	;	(assertz('$pl_thread_alias'(Id0, -, thread)), Id = Id0)
	),
	(integer(Cpu) -> '$pl_thread_pin_cpu'(Id, Cpu) ; true),
	(integer(Priority) -> '$pl_thread_set_priority'(Id, Priority) ; true),
	true.

thread_signal(Alias, Goal) :-
	%format("*** thread_signal(~w,~w)~n", [Alias,Goal]),
	'$pl_thread_alias'(Id, Alias, thread),
	!,
	Goal0 = (Goal, true),
	'$thread_signal'(Id, Goal0).
thread_signal(Id, Goal) :-
	(integer(Id) -> true ; throw(error(domain_error(thread_or_alias, Id), thread_signal/2))),
	Goal0 = (Goal, true),
	'$thread_signal'(Id, Goal0).

thread_join(Alias, Status) :-
	%format("*** thread_join(~w,~w)~n", [Alias,Status]),
	'$pl_thread_alias'(Id, Alias, thread),
	!,
	'$thread_join'(Id, Status),
	retractall('$pl_thread_alias'(_, Alias, thread)).
thread_join(Id, Status) :-
	(integer(Id) -> true ; throw(error(domain_error(thread_or_alias, Id), thread_join/2))),
	'$thread_join'(Id, Status),
	retractall('$pl_thread_alias'(_, Id, thread)).

thread_cancel(Alias) :-
	%format("*** thread_cancel(~w)~n", [Alias]),
	'$pl_thread_alias'(Id, Alias, thread),
	!,
	'$thread_cancel'(Id).
thread_cancel(Id) :-
	(integer(Id) -> true ; throw(error(domain_error(thread_or_alias, Id), thread_cancel/2))),
	'$thread_cancel'(Id).

thread_detach(Alias) :-
	'$pl_thread_alias'(Id, Alias, thread),
	!,
	'$thread_detach'(Id).
thread_detach(Id) :-
	(integer(Id) -> true ; throw(error(domain_error(thread_or_alias, Id), thread_detach/2))),
	'$thread_detach'(Id).

pl_msg_send(Alias, Term) :-
	'$pl_thread_alias'(Id, Alias, _),
	!,
	'$pl_msg_send'(Id, Term).
pl_msg_send(Id, Term) :-
	'$pl_msg_send'(Id, Term).

thread_send_message(Term) :-
	thread_self(Id),
	'$thread_send_message'(Id, Term).

thread_send_message(Alias, Term) :-
	%format("*** thread_send_message(~w,~w)~n", [Alias,Term]),
	'$pl_thread_alias'(Id, Alias, _),
	!,
	'$thread_send_message'(Id, Term).
thread_send_message(Id, Term) :-
	(integer(Id) -> true ; throw(error(domain_error(queue_or_alias, Id), thread_message_send/2))),
	'$thread_send_message'(Id, Term).

thread_get_message(Term) :-
	thread_self(Id),
	'$thread_get_message'(Id, Term).

thread_get_message(Alias, Term) :-
	'$pl_thread_alias'(Id, Alias, _),
	!,
	'$thread_get_message'(Id, Term).
thread_get_message(Id, Term) :-
	(integer(Id) -> true ; throw(error(domain_error(queue_or_alias, Id), thread_message_get/2))),
	'$thread_get_message'(Id, Term).

thread_peek_message(Term) :-
	thread_self(Id),
	'$thread_peek_message'(Id, Term).

thread_peek_message(Alias, Term) :-
	'$pl_thread_alias'(Id, Alias, _),
	!,
	'$thread_peek_message'(Id, Term).
thread_peek_message(Id, Term) :-
	(integer(Id) -> true ; throw(error(domain_error(queue_or_alias, Id), thread_message_peek/2))),
	'$thread_peek_message'(Id, Term).

thread_property(P) :-
	thread_self(Id),
	thread_property(Id, P).

thread_property(Alias, P) :-
	%format("*** thread_property(~w,~w)~n", [Alias,P]),
	'$pl_thread_alias'(_, Alias, thread),
	Alias \= '-',
	P = alias(Alias).
thread_property(Id, P) :-
	'$pl_thread_alias'(Id, Alias, thread),
	Alias \= '-',
	P = alias(Alias).
thread_property(Alias, P) :-
	'$pl_thread_alias'(Id, Alias, thread),
	( catch('$thread_is_detached'(Id), _, fail) ->
		P = detached(true)
	;	P = detached(false)
	).
thread_property(Id, P) :-
	'$pl_thread_alias'(Id, _, thread),
	( catch('$thread_is_detached'(Id), _, fail) ->
		P = detached(true)
	;	P = detached(false)
	).
thread_property(Alias, P) :-
	'$pl_thread_alias'(_, Alias, thread),
	P = status(running).
thread_property(Id, P) :-
	'$pl_thread_alias'(Id, _, thread),
	P = status(running).

xthread_statistics(K, V) :-
	thread_self(Id),
	thread_statistics(Id, K, V).

xthread_statistics(Alias, K, V) :-
	format("*** thread_statistics(~w, ~w,~w)~n", [Alias,K,V]),
	'$pl_thread_alias'(_, Alias, thread),
	fail.
xthread_statistics(Id, _K, _V) :-
	'$pl_thread_alias'(Id, _, thread),
	fail.

message_queue_create(Id) :-
	'$message_queue_create'(Id).

message_queue_create(Id, Options) :-
	%format("*** message_queue_create(~w,~w)~n", [Id,Options]),
	'$must_be'(Options, list, message_queue_create/3, _),
	pl_thread_option_(Options, Alias, _Cpu, _Priority, _Detached, _AtExit),
	'$message_queue_create'(Id0),
	(atom(Alias) ->
		('$pl_thread_alias'(_, Alias, _) ->
			throw(error(permission_error(create,thread,alias(Alias))))
		; Id = Alias
		)
	; Id = Id0
	),
	( atom(Alias) ->
		assertz('$pl_thread_alias'(Id0, Alias, queue))
	;	assertz('$pl_thread_alias'(Id0, -, queue))
	),
	true.

message_queue_destroy(Alias) :-
	%format("*** message_queue_destroy(~w)~n", [Alias]),
	'$pl_thread_alias'(Id, Alias, queue),
	!,
	'$message_queue_destroy'(Id),
	retractall('$pl_thread_alias'(_, Alias, queue)).
message_queue_destroy(Id) :-
	(integer(Id) -> true ; throw(error(domain_error(queue_or_alias, Id), message_queue_destroy/1))),
	'$message_queue_destroy'(Id).

message_queue_property(Alias, P) :-
	%format("*** message_queue_property(~w,~w)~n", [Alias,P]),
	'$pl_thread_alias'(_, Alias, queue),
	Alias \= '-',
	P = alias(Alias).
message_queue_property(Id, P) :-
	'$pl_thread_alias'(Id, Alias, queue),
	Alias \= '-',
	P = alias(Alias).
message_queue_property(Alias, P) :-
	'$pl_thread_alias'(Id, Alias, queue),
	( catch('$message_queue_size'(Id, Size), _, fail) ->
		P = size(Size)
	;	P = size(0)
	).
message_queue_property(Id, P) :-
	'$pl_thread_alias'(Id, _, queue),
	( catch('$message_queue_size'(Id, Size), _, fail) ->
		P = size(Size)
	;	P = size(0)
	).

mutex_create(Id) :-
	%format("*** mutex_create(~w)~n", [Id]),
	'$mutex_create'(Id).

mutex_create(Id, Options) :-
	%format("*** mutex_create(~w,~w)~n", [Id,Options]),
	'$must_be'(Options, list, mutex_create/3, _),
	pl_thread_option_(Options, Alias, _Cpu, _Priority, _Detached, _AtExit),
	'$mutex_create'(Id0),
	(atom(Alias) ->
		('$pl_thread_alias'(_, Alias, _) ->
			throw(error(permission_error(create,thread,alias(Alias))))
		; Id = Alias
		)
	; Id = Id0
	),
	( atom(Alias) ->
		assertz('$pl_thread_alias'(Id0, Alias, mutex))
	;	assertz('$pl_thread_alias'(Id0, -, mutex))
	),
	true.

mutex_destroy(Alias) :-
	%format("*** mutex_destroy(~w)~n", [Alias]),
	'$pl_thread_alias'(Id, Alias, mutex),
	!,
	'$mutex_destroy'(Id),
	retractall('$pl_thread_alias'(_, Alias, mutex)).
mutex_destroy(Id) :-
	(integer(Id) -> true ; throw(error(domain_error(mutex_or_alias, Id), mutex_destroy/1))),
	'$mutex_destroy'(Id).

mutex_trylock(Alias) :-
	%format("*** mutex_trylock(~w)~n", [Alias]),
	'$pl_thread_alias'(Id, Alias, _),
	!,
	'$mutex_trylock'(Id).
mutex_trylock(Id) :-
	(integer(Id) -> true ; throw(error(domain_error(mutex_or_alias, Id), mutex_trylock/1))),
	'$mutex_trylock'(Id).

mutex_lock(Alias) :-
	%format("*** mutex_lock(~w)~n", [Alias]),
	'$pl_thread_alias'(Id, Alias, _),
	!,
	'$mutex_lock'(Id).
mutex_lock(Id) :-
	(integer(Id) -> true ; throw(error(domain_error(mutex_or_alias, Id), mutex_lock/1))),
	'$mutex_lock'(Id).

mutex_unlock(Alias) :-
	%format("*** mutex_unlock(~w)~n", [Alias]),
	'$pl_thread_alias'(Id, Alias, _),
	!,
	'$mutex_unlock'(Id).
mutex_unlock(Id) :-
	(integer(Id) -> true ; throw(error(domain_error(mutex_or_alias, Id), mutex_unlock/1))),
	'$mutex_unlock'(Id).

% Note: with_mutex/2 ignores the mutex if called without any
% thread context. This is not well documented.

:- meta_predicate(with_mutex(+,0)).

with_mutex(Alias, Goal) :-
	%format("*** with_mutex(~w,~w)~n", [Alias,Goal]),
	'$pl_thread_alias'(Id, Alias, _),
	!,
	setup_call_cleanup(mutex_lock(Id), once(Goal), mutex_unlock(Id)).
with_mutex(Id, Goal) :-
	( '$any_threads' ->
		setup_call_cleanup(mutex_lock(Id), once(Goal), mutex_unlock(Id))
	;	ignore(Goal)
	).

mutex_property(Alias, P) :-
	%format("*** mutex_property(~w,~w)~n", [Alias,P]),
	'$pl_thread_alias'(_, Alias, mutex),
	Alias \= '-',
	P = alias(Alias).
mutex_property(Id, P) :-
	'$pl_thread_alias'(Id, Alias, mutex),
	Alias \= '-',
	P = alias(Alias).
mutex_property(Alias, P) :-
	'$pl_thread_alias'(Id, Alias, mutex),
	( catch('$mutex_is_locked'(Id), _, fail) ->
		P = status(unlocked)
	;	P = status(locked)
	).
mutex_property(Id, P) :-
	'$pl_thread_alias'(Id, _, mutex),
	( catch('$mutex_is_locked'(Id), _, fail) ->
		P = status(unlocked)
	;	P = status(locked)
	).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

error(Err, Context) :-
	'$must_be'(Err, nonvar, error/2, _),
	throw(error(Err,Context)).

resource_error(Resource, Context) :-
   throw(error(resource_error(Resource), Context)).

:- help(resource_error(+term,+term), [iso(false)]).

instantiation_error(Context) :-
	throw(error(instantiation_error, Context)).

:- help(instantiation_error(+term), [iso(false)]).

domain_error(Type, Term, Context) :-
	throw(error(domain_error(Type, Term), Context)).

:- help(domain_error(+atom,+term,+term), [iso(false)]).

type_error(Type, Term, Context) :-
	throw(error(type_error(Type, Term), Context)).

:- help(type_error(+atom,+term,+term), [iso(false)]).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% NOTE: this doesn't display var names properly...

pretty(PI) :-
	use_module(library(format)),
	nonvar(PI),
	(   PI = Name/Arity0 ->
		Arity = Arity0
	;   PI = Name//Arity0 ->
		Arity is Arity0 + 2
	;   type_error(predicate_indicator, PI, listing/1)
	),
	functor(Head, Name, Arity),
	\+ \+ clause(Head, _), % only true if there is at least one clause
	(   clause(Head, Body),
		(   Body == true ->
			portray_clause(Head)
		;   portray_clause((Head :- Body))
		),
		false
	;   true
	).

:- help(pretty(+predicateindicator), [iso(false)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

current_op(A, B, C) :- var(A), var(B), var(C),
	!,
	( '$ops_dirty' -> (retractall('$op'(_,_,_)),'$load_ops') ; true ),
	'$op'(C, B, A).
current_op(_, _, C) :- nonvar(C), \+ atom(C),
	!, throw(error(type_error(atom,C), current_op/3)).
current_op(_, B, _) :- nonvar(B), \+ atom(B),
	!, throw(error(domain_error(operator_specifier, B), current_op/3)).
current_op(_, B, _) :- nonvar(B),
	\+ memberchk(B,[xf, yf, fx, fy, xfx, xfy, yfx]),
	!, throw(error(domain_error(operator_specifier, B), current_op/3)).
current_op(A, _, _) :- nonvar(A),
	\+ integer(A),
	!, throw(error(domain_error(operator_priority, A), current_op/3)).
current_op(A, _, _) :- nonvar(A),
	\+ (A >= 0),
	!, throw(error(domain_error(operator_priority, A), current_op/3)).
current_op(A, _, _) :- nonvar(A),
	\+ (A =< 1200),
	!, throw(error(domain_error(operator_priority, A), current_op/3)).
current_op(A, B, C) :-
	( '$ops_dirty' -> (retractall('$op'(_,_,_)),'$load_ops') ; true ),
	'$op'(C, B, A).

:- help(current_op(?integer,?atom,?atom), [iso(true)]).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Debugging...

print_goals_([]).
print_goals_([Goal|Goals]) :-
	write(Goal),
	(Goals == [] -> true ;	write(', ')),
	print_goals_(Goals).

dump_attvars_([], []).
dump_attvars_([Var|Vars], [Gs|Rest]) :-
	copy_term(Var, Var2, Gs),
	Var = Var2,
	dump_attvars_(Vars, Rest).

dump_attvars :-
	'$list_attributed'(Vs0),
	sort(Vs0, Vs),
	dump_attvars_(Vs, Gs0),
	flatten(Gs0, Gs1),
	sort(Gs1, Gs),
	print_goals_(Gs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

sre_match_all_in_file(Pat, Filename, L) :-
	setup_call_cleanup(
		open(Filename, read, S, [mmap(Cs)]),
		sre_match_all(Pat, Cs, L),
		close(S)
	).

:- help(sre_match_all_in_file(+pattern,+filename,-list), [iso(false)]).

sre_match_all(Pat, Text, L) :-
	sre_compile(Pat, Reg),
	sre_match_all_(Reg, Text, [], L2),
	reverse(L2, L).

sre_match_all_(_, [], L, L) :- !.
sre_match_all_(Reg, TextIn, L0, L) :-
	sre_matchp(Reg, TextIn, Match, TextOut),
	(	TextOut \= [] ->
		sre_match_all_(Reg, TextOut, [Match|L0], L)
	;	L = L0
	).

:- help(sre_match_all(+pattern,+text,-list), [iso(false)]).

sre_match_all_pos_in_file(Pat, Filename, L) :-
	setup_call_cleanup(
		open(Filename, read, S, [mmap(Cs)]),
		sre_match_all_pos(Pat, Cs, L),
		close(S)
	).

:- help(sre_match_all_pos_in_file(+pattern,+filename,-list), [iso(false)]).

sre_match_all_pos(Pat, Text, L) :-
	sre_compile(Pat, Reg),
	sre_match_all_pos_(Reg, Text, 0, [], L2),
	reverse(L2, L).

sre_match_all_pos_(_, [], _, L, L) :- !.
sre_match_all_pos_(Reg, TextIn, Offset, L0, L) :-
	sre_matchp(Reg, TextIn, Match, TextOut),
	string_length(TextIn, N0),
	string_length(Match, N1),
	string_length(TextOut, N2),
	Pos is N0 - (N1 + N2) + Offset,
	Pos2 is Pos + 1,
	( TextOut \= [] ->
		sre_match_all_pos_(Reg, TextOut, Pos2, [Pos-N1|L0], L)
	;	L = L0
	).

:- help(sre_match_all_pos(+pattern,+subst,-list), [iso(false)]).

sre_subst_all_in_file(Pat, Filename, Subst, L) :-
	setup_call_cleanup(
		open(Filename, read, S, [mmap(Cs)]),
		sre_subst_all(Pat, Cs, Subst, L),
		close(S)
	).

:- help(sre_subst_all_in_file(+pattern,+filename,+subst,-list), [iso(false)]).

sre_subst_all(Pat, Text, Subst, L) :-
	sre_compile(Pat, Reg),
	sre_subst_all_(Reg, Text, Subst, [], L0),
	reverse(L0, L1),
	append(L1, L).

sre_subst_all_(_, [], _, L, L) :- !.
sre_subst_all_(Reg, TextIn, Subst, L0, L) :-
	sre_substp(Reg, TextIn, Prefix, TextOut),
	(	TextOut \= [] ->
		sre_subst_all_(Reg, TextOut, Subst, [Subst,Prefix|L0], L)
	;	L = [Prefix|L0]
	).

:- help(sre_subst_all(+pattern,+text,+subst,-text), [iso(false)]).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
