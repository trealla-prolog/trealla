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
	must_be(P, callable, predicate_property/2, _),
	'$legacy_predicate_property'(P, A).
predicate_property(P, A) :-
	'$load_properties',
	(	var(A) ->
		true
	; 	(Controls = [
			built_in,choice_construct,
			discontiguous,private,static,
			dynamic,foreign,multifile,
			meta_predicate(_),imported_from(_),template(_),
			iso,visible,tabled
			],
			( memberchk(A, Controls) ->
				true
			;	throw(error(domain_error(predicate_property, A), P))
			)
		)
	),
	must_be(P, callable, predicate_property/2, _),
	(	P = (M:P2) ->
		M:'$predicate_property'(predicate, P2, A)
	;	'$predicate_property'(predicate, P, A)
	).

:- help(predicate_property(+callable,+term), [iso(true)]).

evaluable_property(P, A) :-
	nonvar(P), atom(A), !,
	must_be(P, callable, evaluable_property/2, _),
	'$legacy_evaluable_property'(P, A).
evaluable_property(P, A) :-
	'$load_properties',
	(	var(A) ->
		true
	; 	(Controls = [iso,built_in,static,dynamic,template(_),template(_,_)],
			(memberchk(A, Controls) ->
				true
			;	(
				must_be(A, callable, evaluable_property/2, _),
				throw(error(domain_error(evaluable_property, A), P))
				)
			)
		)
	),
	must_be(P, callable, evaluable_property/2, _),
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

cfor(I0,J0,K) :-
	I is I0,
	J is J0,
	between(I, J, K).

:- help(cfor(+evaluable,+evaluable,-var), [iso(false),desc('C-style for loop')]).

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
	can_be(B, list, findall/4, _),
	can_be(Tail, list, findall/4, _),
	findall(T, G, B0),
	append(B0, Tail, B), !.

:- help(findall(+term,:callable,-list,+list), [iso(false)]).

flatten(List, FlatList) :-
	flatten_(List, [], FlatList0),
	!,
	FlatList = FlatList0.

flatten_(Var, Tl, [Var|Tl]) :-
	var(Var),
	!.
flatten_([], Tl, Tl) :- !.
flatten_([Hd|Tl], Tail, List) :-
	!,
	flatten_(Hd, FlatHeadTail, List),
	flatten_(Tl, Tail, FlatHeadTail).
flatten_(NonList, Tl, [NonList|Tl]).

:- help(flatten(+list,-list), [iso(false)]).

'$post_unify_hook' :-
	'$undo_trail'(Vars, State),
	process_vars_(Vars, [], Goals),
	'$redo_trail'(State),
	maplist(call, Goals).

process_vars_([], Goals, Goals).
process_vars_([Var-Val|Vars], SoFar, Goals) :-
	(	get_atts(Var, Atts) ->
		process_var_(Atts, Var, Val, SoFar, MoreGoals),
		process_vars_(Vars, MoreGoals, Goals)
	;	process_vars_(Vars, SoFar, Goals)
	).

process_var_([], _, _, Goals, Goals).
process_var_([Att|Atts], Var, Val, SoFar, Goals) :-
	functor(Att, F, A),
	attribute(M, F, A),
	M:verify_attributes(Var, Val, NewGoals),
	append(SoFar, NewGoals, MoreGoals),
	process_var_(Atts, Var, Val, MoreGoals, Goals).

term_attvars_([], VsIn, VsIn).
term_attvars_([H|T], VsIn, VsOut) :-
	(	'$attributed_var'(H) ->
		term_attvars_(T, [H|VsIn], VsOut)
	;	term_attvars_(T, VsIn, VsOut)
	).

term_attributed_variables(Term, Vs) :-
	can_be(Vs, list, term_attributed_variables/2, _),
	term_variables(Term, Vs0),
	term_attvars_(Vs0, [], Vs).

:- help(term_attributed_variables(+term,-list), [iso(false), desc('Return list of attributed variables in term')]).

collect_goals_(_, [], GsIn, GsIn).
collect_goals_(V, [H|T], GsIn, GsOut) :-
	nonvar(H),
	H =.. [M, _],
	catch(M:attribute_goals(V, Goal0, []), _, Goal0 = put_atts(V, +H)),
	!,
	(Goal0 = [H2] -> Goal = H2 ; Goal = Goal0),
	collect_goals_(V, T, [Goal|GsIn], GsOut).
collect_goals_(V, [_|T], GsIn, GsOut) :-
	collect_goals_(V, T, GsIn, GsOut).

collect_goals_([], GsIn, GsIn).
collect_goals_([V|T], GsIn, GsOut) :-
	get_atts(V, Ls),
	collect_goals_(V, Ls, GsIn, GsOut2),
	collect_goals_(T, GsOut2, GsOut).

copy_term(Term, Copy, Gs) :-
	duplicate_term(Term, Copy),
	term_attributed_variables(Copy, Vs),
	collect_goals_(Vs, [], Gs).

:- help(copy_term(+term,?term,+list), [iso(false)]).

term_variables(P1, P2, P3) :-
	term_variables(P1, P4),
	append(P4, P3, P2).

:- help(term_variables(+term,-list,?tail), [iso(false)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

:- meta_predicate(setof(-,0,?)).
:- help(setof(+term,+callable,?list), [iso(true)]).

:- meta_predicate(bagof(-,0,?)).
:- help(bagof(+term,:callable,?list), [iso(true)]).

/************************************************************/
/* bagof/3 and setof/3                                      */
/************************************************************/

/**
 * bagof(T, X1^…^Xn^G, L): [ISO 8.10.2]
 * The predicate determines all the solutions to the goal G,
 * whereby collecting copies of the template T and the
 * witness. The predicate then repeatedly succeeds for
 * the witness and the list of associated templates.
 */
% bagof(+Term, +Goal, -List)
bagof(T, G, L) :-
	(var(L) -> true; must_be(L, list_or_partial_list, bagof/3, _)),
	acyclic_term(G),
	sys_globals_kernel(T^G, W, H),
	findall(W-T, H, J),
	sys_same_vars(J, _),
	keysort(J, K),
	sys_enum_runs(K, W, L).

/**
 * setof(T, X1^…^Xn^G, L): [ISO 8.10.3]
 * The predicate determines all the solutions to the goal G,
 * whereby collecting copies of the template T and the
 * witness. The predicate then repeatedly succeeds for
 * the witness and the set of associated templates.
 */
% setof(+Term, +Goal, -List)
setof(T, G, L) :-
	(var(L) -> true; must_be(L, list_or_partial_list, setof/3, _)),
	acyclic_term(G),
	sys_globals_kernel(T^G, W, H),
	findall(W-T, H, J),
	sys_same_vars(J, _),
	sort(J, K),
	sys_enum_runs(K, W, L).

% sys_same_vars(+Pairs, +List)
sys_same_vars([K-_|L], V) :-
	term_variables(K, V, _),
	sys_same_vars(L, V).
sys_same_vars([], _).

% sys_enum_runs(+Pairs, +Term, -List)
sys_enum_runs([K-V|L], W, Q) :-
	sys_key_run(L, K, R, H),
	(K = W, Q = [V|R], (H = [], !; true); sys_enum_runs(H, W, Q)).

% sys_key_run(+Pairs, +Term, -List, -Pairs)
sys_key_run([K-V|L], J, [V|R], H) :- K == J, !,
	sys_key_run(L, J, R, H).
sys_key_run(L, _, [], L).

/********************************************************************/
/* Helpers                                                          */
/********************************************************************/

% sys_goal_split(+Goal, -List, -Goal)
sys_globals_kernel(G, W, H) :-
	sys_goal_split(G, I, H),
	term_variables(H, A),
	term_variables(I, B),
	sys_var_subtract(A, B, W).

% sys_goal_split(+Goal, -List, -Goal)
sys_goal_split(G, [], G) :- var(G), !.
sys_goal_split(V^G, [V|L], H) :- !,
	sys_goal_split(G, L, H).
sys_goal_split(G, [], G).

% sys_var_subtract(+List, +List, -List)
sys_var_subtract([X|L], R, T) :-
	member(Y, R), Y == X, !,
	sys_var_subtract(L, R, T).
sys_var_subtract([X|L], R, [X|S]) :-
	sys_var_subtract(L, R, S).
sys_var_subtract([], _, []).

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

recorda(K, V) :- nonvar(K), nonvar(V), asserta('$record_global_key'(K,V)).
recorda(K, V, R) :- nonvar(K), nonvar(V), asserta('$record_global_key'(K,V), R).
recordz(K, V) :- nonvar(K), nonvar(V), assertz('$record_global_key'(K,V)).
recordz(K, V, R) :- nonvar(K), nonvar(V), assertz('$record_global_key'(K,V), R).
recorded(K, V) :- nonvar(K), '$record_global_key'(K,V).
recorded(K, V, R) :- nonvar(K), clause('$record_global_key'(K,V), _, R).
current_key(K) :- var(K), '$record_global_key'(K,_).

:- help(recorda(+term,+term), [iso(false)]).
:- help(recorda(+term,+term,-ref), [iso(false)]).
:- help(recordz(+term,+term), [iso(false)]).
:- help(recordz(+term,+term,-ref), [iso(false)]).
:- help(recorded(+term,?term), [iso(false)]).
:- help(recorded(+term,?term,-ref), [iso(false)]).
:- help(current_key(-term), [iso(false)]).

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

'$portray_term'(S, T) :-
	compound(T), !,
	functor(T, _, Args),
	T =.. [Functor|_],
	writeq(S, Functor), write(S, '('),
	(between(1, Args, I),
		arg(I, T, Arg),
		'$portray'(S, Arg),
		(I < Args -> write(S, ',') ; true),
		fail  ; true),
	write(S, ')').
'$portray_term'(S, T) :-
	writeq(S, T).

'$portray'(S, T) :-
	current_output(S0),
	set_output(S),
	((catch(once(portray(T)), _, write(T)), !) ; '$portray_term'(S, T)),
	set_output(S0).

'$portray'(T) :-
	current_output(S),
	((catch(once(portray(T)), _, write(T)), !) ; '$portray_term'(S, T)).

print(T) :- current_output(S), format(S, "~p", [T]).
print(S, T) :- format(S, "~p", [T]).

:- help(print(+term), [iso(false)]).
:- help(print(+stream,+term), [iso(false)]).

writeln(T) :- write(T), nl.

:- help(writeln(+term), [iso(false)]).

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

:- help(rational_numerator_denominator(+rational,-integer,-integer), [iso(false)]).

'$skip_list'(Skip, Xs0, Xs) :- '$skip_max_list'(Skip,_, Xs0, Xs).

:- help('$skip_list'(+p1,?p2,?p3,-p4), [iso(false)]).

term_hash(Term, _Opts, Hash) :- term_hash(Term, Hash).

:- help(term_hash(+term,+list,-integer), [iso(false)]).

read_term_from_chars_(T, Cs, Rest) :-
	'$read_term_from_chars'(T, [], Cs, Rest).

read_term_from_chars_(T, Opts, Cs, Rest) :-
	'$read_term_from_chars'(T, Opts, Cs, Rest).

read_from_atom(A, T) :- read_term_from_atom(A, T, []).
read_from_chars(Cs, T) :- read_term_from_chars(Cs, T, []).

:- help(read_from_atom(+atom,?term), [iso(false)]).
:- help(read_from_chars(+chars,?term), [iso(false)]).

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

iso_dif(X, Y) :-
	X \== Y,
	(	X \= Y ->
		true
	;	throw(error(instantiation_error,iso_dif/2))
	).

numbervars(Term, N0, N) :-
   must_be(N0, integer, numbervars/3, _),
   can_be(N, integer, numbervars/3, _),
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

repeat_integer(N) :-
	N > 0.
repeat_integer(N0) :-
	N0 > 0,
	N1 is N0 - 1,
	repeat_integer(N1).

repeat(N) :-
	must_be(N, integer, repeat/1, _),
	repeat_integer(N).

:- help(repeat(+integer), [iso(false)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

thread_property(P) :-
	thread_self(Id),
	thread_property(Id, P).

:- meta_predicate(with_mutex(-,0)).

with_mutex(Id, Goal) :-
	setup_call_cleanup(mutex_lock(Id), once(Goal), mutex_unlock(Id)).

:- meta_predicate(thread_create(0,-)).

thread_create(Goal, Id) :-
	thread_create(Goal, Id, []).

thread_send_message(Term) :-
	thread_self(Id),
	thread_send_message(Id, Term).

thread_get_message(Term) :-
	thread_self(Id),
	thread_get_message(Id, Term).

thread_peek_message(Term) :-
	thread_self(Id),
	thread_peek_message(Id, Term).

xthread_statistics(K, V) :-
	thread_self(Id),
	thread_statistics(Id, K, V).

message_queue_create(Id) :-
	message_queue_create(Id, []).

mutex_create(Id) :-
	mutex_create(Id, []).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

error(Err, Context) :-
	must_be(Err, nonvar, error/2, _),
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

print_goals_(_, []).
print_goals_(Any, [Goal|Goals]) :-
	(Any -> write(', ') ; true),
	write(Goal),
	(Goals == [] -> true ;	write(', ')),
	print_goals_(false, Goals).

dump_attvars_([], []).
dump_attvars_([Var|Vars], [Gs|Rest]) :-
	copy_term(Var, Var2, Gs),
	Var = Var2,
	dump_attvars_(Vars, Rest).

dump_attvars(Any) :-
	'$list_attributed'(Vs0),
	sort(Vs0, Vs),
	dump_attvars_(Vs, Gs0),
	flatten(Gs0, Gs1),
	sort(Gs1, Gs),
	print_goals_(Any, Gs).

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
