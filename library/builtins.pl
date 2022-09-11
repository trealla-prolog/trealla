:- pragma(builtins, [once]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

predicate_property(P, A) :-
	nonvar(P), atom(A), !,
	must_be(P, callable, predicate_property/2, _),
	'$legacy_predicate_property'(P, A).
predicate_property(P, A) :-
	'$load_properties',
	(	var(A)
	->	true
	; 	(	(Controls = [built_in,control_construct,discontiguous,private,static,dynamic,tabled,persist,multifile,meta_predicate(_)],
			memberchk(A, Controls))
		->	true
		;	throw(error(domain_error(predicate_property, A), P))
		)
	),
	must_be(P, callable, predicate_property/2, _),
	(	P = (M:P2)
	->	M:'$predicate_property'(P2, A)
	;	'$predicate_property'(P, A)
	).

current_prolog_flag(P, A) :-
	nonvar(P), !,
	'$legacy_current_prolog_flag'(P, A).
current_prolog_flag(P, A) :-
	'$load_flags',
	'$current_prolog_flag'(P, A).

subsumes_term(G, S) :-
	\+ \+ (
	 term_variables(S, V1),
	 G = S,
	 term_variables(V1, V2),
	 V2 == V1
	).

forall(Cond, Action) :-
	\+ (Cond, \+ Action).

:- meta_predicate(forall(0,0)).

catch(G, E, C) :-
	'$catch'(call(G), E, call(C)).

:- meta_predicate(catch(0,?,0)).

call_cleanup(G, C) :-
	'$register_cleanup'(ignore(C)),
	'$call_cleanup'(
		call(G),
		Err,
		(catch((\+ \+ C), _, true), throw(Err))
	).

:- meta_predicate(call_cleanup(0,0)).

setup_call_cleanup(S, G, C) :-
	once(S),
	'$register_cleanup'(ignore(C)),
	'$call_cleanup'(
		call(G),
		Err,
		(catch((\+ \+ C), _, true), throw(Err))
	).

:- meta_predicate(setup_call_cleanup(0,0,0)).

findall(T, G, B, Tail) :-
	can_be(B, list, findall/4, _),
	can_be(Tail, list, findall/4, _),
	findall(T, G, B0),
	append(B0, Tail, B), !.

:- meta_predicate(findall(?,0,-,?)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

keysort_(List, Sorted) :-
	keysort_(List, List, Sorted, []).

keysort_([Term| _], _, _, _) :-
	var(Term),
	throw(error(instantiation_error, keysort/2)).
keysort_([Key-X| Xs], List, Ys, YsTail) :-
	!,
	'$key_partition'(Xs, Key, List, Left, EQ, EQT, Right),
	keysort_(Left, List,  Ys, [Key-X|EQ]),
	keysort_(Right, List, EQT, YsTail),
	!.
keysort_([], _, Ys, Ys) :-
	!.
keysort_([Term| _], _, _, _) :-
	throw(error(type_error(pair,Term), keysort/2)).
keysort_(Term, List, _, _) :-
	Term \== [],
	throw(error(type_error(list,List), keysort/2)).
keysort_(_, _, Sorted, _) :-
	Sorted \= [_|_],
	throw(error(type_error(list,Sorted), keysort/2)).
keysort_(_, _, [Term| _], _) :-
	Term \= _-_,
	throw(error(type_error(pair,Term), keysort/2)).
keysort_(_, _, Sorted, _) :-
	throw(error(type_error(list,Sorted), keysort/2)).

'$key_partition'([Term| _], _, _, _, _, _, _) :-
	var(Term),
	throw(error(instantiation_error, keysort/2)).
'$key_partition'([XKey-X| Xs], YKey, List, [XKey-X| Ls], EQ, EQT, Rs) :-
	XKey @< YKey,
	!,
	'$key_partition'(Xs, YKey, List, Ls, EQ, EQT, Rs).
'$key_partition'([XKey-X| Xs], YKey, List, Ls, [XKey-X| EQ], EQT, Rs) :-
	XKey == YKey,
	!,
	'$key_partition'(Xs, YKey, List, Ls, EQ, EQT, Rs).
'$key_partition'([XKey-X| Xs], YKey, List, Ls, EQ, EQT, [XKey-X| Rs]) :-
%	XKey @> YKey,
	!,
	'$key_partition'(Xs, YKey, List, Ls, EQ, EQT, Rs).
'$key_partition'([], _, _, [], EQT, EQT, []) :-
	!.
'$key_partition'([Term| _], _, _, _, _, _, _) :-
	throw(error(type_error(pair,Term), keysort/2)).
'$key_partition'(_, _, List, _, _, _, _) :-
	throw(error(type_error(list,List), keysort/2)).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Derived from code by R.A. O'Keefe

setof(Template, Generator, Set) :-
    ( 	var(Set)
    ->	true
    ; 	must_be(Set, list_or_partial_list, setof/3, _)
    ),
	bagof_(Template, Generator, Bag),
	is_list_or_partial_list(Set),
	sort(Bag, Set).

:- meta_predicate(setof(-,0,?)).

bagof(Template, Generator, Bag) :-
    (	var(Bag)
    ->	true
	;	must_be(Bag, list_or_partial_list, bagof/3, _)
	),
	bagof_(Template, Generator, Bag).

:- meta_predicate(bagof(-,0,?)).

bagof_(Template, Generator, Bag) :-
	acyclic_term(Generator),
	free_variables_(Generator, Template, [], Vars, 1),
	Vars \== [],
	!,
	Key =.. [(.)|Vars],
	functor(Key, (.), N),
	findall(Key-Template, Generator, Recorded),
	replace_instance_(Recorded, Key, N, _, OmniumGatherum),
	keysort_(OmniumGatherum, Gamut), !,
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
	arg(N, OldKey, OldVar),
	arg(N, NewKey, OldVar),
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
	(	cyclic_term(Arg) ->	N1 is N-1,
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
%   first clause is there to save a choice point when this is
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
	(	cyclic_term(Argument) -> M is N-1, !,
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

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

current_key(K) :- var(K), '$record_key'(K,_).
recorda(K, V) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V)).
recordz(K, V) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V)).
recorded(K, V) :- nonvar(K), '$record_key'(K,V).
recorda(K, V, R) :- nonvar(K), nonvar(V), asserta('$record_key'(K,V), R).
recordz(K, V, R) :- nonvar(K), nonvar(V), assertz('$record_key'(K,V), R).
recorded(K, V, R) :- nonvar(K), clause('$record_key'(K,V), _, R).

call_with_time_limit(Time, Goal) :-
	Time0 is truncate(Time * 1000),
	'$alarm'(Time0),
	(	catch(once(Goal), E, ('$alarm'(0), throw(E)))
	->	'$alarm'(0)
	;	('$alarm'(0), fail)
	).

time_out(Goal, Time, Result) :-
	'$alarm'(Time),
	(	catch(once(Goal), E, ('$alarm'(0), throw(E)))
	->	('$alarm'(0), Result = success)
	;	('$alarm'(0), fail)
	).

:- meta_predicate(call_with_time_limit(+,0)).
:- meta_predicate(time_out(0,+,-)).

writeln(T) :- write(T), nl.
format(F) :- format(F, []).
open(F, M, S) :- open(F, M, S, []).
samsort(L, R) :- msort(L, R).
atomic_list_concat(L, Atom) :- atomic_list_concat(L, '', Atom).
partial_string(S, P) :- append(S, _, P).
partial_string(S, P, V) :- append(S, V, P).
chars_base64(Plain, Base64, Opts) :- base64(Plain, Base64, Opts).
chars_urlenc(Plain, Url, Opts) :- urlenc(Plain, Url, Opts).
term_to_atom(T, S) :- write_term_to_chars(S, T, []).
absolute_file_name(R, A) :- absolute_file_name(R, A, []).
client(U, H, P, S) :- client(U,H,P,S,[]).
server(H, S) :- server(H,S,[]).
load_files(Files) :- load_files(Files,[]).
consult(Files) :- load_files(Files,[]).
reconsult(Files) :- load_files(Files,[]).
deconsult(Files) :- unload_files(Files).
strip_module(T,M,P) :- T=M:P -> true ; P=T.
read_term_from_chars(T,S) :- read_term_from_chars(T,[],S).
?=(X,Y) :- \+ unifiable(X,Y,[_|_]).
atom_number(A,N) :- atom_codes(A,Codes), number_codes(N,Codes).
'$skip_list'(Skip,Xs0,Xs) :- '$skip_max_list'(Skip,_,Xs0,Xs).
term_hash(Term, _Opts, Hash) :- term_hash(Term, Hash).
not(G) :- G, !, fail.
not(_).

map_create(S) :- map_create(S,[]).

iso_dif(X, Y) :-
	X \== Y,
	(	X \= Y
	->	true
	;	throw(error(instantiation_error,iso_dif/2))
	).

:- meta_predicate(not(0)).

numbervars(Term, N0, N) :-
   must_be(N0, integer, numbervars/3, _),
   can_be(N, integer, numbervars/3, _),
   term_variables(Term, Vars),
   numberlist_(Vars, N0, N).

numberlist_([], N, N).
numberlist_(['$VAR'(N0)|Vars], N0, N) :-
   N1 is N0+1,
   numberlist_(Vars, N1, N).

read_line_to_codes(Stream, Codes) :-
	read_line_to_string(Stream, String),
	string_codes(String, Codes).

instantiation_error(Context) :-
    throw(error(instantiation_error, Context)).

domain_error(Type, Term, Context) :-
    throw(error(domain_error(Type, Term), Context)).

type_error(Type, Term, Context) :-
    throw(error(type_error(Type, Term), Context)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI compatible
%
% Global variables. using the namespace 'user' to make sure they
% are truly global and not just in the current module. This a quick
% hack using assert/retract...

nb_setval(K, _) :-
	must_be(K, atom, nb_setval/2, _),
	user:retract('$global_key'(K, _)),
	fail.
nb_setval(K, V) :-
	must_be(K, atom, nb_setval/2, _),
	user:assertz('$global_key'(K, V)).

nb_getval(K, V) :-
	must_be(K, atom, nb_getval/2, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), nb_getval/2))),
	!.

nb_delete(K) :-
	must_be(K, atom, nb_delete/1, _),
	user:retract('$global_key'(K, _)),
	!.
nb_delete(_).

nb_current(K, V) :-
	user:clause('$global_key'(K, V), nb_current/2).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI compatible
%
% Global variables. using the namespace 'user' to make sure they
% are truly global and not just in the current module. This a quick
% hack using assert/retract...
% The following is not really correct.

b_setval(K, _) :-
	must_be(K, atom, b_setval/2, _),
	\+ user:clause('$global_key'(K, _), _),
	user:asserta('$global_key'(K, [])),
	fail.
b_setval(K, V) :-
	must_be(K, atom, b_setval/2, _),
	user:asserta('$global_key'(K, V)).
b_setval(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

b_setval0(K, _) :-
	must_be(K, atom, b_setval0/2, _),
	\+ user:clause('$global_key'(K, _), _), asserta('$global_key'(K, 0)),
	fail.
b_setval0(K, V) :-
	must_be(K, atom, b_setval0/2, _),
	user:asserta('$global_key'(K, V)).
b_setval0(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

b_getval(K, V) :-
	must_be(K, atom, b_getval/2, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), b_getval/2))),
	!.

b_delete(K) :-
	must_be(K, atom, b_delete/1, _),
	user:retractall('$global_key'(K, _)),
	!.
b_delete(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SICStus compatible

bb_b_put(K, _) :-
	must_be(K, atom, bb_b_put/2, _),
	\+ user:clause('$global_key'(K, _), _),
	user:asserta('$global_key'(K, [])),
	fail.
bb_b_put(K, V) :-
	must_be(K, atom, bb_b_put/2, _),
	user:asserta('$global_key'(K, V)).
bb_b_put(K, _) :-
	user:retract('$global_key'(K, _)),
	!, fail.

bb_b_del(K) :-
	must_be(K, atom, bb_b_del/1, _),
	user:retract('$global_key'(K, _)),
	!.
bb_b_del(_).

bb_put(K, _) :-
	must_be(K, atom, bb_put/2, _),
	user:retract('$global_key'(K, _)),
	fail.
bb_put(K, V) :-
	must_be(K, atom, bb_put/2, _),
	user:assertz('$global_key'(K, V)).

bb_get(K, V) :-
	must_be(K, atom, bb_get/2, _),
	user:catch('$global_key'(K, V), _, throw(error(existence_error(variable, K), bb_get/2))),
	!.

bb_delete(K, V) :-
	must_be(K, atom, bb_delete/2, _),
	user:retract('$global_key'(K, V)),
	!.

bb_update(K, O, V) :-
	must_be(K, atom, bb_update/3, _),
	user:retract('$global_key'(K, O)),
	user:assertz('$global_key'(K, V)),
	!.

bb_del(K) :-
	must_be(K, atom, bb_del/1, _),
	user:retractall('$global_key'(K, _)),
	!.
bb_del(_).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

current_op(A, B, C) :- var(A), var(B), var(C),
	!,
	'$load_ops',
	'$current_op'(C, B, A).
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
current_op(A, B, C) :- nonvar(A), nonvar(B), nonvar(C),
	!,
	'$load_ops',
	'$current_op'(C, B, A),
	!.
current_op(A, B, C) :-
	'$load_ops',
	'$current_op'(C, B, A).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI compatible

put_attr(Var, Module, Value) :-
	Attr =.. [Module,Value],
	put_atts(Var, Attr).

get_attr(Var, Module, Value) :-
	var(Var),
	Attr =.. [Module,Value],
	get_atts(Var, Attr).

del_attr(Var, Module) :-
	var(Var), !,
	Attr =.. [Module,_],
	put_atts(Var, -Attr).
del_attr(_, _).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SICStus compatible

:- use_module(library(dict)).

put_atts(_, []) :- !.
put_atts(Var, [H|T]) :- !,
	put_atts(Var, H),
	put_atts(Var, T).

put_atts(Var, -Attr) :- !,
	var(Var),
	'$get_attributes'(Var, D),
	functor(Attr, Functor, Arity),
	attribute(Module, Functor, Arity),
	dict:del(D, Module, D2),
	(	D2 = []
	->	'$erase_attributes'(Var)
	;	'$put_attributes'(Var, D2)
	).

put_atts(Var, +Attr) :- !,
	var(Var),
	'$get_attributes'(Var, D),
	functor(Attr, Functor, Arity),
	attribute(Module, Functor, Arity),
	dict:set(D, Module, Attr, D2),
	'$put_attributes'(Var, D2).

put_atts(Var, Attr) :- !,
	var(Var),
	'$get_attributes'(Var, D),
	functor(Attr, Functor, Arity),
	attribute(Module, Functor, Arity),
	dict:set(D, Module, Attr, D2),
	'$put_attributes'(Var, D2).

get_atts(Var, L) :- var(L), !,
	var(Var),
	'$get_attributes'(Var, D),
	dict:match(D, _, L).

get_atts(Var, -Attr) :- !,
	var(Var),
	'$get_attributes'(Var, D),
	functor(Attr, Functor, Arity),
	attribute(Module, Functor, Arity),
	\+ dict:get(D, Module, _).

get_atts(Var, +Attr) :- !,
	var(Var),
	'$get_attributes'(Var, D),
	functor(Attr, Functor, Arity),
	attribute(Module, Functor, Arity),
	dict:get(D, Module, Attr).

get_atts(Var, Attr) :- !,
	var(Var),
	'$get_attributes'(Var, D),
	functor(Attr, Functor, Arity),
	attribute(Module, Functor, Arity),
	dict:get(D, Module, Attr).

del_atts(Var) :-
	var(Var),
	'$erase_attribute'(Var).

attvar(Var) :-
	var(Var),
	'$get_attributes'(Var, D),
	D \= [].

term_attvars_([], VsIn, VsIn) :- !.
term_attvars_([H|T], VsIn, VsOut) :-
	(	attvar(H)
	->	term_attvars_(T, [H|VsIn], VsOut)
	;	term_attvars_(T, VsIn, VsOut)
	).

term_attvars(Term, Vs) :-
	term_variables(Term, Vs0),
	term_attvars_(Vs0, [], Vs).

collect_goals_(_, [], GsIn, GsIn) :- !.
collect_goals_(V, [H|T], GsIn, GsOut) :-
	H =.. [M, _],
	catch(M:attribute_goals(V, Goal0, []), _, Goal0 = put_atts(V, +H)),
	(Goal0 = [H2]-> Goal = H2 ; Goal = Goal0),
	collect_goals_(V, T, [Goal|GsIn], GsOut).

collect_goals_([], GsIn, GsIn) :- !.
collect_goals_([V|T], GsIn, GsOut) :-
	get_atts(V, Ls),
	collect_goals_(V, Ls, GsIn, GsOut2),
	collect_goals_(T, GsOut2, GsOut).

copy_term(Term, Copy, Gs) :-
	copy_term_nat(Term, Copy),
	term_attvars(Term, Vs),
	collect_goals_(Vs, [], Gs).

% Debugging...

print_goals([]) :- !.
print_goals([Goal|Goals]) :-
	write_term(Goal, [varnames(true)]),
	(Goals == [] -> write('') ; write(',')),
	print_goals(Goals).

dump_attvars([], []) :- !.
dump_attvars([Var|Vars], [V|Rest]) :-
	copy_term(Var, _, Gs),
	list_to_conjunction(Gs, V),
	dump_attvars(Vars, Rest).

dump_attvars :-
	'$list_attributed'(Vars),
	dump_attvars(Vars, Gs0),
	sort(Gs0, Gs),
	print_goals(Gs).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plus(X,Y,S) :- nonvar(X), nonvar(Y),
	must_be(X, integer, plus/3, _), must_be(Y, integer, plus/3, _), !,
	S is X + Y.
plus(X,Y,S) :- nonvar(X), var(Y), nonvar(S),
	must_be(X, integer, plus/3, _), must_be(S, integer, plus/3, _), !,
	Y is S - X.
plus(X,Y,S) :- var(X), nonvar(Y), nonvar(S),
	must_be(S, integer, plus/3, _), must_be(Y, integer, plus/3, _), !,
	X is S - Y.
plus(_,_,_) :-
	throw(error(instantiation_error, plus/3)).

succ(X,S) :- nonvar(X), Y=1, nonvar(Y),
	must_be(X, integer, succ/2, _), must_be(Y, integer, succ/2, _), !,
	(	X >= 0
	->	true
	; 	throw(error(domain_error(not_less_than_zero, X), succ/2))
	),
	S is X + Y.
succ(X,S) :- var(X), Y=1, nonvar(Y), nonvar(S),
	must_be(S, integer, succ/2, _), must_be(Y, integer, succ/2, _), !,
	(	S >= 0
	->	true
	; 	throw(error(domain_error(not_less_than_zero, S), succ/2))
	),
	!,
	S > 0,
	X is S - Y.
succ(_,_) :-
	throw(error(instantiation_error, succ/2)).
