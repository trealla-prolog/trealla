% Tabling: the (:- table)/1 directive and its driver. Data structures
% (variant/answer tries, worklists, scheduling) are native - see the
% '$tbl_*' builtins in src/tabling.c; suspension uses reset/shift.
%
% The "tabling" prolog flag (default true) gates it at call time:
% when false, tabled predicates run as PLAIN calls - no memoization,
% no termination guarantees (left recursion loops again), which makes
% A/B comparison of a program with and without tabling a one-liner.
%
% Helpers precede the user:term_expansion clauses at the bottom, which
% become active the moment they load.

:- module(tabling, [start_tabling/2, abolish_all_tables/0,
	op(1150, fx, table)]).

:- use_module(library(dcgs)).
:- use_module(library(lists)).

:- dynamic('$tabled'/1).

abolish_all_tables :-
	'$tbl_abolish_all_tables'.

% --- driver ---

start_tabling(Wrapper, Worker) :-
	(  current_prolog_flag(tabling, false) ->
	   call(Worker)
	;  native_start_tabling(Wrapper, Worker)
	).

%
% NB. every shift/1 sits in final-call position of its own clause: the
% continuation capture walks pending goals frame by frame, and keeping
% shifts out of compiled control constructs keeps the captured
% continuations exact.

native_start_tabling(Wrapper, Worker) :-
	'$tbl_variant_table'(Wrapper, T, S),
	start_tabling_(S, T, Wrapper, Worker).

start_tabling_(complete, T, Wrapper, _Worker) :-
	'$tbl_get_answer'(T, Wrapper).
start_tabling_(active, T, Wrapper, _Worker) :-
	shift(call_info(Wrapper, T)).
start_tabling_(fresh, T, Wrapper, Worker) :-
	(  '$tbl_leader' ->
	   run_follower_fresh(T, Wrapper, Worker)
	;  run_leader(T, Wrapper, Worker)
	).

run_follower_fresh(T, Wrapper, Worker) :-
	activate(T, Wrapper, Worker),
	shift(call_info(Wrapper, T)).

run_leader(T, Wrapper, Worker) :-
	'$tbl_set_leader',
	catch(( activate(T, Wrapper, Worker),
	        completion
	      ), Ball,
	      ( '$tbl_clear_leader', throw(Ball) )),
	'$tbl_clear_leader',
	'$tbl_get_answer'(T, Wrapper).

activate(T, Wrapper, Worker) :-
	'$tbl_set_status'(T, active),
	(  delim(T, Wrapper, Worker),
	   fail
	;  true
	).

% One producer step. A completed worker records an answer
% ('$tbl_add_answer' FAILS on duplicates, driving the loop); a shifted
% worker records a dependency on the table it suspended on. dep/4 packs
% SourceCall, continuation and target wrapper in ONE term so they keep
% sharing variables through the image copy.
%
% Exceptions raised by a worker fail that producer branch, as in the
% reference (Desouter) library. Eg. a worker calling setof/3 with its
% output argument bound to a non-list: Trealla's ISO-strict setof
% throws where lenient systems fail, and tabling must not surface a
% difference.

delim(T, Wrapper, Worker) :-
	catch(reset(Worker, Ball, Cont), _, fail),
	(  Cont == none ->
	   '$tbl_add_answer'(T, Wrapper)
	;  Cont = cont(C),
	   Ball = call_info(_, SrcT),
	   '$tbl_add_suspension'(SrcT, dep(Ball, C, Wrapper, T))
	).

% Run to fixpoint: drain tables until no work remains, then complete
% every table created under this leader.

completion :-
	(  '$tbl_pop_worklist'(T) ->
	   (  '$tbl_wkl_work'(T, Answer, dep(call_info(Answer, _), C, W, TT)),
	      delim(TT, W, C),
	      fail
	   ;  true
	   ),
	   completion
	;  '$tbl_mark_all_complete'
	).

% --- (:- table f/N) directive + clause renaming ---

rename_head(Head, NewHead) :-
	functor(Head, Name, Arity),
	functor(Test, Name, Arity),
	'$tabled'(Test),
	atom_concat(Name, ' tabled', WrapName),
	Head =.. [_|Args],
	NewHead =.. [WrapName|Args].

wrappers(Var) --> { var(Var), !, throw(error(instantiation_error, (table)/1)) }.
wrappers((A,B)) --> !, wrappers(A), wrappers(B).
% A DCG non-terminal Name//Arity tables the underlying Name/(Arity+2).
% The clauses themselves reach the rename hook already DCG-translated,
% so only the arity mapping is needed here.
wrappers(Name//Arity) -->
	{ atom(Name), integer(Arity), Arity >= 0, !,
	  Arity2 is Arity + 2 },
	wrappers(Name/Arity2).
wrappers(Name/Arity) -->
	{ atom(Name), integer(Arity), Arity >= 0, !,
	  functor(Head, Name, Arity),
	  atom_concat(Name, ' tabled', WrapName),
	  Head =.. [Name|Args],
	  WrappedHead =.. [WrapName|Args] },
	% The wrapper rule must precede the '$tabled' marker: each emitted
	% element is itself term_expanded, and once the marker is asserted
	% the rename rule would rewrite the wrapper's own head.
	[ (Head :- tabling:start_tabling(Head, WrappedHead)),
	  tabling:'$tabled'(Head) ].

% Active from here on - keep these clauses LAST.

user:term_expansion((:- table Preds), Clauses) :-
	nonvar(Preds),
	phrase(wrappers(Preds), Clauses).
user:term_expansion((Head :- Body), (NewHead :- Body)) :-
	nonvar(Head),
	rename_head(Head, NewHead).
user:term_expansion(Head, NewHead) :-
	nonvar(Head),
	Head \= (:- _), Head \= (_ :- _),
	rename_head(Head, NewHead).
