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

:- module(tabling, [start_tabling/2,
	abolish_all_tables/0, abolish_table/1,
	op(1150, fx, table)]).

:- use_module(library(dcgs)).
:- use_module(library(lists)).

:- dynamic('$tabled'/1).

abolish_all_tables :-
	'$tbl_abolish_all_tables'.

% abolish_table(+Spec) drops every table of ONE predicate, where Spec
% takes the same shapes as the (:- table) directive: Name/Arity,
% Name//Arity for a DCG non-terminal, or a comma-conjunction of those.
%
% Needed because a completed table does NOT notice assert/retract on
% the predicates it derived from - the answers stay as they were. Until
% incremental tabling exists, invalidating by hand after changing the
% facts is the supported route, and abolish_all_tables/0 is too blunt
% for that: it throws away every unrelated table too.

abolish_table(Spec) :-
	(  var(Spec) ->
	   throw(error(instantiation_error, abolish_table/1))
	;  abolish_table_(Spec)
	).

abolish_table_((A,B)) :- !,
	abolish_table_(A),
	abolish_table_(B).
abolish_table_(Name//Arity) :-
	atom(Name), integer(Arity), Arity >= 0, !,
	Arity2 is Arity + 2,
	abolish_table_(Name/Arity2).
abolish_table_(Name/Arity) :-
	atom(Name), integer(Arity), Arity >= 0, !,
	functor(Test, Name, Arity),
	(  '$tabled'(Test) ->
	   '$tbl_abolish'(Name, Arity)
	;  % Silently doing nothing here hides a typo, and the caller
	   % believes stale answers were dropped when they were not.
	   throw(error(existence_error(table, Name/Arity), abolish_table/1))
	).
abolish_table_(Spec) :-
	throw(error(type_error(predicate_indicator, Spec), abolish_table/1)).

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
% A fresh variant is COMPLETED in its own (possibly nested) SCC rather
% than suspending the consumer. Suspension needs the consumer's
% continuation to be capturable, which it is not when the tabled call
% sits inside findall/3, setof/3 or any other collector holding state in
% C. Completing the subgoal side-steps that entirely.
%
% If the SCC turns out to depend on an outer one (a genuine cycle across
% the nesting) its tables are merged into the parent by '$tbl_pop_scc'
% and the consumer suspends after all, deferring to the outer leader.
%
% On an exception escaping, tables created under this SCC are left
% half-built and marked active; without a rollback a later call would
% suspend on a table nobody is going to complete (and silently fail).

start_tabling_(fresh, T, Wrapper, Worker) :-
	run_scc(T, Wrapper, Worker).

run_scc(T, Wrapper, Worker) :-
	'$tbl_push_scc'(T),
	catch(( activate(T, Wrapper, Worker),
	        completion
	      ), Ball,
	      ( '$tbl_reset_incomplete', '$tbl_pop_scc'(_), throw(Ball) )),
	'$tbl_pop_scc'(Escaped),
	run_scc_(Escaped, T, Wrapper).

run_scc_(true, T, Wrapper) :-
	shift(call_info(Wrapper, T)).
run_scc_(false, T, Wrapper) :-
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
	catch(reset(Worker, Ball, Cont), _, ('$tbl_note_exception', fail)),
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
	;  % A fixpoint in which some worker threw may have collected only
	   % part of the answers; caching that as complete would make the
	   % error permanent. Roll those tables back so a later call
	   % recomputes instead.
	   (  '$tbl_saw_exception' ->
	      '$tbl_reset_incomplete'
	   ;  '$tbl_scc_escaped' ->
	      % This SCC depends on an outer one, so this fixpoint saw only
	      % part of the answers. Leave the tables active for
	      % '$tbl_pop_scc' to merge into the parent, which completes
	      % them once it has seen the rest. Marking them complete here
	      % caches a partial table permanently.
	      true
	   ;  '$tbl_mark_all_complete'
	   )
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
% Re-tabling an already-tabled predicate is a no-op rather than a
% second wrapper (harmless but wasteful, and it would re-mark).
wrappers(Name/Arity) -->
	{ atom(Name), integer(Arity), Arity >= 0,
	  functor(Test, Name, Arity),
	  '$tabled'(Test), ! },
	[].
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
% Module-qualified clause heads of tabled predicates (eg. user:q(a)).
% The qualifier is STRIPPED from the renamed clause: M:Head clauses get
% their bodies wrapped in a module-qualify barrier by the loader, and a
% tabled call suspending under that barrier truncates the captured
% continuation (same unsupported class as suspending in an if-then-else
% condition). Stripping is exact when M is the loading module - the
% overwhelmingly common case - and cross-module definition of tabled
% predicates is unsupported regardless (table identity is not
% module-keyed).
user:term_expansion((M:Head :- Body), (NewHead :- Body)) :-
	nonvar(M), nonvar(Head),
	rename_head(Head, NewHead).
user:term_expansion(M:Head, NewHead) :-
	nonvar(M), nonvar(Head),
	Head \= (_ :- _),
	rename_head(Head, NewHead).
user:term_expansion(Head, NewHead) :-
	nonvar(Head),
	Head \= (:- _), Head \= (_ :- _),
	rename_head(Head, NewHead).
