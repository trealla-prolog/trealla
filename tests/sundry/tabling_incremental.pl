% Incremental tabling (DESIGN-tabling-phase2.md item 3): tables survive
% assert/retract on the dynamic predicates they consulted, instead of
% needing a hand-written abolish_table/1.
%
% Both halves opt in - ":- incremental(q/1)" on the dynamic predicate
% and ":- table p/1 as incremental" on the table. A table only collects
% dependencies if it is incremental, and only on predicates that are,
% so nothing changes for a program that declares neither (tests 3 and
% 4 are the negative controls for exactly that).
%
% Attribution is per-SCC, not per-table: the SCC is already the unit of
% completion and its push/pop bracket is the only one in the driver
% that is safe against backtracking. Invalidation is validate-on-READ,
% done by the owning thread at lookup - tables are per-thread but the
% database is shared, so invalidating from the asserting thread would
% mean writing to another thread's tables.

:- use_module(library(tabling)).
:- use_module(library(lists)).

:- initialization(main).

% ---------------------------------------------------------------------
% 1. The basic contract: assert and retract are both picked up, with no
% abolish_table/1 call anywhere.

:- dynamic(edge/2).
:- incremental(edge/2).
:- table path/2 as incremental.

edge(a,b).
edge(b,c).

path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z), path(Z,Y).

test_assert_retract :-
	findall(Y, path(a,Y), B), msort(B, Bs),
	assertz(edge(c,d)),
	findall(Y, path(a,Y), A), msort(A, As),
	retract(edge(b,c)),
	findall(Y, path(a,Y), R), msort(R, Rs),
	(	Bs == [b,c], As == [b,c,d], Rs == [b] ->
		write('assert/retract: ok')
	;	write('assert/retract: FAILED'), nl, write(Bs-As-Rs)
	),
	nl.

% ---------------------------------------------------------------------
% 2. Transitive invalidation through a table->table edge. outer/1 never
% mentions base/1 itself - it only calls inner/1 - so this only works if
% the edge recorded at '$tbl_variant_table' is followed.

:- dynamic(base/1).
:- incremental(base/1).
:- table inner/1 as incremental.
:- table outer/1 as incremental.

base(1).
inner(X) :- base(X).
outer(X) :- inner(X), X > 0.

test_transitive :-
	findall(X, outer(X), B),
	assertz(base(2)),
	findall(X, outer(X), A), msort(A, As),
	(	B == [1], As == [1,2] ->
		write('transitive: ok')
	;	write('transitive: FAILED'), nl, write(B-As)
	),
	nl.

% ---------------------------------------------------------------------
% 3. Negative control: a table that is NOT declared incremental must
% keep its answers over the same assert, exactly as before this item.

:- table frozen/1.

frozen(X) :- base(X).

test_non_incremental_table_frozen :-
	findall(X, frozen(X), B),
	assertz(base(3)),
	findall(X, frozen(X), A),
	(	B == A ->
		write('non-incremental table frozen: ok')
	;	write('non-incremental table frozen: FAILED'), nl, write(B-A)
	),
	nl.

% ---------------------------------------------------------------------
% 4. Negative control: a dynamic predicate NOT declared incremental
% must not invalidate anything, even for an incremental table.

:- dynamic(untracked/1).
:- table ignores/1 as incremental.

untracked(9).
ignores(X) :- untracked(X).

test_untracked_pred_ignored :-
	findall(X, ignores(X), B),
	assertz(untracked(8)),
	findall(X, ignores(X), A),
	(	B == [9], A == [9] ->
		write('untracked pred ignored: ok')
	;	write('untracked pred ignored: FAILED'), nl, write(B-A)
	),
	nl.

% ---------------------------------------------------------------------
% 5. Invalidation must fully DROP the table, not '$tbl_reset_incomplete'
% it (which deliberately keeps answers). With answer subsumption landed
% this is sharp: leaf->value in the answer trie points at live tbl_ans
% structs, so freeing answers without clearing the trie would leave
% dangling pointers in the dedup path. A min-aggregated table that is
% invalidated and recomputed exercises exactly that.

:- dynamic(cost/2).
:- incremental(cost/2).
:- table best(_,min) as incremental.

cost(a,5).
cost(a,9).

best(X,C) :- cost(X,C).

test_invalidate_subsumptive :-
	findall(C, best(a,C), B),
	assertz(cost(a,2)),
	findall(C, best(a,C), A),
	retract(cost(a,2)),
	findall(C, best(a,C), R),
	(	B == [5], A == [2], R == [5] ->
		write('invalidate subsumptive: ok')
	;	write('invalidate subsumptive: FAILED'), nl, write(B-A-R)
	),
	nl.

% ---------------------------------------------------------------------
% 6. Re-validation is keyed on the database generation, so a table that
% is looked up repeatedly with NO intervening change must stay put -
% otherwise incremental tabling silently becomes "recompute every call"
% and the memoization is gone. Checked by counting derivations.

:- dynamic(hits/1).
:- dynamic(fact/1).
:- incremental(fact/1).
:- table counted/1 as incremental.

fact(1).

counted(X) :- fact(X), assertz(hits(X)).

test_no_spurious_recompute :-
	retractall(hits(_)),
	findall(X, counted(X), _),
	findall(X, counted(X), _),
	findall(X, counted(X), _),
	findall(H, hits(H), Hs),
	length(Hs, N),
	(	N == 1 ->
		write('no spurious recompute: ok')
	;	write('no spurious recompute: FAILED'), nl, write(n=N)
	),
	nl.

% ---------------------------------------------------------------------

main :-
	test_assert_retract,
	test_transitive,
	test_non_incremental_table_frozen,
	test_untracked_pred_ignored,
	test_invalidate_subsumptive,
	test_no_spurious_recompute.
