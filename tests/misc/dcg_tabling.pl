% Tabled DCG rules: DCG translation and library(tabling) interacting.
%
% Nothing else in the suite combines `:- table` with `-->`, and the
% combination rests on an ordering that is easy to break by accident:
%
%   library/tabling.pl renames the heads of tabled predicates through
%   user:term_expansion/2, which runs AFTER DCG translation. So the
%   rename sees `expr(S0,S) :- ...` and matches its (Head :- Body)
%   clause. If translation were ever moved to run after user
%   term-expansion - which is what the term_expansion FIXME in parser.c
%   proposes - the rename would instead see `(expr --> ...)`, whose
%   functor is (-->)/2, which its guards reject. Tabled DCG rules would
%   then silently stop being tabled, and the only symptom would be a
%   left-recursive grammar looping instead of terminating.
%
% So this test pins two things:
%
%   1. that the rename ran on the TRANSLATED clause - checked
%      structurally, by the renamed worker existing at arity 2 rather
%      than at the arity of a (-->)/2 term;
%   2. that tabling is actually doing its job - checked behaviourally,
%      by a left-recursive grammar terminating.
%
% (2) is the part that cannot be faked: without tabling this grammar
% dies with resource_error(memory). That control is deliberately NOT run
% here, since it would consume the memory it is meant to demonstrate;
% it was verified separately, and identically, on this branch and on
% main.

:- initialization(main).
:- use_module(library(dcgs)).
:- use_module(library(tabling)).
:- use_module(library(lists)).

% Left-recursive: expr//0 calls itself on the same input. This is the
% natural way to write a left-associative operator and is not writable
% in plain Prolog without restructuring the grammar.

:- table expr//0.

expr --> expr, [+], term.
expr --> term.

term --> [n].

% An ordinary right-recursive tabled non-terminal, for the plain case.

:- table as//0.

as --> [].
as --> [a], as.

report(Name, true) :- !, format("dcg tabling: ~w ok~n", [Name]).
report(Name, _) :- format("DCG-TABLING-FAIL ~w~n", [Name]).

check(Name, Goal) :-
	(  catch(Goal, E, (format("DCG-TABLING-ERROR ~w: ~q~n", [Name, E]), fail))
	-> report(Name, true)
	;  report(Name, false)
	).

% The rename produces `<name> tabled`/<arity>. Arity 2 is the point: it
% is the DCG-translated head that got renamed, not the (-->)/2 term.

renamed_worker_exists :-
	current_predicate('expr tabled'/2).

main :-
	check(left_recursion_parses,   phrase(expr, [n,+,n,+,n])),
	check(left_recursion_rejects,  \+ phrase(expr, [n,+])),
	check(plain_tabled_dcg,        phrase(as, [a,a,a])),
	check(plain_tabled_rejects,    \+ phrase(as, [a,b])),
	check(rename_ran_on_translated_clause, renamed_worker_exists).
