% Differential test for the CONSULT path.
%
% dcg_differential.pl and dcg_corpus.pl both drive '$dcg_rule'/2 - the
% RUNTIME path. Phase 1 moved consult-time translation into C as a
% separate path: same xlate_rule(), but different variable creation
% (named, registered by assign_vars) and different cell copying (plain
% dup_cells rather than by-ref). All 829 corpus rules can agree while
% consult is visibly broken, and during phase 1 that is exactly what
% happened - every regression surfaced through unrelated tests instead.
%
% This closes that. The rules below are consulted for real when this file
% loads, so they go through parser.c's hook, assign_vars, process_clause
% and term_to_body. Each is then read back with clause/2 and compared
% against the reference translation of the same rule.
%
% Each rule needs `:- dynamic` so clause/2 can see it, and a matching
% src/2 fact carrying the source term. The duplication is deliberate: the
% rule has to be a real clause for the consult path to translate it, and
% a term for the reference to translate.

:- initialization(main).
:- use_module(library(dcgs)).
:- ensure_loaded('tests/dcg_reference').
:- use_module(library(lists)).

:- dynamic(c01/2).
c01 --> [].
src(c01, (c01 --> [])).

:- dynamic(c02/2).
c02 --> b.
src(c02, (c02 --> b)).

:- dynamic(c03/2).
c03 --> b, c.
src(c03, (c03 --> b, c)).

:- dynamic(c04/2).
c04 --> b, c, d.
src(c04, (c04 --> b, c, d)).

:- dynamic(c05/2).
c05 --> [x,y,z].
src(c05, (c05 --> [x,y,z])).

:- dynamic(c06/2).
c06 --> "abc".
src(c06, (c06 --> "abc")).

:- dynamic(c07/2).
c07 --> b ; c.
src(c07, (c07 --> b ; c)).

:- dynamic(c08/2).
c08 --> b | c.
src(c08, (c08 --> b | c)).

:- dynamic(c09/2).
c09 --> {g}.
src(c09, (c09 --> {g})).

:- dynamic(c10/2).
c10 --> !.
src(c10, (c10 --> !)).

:- dynamic(c11/2).
c11 --> b -> c ; d.
src(c11, (c11 --> b -> c ; d)).

:- dynamic(c12/2).
c12 --> call(x).
src(c12, (c12 --> call(x))).

:- dynamic(c13/2).
c13 --> phrase(x).
src(c13, (c13 --> phrase(x))).

:- dynamic(c14/3).
c14(X) --> b(X), [X].
src(c14, (c14(X) --> b(X), [X])).

:- dynamic(c15/2).
c15, [p] --> b.
src(c15, (c15, [p] --> b)).

:- dynamic(c16/2).
c16 --> m:b.
src(c16, (c16 --> m:b)).

:- dynamic(c17/2).
c17 --> _X.
src(c17, (c17 --> _X)).

:- dynamic(c18/2).
c18 --> [], b, [].
src(c18, (c18 --> [], b, [])).

:- dynamic(c19/2).
c19 --> (b,c), (d;e), {f}.
src(c19, (c19 --> (b,c), (d;e), {f})).

:- dynamic(c20/2).
c20 --> "ab", c, "de".
src(c20, (c20 --> "ab", c, "de")).

:- dynamic(c21/4).
c21(X,Y) --> b(X), c(Y), [X,Y].
src(c21, (c21(X,Y) --> b(X), c(Y), [X,Y])).

:- dynamic(c22/2).
c22, [p,q] --> b, c.
src(c22, (c22, [p,q] --> b, c)).

% Meta-predicate in a {} body: this is the shape that exposed the
% clause-growth bugs in phase 1, because expand_meta_predicate() inserts
% cells into the freshly built clause.
:- dynamic(c23/2).
c23 --> {maplist(=(1), [_,_])}, b.
src(c23, (c23 --> {maplist(=(1), [_,_])}, b)).

:- dynamic(c24/2).
c24 --> b, {maplist(succ, [1], _)}, c.
src(c24, (c24 --> b, {maplist(succ, [1], _)}, c)).

% --- expected pipeline differences -------------------------------------
%
% The consult path runs stages the reference translation never sees, so
% for these the two SHOULD differ. Asserted as differences rather than
% skipped, so that a stage quietly ceasing to run is a failure and not a
% silent pass.

pipeline_extra(c13, 'phrase/3 inlined by goal_expansion').
pipeline_extra(c23, 'meta-arguments module-qualified by expand_meta_predicate').
pipeline_extra(c24, 'meta-arguments module-qualified by expand_meta_predicate').

% --- checking ----------------------------------------------------------

check(Name, Status) :-
	src(Name, Rule),
	Rule = (Head0 --> _),
	strip_pushback(Head0, Head1),
	functor(Head1, F, A),
	A2 is A + 2,
	functor(Head, F, A2),
	(  clause(Head, Body) -> Got = (Head :- Body) ; Got = no_clause ),
	(  catch(dcg_reference:dcg_rule(Rule, Ref0), E, (Ref0 = err(E)))
	-> Ref = Ref0
	;  Ref = no_reference
	),
	(  pipeline_extra(Name, Why)
	-> (  variant(Got, Ref)
	   -> format("PIPELINE-EXTRA-GONE ~w: expected to differ (~w) but matched~n", [Name, Why]),
	      Status = bad
	   ;  Status = ok
	   )
	;  variant(Got, Ref)
	-> Status = ok
	;  format("CONSULT-DIFF ~w~n   consulted ~q~n   reference ~q~n", [Name, Got, Ref]),
	   Status = bad
	).

strip_pushback((H, _), H) :- !.
strip_pushback(H, H).

main :-
	findall(S, (src(Name,_), check(Name, S)), Ss),
	length(Ss, N),
	findall(x, member(bad, Ss), Bad),
	length(Bad, NBad),
	% A clean run says nothing on stderr. The counts are diagnostics for
	% a run that already has something wrong with it; the guard against
	% the corpus silently collapsing to nothing is the floor below, which
	% goes to stdout where it fails the test.

	(  NBad =:= 0
	-> true
	;  format(user_error, "dcg consult: ~w rules, ~w bad~n", [N, NBad])
	),
	(  N < 20
	-> format("CONSULT-CORPUS-TOO-SMALL: ~w~n", [N])
	;  true
	),
	(  NBad =:= 0
	-> format("dcg consult: all rules agree~n")
	;  format("dcg consult: ~w of ~w disagree~n", [NBad, N])
	).
