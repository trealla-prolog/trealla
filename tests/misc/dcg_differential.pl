% Differential test: native '$dcg_rule'/2 against library(dcgs)'s
% dcg_rule/2, which is still live during phases 0-2.
%
% Built so it CANNOT enforce the reference's bugs. The divergence list is
% checked first, and for a listed case the reference is not the oracle:
% the required behaviour is asserted directly, AND the two are asserted
% to differ, so the entry fails loudly if they ever agree again. Without
% that, a harness like this quietly converts every known defect into a
% regression test.
%
% The oracle is tests/dcg_reference.pl, a frozen copy of the shared
% implementation's translation core. It is loaded only by these tests,
% never by the system.

:- initialization(main).
:- use_module(library(dcgs)).
:- ensure_loaded('tests/dcg_reference').

% --- divergence list -------------------------------------------------
%
% Issue #1102 (== #832). A nonvar non-callable in non-terminal position
% is a permanent condition, so the native translator decides it at
% translation time and reports the bare subterm. The reference drops the
% S0/S arguments and leaves it to call/1, which reports the whole body.
% The reference cannot be fixed in place - it is shared with Scryer and
% UWN - so this divergence is permanent, not transitional. If it ever
% starts passing, the reference was fixed upstream and the entry should
% be deleted.

divergence(noncallable_after_braces, (a --> ({fail},1)), type_error(callable,1)).
divergence(noncallable_first,        (a --> (1,{2})),    type_error(callable,1)).
divergence(noncallable_last,         (a --> ({2},1)),    type_error(callable,1)).

% --- corpus ----------------------------------------------------------
%
% Constructs of ISO 7.14, alone and nested, plus the head forms.

case(empty,          (a --> [])).
case(nonterminal,    (a --> b)).
case(conjunction,    (a --> b, c)).
case(conj3,          (a --> b, c, d)).
case(terminals,      (a --> [x,y,z])).
case(terminal_one,   (a --> [x])).
case(string,         (a --> "abc")).
case(alternation,    (a --> b ; c)).
case(bar,            (a --> b | c)).
case(braces,         (a --> {g})).
case(braces_conj,    (a --> {g,h})).
case(cut,            (a --> !)).
case(cut_mixed,      (a --> [x], !, b)).
case(ite_in_alt,     (a --> b -> c ; d)).
case(ite_in_bar,     (a --> (b -> c | d))).
case(call1,          (a --> call(x))).
case(phrase1,        (a --> phrase(x))).
case(phrase2,        (a --> phrase(x,y))).
case(phrase3,        (a --> phrase(x,y,z))).
case(head_args,      (a(X) --> b(X), [X])).
case(head_args2,     (a(X,Y) --> b(X), c(Y))).
case(pushback,       (a, [p] --> b)).
case(pushback_multi, (a, [p,q] --> b, c)).
case(module_head,    (m:a --> b)).
case(module_body,    (a --> m:b)).
case(module_both,    (m:a --> n:b)).
case(var_body,       (a --> _X)).
case(var_in_conj,    (a --> b, _X, c)).
case(empty_mixed,    (a --> [], b, [])).
case(nested,         (a --> (b,c), (d;e), {f})).
case(nested_deep,    (a --> ((b,c);(d,e)), {f}, [g])).
case(alt_of_alt,     (a --> (b;c);(d;e))).
case(partial_list,   (a --> [x|_T])).
case(improper_list,  (a --> [x|y])).
case(negation,       (a --> \+ b)).
case(ite_toplevel,   (a --> (b -> c))).
case(string_conj,    (a --> "ab", c, "de")).

% Long string terminal: emitted as '$string_prefix'/3 rather than
% materialised, so this one is EXPECTED to differ from the reference.
% Without it the optimisation would be untested here - no rule anywhere
% in the tree has a terminal over the 64-byte threshold.

case(long_literal,   (a --> "0123456789012345678901234567890123456789012345678901234567890123456789")).

% Cases where native and reference SHOULD differ for a reason other than
% a defect. Asserted as differences, so that the optimisation silently
% ceasing to fire is a failure rather than a quiet pass.

expected_diff(long_literal, 'long terminal emitted as $string_prefix/3, not materialised').

% --- runners ---------------------------------------------------------

outcome(G, ok(G)) :- catch(G, E, (throw(caught(E)))), !.
outcome(_, failed).

run_native(R, X) :-
	(  catch('$dcg_rule'(R, Out), E, true)
	-> (var(E) -> X = ok(Out) ; X = err(E))
	;  X = failed
	).

run_ref(R, Y) :-
	(  catch(dcg_reference:dcg_rule(R, Out), E, true)
	-> (var(E) -> Y = ok(Out) ; Y = err(E))
	;  Y = failed
	).

% A listed divergence must (a) give the required native answer and
% (b) NOT agree with the reference.

check_divergence(Name) :-
	divergence(Name, Rule, Formal),
	run_native(Rule, X),
	run_ref(Rule, Y),
	(  X = err(error(Formal, _))
	-> (  variant(X, Y)
	   -> format("DIVERGENCE-GONE ~w: reference now agrees, delete the entry~n", [Name])
	   ;  true
	   )
	;  format("DIVERGENCE-FAILED ~w: wanted ~q, got ~q~n", [Name, Formal, X])
	).

check_case(Name) :-
	case(Name, Rule),
	run_native(Rule, X),
	run_ref(Rule, Y),
	(  expected_diff(Name, Why)
	-> (  variant(X, Y)
	   -> format("EXPECTED-DIFF-GONE ~w: should differ (~w) but matched~n", [Name, Why])
	   ;  true
	   )
	;  variant(X, Y)
	-> true
	;  format("DIFF ~w~n   native ~q~n   ref    ~q~n", [Name, X, Y])
	).

main :-
	forall(divergence(Name, _, _), check_divergence(Name)),
	forall(case(Name, _), check_case(Name)),
	findall(x, divergence(_,_,_), Ds), length(Ds, ND),
	findall(x, case(_,_), Cs), length(Cs, NC),
	findall(x, expected_diff(_,_), Es), length(Es, NE),
	format("dcg differential: ~w cases, ~w divergences, ~w expected diffs~n", [NC, ND, NE]),
	halt.
