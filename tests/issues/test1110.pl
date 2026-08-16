% Issue #1110: \+ (true;1) should raise type_error(callable,(true;1))
% per ISO (matching scryer-prolog's
% error(type_error(callable,(true;1)),(;)/2)), not silently fail.
%
% \+/1 was missing the eager callable check that call/1 and once/1
% already perform on conjunction/disjunction/if-then/soft-cut bodies,
% in two separate code paths:
%   - the interpreted path: bif_iso_negation_1 in src/bif_control.c
%   - the compiled clause-body path: the g_negation_s case of
%     compile_term in src/compile.c (used when \+ appears in a stored
%     predicate body rather than a directive/query)
%
% https://github.com/trealla-prolog/trealla/issues/1110

:- initialization(main).

t(N, G) :-
	(  catch(G, E, (format("~w: ERROR ~w~n", [N,E]), fail))
	-> format("~w: ok~n", [N])
	;  format("~w: FAILED~n", [N])
	).

% These bodies are compiled inline at load time (a separate code path
% from a directive's \+), so they exercise src/compile.c specifically.
c_true_1 :- \+ (true;1).
c_1_true :- \+ (1;true).
c_fail_1 :- \+ (fail,1).
c_fail   :- \+ fail.
c_true   :- \+ true.
c_ff     :- \+ (fail;fail).
c_tf     :- \+ (true,fail).
c_exist  :- \+ (foo(1);bar(2)).

cyclic(X) :- X = f(g(X,_),_).

main :-
	% The reported case, interpreted (top-level \+).
	t(neg_true_1, catch((\+ (true;1), fail), error(type_error(callable,(true;1)),_), true)),

	% Mirror case: the non-callable term is the first disjunct instead
	% of the second. This one already worked before the fix (disjunction
	% checks its first branch eagerly on its own) -- kept as a guard
	% against a regression in that pre-existing behaviour.
	t(neg_1_true, catch((\+ (1;true), fail), error(type_error(callable,(1;true)),_), true)),

	% Same two cases, but compiled into a stored predicate body.
	t(compiled_true_1, catch((c_true_1, fail), error(type_error(callable,(true;1)),_), true)),
	t(compiled_1_true, catch((c_1_true, fail), error(type_error(callable,(1;true)),_), true)),
	t(compiled_fail_1, catch((c_fail_1, fail), error(type_error(callable,(fail,1)),_), true)),

	% Ordinary \+ usage must be unaffected by the extra check.
	t(ok_fail,  c_fail),
	t(ok_true,  \+ c_true),
	t(ok_ff,    c_ff),
	t(ok_tf,    c_tf),
	t(ok_exist, catch((c_exist, fail), error(existence_error(procedure,foo/1),_), true)),
	t(ok_member_neg, \+ member(z,[a,b,c])),
	t(ok_member_pos, \+ \+ member(b,[a,b,c])),

	% Regression guard: the eager check must validate a disposable
	% clone of the goal, not the live term -- otherwise cyclic data
	% passed through \+ gets corrupted. variant/2 is defined as
	% "\+ \+ (...)" in library/iso_ext.pl, so it exercises this
	% directly and is a realistic, commonly-used case.
	cyclic(A), copy_term(A, A2),
	t(variant_of_cyclic_term, variant(A, A2)),

	true.
