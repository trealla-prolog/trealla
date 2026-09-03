% expand_term/2 is the expansion driver, not just a grammar-rule
% translator: term_expansion/2 hook first, then translation of the
% result, then identity. It used to have only the middle clause, so it
% failed on every term that was not a grammar rule and never consulted
% the hook - which also meant a user could not iterate expansion by
% hand, the usual answer to a system that expands only once.

term_expansion(fa, fb).
term_expansion(fb, fc).
term_expansion(mklist, [(ra --> [a]), plain]).

% expansion by hand to a fixed point, which needs the hook to be reached
fixpoint(T, X) :-
	expand_term(T, T1),
	(   T1 == T
	->  X = T
	;   fixpoint(T1, X)
	).

:- initialization(main).

main :-
	check(identity, (expand_term(plain_atom, A), A == plain_atom)),
	check(rule_identity, (expand_term((h :- b), B), B == (h :- b))),
	check(hook, (expand_term(fa, C), C == fb)),
	check(single_pass, (expand_term(fa, D), D \== fc)),
	check(manual_fixpoint, (fixpoint(fa, E), E == fc)),
	check(dcg, (expand_term((g --> [x]), F), F = (g(S0,S) :- S0 = [x|S]))),
	check(hook_output_dcg,
		(expand_term(mklist, G), G = [(ra(T0,T) :- T0 = [a|T]), plain])).

check(Name, Goal) :-
	(   catch(Goal, Err, (format("~w threw ~q~n", [Name,Err]), fail))
	->  format("~w ok~n", [Name])
	;   format("~w FAILED~n", [Name])
	).
