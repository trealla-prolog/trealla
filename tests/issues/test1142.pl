% Issue #1142: a --> rule produced by term_expansion/2 was asserted raw
% as a fact for '-->'/2 instead of being translated.
%
% tokenize() translates grammar rules ahead of the expansion hook, so a
% --> term coming back OUT of the hook had already sailed past that
% branch. It now gets translated on the re-parse of the hook's result,
% which is still ahead of assign_vars() - the order dcg_expand_clause()
% requires. SWI and Scryer both translate hook output.
%
% https://github.com/trealla-prolog/trealla/issues/1142

term_expansion(mk_scalar, (scalar(X) --> [X])).

term_expansion(mk_list, [ (digits([D|Ds]) --> digit(D), digits(Ds)),
                          (digits([D])    --> digit(D)),
                          (digit(D)       --> [D], { memberchk(D, [zero,one]) }),
                          plain_fact,
                          (plain_rule :- plain_fact) ]).

% the hook's output may itself call phrase/2, which goal expansion
% rewrites - the S0/S threading has to survive that
term_expansion(mk_ge, [ (inner --> [i]),
                        (drive(L) :- phrase(inner, L)) ]).

mk_scalar.
mk_list.
mk_ge.

% a hand-written rule must still work alongside
handwritten --> [h].

:- initialization(main).

main :-
	check(scalar, phrase(scalar(a), [a])),
	check(list_recursive, (phrase(digits(L), [one,zero,one]), L == [one,zero,one])),
	check(list_reject, \+ phrase(digits(_), [one,nine])),
	check(list_fact, plain_fact),
	check(list_rule, plain_rule),
	check(goal_expansion, drive([i])),
	check(handwritten, phrase(handwritten, [h])),
	% pre-fix this was asserted as a fact for '-->'/2, so the raw
	% call succeeded; it must not now
	check(no_dcg_fact, \+ catch('-->'(scalar(a), [a]), _, fail)).

check(Name, Goal) :-
	(   catch(Goal, E, (format("~w threw ~q~n", [Name,E]), fail))
	->  format("~w ok~n", [Name])
	;   format("~w FAILED~n", [Name])
	).
