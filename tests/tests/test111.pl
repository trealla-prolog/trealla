:- initialization(main).

% A unique clause head does not make a goal deterministic when another
% clause carries a variable in an indexed argument.
%
% p/2 below is loaded with one clause: a RULE whose head has a variable
% first argument. check_unique() runs at load time, finds nothing after
% it, and marks it is_unique - correctly, at that moment. Clauses are
% then asserted past the indexing threshold, so find_key() sets
% is_var_in_first_arg and diverts the lookup to the linear walk.
%
% The walk reaches the rule first, its head unifies (a var argument
% unifies with anything), and commit_frame() then computed
%
%     is_det = !q->has_vars && cl->is_unique
%
% as true: is_unique was stale, and q->has_vars describes the LAST
% unification rather than the goal - unify() clears it on entry and only
% sets it for variables at depth > 1, so p(target,X) against p(K,V)
% leaves it false. has_next_key() correctly reported more candidates,
% but is_det sits ahead of it in the || chain.
%
% The alternatives choicepoint was therefore dropped, the rule's body
% failed, and the walk never resumed to reach the matching fact. This is
% the shape of Logtalk's logtalk_library_path/2 - a packs rule with a var
% first argument among 500 ground facts.

:- dynamic(p/2).

p(K, V) :- helper(K, V).

helper(_, _) :- fail.

main :-
	( between(1,505,I), assertz(p(k(I), v(I))), fail ; true ),
	assertz(p(target, found)),
	findall(X, p(target,X), L),
	write(L), nl,
	halt.
