% Issue #1143: Name//Arity in a use_module/2 import list was ignored.
%
% module/2 export lists already translate a non-terminal indicator to
% Name/(Arity+2); import lists only matched '/', so p//1 found no
% predicate and use_module/2 silently imported nothing.
%
% https://github.com/trealla-prolog/trealla/issues/1143

:- module(t1143_gram, [p//1, plain/1]).

p(X) --> [X].

plain(ok).

:- module(t1143_use, []).

:- use_module(t1143_gram, [p//1]).
:- use_module(t1143_gram, [p//1 as q]).
:- use_module(t1143_gram, [p//1 as r//1]).
:- use_module(t1143_gram, [plain/1]).
:- use_module(t1143_gram, [plain/1 as plain2]).

:- initialization(main).

main :-
	check(dcg_import, current_predicate(p/3)),
	check(dcg_call, phrase(p(a), [a], [])),
	check(dcg_as_atom, (current_predicate(q/3), phrase(q(b), [b], []))),
	check(dcg_as_pi, (current_predicate(r/3), phrase(r(c), [c], []))),
	check(plain_import, (current_predicate(plain/1), plain(ok))),
	check(plain_as, (current_predicate(plain2/1), plain2(ok))).

check(Name, Goal) :-
	(   catch(Goal, E, (format("~w threw ~q~n", [Name,E]), fail))
	->  format("~w ok~n", [Name])
	;   format("~w FAILED~n", [Name])
	).
