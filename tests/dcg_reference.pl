% Frozen copy of the translation core of the shared library/dcgs.pl, as
% it stood before that file was replaced. NEVER loaded by the system -
% it exists only as the differential oracle for the DCG tests, so that
% "does the native translator still agree with the reference?" stays an
% answerable question after the reference stops being the implementation.
%
% Deliberately stripped of everything that is not translation: no
% phrase/2..5, no seq//1 / seqq//1 / ...//0, and in particular no
% user:term_expansion/2 or user:goal_expansion/2. Loading those hooks
% would re-install the old expansion machinery over the native one and
% quietly make the tests measure the wrong thing.
%
% Do not "fix" anything here. Its whole value is being the unmodified
% reference; where the native implementation deliberately differs, the
% tests carry an explicit divergence entry (see #1102).

:- module(dcg_reference, [dcg_rule/2, dcg_body/4, dcg_constr/1]).

:- use_module(library(error)).
:- use_module(library(lists), [append/3]).
:- use_module(library(loader), [strip_module/3]).

% The same version of the below two dcg_rule clauses, but with module scoping.
dcg_rule(( M:NonTerminal, Terminals --> GRBody ), ( M:Head :- Body )) :-
    dcg_non_terminal(NonTerminal, S0, S, Head),
    dcg_body(GRBody, S0, S1, Goal1),
    dcg_terminals(Terminals, S, S1, Goal2),
    Body = ( Goal1, Goal2 ).
dcg_rule(( M:NonTerminal --> GRBody ), ( M:Head :- Body )) :-
    NonTerminal \= ( _, _ ),
    dcg_non_terminal(NonTerminal, S0, S, Head),
    dcg_body(GRBody, S0, S, Body).

% This program uses append/3 as defined in the Prolog prologue.
% Expands a DCG rule into a Prolog rule, when no error condition applies.
dcg_rule(( NonTerminal, Terminals --> GRBody ), ( Head :- Body )) :-
    dcg_non_terminal(NonTerminal, S0, S, Head),
    dcg_body(GRBody, S0, S1, Goal1),
    dcg_terminals(Terminals, S, S1, Goal2),
    Body = ( Goal1, Goal2 ).
dcg_rule(( NonTerminal --> GRBody ), ( Head :- Body )) :-
    NonTerminal \= ( _, _ ),
    dcg_non_terminal(NonTerminal, S0, S, Head),
    dcg_body(GRBody, S0, S, Body).

dcg_non_terminal(NonTerminal, S0, S, Goal) :-
    NonTerminal =.. NonTerminalUniv,
    append(NonTerminalUniv, [S0, S], GoalUniv),
    (  callable(NonTerminal) ->
       Goal =.. GoalUniv
    ;  Goal = NonTerminal % let call/N throw an error instead of throwing one here.
    ).

dcg_terminals(Terminals, S0, S, S0 = List) :-
    append(Terminals, S, List).

dcg_body(Var, S0, S, Body) :-
    var(Var),
    Body = phrase(Var, S0, S).
dcg_body(GRBody, S0, S, Body) :-
    nonvar(GRBody),
    dcg_constr(GRBody),
    dcg_cbody(GRBody, S0, S, Body).
dcg_body(NonTerminal, S0, S, Goal1) :-
    nonvar(NonTerminal),
    \+ dcg_constr(NonTerminal),
    loader:strip_module(NonTerminal, M, NonTerminal0),
    dcg_non_terminal(NonTerminal0, S0, S, Goal0),
    (  functor(NonTerminal, (:), 2) ->
       Goal1 = M:Goal0
    ;  Goal1 = Goal0
    ).

% The following constructs in a grammar rule body
% are defined in the corresponding subclauses.
dcg_constr([]). % 7.14.1
dcg_constr([_|_]). % 7.14.2 - terminal sequence
dcg_constr(( _, _ )). % 7.14.3 - concatenation
dcg_constr(( _ ; _ )). % 7.14.4 - alternative
dcg_constr(( _'|'_ )). % 7.14.6 - alternative
dcg_constr({_}). % 7.14.7
dcg_constr(call(_)). % 7.14.8
dcg_constr(phrase(_)). % 7.14.9
dcg_constr(phrase(_,_)). % extension of 7.14.9
dcg_constr(phrase(_,_,_)). % extension of 7.14.9
dcg_constr(!). % 7.14.10
dcg_constr(\+ G_0) :- % 7.14.11 - not (existence implementation def.)
    throw(error(representation_error(dcg_body), [culprit- (\+ G_0)])).
dcg_constr((If->Then)) :- % 7.14.12 - if-then (existence implementation def.)
    throw(error(representation_error(dcg_body), [culprit- (If->Then)])).

% The principal functor of the first argument indicates
% the construct to be expanded.
dcg_cbody([], S0, S, S0 = S).
dcg_cbody([T|Ts], S0, S, Goal) :-
    must_be(list, [T|Ts]),
    dcg_terminals([T|Ts], S0, S, Goal).
dcg_cbody(( GRFirst, GRSecond ), S0, S, ( First, Second )) :-
    dcg_body(GRFirst, S0, S1, First),
    dcg_body(GRSecond, S1, S, Second).
dcg_cbody(( GREither ; GROr ), S0, S, ( Either ; Or )) :-
    \+ subsumes_term(( _ -> _ ), GREither),
    dcg_body(GREither, S0, S, Either),
    dcg_body(GROr, S0, S, Or).
dcg_cbody(( GRCond ; GRElse ), S0, S, ( Cond ; Else )) :-
    subsumes_term(( _GRIf -> _GRThen ), GRCond),
    dcg_cbody(GRCond, S0, S, Cond),
    dcg_body(GRElse, S0, S, Else).
dcg_cbody(( GREither '|' GROr ), S0, S, ( Either ; Or )) :-
    dcg_body(GREither, S0, S, Either),
    dcg_body(GROr, S0, S, Or).
dcg_cbody({Goal}, S0, S, ( Goal, S0 = S )).
dcg_cbody(call(Cont), S0, S, call(Cont, S0, S)).
dcg_cbody(phrase(Body), S0, S, phrase(Body, S0, S)).
dcg_cbody(phrase(Body, Arg), S0, S, phrase(Body, Arg, S0, S)).
dcg_cbody(phrase(Body, Arg1, Arg2), S0, S, phrase(Body, Arg1, Arg2, S0, S)).
dcg_cbody(!, S0, S, ( !, S0 = S )).
% dcg_cbody(\+ GRBody, S0, S, ( \+ phrase(GRBody,S0,_), S0 = S )).
dcg_cbody(( GRIf -> GRThen ), S0, S, ( If -> Then )) :-
    dcg_body(GRIf, S0, S1, If),
    dcg_body(GRThen, S1, S, Then).

% then.
error_goal(error(instantiation_error, _Context), _).
error_goal(error(E, must_be/2), error(E, must_be/2)).
error_goal(error(E, (=..)/2), error(E, (=..)/2)).
error_goal(error(representation_error(dcg_body), Context),
           error(representation_error(dcg_body), Context)).
error_goal(E, _) :- throw(E).
