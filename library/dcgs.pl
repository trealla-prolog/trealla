/** Support for Definite Clause Grammars.

A Prolog definite clause grammar (DCG) describes a sequence. Operationally, DCGs
can be used to parse, generate, complete and check sequences manifested as lists.

Check [The Power of Prolog chapter on DCGs](https://www.metalevel.at/prolog/dcg)
to learn more about them.
*/

% Trealla's own implementation. The translation itself is native - see
% src/bif_dcgs.c - and what remains here is the handful of predicates
% that are genuinely better in Prolog, plus the declarations.
%
% This file used to be a copy of the reference implementation shared with
% Scryer and UWN's specification work, which meant Trealla could not
% carry a local patch to it. Issue #1102 (== #832) is a defect in that
% file, so the only way to fix it was to stop using it. A frozen copy
% survives as tests/dcg_reference.pl, loaded only by the differential
% tests, so "do we still agree with the reference?" stays answerable.
%
% The module, its exports and its operator are unchanged, so
% use_module(library(dcgs)) and dcgs:-qualified calls behave exactly as
% before.

:- module(dcgs,
          [op(1105, xfy, '|'),
           phrase/2,
           phrase/3,
           phrase//2,
           phrase//3,
           seq//1,
           seqq//1,
           ... //0,
           (-->)/2
          ]).

:- use_module(library(error)).
:- use_module(library(lists), [append/3]).
:- use_module(library(loader), [strip_module/3]).

:- meta_predicate(phrase(2, ?)).
:- meta_predicate(phrase(2, ?, ?)).
:- meta_predicate(phrase(3, ?, ?, ?)).
:- meta_predicate(phrase(4, ?, ?, ?, ?)).

%% phrase(+Body, ?Ls).

phrase(GRBody, S0) :-
    phrase(GRBody, S0, []).

%% phrase(+Body, ?Ls, ?Ls0).
%
% True iff Body describes part of the list Ls and the rest of Ls is Ls0.
%
% Example:
%
% ```
% ?- phrase(seq(X), "aaa", Y).
%    X = [], Y = "aaa"
% ;  X = "a", Y = "aa"
% ;  X = "aa", Y = "a"
% ;  X = "aaa", Y = [].
% ```

% '$dcg_body'/4 FAILS for anything that is not a 7.14 construct, which is
% what lets the last branch handle an ordinary non-terminal by appending
% the two arguments - so phrase(1, L) still reaches call/3 and reports
% type_error(callable, 1) from there. It throws only where ISO requires,
% including type_error(callable, T) for a non-callable in non-terminal
% position, which is the #1102 fix.

phrase(GRBody, S0, S) :-
    strip_module(GRBody, M, B),
    (   var(GRBody) ->
        instantiation_error(phrase/3)
    ;   '$dcg_body'(B, S0, S, Goal) ->
        call(M:Goal)
    ;   extend(B, [S0,S], Goal) ->
        call(M:Goal)
    ;   call(M:B, S0, S)
    ).

phrase(GRBody, Arg, S0, S) :-
    strip_module(GRBody, M, B),
    (   var(GRBody) ->
        instantiation_error(phrase/4)
    ;   extend(B, [Arg], B2),
        '$dcg_body'(B2, S0, S, Goal) ->
        call(M:Goal)
    ;   extend(B, [Arg,S0,S], Goal) ->
        call(M:Goal)
    ;   call(M:B, Arg, S0, S)
    ).

phrase(GRBody, Arg1, Arg2, S0, S) :-
    strip_module(GRBody, M, B),
    (   var(GRBody) ->
        instantiation_error(phrase/5)
    ;   extend(B, [Arg1,Arg2], B2),
        '$dcg_body'(B2, S0, S, Goal) ->
        call(M:Goal)
    ;   extend(B, [Arg1,Arg2,S0,S], Goal) ->
        call(M:Goal)
    ;   call(M:B, Arg1, Arg2, S0, S)
    ).

% As the reference does it: append the extra arguments to the body term
% first, then translate the result.

extend(B, Extra, B2) :-
    callable(B),
    B =.. L0,
    append(L0, Extra, L),
    B2 =.. L.

%% seq(Seq)//
%
% Describes a sequence.
%
% The first clause is deliberately NOT a DCG rule: it is a hand-written
% seq/3 guarding var(Xs), Cs0 == [], which is what terminates generation.
% Carried over from the reference verbatim.

seq(Xs, Cs0,Cs) :-
   var(Xs),
   Cs0 == [],
   !,
   Xs = [],
   Cs0 = Cs.
seq([]) --> [].
seq([E|Es]) --> [E], seq(Es).

%% seqq(SeqOfSeqs)//
%
% Describes a sequence of sequences.

seqq([]) --> [].
seqq([Es|Ess]) --> seq(Es), seqq(Ess).

%% ...//
%
% Describes an arbitrary number of elements. The hand-written .../2
% clause below terminates generation, as with seq//1.
%
% Written as TWO rules rather than the reference's `[] | [_], ...`.
% Same solutions in the same order, but an in-body disjunction under deep
% recursion is quadratic in this engine, where two clauses are linear:
% skipping to a marker in an 80k-character string took 19s as a
% disjunction and is linear without it. Measured with the disjunction
% reproduced in plain Prolog, so this is not a DCG effect - see the note
% in docs/native-dcg-design.md §11.

...(Cs0,Cs) :-
   Cs0 == [],
   !,
   Cs0 = Cs.
... --> [].
... --> [_], ... .

% Inline phrase/3 at consult time, as the reference did. This MUST NOT
% throw: a compile-time expansion may not raise an error at a different
% moment than the runtime would, so a body whose translation would throw
% is declined here and left to fail at runtime instead, where it belongs
% (see section 5.3 of docs/native-dcg-design.md). Declining is just
% failing the hook, which leaves the ordinary phrase/3 call in place.

user:goal_expansion(phrase(GRBody, S0, S), Goal) :-
    nonvar(GRBody),
    strip_module(GRBody, M, B),
    nonvar(B),
    catch(dcg_inline(B, S0, S, G), _, fail),
    (   GRBody = (_:_) ->
        Goal = M:G
    ;   Goal = G
    ).

% Translate a construct, or append the two arguments to an ordinary
% non-terminal. Deliberately NOT wrapped in its own catch: a body whose
% translation throws must propagate out to the catch above, which
% declines the expansion and leaves the runtime phrase/3 call in place.
%
% Falling back to extend/3 on a throw would be silently wrong - the body
% is a construct, so appending arguments to it builds nonsense like
% ','(A,B,S0,S). That is what issue #832's own test caught.

dcg_inline(B, S0, S, G) :-
    (   '$dcg_body'(B, S0, S, G0) ->
        G = G0
    ;   extend(B, [S0,S], G)
    ).

user:goal_expansion(phrase(GRBody, S0), phrase(GRBody, S0, [])).

% (-->)/2 behaves as if it didn't exist. We export (and define) it
% only so that clauses for (-->)/2 cannot be asserted when
% library(dcgs) is loaded.

(_-->_) :- throw(error(existence_error(procedure,(-->)/2),(-->)/2)).

user:expand_term((H --> B), Out) :-
	'$dcg_rule'((H --> B), Out), !.

