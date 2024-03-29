:- initialization(main).
:- set_prolog_flag(double_quotes, codes).

name(N, L) :-
	( number(N) -> number_codes(N, L) ; atom_codes(N, L) ).

/*

Perl Style Regular Expressions in Prolog
CMPT 383 Lecture Notes
Robert D. Cameron
April 3, 2002
Case Study: Implementing Perl Style Regular Expressions

A Prolog system for processing Perl style regular expressions can be implemented via the following steps.

   1. Defining the BNF grammar of Perl-style regular expressions.
   2. Defining a Prolog representation of Perl regular expressions.
   3. Build a DCG parser for Perl regular expressions.
   4. Building a regular expression matcher in Prolog.

This is a useful case study of symbolic computing in Prolog and also an exploration of the semantics of regular expressions.
BNF Grammar of Regular Expressions

Following the precedence rules given previously, a BNF grammar for Perl-style regular expressions can be constructed as follows.
<RE> 	::= 	<union> | <simple-RE>
<union> 	::=	<RE> "|" <simple-RE>
<simple-RE> 	::= 	<concatenation> | <basic-RE>
<concatenation> 	::=	<simple-RE> <basic-RE>
<basic-RE> 	::=	<star> | <plus> | <elementary-RE>
<star> 	::=	<elementary-RE> "*"
<plus> 	::=	<elementary-RE> "+"
<elementary-RE> 	::=	<group> | <any> | <eos> | <char> | <set>
<group> 	::= 	"(" <RE> ")"
<any> 	::= 	"."
<eos> 	::= 	"$"
<char> 	::= 	any non metacharacter | "\" metacharacter
<set> 	::= 	<positive-set> | <negative-set>
<positive-set> 	::= 	"[" <set-items> "]"
<negative-set> 	::= 	"[^" <set-items> "]"
<set-items> 	::= 	<set-item> | <set-item> <set-items>
<set-items> 	::= 	<range> | <char>
<range> 	::= 	<char> "-" <char>
Prolog Representation of Regular Expressions

To represent regular expressions as symbolic objects in Prolog, we specify that union/2, conc/2, star/1, plus/1, and group/1 represent the five types of structured (recursively defined) regular expression. The symbolic atoms any and eos represent the metacharacters "." and "$", while char/1 represents a single character. Positive and negative sets are represented respectively as posSet/1 and negSet/1, in which set items are enclosed in a list. The set items may be individual char/1 items or range/2 structures for character ranges.
DCG Parser for Regular Expressions

Constructing the DCG parser requires apply the left recursion removal techniques illustrated earlier for both the <RE> and <basic-RE> productions. We also left factor the <simple-RE> production to avoid backtracking.
*/

re(Z) --> basicRE(W), reTail(W, Z).
reTail(W, Z) --> "|", basicRE(X), reTail(union(W,X), Z).
reTail(W, W) --> {true}.
basicRE(Z) --> simpleRE(W), basicREtail(W, Z).
basicREtail(W, Z) --> simpleRE(X), basicREtail(conc(W,X), Z).
basicREtail(W, W) --> {true}.
simpleRE(Z) --> elementalRE(W), simpleREtail(W, Z).
simpleREtail(W, star(W)) --> "*".
simpleREtail(W, plus(W)) --> "+".
simpleREtail(W, W) --> {true}.
re_metachar("\\").
re_metachar("|").
re_metachar("*").
re_metachar("+").
re_metachar(".").
re_metachar("[").
re_metachar("$").
re_metachar("(").
re_metachar(")").
elementalRE(any) --> ".".
elementalRE(group(X)) --> "(", re(X), ")".
elementalRE(eos) --> "$".
elementalRE(char(C)) --> [C], {\+(re_metachar([C]))}.
elementalRE(char(C)) --> "\\", [C], {re_metachar([C])}.
%  For sets, first try the negative set syntax.  If the "[^" recognition
%  succeeds, use cut to make sure that any subsequent failure does not
%  cause the positive set interpretation to be used.
elementalRE(negSet(X)) --> "[^", {!}, setItems(X), "]".
elementalRE(posSet(X)) --> "[", setItems(X), "]".
setItems([Item1|MoreItems]) --> setItem(Item1), setItems(MoreItems).
setItems([Item1]) --> setItem(Item1).
setItem(char(C)) --> [C], {\+(set_metachar([C]))}.
setItem(char(C)) --> "\\", [C], {set_metachar([C])}.
setItem(range(A,B)) --> setItem(char(A)), "-", setItem(char(B)).
set_metachar("\\").
set_metachar("]").
set_metachar("-").


% Logic of Regular Expression Matching and Selection

%
% rematch1(RE, S, Unmatched, Selected) is true if RE matches
% a string Prefix such that S = [Prefix|Unmatched], and
% Selected is the list of substrings of Prefix that matched
% the parenthesized components of RE.

rematch1(union(RE1, _RE2), S, U, Selected) :-
  rematch1(RE1, S, U, Selected).
rematch1(union(_RE1, RE2), S, U, Selected) :-
  rematch1(RE2, S, U, Selected).
rematch1(conc(RE1, RE2), S, U, Selected) :-
  rematch1(RE1, S, U1, Sel1),
  rematch1(RE2, U1, U, Sel2),
  append(Sel1, Sel2, Selected).
% Try longest match first.
rematch1(star(RE), S, U, Selected) :-
  rematch1(RE, S, U1, Sel1),
  rematch1(star(RE), U1, U, Sel2),
  append(Sel1, Sel2, Selected).
rematch1(star(_RE), S, S, []).
rematch1(plus(RE), S, U, Selected) :-
  rematch1(RE, S, U1, Sel1),
  rematch1(star(RE), U1, U, Sel2),
  append(Sel1, Sel2, Selected).
% Match a group and add it to the end of
% list of selected items from the submatch.
rematch1(group(RE), S, U, Selected) :-
  rematch1(RE, S, U, Sel1),
  append(P, U, S),
  append(Sel1, [P], Selected).

rematch1(any, [_C1|U], U, []).
% Note that the following works for matching both regular
% characters and metacharacters.
rematch1(char(C), [C|U], U, []).

rematch1(eos, [], [], []).

rematch1(negSet(Set), [C|U], U, []) :-
  \+(charSetMember(C, Set)).

rematch1(posSet(Set), [C|U], U, []) :-
  charSetMember(C, Set).

charSetMember(C, [char(C) | _]).
charSetMember(C, [range(C1, C2) | _]) :-
  C1 =< C,
  C =< C2.
charSetMember(C, [_|T]) :- charSetMember(C, T).

/*
  Lexical Analysis with Regular Expressions

    * Define a regular expression that matches and extracts tokens.
    * Repeatedly apply this expression until all input is consumed.

The tokenize/3 predicate will do the whole job, given a satisfactory regular expression!
*/

%
%  tokenize(RE, Input, Output) is true if
%    - RE is the string representation of a regular expression,
%         with tokens identified by parenthesized subexpressions
%    - Input is an input string
%    - Output is the list of tokens extracted by repeated application
%      of RE to Input.
%
tokenize(RE, Input, Output) :-
  re(Parsed_RE, RE, []),
  tokenize2(Parsed_RE, Input, Output).

tokenize2(_P_RE, [], []).
tokenize2(P_RE, Input, Output) :-
  rematch1(P_RE, Input, Unmatched, SelStrings),
  names(Tokens, SelStrings),
  tokenize2(P_RE, Unmatched, MoreTokens),
  append(Tokens, MoreTokens, Output).

names([],[]).
names([Sym1|MoreSymbols], [Str1|MoreStrings]) :-
  name(Sym1, Str1),
  names(MoreSymbols, MoreStrings).

/*
To use the tokenizer for lexical analysis, we now need only define a regular expression that specifies the allowable forms of tokens and whitespace. Tokens should be included inside parenthesized regular expressions so they are returned; whitespace should not be included in parenthesized expressions. For example, if we consider the tokens for the numeric expression grammar described previously, an appropriate regular expression is " +|([0-9]+|\+|-)". Note that this expression defines 4 alternative string types: (1) sequences of one or more spaces (" +"), (2) sequences of one or more digits ("[0-9]+"), (3) the + operator (which must be escaped because it is a metacharacter), and (4) the - operator. However, only the last three are selected as tokens by inclusion within parentheses.

Finally, to use this expression in the Prolog tokenizer, the escape character itself must be escaped due to Prolog's string syntax conventions.
*/

main :-
  tokenize(" +|([0-9]+|\\+|-)", "12 + 4 - 29", L),
  write(L), nl, fail.
main.

  /*
L = [12,+,4,-,29] ? ;

L = [12,+,4,-,2,9] ? ;

L = [1,2,+,4,-,29] ? ;

L = [1,2,+,4,-,2,9] ? ;

Concluding Remarks

The regular expression package defined here in Prolog is intended to illustrate both the power of regular expressions and their semantics in logical form. It is not intended to be a practical tool. However, the built-in regular expression support provided by many scripting languages (Perl, Javascript and so on), Unix tools (emacs, ex, grep, and so on), and lexical analyzer generators (lex, flex, flex++, and so on) are indeed practical and can greatly simplify string processing. The use of regular expressions for lexical analysis is a standard technique that is widely used in symbolic computing applications.
*/
