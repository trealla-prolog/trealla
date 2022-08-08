:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(charsio)).
:- use_module(library(between)).

:- use_module(library(apply)).     % Added by AD

% A grammar in DCG

sentence --> np, vp.
np --> det, noun.
vp --> verb, np.
vp --> verb.

noun --> [woman].
noun --> [man].
verb --> [shoots].
det --> [the].
det --> [a].

/*
    Generate all possible sentences...
*/

main :- phrase(sentence,X), write(X), nl, fail.
main.
