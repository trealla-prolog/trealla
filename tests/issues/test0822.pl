:- initialization(main).

:- use_module(library(dcgs)).
:- use_module(library(format)).

gr1(_,[]) --> "".
gr1(N,[_|Ls]) -->
  format_("~d",[N]),
  "",
  "", % this additional "" is the problem in this "configuration"
  gr1(N,Ls).

gr2(_,[]) --> "".
gr2(N,[_|Ls]) -->
  format_("~d",[N]),
  "",
  gr2(N,Ls).

test1 :- phrase(gr1(1,[_]),Str),
	write_term(Str, [double_quotes(true)]), nl.
test2 :- phrase(gr2(1,[_]),Str),
	write_term(Str, [double_quotes(true)]), nl.

main :-
	test1, test2.

