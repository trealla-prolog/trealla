% Very simple demo REPL

:- module(repl, [repl/0]).

:- use_module(library(charsio)).
:- use_module(library(format)).

repl :-
    repl_loop.

repl_loop :-
    '$readline'("\r$- ", Input),
    ( Input = "halt" -> true
    ; once(parse_and_execute(Input))
    ; true
	),
    repl_loop.

parse_and_execute(Input) :-
    catch(
        ( read_term_from_chars(Input, Term, [variable_names(Vars)]),
          execute_term(Term, Vars)
        ),
        Error,
        ( format('~w~n', [Error]), fail )
    ).

execute_term(Term, Vars) :-
    call(Term) *->
    ( display_results(Vars),
      write('; '), flush_output,
      get_single_char(Char),
      (
       Char = ';' -> fail
      ; Char = 'e' -> halt
      ; true
      )
    )
    ; format('false.~n', []).

display_results([]) :- format('true.~n', []).
display_results([Name=Value|Vars]) :-
    format('~w = ~w', [Name, Value]),
    display_rest(Vars).

display_rest([]) :- format('.~n', []).
display_rest([Name=Value|Vars]) :-
    format(', ~w = ~w', [Name, Value]),
    display_rest(Vars).
