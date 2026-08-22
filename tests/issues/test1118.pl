:- initialization(main).

% Issue #1118: a query that writes and then fails. Each attempt runs
% the query once, so the capture holds what that one run wrote; the
% outcome 'none' used to be reached by running the query a second
% time, which wrote its output twice over.

?- put_char(a), false.
   outputs("a"), false.

% the workaround the issue reports, which has to keep working

?- put_char(a), false ; true.
   outputs("a").

% every branch of a failing query writes, and each does so once

?- put_char(a), false ; put_char(b), false.
   outputs("ab"), false.

% the same for a query that writes and then throws

?- put_char(a), atom_length(_, _).
   outputs("a"), instantiation_error.

main :- use_module(library(quads)), run_quads.
