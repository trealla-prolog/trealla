:- initialization(main).

% A toplevel answer reports an answer substitution, so every equation
% in an answer description binds a variable. '1 = X' does not, and is
% rejected when the file is consulted rather than being loaded as a
% clause or run as a quad that passes.

?- X = 1.
   1 = X.

main :- write(should_not_run), nl.
