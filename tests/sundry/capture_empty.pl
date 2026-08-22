:- initialization(main).

% An output capture that caught nothing is the empty character list.
% make_stringn() maps a zero length to '', which is what the atom
% builtins want but not what a list of characters is, so a silent goal
% used to report having written '' -- and no quad could say that its
% query wrote nothing.

?- with_output_to(chars(Cs), true).
   Cs = [].

?- with_output_to(string(Cs), true).
   Cs = [].

% an atom capture is still the empty atom

?- with_output_to(atom(A), true).
   A = ''.

% and a capture that did catch something is unchanged

?- with_output_to(chars(Cs), write(ab)).
   Cs = [a,b].

% so outputs/1 can describe a query that writes nothing

silent_fail ?- fail.
   outputs(""), false.

silent_true ?- true.
   outputs(""), true.

main :- use_module(library(quads)), run_quads.
