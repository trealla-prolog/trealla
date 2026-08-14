:- initialization(main).
:- dynamic(insect/1).

insect(ant).
insect(bee).

% The output of this program using ISO Prolog
% logical update semantics should be:
%
%   ant
%   here
%   bee

main :-
    retract(insect(X)),
        write(X), nl,
        retract(insect(bee)),
        write(here), nl,
        fail.
main.
