:- initialization(main).
:- dynamic(insect/1).

insect(ant).
insect(bee).

% The output of this program using ISO Prolog
% logical update semantics should be:
%
%   ant
%   here1
%   here2
%   bee
%   here1

main :-
	retract(insect(X)),
		write(X), nl,
		write(here1), nl,
		retract(insect(bee)),
		write(here2), nl,
		fail.
main :-
	halt.
