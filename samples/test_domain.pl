%
% To use:
%
%	$ cd samples
%	$ tpl -g main,halt -f test_domain.pl
%

:- use_module(library(format)).
:- use_module(domain).

main :-
	domain(X,[5,6,7,1]),
	domain(Y,[3,4,5,6]),
	domain(Z,[1,6,7,8]),
	X=Y, Y=Z,
	format("X is ~d~n", [X]).
