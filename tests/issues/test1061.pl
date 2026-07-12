:- use_module(library(clpz)).
:- initialization(main).

grid(NTotal, NCols, NRows) :-
	NTotal #= NCols * NRows,
	NCols #>= 1,
	NRows #>= 1,
	NRows #=< 2,
	labeling([max(NRows)], [NTotal, NCols, NRows]).

main :-
	findall(NCols-NRows, grid(2, NCols, NRows), Sols),
	write(Sols), nl.
