:-initialization(main).
:- use_module(library(when)).

main :-
	main1, main2, main3.

main1 :-
	NV1=nv1,
	when((nonvar(NV1) ; nonvar(NV2)), (write(ok1), nl)),
	write(here11), nl,
	NV2=nv2,
	write(here12), nl.

main2 :-
	NV1=nv1,
	when((nonvar(NV1) , nonvar(NV2)), (write(ok2), nl)),
	write(here21), nl,
	NV2=nv2,
	write(here22), nl.

main3 :-
	NV=nv,
	when(nonvar(NV), (write(ok3), nl)),
	write(here3), nl.
