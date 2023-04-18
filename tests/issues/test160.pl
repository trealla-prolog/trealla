:-initialization(main).

main :-
	main1, main2, main3.

main1 :-
	NV1=nv1,
	when((nonvar(NV1) ; nonvar(NV2)), writeln(ok1)),
	writeln(here11),
	NV2=nv2,
	writeln(here12).

main2 :-
	NV1=nv1,
	when((nonvar(NV1) , nonvar(NV2)), writeln(ok2)),
	writeln(here21),
	NV2=nv2,
	writeln(here22).

main3 :-
	NV=nv,
	when(nonvar(NV), writeln(ok3)),
	writeln(here3).
