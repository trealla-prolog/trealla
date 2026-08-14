:-initialization(main).

main :-
	call_residue_vars((A=[A|B],B=[A,B|B],A=B, false),_Vs).
main.
