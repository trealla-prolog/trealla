:- module(charsio, [
	get_line_to_chars/3,
	get_single_char/1,
	get_n_chars/3,
	read_from_chars/2
	]).

get_single_char(C) :-
    get_unbuffered_char(C).

get_line_to_chars(Stream, Cs0, Cs) :-
    getline(Stream, Line,[terminator(true)]),
    partial_string(Line,Cs0,Cs).

get_n_chars(Stream, N, Cs) :-
    bread(Stream, N, Cs).

read_from_chars(Cs, T) :-
    read_term_from_chars(Cs, T, []).

fabricate_var_name(VarType, VarName, N) :-
    char_code('A', AC),
    LN is N mod 26 + AC,
    char_code(LC, LN),
    NN is N // 26,
    (  NN =:= 0 ->
       (  VarType == fabricated ->
          atom_chars(VarName, ['_', LC])
       ;  VarType == numbervars ->
          atom_chars(VarName, [LC])
       )
    ;  number_chars(NN, NNChars),
       (  VarType == fabricated ->
          atom_chars(VarName, ['_', LC | NNChars])
       ;  VarType == numbervars ->
          atom_chars(VarName, [LC | NNChars])
       )
    ).

