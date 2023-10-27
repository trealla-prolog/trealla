:- module(charsio, [
	char_type/2,
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

%% char_type(+Char, -Type).
%
% Given a Char, Type is one of the categories that char fits in.
% Possible categories are:
%
% - `alnum`
% - `alpha`
% - `alphabetic`
% - `alphanumeric`
% - `ascii`
% - `ascii_graphic`
% - `ascii_punctuation`
% - `binary_digit`
% - `control`
% - `decimal_digit`
% - `exponent`
% - `graphic`
% - `graphic_token`
% - `hexadecimal_digit`
% - `layout`
% - `lower`
% - `meta`
% - `numeric`
% - `octal_digit`
% - `octet`
% - `prolog`
% - `sign`
% - `solo`
% - `symbolic_control`
% - `symbolic_hexadecimal`
% - `upper`
% - `to_lower(Lower)`
% - `to_upper(Upper)`
% - `whitespace`
%
% An example:
%
% ```
% ?- char_type(a, Type).
%    Type = alnum
% ;  Type = alpha
% ;  Type = alphabetic
% ;  Type = alphanumeric
% ;  Type = ascii
% ;  Type = ascii_graphic
% ;  Type = hexadecimal_digit
% ;  Type = lower
% ;  Type = octet
% ;  Type = prolog
% ;  Type = symbolic_control
% ;  Type = to_lower("a")
% ;  Type = to_upper("A")
% ;  false.
% ```
%
% Note that uppercase and lowercase transformations use a string. This is because
% some characters do not map 1:1 between lowercase and uppercase.
char_type(Char, Type) :-
        must_be(character, Char),
        (   ground(Type) ->
            (   ctype(Type) ->
                '$char_type'(Char, Type)
            ;   domain_error(char_type, Type, char_type/2)
            )
        ;   ctype(Type),
            '$char_type'(Char, Type)
        ).


ctype(alnum).
ctype(alpha).
ctype(alphabetic).
ctype(alphanumeric).
ctype(ascii).
ctype(ascii_graphic).
ctype(ascii_punctuation).
ctype(binary_digit).
ctype(control).
ctype(decimal_digit).
ctype(exponent).
ctype(graphic).
ctype(graphic_token).
ctype(hexadecimal_digit).
ctype(layout).
ctype(lower).
ctype(meta).
ctype(numeric).
ctype(octal_digit).
ctype(octet).
ctype(prolog).
ctype(sign).
ctype(solo).
ctype(symbolic_control).
ctype(symbolic_hexadecimal).
ctype(to_lower(_)).
ctype(to_upper(_)).
ctype(upper).
ctype(whitespace).
