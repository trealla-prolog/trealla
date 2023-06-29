:- module(charsio, [
	read_line_to_chars/3,
	get_n_chars/3,
	write_term_to_chars/3,
	read_from_chars/2
	]).

read_from_chars(Cs, T) :-
    read_term_from_chars(Cs, T, []).

read_line_to_chars(Stream, Cs0, Cs) :-
    getline(Stream, Line,[terminator(true)]),
    partial_string(Line,Cs0,Cs).

get_n_chars(Stream, N, Cs) :-
    bread(Stream, N, Cs).

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

var_list_contains_name([VarName = _ | VarList], VarName0) :-
    (  VarName == VarName0 -> true
    ;  var_list_contains_name(VarList, VarName0)
    ).

var_list_contains_variable([_ = Var | VarList], Var0) :-
    (  Var == Var0 -> true
    ;  var_list_contains_variable(VarList, Var0)
    ).

make_new_var_name(VarType, V, VarName, N, N1, VarList) :-
    fabricate_var_name(VarType, VarName0, N),
    (  var_list_contains_name(VarList, VarName0) ->
       N0 is N + 1,
       make_new_var_name(VarType, V, VarName, N0, N1, VarList)
    ;  VarName = VarName0,
       N1 is N + 1
    ).

extend_var_list(Vars, VarList, NewVarList, VarType) :-
    extend_var_list_(Vars, 0, VarList, NewVarList0, VarType),
    append(VarList, NewVarList0, NewVarList).

extend_var_list_([], _, _, [], _).
extend_var_list_([V|Vs], N, VarList, NewVarList, VarType) :-
    (  var_list_contains_variable(VarList, V) ->
       extend_var_list_(Vs, N, VarList, NewVarList, VarType)
    ;  make_new_var_name(VarType, V, VarName, N, N1, VarList),
       NewVarList = [VarName = V | NewVarList0],
       extend_var_list_(Vs, N1, VarList, NewVarList0, VarType)
    ).

%% write_term_to_chars(+Term, +Options, -Chars).
%
% Given a Term which is a Prolog term and a set of options, Chars is
% string representation of that term. Options available are:
%
%  * `ignore_ops(+Boolean)` if `true`, the generic term representation is used everywhere. In `false`
%    (default), operators do not use that generic term representation.
%  * `max_depth(+N)` if the term is nested deeper than N, print the reminder as ellipses.
%    If N = 0 (default), there's no limit.
%  * `numbervars(+Boolean)` if true, replaces `$VAR(N)` variables with letters, in order. Default is false.
%  * `quoted(+Boolean)` if true, strings and atoms that need quotes to be valid Prolog synytax, are quoted. Default is false.
%  * `variable_names(+List)` assign names to variables in term. List should be a list of terms of format `Name=Var`.
write_term_to_chars(_, Options, _) :-
    var(Options), instantiation_error(write_term_to_chars/3).
write_term_to_chars(Term, Options, Chars) :-
    term_variables(Term, Vars),
    memberchk(variable_names(VNNames), Options),
    extend_var_list(Vars, VNNames, NewVarNames, numbervars),
    '$write_term_to_chars'(Term, [variable_names(NewVarNames)|Options], Chars).

